//! Runs the rule over the surface and renders the verdict.

use std::collections::{BTreeSet, HashSet};

use syn::{ImplItem, Item, Signature, TraitItem, Type, Visibility};

use crate::{
  exempt::{self, EXEMPTIONS, Exemption},
  rule::{
    self, Cost, Scope, SourcePositions, TextHit, bound_type_list, family_at_concrete_text,
    harvest_source_positions, holds_source_generically, render, signature_types,
  },
  surface::{self, MacroTemplate, Surface, is_doc_hidden},
};

/// One public function or inherent method the census read.
pub struct Entry {
  pub module: String,
  /// The same module, unjoined, for a resolver that has to compare it segment by segment.
  pub module_path: Vec<String>,
  pub name: String,
  pub file: String,
  pub line: usize,
  pub sig: Signature,
  pub self_ty: Option<Type>,
  pub scope: Scope,
}

impl Entry {
  pub fn location(&self) -> String {
    format!("{}:{}", self.file, self.line)
  }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Verdict {
  /// A byte view: `S: AsRef<[u8]>` already produces one, so nothing is imposed on the caller.
  Neutral,
  /// Concrete text, but not the document: a name, a key, a message, an output buffer, a constant.
  Datum,
  /// Concrete text standing where the crate elsewhere puts a source type parameter.
  Narrowed,
}

pub struct Observation {
  pub module: String,
  pub entry: String,
  pub location: String,
  pub param: String,
  pub ty: String,
  /// The concrete text type inside `ty` that the verdict is about, when the parameter's type is
  /// larger than it — `Cow<'static, str>` inside `impl Into<Cow<'static, str>>`.
  pub text: String,
  pub cost: Cost,
  pub verdict: Verdict,
  /// Which test decided it, in the rule's own numbering.
  pub why: String,
}

pub struct Report {
  pub entries_read: usize,
  pub params_read: usize,
  pub observations: Vec<Observation>,
  pub source_positions: SourcePositions,
  pub files_read: usize,
  pub macro_invocations: usize,
  /// `macro_rules!` bodies that template a `pub fn` taking concrete text.
  pub macro_templates: Vec<MacroTemplate>,
  pub public_modules: Vec<String>,
  /// Narrowings with no exemption. Every one is a failure.
  pub unexplained: Vec<usize>,
  /// Narrowings matched to an exemption, paired with it.
  pub recorded: Vec<(usize, &'static Exemption)>,
  /// Exemptions that matched nothing — a table that has gone stale.
  pub stale: Vec<&'static Exemption>,
  /// Problems with the table itself.
  pub table_problems: Vec<String>,
}

/// Classifies every concrete text parameter on the crate's public surface.
///
/// Detection only: the exemption table is applied by [`reconcile`], so that the selftest can
/// exercise the rule against synthetic crates the table knows nothing about.
pub fn detect(surface: &Surface) -> Report {
  let crate_types = declared_types(surface);
  let source_positions = derive_source_positions(surface, &crate_types);
  let entries = collect_entries(surface);

  let mut observations = Vec::new();
  let mut params_read = 0usize;

  for entry in &entries {
    let all_types = signature_types(&entry.sig, entry.self_ty.as_ref());
    let entry_has_family_at_text = all_types
      .iter()
      .find_map(|ty| family_at_concrete_text(ty, &source_positions));

    for arg in &entry.sig.inputs {
      let syn::FnArg::Typed(typed) = arg else {
        continue;
      };
      params_read += 1;
      let param = pattern_name(&typed.pat);
      let hits = rule::text_hits(&typed.ty);
      let Some(hit) = worst(&hits) else { continue };

      let this_ty = render(&typed.ty);
      let others: Vec<Type> = all_types
        .iter()
        .filter(|ty| render(ty) != this_ty)
        .cloned()
        .collect();
      let (verdict, why) = classify(
        entry,
        &param,
        hit,
        &others,
        entry_has_family_at_text.as_ref(),
        &source_positions,
      );

      observations.push(Observation {
        module: entry.module.clone(),
        entry: entry.name.clone(),
        location: entry.location(),
        param,
        ty: render(&typed.ty),
        text: hit.rendered.clone(),
        cost: hit.cost,
        verdict,
        why,
      });
    }
  }

  observations.sort_by(|a, b| {
    (a.module.as_str(), a.entry.as_str(), a.param.as_str()).cmp(&(
      b.module.as_str(),
      b.entry.as_str(),
      b.param.as_str(),
    ))
  });

  let macro_templates = surface::macro_templates(surface);
  let public_modules = surface
    .modules
    .iter()
    .filter(|m| m.named_by_path && m.path.len() > 1)
    .map(|m| m.display_path())
    .collect();

  Report {
    entries_read: entries.len(),
    params_read,
    observations,
    source_positions,
    files_read: surface.files_read,
    macro_invocations: surface.macro_invocations,
    macro_templates,
    public_modules,
    unexplained: Vec::new(),
    recorded: Vec::new(),
    stale: Vec::new(),
    table_problems: Vec::new(),
  }
}

/// Matches every narrowing against the exemption table, and the table against the crate.
pub fn reconcile(report: &mut Report) {
  let mut used: HashSet<usize> = HashSet::new();
  for (index, observation) in report.observations.iter().enumerate() {
    if observation.verdict != Verdict::Narrowed {
      continue;
    }
    let found = EXEMPTIONS.iter().enumerate().find(|(_, exemption)| {
      exemption.matches(&observation.module, &observation.entry, &observation.param)
    });
    match found {
      Some((slot, exemption)) => {
        used.insert(slot);
        report.recorded.push((index, exemption));
      }
      None => report.unexplained.push(index),
    }
  }
  report.stale = EXEMPTIONS
    .iter()
    .enumerate()
    .filter(|(slot, _)| !used.contains(slot))
    .map(|(_, exemption)| exemption)
    .collect();
  report.table_problems = exempt::validate();
}

/// The four tests of `rule`'s header, in order.
fn classify(
  entry: &Entry,
  param: &str,
  hit: &TextHit,
  other_types: &[Type],
  family_hit: Option<&(String, String)>,
  positions: &SourcePositions,
) -> (Verdict, String) {
  if hit.cost == Cost::None {
    return (
      Verdict::Neutral,
      "a byte view — `S: AsRef<[u8]>` produces one, so no representation is imposed".to_string(),
    );
  }
  if hit.out_param {
    return (
      Verdict::Datum,
      "behind `&mut`: an output buffer, not input".to_string(),
    );
  }
  if hit.konstant {
    return (
      Verdict::Datum,
      "explicitly `'static`: a compile-time constant, not a slice of a caller's document"
        .to_string(),
    );
  }
  if let Some((family, argument)) = family_hit {
    return (
      Verdict::Narrowed,
      format!(
        "test 1 — `{family}` carries a source position and this entry applies it to `{argument}`"
      ),
    );
  }
  if rule::SOURCE_LEXICON.contains(&param) {
    return (
      Verdict::Narrowed,
      format!("test 2 — the parameter is named `{param}`, which the crate uses for a document"),
    );
  }
  let holder = other_types
    .iter()
    .find(|ty| holds_source_generically(ty, &entry.scope, positions));
  if let Some(holder) = holder {
    return (
      Verdict::Datum,
      format!(
        "test 3 — the source is already in hand generically, as `{}`",
        render(holder)
      ),
    );
  }
  (
    Verdict::Narrowed,
    "test 4 — nothing in the signature carries a source type, so this parameter is the document"
      .to_string(),
  )
}

/// The costliest text type inside one parameter; a parameter is judged on its worst position.
///
/// `&mut` and `'static` hits are set aside first, so `render(out: &mut String, space: &'static
/// str)` is not convicted by either. Falling back to the first hit when that leaves nothing keeps
/// the parameter reported — as the datum it is — rather than dropped from the census silently.
/// Utf8 outranks Representation only to decide which hit is quoted; both convict.
fn worst(hits: &[TextHit]) -> Option<&TextHit> {
  hits
    .iter()
    .filter(|h| !h.out_param && !h.konstant)
    .max_by_key(|h| match h.cost {
      Cost::None => 0,
      Cost::Representation => 1,
      Cost::Utf8 => 2,
    })
    .or_else(|| hits.first())
}

fn pattern_name(pat: &syn::Pat) -> String {
  use quote::ToTokens as _;
  match pat {
    syn::Pat::Ident(i) => i.ident.to_string(),
    other => other.to_token_stream().to_string(),
  }
}

/// Idents of every type, enum, union, trait and alias the crate declares.
fn declared_types(surface: &Surface) -> BTreeSet<String> {
  let mut out = BTreeSet::new();
  for module in &surface.modules {
    for item in &module.items {
      let ident = match item {
        Item::Struct(i) => Some(i.ident.to_string()),
        Item::Enum(i) => Some(i.ident.to_string()),
        Item::Union(i) => Some(i.ident.to_string()),
        Item::Trait(i) => Some(i.ident.to_string()),
        Item::Type(i) => Some(i.ident.to_string()),
        _ => None,
      };
      out.extend(ident);
    }
  }
  out
}

/// Reads the whole crate — public and private — for the positions it treats as source positions.
fn derive_source_positions(surface: &Surface, crate_types: &BTreeSet<String>) -> SourcePositions {
  let mut positions = SourcePositions::new();
  let harvest = |types: &[Type], scope: &Scope, positions: &mut SourcePositions| {
    for ty in types {
      harvest_source_positions(ty, scope, crate_types, positions);
    }
  };

  for module in &surface.modules {
    for item in &module.items {
      match item {
        Item::Fn(f) => {
          let mut scope = Scope::default();
          scope.extend_from(&f.sig.generics);
          harvest(&signature_types(&f.sig, None), &scope, &mut positions);
        }
        Item::Impl(i) => {
          let mut impl_scope = Scope::default();
          impl_scope.extend_from(&i.generics);
          let mut types = vec![(*i.self_ty).clone()];
          types.extend(bound_type_list(&i.generics));
          harvest(&types, &impl_scope, &mut positions);
          for member in &i.items {
            let ImplItem::Fn(f) = member else { continue };
            let mut scope = impl_scope.clone();
            scope.extend_from(&f.sig.generics);
            harvest(
              &signature_types(&f.sig, Some(&i.self_ty)),
              &scope,
              &mut positions,
            );
          }
        }
        Item::Trait(t) => {
          let mut trait_scope = Scope::default();
          trait_scope.extend_from(&t.generics);
          harvest(&bound_type_list(&t.generics), &trait_scope, &mut positions);
          for member in &t.items {
            let TraitItem::Fn(f) = member else { continue };
            let mut scope = trait_scope.clone();
            scope.extend_from(&f.sig.generics);
            harvest(&signature_types(&f.sig, None), &scope, &mut positions);
          }
        }
        Item::Struct(s) => {
          let mut scope = Scope::default();
          scope.extend_from(&s.generics);
          let types: Vec<Type> = s.fields.iter().map(|f| f.ty.clone()).collect();
          harvest(&types, &scope, &mut positions);
        }
        Item::Enum(e) => {
          let mut scope = Scope::default();
          scope.extend_from(&e.generics);
          let types: Vec<Type> = e
            .variants
            .iter()
            .flat_map(|v| v.fields.iter())
            .map(|f| f.ty.clone())
            .collect();
          harvest(&types, &scope, &mut positions);
        }
        Item::Type(t) => {
          let mut scope = Scope::default();
          scope.extend_from(&t.generics);
          harvest(&[(*t.ty).clone()], &scope, &mut positions);
        }
        _ => {}
      }
    }
  }
  positions
}

/// Every public function, inherent method and trait method the crate exposes.
pub fn collect_entries(surface: &Surface) -> Vec<Entry> {
  let mut out = Vec::new();
  for module in &surface.modules {
    let module_path = module.display_path();
    for item in &module.items {
      match item {
        Item::Fn(f) => {
          if !matches!(f.vis, Visibility::Public(_))
            || is_doc_hidden(&f.attrs)
            || !surface.item_is_public(&module.path, &f.sig.ident.to_string(), module.named_by_path)
          {
            continue;
          }
          let mut scope = Scope::default();
          scope.extend_from(&f.sig.generics);
          out.push(Entry {
            module: module_path.clone(),
            module_path: module.path.clone(),
            name: f.sig.ident.to_string(),
            file: module.file.display().to_string(),
            line: line_of(&f.sig.ident),
            sig: f.sig.clone(),
            self_ty: None,
            scope,
          });
        }
        Item::Trait(t) => {
          if !matches!(t.vis, Visibility::Public(_))
            || is_doc_hidden(&t.attrs)
            || !surface.item_is_public(&module.path, &t.ident.to_string(), module.named_by_path)
          {
            continue;
          }
          let mut trait_scope = Scope::default();
          trait_scope.extend_from(&t.generics);
          for member in &t.items {
            let TraitItem::Fn(f) = member else { continue };
            if is_doc_hidden(&f.attrs) {
              continue;
            }
            let mut scope = trait_scope.clone();
            scope.extend_from(&f.sig.generics);
            out.push(Entry {
              module: module_path.clone(),
              module_path: module.path.clone(),
              name: format!("{}::{}", t.ident, f.sig.ident),
              file: module.file.display().to_string(),
              line: line_of(&f.sig.ident),
              sig: f.sig.clone(),
              self_ty: None,
              scope,
            });
          }
        }
        Item::Impl(i) => {
          if i.trait_.is_some() || is_doc_hidden(&i.attrs) {
            continue;
          }
          let Some(type_name) = self_type_ident(&i.self_ty) else {
            continue;
          };
          if !surface.public_types.contains(&type_name) {
            continue;
          }
          let mut impl_scope = Scope::default();
          impl_scope.extend_from(&i.generics);
          for member in &i.items {
            let ImplItem::Fn(f) = member else { continue };
            if !matches!(f.vis, Visibility::Public(_)) || is_doc_hidden(&f.attrs) {
              continue;
            }
            let mut scope = impl_scope.clone();
            scope.extend_from(&f.sig.generics);
            out.push(Entry {
              module: module_path.clone(),
              module_path: module.path.clone(),
              name: format!("{}::{}", type_name, f.sig.ident),
              file: module.file.display().to_string(),
              line: line_of(&f.sig.ident),
              sig: f.sig.clone(),
              self_ty: Some((*i.self_ty).clone()),
              scope,
            });
          }
        }
        _ => {}
      }
    }
  }
  out
}

fn self_type_ident(ty: &Type) -> Option<String> {
  match ty {
    Type::Path(p) => Some(p.path.segments.last()?.ident.to_string()),
    Type::Reference(r) => self_type_ident(&r.elem),
    Type::Paren(p) => self_type_ident(&p.elem),
    Type::Group(g) => self_type_ident(&g.elem),
    _ => None,
  }
}

fn line_of(ident: &proc_macro2::Ident) -> usize {
  ident.span().start().line
}
