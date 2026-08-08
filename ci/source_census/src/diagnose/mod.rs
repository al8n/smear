//! The diagnostic-contract census: al8n/smear#126's D1–D4, over the same crate reader.
//!
//! Phase A gave the crate `Diagnose`, and gave every kind enum a wildcard-free `match` per
//! accessor, so a *variant* added without a code is a hard `E0004`. Two holes are left that the
//! compiler cannot see, and this module is both of them:
//!
//! * **A new error *type* joins nothing.** A future proto phase can add `pub struct FooError`,
//!   never implement the contract, and every gate stays green — al8n/smear#122's defect shape
//!   exactly, one level up from the parameter it was about.
//! * **An `ALL` inventory can omit a variant.** Phase A measured this rather than supposing it:
//!   planting a variant on `SchemaErrorKind` reddened five matches and `SchemaErrorKind::ALL`
//!   was not one of them.
//!
//! # The four checks
//!
//! * **D1** — every public type named `*Error`, `*Errors` or `*Diagnostic`, every type standing
//!   in a public `Result`'s `Err`, and every type the crate writes an `impl Diagnose for`
//!   against, is either a *contract row* or an exemption with a written reason ([`exempt`]).
//! * **D2** — the contract rows are written into a generated crate that asserts
//!   `is_diagnose::<T>()` for each, and that crate is `cargo check`ed ([`probe`]). **The
//!   inventory is derived by `syn`; the property is decided by `rustc`.** A tool that reads
//!   `impl Diagnose for X` out of the tokens has learnt that the tokens exist, not that the
//!   bound holds at a use site, and the two differ exactly where a `where` clause does.
//! * **D3** — no `_ =>`, and no bare-binding arm, in any `code`, `severity`, `help` or
//!   `related_label` match. Those matches *are* the per-variant enforcement; one wildcard
//!   silently ends it.
//! * **D4** — every `ALL`-style inventory lists every variant its enum declares.
//!
//! # What is derived, and why that is the whole point
//!
//! D4's members are found by *shape* — an associated `const` whose value is an array of paths
//! naming one enum's variants — and not by the name `ALL`, and above all not by a list of the
//! four inventories anybody happens to know about. A hand-kept list of things to check is the
//! same defect as a hand-kept list of things to be, one level up, and it is the argument the
//! tool this extends was built on. The run prints every inventory it found so that the
//! derivation's reach is a number somebody can read, rather than a claim.

pub mod exempt;
pub mod probe;
pub mod selftest;
mod verdict;

pub use verdict::{render, verdict};

use std::collections::{BTreeMap, BTreeSet};

use quote::ToTokens as _;
use syn::{
  Expr, GenericArgument, GenericParam, Generics, ImplItem, Item, ItemImpl, PathArguments, Type,
  Visibility,
};

use crate::{
  census,
  surface::{Surface, is_doc_hidden},
};

/// The trait the contract is spelled as, by its own name.
///
/// Not by its path: al8n/tokora#240 moves the declaration into `tokora` and leaves `smear`
/// re-exporting it, so the path is precisely the part that is about to change. Every public
/// spelling the crate offers is derived and the probe asserts through all of them.
pub const TRAIT: &str = "Diagnose";

/// The accessors D3 governs, from al8n/smear#126 §5.
///
/// Extended at run time by every method the [`TRAIT`] declaration carries, when that declaration
/// is in this crate to be read — so a method added to the contract brings its own per-variant
/// matches under the check without this list being touched. The four are named anyway because
/// `related_label` is not a trait method (it is the per-kind half of the `related()` implication
/// `tests/diagnostic_codes.rs` pins), and because the trait moving out of the crate must not
/// silently empty the registry.
pub const REGISTERED: &[&str] = &["code", "severity", "help", "related_label"];

/// One type the contract has something to say about.
pub struct Row {
  pub ident: String,
  pub location: String,
  /// The shortest path a consumer can name it by.
  pub path: String,
  /// Every reason it is in the inventory, in the rule's own words.
  pub origins: Vec<String>,
  /// How the probe writes an instantiation of it. `None` when nothing could be spelled.
  pub spelling: Option<Spelling>,
}

/// A `cargo check`able instantiation of a [`Row`]'s type.
pub struct Spelling {
  /// `<'p0, S>`, or empty.
  pub generics: String,
  /// `where S: AsRef<[u8]>`, or empty.
  pub where_clause: String,
  /// `smear::validator::DiagnosticDisplay<'p0, S>`.
  pub ty: String,
}

/// A wildcard arm inside an accessor D3 governs.
pub struct Wildcard {
  pub function: String,
  pub location: String,
  /// `_` or the identifier the arm binds.
  pub pattern: String,
}

/// An `ALL`-style inventory and how it compares to the enum it enumerates.
pub struct Inventory {
  /// `SchemaErrorKind::ALL`.
  pub name: String,
  pub location: String,
  pub enum_location: String,
  pub declared: usize,
  pub listed: usize,
  /// Variants the enum declares and the inventory does not list.
  pub missing: Vec<String>,
  /// Names the inventory lists that the enum does not declare, or lists twice.
  pub spurious: Vec<String>,
}

pub struct Report {
  /// Every public spelling of [`TRAIT`], which the probe asserts through all of.
  pub trait_paths: Vec<String>,
  pub rows: Vec<Row>,
  /// Rows matched to an exemption, paired with it.
  pub recorded: Vec<(usize, &'static exempt::Exemption)>,
  /// Rows that must implement the contract, and that D2 hands to `rustc`.
  pub contract: Vec<usize>,
  /// Exemptions that matched no row — a table that has gone stale.
  pub stale: Vec<&'static exempt::Exemption>,
  /// Problems with the table itself.
  pub table_problems: Vec<String>,
  /// Aggregate exemptions whose named element is not itself a contract row.
  pub broken_aggregates: Vec<String>,
  /// Public `Result` `Err` types declared in another crate, which this one cannot make answer.
  pub foreign_err_types: Vec<String>,
  /// Public aliases folded into the row their target is.
  pub folded_aliases: Vec<String>,
  /// Accessor bodies D3 read, and the wildcards in them.
  pub matches_read: usize,
  pub accessors_read: usize,
  pub wildcards: Vec<Wildcard>,
  pub inventories: Vec<Inventory>,
  /// Inventories whose enum could not be resolved, which is a hole in D4's reach.
  pub unresolved_inventories: Vec<String>,
}

impl Report {
  /// Whether the row at `index` must implement the contract.
  fn is_contract(&self, index: usize) -> bool {
    self.contract.contains(&index)
  }
}

/// Reads the crate and answers D1, D3 and D4. D2 is [`probe::run`], which needs this first.
pub fn detect(surface: &Surface) -> Report {
  let enums = enums_by_ident(surface);
  let impls = impl_blocks(surface);

  let mut trait_paths: BTreeSet<String> = BTreeSet::new();
  for module in &surface.modules {
    for item in &module.items {
      let Item::Trait(t) = item else { continue };
      if t.ident != TRAIT {
        continue;
      }
      for path in surface.public_paths(&module.path, TRAIT) {
        trait_paths.insert(path.join("::"));
      }
    }
  }
  for path in surface.published_paths(TRAIT) {
    trait_paths.insert(path.join("::"));
  }

  let mut report = Report {
    trait_paths: trait_paths.into_iter().collect(),
    rows: Vec::new(),
    recorded: Vec::new(),
    contract: Vec::new(),
    stale: Vec::new(),
    table_problems: Vec::new(),
    broken_aggregates: Vec::new(),
    foreign_err_types: Vec::new(),
    folded_aliases: Vec::new(),
    matches_read: 0,
    accessors_read: 0,
    wildcards: Vec::new(),
    inventories: Vec::new(),
    unresolved_inventories: Vec::new(),
  };

  collect_rows(surface, &impls, &mut report);
  collect_wildcards(surface, &impls, &mut report);
  collect_inventories(surface, &impls, &enums, &mut report);
  report
}

// ── D1 ─────────────────────────────────────────────────────────────────────────────────────────

/// The name pattern al8n/smear#126 §5 gives, and the reason it is a pattern and not a list.
fn names_an_error(ident: &str) -> bool {
  ident.ends_with("Error") || ident.ends_with("Errors") || ident.ends_with("Diagnostic")
}

/// One type declaration, flattened out of the module walk.
struct Decl {
  module: Vec<String>,
  ident: String,
  location: String,
  generics: Generics,
  is_alias: bool,
  /// For an alias, the type it aliases, as written.
  alias_head: Option<TypeName>,
  public: bool,
  path: Option<String>,
}

fn declarations(surface: &Surface) -> Vec<Decl> {
  let mut out = Vec::new();
  for module in &surface.modules {
    for item in &module.items {
      let (ident, generics, vis, attrs, is_alias, alias_head) = match item {
        Item::Struct(i) => (
          i.ident.to_string(),
          i.generics.clone(),
          &i.vis,
          &i.attrs,
          false,
          None,
        ),
        Item::Enum(i) => (
          i.ident.to_string(),
          i.generics.clone(),
          &i.vis,
          &i.attrs,
          false,
          None,
        ),
        Item::Union(i) => (
          i.ident.to_string(),
          i.generics.clone(),
          &i.vis,
          &i.attrs,
          false,
          None,
        ),
        Item::Type(i) => (
          i.ident.to_string(),
          i.generics.clone(),
          &i.vis,
          &i.attrs,
          true,
          type_name(&i.ty),
        ),
        _ => continue,
      };
      let public = matches!(vis, Visibility::Public(_)) && !is_doc_hidden(attrs);
      let path = if public {
        surface
          .public_paths(&module.path, &ident)
          .first()
          .map(|path| path.join("::"))
      } else {
        None
      };
      out.push(Decl {
        location: format!("{}:{}", module.file.display(), line_of_ident(item)),
        module: module.path.clone(),
        ident,
        generics,
        is_alias,
        alias_head,
        public,
        path,
      });
    }
  }
  out
}

/// Builds the D1 inventory and reconciles it against the table.
fn collect_rows(surface: &Surface, impls: &[ImplBlock], report: &mut Report) {
  let decls = declarations(surface);

  // Keyed by the declaration, never by the ident: this crate declares `LexerError`, `FloatError`,
  // `SyntaxKind` and `Diagnostic` twice each, once per dialect or once per layer, and a map keyed
  // by name silently keeps one of every pair. That is one row where there are two, which is the
  // undercount the whole check exists to refuse.
  let mut origins: BTreeMap<usize, BTreeSet<String>> = BTreeMap::new();

  for (index, decl) in decls.iter().enumerate() {
    if decl.public && names_an_error(&decl.ident) {
      origins
        .entry(index)
        .or_default()
        .insert("its name matches `*Error`, `*Errors` or `*Diagnostic`".to_string());
    }
  }

  for entry in census::collect_entries(surface) {
    let syn::ReturnType::Type(_, ty) = &entry.sig.output else {
      continue;
    };
    let Some(err) = result_err(ty) else { continue };
    let Some(name) = type_name(&err) else {
      continue;
    };
    match resolve_decl(&decls, &name, &entry.module_path) {
      Some(index) if decls[index].public => {
        origins.entry(index).or_default().insert(format!(
          "it stands in the `Err` of `{}::{}`'s public `Result`",
          entry.module, entry.name
        ));
      }
      Some(_) => {}
      None => report.foreign_err_types.push(format!(
        "{}::{}  -> Result<_, {}>  ({})",
        entry.module,
        entry.name,
        name.ident,
        entry.location()
      )),
    }
  }

  let mut implementor: BTreeMap<usize, &ImplBlock> = BTreeMap::new();
  for block in impls {
    if block.trait_name.as_deref() != Some(TRAIT) {
      continue;
    }
    let Some(name) = type_name(&block.self_ty) else {
      continue;
    };
    let Some(index) = resolve_decl(&decls, &name, &block.module) else {
      continue;
    };
    origins
      .entry(index)
      .or_default()
      .insert("the crate writes an `impl Diagnose for` it".to_string());
    implementor.insert(index, block);
  }

  for (index, why) in origins {
    let decl = &decls[index];
    // An alias of a type that is a row is the same type, so it is folded into it rather than
    // recorded twice. An alias of something this crate does not declare is a row in its own right.
    if decl.is_alias
      && let Some(head) = &decl.alias_head
      && let Some(target) = resolve_decl(&decls, head, &decl.module)
    {
      report.folded_aliases.push(format!(
        "{}::{} = {}::{}",
        decl.module.join("::"),
        decl.ident,
        decls[target].module.join("::"),
        decls[target].ident
      ));
      continue;
    }
    let Some(path) = &decl.path else { continue };
    let spelling = implementor
      .get(&index)
      .map(|block| spell_from_impl(block, path))
      .unwrap_or_else(|| spell_from_decl(decl, path));
    report.rows.push(Row {
      ident: decl.ident.clone(),
      location: decl.location.clone(),
      path: path.clone(),
      origins: why.into_iter().collect(),
      spelling,
    });
  }

  report.rows.sort_by(|a, b| a.path.cmp(&b.path));
  report.folded_aliases.sort();
  reconcile(report);
}

/// Which declaration a type mentioned inside `from` means.
///
/// Rust's own answer needs the `use` graph. This one gets the same answers from two cheaper
/// facts, and it needs both:
///
/// * **The qualifier the source actually wrote.** `error::LexerError` says the declaration is in
///   a module called `error`, and this crate has four `LexerError`s — the two public ones and a
///   private per-handler alias in `handlers::str` and `handlers::slice`. On the ident alone the
///   lexer's public aliases resolved to nothing at all.
/// * **The longest shared module prefix**, so `graphql::syntactic` naming a type means
///   `graphql`'s and not `graphqlx`'s, with an exact module beating a descendant of it.
///
/// A genuine tie resolves to nothing rather than to a guess: a guess would put a row under the
/// wrong path, and therefore under the wrong exemption.
fn resolve_decl(decls: &[Decl], name: &TypeName, from: &[String]) -> Option<usize> {
  let matching: Vec<usize> = decls
    .iter()
    .enumerate()
    .filter(|(_, decl)| decl.ident == name.ident && decl.module.ends_with(&name.qualifier))
    .map(|(index, _)| index)
    .collect();
  let modules: Vec<&[String]> = matching
    .iter()
    .map(|index| decls[*index].module.as_slice())
    .collect();
  nearest(&modules, from).map(|which| matching[which])
}

/// A type as the source spells it: `error::LexerError` is `(["error"], "LexerError")`.
pub struct TypeName {
  /// The module segments written before the name, with `crate`, `self` and `super` dropped —
  /// they say where to start looking, not what the module is called.
  qualifier: Vec<String>,
  ident: String,
}

/// Reads a type's written path, through the wrappers that do not change what is named.
fn type_name(ty: &Type) -> Option<TypeName> {
  match ty {
    Type::Path(path) => {
      let mut segments: Vec<String> = path
        .path
        .segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect();
      let ident = segments.pop()?;
      segments.retain(|segment| !matches!(segment.as_str(), "crate" | "self" | "super"));
      Some(TypeName {
        qualifier: segments,
        ident,
      })
    }
    Type::Reference(reference) => type_name(&reference.elem),
    Type::Paren(paren) => type_name(&paren.elem),
    Type::Group(group) => type_name(&group.elem),
    _ => None,
  }
}

/// Matches every row against the table, and the table against the crate.
fn reconcile(report: &mut Report) {
  let mut used: BTreeSet<usize> = BTreeSet::new();
  for (index, row) in report.rows.iter().enumerate() {
    let found = exempt::EXEMPTIONS
      .iter()
      .enumerate()
      .find(|(_, e)| e.path == row.path);
    match found {
      Some((slot, exemption)) => {
        used.insert(slot);
        report.recorded.push((index, exemption));
      }
      None => report.contract.push(index),
    }
  }
  report.stale = exempt::EXEMPTIONS
    .iter()
    .enumerate()
    .filter(|(slot, _)| !used.contains(slot))
    .map(|(_, e)| e)
    .collect();
  report.table_problems = exempt::validate();

  // An aggregate is exempt because its elements answer. An aggregate whose element is itself
  // exempt answers nothing, and the chain is the kind of thing that reads fine one record at a
  // time, so it is checked rather than trusted.
  let contract: BTreeSet<&str> = report
    .contract
    .iter()
    .map(|index| report.rows[*index].path.as_str())
    .collect();
  let known: BTreeSet<&str> = report.rows.iter().map(|row| row.path.as_str()).collect();
  for (_, exemption) in &report.recorded {
    if !exemption.delegates() {
      continue;
    }
    let Some(element) = exemption.element else {
      continue;
    };
    if !contract.contains(element) {
      report.broken_aggregates.push(format!(
        "`{}` is recorded as answered by `{element}`, and `{element}` {}",
        exemption.path,
        if known.contains(element) {
          "is itself exempt — so nothing in the chain answers the contract"
        } else {
          "is not a row at all — the type named does not exist under that path"
        }
      ));
    }
  }
}

/// The `E` of a `Result<_, E>` return, however the alias is spelled.
fn result_err(ty: &Type) -> Option<Type> {
  let Type::Path(path) = ty else { return None };
  let last = path.path.segments.last()?;
  if last.ident != "Result" {
    return None;
  }
  let PathArguments::AngleBracketed(args) = &last.arguments else {
    return None;
  };
  let types: Vec<&Type> = args
    .args
    .iter()
    .filter_map(|arg| match arg {
      GenericArgument::Type(ty) => Some(ty),
      _ => None,
    })
    .collect();
  types.get(1).map(|ty| (*ty).clone())
}

// ── D2's half of the work: how a row's type is written down ────────────────────────────────────

/// Spells the instantiation the crate's own `impl` block proves.
///
/// Its generics and `where` clause are copied verbatim, which is what makes the assertion mean
/// something: a bound of `T: Diagnose` would make it vacuous, and the impl's own bounds are the
/// conditions under which the crate claims the contract holds.
fn spell_from_impl(block: &ImplBlock, path: &str) -> Option<Spelling> {
  let Type::Path(self_path) = &block.self_ty else {
    return None;
  };
  let last = self_path.path.segments.last()?;
  let mut fresh = 0usize;
  let args = match &last.arguments {
    PathArguments::None => String::new(),
    PathArguments::AngleBracketed(args) => {
      let rendered: Vec<String> = args
        .args
        .iter()
        .map(|arg| match arg {
          // `impl Diagnose for DiagnosticDisplay<'_, S>` elides its lifetime, and an elided one
          // cannot be named in the probe's own generics. A fresh parameter stands in for it.
          GenericArgument::Lifetime(lt) if lt.ident == "_" => {
            let name = format!("'p{fresh}");
            fresh += 1;
            name
          }
          other => tidy(&other.to_token_stream().to_string()),
        })
        .collect();
      format!("<{}>", rendered.join(", "))
    }
    PathArguments::Parenthesized(_) => return None,
  };

  let mut params: Vec<String> = (0..fresh).map(|index| format!("'p{index}")).collect();
  params.extend(generic_params(&block.generics));

  // A fresh lifetime standing in for an elided one needs the outlives bound the type's own
  // well-formedness implies: `DiagnosticDisplay<'a, S>` holds a `&'a Diagnostic<S>`, so `S: 'a`
  // is not a narrowing of the assertion but a condition of the type existing. rustc named this
  // on the probe's first run, which is the generated crate doing its job on itself.
  let mut predicates = predicates(&block.generics);
  for index in 0..fresh {
    for param in block.generics.type_params() {
      predicates.push(format!("{}: 'p{index}", param.ident));
    }
  }

  Some(Spelling {
    generics: if params.is_empty() {
      String::new()
    } else {
      format!("<{}>", params.join(", "))
    },
    where_clause: render_where(&predicates),
    ty: format!("{path}{args}"),
  })
}

/// Spells the type from its own declaration, for a row with no `impl` in the crate.
///
/// This is the shape that reds. A type nobody implemented is asserted at its declared generics,
/// and `rustc` answers `E0277` naming it — which is the whole of D2.
fn spell_from_decl(decl: &Decl, path: &str) -> Option<Spelling> {
  let params = generic_params(&decl.generics);
  let args: Vec<String> = decl
    .generics
    .params
    .iter()
    .map(|param| match param {
      GenericParam::Lifetime(lt) => format!("'{}", lt.lifetime.ident),
      GenericParam::Type(ty) => ty.ident.to_string(),
      GenericParam::Const(c) => c.ident.to_string(),
    })
    .collect();
  Some(Spelling {
    generics: if params.is_empty() {
      String::new()
    } else {
      format!("<{}>", params.join(", "))
    },
    where_clause: render_where(&predicates(&decl.generics)),
    ty: if args.is_empty() {
      path.to_string()
    } else {
      format!("{path}<{}>", args.join(", "))
    },
  })
}

/// The generic parameters, with defaults stripped — a `fn` may not carry them.
fn generic_params(generics: &Generics) -> Vec<String> {
  generics
    .params
    .iter()
    .map(|param| {
      let mut param = param.clone();
      match &mut param {
        GenericParam::Type(ty) => {
          ty.default = None;
          ty.eq_token = None;
        }
        GenericParam::Const(c) => {
          c.default = None;
          c.eq_token = None;
        }
        GenericParam::Lifetime(_) => {}
      }
      tidy(&param.to_token_stream().to_string())
    })
    .collect()
}

/// The `where` clause as one predicate per entry, so more can be appended to it.
fn predicates(generics: &Generics) -> Vec<String> {
  generics
    .where_clause
    .iter()
    .flat_map(|clause| clause.predicates.iter())
    .map(|predicate| tidy(&predicate.to_token_stream().to_string()))
    .collect()
}

fn render_where(predicates: &[String]) -> String {
  if predicates.is_empty() {
    String::new()
  } else {
    format!("where\n  {}", predicates.join(",\n  "))
  }
}

// ── D3 ─────────────────────────────────────────────────────────────────────────────────────────

/// Reads every accessor the registry names and reports the wildcards in them.
fn collect_wildcards(surface: &Surface, impls: &[ImplBlock], report: &mut Report) {
  let mut registry: BTreeSet<String> = REGISTERED.iter().map(|name| (*name).to_string()).collect();
  for module in &surface.modules {
    for item in &module.items {
      let Item::Trait(t) = item else { continue };
      if t.ident != TRAIT {
        continue;
      }
      for member in &t.items {
        if let syn::TraitItem::Fn(f) = member {
          registry.insert(f.sig.ident.to_string());
        }
      }
    }
  }

  for block in impls {
    for member in &block.items {
      let ImplItem::Fn(f) = member else { continue };
      let name = f.sig.ident.to_string();
      if !registry.contains(&name) {
        continue;
      }
      report.accessors_read += 1;
      let owner = head_ident(&block.self_ty).unwrap_or_else(|| "?".to_string());
      let mut finder = MatchFinder {
        matches: Vec::new(),
      };
      syn::visit::visit_block(&mut finder, &f.block);
      for expr in finder.matches {
        if !dispatches_on_self(&expr.expr) {
          continue;
        }
        report.matches_read += 1;
        for arm in &expr.arms {
          for pattern in catch_alls(&arm.pat) {
            report.wildcards.push(Wildcard {
              function: format!("{owner}::{name}"),
              location: format!("{}:{}", block.file, line_of_pat(&arm.pat)),
              pattern,
            });
          }
        }
      }
    }
  }
}

/// Whether a `match` dispatches on the receiver — which is what makes it the per-variant one.
///
/// `match self`, `match *self` and `match self.kind` all are. `match index` is not, and the
/// distinction is load-bearing rather than fussy: `Diagnose::label(&self, index: usize)` matches
/// on a `usize`, where a `_ =>` arm is not a lapse but the only way to write the function.
/// Reading every match in the body instead reported both of the crate's `label` impls as
/// findings, which would have been two false accusations in the first run.
fn dispatches_on_self(scrutinee: &Expr) -> bool {
  match scrutinee {
    Expr::Path(path) => path.qself.is_none() && path.path.is_ident("self"),
    Expr::Field(field) => dispatches_on_self(&field.base),
    Expr::MethodCall(call) => dispatches_on_self(&call.receiver),
    Expr::Unary(syn::ExprUnary {
      op: syn::UnOp::Deref(_),
      expr,
      ..
    }) => dispatches_on_self(expr),
    Expr::Reference(reference) => dispatches_on_self(&reference.expr),
    Expr::Paren(paren) => dispatches_on_self(&paren.expr),
    Expr::Group(group) => dispatches_on_self(&group.expr),
    _ => false,
  }
}

struct MatchFinder {
  matches: Vec<syn::ExprMatch>,
}

impl<'ast> syn::visit::Visit<'ast> for MatchFinder {
  fn visit_expr_match(&mut self, node: &'ast syn::ExprMatch) {
    self.matches.push(node.clone());
    syn::visit::visit_expr_match(self, node);
  }
}

/// The patterns that make an arm a catch-all.
///
/// `_` is one. So is a bare binding — `other => …` matches everything the arms above did not,
/// which is the same silence in a different spelling. A bare `Foo` that is really a unit variant
/// brought in by `use Enum::*` parses identically to a binding and `syn` cannot tell them apart,
/// so the leading case decides: this crate spells every variant `Self::Foo`, and a lowercase
/// leading letter is a binding.
fn catch_alls(pat: &syn::Pat) -> Vec<String> {
  match pat {
    syn::Pat::Wild(_) => vec!["_".to_string()],
    syn::Pat::Ident(ident) if ident.subpat.is_none() => {
      let name = ident.ident.to_string();
      if name.starts_with(|c: char| c.is_ascii_uppercase()) {
        Vec::new()
      } else {
        vec![name]
      }
    }
    syn::Pat::Or(or) => or.cases.iter().flat_map(catch_alls).collect(),
    syn::Pat::Paren(paren) => catch_alls(&paren.pat),
    syn::Pat::Reference(reference) => catch_alls(&reference.pat),
    _ => Vec::new(),
  }
}

// ── D4 ─────────────────────────────────────────────────────────────────────────────────────────

/// One enum declaration, by ident and by module so `SyntaxKind` twice over stays two enums.
struct EnumDecl {
  module: Vec<String>,
  variants: Vec<String>,
  location: String,
}

fn enums_by_ident(surface: &Surface) -> BTreeMap<String, Vec<EnumDecl>> {
  let mut out: BTreeMap<String, Vec<EnumDecl>> = BTreeMap::new();
  for module in &surface.modules {
    for item in &module.items {
      let Item::Enum(e) = item else { continue };
      out.entry(e.ident.to_string()).or_default().push(EnumDecl {
        module: module.path.clone(),
        variants: e.variants.iter().map(|v| v.ident.to_string()).collect(),
        location: format!("{}:{}", module.file.display(), e.ident.span().start().line),
      });
    }
  }
  out
}

/// Finds every inventory by shape and compares it to the enum it claims to enumerate.
///
/// By shape, not by the name `ALL`: an associated `const` whose value is an array of paths, each
/// naming a variant of one enum. Naming the constant would be a list of inventories to check, and
/// a list is the defect.
fn collect_inventories(
  surface: &Surface,
  impls: &[ImplBlock],
  enums: &BTreeMap<String, Vec<EnumDecl>>,
  report: &mut Report,
) {
  let _ = surface;
  for block in impls {
    let Some(owner) = head_ident(&block.self_ty) else {
      continue;
    };
    for member in &block.items {
      let ImplItem::Const(c) = member else { continue };
      let Some(elements) = variant_list(&c.expr) else {
        continue;
      };
      // Which enum: `Self::Query` says the impl's own type, `SyntaxKind::Name` says itself. The
      // two spellings both appear in this crate, in the same file in one case.
      let named: BTreeSet<&str> = elements.iter().map(|(owner, _)| owner.as_str()).collect();
      let mut heads: Vec<&str> = named
        .into_iter()
        .map(|name| if name == "Self" { owner.as_str() } else { name })
        .collect();
      heads.sort_unstable();
      heads.dedup();
      let [head] = heads[..] else {
        report.unresolved_inventories.push(format!(
          "{owner}::{}  ({}:{}) — names {} different types, so it enumerates none of them",
          c.ident,
          block.file,
          c.ident.span().start().line,
          heads.len()
        ));
        continue;
      };

      let Some(target) = resolve_enum(head, &block.module, enums) else {
        report.unresolved_inventories.push(format!(
          "{owner}::{}  ({}:{}) — `{head}` is not an enum this crate declares, or is declared \
           more than once and not beside the `impl`",
          c.ident,
          block.file,
          c.ident.span().start().line,
        ));
        continue;
      };

      let listed: Vec<&str> = elements.iter().map(|(_, v)| v.as_str()).collect();
      let seen: BTreeSet<&str> = listed.iter().copied().collect();
      let declared: BTreeSet<&str> = target.variants.iter().map(String::as_str).collect();
      let mut spurious: Vec<String> = seen
        .difference(&declared)
        .map(|v| (*v).to_string())
        .collect();
      let mut counts: BTreeMap<&str, usize> = BTreeMap::new();
      for variant in &listed {
        *counts.entry(*variant).or_default() += 1;
      }
      spurious.extend(
        counts
          .iter()
          .filter(|(_, count)| **count > 1)
          .map(|(variant, count)| format!("{variant} (listed {count} times)")),
      );
      spurious.sort();

      report.inventories.push(Inventory {
        name: format!("{head}::{}", c.ident),
        location: format!("{}:{}", block.file, c.ident.span().start().line),
        enum_location: target.location.clone(),
        declared: declared.len(),
        listed: seen.len(),
        missing: declared
          .difference(&seen)
          .map(|v| (*v).to_string())
          .collect(),
        spurious,
      });
    }
  }
  report
    .inventories
    .sort_by(|a, b| (a.name.as_str(), a.location.as_str()).cmp(&(b.name.as_str(), &b.location)));
}

/// `[Self::A, Self::B]` or `&[Enum::A, Enum::B]`, as `(owner, variant)` pairs.
///
/// `None` for anything else, including an array with one element that is not a variant path: a
/// constant that is only partly an inventory is not one, and guessing would put D4's verdict on
/// a value nobody meant as an enumeration.
fn variant_list(expr: &Expr) -> Option<Vec<(String, String)>> {
  let array = match expr {
    Expr::Array(array) => array,
    Expr::Reference(reference) => match &*reference.expr {
      Expr::Array(array) => array,
      _ => return None,
    },
    _ => return None,
  };
  if array.elems.is_empty() {
    return None;
  }
  let mut out = Vec::new();
  for element in &array.elems {
    let Expr::Path(path) = element else {
      return None;
    };
    if path.qself.is_some() || path.path.segments.len() < 2 {
      return None;
    }
    let segments: Vec<String> = path
      .path
      .segments
      .iter()
      .map(|segment| segment.ident.to_string())
      .collect();
    let variant = segments.last()?.clone();
    let owner = segments.get(segments.len() - 2)?.clone();
    out.push((owner, variant));
  }
  Some(out)
}

/// The enum `head` names from inside `module`, by the same rule [`resolve_decl`] uses.
///
/// `SyntaxKind` is declared once per dialect and each dialect's `ALL` sits beside its own, so an
/// answer of "the first one" would compare `graphqlx`'s 113 variants against `graphql`'s 87 and
/// report twenty-six omissions that are not there.
fn resolve_enum<'a>(
  head: &str,
  module: &[String],
  enums: &'a BTreeMap<String, Vec<EnumDecl>>,
) -> Option<&'a EnumDecl> {
  let candidates = enums.get(head)?;
  let modules: Vec<&[String]> = candidates
    .iter()
    .map(|decl| decl.module.as_slice())
    .collect();
  nearest(&modules, module).map(|index| &candidates[index])
}

/// The candidate whose module shares the longest prefix with `from`; `None` on a tie.
fn nearest(modules: &[&[String]], from: &[String]) -> Option<usize> {
  let mut best: Option<(usize, usize, usize)> = None;
  let mut tied = false;
  for (index, module) in modules.iter().enumerate() {
    let shared = module.iter().zip(from).take_while(|(a, b)| a == b).count();
    let exact = usize::from(*module == from);
    match best {
      Some((top_shared, top_exact, _)) if (shared, exact) < (top_shared, top_exact) => {}
      Some((top_shared, top_exact, _)) if (shared, exact) == (top_shared, top_exact) => {
        tied = true;
      }
      _ => {
        best = Some((shared, exact, index));
        tied = false;
      }
    }
  }
  if tied {
    None
  } else {
    best.map(|(_, _, index)| index)
  }
}

// ── Reading the crate's `impl` blocks once ─────────────────────────────────────────────────────

pub struct ImplBlock {
  pub module: Vec<String>,
  pub file: String,
  pub self_ty: Type,
  pub trait_name: Option<String>,
  pub generics: Generics,
  pub items: Vec<ImplItem>,
}

fn impl_blocks(surface: &Surface) -> Vec<ImplBlock> {
  let mut out = Vec::new();
  for module in &surface.modules {
    for item in &module.items {
      let Item::Impl(block) = item else { continue };
      out.push(ImplBlock {
        module: module.path.clone(),
        file: module.file.display().to_string(),
        self_ty: (*block.self_ty).clone(),
        trait_name: trait_name(block),
        generics: block.generics.clone(),
        items: block.items.clone(),
      });
    }
  }
  out
}

fn trait_name(block: &ItemImpl) -> Option<String> {
  let (_, path, _) = block.trait_.as_ref()?;
  Some(path.segments.last()?.ident.to_string())
}

/// The last ident of a type's path — `LexerError` out of `error::LexerError<Char, E>`.
fn head_ident(ty: &Type) -> Option<String> {
  match ty {
    Type::Path(path) => Some(path.path.segments.last()?.ident.to_string()),
    Type::Reference(reference) => head_ident(&reference.elem),
    Type::Paren(paren) => head_ident(&paren.elem),
    Type::Group(group) => head_ident(&group.elem),
    _ => None,
  }
}

/// The line a type declaration's name is on; zero for an item that has no name.
fn line_of_ident(item: &Item) -> usize {
  match item {
    Item::Struct(i) => i.ident.span().start().line,
    Item::Enum(i) => i.ident.span().start().line,
    Item::Union(i) => i.ident.span().start().line,
    Item::Type(i) => i.ident.span().start().line,
    _ => 0,
  }
}

fn line_of_pat(pat: &syn::Pat) -> usize {
  use syn::spanned::Spanned as _;
  pat.span().start().line
}

/// Undoes the whitespace `to_token_stream` inserts, so generated Rust reads like Rust.
fn tidy(text: &str) -> String {
  text
    .replace(" ::", "::")
    .replace(":: ", "::")
    .replace(" <", "<")
    .replace("< ", "<")
    .replace(" >", ">")
    .replace(" ,", ",")
    .replace("& ", "&")
    .replace("' ", "'")
}
