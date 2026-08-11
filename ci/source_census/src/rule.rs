//! What counts as a parameter carrying source text, and why.
//!
//! # The property
//!
//! `smear` ships `bstr`, `bytes`, `hipstr`, `smallvec` and `smol-bytes` integrations so that a
//! consumer picks its own source representation. §3, syntactic §5 and §6/§7 honour that with
//! `S: AsRef<[u8]>`. A public entry that spells a source parameter `&str` instead takes that
//! choice away: a caller holding `bytes::Bytes` must validate UTF-8 and re-borrow before it can
//! call, and nothing in the crate says so — the constraint is discovered by the borrow checker at
//! the call site, possibly years later (al8n/smear#121, #122).
//!
//! # Step 1 — does the type constrain anything at all
//!
//! Not every concrete text type is a narrowing:
//!
//! * **`&[u8]` constrains nobody.** `S: AsRef<[u8]>` *is* the promise that a source produces one,
//!   so a byte view is the currency every representation already speaks. `Schema::sym(&[u8])` and
//!   `Schema::type_by_name(&[u8])` are correct as written and this census says nothing about them.
//! * **`&str` and `String` do constrain.** They add a UTF-8 obligation the byte bound does not
//!   carry.
//! * **`Vec<u8>`, `Bytes`, `HipStr`, `SmallVec<[u8; N]>` … constrain**, differently: they pin one
//!   owned representation, which is the same defect wearing the other hat.
//!
//! # Step 2 — is this parameter the document
//!
//! This is the judgement the whole census turns on. Too narrow and the next narrowing walks past
//! it; too broad and it fires on `Executor::start`'s `operation: Option<&str>`, which is a lookup
//! key the caller composes — draft §6.1's `GetOperation` argument — and is correctly `&str`.
//!
//! The rule, in one sentence:
//!
//! > **A concrete text parameter is the document unless the entry demonstrably already has the
//! > document in hand generically.**
//!
//! Default-convict, because the two failure directions are not symmetric. A false positive is a
//! line in the exemption table with a reason on it, read once by a human. A false negative is the
//! defect this census exists to prevent, and it is silent. So the burden of proof sits on the
//! entry: `&str` is a narrowing until the signature shows otherwise.
//!
//! Four tests run in order.
//!
//! 1. **A source-generic family applied to a concrete text type** — convict. The set of
//!    "source-generic families" is *derived from the crate*, never listed: any generic type or
//!    trait that is anywhere applied to a type parameter carrying a source bound owns a source
//!    position at that argument index. `validate_executable<S, K>(…) where S: AsRef<[u8]>,
//!    K: Sink<S>` is what makes `Sink`'s first argument a source position, which is what makes
//!    `K: Sink<&'src str>` in `validate_executable_lossless` a narrowing — stated by the crate
//!    about itself.
//! 2. **The parameter's name is in the source lexicon** — convict. This is name evidence and it is
//!    deliberately *conviction-only*: being in the lexicon adds a finding, being absent from it
//!    never removes one. A vocabulary that could acquit would be a hand-maintained list of what to
//!    skip, which is the defect one level up; a vocabulary that can only accuse is a strictly
//!    tightening heuristic that cannot cause a miss.
//! 3. **The source is already in hand generically** — acquit. The receiver, or another parameter,
//!    is a source-generic family applied to a *type parameter* (`&'a ExecutableDocument<S>`,
//!    `Executor<'a, S, V>`, `Cst<'inp, Lx, Em>`) or is a bare source-bound parameter. The document
//!    is then already present in the caller's own representation, so a second, concrete text value
//!    in the same signature is necessarily *about* that document rather than being it: a name to
//!    look up, a message to attach, a key to compare.
//! 4. **Otherwise** — convict.
//!
//! Three things are acquitted before any of that, because the type says so outright:
//!
//! * `&mut str` / `&mut String` / `&mut Vec<u8>` — a mutable borrow is an output buffer, not input.
//! * `&'static str` — an explicitly `'static` borrow cannot be a slice of a caller's document
//!   without the caller leaking it, so it is a compile-time constant. `finish_root`'s
//!   `space: &'static str` is the dialect's own name.
//! * A type mentioning no concrete text type at all.
//!
//! # Worked answers
//!
//! | entry | parameter | test | verdict |
//! |---|---|---|---|
//! | `Executor::start` | `operation: Option<&str>` | 3 — `&mut self` is `Executor<'a, S, V>`, and `Executor`'s first type argument is a source position | **datum** |
//! | `Executor::handle_field_error` | `message: &str` | 3 — same receiver | **datum** |
//! | `validate_executable_lossless` | `source: &'src str` | 1 — `K: Sink<&'src str>`, and `Sink`'s first argument is a source position | **narrowed** |
//! | `validate_schema_lossless` | `source: &str` | 2, then 4 — `&Parse` is a rowan tree and carries no source type, so nothing is in hand | **narrowed** |
//! | `Schema::from_introspection` | `response: &str` | 2, then 4 — an associated function with nothing else in the signature | **narrowed** |
//! | `Schema::sym` | `bytes: &[u8]` | step 1 — a byte view constrains nobody | **neutral** |
//!
//! # Where the rule is known to be weak
//!
//! Test 3 acquits every concrete text parameter on a source-generic receiver, so a hypothetical
//! `Executor::parse_extension(&mut self, sdl: &str)` — a *second* document handed to a type that
//! already holds one — is caught only by test 2, on the name. That is the reason the lexicon
//! exists and the reason it can only convict. A shape that defeats both is a shape the exemption
//! table cannot help with, and it is the one to widen the rule for when it appears.

use std::collections::{BTreeMap, BTreeSet};

use quote::ToTokens as _;
use syn::{
  GenericArgument, GenericParam, Generics, PathArguments, ReturnType, Signature, Type,
  TypeParamBound, WherePredicate,
};

/// Parameter names that say "this is the document", used only to accuse.
///
/// Every entry here is a word the crate already uses for a whole document at a public door:
/// `source` and `src` at the lossless doors, `response` at the introspection doors, `sdl` in
/// `to_sdl`'s own prose. The rest are the words the next such door is likely to use.
pub const SOURCE_LEXICON: &[&str] = &[
  "body",
  "bytes",
  "content",
  "doc",
  "document",
  "input",
  "payload",
  "query",
  "raw",
  "response",
  "schema_source",
  "sdl",
  "source",
  "src",
  "text",
];

/// Parameterised trait bounds that mean "this type parameter is a source representation", when
/// their argument is itself a text type: `AsRef<[u8]>` and `AsRef<str>` qualify, `AsRef<Path>`
/// does not. `CstText` — tokora's, bound at `parser/lossless/runner.rs`'s `Lx::Source` — takes no
/// argument and is recognised on its own.
const SOURCE_BOUNDS: &[&str] = &["AsRef", "Borrow"];

/// What a concrete text type in a signature costs a caller.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Cost {
  /// `&[u8]`, `&BStr` — `S: AsRef<[u8]>` already produces one, so nothing is imposed.
  None,
  /// `&str` — a UTF-8 obligation on top of the byte bound.
  Utf8,
  /// `String`, `Vec<u8>`, `Bytes`, `HipStr`, `SmallVec<[u8; N]>` — one owned representation pinned.
  Representation,
}

/// One concrete text type found inside a parameter's type.
#[derive(Clone, Debug)]
pub struct TextHit {
  pub rendered: String,
  pub cost: Cost,
  /// Behind a `&mut`, so it is an output buffer rather than input.
  pub out_param: bool,
  /// Behind an explicit `&'static`, so it is a compile-time constant.
  pub konstant: bool,
}

/// `family ident -> type-argument indices that stand for a source representation`.
///
/// Derived by reading the whole crate: wherever a generic path is applied to a type parameter that
/// carries a source bound, that argument index is a source position for that family.
pub type SourcePositions = BTreeMap<String, BTreeSet<usize>>;

/// The generic environment a signature is read in: the function's own parameters plus the
/// enclosing `impl` or `trait`'s.
#[derive(Default, Clone)]
pub struct Scope {
  pub all_params: BTreeSet<String>,
  pub source_params: BTreeSet<String>,
}

impl Scope {
  pub fn extend_from(&mut self, generics: &Generics) {
    for param in &generics.params {
      if let GenericParam::Type(t) = param {
        let name = t.ident.to_string();
        self.all_params.insert(name.clone());
        if t.bounds.iter().any(is_source_bound) {
          self.source_params.insert(name);
        }
      }
    }
    for predicate in generics.where_clause.iter().flat_map(|w| &w.predicates) {
      let WherePredicate::Type(pt) = predicate else {
        continue;
      };
      let Some(root) = root_param_of(&pt.bounded_ty) else {
        continue;
      };
      if pt.bounds.iter().any(is_source_bound) {
        // `S: AsRef<[u8]>` marks `S`; `Lx::Source: CstText` marks `Lx`, because a lexer whose
        // source is text is exactly as much "the caller's representation, generically" as the
        // text itself.
        self.source_params.insert(root);
      }
    }
  }
}

fn is_source_bound(bound: &TypeParamBound) -> bool {
  let TypeParamBound::Trait(t) = bound else {
    return false;
  };
  let Some(last) = t.path.segments.last() else {
    return false;
  };
  let name = last.ident.to_string();
  if name == "CstText" {
    return true;
  }
  if !SOURCE_BOUNDS.contains(&name.as_str()) {
    return false;
  }
  // `AsRef<[u8]>` and `AsRef<str>` are source bounds; `AsRef<Path>` is not.
  type_args(&last.arguments)
    .iter()
    .any(|arg| matches!(text_cost(arg), Some(Cost::None | Cost::Utf8)))
}

/// The leading identifier of a bounded type: `S` for `S`, `Lx` for `Lx::Source`.
fn root_param_of(ty: &Type) -> Option<String> {
  let Type::Path(p) = ty else { return None };
  Some(p.path.segments.first()?.ident.to_string())
}

/// The type arguments of a path segment, lifetimes and const arguments skipped.
fn type_args(arguments: &PathArguments) -> Vec<&Type> {
  match arguments {
    PathArguments::AngleBracketed(a) => a
      .args
      .iter()
      .filter_map(|arg| match arg {
        GenericArgument::Type(t) => Some(t),
        _ => None,
      })
      .collect(),
    _ => Vec::new(),
  }
}

/// What one type costs a caller, if it is a concrete text type at all.
///
/// `None` is returned for anything that is not text, and [`Cost::None`] for text that costs
/// nothing — the two are different answers and the caller needs both.
fn text_cost(ty: &Type) -> Option<Cost> {
  match ty {
    Type::Slice(s) => is_u8(&s.elem).then_some(Cost::None),
    Type::Array(a) => is_u8(&a.elem).then_some(Cost::None),
    Type::Path(p) => {
      let last = p.path.segments.last()?;
      let name = last.ident.to_string();
      let args = type_args(&last.arguments);
      match name.as_str() {
        "str" => Some(Cost::Utf8),
        "BStr" => Some(Cost::None),
        "String" | "HipStr" | "SmolStr" | "CompactString" | "BString" | "Bytes" | "BytesMut"
        | "SmolBytes" => Some(Cost::Representation),
        // An owned container of text or of bytes pins one owned representation either way.
        "Cow" | "Box" | "Rc" | "Arc" => args
          .first()
          .and_then(|inner| text_cost(inner))
          .map(|_| Cost::Representation),
        "Vec" | "SmallVec" | "TinyVec" | "ArrayVec" => args
          .first()
          .filter(|inner| is_u8(inner) || matches!(text_cost(inner), Some(Cost::None)))
          .map(|_| Cost::Representation),
        _ => None,
      }
    }
    _ => None,
  }
}

fn is_u8(ty: &Type) -> bool {
  matches!(ty, Type::Path(p) if p.path.is_ident("u8"))
}

/// Does the outermost path segment carry an explicit `'static` lifetime argument?
fn has_static_argument(ty: &Type) -> bool {
  let Type::Path(p) = ty else { return false };
  let Some(last) = p.path.segments.last() else {
    return false;
  };
  let PathArguments::AngleBracketed(args) = &last.arguments else {
    return false;
  };
  args
    .args
    .iter()
    .any(|arg| matches!(arg, GenericArgument::Lifetime(lt) if lt.ident == "static"))
}

/// Every concrete text type inside `ty`, with the borrow context that surrounds it.
pub fn text_hits(ty: &Type) -> Vec<TextHit> {
  fn walk(ty: &Type, out_param: bool, konstant: bool, out: &mut Vec<TextHit>) {
    // A reference is scored as a whole, so the finding reads `&'src str` rather than `str` and so
    // that `&mut` and `'static` reach the type they qualify.
    if let Type::Reference(r) = ty {
      let mutable = out_param || r.mutability.is_some();
      let is_static = konstant || r.lifetime.as_ref().is_some_and(|lt| lt.ident == "static");
      if let Some(cost) = text_cost(&r.elem) {
        out.push(TextHit {
          rendered: render(ty),
          cost,
          out_param: mutable,
          konstant: is_static,
        });
        return;
      }
      walk(&r.elem, mutable, is_static, out);
      return;
    }
    if let Some(cost) = text_cost(ty) {
      out.push(TextHit {
        rendered: render(ty),
        cost,
        out_param,
        // `Cow<'static, str>` says the same thing `&'static str` does — it cannot be a slice of a
        // caller's document without the caller leaking one — so `LexerErrorData::other`'s
        // `impl Into<Cow<'static, str>>` is a message, not a source.
        konstant: konstant || has_static_argument(ty),
      });
      return;
    }
    match ty {
      Type::Paren(p) => walk(&p.elem, out_param, konstant, out),
      Type::Group(g) => walk(&g.elem, out_param, konstant, out),
      Type::Ptr(p) => walk(&p.elem, out_param, konstant, out),
      Type::Tuple(t) => {
        for elem in &t.elems {
          walk(elem, out_param, konstant, out);
        }
      }
      Type::Path(p) => {
        for segment in &p.path.segments {
          for arg in type_args(&segment.arguments) {
            walk(arg, out_param, konstant, out);
          }
          if let PathArguments::Parenthesized(pa) = &segment.arguments {
            for input in &pa.inputs {
              walk(input, out_param, konstant, out);
            }
          }
        }
      }
      Type::ImplTrait(i) => walk_bounds(&i.bounds, out_param, konstant, out),
      Type::TraitObject(t) => walk_bounds(&t.bounds, out_param, konstant, out),
      _ => {}
    }
  }

  fn walk_bounds(
    bounds: &syn::punctuated::Punctuated<TypeParamBound, syn::Token![+]>,
    out_param: bool,
    konstant: bool,
    out: &mut Vec<TextHit>,
  ) {
    for bound in bounds {
      let TypeParamBound::Trait(t) = bound else {
        continue;
      };
      // `impl AsRef<[u8]>` and `impl Fn(&str) -> …` are read for what they mention, but an
      // `AsRef<[u8]>` bound is the representation-agnostic spelling and costs nothing.
      if is_source_bound(bound) {
        continue;
      }
      for segment in &t.path.segments {
        for arg in type_args(&segment.arguments) {
          walk(arg, out_param, konstant, out);
        }
      }
    }
  }

  let mut out = Vec::new();
  walk(ty, false, false, &mut out);
  out
}

pub fn render(ty: &Type) -> String {
  let text = ty.to_token_stream().to_string();
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

/// Records every `family<… T …>` where `T` is a source-bound type parameter.
///
/// `crate_types` restricts this to families the crate under census declares itself. Without it
/// `Option<S>` — written once anywhere for an optional description — would make `(Option, 0)` a
/// source position, and `Executor::start`'s `operation: Option<&str>` would then be convicted by
/// test 1 for no better reason than that `Option` is generic. The crate's own type vocabulary is
/// read out of its own `struct`, `enum`, `trait` and `type` declarations, so nothing here is a
/// list of names to skip.
pub fn harvest_source_positions(
  ty: &Type,
  scope: &Scope,
  crate_types: &BTreeSet<String>,
  out: &mut SourcePositions,
) {
  let Type::Path(p) = ty else {
    walk_children(ty, &mut |child| {
      harvest_source_positions(child, scope, crate_types, out)
    });
    return;
  };
  for segment in &p.path.segments {
    let family = segment.ident.to_string();
    let args = type_args(&segment.arguments);
    for (index, arg) in args.iter().enumerate() {
      if mentions_source_param(arg, scope) && crate_types.contains(&family) {
        out.entry(family.clone()).or_default().insert(index);
      }
      harvest_source_positions(arg, scope, crate_types, out);
    }
  }
}

/// Is `ty` a source-bound type parameter, or a borrow of one?
fn mentions_source_param(ty: &Type, scope: &Scope) -> bool {
  match ty {
    Type::Path(p) => p
      .path
      .get_ident()
      .is_some_and(|id| scope.source_params.contains(&id.to_string())),
    Type::Reference(r) => mentions_source_param(&r.elem, scope),
    Type::Paren(p) => mentions_source_param(&p.elem, scope),
    Type::Group(g) => mentions_source_param(&g.elem, scope),
    _ => false,
  }
}

/// Does `ty` put the caller's source in the entry's hands generically?
///
/// Either a bare source-bound parameter, or any source-generic family applied to a type parameter
/// rather than to a concrete type.
pub fn holds_source_generically(ty: &Type, scope: &Scope, positions: &SourcePositions) -> bool {
  if mentions_source_param(ty, scope) {
    return true;
  }
  let mut held = false;
  if let Type::Path(p) = ty {
    for segment in &p.path.segments {
      let family = segment.ident.to_string();
      let args = type_args(&segment.arguments);
      let source_indices = positions.get(&family);
      for (index, arg) in args.iter().enumerate() {
        let at_source_position = source_indices.is_some_and(|s| s.contains(&index));
        if at_source_position && is_type_parameter(arg, scope) {
          held = true;
        }
        if holds_source_generically(arg, scope, positions) {
          held = true;
        }
      }
    }
  } else {
    walk_children(ty, &mut |child| {
      if holds_source_generically(child, scope, positions) {
        held = true;
      }
    });
  }
  held
}

fn is_type_parameter(ty: &Type, scope: &Scope) -> bool {
  match ty {
    Type::Path(p) => p
      .path
      .get_ident()
      .is_some_and(|id| scope.all_params.contains(&id.to_string())),
    Type::Reference(r) => is_type_parameter(&r.elem, scope),
    Type::Paren(p) => is_type_parameter(&p.elem, scope),
    Type::Group(g) => is_type_parameter(&g.elem, scope),
    _ => false,
  }
}

/// Does `ty` apply a source-generic family to a *concrete text type*?
///
/// Returns the family and the argument, for the finding's explanation.
pub fn family_at_concrete_text(ty: &Type, positions: &SourcePositions) -> Option<(String, String)> {
  if let Type::Path(p) = ty {
    for segment in &p.path.segments {
      let family = segment.ident.to_string();
      let args = type_args(&segment.arguments);
      let source_indices = positions.get(&family);
      for (index, arg) in args.iter().enumerate() {
        if source_indices.is_some_and(|s| s.contains(&index))
          && text_hits(arg)
            .iter()
            .any(|hit| hit.cost != Cost::None && !hit.out_param)
        {
          return Some((family.clone(), render(arg)));
        }
        if let Some(found) = family_at_concrete_text(arg, positions) {
          return Some(found);
        }
      }
    }
    return None;
  }
  let mut found = None;
  walk_children(ty, &mut |child| {
    if found.is_none() {
      found = family_at_concrete_text(child, positions);
    }
  });
  found
}

/// Applies `f` to the immediate type children of `ty`, for the non-path shapes.
fn walk_children(ty: &Type, f: &mut impl FnMut(&Type)) {
  match ty {
    Type::Reference(r) => f(&r.elem),
    Type::Paren(p) => f(&p.elem),
    Type::Group(g) => f(&g.elem),
    Type::Ptr(p) => f(&p.elem),
    Type::Slice(s) => f(&s.elem),
    Type::Array(a) => f(&a.elem),
    Type::Tuple(t) => t.elems.iter().for_each(f),
    Type::ImplTrait(i) => bound_types(&i.bounds, f),
    Type::TraitObject(t) => bound_types(&t.bounds, f),
    _ => {}
  }
}

fn bound_types(
  bounds: &syn::punctuated::Punctuated<TypeParamBound, syn::Token![+]>,
  f: &mut impl FnMut(&Type),
) {
  for bound in bounds {
    let TypeParamBound::Trait(t) = bound else {
      continue;
    };
    for segment in &t.path.segments {
      for arg in type_args(&segment.arguments) {
        f(arg);
      }
    }
  }
}

/// Every type a signature mentions: the receiver, the parameters, the return type, and the types
/// named in its own bounds.
pub fn signature_types(sig: &Signature, self_ty: Option<&Type>) -> Vec<Type> {
  let mut out = Vec::new();
  if let Some(ty) = self_ty {
    out.push(ty.clone());
  }
  for arg in &sig.inputs {
    match arg {
      syn::FnArg::Typed(t) => out.push((*t.ty).clone()),
      syn::FnArg::Receiver(r) => out.push((*r.ty).clone()),
    }
  }
  if let ReturnType::Type(_, ty) = &sig.output {
    out.push((**ty).clone());
  }
  out.extend(bound_type_list(&sig.generics));
  out
}

/// The types named in an item's bounds, as `Type` values — `Sink<&'src str>` from
/// `where K: Sink<&'src str>`.
pub fn bound_type_list(generics: &Generics) -> Vec<Type> {
  let mut out = Vec::new();
  let mut push_bounds = |bounds: &syn::punctuated::Punctuated<TypeParamBound, syn::Token![+]>| {
    for bound in bounds {
      if let TypeParamBound::Trait(t) = bound {
        out.push(Type::Path(syn::TypePath {
          qself: None,
          path: t.path.clone(),
        }));
      }
    }
  };
  for param in &generics.params {
    if let GenericParam::Type(t) = param {
      push_bounds(&t.bounds);
    }
  }
  for predicate in generics.where_clause.iter().flat_map(|w| &w.predicates) {
    if let WherePredicate::Type(pt) = predicate {
      push_bounds(&pt.bounds);
    }
  }
  out
}
