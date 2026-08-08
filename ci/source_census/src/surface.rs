//! The crate's public surface, read out of the source rather than written down.
//!
//! Two things are derived here and nowhere else in this tool: which modules a consumer can name,
//! and which functions inside them a consumer can call. Both have to come from the code. A list of
//! entries to check is the same defect the census exists to catch, one level up — it agrees with
//! the crate on the day it is written and goes stale in silence afterwards (al8n/smear#122).
//!
//! # What counts as reachable
//!
//! A module is reachable when every `mod` on the path from the crate root to it is `pub`. A
//! `pub(crate)`, `pub(super)` or `pub(in …)` module is not: nothing outside the crate can name it,
//! so a `&str` inside constrains nobody.
//!
//! `mod m; pub use m::{A, B};` — the shape `validator/mod.rs` and `schema/mod.rs` both use — makes
//! `A` and `B` public while `m` stays private, so a private module's items are read too, filtered
//! by what the re-export names. A glob re-export takes the module whole.
//!
//! An inherent `impl` contributes its `pub fn`s whenever the type it is on is publicly nameable,
//! wherever that `impl` block happens to live: Rust does not care that `impl Schema` for a
//! re-exported `Schema` sits in a private module.
//!
//! # What is deliberately not read
//!
//! * `#[cfg(test)]` items. Not API.
//! * `#[doc(hidden)]` items. That attribute is the crate's own statement that something is not
//!   API, and it is how the `test-support` coverage harnesses under `parser::*::lossless` are
//!   spelled. Reading the attribute rather than naming those modules keeps this derived.
//! * Items produced by a `macro_rules!` expansion. `syn` sees the macro's tokens, not the items
//!   they become, so a public entry that only exists after expansion is invisible here. That is a
//!   real bound on the census and it is reported on every run rather than left to be discovered.
//! * Trait `impl` blocks. A trait impl cannot choose its own signature — the `trait` declaration
//!   does, and that declaration is read.

use std::{
  collections::{BTreeMap, BTreeSet},
  path::{Path, PathBuf},
};

use quote::ToTokens as _;
use syn::{Item, UseTree, Visibility};

/// One module of the crate under census, with the items written in it.
pub struct Module {
  /// `["smear", "validator", "lossless"]`.
  pub path: Vec<String>,
  /// The file the module's body was read from. Inline modules share their parent's.
  pub file: PathBuf,
  pub items: Vec<Item>,
  /// Every `mod` from the crate root to here is `pub`, and none carries `#[doc(hidden)]`.
  pub named_by_path: bool,
}

impl Module {
  pub fn display_path(&self) -> String {
    self.path.join("::")
  }
}

/// One `pub use`, kept as the edge it is so a path can be followed across it.
///
/// [`Surface::item_is_public`] only ever needs to know *whether* an item is reachable, and the two
/// sets below answer that. Spelling a type in generated code needs the other half — *by what path*
/// — and a re-export chain has to be walked for it: `SchemaError` is declared in
/// `validator::schema::error`, re-exported by `validator::schema`, and a consumer writes neither.
#[derive(Clone)]
pub struct Reexport {
  /// The module the `pub use` is written in.
  pub at: Vec<String>,
  /// The name it publishes under, which `as` can make differ from the target's own.
  pub published: String,
  /// The absolute path of what it names; for a glob, the module taken whole.
  pub target: Vec<String>,
  pub glob: bool,
  /// The target is in another crate, so nothing in this one declares it.
  ///
  /// It is recorded rather than dropped because a trait can be *defined* elsewhere and reach a
  /// consumer through this crate's name — al8n/tokora#240 does exactly that to `Diagnose` — and a
  /// walk that only starts from declarations would then find no path to it at all.
  pub foreign: bool,
}

/// Everything the census needs to know about who can call what.
pub struct Surface {
  pub modules: Vec<Module>,
  /// Fully-qualified paths of items a `pub use` in a reachable module makes public.
  exported_items: BTreeSet<Vec<String>>,
  /// Modules taken whole by a `pub use m::*` in a reachable module.
  glob_exported: BTreeSet<Vec<String>>,
  /// Every `pub use` in the crate, reachable or not — a chain runs through private modules.
  pub reexports: Vec<Reexport>,
  /// Whether every `mod` from the crate root to this one is `pub`, by module path.
  reachable: BTreeMap<Vec<String>, bool>,
  /// Idents of publicly nameable types, for matching inherent `impl` blocks against.
  pub public_types: BTreeSet<String>,
  /// Files read, and item-position macro invocations found in them.
  ///
  /// The second is the census's blind spot, counted so that it is stated on every run rather than
  /// assumed away: `syn` sees a macro's tokens, not the items they expand to, so a public entry
  /// that exists only after expansion is not censused. Measured on this crate the expansions are
  /// either `pub(crate)` (`lossless_production!`) or `#[cfg(feature = "test-support")]
  /// #[doc(hidden)]` (`lossless_drivers!`), so nothing public hides there today — but "today" is
  /// exactly the kind of claim this tool exists to stop trusting, so the count is printed.
  pub files_read: usize,
  pub macro_invocations: usize,
}

/// Reads the crate rooted at `lib_rs` into a [`Surface`].
pub fn load(lib_rs: &Path, crate_name: &str) -> Result<Surface, String> {
  let mut modules = Vec::new();
  let mut macro_invocations = 0usize;
  let children_dir = lib_rs
    .parent()
    .ok_or_else(|| format!("{} has no parent directory", lib_rs.display()))?
    .to_path_buf();

  read_module(
    lib_rs,
    &children_dir,
    vec![crate_name.to_string()],
    true,
    &mut modules,
    &mut macro_invocations,
  )?;

  let mut surface = Surface {
    files_read: modules
      .iter()
      .map(|m| m.file.clone())
      .collect::<BTreeSet<_>>()
      .len(),
    reachable: modules
      .iter()
      .map(|m| (m.path.clone(), m.named_by_path))
      .collect(),
    modules,
    exported_items: BTreeSet::new(),
    glob_exported: BTreeSet::new(),
    reexports: Vec::new(),
    public_types: BTreeSet::new(),
    macro_invocations,
  };
  surface.resolve_reexports();
  surface.collect_public_types();
  Ok(surface)
}

/// Parses one file and reads it as a module.
fn read_module(
  file: &Path,
  children_dir: &Path,
  path: Vec<String>,
  named_by_path: bool,
  out: &mut Vec<Module>,
  macro_invocations: &mut usize,
) -> Result<(), String> {
  let text = std::fs::read_to_string(file).map_err(|e| format!("{}: {e}", file.display()))?;
  let parsed =
    syn::parse_file(&text).map_err(|e| format!("{}: not parseable Rust: {e}", file.display()))?;
  read_items(
    parsed.items,
    file,
    children_dir,
    path,
    named_by_path,
    out,
    macro_invocations,
  )
}

/// Records one module's own items and descends into the `mod`s it declares, inline or not.
///
/// Written over items rather than over files so that an inline `mod` is the same case as a file
/// one at any depth: its children resolve under `<children_dir>/<name>/`, and its own `mod`s
/// recurse here again.
fn read_items(
  items: Vec<Item>,
  file: &Path,
  children_dir: &Path,
  path: Vec<String>,
  named_by_path: bool,
  out: &mut Vec<Module>,
  macro_invocations: &mut usize,
) -> Result<(), String> {
  let mut own = Vec::new();
  let mut mods: Vec<syn::ItemMod> = Vec::new();

  for item in items {
    if is_cfg_test(attrs_of(&item)) {
      continue;
    }
    match item {
      Item::Mod(m) => mods.push(m),
      // An item-position macro INVOCATION carries no ident; a `macro_rules!` definition does.
      Item::Macro(ref m) if m.ident.is_none() => {
        *macro_invocations += 1;
        own.push(item);
      }
      other => own.push(other),
    }
  }

  out.push(Module {
    path: path.clone(),
    file: file.to_path_buf(),
    items: own,
    named_by_path,
  });

  for m in mods {
    let name = m.ident.to_string();
    let child_public =
      named_by_path && matches!(m.vis, Visibility::Public(_)) && !is_doc_hidden(&m.attrs);
    let mut child_path = path.clone();
    child_path.push(name.clone());

    match m.content {
      Some((_, inner)) => read_items(
        inner,
        file,
        &children_dir.join(&name),
        child_path,
        child_public,
        out,
        macro_invocations,
      )?,
      None => {
        let (child_file, child_children_dir) = resolve_module_file(children_dir, &m)?;
        read_module(
          &child_file,
          &child_children_dir,
          child_path,
          child_public,
          out,
          macro_invocations,
        )?;
      }
    }
  }

  Ok(())
}

/// `mod m;` in a module whose children live in `dir` — `dir/m.rs` or `dir/m/mod.rs`, or whatever
/// `#[path = "…"]` says.
fn resolve_module_file(dir: &Path, m: &syn::ItemMod) -> Result<(PathBuf, PathBuf), String> {
  if let Some(explicit) = path_attr(&m.attrs) {
    let file = dir.join(&explicit);
    let children = file
      .parent()
      .map(Path::to_path_buf)
      .unwrap_or_else(|| dir.to_path_buf());
    return Ok((file, children));
  }
  let name = m.ident.to_string();
  let flat = dir.join(format!("{name}.rs"));
  if flat.is_file() {
    return Ok((flat, dir.join(&name)));
  }
  let nested = dir.join(&name).join("mod.rs");
  if nested.is_file() {
    return Ok((nested, dir.join(&name)));
  }
  Err(format!(
    "`mod {name};` resolves to neither {} nor {} — the census cannot read a module it cannot \
     find, and skipping it would hide whatever is inside",
    flat.display(),
    nested.display()
  ))
}

impl Surface {
  /// Walks every `pub use` in the crate, recording the edge and what it makes public.
  ///
  /// The two sets are built from reachable modules only, because that is what publicity means.
  /// The edge list is built from all of them, because a chain of re-exports out of a private
  /// module is how most of this crate's public names arrive.
  fn resolve_reexports(&mut self) {
    let known: BTreeSet<Vec<String>> = self.modules.iter().map(|m| m.path.clone()).collect();
    let mut items = BTreeSet::new();
    let mut globs = BTreeSet::new();
    let mut edges = Vec::new();

    for module in &self.modules {
      for item in &module.items {
        let Item::Use(u) = item else { continue };
        if !matches!(u.vis, Visibility::Public(_)) {
          continue;
        }
        let mut leaves = Vec::new();
        walk_use(&u.tree, &mut Vec::new(), &mut leaves);
        for (prefix, leaf) in leaves {
          let inside = resolve_path(&prefix, &module.path, &known);
          let base = inside.clone().unwrap_or_else(|| prefix.clone());
          match leaf {
            Leaf::Glob => {
              edges.push(Reexport {
                at: module.path.clone(),
                published: String::new(),
                target: base.clone(),
                glob: true,
                foreign: inside.is_none(),
              });
              if module.named_by_path && inside.is_some() {
                globs.insert(base);
              }
            }
            Leaf::Name { target, published } => {
              let mut full = base;
              full.push(target);
              edges.push(Reexport {
                at: module.path.clone(),
                published,
                target: full.clone(),
                glob: false,
                foreign: inside.is_none(),
              });
              if module.named_by_path && inside.is_some() {
                items.insert(full);
              }
            }
          }
        }
      }
    }

    self.exported_items = items;
    self.glob_exported = globs;
    self.reexports = edges;
  }

  /// Every path a consumer can write to name the item declared as `module::ident`.
  ///
  /// Empty when there is none, which is the answer for a private item. Shortest first, then
  /// alphabetical, so a caller that wants "the" path takes the first and gets the same one on
  /// every run.
  pub fn public_paths(&self, module: &[String], ident: &str) -> Vec<Vec<String>> {
    let mut declared = module.to_vec();
    declared.push(ident.to_string());
    self.walk_paths(vec![declared])
  }

  /// Every path a consumer can write for `name`, counting a re-export of another crate's item.
  ///
  /// [`public_paths`](Self::public_paths) walks out from a declaration this crate makes, which is
  /// the wrong starting point for a trait that has moved out of the crate and comes back through
  /// `pub use tokora::diagnostic::Diagnose`. Both spellings have to resolve, so both walks exist
  /// and the caller takes the union.
  pub fn published_paths(&self, name: &str) -> Vec<Vec<String>> {
    let seeds = self
      .reexports
      .iter()
      .filter(|edge| !edge.glob && edge.published == name)
      .map(|edge| {
        let mut path = edge.at.clone();
        path.push(edge.published.clone());
        path
      })
      .collect();
    self.walk_paths(seeds)
  }

  /// Follows `pub use` edges out of `seeds` and keeps the paths a consumer can name.
  fn walk_paths(&self, seeds: Vec<Vec<String>>) -> Vec<Vec<String>> {
    // Every path that names the item, publishable or not: a private module's alias is a step on
    // the way to a public one, so it has to stay in the walk even though it is not an answer.
    let mut seen: BTreeSet<Vec<String>> = BTreeSet::new();
    let mut frontier = Vec::new();
    for seed in seeds {
      if seen.insert(seed.clone()) {
        frontier.push(seed);
      }
    }

    while let Some(path) = frontier.pop() {
      let Some((leaf, parent)) = path.split_last() else {
        continue;
      };
      for edge in &self.reexports {
        // A foreign edge is a starting point, never a step: its target names another crate, so
        // matching a path against it would be matching against a namespace this walk is not in.
        if edge.foreign {
          continue;
        }
        let candidate = if edge.glob {
          if edge.target != parent {
            continue;
          }
          let mut out = edge.at.clone();
          out.push(leaf.clone());
          out
        } else {
          if edge.target != path {
            continue;
          }
          let mut out = edge.at.clone();
          out.push(edge.published.clone());
          out
        };
        if seen.insert(candidate.clone()) {
          frontier.push(candidate);
        }
      }
    }

    let mut out: Vec<Vec<String>> = seen
      .into_iter()
      .filter(|path| {
        path
          .split_last()
          .is_some_and(|(_, parent)| self.reachable.get(parent).copied().unwrap_or(false))
      })
      .collect();
    out.sort_by_key(|path| (path.len(), path.join("::")));
    out
  }

  /// Idents of every publicly nameable type, so an inherent `impl` can be matched to one.
  fn collect_public_types(&mut self) {
    let mut types = BTreeSet::new();
    for module in &self.modules {
      for item in &module.items {
        let Some((ident, vis)) = type_decl(item) else {
          continue;
        };
        if !matches!(vis, Visibility::Public(_)) || is_doc_hidden(attrs_of(item)) {
          continue;
        }
        if self.item_is_public(&module.path, &ident, module.named_by_path) {
          types.insert(ident);
        }
      }
    }
    // A `pub use` can also name a type that lives behind a private module and is not declared
    // `pub` in a module this loop reaches, so take the re-exported leaf names as well. Over-
    // matching here is harmless: a re-exported `const MAX_SYMBOLS` lands in the set and no `impl`
    // block ever names it, so nothing extra is read.
    for path in &self.exported_items {
      if let Some(last) = path.last()
        && last.starts_with(|c: char| c.is_ascii_uppercase())
      {
        types.insert(last.clone());
      }
    }
    self.public_types = types;
  }

  /// Can a consumer name `module::name`?
  pub fn item_is_public(&self, module: &[String], name: &str, named_by_path: bool) -> bool {
    if named_by_path {
      return true;
    }
    if self.glob_exported.contains(module) {
      return true;
    }
    let mut full = module.to_vec();
    full.push(name.to_string());
    self.exported_items.contains(&full)
  }
}

enum Leaf {
  /// `target` is the item's own name and `published` the one `as` gave it, equal without one.
  Name {
    target: String,
    published: String,
  },
  Glob,
}

/// Flattens a `use` tree into `(prefix, leaf)` pairs.
fn walk_use(tree: &UseTree, prefix: &mut Vec<String>, out: &mut Vec<(Vec<String>, Leaf)>) {
  match tree {
    UseTree::Path(p) => {
      prefix.push(p.ident.to_string());
      walk_use(&p.tree, prefix, out);
      prefix.pop();
    }
    UseTree::Name(n) => out.push((
      prefix.clone(),
      Leaf::Name {
        target: n.ident.to_string(),
        published: n.ident.to_string(),
      },
    )),
    UseTree::Rename(r) => out.push((
      prefix.clone(),
      Leaf::Name {
        target: r.ident.to_string(),
        published: r.rename.to_string(),
      },
    )),
    UseTree::Glob(_) => out.push((prefix.clone(), Leaf::Glob)),
    UseTree::Group(g) => {
      for tree in &g.items {
        walk_use(tree, prefix, out);
      }
    }
  }
}

/// Turns a `use` prefix into an absolute module path, or `None` when it leaves the crate.
fn resolve_path(
  prefix: &[String],
  here: &[String],
  known: &BTreeSet<Vec<String>>,
) -> Option<Vec<String>> {
  let crate_name = here.first()?;
  let mut out: Vec<String>;
  let rest: &[String];

  match prefix.first().map(String::as_str) {
    None => return Some(here.to_vec()),
    Some("crate") => {
      out = vec![crate_name.clone()];
      rest = &prefix[1..];
    }
    Some("self") => {
      out = here.to_vec();
      rest = &prefix[1..];
    }
    Some("super") => {
      out = here.to_vec();
      out.pop()?;
      rest = &prefix[1..];
    }
    Some(first) => {
      let mut candidate = here.to_vec();
      candidate.push(first.to_string());
      if !known.contains(&candidate) {
        // Another crate, or a re-export of one. Nothing here to census.
        return None;
      }
      out = candidate;
      rest = &prefix[1..];
    }
  }

  for segment in rest {
    if segment == "super" {
      out.pop()?;
    } else {
      out.push(segment.clone());
    }
  }
  Some(out)
}

pub fn attrs_of(item: &Item) -> &[syn::Attribute] {
  match item {
    Item::Const(i) => &i.attrs,
    Item::Enum(i) => &i.attrs,
    Item::ExternCrate(i) => &i.attrs,
    Item::Fn(i) => &i.attrs,
    Item::ForeignMod(i) => &i.attrs,
    Item::Impl(i) => &i.attrs,
    Item::Macro(i) => &i.attrs,
    Item::Mod(i) => &i.attrs,
    Item::Static(i) => &i.attrs,
    Item::Struct(i) => &i.attrs,
    Item::Trait(i) => &i.attrs,
    Item::TraitAlias(i) => &i.attrs,
    Item::Type(i) => &i.attrs,
    Item::Union(i) => &i.attrs,
    Item::Use(i) => &i.attrs,
    _ => &[],
  }
}

fn type_decl(item: &Item) -> Option<(String, &Visibility)> {
  match item {
    Item::Struct(i) => Some((i.ident.to_string(), &i.vis)),
    Item::Enum(i) => Some((i.ident.to_string(), &i.vis)),
    Item::Union(i) => Some((i.ident.to_string(), &i.vis)),
    Item::Type(i) => Some((i.ident.to_string(), &i.vis)),
    Item::Trait(i) => Some((i.ident.to_string(), &i.vis)),
    _ => None,
  }
}

pub fn is_doc_hidden(attrs: &[syn::Attribute]) -> bool {
  attrs.iter().any(|a| {
    if !a.path().is_ident("doc") {
      return false;
    }
    let mut hidden = false;
    let _ = a.parse_nested_meta(|meta| {
      if meta.path.is_ident("hidden") {
        hidden = true;
      }
      Ok(())
    });
    hidden
  })
}

/// `#[cfg(test)]` and `#[cfg(all(test, …))]`, which are not API under any feature set.
///
/// The bare identifier is what is looked for, so `feature = "test-support"` — a string literal —
/// cannot be mistaken for it. That distinction is load-bearing: `test-support` items ARE compiled
/// into a shipped build when a consumer turns the feature on, and they are excluded by
/// `#[doc(hidden)]` instead, which is what the crate itself uses to say they are not API.
fn is_cfg_test(attrs: &[syn::Attribute]) -> bool {
  fn mentions_test(tokens: proc_macro2::TokenStream) -> bool {
    tokens.into_iter().any(|tt| match tt {
      proc_macro2::TokenTree::Ident(id) => id == "test",
      proc_macro2::TokenTree::Group(g) => mentions_test(g.stream()),
      _ => false,
    })
  }
  attrs
    .iter()
    .any(|a| a.path().is_ident("cfg") && mentions_test(a.meta.to_token_stream()))
}

fn path_attr(attrs: &[syn::Attribute]) -> Option<String> {
  for a in attrs {
    if !a.path().is_ident("path") {
      continue;
    }
    if let syn::Meta::NameValue(nv) = &a.meta
      && let syn::Expr::Lit(syn::ExprLit {
        lit: syn::Lit::Str(s),
        ..
      }) = &nv.value
    {
      return Some(s.value());
    }
  }
  None
}

/// A `pub fn` template inside a `macro_rules!` body, with a concrete text parameter.
///
/// The census's one real blind spot: `syn` sees a macro's tokens, not the items they become, so a
/// public entry that exists only after expansion is not read by anything above. Rather than record
/// that as a caveat and move on, the templates themselves are scanned — a `pub fn` written inside
/// a `macro_rules!` with a `&str` parameter narrows every entry it expands to, and it is visible
/// in the tokens whether or not the expansion is.
pub struct MacroTemplate {
  pub macro_name: String,
  pub file: String,
  pub line: usize,
  pub param: String,
  pub rendered: String,
  /// The parameter is `&'static str`, so it is a compile-time constant like any other.
  pub konstant: bool,
  /// The macro body carries `doc(hidden)`, so what it expands to is not API.
  pub doc_hidden: bool,
}

/// Reads every `macro_rules!` in the crate for `pub fn` templates taking concrete text.
pub fn macro_templates(surface: &Surface) -> Vec<MacroTemplate> {
  let mut out = Vec::new();
  for module in &surface.modules {
    for item in &module.items {
      let Item::Macro(m) = item else { continue };
      let Some(name) = &m.ident else { continue };
      let tokens: Vec<proc_macro2::TokenTree> = flatten(m.mac.tokens.clone());
      let doc_hidden = mentions_ident(&tokens, "doc") && mentions_ident(&tokens, "hidden");
      for (param, rendered, konstant, line) in text_params_of_pub_fns(&tokens) {
        out.push(MacroTemplate {
          macro_name: name.to_string(),
          file: module.file.display().to_string(),
          line,
          param,
          rendered,
          konstant,
          doc_hidden,
        });
      }
    }
  }
  out
}

/// Every token in the stream, groups included, in source order.
fn flatten(stream: proc_macro2::TokenStream) -> Vec<proc_macro2::TokenTree> {
  let mut out = Vec::new();
  for tt in stream {
    if let proc_macro2::TokenTree::Group(g) = &tt {
      out.push(tt.clone());
      out.extend(flatten(g.stream()));
    } else {
      out.push(tt);
    }
  }
  out
}

fn mentions_ident(tokens: &[proc_macro2::TokenTree], want: &str) -> bool {
  tokens
    .iter()
    .any(|tt| matches!(tt, proc_macro2::TokenTree::Ident(id) if id == want))
}

/// `(parameter name, rendered type, is `'static`, line)` for each `pub fn(… : &str …)` template.
///
/// `pub` IMMEDIATELY followed by `fn` is what makes this public-only: `pub(crate) fn` puts a
/// parenthesised group between the two.
fn text_params_of_pub_fns(tokens: &[proc_macro2::TokenTree]) -> Vec<(String, String, bool, usize)> {
  use proc_macro2::{Delimiter, TokenTree};
  let mut out = Vec::new();
  for index in 0..tokens.len().saturating_sub(1) {
    let (TokenTree::Ident(first), TokenTree::Ident(second)) = (&tokens[index], &tokens[index + 1])
    else {
      continue;
    };
    if first != "pub" || second != "fn" {
      continue;
    }
    let params = tokens[index + 2..].iter().find_map(|tt| match tt {
      TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => Some(g.clone()),
      _ => None,
    });
    let Some(params) = params else { continue };
    for chunk in split_top_level(params.stream()) {
      let mentions_text = chunk
        .iter()
        .any(|tt| matches!(tt, TokenTree::Ident(id) if id == "str" || id == "String"));
      if !mentions_text {
        continue;
      }
      let konstant = chunk
        .iter()
        .any(|tt| matches!(tt, TokenTree::Ident(id) if id == "static"));
      let name = match chunk.first() {
        Some(TokenTree::Ident(id)) => id.to_string(),
        _ => "<pattern>".to_string(),
      };
      let rendered = tidy(
        &chunk
          .iter()
          .map(std::string::ToString::to_string)
          .collect::<Vec<_>>()
          .join(" "),
      );
      out.push((name, rendered, konstant, params.span().start().line));
    }
  }
  out
}

/// Undoes the whitespace `TokenTree::to_string` inserts, so a template reads like Rust.
fn tidy(text: &str) -> String {
  text
    .replace(" : ", ": ")
    .replace("& ", "&")
    .replace("' ", "'")
    .replace(" ::", "::")
    .replace(":: ", "::")
    .replace(" <", "<")
    .replace("< ", "<")
    .replace(" >", ">")
    .replace(" ,", ",")
}

/// Splits a token stream on top-level commas.
fn split_top_level(stream: proc_macro2::TokenStream) -> Vec<Vec<proc_macro2::TokenTree>> {
  let mut out = vec![Vec::new()];
  for tt in stream {
    if matches!(&tt, proc_macro2::TokenTree::Punct(p) if p.as_char() == ',') {
      out.push(Vec::new());
    } else {
      out.last_mut().expect("always one open chunk").push(tt);
    }
  }
  out.retain(|chunk| !chunk.is_empty());
  out
}
