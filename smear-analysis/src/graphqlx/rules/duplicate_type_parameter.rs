//! A definition's type parameters have distinct names, and so do the names an extension's own list
//! gives them.
//!
//! A type argument binds by position, and what the definition writes names a parameter by its
//! name: of two parameters of one name, the name would name the first alone, and nothing would
//! name the later one. An extension's own list names its target's parameters by position, and
//! what the extension writes names them by those names, so the same holds of it. So each type
//! parameter whose name an earlier one of its list has is reported, at its own name, with a label
//! at the first of that name, as `smear::graphqlx::duplicate-type-parameter`: in the definition's
//! own list, and in each extension's.
//!
//! The definition's own list is its own contribution's, its first, and is judged when a supplied
//! package wrote that contribution and no phase refused anything in it. An extension's list is
//! judged when no phase refused anything in that extension: a list of another count than the
//! definition's is lowering's `generic-arity`, and nothing more is said of it here.
//!
//! The rule reads the public model, and one crate-private accessor, `Def::extension_params`: the
//! names an extension's own list writes, which the public model does not hold.

use std::{format, string::String, vec::Vec};

use tokora::diagnostic::Location;

use crate::graphqlx::{
  Cx, Def, DefId, DefKind, Diagnostic, DiagnosticKind, PackageId, PackageKind, Sym, TypeParam,
  Visitor, diagnostic::Excerpt,
};

use super::own_contribution;

/// Reports each type parameter whose name an earlier one of its list has: in a definition's own
/// list, and in each extension's.
///
/// It reads a definition's own contribution and its type parameters, and each extension and the
/// names its own list writes, in the `enter_def` hook. It says nothing about a list in a
/// contribution a phase refused something in, nor about the own list of a built-in package's
/// definition: an extension a supplied package writes of a built-in is judged as any other.
///
/// It costs one look at each definition the walk enters and at each of its contributions, and one
/// sort of each list of two names or more, in a buffer it keeps and clears, which holds one list
/// at a time. Its message quotes the parameter's and the definition's names as excerpts, so it is
/// bounded whatever their lengths.
#[derive(Default)]
pub(super) struct DuplicateTypeParameter {
  /// The names of the list at hand, each with its index in the list and where it is written,
  /// sorted by name, then index.
  names: Vec<(Sym, u32, Location)>,
}

/// Which list the names at hand are.
#[derive(Clone, Copy)]
enum List {
  /// The definition's own type parameters.
  Own,
  /// The names an extension's own list gives them.
  Extension,
}

impl Visitor for DuplicateTypeParameter {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.names.clear();
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let sources = cx.sources();
    let definition = model.def(def);
    // The definition's own list, which its own contribution writes.
    if definition.type_params().len() >= 2
      && let Some((own, writer)) = own_contribution(cx, def)
      && !own.has_errors()
      && model.package(writer).kind() != PackageKind::BuiltIn
    {
      self.names.clear();
      self.names.extend(
        definition
          .type_params()
          .map(|param: TypeParam<'_>| (param.sym(), param.index(), param.location())),
      );
      self.report(cx, definition, List::Own);
    }
    // Each extension's own list, in the order of the contributions.
    for (position, part) in (0..).zip(definition.contributions()) {
      if !part.is_extension() || part.has_errors() {
        continue;
      }
      let written = definition.extension_params(position);
      if written.len() < 2 {
        continue;
      }
      let supplied = sources
        .written_by(part.location())
        .is_some_and(|writer: PackageId| model.package(writer).kind() != PackageKind::BuiltIn);
      if !supplied {
        continue;
      }
      self.names.clear();
      self.names.extend(
        (0..)
          .zip(written)
          .map(|(index, &(name, at)): (u32, &(Sym, Location))| (name, index, at)),
      );
      self.report(cx, definition, List::Extension);
    }
  }
}

impl DuplicateTypeParameter {
  /// Sorts the names at hand, `list` of `definition`, and reports each whose name an earlier one
  /// has, labelled at the first of that name.
  fn report(&mut self, cx: &mut Cx<'_>, definition: Def<'_>, list: List) {
    let model = cx.model();
    self
      .names
      .sort_unstable_by_key(|&(name, index, _): &(Sym, u32, Location)| (name, index));
    let sigil = if definition.kind() == DefKind::Directive {
      "@"
    } else {
      ""
    };
    let kind = DiagnosticKind::DuplicateTypeParameter;
    // The first name of the run of one name at hand, and where it is written.
    let mut first: Option<(Sym, Location)> = None;
    for &(name, _, at) in &self.names {
      let earlier = match first {
        Some((seen, earlier)) if seen == name => earlier,
        _ => {
          first = Some((name, at));
          continue;
        }
      };
      let spelled = Excerpt(model.name(name).as_bytes());
      let owner = Excerpt(definition.name().as_bytes());
      let (message, help): (String, &'static str) = match list {
        List::Own => (
          format!(
            "the type parameter `{spelled}` of `{sigil}{owner}` has the name of an earlier one, \
             and a definition's type parameters have distinct names"
          ),
          "rename this type parameter: the definition names a type parameter by its name, which \
           names the first of that name alone",
        ),
        List::Extension => (
          format!(
            "the type parameter `{spelled}` this extension of `{sigil}{owner}` names has the \
             name of an earlier one of its list, and an extension's list gives its target's type \
             parameters distinct names"
          ),
          "rename this type parameter: the extension names a type parameter by its name, which \
           names the first of that name alone",
        ),
      };
      cx.report(
        Diagnostic::builder(kind.code(), at)
          .severity(kind.severity())
          .message(message)
          .primary_label(kind.title())
          .label(earlier, "the first of that name")
          .help(help)
          .build(),
      );
    }
  }
}
