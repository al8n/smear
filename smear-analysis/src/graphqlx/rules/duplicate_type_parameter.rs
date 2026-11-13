//! A definition's type parameters have distinct names.
//!
//! A type argument binds by position, and what the definition writes names a parameter by its
//! name: of two parameters of one name, the name would name the first alone, and nothing would
//! name the later one. So each type parameter whose name an earlier one of its definition has is
//! reported, at its own name, with a label at the first of that name, as
//! `smear::graphqlx::duplicate-type-parameter`.
//!
//! The parameters are the definition's own contribution's, its first, which writes them, and they
//! are judged when a supplied package wrote that contribution and no phase refused anything in it.
//! The names an extension gives its target's parameters are not the model's, and not judged here.
//!
//! The rule reads the public model and nothing else.

use std::{format, vec::Vec};

use tokora::diagnostic::Location;

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, DiagnosticKind, PackageKind, Sym, TypeParam,
  Visitor, diagnostic::Excerpt,
};

/// Reports each type parameter whose name an earlier one of its definition has.
///
/// It reads a definition's own contribution and its type parameters in the `enter_def` hook. It
/// says nothing about a definition whose own contribution a phase refused something in, nor about
/// a built-in package's definition.
///
/// It costs one look at each definition the walk enters, and one sort of the names of each that
/// declares two type parameters or more, in a buffer it keeps and clears, which holds one
/// definition's parameters at a time. Its message quotes the parameter's and the definition's
/// names as excerpts, so it is bounded whatever their lengths.
#[derive(Default)]
pub(super) struct DuplicateTypeParameter {
  /// The type parameters of the definition entered, each by its name, its index and where its name
  /// is written, sorted by name, then index.
  names: Vec<(Sym, u32, Location)>,
}

impl Visitor for DuplicateTypeParameter {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.names.clear();
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let sources = cx.sources();
    let definition = model.def(def);
    if definition.type_params().len() < 2 {
      return;
    }
    // The contribution that writes the parameters: the definition's own, its first.
    let Some(own) = definition
      .contributions()
      .next()
      .filter(|own: &Contribution<'_>| !own.is_extension() && !own.has_errors())
    else {
      return;
    };
    let Some(writer) = sources.written_by(own.location()) else {
      return;
    };
    if model.package(writer).kind() == PackageKind::BuiltIn {
      return;
    }
    self.names.clear();
    self.names.extend(
      definition
        .type_params()
        .map(|param: TypeParam<'_>| (param.sym(), param.index(), param.location())),
    );
    self
      .names
      .sort_unstable_by_key(|&(name, index, _): &(Sym, u32, Location)| (name, index));
    let sigil = if definition.kind() == DefKind::Directive {
      "@"
    } else {
      ""
    };
    let kind = DiagnosticKind::DuplicateTypeParameter;
    // The first parameter of the run of one name at hand, and where its name is written.
    let mut first: Option<(Sym, Location)> = None;
    for &(name, _, at) in &self.names {
      match first {
        Some((seen, earlier)) if seen == name => cx.report(
          Diagnostic::builder(kind.code(), at)
            .severity(kind.severity())
            .message(format!(
              "the type parameter `{}` of `{sigil}{}` has the name of an earlier one, and a \
               definition's type parameters have distinct names",
              Excerpt(model.name(name).as_bytes()),
              Excerpt(definition.name().as_bytes())
            ))
            .primary_label(kind.title())
            .label(earlier, "the first of that name")
            .help(
              "rename this type parameter: the definition names a type parameter by its name, \
               which names the first of that name alone",
            )
            .build(),
        ),
        _ => first = Some((name, at)),
      }
    }
  }
}
