//! An input object takes `@oneOf` in its definition alone.
//!
//! `@oneOf` changes what every field of an input object means, the fields its definition wrote
//! among them, so GraphQL refuses it where an input object type extension provides it. Each
//! application of `core`'s `@oneOf` in an extension of an input object is reported at its path,
//! under the code the same rule has in GraphQL: `smear-schema`'s
//! `one-of-on-input-object-extension`, with its label and its help. The input object is a OneOf
//! input object all the same when the extension is its own package's, as in `smear-schema`: its
//! fields are judged as one's. Another package's extension is an orphan, and its `@oneOf` makes
//! nothing OneOf.
//!
//! The directive is told by the definition its application resolved to, never by how it is
//! spelled. An extension of another kind of definition is not this rule's: `@oneOf` stands on an
//! input object alone, which the rule on directive locations says.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  Cx, DefId, DefKind, Diagnostic, Resolution, Visitor, code::schema, diagnostic::Excerpt,
};

/// Reports each application of `core`'s `@oneOf` in an extension of an input object.
///
/// It reads an input object's contributions and each extension's applications in the `enter_def`
/// hook. It says nothing about an extension whose `has_errors()` is set: a phase already refused
/// something in it, and a second diagnostic within it would repeat the phase's.
///
/// It costs, for each input object the walk enters, one look at each application of each of its
/// extensions, and keeps nothing between two walks. Its messages quote the input object's name as
/// an excerpt, so each is bounded whatever the name's length.
#[derive(Default)]
pub(super) struct OneOfOnInputObjectExtension;

impl Visitor for OneOfOnInputObjectExtension {
  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let definition = model.def(def);
    if definition.kind() != DefKind::InputObject {
      return;
    }
    let one_of = model.builtins().one_of();
    for part in definition.contributions() {
      if !part.is_extension() || part.has_errors() {
        continue;
      }
      for id in part.applications() {
        let application = model.application(*id);
        if !matches!(
          application.directive(),
          Resolution::Resolved(directive) if directive == one_of
        ) {
          continue;
        }
        cx.report(
          Diagnostic::builder(
            schema::ONE_OF_ON_INPUT_OBJECT_EXTENSION,
            application.path_location(),
          )
          .message(format!(
            "this extension of the input object `{}` applies `@oneOf`, which belongs on its \
             definition",
            Excerpt(definition.name().as_bytes())
          ))
          .primary_label("@oneOf is provided by an input object type extension")
          .help(
            "`@oneOf` changes what every existing field of the type means, so it belongs on the \
             definition.",
          )
          .build(),
        );
      }
    }
  }
}
