//! A built-in scalar takes no `@specifiedBy`.
//!
//! `@specifiedBy` gives a scalar the address of the document that specifies it, and GraphQL refuses
//! it on a built-in scalar, which its specification specifies itself, whether a definition or an
//! extension applies it. Here a built-in scalar is every scalar of the built-in library: `core`'s
//! primitives, `NonZero` and `TypeRef`, and `alloc`'s and `std`'s scalars, each of which the
//! language specifies as GraphQL's specification does its five; and a GraphQL scalar the root
//! package replaces, which keeps its built-in's identity. Each application of `core`'s
//! `@specifiedBy` on one is reported at its path, under the code the same rule has in GraphQL:
//! `smear-schema`'s `specified-by-on-built-in-scalar`, with its label and its help, whose words
//! for a scalar that is none of GraphQL's five say the language where GraphQL's say its
//! specification.
//!
//! A scalar is told by its definition, and the directive by the definition its application
//! resolved to, never by a spelling: `scalar Int` in a module that is no root module is a
//! package's own scalar.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  ApplicationId, Contribution, Cx, DefKind, Diagnostic, Origin, Primitive, Resolution, Visitor,
  code::schema, diagnostic::Excerpt,
};

/// Reports each application of `core`'s `@specifiedBy` on a scalar of the built-in library.
///
/// It reads each application, and the definition it stands on, in the `application` hook. It says
/// nothing about an application whose contribution has its `has_errors()` set: a phase already
/// refused something in it, and a second diagnostic within it would repeat the phase's.
///
/// It costs, for each application, a few comparisons, and for one it reports up to five more,
/// which tell GraphQL's own scalars by their definitions; it keeps nothing between two walks. Its
/// messages quote the scalar's name as an excerpt, so each is bounded whatever the name's length.
#[derive(Default)]
pub(super) struct SpecifiedByOnBuiltInScalar;

impl Visitor for SpecifiedByOnBuiltInScalar {
  fn application(&mut self, cx: &mut Cx<'_>, application: ApplicationId) {
    // An application on a member, or on the schema, stands on no scalar.
    if cx.member().is_some() {
      return;
    }
    let Some(def) = cx.def() else {
      return;
    };
    if cx
      .contribution()
      .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    let model = cx.model();
    let builtins = model.builtins();
    let applied = model.application(application);
    if !matches!(
      applied.directive(),
      Resolution::Resolved(directive) if directive == builtins.specified_by()
    ) {
      return;
    }
    let scalar = model.def(def);
    if scalar.kind() != DefKind::Scalar || scalar.origin() != Origin::BuiltIn {
      return;
    }
    // GraphQL's own five, each told by its definition.
    let one_of_the_five = match scalar.intrinsic() {
      Some(primitive) => {
        primitive == Primitive::signed(32)
          || primitive == Primitive::F64
          || primitive == Primitive::Bool
      }
      None => def == builtins.string() || def == builtins.id(),
    };
    let help = if one_of_the_five {
      "a built-in scalar is specified by the GraphQL specification itself, so it takes no \
       specification URL; drop the `@specifiedBy`."
    } else {
      "a built-in scalar is specified by the language itself, so it takes no specification URL; \
       drop the `@specifiedBy`."
    };
    cx.report(
      Diagnostic::builder(
        schema::SPECIFIED_BY_ON_BUILT_IN_SCALAR,
        applied.path_location(),
      )
      .message(format!(
        "`@specifiedBy` is applied to the built-in scalar `{}`, which takes no specification URL",
        Excerpt(scalar.name().as_bytes())
      ))
      .primary_label("@specifiedBy is applied to a built-in scalar")
      .help(help)
      .build(),
    );
  }
}
