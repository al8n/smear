//! An application passes each argument once.
//!
//! GraphQL refuses a directive application that writes one argument twice. An application keeps
//! the arguments its directive declares in the directive's order, an argument written twice twice
//! and in written order, so the uses of one argument are neighbours: each after the first of its
//! run is reported at its name, with a label at the first, under the code the same rule has in
//! GraphQL: `smear-schema`'s `duplicate-directive-argument-use`, with its labels.
//!
//! An argument the directive does not declare is lowering's, which reports each one written and
//! keeps none, so one written twice is two of lowering's diagnostics and none of this rule's. An
//! argument a default fills has no name written, and is never one of a run.
//!
//! The rule reads the public model and nothing else.

use std::format;

use tokora::diagnostic::Location;

use crate::graphqlx::{
  ApplicationId, Contribution, Cx, Diagnostic, MemberId, Resolution, Visitor, code::schema,
  diagnostic::Excerpt,
};

/// Reports each argument an application writes again.
///
/// It reads an application's arguments in the `application` hook. It says nothing about an
/// application whose contribution has its `has_errors()` set: a phase already refused something
/// in it, and a second diagnostic within it would repeat the phase's.
///
/// It costs one look at each argument of each application, and keeps nothing between two walks.
/// Its messages quote the directive's and the argument's names as excerpts, so each is bounded
/// whatever the names' lengths.
#[derive(Default)]
pub(super) struct DuplicateDirectiveArgumentUse;

impl Visitor for DuplicateDirectiveArgumentUse {
  fn application(&mut self, cx: &mut Cx<'_>, application: ApplicationId) {
    if cx
      .contribution()
      .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    let model = cx.model();
    let applied = model.application(application);
    let Resolution::Resolved(directive) = applied.directive() else {
      return;
    };
    // The argument of the run the arguments have reached, and where its first use names it.
    let mut run: Option<(MemberId, Location)> = None;
    for entry in applied.arguments() {
      let Some(at) = entry.name_location() else {
        continue;
      };
      if let Some((argument, first)) = run
        && argument == entry.argument()
      {
        cx.report(
          Diagnostic::builder(schema::DUPLICATE_DIRECTIVE_ARGUMENT_USE, at)
            .message(format!(
              "this application of `@{}` passes the argument `{}` again",
              Excerpt(model.def(directive).name().as_bytes()),
              Excerpt(model.member(argument).name().as_bytes())
            ))
            .primary_label("directive argument is passed twice")
            .label(first, "first passed here")
            .build(),
        );
      } else {
        run = Some((entry.argument(), at));
      }
    }
  }
}
