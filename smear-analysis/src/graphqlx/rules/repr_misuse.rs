//! `@repr` writes `for` on the schema alone, and chooses one host type a language at each place.
//!
//! `core`'s `@repr` chooses the host type of one language: on a field, a field's argument, a
//! directive definition's argument or an input field, that position's own; on the schema, the
//! default for every position of the type its `for` names. The model keeps it as its applications,
//! with their typed arguments, and records the choice without interpreting it. So:
//!
//! - an `@repr` at a position that writes `for`, whatever its value, `null` included, is reported
//!   at `for`'s name: `for` is written on the schema alone;
//! - an `@repr` on the schema whose `for` is absent or `null` is reported at its path: on the
//!   schema, `for` names the type it chooses for;
//! - a position's `@repr`s choose once for each `lang`: each whose `lang` an earlier one of the
//!   position has, the cooked strings compared bytewise, is reported at its path, with a label at
//!   the earlier one's. A field and its argument are two positions;
//! - the schema's `@repr`s choose once for each type and `lang`: each whose `for` and `lang` an
//!   earlier one has is reported at its path, with a label at the earlier one's. `for` is told by
//!   the definition it names, so `String` and `string` are one type.
//!
//! Each is `smear::graphqlx::repr-misuse`. `@repr` and its arguments are found by the definition
//! the application resolved to, and by their names among the library's own arguments. What `for`
//! names is not judged: any definition a type value names. Nothing else is: an `@repr` on a
//! definition or an enum value stands where `@repr` may not, which is not this rule's to say; and
//! a `for` or a `lang` that did not coerce, or did not resolve, was reported where it is written,
//! within a contribution this rule then says nothing about.
//!
//! The rule reads the public model and nothing else.

use std::{format, vec::Vec};

use smear_semantics::duplicates::Duplicates;

use crate::graphqlx::{
  ApplicationArgument, ApplicationId, Contribution, Cx, DefId, Diagnostic, DiagnosticKind,
  MemberId, MemberKind, Model, Resolution, ValueKind, Visitor, diagnostic::Excerpt,
};

/// Reports each `@repr` that writes `for` at a position, each on the schema whose `for` is absent
/// or `null`, each whose `lang` an earlier `@repr` of its position has, and each whose `for` and
/// `lang` an earlier `@repr` of the schema has.
///
/// It reads a member's applications in the `enter_member` hook, and the schema's contributions and
/// their applications in the `schema` hook. It says nothing about a member's applications when a
/// phase refused something in the contribution the walk reaches it in, nor about an application of
/// a schema contribution a phase refused something in; the earlier `@repr` of a pair is the first
/// whether or not a phase refused something where it is written.
///
/// It costs one look at each member the walk enters and at each application it holds, one at each
/// application of the schema, and for each of those that resolved to `@repr` one at each of its
/// arguments; and one search by the shared kernel for each member and for the schema, a scan up to
/// sixty-four and one sort above, in a list it makes for the one position at hand. It keeps
/// `@repr`'s identity between two hooks, and nothing between two walks. Its messages quote a `lang`
/// and a type's name as excerpts, so each is bounded whatever their lengths.
#[derive(Default)]
pub(super) struct ReprMisuse {
  /// `core`'s `@repr` and its arguments, found when the walk begins.
  keys: Option<Keys>,
}

/// `core`'s `@repr`, and its `lang` and `for` arguments.
#[derive(Clone, Copy)]
struct Keys {
  repr: DefId,
  lang: MemberId,
  target: MemberId,
}

impl Visitor for ReprMisuse {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    let repr = model.builtins().repr();
    let named = |name: &str| -> Option<MemberId> {
      model
        .def(repr)
        .arguments()
        .into_iter()
        .find(|argument: &MemberId| model.member(*argument).name() == name)
    };
    self.keys = match (named("lang"), named("for")) {
      (Some(lang), Some(target)) => Some(Keys { repr, lang, target }),
      _ => None,
    };
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    let Some(keys) = self.keys else {
      return;
    };
    let model = cx.model();
    let position = model.member(member);
    if position.kind() == MemberKind::EnumValue {
      return;
    }
    if cx
      .contribution()
      .is_none_or(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    let kind = DiagnosticKind::ReprMisuse;
    // Each `@repr` whose `lang` is a string, with that string, in application order.
    let mut langs: Vec<(&str, ApplicationId)> = Vec::new();
    for &application in position.applications() {
      let applied = model.application(application);
      if applied.directive() != Resolution::Resolved(keys.repr) {
        continue;
      }
      let arguments = applied.arguments();
      if let Some(written) = first(arguments, keys.target)
        .and_then(|argument: &ApplicationArgument| argument.name_location())
      {
        cx.report(
          Diagnostic::builder(kind.code(), written)
            .severity(kind.severity())
            .message(
              "`for` is written on the schema alone: on a field, an argument or an input field, \
               `@repr` chooses that position's own host type",
            )
            .primary_label(kind.title())
            .help(
              "remove `for` to choose this position's host type, or choose it for a type with \
               `extend schema @repr(for: …, lang: …, type: …)`",
            )
            .build(),
        );
      }
      if let Some(lang) = string_of(model, arguments, keys.lang) {
        langs.push((lang, application));
      }
    }
    let mut duplicates: Duplicates<&str> =
      Duplicates::over(langs.len(), |at: usize| lang_at(&langs, at));
    for (at, &(lang, application)) in langs.iter().enumerate() {
      let Some(earlier) = duplicates
        .first(at, lang)
        .and_then(|earlier: usize| langs.get(earlier))
      else {
        continue;
      };
      cx.report(
        Diagnostic::builder(kind.code(), model.application(application).path_location())
          .severity(kind.severity())
          .message(format!(
            "this position chooses its host type in `{}` twice",
            Excerpt(lang.as_bytes())
          ))
          .primary_label(kind.title())
          .label(
            model.application(earlier.1).path_location(),
            "first chosen in this language here",
          )
          .help("write one `@repr` for each language at a position")
          .build(),
      );
    }
  }

  fn schema(&mut self, cx: &mut Cx<'_>) {
    let Some(keys) = self.keys else {
      return;
    };
    let model = cx.model();
    let kind = DiagnosticKind::ReprMisuse;
    // Each `@repr` whose `for` names a type and whose `lang` is a string, with the two, its
    // application, and whether a phase refused something in its contribution, in order.
    let mut pairs: Vec<((DefId, &str), ApplicationId, bool)> = Vec::new();
    for part in model.schema().contributions() {
      for &application in part.applications() {
        let applied = model.application(application);
        if applied.directive() != Resolution::Resolved(keys.repr) {
          continue;
        }
        let arguments = applied.arguments();
        let target = first(arguments, keys.target)
          .map(|argument: &ApplicationArgument| model.value(argument.value()).kind());
        match target {
          None | Some(ValueKind::Null) => {
            if part.has_errors() {
              continue;
            }
            cx.report(
              Diagnostic::builder(kind.code(), applied.path_location())
                .severity(kind.severity())
                .message(
                  "`@repr` on the schema names, with `for`, the type it chooses a default host \
                   type for, and this one names none",
                )
                .primary_label(kind.title())
                .help("name the type with `for`, as `@repr(for: …, lang: …, type: …)` does")
                .build(),
            );
          }
          Some(ValueKind::Type { def, .. }) => {
            if let Some(lang) = string_of(model, arguments, keys.lang) {
              pairs.push(((def, lang), application, part.has_errors()));
            }
          }
          Some(_) => {}
        }
      }
    }
    let mut duplicates: Duplicates<(DefId, &str)> =
      Duplicates::over(pairs.len(), |at: usize| pair_at(&pairs, at));
    for (at, &((def, lang), application, refused)) in pairs.iter().enumerate() {
      // Every position is asked, in order, so that the kernel's scan and its sort agree.
      let Some(earlier) = duplicates
        .first(at, (def, lang))
        .and_then(|earlier: usize| pairs.get(earlier))
      else {
        continue;
      };
      if refused {
        continue;
      }
      cx.report(
        Diagnostic::builder(kind.code(), model.application(application).path_location())
          .severity(kind.severity())
          .message(format!(
            "the schema chooses the host type of `{}` in `{}` twice",
            Excerpt(model.def(def).name().as_bytes()),
            Excerpt(lang.as_bytes())
          ))
          .primary_label(kind.title())
          .label(
            model.application(earlier.1).path_location(),
            "first chosen for this type in this language here",
          )
          .help("write one `@repr` on the schema for each type and language")
          .build(),
      );
    }
  }
}

/// The first of `arguments` that sets `member`.
fn first(arguments: &[ApplicationArgument], member: MemberId) -> Option<&ApplicationArgument> {
  arguments
    .iter()
    .find(|argument: &&ApplicationArgument| argument.argument() == member)
}

/// The string the first of `arguments` that sets `member` holds, when it is one.
fn string_of<'a>(
  model: &'a Model,
  arguments: &[ApplicationArgument],
  member: MemberId,
) -> Option<&'a str> {
  match model.value(first(arguments, member)?.value()).kind() {
    ValueKind::String(text) => Some(text),
    _ => None,
  }
}

/// The `lang` at `at` of `langs`, or `""` past its end.
fn lang_at<'k>(langs: &[(&'k str, ApplicationId)], at: usize) -> &'k str {
  langs
    .get(at)
    .map_or("", |entry: &(&'k str, ApplicationId)| entry.0)
}

/// The pair at `at` of `pairs`, or one of no `lang` past its end.
fn pair_at<'k>(pairs: &[((DefId, &'k str), ApplicationId, bool)], at: usize) -> (DefId, &'k str) {
  pairs.get(at).map_or(
    (DefId::new(0), ""),
    |entry: &((DefId, &'k str), ApplicationId, bool)| entry.0,
  )
}
