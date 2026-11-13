//! `NonZero`'s argument is an integer primitive.
//!
//! `core`'s `NonZero<T>` is an integer that is never zero, so its one type argument is an integer
//! primitive, `iN` or `uN`, at least one bit wide, and nullable: the integer itself, which
//! `NonZero` makes never zero, while whether a value may be null is the `!` written after
//! `NonZero<…>`. An occurrence that names `NonZero`, under a name a `use` gives it or as an alias
//! such as `NonZeroU32`, is judged by its type, which instantiation gave it: `NonZero` of anything
//! else is reported where it is written, with a label at its type argument when it writes one. A
//! float, `string`, an object, a list, `u0`, which holds zero alone, a type parameter, which no
//! bound makes an integer primitive, and an integer written with `!` are each
//! `smear::graphqlx::nonzero-argument`. An alias spells an integer at least one bit wide, and is
//! never reported.
//!
//! A `NonZero` this rule refuses satisfies no `MapKey`, and the bounds pass leaves a map or a set
//! keyed by one to this rule, so that the mistake is reported once.
//!
//! The rule reads the public model and nothing else.

use crate::graphqlx::{
  Cx, Diagnostic, DiagnosticKind, Primitive, TyKind, TypeExprId, TypeExprKind, Visitor,
};

use super::Silence;

/// What the rule tells the author of `NonZero` of what is no integer primitive.
const INTEGER_HELP: &str = "write `NonZero` of an integer primitive, `iN` or `uN` with `N` at \
                            least 1, as `NonZero<u32>` and its second name `NonZeroU32` do";

/// What the rule tells the author of `NonZero` of an integer written with `!`.
const NULLABLE_HELP: &str = "drop the `!` after the type argument: `NonZero<u32>` is never zero, \
                             and `NonZero<u32>!` is never null either";

/// Reports each occurrence of `core`'s `NonZero` whose type argument is no nullable integer
/// primitive at least one bit wide.
///
/// It reads an occurrence and its type in the `type_expr` hook. It says nothing about an
/// occurrence whose type is unresolved, nor about one whose contribution, or for a root the
/// schema's, a phase refused something in. It reports only what a supplied package wrote: the walk
/// reaches nothing else.
///
/// It costs one look at each occurrence the walk reaches, and one at the type of each that names
/// `NonZero`, and one look at the schema's contributions for the whole walk; it keeps a flag
/// between two occurrences, and nothing between two walks. Its message is static text.
#[derive(Default)]
pub(super) struct NonZeroArgument {
  /// Whether it says nothing at an occurrence.
  silence: Silence,
}

impl Visitor for NonZeroArgument {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    self.silence = Silence::of(cx);
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    let model = cx.model();
    let occurrence = model.type_expr(ty);
    let non_zero = model.builtins().non_zero();
    let TypeExprKind::Named {
      def: named,
      args: written_args,
    } = occurrence.kind()
    else {
      return;
    };
    if named != non_zero || self.silence.silenced(cx) {
      return;
    }
    // `NonZero` of its one argument, unless the occurrence has the unresolved type.
    let argument = match model.ty(occurrence.ty()).kind() {
      TyKind::Named {
        def,
        args: &[argument],
      } if def == non_zero => model.ty(argument),
      _ => return,
    };
    let integer = match argument.kind() {
      TyKind::Named { def, args } => {
        args.is_empty()
          && matches!(
            model.def(def).intrinsic(),
            Some(Primitive::Int { width, .. }) if width >= 1
          )
      }
      _ => false,
    };
    if integer && !argument.non_null() {
      return;
    }
    let (label, help) = if integer {
      ("written with `!`", NULLABLE_HELP)
    } else {
      ("no integer primitive at least one bit wide", INTEGER_HELP)
    };
    let kind = DiagnosticKind::NonZeroArgument;
    let mut report = Diagnostic::builder(kind.code(), occurrence.location())
      .severity(kind.severity())
      .message(
        "`NonZero` takes an integer primitive at least one bit wide, written without `!`, as its \
         type argument, and this one is not",
      )
      .primary_label(kind.title())
      .help(help);
    if let Some(&written) = written_args.first() {
      report = report.label(model.type_expr(written).location(), label);
    }
    cx.report(report.build());
  }
}
