//! A package uses nothing above the tier it declares.
//!
//! A package declares its tier, `core`, `alloc` or `std`, as a crate declares `#![no_std]`. Every
//! name of the prelude resolves whatever the tier, and a use above the tier is refused where it is
//! written, as `smear::graphqlx::tier`, by the package that wrote it, `P`. Nothing is judged in a
//! package declared at `std`. Below it:
//!
//! - a type occurrence `P` writes, wherever it stands, a directive definition's argument types, its
//!   type parameters' defaults and bounds and an application's type arguments included, which is
//!   the letter of the language's decisions until its owner rules otherwise:
//!   - a list needs `alloc`;
//!   - an occurrence of a definition of `P`'s own needs nothing: such a definition rises above
//!     `P`'s tier only through a cause `P` wrote inside it, which is judged where it is written;
//!   - an occurrence of another package's definition `E`, of the package `H`, needs the greater of
//!     `E`'s tier and the tiers the defaults of the type parameters it leaves out need of their own,
//!     a default that names a parameter counting that parameter as `core`, capped at `H`'s
//!     declared tier: what `E` needs past `H`'s tier is `H`'s mistake, reported in `H`, and not
//!     again at each use. When a default is what raises the need, the report has a label at it;
//! - an application of `core`'s `@heap` on a field, a field's argument or an input field, in a
//!   package declared at `core`: `@heap` needs `alloc`. On a directive definition's argument it
//!   raises no tier, and is not judged;
//! - an extern whose tier is above the tier of the package that declares its scalar, which wrote
//!   the binding: the tier is reported at the value of the `@extern`'s first `tier`, which is
//!   written, since an extern that writes none takes its package's tier.
//!
//! So one cause is one report: a `string` field in a `core` package is reported at `string`, and
//! not at the uses of the type that holds it; a dependency's type that needs more than the root
//! package's tier is reported at each use in the root package. A definition of `P` rises above
//! `P`'s tier only along a chain that starts at a list, `@heap`, an extern's written `tier` or a
//! built-in's tag: where the source's tier is above its writer's, the source is reported there,
//! and otherwise the first step of the chain whose writer's tier is below the source's is an
//! occurrence of another package's definition, whose capped need is at least the source's tier,
//! and which is reported. So in an analysis that is ok, every definition's tier is at most its
//! package's.
//!
//! The rule says nothing where a phase refused something in the contribution that holds what it
//! judges, or, for a written root, in one of the schema's. A `use` item is no use: an unused one
//! says nothing.
//!
//! The rule reads the public model, and two crate-private accessors: `Extern::application`, the
//! application an extern is bound from, and `SourceMap::written_by`, the package that wrote what
//! is judged.

use std::{collections::BTreeMap, format, vec, vec::Vec};

use crate::graphqlx::{
  ApplicationArgument, ApplicationId, Cx, DefId, DefKind, Diagnostic, DiagnosticKind, MemberId,
  MemberKind, Model, Origin, Resolution, Tier, TypeExprId, TypeExprKind, TypeParam, Visitor,
  diagnostic::Excerpt,
};

use super::Silence;

/// Reports each use of what needs more than the tier the package that writes it declares: a
/// type occurrence, `@heap`, and an extern's tier.
///
/// It reads an occurrence and its type in the `type_expr` hook; an application in the
/// `application` hook; and a scalar's externs, each with its application and its tier, in the
/// `enter_def` hook. It says nothing about what a phase refused something in the contribution of,
/// or for a root, the schema's, nor in a package declared at `std`; it reports only what a
/// supplied package wrote, which is all the walk reaches.
///
/// It costs one look at each occurrence, application and extern the walk reaches, and below `std`
/// one look at the package that wrote each and at the type of an occurrence of another package's
/// definition; a binary search of the scalar's externs, sorted when the walk enters it, for each
/// application of `@extern`; and once for each definition whose defaults an occurrence may fill,
/// one look at each of its type parameters, kept. It keeps the scalar's externs, one at a time,
/// and those defaults' tiers, each definition's once, until the next walk. Its messages quote a
/// definition's own name and a package's name as excerpts, so each is bounded whatever their
/// lengths.
#[derive(Default)]
pub(super) struct TierViolation {
  /// Whether it says nothing at an occurrence.
  silence: Silence,
  /// `core`'s `@heap`, `@extern`, and `@extern`'s `tier`, found when the walk begins.
  keys: Option<Keys>,
  /// The externs of the scalar the walk is in, each its application and its tier, sorted by the
  /// application.
  externs: Vec<(ApplicationId, Tier)>,
  /// For each definition an occurrence asked about, for each of its type parameters, the greatest
  /// tier the defaults from that one to the last need of their own, and the first default that
  /// needs it.
  defaults: BTreeMap<DefId, Vec<(Tier, Option<TypeExprId>)>>,
}

/// `core`'s `@heap`, its `@extern` and that directive's `tier` argument.
#[derive(Clone, Copy)]
struct Keys {
  heap: DefId,
  host: DefId,
  tier: MemberId,
}

impl Visitor for TierViolation {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    let builtins = model.builtins();
    let host = builtins.r#extern();
    let tier = model
      .def(host)
      .arguments()
      .into_iter()
      .find(|argument: &MemberId| model.member(*argument).name() == "tier");
    self.keys = tier.map(|tier: MemberId| Keys {
      heap: builtins.heap(),
      host,
      tier,
    });
    self.silence = Silence::of(cx);
    self.externs.clear();
    self.defaults.clear();
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    self.externs.clear();
    let definition = cx.model().def(def);
    if definition.kind() != DefKind::Scalar || definition.origin() != Origin::User {
      return;
    }
    for bound in definition.externs() {
      if let Some(application) = bound.application() {
        self.externs.push((application, bound.tier()));
      }
    }
    self
      .externs
      .sort_unstable_by_key(|&(application, _): &(ApplicationId, Tier)| application);
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    if self.silence.silenced(cx) {
      return;
    }
    let model = cx.model();
    let occurrence = model.type_expr(ty);
    let Some(writer) = cx.sources().written_by(occurrence.location()) else {
      return;
    };
    let declared = model.package(writer).tier();
    if declared == Tier::Std {
      return;
    }
    let kind = DiagnosticKind::Tier;
    let package = Excerpt(model.package(writer).name().as_bytes());
    match occurrence.kind() {
      TypeExprKind::List { .. } if declared < Tier::Alloc => cx.report(
        Diagnostic::builder(kind.code(), occurrence.location())
          .severity(kind.severity())
          .message(format!(
            "a list needs `alloc`, and package `{package}` is declared at tier `core`"
          ))
          .primary_label(kind.title())
          .help(format!(
            "declare package `{package}` at tier `alloc` or above, or hold what `core` holds"
          ))
          .build(),
      ),
      TypeExprKind::Named { def, args, .. } => {
        let home = model.module(model.def(def).module()).package();
        if home == writer {
          return;
        }
        let ceiling = model.package(home).tier();
        if ceiling <= declared {
          return;
        }
        let own = model.def(def).tier();
        // The defaults the occurrence leaves out could raise its need only past its definition's
        // own tier and the package's, and only when its whole type needs more than both: a slot's
        // type needs at least what its default needs of its own.
        let whole = model.ty(occurrence.ty()).tier();
        let (filled, default) = if own >= ceiling || whole <= own.max(declared) {
          (Tier::Core, None)
        } else {
          self.filled(model, def, args.len())
        };
        let need = own.max(filled).min(ceiling);
        if need <= declared {
          return;
        }
        let name = Excerpt(model.def(def).name().as_bytes());
        let mut report = Diagnostic::builder(kind.code(), occurrence.location())
          .severity(kind.severity())
          .message(format!(
            "`{name}` needs `{}`, and package `{package}` is declared at tier `{}`",
            spelled(need),
            spelled(declared)
          ))
          .primary_label(kind.title())
          .help(format!(
            "declare package `{package}` at tier `{}` or above, or use what tier `{}` holds",
            spelled(need),
            spelled(declared)
          ));
        if filled > own
          && let Some(default) = default
        {
          report = report.label(
            model.type_expr(default).location(),
            "filled from this default",
          );
        }
        cx.report(report.build());
      }
      _ => {}
    }
  }

  fn application(&mut self, cx: &mut Cx<'_>, application: ApplicationId) {
    let Some(keys) = self.keys else {
      return;
    };
    if self.silence.silenced(cx) {
      return;
    }
    let model = cx.model();
    let applied = model.application(application);
    let Resolution::Resolved(directive) = applied.directive() else {
      return;
    };
    let Some(writer) = cx.sources().written_by(applied.path_location()) else {
      return;
    };
    let declared = model.package(writer).tier();
    let kind = DiagnosticKind::Tier;
    let package = Excerpt(model.package(writer).name().as_bytes());
    if directive == keys.heap {
      // A field's, a field's argument's or an input field's: not a directive definition's
      // argument, which counts nowhere, nor an enum value's.
      let position = cx
        .member()
        .is_some_and(|member: MemberId| model.member(member).kind() != MemberKind::EnumValue)
        && cx
          .def()
          .is_some_and(|def: DefId| model.def(def).kind() != DefKind::Directive);
      if !position || declared >= Tier::Alloc {
        return;
      }
      cx.report(
        Diagnostic::builder(kind.code(), applied.path_location())
          .severity(kind.severity())
          .message(format!(
            "`@heap` keeps this value on the heap, which needs `alloc`, and package `{package}` \
             is declared at tier `core`"
          ))
          .primary_label(kind.title())
          .help(format!(
            "remove `@heap`, or declare package `{package}` at tier `alloc` or above"
          ))
          .build(),
      );
    } else if directive == keys.host {
      let Ok(at) = self
        .externs
        .binary_search_by_key(&application, |&(bound, _): &(ApplicationId, Tier)| bound)
      else {
        return;
      };
      let Some(&(_, tier)) = self.externs.get(at) else {
        return;
      };
      if tier <= declared {
        return;
      }
      let Some(written) = applied
        .arguments()
        .iter()
        .find(|argument: &&ApplicationArgument| argument.argument() == keys.tier)
      else {
        return;
      };
      cx.report(
        Diagnostic::builder(kind.code(), model.value(written.value()).location())
          .severity(kind.severity())
          .message(format!(
            "this `@extern` says its host type needs `{}`, and package `{package}`, which declares \
             the scalar, is declared at tier `{}`",
            spelled(tier),
            spelled(declared)
          ))
          .primary_label(kind.title())
          .help(format!(
            "lower this `tier`, or declare package `{package}` at tier `{}` or above",
            spelled(tier)
          ))
          .build(),
      );
    }
  }
}

impl TierViolation {
  /// The greatest tier the defaults of `def`'s type parameters from the one at `written` on need
  /// of their own, and the first default that needs it: what an occurrence that writes `written`
  /// type arguments fills. Read once for each definition, from its last parameter back, and kept.
  fn filled(&mut self, model: &Model, def: DefId, written: usize) -> (Tier, Option<TypeExprId>) {
    let defaults = self.defaults.entry(def).or_insert_with(|| {
      let params: Vec<Option<TypeExprId>> = model
        .def(def)
        .type_params()
        .map(|param: TypeParam<'_>| param.default())
        .collect();
      let mut from: Vec<(Tier, Option<TypeExprId>)> = vec![(Tier::Core, None); params.len()];
      let mut greatest: (Tier, Option<TypeExprId>) = (Tier::Core, None);
      for (index, default) in params.iter().enumerate().rev() {
        if let Some(default) = *default {
          // The default's own type, its parameters counting as `core`.
          let tier = model.ty(model.type_expr(default).ty()).tier();
          if tier > Tier::Core && tier >= greatest.0 {
            greatest = (tier, Some(default));
          }
        }
        if let Some(slot) = from.get_mut(index) {
          *slot = greatest;
        }
      }
      from
    });
    defaults.get(written).copied().unwrap_or((Tier::Core, None))
  }
}

/// `tier` as a package spells it.
fn spelled(tier: Tier) -> &'static str {
  match tier {
    Tier::Core => "core",
    Tier::Alloc => "alloc",
    Tier::Std => "std",
  }
}
