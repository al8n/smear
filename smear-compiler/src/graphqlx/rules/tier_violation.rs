//! A package uses nothing above the tier it declares.
//!
//! A package declares its tier, `core`, `alloc` or `std`, as a crate declares `#![no_std]`. Every
//! name of the prelude resolves whatever the tier, and a use above the tier is refused where it is
//! written, as `smear::graphqlx::tier`, by the package that wrote it, `P`. Nothing is judged in a
//! package declared at `std`. Below it, outside compile-time metadata:
//!
//! - a type occurrence `P` writes:
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
//! Compile-time metadata counts nowhere, and none of it is judged, unless the analysis runs under
//! `Options::judged_metadata`, which judges it as everything else is judged. It is every type
//! occurrence under a directive definition, its arguments' types and its type parameters' defaults
//! and bounds; every type argument of an application of any directive, the written ones and those
//! its directive's defaults fill, each with every type nested in it; and everything within a
//! definition only compile-time metadata reaches, which `Def::is_metadata` answers: its type
//! occurrences, its `@heap`s and its externs. Such a definition is a scalar, an enum or an input
//! object of a supplied package that a type a type-system directive writes, one that lists no
//! executable location, or an application's type argument names, through lists and every type
//! argument, or that such a definition's own types name in turn, and that no type written anywhere
//! else names, every package's together, while no directive an operation can apply holds one of
//! its own type parameters in an argument's type. So a `core` package may declare a directive with
//! a `string` argument, apply another package's directive with a `string` type argument, and take
//! in a directive's argument an input object that holds a `string`.
//!
//! A definition that a type outside metadata names too, through any type argument and in whatever
//! package, is judged where it is written, like any other. So is what the argument types of an
//! executable directive name, one an operation can apply, whose values arrive at run time; and
//! while such a directive holds one of its own type parameters in an argument's type, every
//! definition only metadata would reach is judged, since an operation can fill that parameter with
//! any of them. The types such a directive writes are exempt all the same, as every directive's
//! are. An object, an interface and a union are never metadata: one can be a runtime type that no
//! type names.
//!
//! Under `Options::judged_metadata` nothing is skipped as metadata: every use above is judged
//! wherever it stands, what a directive definition writes, an application's type arguments and what
//! `Def::is_metadata` marks among it, and so is `@heap` on a directive definition's argument, in a
//! package declared at `core`. And an application `P` writes of another package's generic
//! directive is judged by the type arguments it leaves to its directive's defaults, which need what
//! those defaults need of their own, read off its written count and capped at that package's tier,
//! as an occurrence's are: a directive needs `core` itself, and its written type arguments are
//! occurrences, judged where they are written. It is reported once, at its path, with a label at
//! the default that raises it. `Def::is_metadata` and `Def::tier` answer the same either way: the
//! switch changes what the rule reports, and nothing the model holds but the marks of those
//! reports.
//!
//! A report within a definition both reach has a last label at where the reach from outside
//! metadata entered, "`A` is reached from here, outside compile-time metadata": the occurrence
//! outside metadata whose type names it; for a definition that another such definition holds, that
//! definition's own entry; and for what an executable directive's own type parameter can receive,
//! the type of the argument that holds that parameter, though that type need not name it. So when
//! a field's argument takes `B` and `B` holds `A`, a report in `A` is labelled at `B` in that
//! argument, where the chain starts, and not at `B`'s field.
//!
//! So one cause is one report: a `string` field in a `core` package is reported at `string`, and
//! not at the uses of the type that holds it; a dependency's type that needs more than the root
//! package's tier is reported at each use in the root package. A definition of `P` that is no
//! metadata rises above `P`'s tier only along a chain that starts at a list, `@heap`, an extern's
//! written `tier` or a built-in's tag, every step of which is an occurrence outside metadata: a
//! type written outside metadata that names a definition, through any type argument, makes it no
//! metadata, so every definition on the chain is no metadata, and every occurrence on it is
//! judged. Where the source's tier is above its writer's, the source is reported there, and
//! otherwise the first step of the chain whose writer's tier is below the source's is an
//! occurrence of another package's definition, whose capped need is at least the source's tier,
//! and which is reported. So in an analysis that is ok, every definition's tier is at most its
//! package's, but a metadata definition's, whose own tier feeds no definition that is no metadata;
//! under `Options::judged_metadata` that exception is gone, every occurrence on a chain being
//! judged, a metadata definition's included.
//!
//! The rule says nothing where a phase refused something in the contribution that holds what it
//! judges, or, for a written root, in one of the schema's. A `use` item is no use: an unused one
//! says nothing.
//!
//! The rule reads the public model, the public `SourceMap::written_by` among it, the package that
//! wrote what is judged, the public `Extern::application`, the application an extern is bound
//! from, and the public `Def::is_metadata`. It reads one crate-private accessor,
//! `Def::runtime_entry`: where a type outside compile-time metadata first reaches a definition
//! metadata reaches too, which no public view answers, and which is a candidate for a public one.

use std::{collections::BTreeMap, format, string::String, vec, vec::Vec};

use tokora::diagnostic::Location;

use crate::graphqlx::{
  ApplicationArgument, ApplicationId, Cx, DefId, DefKind, Diagnostic, DiagnosticKind, MemberId,
  MemberKind, Model, NamedTypeExpr, Origin, Resolution, Site, Tier, TypeExprId, TypeExprKind,
  TypeParam, Visitor, diagnostic::Excerpt,
};

use super::Silence;

/// Reports each use of what needs more than the tier the package that writes it declares: a
/// type occurrence, `@heap`, and an extern's tier.
///
/// It reads an occurrence and its type in the `type_expr` hook; an application in the
/// `application` hook; and a scalar's externs, each with its application and its tier, in the
/// `enter_def` hook. It says nothing within compile-time metadata, which counts nowhere: at an
/// application's type argument, nor at anything within a directive definition or a definition
/// only compile-time metadata reaches, one predicate, `exempt`, saying so at each of the three
/// hooks, unless it judges metadata, when it skips none. It says nothing about what a phase
/// refused something in the contribution of, or for a root, the schema's, nor in a package declared
/// at `std`; it reports only what a supplied package wrote, which is all the walk reaches.
///
/// It costs one look at each occurrence, application and extern the walk reaches, and at the
/// definition the walk is in for each; below `std` one look at the package that wrote each and at
/// the type of an occurrence of another package's definition; a binary search of the scalar's
/// externs, sorted when the walk enters it, for each application of `@extern`; and once for each
/// definition whose defaults an occurrence may fill, or, when it judges metadata, an application of
/// another package's directive, one look at each of its type parameters, kept. It keeps the
/// scalar's externs, one at a time, and those defaults' tiers, each definition's once, until the
/// next walk. Its messages quote a definition's own name and a package's name as excerpts, so each
/// is bounded whatever their lengths.
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
  /// Whether compile-time metadata is judged too, as `Options::judged_metadata` says:
  /// configuration, which `begin` leaves alone.
  pub(super) judge_metadata: bool,
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
    let model = cx.model();
    let definition = model.def(def);
    // A metadata scalar's externs are not judged: with none collected, the extern arm finds none.
    if definition.kind() != DefKind::Scalar
      || definition.origin() != Origin::User
      || exempt(model, self.judge_metadata, Some(def), None)
    {
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
    let model = cx.model();
    if exempt(model, self.judge_metadata, cx.def(), cx.site()) {
      return;
    }
    if self.silence.silenced(cx) {
      return;
    }
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
      TypeExprKind::List(_) if declared < Tier::Alloc => {
        let mut report = Diagnostic::builder(kind.code(), occurrence.location())
          .severity(kind.severity())
          .message(format!(
            "a list needs `alloc`, and package `{package}` is declared at tier `core`"
          ))
          .primary_label(kind.title())
          .help(format!(
            "declare package `{package}` at tier `alloc` or above, or hold what `core` holds"
          ));
        if let Some((entry, text)) = entered(model, cx.def()) {
          report = report.label(entry, text);
        }
        cx.report(report.build());
      }
      TypeExprKind::Named(NamedTypeExpr { def, args }) => {
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
        if let Some((entry, text)) = entered(model, cx.def()) {
          report = report.label(entry, text);
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
      // A field's, a field's argument's or an input field's outside compile-time metadata, which
      // counts nowhere: not a directive definition's argument, nor a metadata definition's input
      // field, nor an enum value's. Under `Options::judged_metadata`, metadata is judged too.
      let position = cx
        .member()
        .is_some_and(|member: MemberId| model.member(member).kind() != MemberKind::EnumValue)
        && !exempt(model, self.judge_metadata, cx.def(), cx.site());
      if !position || declared >= Tier::Alloc {
        return;
      }
      let mut report = Diagnostic::builder(kind.code(), applied.path_location())
        .severity(kind.severity())
        .message(format!(
          "`@heap` keeps this value on the heap, which needs `alloc`, and package `{package}` \
           is declared at tier `core`"
        ))
        .primary_label(kind.title())
        .help(format!(
          "remove `@heap`, or declare package `{package}` at tier `alloc` or above"
        ));
      if let Some((entry, text)) = entered(model, cx.def()) {
        report = report.label(entry, text);
      }
      cx.report(report.build());
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
      let mut report = Diagnostic::builder(kind.code(), model.value(written.value()).location())
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
        ));
      if let Some((entry, text)) = entered(model, cx.def()) {
        report = report.label(entry, text);
      }
      cx.report(report.build());
    } else if self.judge_metadata {
      // Any other directive another package defines, under `Options::judged_metadata`: the type
      // arguments the application leaves to its directive's defaults need what those defaults need
      // of their own, read off its written count as an occurrence's are, the directive itself
      // needing `core`. One whose type arguments could not be completed was refused at its path,
      // and its contribution is silent.
      let home = model.module(model.def(directive).module()).package();
      if home == writer {
        return;
      }
      let ceiling = model.package(home).tier();
      if ceiling <= declared {
        return;
      }
      let own = model.def(directive).tier();
      let (filled, default) = self.filled(model, directive, applied.type_args().len());
      let need = own.max(filled).min(ceiling);
      if need <= declared {
        return;
      }
      let name = Excerpt(model.def(directive).name().as_bytes());
      let mut report = Diagnostic::builder(kind.code(), applied.path_location())
        .severity(kind.severity())
        .message(format!(
          "`@{name}`, with the type arguments this application leaves to its defaults, needs \
           `{}`, and package `{package}` is declared at tier `{}`",
          spelled(need),
          spelled(declared)
        ))
        .primary_label(kind.title())
        .help(format!(
          "declare package `{package}` at tier `{}` or above, or write the type arguments with \
           what tier `{}` holds",
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
      if let Some((entry, text)) = entered(model, cx.def()) {
        report = report.label(entry, text);
      }
      cx.report(report.build());
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

/// Whether what the walk is at is compile-time metadata, which the rule does not judge: an
/// application's type argument, every type nested in it sharing its `site`; or anything within a
/// directive definition, or within a definition only compile-time metadata reaches, `def`; unless
/// `judge`, which judges everything.
fn exempt(model: &Model, judge: bool, def: Option<DefId>, site: Option<Site>) -> bool {
  if judge {
    return false;
  }
  site == Some(Site::ApplicationTypeArgument)
    || def.is_some_and(|def: DefId| {
      let definition = model.def(def);
      definition.kind() == DefKind::Directive || definition.is_metadata()
    })
}

/// Where a type outside compile-time metadata first reaches `def`, the definition the walk is in,
/// with the text of the label there, when compile-time metadata reaches it too: the last label of
/// each report within it. `None` for every other definition, and outside every definition.
fn entered(model: &Model, def: Option<DefId>) -> Option<(Location, String)> {
  let definition = model.def(def?);
  let at = definition.runtime_entry()?;
  let name = Excerpt(definition.name().as_bytes());
  Some((
    at,
    format!("`{name}` is reached from here, outside compile-time metadata"),
  ))
}

/// `tier` as a package spells it.
fn spelled(tier: Tier) -> &'static str {
  match tier {
    Tier::Core => "core",
    Tier::Alloc => "alloc",
    Tier::Std => "std",
  }
}
