//! Derived instantiations, design Z's D11: an instantiation's entries and members, and the
//! arguments it makes required, at each instantiation the closure coercion built derives, reported
//! at the written occurrence that derives it.
//!
//! Each instantiation the closure derives whose definition does not grow is judged as an
//! occurrence's type is, by the same verdicts, kept by type and paid from the phase's budget: its
//! distinct written entries and members compared under its arguments, when its definition is
//! listed; and the arguments its definition's fields add beside their interfaces' fields, typed by
//! its own bare nullable type parameter, which it makes required where its argument for that
//! parameter is non-null. Each occurrence of a root a supplied package writes, and each application
//! of one, in id order, whose own verdict holds, reports the failing instantiation of least index
//! its expansion reaches, as the closure's blame finds it: two entries, or two members, of one type
//! there, `duplicate-implements-interface` or `duplicate-union-member`, at most one of each,
//! labelled at the type or the application that derives the instantiation and at the two; an
//! argument made required, `unexpected-required-argument`, labelled there, at the argument and at
//! the interface's field; or a stop, `resource-limit`. A derived instantiation of a definition that
//! grows is not judged, nor is what only it derives. The report pass reads each occurrence and
//! application once, finding its root by its type, whose nullable form is found once for each type
//! and kept: so repeated occurrences of a wide type cost a lookup each.

use std::{collections::BTreeMap, format, string::String, vec::Vec};

use tokora::diagnostic::Location;

use super::{Comparison, Implementing, Latent, List, Made, Twin, Verdict, labelled};
use crate::graphqlx::{
  code::schema,
  diagnostic::{Diagnostic, Excerpt},
  id::{ApplicationId, DefId, TyId, TypeExprId},
  model::{Applied, DefKind, Occurrence, Resolution, Shape, Written},
  set::ROOT,
  types::closure::{Blame, Closure, DERIVES, Place},
};

/// What each node of the closure reaches that fails, for each of the phase's two checks.
struct Blamed {
  /// Two entries or two members of one type, or a stop.
  met: Vec<Option<Blame>>,
  /// An argument made required, or a stop.
  made: Vec<Option<Blame>>,
}

impl Implementing<'_> {
  /// Judges each instantiation the closure derives, and reports, at each occurrence and each
  /// application of a root whose own verdict holds, the failing instantiation its expansion reaches
  /// first. The closure is the type services' no longer: nothing after this phase reads it.
  pub(super) fn derived(&mut self) {
    let closure = core::mem::take(&mut self.types.closure);
    if closure.is_empty() {
      return;
    }
    let mut meeting: Vec<u32> = Vec::new();
    let mut making: Vec<u32> = Vec::new();
    for at in 0..closure.len() {
      let Some(node) = closure.node(at) else {
        break;
      };
      if node.written || node.grows {
        continue;
      }
      if let Some(listed) = self.listed(node.def)
        && !matches!(
          self.verdict(node.ty, node.def, listed),
          Verdict::Compared(Comparison {
            entries: None,
            members: None
          })
        )
      {
        meeting.push(at);
      }
      if !matches!(self.made_by(node.ty, node.def), Made::Nothing) {
        making.push(at);
      }
    }
    if meeting.is_empty() && making.is_empty() {
      return;
    }
    let blamed = Blamed {
      met: closure.blame(&meeting),
      made: closure.blame(&making),
    };
    let supplied = self.sources.package_sources(ROOT).start();
    // The root of each type an occurrence or an application has, by that type, either nullness.
    let mut roots: BTreeMap<TyId, Option<u32>> = BTreeMap::new();
    for index in 0..self.builder.type_expr_count() {
      let Some((written, ty, at)) = self
        .builder
        .occurrence(TypeExprId::new(index))
        .map(|occurrence: Occurrence<'_>| (occurrence.written, occurrence.ty, occurrence.location))
      else {
        continue;
      };
      let (Written::Named(def), Some(ty)) = (written, ty) else {
        continue;
      };
      if at.source() >= supplied {
        self.report_derived(&closure, &blamed, &mut roots, at, def, ty);
      }
    }
    for index in 0..self.builder.application_count() {
      let application = ApplicationId::new(index);
      let (
        Some(Applied {
          directive: Resolution::Resolved(def),
          at,
          ..
        }),
        Some(ty),
      ) = (
        self.builder.applied(application),
        self.builder.application_ty(application),
      )
      else {
        continue;
      };
      if at.source() >= supplied {
        self.report_derived(&closure, &blamed, &mut roots, at, def, ty);
      }
    }
  }

  /// What `ty`, an instantiation of `def`, makes required of the arguments `def`'s fields add
  /// beside their interfaces' fields: kept by the type, made the first time it is asked, as an
  /// occurrence's is.
  fn made_by(&mut self, ty: TyId, def: DefId) -> Made {
    let from = self
      .latent
      .partition_point(|latent: &Latent| latent.def < def);
    let to = self
      .latent
      .partition_point(|latent: &Latent| latent.def <= def);
    if from == to {
      return Made::Nothing;
    }
    if let Some(&known) = self.made.get(&ty) {
      return known;
    }
    let found = self.make(ty, from, to);
    self.made.insert(ty, found);
    found
  }

  /// Reports at `at`, an occurrence or an application of `ty`, an instantiation of `user`, what its
  /// expansion reaches first that fails, for each check whose own verdict at `ty` holds, when `ty`
  /// is a closed named type of `user` and a root of `closure`: found through `roots`, which keeps
  /// each type's root the first time it is asked.
  fn report_derived(
    &mut self,
    closure: &Closure,
    blamed: &Blamed,
    roots: &mut BTreeMap<TyId, Option<u32>>,
    at: Location,
    user: DefId,
    ty: TyId,
  ) {
    if self.builder.type_param_count(user) == 0
      || !self.builder.ty_closed(ty)
      || !matches!(self.builder.ty_shape(ty), Some(Shape::Named(named)) if named == user)
    {
      return;
    }
    let found = if let Some(&known) = roots.get(&ty) {
      known
    } else {
      let nullable = if self.builder.ty_non_null(ty) {
        self
          .builder
          .find_named(user, self.builder.ty_args(ty), false)
          .unwrap_or(ty)
      } else {
        ty
      };
      let found = closure.find(nullable);
      roots.insert(ty, found);
      found
    };
    let Some(root) = found else {
      return;
    };
    // Its own verdicts, made where it is written: one that fails was reported there.
    let compared = self.verdicts.get(&ty).is_none_or(|verdict: &Verdict| {
      matches!(
        verdict,
        Verdict::Compared(Comparison {
          entries: None,
          members: None
        })
      )
    });
    if compared && let Some(&Some(blame)) = blamed.met.get(root as usize) {
      self.report_derived_meeting(closure, at, user, blame);
    }
    let required = self
      .made
      .get(&ty)
      .is_some_and(|made: &Made| !matches!(made, Made::Nothing));
    if !required && let Some(&Some(blame)) = blamed.made.get(root as usize) {
      self.report_derived_made(closure, at, user, blame);
    }
  }

  /// How a report at a use of an instantiation of `def` begins: a type instantiated there, or a
  /// directive applied there.
  fn used(&self, def: DefId) -> String {
    let name = Excerpt(self.builder.def_name(def));
    if self.builder.def_kind(def) == Some(DefKind::Directive) {
      format!("`@{name}` is applied here")
    } else {
      format!("`{name}` is instantiated here")
    }
  }

  /// Where `place` is written, when it is somewhere a label can point.
  fn place_location(&self, place: Option<Place>) -> Option<Location> {
    let at = match place? {
      Place::Type(written) => self.located(written)?,
      Place::Application(application) => self.builder.applied(application)?.at,
    };
    (!at.is_entire()).then_some(at)
  }

  /// Reports at `at`, a use of an instantiation of `user`, the instantiation `blame` names, whose
  /// entries or members meet under its arguments, or whose comparison a limit stopped.
  fn report_derived_meeting(&mut self, closure: &Closure, at: Location, user: DefId, blame: Blame) {
    let Some(failed) = closure.node(blame.node) else {
      return;
    };
    match self.verdicts.get(&failed.ty).copied() {
      Some(Verdict::Compared(Comparison { entries, members })) => {
        let derives = self.place_location(blame.place);
        if let Some(twin) = entries {
          self.report_derived_twin(at, user, failed.def, derives, List::Implements, twin);
        }
        if let Some(twin) = members {
          self.report_derived_twin(at, user, failed.def, derives, List::Members, twin);
        }
      }
      Some(Verdict::Stopped(stop)) => {
        let subject = format!(
          "the entries and members of an instantiation of `{}` that `{}` derives here",
          Excerpt(self.builder.def_name(failed.def)),
          Excerpt(self.builder.def_name(user))
        );
        self.report_limit(at, &subject, stop);
      }
      None => {}
    }
  }

  /// Reports at `at`, a use of an instantiation of `user` that derives an instantiation of `def`
  /// at `derives`, that `twin`, two items of `def`'s list `list`, are one type under its arguments.
  fn report_derived_twin(
    &mut self,
    at: Location,
    user: DefId,
    def: DefId,
    derives: Option<Location>,
    list: List,
    twin: Twin,
  ) {
    let (Some(first), Some(later)) = (self.located(twin.earlier), self.located(twin.later)) else {
      return;
    };
    let used = self.used(user);
    let named = Excerpt(self.builder.def_name(def));
    let (code, message, title, labels) = match list {
      List::Implements => (
        schema::DUPLICATE_IMPLEMENTS_INTERFACE,
        format!(
          "{used} with type arguments that derive an instantiation of `{named}` under which two \
           of its `implements` entries are one interface type"
        ),
        "duplicate interface",
        ("this entry", "the same interface type here"),
      ),
      List::Members => (
        schema::DUPLICATE_UNION_MEMBER,
        format!(
          "{used} with type arguments that derive an instantiation of `{named}` under which two \
           of its members are one type"
        ),
        "duplicate union member",
        ("this member", "the same type here"),
      ),
    };
    let mut report = Diagnostic::builder(code, at)
      .message(message)
      .primary_label(title)
      .help(
        "instantiate it with type arguments that derive no instantiation under which they meet",
      );
    if let Some(derives) = derives {
      report = report.label(derives, DERIVES);
    }
    let report = labelled(labelled(report, first, labels.0), later, labels.1);
    self.reporter.report(report.build());
  }

  /// Reports at `at`, a use of an instantiation of `user`, the instantiation `blame` names, which
  /// makes an argument required, or whose judging a limit stopped.
  fn report_derived_made(&mut self, closure: &Closure, at: Location, user: DefId, blame: Blame) {
    let Some(failed) = closure.node(blame.node) else {
      return;
    };
    match self.made.get(&failed.ty).copied() {
      Some(Made::Required(index)) => {
        let derives = self.place_location(blame.place);
        self.report_derived_required(at, user, index, derives);
      }
      Some(Made::Stopped(stop)) => {
        let subject = format!(
          "the arguments the fields of an instantiation of `{}` that `{}` derives here require",
          Excerpt(self.builder.def_name(failed.def)),
          Excerpt(self.builder.def_name(user))
        );
        self.report_limit(at, &subject, stop);
      }
      Some(Made::Nothing) | None => {}
    }
  }

  /// Reports at `at`, a use of an instantiation of `user` that derives, at `derives`, an
  /// instantiation making the latent argument at `index` required: labelled there, at the argument
  /// and at the interface's field that does not declare it.
  fn report_derived_required(
    &mut self,
    at: Location,
    user: DefId,
    index: u32,
    derives: Option<Location>,
  ) {
    let Some(&Latent {
      def,
      argument,
      field,
      interface,
      ..
    }) = self.latent.get(index as usize)
    else {
      return;
    };
    let (Some(written), Some(declared)) = (
      self.builder.member_location(argument),
      self.builder.member_location(field),
    ) else {
      return;
    };
    let used = self.used(user);
    let owner = Excerpt(self.builder.def_name(def));
    let interface = Excerpt(self.builder.def_name(interface));
    let named = Excerpt(self.member_spelling(field));
    let name = Excerpt(self.member_spelling(argument));
    let mut report = Diagnostic::builder(schema::UNEXPECTED_REQUIRED_ARGUMENT, at)
      .message(format!(
        "{used} with type arguments that derive an instantiation of `{owner}` under which its \
         field `{named}` requires the argument `{name}`, which `{interface}`'s field `{named}` \
         does not declare"
      ))
      .primary_label("unexpected required argument")
      .help(
        "make the argument nullable, give it a default, or declare it on the interface's field",
      );
    if let Some(derives) = derives {
      report = report.label(derives, DERIVES);
    }
    report = report.label(written, "required under these type arguments");
    self
      .reporter
      .report(labelled(report, declared, "the interface's field").build());
  }
}
