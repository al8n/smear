//! Implementation: the phase after binding, which checks what each definition says it is with its
//! `implements` entries, and a union with its members.
//!
//! It reads no document, writes no row of the model and no text, and interns no name: it reads
//! the rows the phases before it wrote, through the builder, and reports.
//!
//! # What a definition is, here
//!
//! What its own package writes: the package that wrote the definition. An `implements` entry and a
//! union member are read when that package wrote them, in the definition or in one of its own
//! extensions, wherever the file. What another package writes into a definition is the orphan
//! rule's mistake, and is read nowhere here. An entry or a member whose type is the unresolved
//! type is passed by everywhere: the resolver reported it.
//!
//! # What is checked
//!
//! Each definition, in id order. An `implements` entry of a type an earlier entry of the
//! definition has is `smear::schema::duplicate-implements-interface`, at it, with a label at the
//! first of that type, and so is every later one: `implements I & I & I` is two. A union's member of
//! a type an earlier member has is `smear::schema::duplicate-union-member`, likewise. Two entries
//! are told apart by their types, so `Cmp<Int>` and `Cmp<String>` are two, and an unresolved one is
//! compared with nothing.
//!
//! # What it costs
//!
//! A definition's own entries and members are written, and each is read once, by one scan of the
//! shared kernel, or one sort when the list is wide.

use std::{format, vec::Vec};

use smear_semantics::duplicates::Duplicates;
use tokora::diagnostic::Location;

use super::{
  code::schema,
  declare::Declarations,
  diagnostic::{Diagnostic, DiagnosticBuilder, Excerpt, Reporter},
  id::{DefId, PackageId, TyId, TypeExprId},
  instantiate::own_package,
  model::{DefKind, ModelBuilder, Occurrence, Shape},
  source::SourceMap,
};

/// Checks what each definition `builder` holds says it is, whose declarations are `declarations`
/// and whose sources `sources` numbers, reporting into `reporter`: that no two of its `implements`
/// entries, nor of a union's members, are one type.
pub(crate) fn implement(
  declarations: &Declarations<'_, '_>,
  sources: &SourceMap,
  builder: &mut ModelBuilder,
  reporter: &mut Reporter,
) {
  let count = builder.def_count();
  let mut owners: Vec<Option<PackageId>> = Vec::with_capacity(count as usize);
  for index in 0..count {
    owners.push(own_package(declarations, DefId::new(index)));
  }
  let mut implementing = Implementing {
    builder,
    reporter,
    sources,
    owners,
    entries: Vec::new(),
    members: Vec::new(),
    twice: Vec::new(),
  };
  implementing.definitions();
}

/// One of a definition's lists of types.
#[derive(Clone, Copy)]
enum List {
  /// Its `implements` entries.
  Implements,
  /// A union's members.
  Members,
}

/// What the phase keeps from one definition to the next.
struct Implementing<'p> {
  builder: &'p mut ModelBuilder,
  reporter: &'p mut Reporter,
  sources: &'p SourceMap,
  /// Each definition's own package, by `DefId`: the package that wrote it.
  owners: Vec<Option<PackageId>>,
  /// The definition being checked's own entries whose types resolved, in order, each with its
  /// type.
  entries: Vec<(TypeExprId, TyId)>,
  /// The union being checked's own members whose types resolved, in order, each with its type.
  members: Vec<(TypeExprId, TyId)>,
  /// The written duplicates of the list being checked: each later item, the first of its type,
  /// and the type.
  twice: Vec<(TypeExprId, TypeExprId, TyId)>,
}

impl Implementing<'_> {
  /// Checks every definition, in id order. One no package wrote, which no analysis holds, is
  /// passed by.
  fn definitions(&mut self) {
    for index in 0..self.builder.def_count() {
      let def = DefId::new(index);
      let Some(own) = self.owners.get(index as usize).copied().flatten() else {
        continue;
      };
      if self.builder.def_kind(def) == Some(DefKind::Union) {
        own_items(
          self.builder,
          self.sources,
          def,
          own,
          List::Members,
          &mut self.members,
        );
        self.written_duplicates(def, List::Members);
      }
      own_items(
        self.builder,
        self.sources,
        def,
        own,
        List::Implements,
        &mut self.entries,
      );
      self.written_duplicates(def, List::Implements);
    }
  }

  /// Reports each item of `def`'s list `list`, as gathered, whose type an earlier item has: at
  /// it, with a label at the first of that type.
  fn written_duplicates(&mut self, def: DefId, list: List) {
    self.twice.clear();
    let items: &[(TypeExprId, TyId)] = match list {
      List::Implements => &self.entries,
      List::Members => &self.members,
    };
    let mut seen: Duplicates<TyId> = Duplicates::over(items.len(), |at: usize| {
      items
        .get(at)
        .map_or(TyId::new(0), |&(_, ty): &(TypeExprId, TyId)| ty)
    });
    for (position, &(later, ty)) in items.iter().enumerate() {
      if let Some(earlier) = seen.first(position, ty)
        && let Some(&(first, _)) = items.get(earlier)
      {
        self.twice.push((later, first, ty));
      }
    }
    for index in 0..self.twice.len() {
      let Some(&(later, first, ty)) = self.twice.get(index) else {
        break;
      };
      self.report_written(def, list, later, first, ty);
    }
  }

  /// Reports `later`, an item of `def`'s list `list` of the type `ty`, which `first`, an earlier
  /// one, has.
  fn report_written(
    &mut self,
    def: DefId,
    list: List,
    later: TypeExprId,
    first: TypeExprId,
    ty: TyId,
  ) {
    let (Some(at), Some(earlier)) = (self.located(later), self.located(first)) else {
      return;
    };
    let owner = Excerpt(self.builder.def_name(def));
    let named = Excerpt(type_name(self.builder, ty));
    let (code, message, title, label) = match list {
      List::Implements => (
        schema::DUPLICATE_IMPLEMENTS_INTERFACE,
        format!("`{owner}` implements `{named}` twice"),
        "duplicate interface",
        "first implemented here",
      ),
      List::Members => (
        schema::DUPLICATE_UNION_MEMBER,
        format!("the union `{owner}` has `{named}` as a member twice"),
        "duplicate union member",
        "first a member here",
      ),
    };
    let report = Diagnostic::builder(code, at)
      .message(message)
      .primary_label(title)
      .help("remove the later one");
    self
      .reporter
      .report(labelled(report, earlier, label).build());
  }

  /// Where the type occurrence `id` is written.
  fn located(&self, id: TypeExprId) -> Option<Location> {
    self
      .builder
      .occurrence(id)
      .map(|occurrence: Occurrence<'_>| occurrence.location)
  }
}

/// Gathers into `into` the items of `def`'s list `list` that `own`, its own package, wrote and
/// whose types resolved, in order, each with its type, `sources` telling who wrote each.
fn own_items(
  builder: &ModelBuilder,
  sources: &SourceMap,
  def: DefId,
  own: PackageId,
  list: List,
  into: &mut Vec<(TypeExprId, TyId)>,
) {
  into.clear();
  let items: &[TypeExprId] = match list {
    List::Implements => builder.def_implements(def),
    List::Members => builder.def_union_members(def),
  };
  for &item in items {
    let Some(occurrence) = builder.occurrence(item) else {
      continue;
    };
    if sources.written_by(occurrence.location) != Some(own) {
      continue;
    }
    let Some(ty) = occurrence.ty else {
      continue;
    };
    if matches!(builder.ty_shape(ty), Some(Shape::Unresolved) | None) {
      continue;
    }
    into.push((item, ty));
  }
}

/// The name an entry or a member of the type `ty` is written with: its definition's, or its type
/// parameter's.
fn type_name(builder: &ModelBuilder, ty: TyId) -> &[u8] {
  match builder.ty_shape(ty) {
    Some(Shape::Named(def)) => builder.def_name(def),
    Some(Shape::Param { def, index }) => match builder.type_param(def, index) {
      Some((name, _, _)) => builder.spelling(name),
      None => &[],
    },
    Some(Shape::List(_) | Shape::Unresolved) | None => &[],
  }
}

/// `report` with a label saying `text` at `at`, unless `at` is a whole source, which no label
/// points at.
fn labelled(report: DiagnosticBuilder, at: Location, text: &'static str) -> DiagnosticBuilder {
  if at.is_entire() {
    report
  } else {
    report.label(at, text)
  }
}
