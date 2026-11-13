//! Directive applications: each `@directive(…)` as written, and the arguments it has.

use tokora::diagnostic::Location;

use crate::graphqlx::id::{ApplicationId, DefId, MemberId, TypeExprId, ValueId};

use super::{Model, table::ApplicationRow, ty::Path};

/// What a written name resolved to, or that it did not resolve.
///
/// It is never `Unresolved` in an analysis that [`is_ok`](crate::graphqlx::Analysis::is_ok): it
/// appears only within the extent of an error a phase reported.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Resolution<T> {
  /// It resolved to this.
  Resolved(T),
  /// It did not resolve, and a diagnostic says why.
  Unresolved,
}

/// One argument of a directive application: one the author wrote.
//
// Coercion, still to come, adds one for each argument the author left out whose definition has a
// default, with no name written for it; `name_location` and `written` answer for it already.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ApplicationArgument {
  pub(super) argument: MemberId,
  pub(super) name: Option<Location>,
  pub(super) value: ValueId,
}

impl ApplicationArgument {
  /// The argument of the directive definition it sets.
  #[inline]
  pub const fn argument(&self) -> MemberId {
    self.argument
  }

  /// Where its name is written, or `None` when its definition's default filled it.
  #[inline]
  pub const fn name_location(&self) -> Option<Location> {
    self.name
  }

  /// Its value.
  #[inline]
  pub const fn value(&self) -> ValueId {
    self.value
  }

  /// Whether the author wrote it, rather than its definition's default filling it.
  #[inline]
  pub const fn written(&self) -> bool {
    self.name.is_some()
  }
}

impl Model {
  /// The application `id` names.
  ///
  /// # Panics
  ///
  /// If `id` is not one of this model's applications: an id belongs to the analysis that issued
  /// it.
  #[inline]
  pub fn application(&self, id: ApplicationId) -> Application<'_> {
    assert!(
      (id.get() as usize) < self.applications.len(),
      "application id {} is past the {} applications of this model",
      id.get(),
      self.applications.len()
    );
    Application { model: self, id }
  }
}

/// One directive application of the model: one `@directive(…)` as written.
///
/// An application whose directive did not resolve stays, with its locations and its path, and
/// with no type argument and no argument.
///
/// `Copy`: it holds the model and an id, and everything it answers is borrowed from the model.
#[derive(Clone, Copy)]
pub struct Application<'a> {
  model: &'a Model,
  id: ApplicationId,
}

impl<'a> Application<'a> {
  /// Its id.
  #[inline]
  pub const fn id(&self) -> ApplicationId {
    self.id
  }

  /// The directive definition it applies.
  #[inline]
  pub fn directive(&self) -> Resolution<DefId> {
    self.row().directive
  }

  /// Its type arguments as written, in order: what a generic directive's parameters take.
  #[inline]
  pub fn type_args(&self) -> &'a [TypeExprId] {
    let model: &'a Model = self.model;
    self.row().type_args.of(&model.application_type_args)
  }

  /// Where the whole application is written, from its `@` to its last token.
  #[inline]
  pub fn location(&self) -> Location {
    self.row().location
  }

  /// Where its directive's name or path is written.
  #[inline]
  pub fn path_location(&self) -> Location {
    self.row().path.location
  }

  /// Its directive's name or path, as written.
  #[inline]
  pub fn path(&self) -> Path<'a> {
    Path::of(self.model, self.row().path)
  }

  /// Its arguments, each one the author wrote, in its definition's order: an argument written twice
  /// is here twice, and one not written is absent.
  //
  // Coercion, still to come, fills each argument the author left out whose definition has a
  // default; until it lands no default fills one.
  #[inline]
  pub fn arguments(&self) -> &'a [ApplicationArgument] {
    let model: &'a Model = self.model;
    self.row().arguments.of(&model.arguments)
  }

  /// The model's record of this application.
  fn row(&self) -> &'a ApplicationRow {
    let model: &'a Model = self.model;
    &model.applications[self.id.get() as usize]
  }
}

impl core::fmt::Debug for Application<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Application")
      .field("id", &self.id)
      .field("directive", &self.directive())
      .field("location", &self.location())
      .field("arguments", &self.arguments())
      .finish()
  }
}
