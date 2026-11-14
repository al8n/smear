//! Directive applications: each `@directive(…)` as written, and the arguments it has.

use tokora::diagnostic::Location;

use crate::graphqlx::id::{ApplicationId, DefId, MemberId, TyId, TypeExprId, ValueId};

use super::{
  Model,
  table::{ApplicationRow, KIND_NAMED},
  ty::Path,
};

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

/// One argument of a directive application: one the author wrote, or one the author left out
/// whose definition has a default, which a copy of the default fills.
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

  /// The complete type arguments of the application `id`: the arguments of its directive's
  /// instantiation, which its row holds, and none when it holds none, or the unresolved type.
  #[cfg(feature = "test-support")]
  pub(crate) fn type_arguments(&self, id: ApplicationId) -> &[TyId] {
    let Some(row) = self.applications.get(id.get() as usize) else {
      return &[];
    };
    match self.tys.get(row.ty.get() as usize) {
      Some(ty) if ty.shape == KIND_NAMED => ty.args.of(&self.ty_args),
      _ => &[],
    }
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

  /// Its type: its directive's instantiation, a named type of the directive with every type
  /// argument, the written ones and those its defaults fill; `None` for a directive of no type
  /// parameter, one that did not resolve, and a count of type arguments that was refused, and when
  /// the arguments could not be completed.
  //
  // Crate-private: what an application is as a type is no part of the public model yet. The rules
  // on what an instantiation's arguments take read it. A candidate public accessor for the model's
  // review.
  #[inline]
  pub(crate) fn ty(&self) -> Option<TyId> {
    let model: &'a Model = self.model;
    let ty = self.row().ty;
    match model.tys.get(ty.get() as usize) {
      Some(row) if row.shape == KIND_NAMED => Some(ty),
      _ => None,
    }
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

  /// Its arguments, in its definition's order: each one the author wrote, an argument written twice
  /// here twice, and each one the author left out whose definition has a default, a copy of the
  /// default filling it. One neither written nor defaulted is absent, and so is a defaulted one
  /// that [`Limits::max_value_steps`](crate::graphqlx::Limits::max_value_steps) could not pay for,
  /// or whose default's copy would hold a copy of itself.
  /// An application of a generic directive with its complete type arguments has each value, a
  /// copy included, coerced against its argument's type under them, and reads "required" there.
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
