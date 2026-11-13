//! Types: each written occurrence, and each type's interned identity.

use core::num::NonZeroU32;

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{DefId, Sym, TyId, TypeExprId},
  package::Tier,
};

use super::{
  Model,
  table::{KIND_LIST, KIND_NAMED, KIND_PARAM, KIND_UNRESOLVED, PathRow, TyRow, TypeExprRow},
};

/// Where a written path starts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum PathRoot {
  /// No root: the path starts in the scope it is written in, as `Int` and `geo::Point` do.
  Relative,
  /// `~::`: the root module of the package it is written in.
  Package,
  /// `^::`: a package the one it is written in may reach, named by the path's first segment.
  Dependency,
  /// `..::`, as many times as it holds: that many modules up from the one it is written in.
  Parent(NonZeroU32),
}

/// A path as written: where it starts, and its segments.
///
/// The written spelling is kept beside what it resolved to: `Int` stays `Int` although it names
/// `i32`, and an extension's own name for a type parameter stays its own.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Path<'a> {
  root: PathRoot,
  segments: &'a [Sym],
}

impl<'a> Path<'a> {
  /// The path `row` holds in `model`.
  pub(super) fn of(model: &'a Model, row: PathRow) -> Self {
    Self {
      root: row.root,
      segments: row.segments.of(&model.paths),
    }
  }

  /// Where it starts.
  #[inline]
  pub const fn root(&self) -> PathRoot {
    self.root
  }

  /// Its segments, in order: `geo` and `Point` for `^::geo::Point`. Each reads through
  /// [`Model::name`].
  #[inline]
  pub const fn segments(&self) -> &'a [Sym] {
    self.segments
  }
}

/// What one written type occurrence is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum TypeExprKind<'a> {
  /// A definition, and the arguments written for it.
  #[non_exhaustive]
  Named {
    /// The definition it names.
    def: DefId,
    /// The arguments as written, in order: fewer than the definition's parameters when trailing
    /// defaults fill the rest, and none for an alias such as `NonZeroU32`. The complete
    /// instantiation is the occurrence's type.
    args: &'a [TypeExprId],
  },
  /// One of a definition's type parameters.
  #[non_exhaustive]
  Param {
    /// The definition the parameter belongs to.
    def: DefId,
    /// The parameter's position: its index among the definition's type parameters. An extension
    /// names its target's parameters by position, so a name it gives past them has a position
    /// past them too: the extension's list is refused where it is written, and the occurrence's
    /// type is the unresolved one.
    index: u32,
  },
  /// A list.
  #[non_exhaustive]
  List {
    /// Its element, as written.
    element: TypeExprId,
  },
  /// A path that did not resolve. Only an analysis that is not ok has one, within the extent of
  /// the error that says why.
  Unresolved,
}

/// What one type is: the shapes of [`TypeExprKind`], with every argument a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum TyKind<'a> {
  /// A definition with every argument it has: the written ones, then the defaults that filled the
  /// rest, substituted, with an alias expanded.
  #[non_exhaustive]
  Named {
    /// The definition.
    def: DefId,
    /// Every argument, in the definition's order. A collection's key or element, the first
    /// argument of a definition `@set` or `@map` makes one, is non-null here however it is written,
    /// so it may differ in nullness from the type of the occurrence that writes it.
    args: &'a [TyId],
  },
  /// One of a definition's type parameters: the `T` of `Page<T>` and the `U` of `Cursor<U>` are
  /// two types.
  #[non_exhaustive]
  Param {
    /// The definition the parameter belongs to.
    def: DefId,
    /// The parameter's index among the definition's type parameters.
    index: u32,
  },
  /// A list.
  #[non_exhaustive]
  List {
    /// Its element.
    element: TyId,
  },
  /// The one type every unresolved occurrence shares, which a rule tests for before it compares
  /// two types.
  Unresolved,
}

impl Model {
  /// The type occurrence `id` names.
  ///
  /// # Panics
  ///
  /// If `id` is not one of this model's type occurrences: an id belongs to the analysis that
  /// issued it.
  #[inline]
  pub fn type_expr(&self, id: TypeExprId) -> TypeExpr<'_> {
    assert!(
      (id.get() as usize) < self.type_exprs.len(),
      "type occurrence id {} is past the {} type occurrences of this model",
      id.get(),
      self.type_exprs.len()
    );
    TypeExpr { model: self, id }
  }

  /// The type `id` names.
  ///
  /// # Panics
  ///
  /// If `id` is not one of this model's types: an id belongs to the analysis that issued it.
  #[inline]
  pub fn ty(&self, id: TyId) -> Ty<'_> {
    assert!(
      (id.get() as usize) < self.tys.len(),
      "type id {} is past the {} types of this model",
      id.get(),
      self.tys.len()
    );
    Ty { model: self, id }
  }

  /// The distinct closed types that the program's written occurrences of `def` resolve to, with
  /// defaults filled, in id order.
  ///
  /// Substitution is not followed: a written `Page<User>` is listed, and a `Cursor<User>` that
  /// `Page`'s fields would need is listed only where the program writes one. A code generator that
  /// monomorphises computes that closure from these.
  ///
  /// # Panics
  ///
  /// If `def` is not one of [`defs`](Self::defs).
  //
  // The instantiation phase records each written occurrence's own type, both nullnesses when both
  // are written; the builder sorts them and keeps each once.
  #[inline]
  pub fn instantiations(&self, def: DefId) -> &[TyId] {
    self.defs[def.get() as usize]
      .instantiations
      .of(&self.instantiations)
  }

  /// Whether `ty` satisfies the interface or marker `interface`: through an `implements` it
  /// declares, or intrinsically, as every integer primitive, `bool`, `string`, `id` and every enum
  /// satisfies `core`'s `MapKey`.
  ///
  /// [`Def::implements`](super::Def::implements) lists only what is written; this answers whether
  /// the type may stand where the interface is required.
  //
  // The next part of the type phase, which judges bounds, records them. Until it lands nothing
  // does, and every pair answers `false`.
  #[inline]
  pub fn satisfies(&self, ty: TyId, interface: DefId) -> bool {
    self.satisfactions.binary_search(&(ty, interface)).is_ok()
  }
}

/// One written type occurrence of the model: what a diagnostic about a type points at.
///
/// `Copy`: it holds the model and an id, and everything it answers is borrowed from the model.
#[derive(Clone, Copy)]
pub struct TypeExpr<'a> {
  model: &'a Model,
  id: TypeExprId,
}

impl<'a> TypeExpr<'a> {
  /// Its id.
  #[inline]
  pub const fn id(&self) -> TypeExprId {
    self.id
  }

  /// What it is.
  #[inline]
  pub fn kind(&self) -> TypeExprKind<'a> {
    let model: &'a Model = self.model;
    let row = self.row();
    match row.shape {
      KIND_NAMED => TypeExprKind::Named {
        def: row.def,
        args: row.args.of(&model.type_expr_args),
      },
      KIND_PARAM => TypeExprKind::Param {
        def: row.def,
        index: row.index,
      },
      KIND_LIST => TypeExprKind::List {
        element: row.element,
      },
      KIND_UNRESOLVED => TypeExprKind::Unresolved,
      _ => TypeExprKind::Unresolved,
    }
  }

  /// Whether it is written non-null, with `!`.
  #[inline]
  pub fn non_null(&self) -> bool {
    self.row().non_null
  }

  /// Where it is written. Its source is its own, which for an occurrence an extension wrote is
  /// the extension's.
  #[inline]
  pub fn location(&self) -> Location {
    self.row().location
  }

  /// Its type: the complete instantiation, with defaults filled and substituted and an alias
  /// expanded. Two occurrences of one type answer one id.
  //
  // The instantiation phase gives it. An occurrence it could not type, or that holds one it could
  // not, has the one unresolved type, within the extent of the error a phase reported for it.
  #[inline]
  pub fn ty(&self) -> TyId {
    self.row().ty
  }

  /// The path as written, for every occurrence but a list: `Int`, `NonZeroU32`, `^::geo::Point`.
  #[inline]
  pub fn path(&self) -> Option<Path<'a>> {
    let row = self.row().path?;
    Some(Path::of(self.model, row))
  }

  /// Where the path is written, for every occurrence but a list.
  #[inline]
  pub fn path_location(&self) -> Option<Location> {
    self.row().path.map(|row: PathRow| row.location)
  }

  /// The arguments as written, in order, whatever the occurrence resolved to: one that did not
  /// resolve, or that names a type parameter, keeps those it was written with. They are the same
  /// as [`kind`](Self::kind)'s `args` for a named occurrence, and the walk reaches them for every
  /// shape.
  #[inline]
  pub(crate) fn written_args(&self) -> &'a [TypeExprId] {
    let model: &'a Model = self.model;
    self.row().args.of(&model.type_expr_args)
  }

  /// The model's record of this occurrence.
  fn row(&self) -> &'a TypeExprRow {
    let model: &'a Model = self.model;
    &model.type_exprs[self.id.get() as usize]
  }
}

impl core::fmt::Debug for TypeExpr<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("TypeExpr")
      .field("id", &self.id)
      .field("kind", &self.kind())
      .field("non_null", &self.non_null())
      .field("ty", &self.ty())
      .finish()
  }
}

/// One type of the model: its interned identity, so `==` on [`TyId`] is type equality.
///
/// `Copy`: it holds the model and an id, and everything it answers is borrowed from the model.
#[derive(Clone, Copy)]
pub struct Ty<'a> {
  model: &'a Model,
  id: TyId,
}

impl<'a> Ty<'a> {
  /// Its id.
  #[inline]
  pub const fn id(&self) -> TyId {
    self.id
  }

  /// What it is.
  #[inline]
  pub fn kind(&self) -> TyKind<'a> {
    let model: &'a Model = self.model;
    let row = self.row();
    match row.shape {
      KIND_NAMED => TyKind::Named {
        def: row.def,
        args: row.args.of(&model.ty_args),
      },
      KIND_PARAM => TyKind::Param {
        def: row.def,
        index: row.index,
      },
      KIND_LIST => TyKind::List {
        element: row.element,
      },
      KIND_UNRESOLVED => TyKind::Unresolved,
      _ => TyKind::Unresolved,
    }
  }

  /// Whether it is non-null. The unresolved type is not.
  #[inline]
  pub fn non_null(&self) -> bool {
    self.row().non_null
  }

  /// The tier it requires, counting a type parameter as `core`'s: for a type that names a
  /// parameter, a lower bound.
  //
  // The tier phase computes it. Until that phase lands, every type answers `Core`, the least tier
  // and so a lower bound of the tier the phase will compute.
  #[inline]
  pub fn tier(&self) -> Tier {
    self.row().tier
  }

  /// Whether it names no type parameter, in itself or in any argument or element.
  #[inline]
  pub fn is_closed(&self) -> bool {
    self.row().closed
  }

  /// The model's record of this type.
  fn row(&self) -> &'a TyRow {
    let model: &'a Model = self.model;
    &model.tys[self.id.get() as usize]
  }
}

impl core::fmt::Debug for Ty<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Ty")
      .field("id", &self.id)
      .field("kind", &self.kind())
      .field("non_null", &self.non_null())
      .finish()
  }
}
