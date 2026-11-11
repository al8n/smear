//! The model's tables: one `Copy` row per entity, every child list a run of a sibling pool, and
//! every spelling a run of the model's name or text arena, as `smear-schema`'s `repr` stores a
//! schema.
//!
//! Nothing here allocates per row. A row holds ids, runs, locations and flags; the views in the
//! sibling modules read a row and the pools its runs name.

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{DefId, IdRange, MemberId, ModuleId, PackageId, SourceId, Sym, TyId, TypeExprId, ValueId},
  package::{Layout, Tier},
};

use super::{
  PackageKind,
  application::Resolution,
  def::{Collection, DefKind, Origin, Primitive},
  member::MemberKind,
  module::ModuleKind,
  schema::Root,
  ty::PathRoot,
  value::LiteralKind,
};

/// A run of consecutive entries in one pool: a child list, as `repr`'s `Range32` is one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(super) struct Run {
  /// The index of the run's first entry.
  pub(super) start: u32,
  /// How many entries the run holds.
  pub(super) len: u32,
}

impl Run {
  /// One past the run's last entry.
  #[inline]
  pub(super) fn end(self) -> u32 {
    self.start.saturating_add(self.len)
  }

  /// The run's entries in `pool`, or none when the run is out of `pool`'s bounds, which only a run
  /// of another model can be.
  #[inline]
  pub(super) fn of<T>(self, pool: &[T]) -> &[T] {
    let start = self.start as usize;
    pool.get(start..start + self.len as usize).unwrap_or(&[])
  }

  /// The run's text in `arena`, or `""` when the run is out of `arena`'s bounds.
  #[inline]
  pub(super) fn text(self, arena: &str) -> &str {
    let start = self.start as usize;
    arena.get(start..start + self.len as usize).unwrap_or("")
  }
}

// A type-expression, type or value row says which variant of its public kind it is with one of
// the numbers below, rather than with a private enum: until the lowering lands, only the
// test-support door writes such rows, and a private variant that no shipped build constructs is
// dead code.

/// A named type, a definition and its arguments: `def` and `args` hold it.
pub(super) const KIND_NAMED: u8 = 0;
/// A type parameter: `def` and `index` hold it.
pub(super) const KIND_PARAM: u8 = 1;
/// A list: `element` holds it.
pub(super) const KIND_LIST: u8 = 2;
/// A path that did not resolve, and every row until a phase resolves it.
pub(super) const KIND_UNRESOLVED: u8 = 3;

/// `ValueKind::Null`.
pub(super) const VALUE_NULL: u8 = 0;
/// `ValueKind::Bool`: `payload` is 1 for `true` and 0 for `false`.
pub(super) const VALUE_BOOL: u8 = 1;
/// `ValueKind::Int`: `payload` indexes the model's integer table.
pub(super) const VALUE_INT: u8 = 2;
/// `ValueKind::Float`: `payload` indexes the model's float table.
pub(super) const VALUE_FLOAT: u8 = 3;
/// `ValueKind::String`: `run` is the cooked text, in the text arena.
pub(super) const VALUE_STRING: u8 = 4;
/// `ValueKind::Enum`: `def` and `member` hold it.
pub(super) const VALUE_ENUM: u8 = 5;
/// `ValueKind::Type`: `def` holds it.
pub(super) const VALUE_TYPE: u8 = 6;
/// `ValueKind::List`: `run` is the elements, in the model's value-item pool.
pub(super) const VALUE_LIST: u8 = 7;
/// `ValueKind::Set`: `run` is the elements, in the model's value-item pool.
pub(super) const VALUE_SET: u8 = 8;
/// `ValueKind::Map`: `run` is the entries, in the model's map-entry pool.
pub(super) const VALUE_MAP: u8 = 9;
/// `ValueKind::Object`: `run` is the entries, in the model's object-entry pool.
pub(super) const VALUE_OBJECT: u8 = 10;
/// `ValueKind::Literal`: `payload` indexes the model's literal table.
pub(super) const VALUE_LITERAL: u8 = 11;
/// `ValueKind::Unresolved`, and every row until a phase gives it its shape.
pub(super) const VALUE_UNRESOLVED: u8 = 12;

/// A description: its cooked text, in the text arena, and where it is written.
#[derive(Debug, Clone, Copy)]
pub(super) struct DescriptionRow {
  pub(super) text: Run,
  pub(super) location: Location,
}

/// A path as written: its root, its segments in the model's path pool, and where it is written.
#[derive(Debug, Clone, Copy)]
pub(super) struct PathRow {
  pub(super) root: PathRoot,
  pub(super) segments: Run,
  pub(super) location: Location,
}

/// One package. Appended by `analyze`, in [`PackageId`] order.
#[derive(Debug, Clone, Copy)]
pub(super) struct PackageRow {
  /// The name, in the text arena.
  pub(super) name: Run,
  pub(super) kind: PackageKind,
  pub(super) tier: Tier,
  pub(super) layout: Layout,
  /// Its package source, then its files.
  pub(super) sources: IdRange<SourceId>,
  /// Its resolved dependencies, in the model's dependency pool.
  pub(super) dependencies: Run,
  /// Its integer provider, as an index into the model's provider table.
  pub(super) integers: Option<u32>,
  /// Its root module: [`UNSET_MODULE`] until the module is appended.
  pub(super) root_module: ModuleId,
}

/// The root module of a package whose root module has not been appended yet.
pub(super) const UNSET_MODULE: ModuleId = ModuleId::new(u32::MAX);

/// One module. Appended by the module tree, in [`ModuleId`] order.
#[derive(Debug, Clone, Copy)]
pub(super) struct ModuleRow {
  pub(super) package: PackageId,
  pub(super) parent: Option<ModuleId>,
  /// Its path from its package's root module, in the model's path pool: empty for a root module.
  pub(super) path: Run,
  pub(super) kind: ModuleKind,
  /// Where an inline `mod` declares it.
  pub(super) location: Option<Location>,
  pub(super) description: Option<DescriptionRow>,
  /// Its definitions: derived when the model is finished.
  pub(super) defs: IdRange<DefId>,
  /// Its child modules, in the model's child pool: derived when the model is finished.
  pub(super) children: Run,
}

/// One type or directive definition. Appended by item collection, in [`DefId`] order.
#[derive(Debug, Clone, Copy)]
pub(super) struct DefRow {
  pub(super) kind: DefKind,
  pub(super) name: Sym,
  pub(super) module: ModuleId,
  /// Where its name is written.
  pub(super) location: Location,
  pub(super) origin: Origin,
  pub(super) description: Option<DescriptionRow>,
  /// Its definition and extensions, in the model's contribution table.
  pub(super) contributions: Run,
  /// Its type parameters, in the model's type-parameter table.
  pub(super) type_params: Run,
  /// Its bounds, in the model's bound pool.
  pub(super) bounds: Run,
  /// Its `implements` entries, every contribution's, in the model's implements pool.
  pub(super) implements: Run,
  /// Its union members, every contribution's, in the model's union-member pool.
  pub(super) union_members: Run,
  /// Its applications, every contribution's, in the model's listed-application pool.
  pub(super) applications: Run,
  /// Its direct members: fields, input fields, enum values, or a directive's arguments.
  pub(super) members: IdRange<MemberId>,
  /// A directive definition's locations, in the model's location pool.
  pub(super) locations: Run,
  pub(super) repeatable: bool,
  /// The tier it requires: `Core` until the tier phase computes it.
  pub(super) tier: Tier,
  pub(super) intrinsic: Option<Primitive>,
  pub(super) marker: bool,
  pub(super) collection: Option<Collection>,
  /// Its `@extern`s, in the model's extern table.
  pub(super) externs: Run,
  /// Its closed instantiations, in the model's instantiation pool: derived when the model is
  /// finished.
  pub(super) instantiations: Run,
}

/// The definition of a definition, one of its extensions, or one of the schema's: what one
/// written item contributed. Appended with its owner, in contribution order.
#[derive(Debug, Clone, Copy)]
pub(super) struct ContributionRow {
  /// Where its name is written, or the `schema` keyword.
  pub(super) location: Location,
  /// From its description, or its first keyword, to its last token.
  pub(super) extent: Location,
  /// The direct members it contributed.
  pub(super) members: IdRange<MemberId>,
  /// The `implements` entries it contributed, in the model's implements pool.
  pub(super) implements: Run,
  /// The union members it contributed, in the model's union-member pool.
  pub(super) union_members: Run,
  /// The applications it contributed, in the model's listed-application pool.
  pub(super) applications: Run,
  /// Whether an error a phase or one of smear's rules reported lies within its extent.
  pub(super) marked: bool,
}

/// One type parameter of a definition.
#[derive(Debug, Clone, Copy)]
pub(super) struct TypeParamRow {
  pub(super) name: Sym,
  pub(super) location: Location,
  pub(super) default: Option<TypeExprId>,
}

/// One `@extern` of a definition, resolved.
#[derive(Debug, Clone, Copy)]
pub(super) struct ExternRow {
  /// Its `lang`, in the text arena.
  pub(super) lang: Run,
  /// Its `type`, in the text arena.
  pub(super) ty: Run,
  /// Its effective tier.
  pub(super) tier: Tier,
}

/// A second name a module gives a definition: `Int` for `i32`.
#[derive(Debug, Clone, Copy)]
pub(super) struct AliasRow {
  pub(super) module: ModuleId,
  pub(super) name: Sym,
  pub(super) def: DefId,
}

/// One member. Appended by item collection, in [`MemberId`] order.
#[derive(Debug, Clone, Copy)]
pub(super) struct MemberRow {
  pub(super) kind: MemberKind,
  pub(super) name: Sym,
  pub(super) owner: DefId,
  /// The field, for a field's argument.
  pub(super) parent: Option<MemberId>,
  /// Where its name is written.
  pub(super) location: Location,
  /// From its description, or its first token, to its last token.
  pub(super) extent: Location,
  pub(super) description: Option<DescriptionRow>,
  pub(super) ty: Option<TypeExprId>,
  pub(super) default: Option<ValueId>,
  /// A field's arguments.
  pub(super) arguments: IdRange<MemberId>,
  /// Its applications, in the model's member-application pool.
  pub(super) applications: Run,
  /// Whether an error a phase or one of smear's rules reported lies within its extent.
  pub(super) marked: bool,
}

/// One written type occurrence. Appended in the walk's order, an occurrence before what it nests.
#[derive(Debug, Clone, Copy)]
pub(super) struct TypeExprRow {
  /// One of the `KIND_*` constants.
  pub(super) shape: u8,
  /// `KIND_NAMED` and `KIND_PARAM`: the definition.
  pub(super) def: DefId,
  /// `KIND_PARAM`: the parameter's index.
  pub(super) index: u32,
  /// `KIND_LIST`: the element.
  pub(super) element: TypeExprId,
  /// The arguments as written, in the model's type-expression argument pool, whatever the shape.
  pub(super) args: Run,
  pub(super) non_null: bool,
  pub(super) location: Location,
  /// The path as written; `None` for a list.
  pub(super) path: Option<PathRow>,
  /// Its type: [`UNSET_TY`] until a phase sets it.
  pub(super) ty: TyId,
}

/// A type an occurrence or a value has not been given yet. The model is finished with every such
/// type replaced by the one unresolved type.
pub(super) const UNSET_TY: TyId = TyId::new(u32::MAX);

/// One type, interned. Appended by interning, a type's arguments before the type.
#[derive(Debug, Clone, Copy)]
pub(super) struct TyRow {
  /// One of the `KIND_*` constants.
  pub(super) shape: u8,
  /// `KIND_NAMED` and `KIND_PARAM`: the definition.
  pub(super) def: DefId,
  /// `KIND_PARAM`: the parameter's index.
  pub(super) index: u32,
  /// `KIND_LIST`: the element.
  pub(super) element: TyId,
  /// `KIND_NAMED`: every argument, in the model's type argument pool.
  pub(super) args: Run,
  pub(super) non_null: bool,
  /// Whether it names no type parameter.
  pub(super) closed: bool,
  /// The tier it requires: `Core` until the tier phase computes it.
  pub(super) tier: Tier,
}

/// One value occurrence. Appended in the walk's order, a value before what it holds.
#[derive(Debug, Clone, Copy)]
pub(super) struct ValueRow {
  /// One of the `VALUE_*` constants.
  pub(super) shape: u8,
  /// `VALUE_ENUM` and `VALUE_TYPE`: the definition.
  pub(super) def: DefId,
  /// `VALUE_ENUM`: the enum value.
  pub(super) member: MemberId,
  /// `VALUE_BOOL`: 1 or 0. `VALUE_INT`, `VALUE_FLOAT` and `VALUE_LITERAL`: the index of its row in
  /// the integer, float or literal table.
  pub(super) payload: u32,
  /// `VALUE_STRING`: the text, in the text arena. `VALUE_LIST` and `VALUE_SET`: the elements, in
  /// the value-item pool. `VALUE_MAP`: the entries, in the map-entry pool. `VALUE_OBJECT`: the
  /// entries, in the object-entry pool.
  pub(super) run: Run,
  pub(super) location: Location,
  /// Its type: [`UNSET_TY`] until a phase sets it.
  pub(super) ty: TyId,
  pub(super) written: bool,
  /// The path as written, for an enum or type value.
  pub(super) path: Option<PathRow>,
}

/// An integer literal's value.
#[derive(Debug, Clone, Copy)]
pub(super) struct IntRow {
  /// Below zero; never set for a zero magnitude.
  pub(super) negative: bool,
  /// The absolute value, little-endian with no trailing zero byte, in the model's magnitude pool.
  pub(super) magnitude: Run,
  /// The literal as written, in the text arena.
  pub(super) text: Run,
}

/// A float literal's value.
#[derive(Debug, Clone, Copy)]
pub(super) struct FloatRow {
  pub(super) value: f64,
  /// The literal as written, in the text arena.
  pub(super) text: Run,
}

/// A literal kept as written.
#[derive(Debug, Clone, Copy)]
pub(super) struct LiteralRow {
  pub(super) kind: LiteralKind,
  /// The literal as written, in the text arena.
  pub(super) text: Run,
}

/// One directive application. Appended in the walk's order.
#[derive(Debug, Clone, Copy)]
pub(super) struct ApplicationRow {
  pub(super) directive: Resolution<DefId>,
  /// Its type arguments as written, in the model's application type-argument pool.
  pub(super) type_args: Run,
  /// The whole application.
  pub(super) location: Location,
  pub(super) path: PathRow,
  /// Its arguments, in the model's argument pool.
  pub(super) arguments: Run,
}

/// The schema: there is one per model, whether a schema definition is written or not.
#[derive(Debug, Clone, Copy, Default)]
pub(super) struct SchemaRow {
  /// The query, mutation and subscription roots, in that order.
  pub(super) roots: [Option<Root>; 3],
  /// Its applications, every contribution's, in the model's listed-application pool.
  pub(super) applications: Run,
  pub(super) description: Option<DescriptionRow>,
  /// Its schema definition and extensions, in the model's contribution table.
  pub(super) contributions: Run,
}

/// The built-in definitions a consumer keys on, found by name in the built-in packages when the
/// model is finished.
#[derive(Debug, Clone, Copy, Default)]
pub(super) struct BuiltinsRow {
  /// In the order of [`BUILTIN_DIRECTIVES`].
  pub(super) directives: [Option<DefId>; 11],
  /// In the order of [`BUILTIN_TYPES`].
  pub(super) types: [Option<DefId>; 6],
}

/// The built-in directives' names, in the order [`BuiltinsRow::directives`] keeps them.
pub(super) const BUILTIN_DIRECTIVES: [&str; 11] = [
  "marker",
  "set",
  "map",
  "extern",
  "repr",
  "heap",
  "deprecated",
  "specifiedBy",
  "oneOf",
  "skip",
  "include",
];

/// The built-in types' names, in the order [`BuiltinsRow::types`] keeps them.
pub(super) const BUILTIN_TYPES: [&str; 6] =
  ["TypeRef", "MapKey", "NonZero", "string", "bytes", "id"];

/// Unused ids a row holds where its shape gives a field no meaning.
pub(super) const NO_DEF: DefId = DefId::new(0);
