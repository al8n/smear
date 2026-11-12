//! The GraphQLx analyzer's model: every accessor, over a model the test-support door builds with
//! one of each kind of entity.
//!
//! The analyzer does not lower documents into models yet, so these tests write one through the
//! builder the analysis writes its own with. The fixture asserts every id the builder hands back,
//! so the constants below are the model's numbering, and a change to it fails here first.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Application, ApplicationArgument, ApplicationId, Builtins, CanonicalName, Collection,
    Contribution, Contributions, Def, DefId, DefKind, Description, DirectiveLocation, Extern,
    Externs, FloatValue, IntValue, IntegerProvider, Layout, Limits, Literal, LiteralKind, Member,
    MemberId, MemberKind, Model, Module, ModuleId, ModuleKind, Namespace, ObjectEntry, Origin,
    Package, PackageId, PackageInfo, PackageKind, Path, PathRoot, Primitive, Resolution, Root,
    RootOperation, Schema, SourceId, Sym, Tier, Ty, TyId, TyKind, TypeBound, TypeExpr, TypeExprId,
    TypeExprKind, TypeParam, TypeParams, Value, ValueId, ValueKind, analyze,
    test_support::ModelBuilder,
  },
  diagnostic::Location,
  lexer::tokora::SimpleSpan,
};

// Sources: each package's package source, then its files.
const CORE_SOURCE: u32 = 0;
const CORE_LIB: u32 = 1;
const APP_MEDIA: u32 = 5;
const APP_SCHEMA: u32 = 6;
const GEO_POINT: u32 = 8;

// Packages.
const CORE: PackageId = PackageId::new(0);
const ALLOC: PackageId = PackageId::new(1);
const STD: PackageId = PackageId::new(2);
const APP: PackageId = PackageId::new(3);
const GEO: PackageId = PackageId::new(4);

// Modules.
const CORE_ROOT: ModuleId = ModuleId::new(0);
const ALLOC_ROOT: ModuleId = ModuleId::new(1);
const STD_ROOT: ModuleId = ModuleId::new(2);
const APP_ROOT: ModuleId = ModuleId::new(3);
const MEDIA: ModuleId = ModuleId::new(4);
const VISION: ModuleId = ModuleId::new(5);
const GEO_ROOT: ModuleId = ModuleId::new(6);

// Definitions.
const I32: DefId = DefId::new(0);
const STRING: DefId = DefId::new(1);
const DEPRECATED: DefId = DefId::new(2);
const MAP_KEY: DefId = DefId::new(3);
const TYPE_META: DefId = DefId::new(4);
const NODE: DefId = DefId::new(5);
const QUERY: DefId = DefId::new(6);
const COLOR: DefId = DefId::new(7);
const FILTER: DefId = DefId::new(8);
const PAGE: DefId = DefId::new(9);
const SEARCH: DefId = DefId::new(10);
const CACHE: DefId = DefId::new(11);
const PHOTO: DefId = DefId::new(12);
const TAGS: DefId = DefId::new(13);
const POINT: DefId = DefId::new(14);

// Members.
const REASON: MemberId = MemberId::new(0);
const NODE_ID: MemberId = MemberId::new(1);
const QUERY_NODE: MemberId = MemberId::new(2);
const QUERY_VERSION: MemberId = MemberId::new(3);
const NODE_ARG_ID: MemberId = MemberId::new(4);
const RED: MemberId = MemberId::new(5);
const GREEN: MemberId = MemberId::new(6);
const FILTER_TAGS: MemberId = MemberId::new(7);
const FILTER_COLOR: MemberId = MemberId::new(8);
const PAGE_ITEMS: MemberId = MemberId::new(9);
const CACHE_TTL: MemberId = MemberId::new(10);
const CACHE_TAGS: MemberId = MemberId::new(11);
const POINT_X: MemberId = MemberId::new(12);
const POINT_Y: MemberId = MemberId::new(13);

// Type occurrences, in the walk's order.
const REASON_TYPE: TypeExprId = TypeExprId::new(0);
const NODE_ID_TYPE: TypeExprId = TypeExprId::new(1);
const QUERY_IMPLEMENTS: TypeExprId = TypeExprId::new(2);
const NODE_TYPE: TypeExprId = TypeExprId::new(3);
const NODE_ARG_TYPE: TypeExprId = TypeExprId::new(4);
const VERSION_TYPE: TypeExprId = TypeExprId::new(5);
const TAGS_TYPE: TypeExprId = TypeExprId::new(6);
const TAGS_ELEMENT: TypeExprId = TypeExprId::new(7);
const COLOR_TYPE: TypeExprId = TypeExprId::new(8);
const T_DEFAULT: TypeExprId = TypeExprId::new(9);
const T_BOUND: TypeExprId = TypeExprId::new(10);
const ITEMS_TYPE: TypeExprId = TypeExprId::new(11);
const ITEMS_ELEMENT: TypeExprId = TypeExprId::new(12);
const SEARCH_QUERY: TypeExprId = TypeExprId::new(13);
const SEARCH_PAGE: TypeExprId = TypeExprId::new(14);
const SEARCH_PAGE_ARG: TypeExprId = TypeExprId::new(15);
const TTL_TYPE: TypeExprId = TypeExprId::new(16);
const CACHE_TAGS_TYPE: TypeExprId = TypeExprId::new(17);
const CACHE_TAGS_ELEMENT: TypeExprId = TypeExprId::new(18);
const X_TYPE: TypeExprId = TypeExprId::new(19);
const Y_TYPE: TypeExprId = TypeExprId::new(20);
const QUERY_ROOT: TypeExprId = TypeExprId::new(21);
const CACHE_TYPE_ARG: TypeExprId = TypeExprId::new(22);

// Types, in the order the walk first needs them.
const STRING_TY: TyId = TyId::new(0);
const I32_NON_NULL_TY: TyId = TyId::new(1);
const NODE_TY: TyId = TyId::new(2);
const I32_TY: TyId = TyId::new(3);
const STRINGS_TY: TyId = TyId::new(4);
const COLOR_TY: TyId = TyId::new(5);
const T_TY: TyId = TyId::new(6);
const TS_TY: TyId = TyId::new(7);
const QUERY_TY: TyId = TyId::new(8);
const PAGE_QUERY_TY: TyId = TyId::new(9);
const UNRESOLVED_TY: TyId = TyId::new(10);

// Values, in the walk's order.
const REASON_DEFAULT: ValueId = ValueId::new(0);
const TTL_VALUE: ValueId = ValueId::new(1);
const TAGS_COPY: ValueId = ValueId::new(2);
const TAGS_COPY_ITEM: ValueId = ValueId::new(3);
const NODE_ARG_DEFAULT: ValueId = ValueId::new(4);
const REASON_COPY: ValueId = ValueId::new(5);
const COLOR_DEFAULT: ValueId = ValueId::new(6);
const CACHE_TAGS_DEFAULT: ValueId = ValueId::new(7);
const CACHE_TAGS_DEFAULT_ITEM: ValueId = ValueId::new(8);
const EVERYTHING: ValueId = ValueId::new(9);
const V_NULL: ValueId = ValueId::new(10);
const V_BOOL: ValueId = ValueId::new(11);
const V_NEGATIVE: ValueId = ValueId::new(12);
const V_FLOAT: ValueId = ValueId::new(13);
const V_STRING: ValueId = ValueId::new(14);
const V_ENUM: ValueId = ValueId::new(15);
const V_TYPE: ValueId = ValueId::new(16);
const V_LIST: ValueId = ValueId::new(17);
const V_LIST_ITEM: ValueId = ValueId::new(18);
const V_SET: ValueId = ValueId::new(19);
const V_SET_ITEM: ValueId = ValueId::new(20);
const V_MAP: ValueId = ValueId::new(21);
const V_MAP_KEY: ValueId = ValueId::new(22);
const V_MAP_VALUE: ValueId = ValueId::new(23);
const V_OBJECT: ValueId = ValueId::new(24);
const V_OBJECT_TAGS: ValueId = ValueId::new(25);
const V_OBJECT_COLOR: ValueId = ValueId::new(26);
const V_LITERAL: ValueId = ValueId::new(27);
const V_UNRESOLVED: ValueId = ValueId::new(28);
const V_WIDE: ValueId = ValueId::new(29);
const V_MIN: ValueId = ValueId::new(30);

// Applications, in the walk's order.
const QUERY_CACHE: ApplicationId = ApplicationId::new(0);
const VERSION_DEPRECATED: ApplicationId = ApplicationId::new(1);
const POINT_MISSING: ApplicationId = ApplicationId::new(2);
const SCHEMA_CACHE: ApplicationId = ApplicationId::new(3);

/// `start..end` of source `source`.
fn at(source: u32, start: usize, end: usize) -> Location {
  Location::new(source, SimpleSpan::new(start, end))
}

/// Appends a definition named `name`.
fn def(
  b: &mut ModelBuilder,
  module: ModuleId,
  kind: DefKind,
  name: &str,
  location: Location,
  origin: Origin,
) -> DefId {
  let name = b.intern(name);
  b.push_def(module, kind, name, location, origin)
}

/// Appends a direct member named `name` of `owner`'s contribution `contribution`.
fn member(
  b: &mut ModelBuilder,
  owner: DefId,
  contribution: u32,
  kind: MemberKind,
  name: &str,
  location: Location,
  extent: Location,
) -> MemberId {
  let name = b.intern(name);
  b.push_member(owner, contribution, kind, name, location, extent)
}

/// Appends a type occurrence that names `def` through the relative path `name`, of type `ty`.
fn named(
  b: &mut ModelBuilder,
  location: Location,
  non_null: bool,
  name: &str,
  def: DefId,
  ty: TyId,
) -> TypeExprId {
  let id = b.push_type_expr(location, non_null);
  let segment = b.intern(name);
  b.set_type_expr_path(id, PathRoot::Relative, &[segment], location);
  b.resolve_named(id, def);
  b.set_type_expr_ty(id, ty);
  id
}

/// Appends a type occurrence whose relative path `name` resolved to nothing.
fn unresolved(b: &mut ModelBuilder, location: Location, name: &str) -> TypeExprId {
  let id = b.push_type_expr(location, false);
  let segment = b.intern(name);
  b.set_type_expr_path(id, PathRoot::Relative, &[segment], location);
  id
}

/// Appends a string value.
fn string(b: &mut ModelBuilder, location: Location, written: bool, text: &str) -> ValueId {
  let id = b.push_value(location, written);
  b.set_string(id, text);
  id
}

/// Appends a written integer value.
fn int(
  b: &mut ModelBuilder,
  location: Location,
  negative: bool,
  magnitude: &[u8],
  text: &str,
) -> ValueId {
  let id = b.push_value(location, true);
  b.set_int(id, negative, magnitude, text);
  id
}

/// Appends an enum value, written as the relative path `name`.
fn enum_value(
  b: &mut ModelBuilder,
  location: Location,
  written: bool,
  name: &str,
  def: DefId,
  value: MemberId,
) -> ValueId {
  let id = b.push_value(location, written);
  b.set_enum(id, def, value);
  let segment = b.intern(name);
  b.set_value_path(id, PathRoot::Relative, &[segment]);
  id
}

/// Appends an application of the directive written as the relative path `name`.
fn application(
  b: &mut ModelBuilder,
  location: Location,
  name: &str,
  path_location: Location,
) -> ApplicationId {
  let segment = b.intern(name);
  b.push_application(location, PathRoot::Relative, &[segment], path_location)
}

/// A model with one of each kind of entity, appended in the order the builder requires.
///
/// The packages are `core`, `alloc`, `std`, the root `app` and its dependency `geo`. `core` holds a
/// materialised `i32`, `string`, `@deprecated`, the marker `MapKey` and a meta-type. `app` holds,
/// in its root module, an interface, an object with an extension, an enum, an input object, a
/// generic object, a union and a directive, and in `app::media` two scalars; `app::media::vision`
/// is an inline module with nothing in it. `geo` holds an object whose two field types did not
/// resolve. The schema has a written root, a presumed root, a description and an application whose
/// argument is a list of one value of every kind.
fn fixture() -> Model {
  let mut b = ModelBuilder::default();

  // Packages, each with its package source and then its files.
  assert_eq!(
    b.push_package(
      "core",
      PackageKind::BuiltIn,
      Tier::Core,
      Layout::Directory,
      2
    ),
    CORE
  );
  assert_eq!(
    b.push_package(
      "alloc",
      PackageKind::BuiltIn,
      Tier::Alloc,
      Layout::Directory,
      1
    ),
    ALLOC
  );
  assert_eq!(
    b.push_package("std", PackageKind::BuiltIn, Tier::Std, Layout::Directory, 1),
    STD
  );
  assert_eq!(
    b.push_package("app", PackageKind::Root, Tier::Std, Layout::Directory, 3),
    APP
  );
  assert_eq!(
    b.push_package("geo", PackageKind::Dependency, Tier::Core, Layout::Flat, 2),
    GEO
  );
  b.push_dependency(APP, GEO);
  b.set_integers(GEO, IntegerProvider::numo());

  // Modules, by package, then path.
  assert_eq!(b.push_module(CORE, None, None, ModuleKind::Root), CORE_ROOT);
  assert_eq!(
    b.push_module(ALLOC, None, None, ModuleKind::Root),
    ALLOC_ROOT
  );
  assert_eq!(b.push_module(STD, None, None, ModuleKind::Root), STD_ROOT);
  assert_eq!(b.push_module(APP, None, None, ModuleKind::Root), APP_ROOT);
  assert_eq!(
    b.push_module(APP, Some(APP_ROOT), Some("media"), ModuleKind::Directory),
    MEDIA
  );
  assert_eq!(
    b.push_module(APP, Some(MEDIA), Some("vision"), ModuleKind::Inline),
    VISION
  );
  assert_eq!(b.push_module(GEO, None, None, ModuleKind::Root), GEO_ROOT);
  b.set_module_location(VISION, at(APP_MEDIA, 100, 106));
  b.set_module_description(VISION, "Vision.", at(APP_MEDIA, 90, 99));

  // Definitions, by module, then source, then position: `core`'s materialised `i32` first.
  let whole_core = Location::entire(CORE_SOURCE);
  let defs = [
    (
      CORE_ROOT,
      DefKind::Scalar,
      "i32",
      whole_core,
      Origin::BuiltIn,
      I32,
    ),
    (
      CORE_ROOT,
      DefKind::Scalar,
      "string",
      at(CORE_LIB, 7, 13),
      Origin::BuiltIn,
      STRING,
    ),
    (
      CORE_ROOT,
      DefKind::Directive,
      "deprecated",
      at(CORE_LIB, 30, 40),
      Origin::BuiltIn,
      DEPRECATED,
    ),
    (
      CORE_ROOT,
      DefKind::Interface,
      "MapKey",
      at(CORE_LIB, 100, 106),
      Origin::BuiltIn,
      MAP_KEY,
    ),
    (
      CORE_ROOT,
      DefKind::Object,
      "__Type",
      at(CORE_LIB, 130, 136),
      Origin::Compat,
      TYPE_META,
    ),
    (
      APP_ROOT,
      DefKind::Interface,
      "Node",
      at(APP_SCHEMA, 100, 104),
      Origin::User,
      NODE,
    ),
    (
      APP_ROOT,
      DefKind::Object,
      "Query",
      at(APP_SCHEMA, 215, 220),
      Origin::User,
      QUERY,
    ),
    (
      APP_ROOT,
      DefKind::Enum,
      "Color",
      at(APP_SCHEMA, 400, 405),
      Origin::User,
      COLOR,
    ),
    (
      APP_ROOT,
      DefKind::InputObject,
      "Filter",
      at(APP_SCHEMA, 500, 506),
      Origin::User,
      FILTER,
    ),
    (
      APP_ROOT,
      DefKind::Object,
      "Page",
      at(APP_SCHEMA, 600, 604),
      Origin::User,
      PAGE,
    ),
    (
      APP_ROOT,
      DefKind::Union,
      "Search",
      at(APP_SCHEMA, 700, 706),
      Origin::User,
      SEARCH,
    ),
    (
      APP_ROOT,
      DefKind::Directive,
      "cache",
      at(APP_SCHEMA, 800, 805),
      Origin::User,
      CACHE,
    ),
    (
      MEDIA,
      DefKind::Scalar,
      "Photo",
      at(APP_MEDIA, 10, 15),
      Origin::User,
      PHOTO,
    ),
    (
      MEDIA,
      DefKind::Scalar,
      "Tags",
      at(APP_MEDIA, 30, 34),
      Origin::User,
      TAGS,
    ),
    (
      GEO_ROOT,
      DefKind::Object,
      "Point",
      at(GEO_POINT, 10, 15),
      Origin::User,
      POINT,
    ),
  ];
  for (module, kind, name, location, origin, expected) in defs {
    assert_eq!(def(&mut b, module, kind, name, location, origin), expected);
  }

  // Each definition's contributions, in definition order: `Query` has its definition and an
  // extension. A contribution spans from its first keyword to its last token.
  for (owner, location, extent) in [
    (I32, whole_core, whole_core),
    (STRING, at(CORE_LIB, 7, 13), at(CORE_LIB, 0, 14)),
    (DEPRECATED, at(CORE_LIB, 30, 40), at(CORE_LIB, 20, 90)),
    (MAP_KEY, at(CORE_LIB, 100, 106), at(CORE_LIB, 90, 120)),
    (TYPE_META, at(CORE_LIB, 130, 136), at(CORE_LIB, 125, 140)),
    (NODE, at(APP_SCHEMA, 100, 104), at(APP_SCHEMA, 95, 120)),
  ] {
    assert_eq!(b.push_contribution(owner, location, extent), 0);
  }
  assert_eq!(
    b.push_contribution(QUERY, at(APP_SCHEMA, 215, 220), at(APP_SCHEMA, 200, 300)),
    0
  );
  assert_eq!(
    b.push_contribution(QUERY, at(APP_SCHEMA, 900, 905), at(APP_SCHEMA, 890, 960)),
    1
  );
  for (owner, location, extent) in [
    (COLOR, at(APP_SCHEMA, 400, 405), at(APP_SCHEMA, 395, 425)),
    (FILTER, at(APP_SCHEMA, 500, 506), at(APP_SCHEMA, 495, 550)),
    (PAGE, at(APP_SCHEMA, 600, 604), at(APP_SCHEMA, 595, 655)),
    (SEARCH, at(APP_SCHEMA, 700, 706), at(APP_SCHEMA, 695, 730)),
    (CACHE, at(APP_SCHEMA, 800, 805), at(APP_SCHEMA, 795, 860)),
    (PHOTO, at(APP_MEDIA, 10, 15), at(APP_MEDIA, 3, 25)),
    (TAGS, at(APP_MEDIA, 30, 34), at(APP_MEDIA, 26, 40)),
    (POINT, at(GEO_POINT, 10, 15), at(GEO_POINT, 5, 60)),
  ] {
    assert_eq!(b.push_contribution(owner, location, extent), 0);
  }
  assert_eq!(
    b.push_schema_contribution(at(APP_SCHEMA, 1000, 1006), at(APP_SCHEMA, 990, 1100)),
    0
  );

  // What the definitions mean, beyond their members.
  b.set_intrinsic(I32, Primitive::signed(32));
  b.set_marker(MAP_KEY);
  b.set_collection(TAGS, Collection::Set);
  b.set_def_tier(PHOTO, Tier::Alloc);
  b.push_extern(PHOTO, "rust", "image::Photo", Tier::Std);
  b.set_def_description(QUERY, "The root.", at(APP_SCHEMA, 200, 210));
  b.push_directive_location(DEPRECATED, DirectiveLocation::FieldDefinition);
  b.push_directive_location(DEPRECATED, DirectiveLocation::EnumValue);
  b.push_directive_location(CACHE, DirectiveLocation::Object);
  b.push_directive_location(CACHE, DirectiveLocation::FieldDefinition);
  b.set_repeatable(CACHE);
  let t = b.intern("T");
  assert_eq!(b.push_type_param(PAGE, t, at(APP_SCHEMA, 605, 606)), 0);

  // Members, owner by owner: direct members by contribution, then each field's arguments.
  let members = [
    (
      DEPRECATED,
      0,
      MemberKind::Argument,
      "reason",
      at(CORE_LIB, 41, 47),
      at(CORE_LIB, 41, 85),
      REASON,
    ),
    (
      NODE,
      0,
      MemberKind::Field,
      "id",
      at(APP_SCHEMA, 106, 108),
      at(APP_SCHEMA, 106, 115),
      NODE_ID,
    ),
    (
      QUERY,
      0,
      MemberKind::Field,
      "node",
      at(APP_SCHEMA, 250, 254),
      at(APP_SCHEMA, 250, 280),
      QUERY_NODE,
    ),
    (
      QUERY,
      1,
      MemberKind::Field,
      "version",
      at(APP_SCHEMA, 910, 917),
      at(APP_SCHEMA, 910, 940),
      QUERY_VERSION,
    ),
  ];
  for (owner, contribution, kind, name, location, extent, expected) in members {
    assert_eq!(
      member(&mut b, owner, contribution, kind, name, location, extent),
      expected
    );
  }
  let id = b.intern("id");
  assert_eq!(
    b.push_argument(
      QUERY_NODE,
      id,
      at(APP_SCHEMA, 255, 257),
      at(APP_SCHEMA, 255, 266)
    ),
    NODE_ARG_ID
  );
  let members = [
    (
      COLOR,
      MemberKind::EnumValue,
      "RED",
      at(APP_SCHEMA, 410, 413),
      RED,
    ),
    (
      COLOR,
      MemberKind::EnumValue,
      "GREEN",
      at(APP_SCHEMA, 415, 420),
      GREEN,
    ),
    (
      FILTER,
      MemberKind::InputField,
      "tags",
      at(APP_SCHEMA, 510, 514),
      FILTER_TAGS,
    ),
    (
      FILTER,
      MemberKind::InputField,
      "color",
      at(APP_SCHEMA, 530, 535),
      FILTER_COLOR,
    ),
    (
      PAGE,
      MemberKind::Field,
      "items",
      at(APP_SCHEMA, 640, 645),
      PAGE_ITEMS,
    ),
    (
      CACHE,
      MemberKind::Argument,
      "ttl",
      at(APP_SCHEMA, 806, 809),
      CACHE_TTL,
    ),
    (
      CACHE,
      MemberKind::Argument,
      "tags",
      at(APP_SCHEMA, 816, 820),
      CACHE_TAGS,
    ),
    (
      POINT,
      MemberKind::Field,
      "x",
      at(GEO_POINT, 26, 27),
      POINT_X,
    ),
    (
      POINT,
      MemberKind::Field,
      "y",
      at(GEO_POINT, 40, 41),
      POINT_Y,
    ),
  ];
  for (owner, kind, name, location, expected) in members {
    assert_eq!(
      member(&mut b, owner, 0, kind, name, location, location),
      expected
    );
  }
  b.set_member_description(RED, "", at(APP_SCHEMA, 407, 409));

  // Types, in the order the walk first needs them: a type's arguments before it.
  assert_eq!(b.intern_named(STRING, &[], false), STRING_TY);
  assert_eq!(b.intern_named(I32, &[], true), I32_NON_NULL_TY);
  assert_eq!(b.intern_named(NODE, &[], false), NODE_TY);
  assert_eq!(b.intern_named(I32, &[], false), I32_TY);
  assert_eq!(b.intern_list(STRING_TY, false), STRINGS_TY);
  assert_eq!(b.intern_named(COLOR, &[], false), COLOR_TY);
  assert_eq!(b.intern_param(PAGE, 0, true), T_TY);
  assert_eq!(b.intern_list(T_TY, true), TS_TY);
  assert_eq!(b.intern_named(QUERY, &[], false), QUERY_TY);
  assert_eq!(b.intern_named(PAGE, &[QUERY_TY], false), PAGE_QUERY_TY);
  assert_eq!(b.intern_unresolved(), UNRESOLVED_TY);
  // Interning a type again answers the type it already is.
  assert_eq!(b.intern_named(NODE, &[], false), NODE_TY);
  assert_eq!(b.intern_list(T_TY, true), TS_TY);

  // The walk. `deprecated`: `reason: string = "No longer supported"`.
  assert_eq!(
    named(
      &mut b,
      at(CORE_LIB, 49, 55),
      false,
      "string",
      STRING,
      STRING_TY
    ),
    REASON_TYPE
  );
  b.set_member_type(REASON, REASON_TYPE);
  assert_eq!(
    string(&mut b, at(CORE_LIB, 58, 80), true, "No longer supported"),
    REASON_DEFAULT
  );
  b.set_member_default(REASON, REASON_DEFAULT);

  // `Node`: `id: i32!`.
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 110, 114),
      true,
      "i32",
      I32,
      I32_NON_NULL_TY
    ),
    NODE_ID_TYPE
  );
  b.set_member_type(NODE_ID, NODE_ID_TYPE);

  // `Query implements Node @cache(ttl: 60) { node(id: i32 = 1): Node }`, extended with
  // `version: string @deprecated`.
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 225, 229),
      false,
      "Node",
      NODE,
      NODE_TY
    ),
    QUERY_IMPLEMENTS
  );
  b.push_implements(QUERY, 0, QUERY_IMPLEMENTS);
  assert_eq!(
    application(
      &mut b,
      at(APP_SCHEMA, 230, 245),
      "cache",
      at(APP_SCHEMA, 231, 236)
    ),
    QUERY_CACHE
  );
  b.resolve_application(QUERY_CACHE, CACHE);
  b.push_def_application(QUERY, 0, QUERY_CACHE);
  assert_eq!(
    int(&mut b, at(APP_SCHEMA, 242, 244), false, &[60], "60"),
    TTL_VALUE
  );
  b.set_value_ty(TTL_VALUE, I32_TY);
  b.push_application_argument(
    QUERY_CACHE,
    CACHE_TTL,
    Some(at(APP_SCHEMA, 237, 240)),
    TTL_VALUE,
  );
  // `tags` is not written: a copy of its default fills it, at the default's own location.
  assert_eq!(b.push_value(at(APP_SCHEMA, 833, 838), false), TAGS_COPY);
  assert_eq!(
    string(&mut b, at(APP_SCHEMA, 834, 837), false, "a"),
    TAGS_COPY_ITEM
  );
  b.set_list(TAGS_COPY, &[TAGS_COPY_ITEM]);
  b.push_application_argument(QUERY_CACHE, CACHE_TAGS, None, TAGS_COPY);
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 270, 274),
      false,
      "Node",
      NODE,
      NODE_TY
    ),
    NODE_TYPE
  );
  b.set_member_type(QUERY_NODE, NODE_TYPE);
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 259, 262), false, "i32", I32, I32_TY),
    NODE_ARG_TYPE
  );
  b.set_member_type(NODE_ARG_ID, NODE_ARG_TYPE);
  assert_eq!(
    int(&mut b, at(APP_SCHEMA, 265, 266), false, &[1], "1"),
    NODE_ARG_DEFAULT
  );
  b.set_member_default(NODE_ARG_ID, NODE_ARG_DEFAULT);
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 919, 925),
      false,
      "string",
      STRING,
      STRING_TY
    ),
    VERSION_TYPE
  );
  b.set_member_type(QUERY_VERSION, VERSION_TYPE);
  assert_eq!(
    application(
      &mut b,
      at(APP_SCHEMA, 926, 937),
      "deprecated",
      at(APP_SCHEMA, 927, 937)
    ),
    VERSION_DEPRECATED
  );
  b.resolve_application(VERSION_DEPRECATED, DEPRECATED);
  b.push_member_application(QUERY_VERSION, VERSION_DEPRECATED);
  assert_eq!(
    string(&mut b, at(CORE_LIB, 58, 80), false, "No longer supported"),
    REASON_COPY
  );
  b.push_application_argument(VERSION_DEPRECATED, REASON, None, REASON_COPY);

  // `Filter`: `tags: [string]`, `color: Color = RED`.
  assert_eq!(b.push_type_expr(at(APP_SCHEMA, 516, 524), false), TAGS_TYPE);
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 517, 523),
      false,
      "string",
      STRING,
      STRING_TY
    ),
    TAGS_ELEMENT
  );
  b.set_type_expr_list(TAGS_TYPE, TAGS_ELEMENT);
  b.set_type_expr_ty(TAGS_TYPE, STRINGS_TY);
  b.set_member_type(FILTER_TAGS, TAGS_TYPE);
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 537, 542),
      false,
      "Color",
      COLOR,
      COLOR_TY
    ),
    COLOR_TYPE
  );
  b.set_member_type(FILTER_COLOR, COLOR_TYPE);
  assert_eq!(
    enum_value(&mut b, at(APP_SCHEMA, 545, 548), true, "RED", COLOR, RED),
    COLOR_DEFAULT
  );
  b.set_member_default(FILTER_COLOR, COLOR_DEFAULT);

  // `Page<T = string> where T: Node { items: [T!]! }`.
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 609, 615),
      false,
      "string",
      STRING,
      STRING_TY
    ),
    T_DEFAULT
  );
  b.set_type_param_default(PAGE, 0, T_DEFAULT);
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 626, 630),
      false,
      "Node",
      NODE,
      NODE_TY
    ),
    T_BOUND
  );
  b.push_bound(PAGE, 0, T_BOUND, at(APP_SCHEMA, 623, 630));
  assert_eq!(b.push_type_expr(at(APP_SCHEMA, 647, 652), true), ITEMS_TYPE);
  assert_eq!(
    b.push_type_expr(at(APP_SCHEMA, 648, 650), true),
    ITEMS_ELEMENT
  );
  b.set_type_expr_path(
    ITEMS_ELEMENT,
    PathRoot::Relative,
    &[t],
    at(APP_SCHEMA, 648, 649),
  );
  b.resolve_param(ITEMS_ELEMENT, PAGE, 0);
  b.set_type_expr_ty(ITEMS_ELEMENT, T_TY);
  b.set_type_expr_list(ITEMS_TYPE, ITEMS_ELEMENT);
  b.set_type_expr_ty(ITEMS_TYPE, TS_TY);
  b.set_member_type(PAGE_ITEMS, ITEMS_TYPE);
  b.push_instantiation(PAGE, PAGE_QUERY_TY);
  b.push_instantiation(PAGE, PAGE_QUERY_TY);

  // `union Search = Query | Page<Query>`.
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 709, 714),
      false,
      "Query",
      QUERY,
      QUERY_TY
    ),
    SEARCH_QUERY
  );
  b.push_union_member(SEARCH, 0, SEARCH_QUERY);
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 717, 728),
      false,
      "Page",
      PAGE,
      PAGE_QUERY_TY
    ),
    SEARCH_PAGE
  );
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 722, 727),
      false,
      "Query",
      QUERY,
      QUERY_TY
    ),
    SEARCH_PAGE_ARG
  );
  b.set_type_expr_args(SEARCH_PAGE, &[SEARCH_PAGE_ARG]);
  b.push_union_member(SEARCH, 0, SEARCH_PAGE);

  // `directive @cache(ttl: i32, tags: [string] = ["a"]) repeatable on OBJECT | FIELD_DEFINITION`.
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 811, 814), false, "i32", I32, I32_TY),
    TTL_TYPE
  );
  b.set_member_type(CACHE_TTL, TTL_TYPE);
  assert_eq!(
    b.push_type_expr(at(APP_SCHEMA, 822, 830), false),
    CACHE_TAGS_TYPE
  );
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 823, 829),
      false,
      "string",
      STRING,
      STRING_TY
    ),
    CACHE_TAGS_ELEMENT
  );
  b.set_type_expr_list(CACHE_TAGS_TYPE, CACHE_TAGS_ELEMENT);
  b.set_type_expr_ty(CACHE_TAGS_TYPE, STRINGS_TY);
  b.set_member_type(CACHE_TAGS, CACHE_TAGS_TYPE);
  assert_eq!(
    b.push_value(at(APP_SCHEMA, 833, 838), true),
    CACHE_TAGS_DEFAULT
  );
  assert_eq!(
    string(&mut b, at(APP_SCHEMA, 834, 837), true, "a"),
    CACHE_TAGS_DEFAULT_ITEM
  );
  b.set_list(CACHE_TAGS_DEFAULT, &[CACHE_TAGS_DEFAULT_ITEM]);
  b.set_member_default(CACHE_TAGS, CACHE_TAGS_DEFAULT);

  // `geo`'s `Point @missing { x: Adress, y: Strng }`: the directive and both types are misspelt.
  assert_eq!(
    application(
      &mut b,
      at(GEO_POINT, 16, 24),
      "missing",
      at(GEO_POINT, 17, 24)
    ),
    POINT_MISSING
  );
  b.push_def_application(POINT, 0, POINT_MISSING);
  assert_eq!(unresolved(&mut b, at(GEO_POINT, 29, 35), "Adress"), X_TYPE);
  b.set_type_expr_ty(X_TYPE, UNRESOLVED_TY);
  b.set_member_type(POINT_X, X_TYPE);
  // `y`'s type is given none: finishing the model gives it the one unresolved type.
  assert_eq!(unresolved(&mut b, at(GEO_POINT, 43, 48), "Strng"), Y_TYPE);
  b.set_member_type(POINT_Y, Y_TYPE);

  // The schema: `query: Query` written, the mutation root presumed, and `@cache<i32>(ttl: […])`
  // with one value of every kind.
  b.set_schema_description("The schema.", at(APP_SCHEMA, 990, 999));
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 1015, 1020),
      false,
      "Query",
      QUERY,
      QUERY_TY
    ),
    QUERY_ROOT
  );
  b.set_schema_root(RootOperation::Query, Root::Written(QUERY_ROOT));
  b.set_schema_root(RootOperation::Mutation, Root::Presumed(POINT));
  assert_eq!(
    application(
      &mut b,
      at(APP_SCHEMA, 1030, 1095),
      "cache",
      at(APP_SCHEMA, 1031, 1036)
    ),
    SCHEMA_CACHE
  );
  b.resolve_application(SCHEMA_CACHE, CACHE);
  assert_eq!(
    named(
      &mut b,
      at(APP_SCHEMA, 1037, 1040),
      false,
      "i32",
      I32,
      I32_TY
    ),
    CACHE_TYPE_ARG
  );
  b.set_application_type_args(SCHEMA_CACHE, &[CACHE_TYPE_ARG]);
  b.push_schema_application(0, SCHEMA_CACHE);
  assert_eq!(b.push_value(at(APP_SCHEMA, 1047, 1094), true), EVERYTHING);
  assert_eq!(b.push_value(at(APP_SCHEMA, 1048, 1052), true), V_NULL);
  b.set_null(V_NULL);
  assert_eq!(b.push_value(at(APP_SCHEMA, 1054, 1058), true), V_BOOL);
  b.set_bool(V_BOOL, true);
  assert_eq!(
    int(&mut b, at(APP_SCHEMA, 1060, 1063), true, &[42], "-42"),
    V_NEGATIVE
  );
  assert_eq!(b.push_value(at(APP_SCHEMA, 1064, 1067), true), V_FLOAT);
  b.set_float(V_FLOAT, 1.5, "1.5");
  assert_eq!(
    string(&mut b, at(APP_SCHEMA, 1068, 1072), true, "hi"),
    V_STRING
  );
  assert_eq!(
    enum_value(
      &mut b,
      at(APP_SCHEMA, 1073, 1078),
      true,
      "GREEN",
      COLOR,
      GREEN
    ),
    V_ENUM
  );
  assert_eq!(b.push_value(at(APP_SCHEMA, 1079, 1080), true), V_TYPE);
  b.set_type_value(V_TYPE, POINT);
  let geo = b.intern("geo");
  let point = b.intern("Point");
  b.set_value_path(V_TYPE, PathRoot::Dependency, &[geo, point]);
  assert_eq!(b.push_value(at(APP_SCHEMA, 1081, 1084), true), V_LIST);
  assert_eq!(
    int(&mut b, at(APP_SCHEMA, 1082, 1083), false, &[1], "1"),
    V_LIST_ITEM
  );
  b.set_list(V_LIST, &[V_LIST_ITEM]);
  assert_eq!(b.push_value(at(APP_SCHEMA, 1084, 1085), true), V_SET);
  assert_eq!(b.push_value(at(APP_SCHEMA, 1084, 1085), true), V_SET_ITEM);
  b.set_bool(V_SET_ITEM, false);
  b.set_set(V_SET, &[V_SET_ITEM]);
  assert_eq!(b.push_value(at(APP_SCHEMA, 1085, 1086), true), V_MAP);
  assert_eq!(
    string(&mut b, at(APP_SCHEMA, 1085, 1086), true, "k"),
    V_MAP_KEY
  );
  assert_eq!(
    int(&mut b, at(APP_SCHEMA, 1085, 1086), false, &[2, 0, 0], "2"),
    V_MAP_VALUE
  );
  b.set_map(V_MAP, &[(V_MAP_KEY, V_MAP_VALUE)]);
  assert_eq!(b.push_value(at(APP_SCHEMA, 1086, 1088), true), V_OBJECT);
  assert_eq!(
    b.push_value(at(APP_SCHEMA, 1087, 1088), true),
    V_OBJECT_TAGS
  );
  b.set_list(V_OBJECT_TAGS, &[]);
  assert_eq!(
    enum_value(&mut b, at(APP_SCHEMA, 545, 548), false, "RED", COLOR, RED),
    V_OBJECT_COLOR
  );
  b.set_object(
    V_OBJECT,
    &[
      (FILTER_TAGS, Some(at(APP_SCHEMA, 1086, 1087)), V_OBJECT_TAGS),
      (FILTER_COLOR, None, V_OBJECT_COLOR),
    ],
  );
  assert_eq!(b.push_value(at(APP_SCHEMA, 1088, 1092), true), V_LITERAL);
  b.set_literal(V_LITERAL, LiteralKind::Int, "0xFF");
  assert_eq!(b.push_value(at(APP_SCHEMA, 1092, 1093), true), V_UNRESOLVED);
  let mut wide = [0u8; 17];
  wide[16] = 1;
  assert_eq!(
    int(
      &mut b,
      at(APP_SCHEMA, 1093, 1094),
      false,
      &wide,
      "340282366920938463463374607431768211456"
    ),
    V_WIDE
  );
  let mut min = [0u8; 16];
  min[15] = 0x80;
  assert_eq!(
    int(
      &mut b,
      at(APP_SCHEMA, 1093, 1094),
      true,
      &min,
      "-170141183460469231731687303715884105728"
    ),
    V_MIN
  );
  b.set_list(
    EVERYTHING,
    &[
      V_NULL,
      V_BOOL,
      V_NEGATIVE,
      V_FLOAT,
      V_STRING,
      V_ENUM,
      V_TYPE,
      V_LIST,
      V_SET,
      V_MAP,
      V_OBJECT,
      V_LITERAL,
      V_UNRESOLVED,
      V_WIDE,
      V_MIN,
    ],
  );
  b.push_application_argument(
    SCHEMA_CACHE,
    CACHE_TTL,
    Some(at(APP_SCHEMA, 1042, 1045)),
    EVERYTHING,
  );

  // What the instantiation phase adds, a second name, and one reported error.
  b.push_satisfaction(QUERY_TY, NODE);
  b.push_satisfaction(I32_TY, MAP_KEY);
  b.push_satisfaction(QUERY_TY, NODE);
  let int_alias = b.intern("Int");
  b.push_alias(CORE_ROOT, int_alias, I32);
  b.mark(at(APP_SCHEMA, 912, 915));

  b.finish()
}

/// The ids of `range`, in order.
fn ids<I: IntoIterator>(range: I) -> Vec<I::Item> {
  range.into_iter().collect::<Vec<I::Item>>()
}

#[test]
fn packages_answer_their_kind_name_tier_layout_sources_dependencies_provider_and_root_module() {
  let model = fixture();
  assert_eq!(ids(model.packages()), vec![CORE, ALLOC, STD, APP, GEO]);
  assert_eq!(model.root_package(), APP);

  let expected = [
    (
      "core",
      PackageKind::BuiltIn,
      Tier::Core,
      Layout::Directory,
      0,
      2,
      CORE_ROOT,
    ),
    (
      "alloc",
      PackageKind::BuiltIn,
      Tier::Alloc,
      Layout::Directory,
      2,
      3,
      ALLOC_ROOT,
    ),
    (
      "std",
      PackageKind::BuiltIn,
      Tier::Std,
      Layout::Directory,
      3,
      4,
      STD_ROOT,
    ),
    (
      "app",
      PackageKind::Root,
      Tier::Std,
      Layout::Directory,
      4,
      7,
      APP_ROOT,
    ),
    (
      "geo",
      PackageKind::Dependency,
      Tier::Core,
      Layout::Flat,
      7,
      9,
      GEO_ROOT,
    ),
  ];
  for (id, (name, kind, tier, layout, start, end, root)) in
    model.packages().into_iter().zip(expected)
  {
    let package: PackageInfo<'_> = model.package(id);
    assert_eq!(package.id(), id);
    assert_eq!(package.name(), name);
    assert_eq!(package.kind(), kind);
    assert_eq!(package.tier(), tier);
    assert_eq!(package.layout(), layout);
    let sources: Vec<u32> = package
      .sources()
      .into_iter()
      .map(|source: SourceId| source.get())
      .collect::<Vec<u32>>();
    assert_eq!(
      sources,
      (start..end).collect::<Vec<u32>>(),
      "{name}'s sources"
    );
    assert_eq!(package.root_module(), root, "{name}'s root module");
  }
  assert_eq!(model.package(APP).dependencies(), &[GEO]);
  assert!(model.package(GEO).dependencies().is_empty());
  assert_eq!(
    model.package(GEO).integers(),
    Some(&IntegerProvider::numo())
  );
  assert_eq!(model.package(APP).integers(), None);
}

#[test]
fn analyze_gives_every_package_its_root_module() {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  let analysis = analyze(&root, &[], &Limits::new());
  let model = analysis.model();
  assert_eq!(model.packages().len(), 4);
  assert_eq!(model.modules().len(), 4);
  for (package, module) in model.packages().into_iter().zip(model.modules()) {
    assert_eq!(model.package(package).root_module(), module);
    let module = model.module(module);
    assert_eq!(module.package(), package);
    assert_eq!(module.kind(), ModuleKind::Root);
    assert!(module.path().is_empty());
    assert_eq!(module.parent(), None);
    assert!(module.children().is_empty());
    assert!(module.defs().is_empty());
  }
  assert!(model.defs().is_empty());
}

#[test]
fn modules_answer_their_package_path_parent_children_kind_location_description_and_defs() {
  let model = fixture();
  assert_eq!(
    ids(model.modules()),
    vec![
      CORE_ROOT, ALLOC_ROOT, STD_ROOT, APP_ROOT, MEDIA, VISION, GEO_ROOT
    ]
  );

  let root: Module<'_> = model.module(APP_ROOT);
  assert_eq!(root.id(), APP_ROOT);
  assert_eq!(root.package(), APP);
  assert!(root.path().is_empty());
  assert_eq!(root.parent(), None);
  assert_eq!(root.children(), &[MEDIA]);
  assert_eq!(root.kind(), ModuleKind::Root);
  assert_eq!(root.location(), None);
  assert_eq!(root.description(), None);
  assert_eq!(
    ids(root.defs()),
    vec![NODE, QUERY, COLOR, FILTER, PAGE, SEARCH, CACHE]
  );

  let media = model.module(MEDIA);
  let path: Vec<&str> = media
    .path()
    .iter()
    .map(|segment: &Sym| model.name(*segment))
    .collect::<Vec<&str>>();
  assert_eq!(path, vec!["media"]);
  assert_eq!(media.parent(), Some(APP_ROOT));
  assert_eq!(media.children(), &[VISION]);
  assert_eq!(media.kind(), ModuleKind::Directory);
  assert_eq!(ids(media.defs()), vec![PHOTO, TAGS]);

  let vision = model.module(VISION);
  let path: Vec<&str> = vision
    .path()
    .iter()
    .map(|segment: &Sym| model.name(*segment))
    .collect::<Vec<&str>>();
  assert_eq!(path, vec!["media", "vision"]);
  assert_eq!(vision.parent(), Some(MEDIA));
  assert!(vision.children().is_empty());
  assert_eq!(vision.kind(), ModuleKind::Inline);
  assert_eq!(vision.location(), Some(at(APP_MEDIA, 100, 106)));
  let description = vision.description().expect("the inline `mod` is described");
  assert_eq!(description.text(), "Vision.");
  assert_eq!(description.location(), at(APP_MEDIA, 90, 99));
  assert!(vision.defs().is_empty());

  assert_eq!(
    ids(model.module(CORE_ROOT).defs()),
    vec![I32, STRING, DEPRECATED, MAP_KEY, TYPE_META]
  );
  assert!(model.module(ALLOC_ROOT).defs().is_empty());
  assert!(model.module(STD_ROOT).defs().is_empty());
  assert_eq!(ids(model.module(GEO_ROOT).defs()), vec![POINT]);
}

/// One contribution, as everything it answers: its location, its members, its `implements`
/// entries, its applications, and whether it has errors.
type Contributed = (
  Location,
  Vec<MemberId>,
  Vec<TypeExprId>,
  Vec<ApplicationId>,
  bool,
);

/// What each of `def`'s contributions answers, in order.
fn contributed(def: Def<'_>) -> Vec<Contributed> {
  def
    .contributions()
    .map(|part: Contribution<'_>| {
      (
        part.location(),
        ids(part.members()),
        part.implements().to_vec(),
        part.applications().to_vec(),
        part.has_errors(),
      )
    })
    .collect::<Vec<Contributed>>()
}

#[test]
fn a_definition_answers_every_accessor() {
  let model = fixture();
  assert_eq!(model.defs().len(), 15);

  let query = model.def(QUERY);
  assert_eq!(query.id(), QUERY);
  assert_eq!(query.kind(), DefKind::Object);
  assert_eq!(query.name(), "Query");
  assert_eq!(model.name(query.sym()), "Query");
  assert_eq!(query.module(), APP_ROOT);
  assert_eq!(query.location(), at(APP_SCHEMA, 215, 220));
  assert_eq!(
    contributed(query),
    vec![
      (
        at(APP_SCHEMA, 215, 220),
        vec![QUERY_NODE],
        vec![QUERY_IMPLEMENTS],
        vec![QUERY_CACHE],
        false
      ),
      (
        at(APP_SCHEMA, 900, 905),
        vec![QUERY_VERSION],
        vec![],
        vec![],
        true
      ),
    ]
  );
  assert_eq!(query.contributions().len(), 2);
  let mut parts: Contributions<'_> = query.contributions();
  assert_eq!(
    parts
      .next_back()
      .map(|part: Contribution<'_>| part.location()),
    Some(at(APP_SCHEMA, 900, 905))
  );
  assert_eq!(parts.len(), 1);
  assert_eq!(
    parts.next().map(|part: Contribution<'_>| part.location()),
    Some(at(APP_SCHEMA, 215, 220))
  );
  assert!(parts.next().is_none());
  assert_eq!(query.origin(), Origin::User);
  assert!(query.has_errors());
  let description = query.description().expect("`Query` is described");
  assert_eq!(description.text(), "The root.");
  assert_eq!(description.location(), at(APP_SCHEMA, 200, 210));
  assert_eq!(query.type_params().len(), 0);
  assert!(query.bounds().is_empty());
  assert_eq!(query.implements(), &[QUERY_IMPLEMENTS]);
  assert_eq!(ids(query.fields()), vec![QUERY_NODE, QUERY_VERSION]);
  assert!(query.input_fields().is_empty());
  assert!(query.enum_values().is_empty());
  assert!(query.arguments().is_empty());
  assert!(query.union_members().is_empty());
  assert!(query.locations().is_empty());
  assert!(!query.is_repeatable());
  assert_eq!(query.applications(), &[QUERY_CACHE]);
  assert_eq!(query.tier(), Tier::Core);
  assert_eq!(query.intrinsic(), None);
  assert!(!query.is_marker());
  assert_eq!(query.collection(), None);
  assert_eq!(query.externs().len(), 0);

  let page = model.def(PAGE);
  let mut params: TypeParams<'_> = page.type_params();
  assert_eq!(params.len(), 1);
  let param: TypeParam<'_> = params.next().expect("`Page` has a type parameter");
  assert!(params.next().is_none());
  assert_eq!(param.name(), "T");
  assert_eq!(model.name(param.sym()), "T");
  assert_eq!(param.index(), 0);
  assert_eq!(param.location(), at(APP_SCHEMA, 605, 606));
  assert_eq!(param.default(), Some(T_DEFAULT));
  let bounds: &[TypeBound] = page.bounds();
  assert_eq!(bounds.len(), 1);
  assert_eq!(bounds[0].parameter(), 0);
  assert_eq!(bounds[0].interface(), T_BOUND);
  assert_eq!(bounds[0].location(), at(APP_SCHEMA, 623, 630));
  assert_eq!(ids(page.fields()), vec![PAGE_ITEMS]);
  assert!(!page.has_errors());

  let cache = model.def(CACHE);
  assert_eq!(cache.kind(), DefKind::Directive);
  assert_eq!(ids(cache.arguments()), vec![CACHE_TTL, CACHE_TAGS]);
  assert!(cache.fields().is_empty());
  assert_eq!(
    cache.locations(),
    &[
      DirectiveLocation::Object,
      DirectiveLocation::FieldDefinition
    ]
  );
  assert!(cache.is_repeatable());
  assert!(!model.def(DEPRECATED).is_repeatable());

  assert_eq!(ids(model.def(COLOR).enum_values()), vec![RED, GREEN]);
  assert_eq!(
    ids(model.def(FILTER).input_fields()),
    vec![FILTER_TAGS, FILTER_COLOR]
  );
  assert_eq!(
    model.def(SEARCH).union_members(),
    &[SEARCH_QUERY, SEARCH_PAGE]
  );
  assert_eq!(
    contributed(model.def(SEARCH)),
    vec![(at(APP_SCHEMA, 700, 706), vec![], vec![], vec![], false)]
  );
  let unions: Vec<Vec<TypeExprId>> = model
    .def(SEARCH)
    .contributions()
    .map(|part: Contribution<'_>| part.union_members().to_vec())
    .collect::<Vec<Vec<TypeExprId>>>();
  assert_eq!(unions, vec![vec![SEARCH_QUERY, SEARCH_PAGE]]);

  let i32_def = model.def(I32);
  assert_eq!(i32_def.location(), Location::entire(CORE_SOURCE));
  assert_eq!(i32_def.origin(), Origin::BuiltIn);
  assert_eq!(i32_def.intrinsic(), Some(Primitive::signed(32)));
  assert_eq!(
    contributed(i32_def),
    vec![(Location::entire(CORE_SOURCE), vec![], vec![], vec![], false)]
  );
  assert!(model.def(MAP_KEY).is_marker());
  assert_eq!(model.def(TYPE_META).origin(), Origin::Compat);
  assert_eq!(model.def(TAGS).collection(), Some(Collection::Set));

  let photo = model.def(PHOTO);
  assert_eq!(photo.tier(), Tier::Alloc);
  let mut externs: Externs<'_> = photo.externs();
  assert_eq!(externs.len(), 1);
  let item: Extern<'_> = externs.next().expect("`Photo` has an `@extern`");
  assert!(externs.next().is_none());
  assert_eq!(item.lang(), "rust");
  assert_eq!(item.r#type(), "image::Photo");
  assert_eq!(item.tier(), Tier::Std);
  assert_eq!(model.def(POINT).applications(), &[POINT_MISSING]);
}

#[test]
fn a_member_answers_every_accessor() {
  let model = fixture();

  let node: Member<'_> = model.member(QUERY_NODE);
  assert_eq!(node.id(), QUERY_NODE);
  assert_eq!(node.kind(), MemberKind::Field);
  assert_eq!(node.name(), "node");
  assert_eq!(model.name(node.sym()), "node");
  assert_eq!(node.owner(), QUERY);
  assert_eq!(node.parent(), None);
  assert_eq!(node.location(), at(APP_SCHEMA, 250, 254));
  assert_eq!(node.description(), None);
  assert_eq!(node.ty(), Some(NODE_TYPE));
  assert_eq!(node.default(), None);
  assert_eq!(ids(node.arguments()), vec![NODE_ARG_ID]);
  assert!(node.applications().is_empty());
  assert!(!node.has_errors());

  let argument = model.member(NODE_ARG_ID);
  assert_eq!(argument.kind(), MemberKind::Argument);
  assert_eq!(argument.name(), "id");
  assert_eq!(argument.owner(), QUERY);
  assert_eq!(argument.parent(), Some(QUERY_NODE));
  assert_eq!(argument.ty(), Some(NODE_ARG_TYPE));
  assert_eq!(argument.default(), Some(NODE_ARG_DEFAULT));
  assert!(argument.arguments().is_empty());

  let version = model.member(QUERY_VERSION);
  assert_eq!(version.applications(), &[VERSION_DEPRECATED]);
  assert!(version.has_errors());
  assert_eq!(version.location().source(), APP_SCHEMA);

  let reason = model.member(REASON);
  assert_eq!(reason.kind(), MemberKind::Argument);
  assert_eq!(reason.parent(), None);
  assert_eq!(reason.default(), Some(REASON_DEFAULT));
  assert_eq!(model.member(CACHE_TAGS).default(), Some(CACHE_TAGS_DEFAULT));
  assert_eq!(model.member(FILTER_COLOR).default(), Some(COLOR_DEFAULT));
  assert_eq!(model.member(FILTER_TAGS).kind(), MemberKind::InputField);
  assert_eq!(model.member(PAGE_ITEMS).ty(), Some(ITEMS_TYPE));
  assert_eq!(model.member(NODE_ID).ty(), Some(NODE_ID_TYPE));
  assert_eq!(model.member(CACHE_TTL).ty(), Some(TTL_TYPE));
  assert_eq!(model.member(POINT_X).ty(), Some(X_TYPE));
  assert_eq!(model.member(POINT_Y).ty(), Some(Y_TYPE));
  assert_eq!(model.member(REASON).ty(), Some(REASON_TYPE));
  assert_eq!(model.member(QUERY_VERSION).ty(), Some(VERSION_TYPE));
  assert_eq!(model.member(FILTER_TAGS).ty(), Some(TAGS_TYPE));
  assert_eq!(model.member(FILTER_COLOR).ty(), Some(COLOR_TYPE));
  assert_eq!(model.member(CACHE_TAGS).ty(), Some(CACHE_TAGS_TYPE));

  let red = model.member(RED);
  assert_eq!(red.kind(), MemberKind::EnumValue);
  assert_eq!(red.ty(), None);
  assert_eq!(red.default(), None);
  assert_eq!(model.member(GREEN).ty(), None);
}

#[test]
fn a_description_that_is_absent_differs_from_an_empty_one() {
  let model = fixture();
  let empty: Description<'_> = model
    .member(RED)
    .description()
    .expect("`RED` has an empty description");
  assert_eq!(empty.text(), "");
  assert_eq!(empty.location(), at(APP_SCHEMA, 407, 409));
  assert_eq!(model.member(GREEN).description(), None);
  assert_eq!(model.def(COLOR).description(), None);
}

#[test]
fn ranges_and_slices_hold_what_was_appended() {
  let model = fixture();
  let fields = model.def(QUERY).fields();
  assert_eq!(fields.len(), 2);
  assert!(!fields.is_empty());
  assert!(fields.contains(QUERY_NODE));
  assert!(fields.contains(QUERY_VERSION));
  assert!(!fields.contains(NODE_ARG_ID));
  assert!(!fields.contains(NODE_ID));
  assert_eq!(
    fields.into_iter().rev().collect::<Vec<MemberId>>(),
    vec![QUERY_VERSION, QUERY_NODE]
  );
  assert_eq!(fields.into_iter().len(), 2);

  // Every definition's fields, input fields, enum values and arguments are its direct members,
  // which tile the member table with the fields' arguments in id order.
  let mut seen: Vec<MemberId> = Vec::new();
  for id in model.defs() {
    let def = model.def(id);
    for member in def
      .fields()
      .into_iter()
      .chain(def.input_fields())
      .chain(def.enum_values())
      .chain(def.arguments())
    {
      assert_eq!(model.member(member).owner(), id);
      seen.push(member);
    }
    for member in def.fields() {
      seen.extend(model.member(member).arguments());
    }
  }
  seen.sort_unstable();
  assert_eq!(seen, (0..14).map(MemberId::new).collect::<Vec<MemberId>>());

  // Each module's definitions, in module order, are every definition once.
  let all: Vec<DefId> = model
    .modules()
    .into_iter()
    .flat_map(|module: ModuleId| model.module(module).defs())
    .collect::<Vec<DefId>>();
  assert_eq!(all, ids(model.defs()));
}

/// Whether no two of `items` are equal.
fn distinct<T: PartialEq>(items: &[T]) -> bool {
  items
    .iter()
    .enumerate()
    .all(|(index, item): (usize, &T)| !items[index + 1..].contains(item))
}

#[test]
fn every_enum_is_built_as_the_design_says() {
  // `Primitive::Int` cannot be written outside smear, because its fields may grow: it is built
  // through these two, and read with a pattern that ends with `..`. Its documentation carries the
  // `compile_fail` example that a struct expression is refused.
  for (primitive, expected) in [
    (Primitive::signed(32), (true, 32)),
    (Primitive::unsigned(8), (false, 8)),
  ] {
    match primitive {
      Primitive::Int { signed, width, .. } => assert_eq!((signed, width), expected),
      other => panic!("{other:?} is not an integer"),
    }
  }
  assert_ne!(Primitive::signed(8), Primitive::unsigned(8));
  for primitive in [Primitive::Bool, Primitive::F32, Primitive::F64] {
    assert!(!matches!(primitive, Primitive::Int { .. }));
  }

  // Every other variant of every enum is a value a consumer writes as it is.
  assert!(distinct(&[
    PackageKind::BuiltIn,
    PackageKind::Root,
    PackageKind::Dependency
  ]));
  assert!(distinct(&[
    ModuleKind::Root,
    ModuleKind::Directory,
    ModuleKind::Inline
  ]));
  assert!(distinct(&[Origin::User, Origin::BuiltIn, Origin::Compat]));
  assert!(distinct(&[Collection::Set, Collection::Map]));
  assert!(distinct(&[
    MemberKind::Field,
    MemberKind::Argument,
    MemberKind::InputField,
    MemberKind::EnumValue
  ]));
  assert!(distinct(&[
    DefKind::Scalar,
    DefKind::Object,
    DefKind::Interface,
    DefKind::Union,
    DefKind::Enum,
    DefKind::InputObject,
    DefKind::Directive
  ]));
  assert!(distinct(&[
    DirectiveLocation::Query,
    DirectiveLocation::Mutation,
    DirectiveLocation::Subscription,
    DirectiveLocation::Field,
    DirectiveLocation::FragmentDefinition,
    DirectiveLocation::FragmentSpread,
    DirectiveLocation::InlineFragment,
    DirectiveLocation::VariableDefinition,
    DirectiveLocation::Schema,
    DirectiveLocation::Scalar,
    DirectiveLocation::Object,
    DirectiveLocation::FieldDefinition,
    DirectiveLocation::ArgumentDefinition,
    DirectiveLocation::Interface,
    DirectiveLocation::Union,
    DirectiveLocation::Enum,
    DirectiveLocation::EnumValue,
    DirectiveLocation::InputObject,
    DirectiveLocation::InputFieldDefinition,
    DirectiveLocation::DirectiveDefinition
  ]));
}

/// The spelling of each of `path`'s segments.
fn spelled<'m>(model: &'m Model, path: Path<'_>) -> Vec<&'m str> {
  path
    .segments()
    .iter()
    .map(|segment: &Sym| model.name(*segment))
    .collect::<Vec<&'m str>>()
}

#[test]
fn a_type_occurrence_answers_its_kind_path_location_and_type() {
  let model = fixture();

  let implements: TypeExpr<'_> = model.type_expr(QUERY_IMPLEMENTS);
  assert_eq!(implements.id(), QUERY_IMPLEMENTS);
  match implements.kind() {
    TypeExprKind::Named { def, args, .. } => {
      assert_eq!(def, NODE);
      assert!(args.is_empty());
    }
    other => panic!("{other:?} is not a named type"),
  }
  assert!(!implements.non_null());
  assert_eq!(implements.location(), at(APP_SCHEMA, 225, 229));
  assert_eq!(implements.ty(), NODE_TY);
  let path: Path<'_> = implements.path().expect("a named type has a path");
  assert_eq!(path.root(), PathRoot::Relative);
  assert_eq!(spelled(&model, path), vec!["Node"]);
  assert_eq!(implements.path_location(), Some(at(APP_SCHEMA, 225, 229)));

  let id: TypeExpr<'_> = model.type_expr(NODE_ID_TYPE);
  assert!(id.non_null());
  assert_eq!(id.ty(), I32_NON_NULL_TY);

  // `[string]`: a list has an element and no path.
  let tags: TypeExpr<'_> = model.type_expr(TAGS_TYPE);
  match tags.kind() {
    TypeExprKind::List { element, .. } => assert_eq!(element, TAGS_ELEMENT),
    other => panic!("{other:?} is not a list"),
  }
  assert_eq!(tags.path(), None);
  assert_eq!(tags.path_location(), None);
  assert_eq!(tags.ty(), STRINGS_TY);

  // `T!` inside `Page`: a parameter, which belongs to its definition.
  let element: TypeExpr<'_> = model.type_expr(ITEMS_ELEMENT);
  match element.kind() {
    TypeExprKind::Param { def, index, .. } => assert_eq!((def, index), (PAGE, 0)),
    other => panic!("{other:?} is not a type parameter"),
  }
  assert!(element.non_null());
  let path: Path<'_> = element.path().expect("a parameter is written as a path");
  assert_eq!(spelled(&model, path), vec!["T"]);
  assert_eq!(element.ty(), T_TY);

  // `Page<Query>`: the written arguments, and the complete instantiation as its type.
  match model.type_expr(SEARCH_PAGE).kind() {
    TypeExprKind::Named { def, args, .. } => {
      assert_eq!(def, PAGE);
      assert_eq!(args, &[SEARCH_PAGE_ARG]);
    }
    other => panic!("{other:?} is not a named type"),
  }
  assert_eq!(model.type_expr(SEARCH_PAGE).ty(), PAGE_QUERY_TY);

  // Every occurrence of one type shares its id.
  for occurrence in [QUERY_IMPLEMENTS, NODE_TYPE, T_BOUND] {
    assert_eq!(model.type_expr(occurrence).ty(), NODE_TY);
  }
  for occurrence in [
    REASON_TYPE,
    VERSION_TYPE,
    TAGS_ELEMENT,
    T_DEFAULT,
    CACHE_TAGS_ELEMENT,
  ] {
    assert_eq!(model.type_expr(occurrence).ty(), STRING_TY);
  }
  for occurrence in [NODE_ARG_TYPE, TTL_TYPE, CACHE_TYPE_ARG] {
    assert_eq!(model.type_expr(occurrence).ty(), I32_TY);
  }
  for occurrence in [SEARCH_QUERY, SEARCH_PAGE_ARG, QUERY_ROOT] {
    assert_eq!(model.type_expr(occurrence).ty(), QUERY_TY);
  }
  assert_eq!(model.type_expr(CACHE_TAGS_TYPE).ty(), STRINGS_TY);
  assert_eq!(model.type_expr(COLOR_TYPE).ty(), COLOR_TY);
  assert_eq!(model.type_expr(ITEMS_TYPE).ty(), TS_TY);
}

#[test]
fn a_type_answers_its_kind_nullness_tier_and_closedness() {
  let model = fixture();

  let string: Ty<'_> = model.ty(STRING_TY);
  assert_eq!(string.id(), STRING_TY);
  match string.kind() {
    TyKind::Named { def, args, .. } => {
      assert_eq!(def, STRING);
      assert!(args.is_empty());
    }
    other => panic!("{other:?} is not a named type"),
  }
  assert!(!string.non_null());
  assert_eq!(string.tier(), Tier::Core);
  assert!(string.is_closed());
  assert!(model.ty(I32_NON_NULL_TY).non_null());

  let param: Ty<'_> = model.ty(T_TY);
  match param.kind() {
    TyKind::Param { def, index, .. } => assert_eq!((def, index), (PAGE, 0)),
    other => panic!("{other:?} is not a type parameter"),
  }
  assert!(param.non_null());
  assert!(!param.is_closed());

  match model.ty(TS_TY).kind() {
    TyKind::List { element, .. } => assert_eq!(element, T_TY),
    other => panic!("{other:?} is not a list"),
  }
  assert!(
    !model.ty(TS_TY).is_closed(),
    "a list of a parameter names it"
  );
  match model.ty(STRINGS_TY).kind() {
    TyKind::List { element, .. } => assert_eq!(element, STRING_TY),
    other => panic!("{other:?} is not a list"),
  }
  assert!(model.ty(STRINGS_TY).is_closed());

  match model.ty(PAGE_QUERY_TY).kind() {
    TyKind::Named { def, args, .. } => {
      assert_eq!(def, PAGE);
      assert_eq!(args, &[QUERY_TY]);
    }
    other => panic!("{other:?} is not a named type"),
  }
  assert!(model.ty(PAGE_QUERY_TY).is_closed());

  // Each closed instantiation once; none for a definition the program never instantiates.
  assert_eq!(model.instantiations(PAGE), &[PAGE_QUERY_TY]);
  assert!(model.instantiations(QUERY).is_empty());

  // Each satisfaction recorded, whether added once or twice, and nothing else.
  assert!(model.satisfies(QUERY_TY, NODE));
  assert!(model.satisfies(I32_TY, MAP_KEY));
  assert!(!model.satisfies(STRING_TY, MAP_KEY));
  assert!(!model.satisfies(QUERY_TY, MAP_KEY));
}

#[test]
fn a_value_answers_every_kind_its_location_type_writtenness_and_path() {
  let model = fixture();

  let everything: Value<'_> = model.value(EVERYTHING);
  assert_eq!(everything.id(), EVERYTHING);
  assert_eq!(
    everything.kind(),
    ValueKind::List(&[
      V_NULL,
      V_BOOL,
      V_NEGATIVE,
      V_FLOAT,
      V_STRING,
      V_ENUM,
      V_TYPE,
      V_LIST,
      V_SET,
      V_MAP,
      V_OBJECT,
      V_LITERAL,
      V_UNRESOLVED,
      V_WIDE,
      V_MIN
    ])
  );
  assert_eq!(everything.location(), at(APP_SCHEMA, 1047, 1094));
  assert!(everything.written());
  assert_eq!(everything.path(), None);

  assert_eq!(model.value(V_NULL).kind(), ValueKind::Null);
  assert_eq!(model.value(V_BOOL).kind(), ValueKind::Bool(true));
  assert_eq!(model.value(V_SET_ITEM).kind(), ValueKind::Bool(false));
  assert_eq!(model.value(V_STRING).kind(), ValueKind::String("hi"));
  assert_eq!(model.value(V_LIST).kind(), ValueKind::List(&[V_LIST_ITEM]));
  assert_eq!(model.value(V_SET).kind(), ValueKind::Set(&[V_SET_ITEM]));
  assert_eq!(
    model.value(V_MAP).kind(),
    ValueKind::Map(&[(V_MAP_KEY, V_MAP_VALUE)])
  );
  assert_eq!(model.value(V_OBJECT_TAGS).kind(), ValueKind::List(&[]));
  assert_eq!(model.value(V_UNRESOLVED).kind(), ValueKind::Unresolved);

  let ValueKind::Float(float) = model.value(V_FLOAT).kind() else {
    panic!("not a float");
  };
  assert_eq!(float.to_f64().to_bits(), 1.5f64.to_bits());
  assert_eq!(float.text(), "1.5");

  let ValueKind::Enum { def, value, .. } = model.value(V_ENUM).kind() else {
    panic!("not an enum value");
  };
  assert_eq!((def, value), (COLOR, GREEN));
  let path: Path<'_> = model
    .value(V_ENUM)
    .path()
    .expect("an enum value writes a path");
  assert_eq!(path.root(), PathRoot::Relative);
  assert_eq!(spelled(&model, path), vec!["GREEN"]);

  let ValueKind::Type { def, .. } = model.value(V_TYPE).kind() else {
    panic!("not a type value");
  };
  assert_eq!(def, POINT);
  let path: Path<'_> = model
    .value(V_TYPE)
    .path()
    .expect("a type value writes a path");
  assert_eq!(path.root(), PathRoot::Dependency);
  assert_eq!(spelled(&model, path), vec!["geo", "Point"]);

  // An object holds what was written and what a default filled, and the filled one has no name.
  let ValueKind::Object(entries) = model.value(V_OBJECT).kind() else {
    panic!("not an object");
  };
  assert_eq!(entries.len(), 2);
  assert_eq!(entries[0].field(), FILTER_TAGS);
  assert_eq!(entries[0].name(), Some(at(APP_SCHEMA, 1086, 1087)));
  assert_eq!(entries[0].value(), V_OBJECT_TAGS);
  assert_eq!(entries[1].field(), FILTER_COLOR);
  assert_eq!(entries[1].name(), None);
  assert_eq!(entries[1].value(), V_OBJECT_COLOR);
  assert!(!model.value(V_OBJECT_COLOR).written());
  assert_eq!(
    model.value(V_OBJECT_COLOR).location(),
    at(APP_SCHEMA, 545, 548)
  );

  let ValueKind::Literal(literal) = model.value(V_LITERAL).kind() else {
    panic!("not a literal");
  };
  assert_eq!(literal.kind(), LiteralKind::Int);
  assert_eq!(literal.text(), "0xFF");
  assert_eq!(literal.location(), at(APP_SCHEMA, 1088, 1092));

  // The type a value was coerced against, and the one unresolved type where nothing coerced it.
  assert_eq!(model.value(TTL_VALUE).ty(), I32_TY);
  assert_eq!(model.value(V_NULL).ty(), UNRESOLVED_TY);

  // A default's copy, and everything inside it, is not written.
  assert!(!model.value(TAGS_COPY).written());
  assert!(!model.value(TAGS_COPY_ITEM).written());
  assert_eq!(model.value(TAGS_COPY).location(), at(APP_SCHEMA, 833, 838));
  assert!(model.value(CACHE_TAGS_DEFAULT).written());
  assert_eq!(
    model.value(CACHE_TAGS_DEFAULT).kind(),
    ValueKind::List(&[CACHE_TAGS_DEFAULT_ITEM])
  );
  assert_eq!(
    model.value(CACHE_TAGS_DEFAULT_ITEM).kind(),
    ValueKind::String("a")
  );
  assert_eq!(
    model.value(REASON_DEFAULT).kind(),
    ValueKind::String("No longer supported")
  );
  let ValueKind::Enum { def, value, .. } = model.value(COLOR_DEFAULT).kind() else {
    panic!("not an enum value");
  };
  assert_eq!((def, value), (COLOR, RED));
}

/// The integer value `id` holds.
fn int_value(model: &Model, id: ValueId) -> IntValue<'_> {
  match model.value(id).kind() {
    ValueKind::Int(int) => int,
    other => panic!("{other:?} is not an integer"),
  }
}

#[test]
fn an_integer_answers_its_sign_magnitude_text_and_value_in_any_width() {
  let model = fixture();

  let negative = int_value(&model, V_NEGATIVE);
  assert!(negative.is_negative());
  assert_eq!(negative.magnitude(), &[42]);
  assert_eq!(negative.text(), "-42");
  assert_eq!(negative.to_i128(), Some(-42));
  assert_eq!(negative.to_u128(), None);

  let one = int_value(&model, V_LIST_ITEM);
  assert!(!one.is_negative());
  assert_eq!(one.to_i128(), Some(1));
  assert_eq!(one.to_u128(), Some(1));

  // Trailing zero bytes are not kept: `2` is one byte however it was given.
  let two = int_value(&model, V_MAP_VALUE);
  assert_eq!(two.magnitude(), &[2]);
  assert_eq!(two.to_u128(), Some(2));

  // `2^128`: wider than both, and read through its magnitude.
  let wide = int_value(&model, V_WIDE);
  assert_eq!(wide.magnitude().len(), 17);
  assert_eq!(wide.magnitude()[16], 1);
  assert_eq!(wide.to_u128(), None);
  assert_eq!(wide.to_i128(), None);
  assert_eq!(wide.text(), "340282366920938463463374607431768211456");

  // `-2^127` is `i128::MIN`, which only the negative side reaches.
  let min = int_value(&model, V_MIN);
  assert!(min.is_negative());
  assert_eq!(min.to_i128(), Some(i128::MIN));
  assert_eq!(min.to_u128(), None);

  assert_eq!(int_value(&model, TTL_VALUE).to_i128(), Some(60));
  assert_eq!(int_value(&model, NODE_ARG_DEFAULT).to_u128(), Some(1));
}

#[test]
fn an_application_answers_its_directive_type_arguments_locations_path_and_arguments() {
  let model = fixture();

  let cache: Application<'_> = model.application(QUERY_CACHE);
  assert_eq!(cache.id(), QUERY_CACHE);
  assert_eq!(cache.directive(), Resolution::Resolved(CACHE));
  assert!(cache.type_args().is_empty());
  assert_eq!(cache.location(), at(APP_SCHEMA, 230, 245));
  assert_eq!(cache.path_location(), at(APP_SCHEMA, 231, 236));
  assert_eq!(cache.path().root(), PathRoot::Relative);
  assert_eq!(spelled(&model, cache.path()), vec!["cache"]);
  // Every argument written or defaulted, in the definition's order.
  let arguments: &[ApplicationArgument] = cache.arguments();
  assert_eq!(arguments.len(), 2);
  assert_eq!(arguments[0].argument(), CACHE_TTL);
  assert_eq!(arguments[0].name_location(), Some(at(APP_SCHEMA, 237, 240)));
  assert_eq!(arguments[0].value(), TTL_VALUE);
  assert!(arguments[0].written());
  assert_eq!(arguments[1].argument(), CACHE_TAGS);
  assert_eq!(arguments[1].name_location(), None);
  assert_eq!(arguments[1].value(), TAGS_COPY);
  assert!(!arguments[1].written());

  let deprecated: Application<'_> = model.application(VERSION_DEPRECATED);
  assert_eq!(deprecated.directive(), Resolution::Resolved(DEPRECATED));
  assert_eq!(deprecated.arguments().len(), 1);
  assert_eq!(deprecated.arguments()[0].argument(), REASON);
  assert_eq!(deprecated.arguments()[0].value(), REASON_COPY);
  assert!(!deprecated.arguments()[0].written());

  let schema_cache: Application<'_> = model.application(SCHEMA_CACHE);
  assert_eq!(schema_cache.type_args(), &[CACHE_TYPE_ARG]);
  assert_eq!(schema_cache.arguments().len(), 1);
  assert_eq!(schema_cache.arguments()[0].value(), EVERYTHING);
}

#[test]
fn the_schema_answers_its_roots_applications_description_and_contributions() {
  let model = fixture();
  let schema: Schema<'_> = model.schema();
  assert_eq!(
    schema.root(RootOperation::Query),
    Some(Root::Written(QUERY_ROOT))
  );
  assert_eq!(
    schema.root(RootOperation::Mutation),
    Some(Root::Presumed(POINT))
  );
  assert_eq!(schema.root(RootOperation::Subscription), None);
  assert_eq!(schema.applications(), &[SCHEMA_CACHE]);
  let description = schema.description().expect("the schema is described");
  assert_eq!(description.text(), "The schema.");
  assert_eq!(description.location(), at(APP_SCHEMA, 990, 999));
  let mut parts: Contributions<'_> = schema.contributions();
  assert_eq!(parts.len(), 1);
  let part: Contribution<'_> = parts.next().expect("the schema has its definition");
  assert_eq!(part.location(), at(APP_SCHEMA, 1000, 1006));
  assert!(part.members().is_empty());
  assert!(part.implements().is_empty());
  assert_eq!(part.applications(), &[SCHEMA_CACHE]);
  assert!(!part.has_errors());
}

#[test]
fn the_builtins_are_found_by_name_in_the_built_in_packages() {
  let model = fixture();
  let builtins: Builtins<'_> = model.builtins();
  assert_eq!(builtins.deprecated(), Some(DEPRECATED));
  assert_eq!(builtins.string(), Some(STRING));
  assert_eq!(builtins.map_key(), Some(MAP_KEY));
  // `core` here defines none of the others.
  for missing in [
    builtins.marker(),
    builtins.set(),
    builtins.map(),
    builtins.r#extern(),
    builtins.repr(),
    builtins.heap(),
    builtins.specified_by(),
    builtins.one_of(),
    builtins.skip(),
    builtins.include(),
    builtins.type_ref(),
    builtins.non_zero(),
    builtins.bytes(),
    builtins.id(),
  ] {
    assert_eq!(missing, None);
  }
  assert_eq!(builtins.primitive(Primitive::signed(32)), Some(I32));
  assert_eq!(builtins.primitive(Primitive::unsigned(32)), None);
  assert_eq!(builtins.primitive(Primitive::Bool), None);

  // A model whose built-in packages define nothing has no built-in at all.
  let analysis = analyze(
    &Package::<&str>::new("app", Layout::Flat, Tier::Std),
    &[],
    &Limits::new(),
  );
  let empty: Builtins<'_> = analysis.model().builtins();
  assert_eq!(empty.deprecated(), None);
  assert_eq!(empty.primitive(Primitive::signed(32)), None);
}

#[test]
fn every_unresolved_shape_and_the_one_type_unresolved_occurrences_share() {
  let model = fixture();

  // Two misspelt types: each occurrence stays, unresolved, with its written path.
  for (occurrence, spelling) in [(X_TYPE, "Adress"), (Y_TYPE, "Strng")] {
    let ty: TypeExpr<'_> = model.type_expr(occurrence);
    assert_eq!(ty.kind(), TypeExprKind::Unresolved);
    let path: Path<'_> = ty.path().expect("an unresolved type keeps its path");
    assert_eq!(spelled(&model, path), vec![spelling]);
    // One was given the unresolved type and one was given none: they share one type.
    assert_eq!(ty.ty(), UNRESOLVED_TY);
  }
  assert_eq!(model.ty(UNRESOLVED_TY).kind(), TyKind::Unresolved);
  assert!(!model.ty(UNRESOLVED_TY).non_null());
  assert!(model.ty(UNRESOLVED_TY).is_closed());

  // A value that did not coerce, and an application whose directive did not resolve: it stays,
  // with its locations and its path, and with no type argument and no argument.
  assert_eq!(model.value(V_UNRESOLVED).kind(), ValueKind::Unresolved);
  assert_eq!(model.value(V_UNRESOLVED).ty(), UNRESOLVED_TY);
  let missing: Application<'_> = model.application(POINT_MISSING);
  assert_eq!(missing.directive(), Resolution::Unresolved);
  assert!(missing.type_args().is_empty());
  assert!(missing.arguments().is_empty());
  assert_eq!(missing.location(), at(GEO_POINT, 16, 24));
  assert_eq!(spelled(&model, missing.path()), vec!["missing"]);
}

/// The namespace `def` is named in.
fn namespace(model: &Model, def: DefId) -> Namespace {
  if model.def(def).kind() == DefKind::Directive {
    Namespace::Directive
  } else {
    Namespace::Type
  }
}

#[test]
fn every_definition_has_a_canonical_name_that_lookup_reads_back() {
  let model = fixture();
  let names = [
    (I32, "^::core::i32"),
    (STRING, "^::core::string"),
    (DEPRECATED, "@^::core::deprecated"),
    (MAP_KEY, "^::core::MapKey"),
    (TYPE_META, "^::core::__Type"),
    (NODE, "^::app::Node"),
    (QUERY, "^::app::Query"),
    (COLOR, "^::app::Color"),
    (FILTER, "^::app::Filter"),
    (PAGE, "^::app::Page"),
    (SEARCH, "^::app::Search"),
    (CACHE, "@^::app::cache"),
    (PHOTO, "^::app::media::Photo"),
    (TAGS, "^::app::media::Tags"),
    (POINT, "^::geo::Point"),
  ];
  assert_eq!(names.len(), model.defs().len() as usize);
  for (def, name) in names {
    let canonical: CanonicalName<'_> = model.canonical_name(def);
    assert_eq!(canonical.to_string(), name);
    let namespace = namespace(&model, def);
    assert_eq!(model.lookup(namespace, name), Some(def), "{name}");
    assert_eq!(
      model.lookup(namespace, name.as_bytes()),
      Some(def),
      "{name}"
    );
    assert_eq!(
      model.lookup(namespace, canonical.to_string()),
      Some(def),
      "{name}"
    );
  }
  assert_eq!(
    format!("{:?}", model.canonical_name(QUERY)),
    "CanonicalName(^::app::Query)"
  );
}

#[test]
fn lookup_reads_a_second_name_and_finds_nothing_for_what_names_nothing() {
  let model = fixture();
  // `Int` is a second name `core` gives `i32`.
  assert_eq!(model.lookup(Namespace::Type, "^::core::Int"), Some(I32));
  assert_eq!(model.lookup(Namespace::Type, "^::app::Int"), None);
  assert_eq!(model.lookup(Namespace::Directive, "@^::core::Int"), None);

  // The namespaces are apart: a directive is no type, and a type no directive.
  assert_eq!(model.lookup(Namespace::Type, "^::app::cache"), None);
  assert_eq!(model.lookup(Namespace::Directive, "@^::app::Query"), None);
  // A directive's name is spelled with its `@`, and only a directive's.
  assert_eq!(model.lookup(Namespace::Directive, "^::app::cache"), None);
  assert_eq!(model.lookup(Namespace::Type, "@^::app::Query"), None);

  // Only a dependency path to a definition is a canonical name.
  for name in [
    "",
    "Query",
    "app::Query",
    "~::Query",
    "::app::Query",
    "^::",
    "^::app",
    "^::app::",
    "^::app::::Query",
    "^::app::media",
    "^::app::Missing",
    "^::nope::Query",
    "^::app::media::Query",
    "^::app::vision::Photo",
  ] {
    assert_eq!(model.lookup(Namespace::Type, name), None, "{name}");
  }
  assert_eq!(model.lookup(Namespace::Type, b"^::app::\xffQuery"), None);
}

#[test]
fn module_by_path_finds_every_module_by_its_dependency_path() {
  let model = fixture();
  for (module, path) in [
    (CORE_ROOT, "^::core"),
    (ALLOC_ROOT, "^::alloc"),
    (STD_ROOT, "^::std"),
    (APP_ROOT, "^::app"),
    (MEDIA, "^::app::media"),
    (VISION, "^::app::media::vision"),
    (GEO_ROOT, "^::geo"),
  ] {
    assert_eq!(model.module_by_path(path), Some(module), "{path}");
    assert_eq!(
      model.module_by_path(path.as_bytes()),
      Some(module),
      "{path}"
    );
  }
  for path in [
    "",
    "app",
    "~::media",
    "^::",
    "^::nope",
    "^::app::",
    "^::app::vision",
    "^::app::media::vision::deeper",
    "^::app::Query",
  ] {
    assert_eq!(model.module_by_path(path), None, "{path}");
  }
  assert_eq!(model.module_by_path(b"^::app::\xffmedia"), None);
}

// Every result type and every view may cross threads.
const _: () = {
  const fn assert_send_sync<T: Send + Sync>() {}
  let _ = assert_send_sync::<Model>;
  let _ = assert_send_sync::<PackageInfo<'static>>;
  let _ = assert_send_sync::<Module<'static>>;
  let _ = assert_send_sync::<Def<'static>>;
  let _ = assert_send_sync::<Contribution<'static>>;
  let _ = assert_send_sync::<Contributions<'static>>;
  let _ = assert_send_sync::<TypeParam<'static>>;
  let _ = assert_send_sync::<TypeParams<'static>>;
  let _ = assert_send_sync::<TypeBound>;
  let _ = assert_send_sync::<Extern<'static>>;
  let _ = assert_send_sync::<Externs<'static>>;
  let _ = assert_send_sync::<Member<'static>>;
  let _ = assert_send_sync::<Description<'static>>;
  let _ = assert_send_sync::<TypeExpr<'static>>;
  let _ = assert_send_sync::<TypeExprKind<'static>>;
  let _ = assert_send_sync::<Ty<'static>>;
  let _ = assert_send_sync::<TyKind<'static>>;
  let _ = assert_send_sync::<Path<'static>>;
  let _ = assert_send_sync::<Value<'static>>;
  let _ = assert_send_sync::<ValueKind<'static>>;
  let _ = assert_send_sync::<IntValue<'static>>;
  let _ = assert_send_sync::<FloatValue<'static>>;
  let _ = assert_send_sync::<Literal<'static>>;
  let _ = assert_send_sync::<ObjectEntry>;
  let _ = assert_send_sync::<Application<'static>>;
  let _ = assert_send_sync::<ApplicationArgument>;
  let _ = assert_send_sync::<Resolution<DefId>>;
  let _ = assert_send_sync::<Schema<'static>>;
  let _ = assert_send_sync::<Builtins<'static>>;
  let _ = assert_send_sync::<CanonicalName<'static>>;
  let _ = assert_send_sync::<Namespace>;
};
