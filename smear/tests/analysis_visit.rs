//! The GraphQLx analyzer's walk: the `Visitor` trait, the context `Cx` and `visit`, over a model
//! the test-support door builds with every kind of entity, extensions one package writes for
//! another's definitions, nested type occurrences and values, and each unresolved shape.
//!
//! The fixture asserts every id the builder hands back, so the walks written out below are in the
//! model's numbering: a change to it fails in the fixture first.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationId, Contribution, Cx, DefId, DefKind, Diagnostic, Diagnostics, Layout,
    Limits, MemberId, MemberKind, Model, ModuleId, ModuleKind, Origin, PackageId, PackageKind,
    Packages, PathRoot, Resolution, Root, RootOperation, Site, SourceId, Sym, Tier, TypeExprId,
    TypeExprKind, ValueId, ValueKind, Visitor,
    test_support::{ModelBuilder, analysis_of, conclude},
    visit,
  },
  diagnostic::{Code, Diagnose, Location, Severity},
  lexer::tokora::SimpleSpan,
};

// Sources: each package's package source, then its files.
const CORE_LIB: u32 = 1;
const APP_SOURCE: u32 = 4;
const APP_SCHEMA: u32 = 5;
const APP_INNER: u32 = 6;
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
const INNER: ModuleId = ModuleId::new(4);
const GEO_ROOT: ModuleId = ModuleId::new(5);

// Definitions.
const STRING: DefId = DefId::new(0);
const DEPRECATED: DefId = DefId::new(1);
const NODE: DefId = DefId::new(2);
const QUERY: DefId = DefId::new(3);
const PAGE: DefId = DefId::new(4);
const SEARCH: DefId = DefId::new(5);
const CACHE: DefId = DefId::new(6);
const COLOR: DefId = DefId::new(7);
const FILTER: DefId = DefId::new(8);
const POINT: DefId = DefId::new(9);

// Members.
const REASON: MemberId = MemberId::new(0);
const NODE_ID: MemberId = MemberId::new(1);
const QUERY_NODE: MemberId = MemberId::new(2);
const QUERY_NODES: MemberId = MemberId::new(3);
const NODE_ARG: MemberId = MemberId::new(4);
const ITEMS: MemberId = MemberId::new(5);
const TTL: MemberId = MemberId::new(6);
const RED: MemberId = MemberId::new(7);
const GREEN: MemberId = MemberId::new(8);
const FILTER_COLOR: MemberId = MemberId::new(9);
const FILTER_TAGS: MemberId = MemberId::new(10);
const FILTER_BAD: MemberId = MemberId::new(11);
const POINT_X: MemberId = MemberId::new(12);
const POINT_LABEL: MemberId = MemberId::new(13);

// The unresolved shapes: `bad: Missing<string>` and its written argument, the root
// `mutation: Missing`, the default `BLUE`, and the application `@missing`.
const MISSING_TYPE: TypeExprId = TypeExprId::new(19);
const MISSING_ARG: TypeExprId = TypeExprId::new(20);
const MISSING_ROOT: TypeExprId = TypeExprId::new(24);
const BLUE: ValueId = ValueId::new(8);
const MISSING: ApplicationId = ApplicationId::new(2);

/// A code of this file's own, as a consumer's rule has.
const EVERY_DEF: Code = Code::new("test::visit::every-def");
/// A second code of this file's own.
const EVERY_MEMBER: Code = Code::new("test::visit::every-member");
/// A third code of this file's own: a rule that refuses where the marks tests need it to.
const REFUSED: Code = Code::new("test::visit::refused");

/// The bytes `start..end` of source `source`.
fn at(source: u32, start: usize, end: usize) -> Location {
  Location::new(source, SimpleSpan::new(start, end))
}

/// Appends a definition named `name`, and its own contribution, named at `location` and spanning
/// `extent`.
fn def(
  b: &mut ModelBuilder,
  module: ModuleId,
  kind: DefKind,
  name: &str,
  location: Location,
  extent: Location,
  origin: Origin,
) -> DefId {
  let name: Sym = b.intern(name);
  let id = b.push_def(module, kind, name, location, origin);
  assert_eq!(b.push_contribution(id, location, extent), 0);
  id
}

/// Appends a type occurrence written at `location` that resolved to `def`.
fn named(b: &mut ModelBuilder, location: Location, non_null: bool, def: DefId) -> TypeExprId {
  let id = b.push_type_expr(location, non_null);
  b.resolve_named(id, def);
  id
}

/// Appends a string value.
fn string(b: &mut ModelBuilder, location: Location, written: bool, text: &str) -> ValueId {
  let id = b.push_value(location, written);
  b.set_string(id, text);
  id
}

/// Appends a written integer value.
fn int(b: &mut ModelBuilder, location: Location, magnitude: u8, text: &str) -> ValueId {
  let id = b.push_value(location, true);
  b.set_int(id, false, &[magnitude], text);
  id
}

/// Appends an application of the directive written as the relative path `name`.
fn application(
  b: &mut ModelBuilder,
  location: Location,
  name: &str,
  path: Location,
) -> ApplicationId {
  let segment: Sym = b.intern(name);
  b.push_application(location, PathRoot::Relative, &[segment], path)
}

/// A model with every kind of entity, appended in the order the builder requires.
///
/// The packages are `core`, `alloc`, `std`, the root `app` and its dependency `geo`. `core` holds
/// `string` and `@deprecated(reason: string = "gone")`. `app` holds, in its root module, the
/// interface `Node`, the object `Query implements Node @cache<string>(ttl: [1, {"k": 2}])` with
/// `node(id: string = "x"): Node` and `nodes: [Node!]!`, the generic `Page<T = string> where T:
/// Node`, the union `Search = Query | Page<Query>` and the directive `@cache<T>(ttl: T)`; and in
/// `app::inner`, the enum `Color` and the input object `Filter`, whose `color: Color = BLUE` names a
/// value that does not exist and whose `bad: Missing<string>` a type that does not. `geo` holds
/// `Point @missing { x: string }`, where `@missing` does not resolve. `app` writes `extend scalar
/// string @deprecated(reason: "old")` and `extend type Point { label: string @deprecated }`, and its
/// schema `@cache<string>(ttl: "5") { query: Query mutation: Missing }`. One error is marked, at
/// `Missing` in `bad`.
fn fixture() -> Model {
  let mut b = ModelBuilder::default();

  // Packages, each numbering its package source and then its files: `core` 0 and 1, `alloc` 2,
  // `std` 3, `app` 4 to 6, `geo` 7 and 8.
  let packages = [
    ("core", PackageKind::BuiltIn, Tier::Core, 2, CORE),
    ("alloc", PackageKind::BuiltIn, Tier::Alloc, 1, ALLOC),
    ("std", PackageKind::BuiltIn, Tier::Std, 1, STD),
    ("app", PackageKind::Root, Tier::Std, 3, APP),
    ("geo", PackageKind::Dependency, Tier::Core, 2, GEO),
  ];
  for (name, kind, tier, sources, expected) in packages {
    assert_eq!(
      b.push_package(name, kind, tier, Layout::Directory, sources),
      expected
    );
  }
  b.push_dependency(APP, GEO);

  // Modules: every package's root module, and `app::inner`.
  assert_eq!(b.push_module(CORE, None, None, ModuleKind::Root), CORE_ROOT);
  assert_eq!(
    b.push_module(ALLOC, None, None, ModuleKind::Root),
    ALLOC_ROOT
  );
  assert_eq!(b.push_module(STD, None, None, ModuleKind::Root), STD_ROOT);
  assert_eq!(b.push_module(APP, None, None, ModuleKind::Root), APP_ROOT);
  assert_eq!(
    b.push_module(APP, Some(APP_ROOT), Some("inner"), ModuleKind::Directory),
    INNER
  );
  assert_eq!(b.push_module(GEO, None, None, ModuleKind::Root), GEO_ROOT);

  // Definitions, by module, then source, then position, each with its own contribution and then
  // the extensions `app` writes for it.
  assert_eq!(
    def(
      &mut b,
      CORE_ROOT,
      DefKind::Scalar,
      "string",
      at(CORE_LIB, 7, 13),
      at(CORE_LIB, 0, 13),
      Origin::BuiltIn,
    ),
    STRING
  );
  assert_eq!(
    b.push_extension(STRING, at(APP_SCHEMA, 314, 320), at(APP_SCHEMA, 300, 346)),
    1
  );
  let defs = [
    (
      CORE_ROOT,
      DefKind::Directive,
      "deprecated",
      at(CORE_LIB, 25, 35),
      at(CORE_LIB, 14, 90),
      Origin::BuiltIn,
      DEPRECATED,
    ),
    (
      APP_ROOT,
      DefKind::Interface,
      "Node",
      at(APP_SCHEMA, 10, 14),
      at(APP_SCHEMA, 0, 60),
      Origin::User,
      NODE,
    ),
    (
      APP_ROOT,
      DefKind::Object,
      "Query",
      at(APP_SCHEMA, 105, 110),
      at(APP_SCHEMA, 100, 260),
      Origin::User,
      QUERY,
    ),
    (
      APP_ROOT,
      DefKind::Object,
      "Page",
      at(APP_SCHEMA, 405, 409),
      at(APP_SCHEMA, 400, 470),
      Origin::User,
      PAGE,
    ),
    (
      APP_ROOT,
      DefKind::Union,
      "Search",
      at(APP_SCHEMA, 506, 512),
      at(APP_SCHEMA, 500, 540),
      Origin::User,
      SEARCH,
    ),
    (
      APP_ROOT,
      DefKind::Directive,
      "cache",
      at(APP_SCHEMA, 611, 616),
      at(APP_SCHEMA, 600, 660),
      Origin::User,
      CACHE,
    ),
    (
      INNER,
      DefKind::Enum,
      "Color",
      at(APP_INNER, 5, 10),
      at(APP_INNER, 0, 40),
      Origin::User,
      COLOR,
    ),
    (
      INNER,
      DefKind::InputObject,
      "Filter",
      at(APP_INNER, 56, 62),
      at(APP_INNER, 50, 130),
      Origin::User,
      FILTER,
    ),
    (
      GEO_ROOT,
      DefKind::Object,
      "Point",
      at(GEO_POINT, 5, 10),
      at(GEO_POINT, 0, 60),
      Origin::User,
      POINT,
    ),
  ];
  for (module, kind, name, location, extent, origin, expected) in defs {
    assert_eq!(
      def(&mut b, module, kind, name, location, extent, origin),
      expected
    );
  }
  assert_eq!(
    b.push_extension(POINT, at(APP_SCHEMA, 712, 717), at(APP_SCHEMA, 700, 760)),
    1
  );
  assert_eq!(
    b.push_schema_contribution(at(APP_SCHEMA, 800, 806), at(APP_SCHEMA, 800, 900)),
    0
  );

  // Type parameters: `Page`'s `T` and `@cache`'s `T`.
  let t: Sym = b.intern("T");
  assert_eq!(b.push_type_param(PAGE, t, at(APP_SCHEMA, 410, 411)), 0);
  assert_eq!(b.push_type_param(CACHE, t, at(APP_SCHEMA, 617, 618)), 0);

  // Members, owner by owner: the direct members by contribution, then each field's arguments.
  let members = [
    (
      DEPRECATED,
      0,
      MemberKind::Argument,
      "reason",
      at(CORE_LIB, 36, 42),
      at(CORE_LIB, 36, 67),
      REASON,
    ),
    (
      NODE,
      0,
      MemberKind::Field,
      "id",
      at(APP_SCHEMA, 17, 19),
      at(APP_SCHEMA, 17, 27),
      NODE_ID,
    ),
    (
      QUERY,
      0,
      MemberKind::Field,
      "node",
      at(APP_SCHEMA, 165, 169),
      at(APP_SCHEMA, 165, 200),
      QUERY_NODE,
    ),
    (
      QUERY,
      0,
      MemberKind::Field,
      "nodes",
      at(APP_SCHEMA, 205, 210),
      at(APP_SCHEMA, 205, 225),
      QUERY_NODES,
    ),
  ];
  for (owner, contribution, kind, name, location, extent, expected) in members {
    let name: Sym = b.intern(name);
    assert_eq!(
      b.push_member(owner, contribution, kind, name, location, extent),
      expected
    );
  }
  let id: Sym = b.intern("id");
  assert_eq!(
    b.push_argument(
      QUERY_NODE,
      id,
      at(APP_SCHEMA, 170, 172),
      at(APP_SCHEMA, 170, 187)
    ),
    NODE_ARG
  );
  let members = [
    (
      PAGE,
      0,
      MemberKind::Field,
      "items",
      at(APP_SCHEMA, 438, 443),
      at(APP_SCHEMA, 438, 450),
      ITEMS,
    ),
    (
      CACHE,
      0,
      MemberKind::Argument,
      "ttl",
      at(APP_SCHEMA, 620, 623),
      at(APP_SCHEMA, 620, 626),
      TTL,
    ),
    (
      COLOR,
      0,
      MemberKind::EnumValue,
      "RED",
      at(APP_INNER, 13, 16),
      at(APP_INNER, 13, 16),
      RED,
    ),
    (
      COLOR,
      0,
      MemberKind::EnumValue,
      "GREEN",
      at(APP_INNER, 17, 22),
      at(APP_INNER, 17, 22),
      GREEN,
    ),
    (
      FILTER,
      0,
      MemberKind::InputField,
      "color",
      at(APP_INNER, 65, 70),
      at(APP_INNER, 65, 84),
      FILTER_COLOR,
    ),
    (
      FILTER,
      0,
      MemberKind::InputField,
      "tags",
      at(APP_INNER, 85, 89),
      at(APP_INNER, 85, 106),
      FILTER_TAGS,
    ),
    (
      FILTER,
      0,
      MemberKind::InputField,
      "bad",
      at(APP_INNER, 108, 111),
      at(APP_INNER, 108, 128),
      FILTER_BAD,
    ),
    (
      POINT,
      0,
      MemberKind::Field,
      "x",
      at(GEO_POINT, 22, 23),
      at(GEO_POINT, 22, 31),
      POINT_X,
    ),
    (
      POINT,
      1,
      MemberKind::Field,
      "label",
      at(APP_SCHEMA, 720, 725),
      at(APP_SCHEMA, 720, 758),
      POINT_LABEL,
    ),
  ];
  for (owner, contribution, kind, name, location, extent, expected) in members {
    let name: Sym = b.intern(name);
    assert_eq!(
      b.push_member(owner, contribution, kind, name, location, extent),
      expected
    );
  }

  // The walk, definition by definition. `string`: the extension's `@deprecated(reason: "old")`.
  assert_eq!(
    application(
      &mut b,
      at(APP_SCHEMA, 321, 346),
      "deprecated",
      at(APP_SCHEMA, 322, 332)
    ),
    ApplicationId::new(0)
  );
  b.resolve_application(ApplicationId::new(0), DEPRECATED);
  b.push_def_application(STRING, 1, ApplicationId::new(0));
  assert_eq!(
    string(&mut b, at(APP_SCHEMA, 341, 346), true, "old"),
    ValueId::new(0)
  );
  b.push_application_argument(
    ApplicationId::new(0),
    REASON,
    Some(at(APP_SCHEMA, 333, 339)),
    ValueId::new(0),
  );

  // `@deprecated`: `reason: string = "gone"`.
  assert_eq!(
    named(&mut b, at(CORE_LIB, 44, 50), false, STRING),
    TypeExprId::new(0)
  );
  b.set_member_type(REASON, TypeExprId::new(0));
  assert_eq!(
    string(&mut b, at(CORE_LIB, 53, 59), true, "gone"),
    ValueId::new(1)
  );
  b.set_member_default(REASON, ValueId::new(1));

  // `Node`: `id: string`.
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 21, 27), false, STRING),
    TypeExprId::new(1)
  );
  b.set_member_type(NODE_ID, TypeExprId::new(1));

  // `Query implements Node @cache<string>(ttl: [1, {"k": 2}])`.
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 122, 126), false, NODE),
    TypeExprId::new(2)
  );
  b.push_implements(QUERY, 0, TypeExprId::new(2));
  assert_eq!(
    application(
      &mut b,
      at(APP_SCHEMA, 127, 160),
      "cache",
      at(APP_SCHEMA, 128, 133)
    ),
    ApplicationId::new(1)
  );
  b.resolve_application(ApplicationId::new(1), CACHE);
  b.push_def_application(QUERY, 0, ApplicationId::new(1));
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 134, 140), false, STRING),
    TypeExprId::new(3)
  );
  b.set_application_type_args(ApplicationId::new(1), &[TypeExprId::new(3)]);
  assert_eq!(
    b.push_value(at(APP_SCHEMA, 147, 159), true),
    ValueId::new(2)
  );
  assert_eq!(
    int(&mut b, at(APP_SCHEMA, 148, 149), 1, "1"),
    ValueId::new(3)
  );
  assert_eq!(
    b.push_value(at(APP_SCHEMA, 151, 158), true),
    ValueId::new(4)
  );
  assert_eq!(
    string(&mut b, at(APP_SCHEMA, 152, 155), true, "k"),
    ValueId::new(5)
  );
  assert_eq!(
    int(&mut b, at(APP_SCHEMA, 157, 158), 2, "2"),
    ValueId::new(6)
  );
  b.set_map(ValueId::new(4), &[(ValueId::new(5), ValueId::new(6))]);
  b.set_list(ValueId::new(2), &[ValueId::new(3), ValueId::new(4)]);
  b.push_application_argument(
    ApplicationId::new(1),
    TTL,
    Some(at(APP_SCHEMA, 142, 145)),
    ValueId::new(2),
  );
  // `node(id: string = "x"): Node`, the field's type, then its argument.
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 195, 199), false, NODE),
    TypeExprId::new(4)
  );
  b.set_member_type(QUERY_NODE, TypeExprId::new(4));
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 174, 180), false, STRING),
    TypeExprId::new(5)
  );
  b.set_member_type(NODE_ARG, TypeExprId::new(5));
  assert_eq!(
    string(&mut b, at(APP_SCHEMA, 183, 186), true, "x"),
    ValueId::new(7)
  );
  b.set_member_default(NODE_ARG, ValueId::new(7));
  // `nodes: [Node!]!`.
  assert_eq!(
    b.push_type_expr(at(APP_SCHEMA, 212, 219), true),
    TypeExprId::new(6)
  );
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 213, 218), true, NODE),
    TypeExprId::new(7)
  );
  b.set_type_expr_list(TypeExprId::new(6), TypeExprId::new(7));
  b.set_member_type(QUERY_NODES, TypeExprId::new(6));

  // `Page<T = string> where T: Node { items: [T] }`.
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 414, 420), false, STRING),
    TypeExprId::new(8)
  );
  b.set_type_param_default(PAGE, 0, TypeExprId::new(8));
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 431, 435), false, NODE),
    TypeExprId::new(9)
  );
  b.push_bound(PAGE, 0, TypeExprId::new(9), at(APP_SCHEMA, 428, 435));
  assert_eq!(
    b.push_type_expr(at(APP_SCHEMA, 445, 448), false),
    TypeExprId::new(10)
  );
  assert_eq!(
    b.push_type_expr(at(APP_SCHEMA, 446, 447), false),
    TypeExprId::new(11)
  );
  b.resolve_param(TypeExprId::new(11), PAGE, 0);
  b.set_type_expr_list(TypeExprId::new(10), TypeExprId::new(11));
  b.set_member_type(ITEMS, TypeExprId::new(10));

  // `Search = Query | Page<Query>`.
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 515, 520), false, QUERY),
    TypeExprId::new(12)
  );
  b.push_union_member(SEARCH, 0, TypeExprId::new(12));
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 523, 534), false, PAGE),
    TypeExprId::new(13)
  );
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 528, 533), false, QUERY),
    TypeExprId::new(14)
  );
  b.set_type_expr_args(TypeExprId::new(13), &[TypeExprId::new(14)]);
  b.push_union_member(SEARCH, 0, TypeExprId::new(13));

  // `@cache<T>(ttl: T)`.
  assert_eq!(
    b.push_type_expr(at(APP_SCHEMA, 625, 626), false),
    TypeExprId::new(15)
  );
  b.resolve_param(TypeExprId::new(15), CACHE, 0);
  b.set_member_type(TTL, TypeExprId::new(15));

  // `Color` has no occurrence. `Filter`: `color: Color = BLUE`, `tags: [string] = ["a"]` and
  // `bad: Missing<string>`.
  assert_eq!(
    named(&mut b, at(APP_INNER, 72, 77), false, COLOR),
    TypeExprId::new(16)
  );
  b.set_member_type(FILTER_COLOR, TypeExprId::new(16));
  assert_eq!(b.push_value(at(APP_INNER, 80, 84), true), BLUE);
  b.set_member_default(FILTER_COLOR, BLUE);
  assert_eq!(
    b.push_type_expr(at(APP_INNER, 91, 99), false),
    TypeExprId::new(17)
  );
  assert_eq!(
    named(&mut b, at(APP_INNER, 92, 98), false, STRING),
    TypeExprId::new(18)
  );
  b.set_type_expr_list(TypeExprId::new(17), TypeExprId::new(18));
  b.set_member_type(FILTER_TAGS, TypeExprId::new(17));
  assert_eq!(b.push_value(at(APP_INNER, 102, 107), true), ValueId::new(9));
  assert_eq!(
    string(&mut b, at(APP_INNER, 103, 106), true, "a"),
    ValueId::new(10)
  );
  b.set_list(ValueId::new(9), &[ValueId::new(10)]);
  b.set_member_default(FILTER_TAGS, ValueId::new(9));
  assert_eq!(
    b.push_type_expr(at(APP_INNER, 113, 128), false),
    MISSING_TYPE
  );
  assert_eq!(
    named(&mut b, at(APP_INNER, 121, 127), false, STRING),
    MISSING_ARG
  );
  b.set_type_expr_args(MISSING_TYPE, &[MISSING_ARG]);
  b.set_member_type(FILTER_BAD, MISSING_TYPE);

  // `Point @missing { x: string }`, then the extension's `label: string @deprecated`, whose
  // `reason` a copy of its default fills.
  assert_eq!(
    application(
      &mut b,
      at(GEO_POINT, 11, 19),
      "missing",
      at(GEO_POINT, 12, 19)
    ),
    MISSING
  );
  b.push_def_application(POINT, 0, MISSING);
  assert_eq!(
    named(&mut b, at(GEO_POINT, 25, 31), false, STRING),
    TypeExprId::new(21)
  );
  b.set_member_type(POINT_X, TypeExprId::new(21));
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 727, 733), false, STRING),
    TypeExprId::new(22)
  );
  b.set_member_type(POINT_LABEL, TypeExprId::new(22));
  assert_eq!(
    application(
      &mut b,
      at(APP_SCHEMA, 734, 745),
      "deprecated",
      at(APP_SCHEMA, 735, 745)
    ),
    ApplicationId::new(3)
  );
  b.resolve_application(ApplicationId::new(3), DEPRECATED);
  b.push_member_application(POINT_LABEL, ApplicationId::new(3));
  assert_eq!(
    string(&mut b, at(CORE_LIB, 53, 59), false, "gone"),
    ValueId::new(11)
  );
  b.push_application_argument(ApplicationId::new(3), REASON, None, ValueId::new(11));

  // The schema: `query: Query`, `mutation: Missing`, then `@cache<string>(ttl: "5")`.
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 850, 855), false, QUERY),
    TypeExprId::new(23)
  );
  b.set_schema_root(RootOperation::Query, Root::Written(TypeExprId::new(23)));
  assert_eq!(
    b.push_type_expr(at(APP_SCHEMA, 867, 874), false),
    MISSING_ROOT
  );
  b.set_schema_root(RootOperation::Mutation, Root::Written(MISSING_ROOT));
  assert_eq!(
    application(
      &mut b,
      at(APP_SCHEMA, 807, 832),
      "cache",
      at(APP_SCHEMA, 808, 813)
    ),
    ApplicationId::new(4)
  );
  b.resolve_application(ApplicationId::new(4), CACHE);
  b.push_schema_application(0, ApplicationId::new(4));
  assert_eq!(
    named(&mut b, at(APP_SCHEMA, 814, 820), false, STRING),
    TypeExprId::new(25)
  );
  b.set_application_type_args(ApplicationId::new(4), &[TypeExprId::new(25)]);
  assert_eq!(
    string(&mut b, at(APP_SCHEMA, 827, 830), true, "5"),
    ValueId::new(12)
  );
  b.push_application_argument(
    ApplicationId::new(4),
    TTL,
    Some(at(APP_SCHEMA, 822, 825)),
    ValueId::new(12),
  );

  // The one error a phase reported: at `Missing`, inside `bad`.
  b.mark(at(APP_INNER, 113, 120));

  b.finish()
}

/// What `core` holds, walked whole.
const CORE_ALL: &[&str] = &[
  "enter_package 0",
  "enter_module 0",
  "enter_def 0",
  "application 0",
  "value 0 ApplicationArgument in 0",
  "exit_def 0",
  "enter_def 1",
  "enter_member 0",
  "type_expr 0 MemberType",
  "value 1 Default",
  "exit_member 0",
  "exit_def 1",
  "exit_module 0",
  "exit_package 0",
];

/// What `app` wrote in `core`: the extension of `string`.
const CORE_EXTENDED: &[&str] = &[
  "enter_package 0",
  "enter_module 0",
  "enter_def 0",
  "application 0",
  "value 0 ApplicationArgument in 0",
  "exit_def 0",
  "exit_module 0",
  "exit_package 0",
];

/// `alloc` and `std`, which hold nothing but their root modules.
const ALLOC_STD: &[&str] = &[
  "enter_package 1",
  "enter_module 1",
  "exit_module 1",
  "exit_package 1",
  "enter_package 2",
  "enter_module 2",
  "exit_module 2",
  "exit_package 2",
];

/// What `app` holds, all of it its own writing.
const APP_ALL: &[&str] = &[
  "enter_package 3",
  "enter_module 3",
  "enter_def 2",
  "enter_member 1",
  "type_expr 1 MemberType",
  "exit_member 1",
  "exit_def 2",
  "enter_def 3",
  "type_expr 2 Implements",
  "application 1",
  "type_expr 3 ApplicationTypeArgument in 1",
  "value 2 ApplicationArgument in 1",
  "value 3 ApplicationArgument in 1",
  "value 4 ApplicationArgument in 1",
  "value 5 ApplicationArgument in 1",
  "value 6 ApplicationArgument in 1",
  "enter_member 2",
  "type_expr 4 MemberType",
  "enter_member 4",
  "type_expr 5 MemberType",
  "value 7 Default",
  "exit_member 4",
  "exit_member 2",
  "enter_member 3",
  "type_expr 6 MemberType",
  "type_expr 7 MemberType",
  "exit_member 3",
  "exit_def 3",
  "enter_def 4",
  "type_expr 8 TypeParamDefault",
  "type_expr 9 Bound",
  "enter_member 5",
  "type_expr 10 MemberType",
  "type_expr 11 MemberType",
  "exit_member 5",
  "exit_def 4",
  "enter_def 5",
  "type_expr 12 UnionMember",
  "type_expr 13 UnionMember",
  "type_expr 14 UnionMember",
  "exit_def 5",
  "enter_def 6",
  "enter_member 6",
  "type_expr 15 MemberType",
  "exit_member 6",
  "exit_def 6",
  "exit_module 3",
  "enter_module 4",
  "enter_def 7",
  "enter_member 7",
  "exit_member 7",
  "enter_member 8",
  "exit_member 8",
  "exit_def 7",
  "enter_def 8",
  "enter_member 9",
  "type_expr 16 MemberType",
  "value 8 Default",
  "exit_member 9",
  "enter_member 10",
  "type_expr 17 MemberType",
  "type_expr 18 MemberType",
  "value 9 Default",
  "value 10 Default",
  "exit_member 10",
  "enter_member 11",
  "type_expr 19 MemberType",
  "type_expr 20 MemberType",
  "exit_member 11",
  "exit_def 8",
  "exit_module 4",
  "exit_package 3",
];

/// What `geo` holds, walked whole: `Point`, and the member `app`'s extension wrote.
const GEO_ALL: &[&str] = &[
  "enter_package 4",
  "enter_module 5",
  "enter_def 9",
  "application 2",
  "enter_member 12",
  "type_expr 21 MemberType",
  "exit_member 12",
  "enter_member 13",
  "type_expr 22 MemberType",
  "application 3",
  "value 11 ApplicationArgument in 3",
  "exit_member 13",
  "exit_def 9",
  "exit_module 5",
  "exit_package 4",
];

/// What `app` wrote in `geo`: the extension of `Point`.
const GEO_EXTENDED: &[&str] = &[
  "enter_package 4",
  "enter_module 5",
  "enter_def 9",
  "enter_member 13",
  "type_expr 22 MemberType",
  "application 3",
  "value 11 ApplicationArgument in 3",
  "exit_member 13",
  "exit_def 9",
  "exit_module 5",
  "exit_package 4",
];

/// The schema, which is the root package's.
const SCHEMA: &[&str] = &[
  "schema",
  "type_expr 23 RootOperation",
  "type_expr 24 RootOperation",
  "application 4",
  "type_expr 25 ApplicationTypeArgument in 4",
  "value 12 ApplicationArgument in 4",
];

/// The walk over `packages`, each the lines of the packages it enters: `begin`, each package's
/// lines, the schema's, `end`.
fn walk(packages: &[&[&'static str]]) -> Vec<&'static str> {
  let mut lines: Vec<&'static str> = vec!["begin"];
  for package in packages {
    lines.extend_from_slice(package);
  }
  lines.extend_from_slice(SCHEMA);
  lines.push("end");
  lines
}

/// Asserts that the walk `lines` recorded is `expected`, line by line.
fn same(lines: &[String], expected: &[&str]) {
  for (index, (line, want)) in lines.iter().zip(expected).enumerate() {
    assert_eq!(line.as_str(), *want, "line {index} of the walk");
  }
  assert_eq!(lines.len(), expected.len(), "the walk's length");
}

/// Writes down every hook the walk calls: its name and the id it is handed, and for an occurrence
/// where its outermost occurrence sits and the application it is in.
#[derive(Default)]
struct Recorder {
  lines: Vec<String>,
}

/// An occurrence's line: the hook, the id, the site, and the application it is in, if any.
fn occurrence(hook: &str, id: u32, cx: &Cx<'_>) -> String {
  let site: String = match cx.site() {
    Some(site) => format!("{site:?}"),
    None => String::from("nowhere"),
  };
  match cx.application() {
    Some(application) => format!("{hook} {id} {site} in {}", application.get()),
    None => format!("{hook} {id} {site}"),
  }
}

impl Visitor for Recorder {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.lines.push(String::from("begin"));
  }

  fn enter_package(&mut self, _cx: &mut Cx<'_>, package: PackageId) {
    self.lines.push(format!("enter_package {}", package.get()));
  }

  fn exit_package(&mut self, _cx: &mut Cx<'_>, package: PackageId) {
    self.lines.push(format!("exit_package {}", package.get()));
  }

  fn enter_module(&mut self, _cx: &mut Cx<'_>, module: ModuleId) {
    self.lines.push(format!("enter_module {}", module.get()));
  }

  fn exit_module(&mut self, _cx: &mut Cx<'_>, module: ModuleId) {
    self.lines.push(format!("exit_module {}", module.get()));
  }

  fn enter_def(&mut self, _cx: &mut Cx<'_>, def: DefId) {
    self.lines.push(format!("enter_def {}", def.get()));
  }

  fn exit_def(&mut self, _cx: &mut Cx<'_>, def: DefId) {
    self.lines.push(format!("exit_def {}", def.get()));
  }

  fn enter_member(&mut self, _cx: &mut Cx<'_>, member: MemberId) {
    self.lines.push(format!("enter_member {}", member.get()));
  }

  fn exit_member(&mut self, _cx: &mut Cx<'_>, member: MemberId) {
    self.lines.push(format!("exit_member {}", member.get()));
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    self.lines.push(occurrence("type_expr", ty.get(), cx));
  }

  fn application(&mut self, _cx: &mut Cx<'_>, application: ApplicationId) {
    self
      .lines
      .push(format!("application {}", application.get()));
  }

  fn value(&mut self, cx: &mut Cx<'_>, value: ValueId) {
    self.lines.push(occurrence("value", value.get(), cx));
  }

  fn schema(&mut self, _cx: &mut Cx<'_>) {
    self.lines.push(String::from("schema"));
  }

  fn end(&mut self, _cx: &mut Cx<'_>) {
    self.lines.push(String::from("end"));
  }
}

/// The walk `visit` makes over `analysis` for `packages`, one line per hook.
fn recorded(analysis: &Analysis, packages: Packages) -> Vec<String> {
  let mut recorder = Recorder::default();
  let diagnostics = visit(analysis, packages, &mut recorder);
  assert!(diagnostics.is_empty(), "the recorder reports nothing");
  recorder.lines
}

/// Every id each hook was handed, in the order it was handed them.
#[derive(Default)]
struct Census {
  begins: u32,
  schemas: u32,
  ends: u32,
  packages: Vec<u32>,
  left_packages: Vec<u32>,
  modules: Vec<u32>,
  left_modules: Vec<u32>,
  defs: Vec<u32>,
  left_defs: Vec<u32>,
  members: Vec<u32>,
  left_members: Vec<u32>,
  type_exprs: Vec<u32>,
  applications: Vec<u32>,
  values: Vec<u32>,
}

impl Visitor for Census {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.begins += 1;
  }

  fn enter_package(&mut self, _cx: &mut Cx<'_>, package: PackageId) {
    self.packages.push(package.get());
  }

  fn exit_package(&mut self, _cx: &mut Cx<'_>, package: PackageId) {
    self.left_packages.push(package.get());
  }

  fn enter_module(&mut self, _cx: &mut Cx<'_>, module: ModuleId) {
    self.modules.push(module.get());
  }

  fn exit_module(&mut self, _cx: &mut Cx<'_>, module: ModuleId) {
    self.left_modules.push(module.get());
  }

  fn enter_def(&mut self, _cx: &mut Cx<'_>, def: DefId) {
    self.defs.push(def.get());
  }

  fn exit_def(&mut self, _cx: &mut Cx<'_>, def: DefId) {
    self.left_defs.push(def.get());
  }

  fn enter_member(&mut self, _cx: &mut Cx<'_>, member: MemberId) {
    self.members.push(member.get());
  }

  fn exit_member(&mut self, _cx: &mut Cx<'_>, member: MemberId) {
    self.left_members.push(member.get());
  }

  fn type_expr(&mut self, _cx: &mut Cx<'_>, ty: TypeExprId) {
    self.type_exprs.push(ty.get());
  }

  fn application(&mut self, _cx: &mut Cx<'_>, application: ApplicationId) {
    self.applications.push(application.get());
  }

  fn value(&mut self, _cx: &mut Cx<'_>, value: ValueId) {
    self.values.push(value.get());
  }

  fn schema(&mut self, _cx: &mut Cx<'_>) {
    self.schemas += 1;
  }

  fn end(&mut self, _cx: &mut Cx<'_>) {
    self.ends += 1;
  }
}

/// The census of a walk over `analysis` for `packages`.
fn census_of(analysis: &Analysis, packages: Packages) -> Census {
  let mut census = Census::default();
  let diagnostics = visit(analysis, packages, &mut census);
  assert!(diagnostics.is_empty(), "the census reports nothing");
  census
}

/// `seen`, sorted, after asserting that no id of it was seen twice.
fn once(seen: &[u32], what: &str) -> Vec<u32> {
  let mut sorted: Vec<u32> = seen.to_vec();
  sorted.sort_unstable();
  sorted.dedup();
  assert_eq!(sorted.len(), seen.len(), "every {what} is reached once");
  sorted
}

/// Asserts that `seen` ascends: the order the walk handed a hook its ids in is id order.
fn ascending(seen: &[u32], what: &str) {
  assert!(
    seen.windows(2).all(|pair: &[u32]| pair[0] < pair[1]),
    "the {what} are reached in id order: {seen:?}"
  );
}

/// What a walk must reach: each kind's ids, those its covered contributions wrote and those that
/// hold them, in id order.
struct Expected {
  packages: &'static [u32],
  modules: &'static [u32],
  defs: &'static [u32],
  members: &'static [u32],
  type_exprs: &'static [u32],
  applications: &'static [u32],
  values: &'static [u32],
}

/// Every package's writing: every entity of the model.
const EVERY_PACKAGE: Expected = Expected {
  packages: &[0, 1, 2, 3, 4],
  modules: &[0, 1, 2, 3, 4, 5],
  defs: &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
  members: &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13],
  type_exprs: &[
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
  ],
  applications: &[0, 1, 2, 3, 4],
  values: &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
};

/// `app`'s writing and `geo`'s: all but `@deprecated`, which is `core`'s, and its member,
/// occurrence and default; `core` is entered for the extension of `string`.
const SUPPLIED: Expected = Expected {
  packages: &[0, 3, 4],
  modules: &[0, 3, 4, 5],
  defs: &[0, 2, 3, 4, 5, 6, 7, 8, 9],
  members: &[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13],
  type_exprs: &[
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
  ],
  applications: &[0, 1, 2, 3, 4],
  values: &[0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
};

/// `app`'s writing alone: also without what `geo` wrote of `Point`, its `@missing`, `x` and `x`'s
/// type; `geo` is entered for the extension of `Point`.
const ROOT: Expected = Expected {
  packages: &[0, 3, 4],
  modules: &[0, 3, 4, 5],
  defs: &[0, 2, 3, 4, 5, 6, 7, 8, 9],
  members: &[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13],
  type_exprs: &[
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 23, 24, 25,
  ],
  applications: &[0, 1, 3, 4],
  values: &[0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
};

/// Object safety: a walk runs through a borrowed trait object and through a boxed one.
#[test]
fn a_walk_runs_through_a_borrowed_and_a_boxed_trait_object() {
  let analysis = analysis_of(fixture(), &Limits::new());

  let mut recorder = Recorder::default();
  let borrowed: &mut dyn Visitor = &mut recorder;
  let diagnostics = visit(&analysis, Packages::Root, borrowed);
  assert!(diagnostics.is_empty());
  same(
    &recorder.lines,
    &walk(&[CORE_EXTENDED, APP_ALL, GEO_EXTENDED]),
  );

  let mut boxed: Box<dyn Visitor> = Box::new(EveryDef);
  let diagnostics = visit(&analysis, Packages::All, &mut *boxed);
  assert_eq!(diagnostics.len(), 10, "one diagnostic per definition");
}

/// The order: the walk over each selection, written out hook by hook, and within each kind of hook
/// the ids in id order, a field's arguments nested in it apart.
#[test]
fn each_walk_is_the_written_sequence_of_hooks_in_id_order() {
  let analysis = analysis_of(fixture(), &Limits::new());
  same(
    &recorded(&analysis, Packages::All),
    &walk(&[CORE_ALL, ALLOC_STD, APP_ALL, GEO_ALL]),
  );
  same(
    &recorded(&analysis, Packages::Supplied),
    &walk(&[CORE_EXTENDED, APP_ALL, GEO_ALL]),
  );
  same(
    &recorded(&analysis, Packages::Root),
    &walk(&[CORE_EXTENDED, APP_ALL, GEO_EXTENDED]),
  );

  let model = analysis.model();
  for packages in [Packages::All, Packages::Supplied, Packages::Root] {
    let census = census_of(&analysis, packages);
    ascending(&census.packages, "packages");
    ascending(&census.modules, "modules");
    ascending(&census.defs, "definitions");
    ascending(&census.type_exprs, "type occurrences");
    ascending(&census.applications, "applications");
    ascending(&census.values, "values");
    // A field's arguments come after every direct member in id order, and are reached nested in
    // their field: the direct members ascend, and the arguments do.
    let (arguments, direct): (Vec<u32>, Vec<u32>) = census
      .members
      .iter()
      .partition(|member: &&u32| model.member(MemberId::new(**member)).parent().is_some());
    ascending(&direct, "direct members");
    ascending(&arguments, "arguments");
  }
}

/// The census: for each selection, each hook is called once per entity the covered contributions
/// wrote, and once per package, module and definition that holds them; each id once.
#[test]
fn each_hook_is_called_once_per_entity_the_covered_contributions_wrote() {
  let analysis = analysis_of(fixture(), &Limits::new());
  for (packages, expected) in [
    (Packages::All, EVERY_PACKAGE),
    (Packages::Supplied, SUPPLIED),
    (Packages::Root, ROOT),
  ] {
    let census = census_of(&analysis, packages);
    assert_eq!(
      (census.begins, census.schemas, census.ends),
      (1, 1, 1),
      "{packages:?}"
    );
    for (seen, left, expected, what) in [
      (
        &census.packages,
        &census.left_packages,
        expected.packages,
        "package",
      ),
      (
        &census.modules,
        &census.left_modules,
        expected.modules,
        "module",
      ),
      (&census.defs, &census.left_defs, expected.defs, "definition"),
      (
        &census.members,
        &census.left_members,
        expected.members,
        "member",
      ),
    ] {
      assert_eq!(once(seen, what), expected, "{packages:?}: entered");
      assert_eq!(once(left, what), expected, "{packages:?}: left");
    }
    assert_eq!(
      once(&census.type_exprs, "type occurrence"),
      expected.type_exprs,
      "{packages:?}"
    );
    assert_eq!(
      once(&census.applications, "application"),
      expected.applications,
      "{packages:?}"
    );
    assert_eq!(
      once(&census.values, "value"),
      expected.values,
      "{packages:?}"
    );
  }
}

/// Checks, at every hook, where the context says the walk is, and writes down each member's
/// contribution.
#[derive(Default)]
struct Positions {
  /// Each member entered, with where the contribution the walk is reaching it under is named.
  members: Vec<(MemberId, Location)>,
}

/// Asserts that `cx` is at no entity.
fn nowhere(cx: &Cx<'_>, hook: &str) {
  assert_eq!(
    (cx.package(), cx.module(), cx.def(), cx.member()),
    (None, None, None, None),
    "{hook}"
  );
  assert!(cx.contribution().is_none(), "{hook}");
  assert_eq!((cx.site(), cx.application()), (None, None), "{hook}");
}

impl Visitor for Positions {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    nowhere(cx, "begin");
  }

  fn enter_package(&mut self, cx: &mut Cx<'_>, package: PackageId) {
    assert_eq!(cx.package(), Some(package));
    assert_eq!((cx.module(), cx.def(), cx.member()), (None, None, None));
    assert!(cx.contribution().is_none());
  }

  fn enter_module(&mut self, cx: &mut Cx<'_>, module: ModuleId) {
    assert_eq!(cx.module(), Some(module));
    assert_eq!(cx.package(), Some(cx.model().module(module).package()));
    assert_eq!(cx.def(), None);
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    assert_eq!(cx.def(), Some(def));
    assert_eq!(cx.module(), Some(cx.model().def(def).module()));
    assert_eq!(cx.member(), None);
    assert!(cx.contribution().is_none(), "no contribution yet");
  }

  fn exit_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    assert_eq!(cx.def(), Some(def));
    assert!(cx.contribution().is_none(), "no contribution any more");
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    let view = cx.model().member(member);
    assert_eq!(cx.member(), Some(member));
    assert_eq!(cx.def(), Some(view.owner()));
    let Some(contribution) = cx.contribution() else {
      panic!("member {} is reached under no contribution", member.get());
    };
    // An argument is its field's contribution's.
    assert!(
      contribution
        .members()
        .contains(view.parent().unwrap_or(member))
    );
    self.members.push((member, contribution.location()));
  }

  fn exit_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    assert_eq!(cx.member(), Some(member));
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, _ty: TypeExprId) {
    assert!(cx.site().is_some(), "an occurrence has a site");
  }

  fn application(&mut self, cx: &mut Cx<'_>, application: ApplicationId) {
    assert_eq!(cx.application(), Some(application));
    assert_eq!(cx.site(), None, "an application is no occurrence");
  }

  fn value(&mut self, cx: &mut Cx<'_>, _value: ValueId) {
    assert!(cx.site().is_some(), "a value has a site");
  }

  fn schema(&mut self, cx: &mut Cx<'_>) {
    assert_eq!(cx.package(), Some(cx.model().root_package()));
    assert_eq!((cx.module(), cx.def(), cx.member()), (None, None, None));
    assert!(cx.contribution().is_none());
  }

  fn end(&mut self, cx: &mut Cx<'_>) {
    nowhere(cx, "end");
  }
}

/// The context says where the walk is, and each member is reached under the contribution that
/// wrote it, whose source says which package wrote it.
#[test]
fn the_context_says_where_the_walk_is_and_who_wrote_each_member() {
  let analysis = analysis_of(fixture(), &Limits::new());
  let mut positions = Positions::default();
  let diagnostics = visit(&analysis, Packages::All, &mut positions);
  assert!(diagnostics.is_empty());

  let writer = |member: MemberId| -> PackageId {
    let (_, location) = positions
      .members
      .iter()
      .find(|(seen, _): &&(MemberId, Location)| *seen == member)
      .copied()
      .unwrap_or_else(|| panic!("member {} is reached", member.get()));
    analysis
      .sources()
      .source(SourceId::new(location.source()))
      .package()
  };
  assert_eq!(writer(REASON), CORE);
  assert_eq!(writer(NODE_ARG), APP, "an argument, under its field's");
  assert_eq!(writer(POINT_X), GEO, "`Point`'s own member");
  assert_eq!(
    writer(POINT_LABEL),
    APP,
    "the member `app`'s extension wrote"
  );
  let label = positions
    .members
    .iter()
    .find(|(seen, _): &&(MemberId, Location)| *seen == POINT_LABEL)
    .map(|(_, location): &(MemberId, Location)| *location);
  assert_eq!(
    label,
    Some(at(APP_SCHEMA, 712, 717)),
    "the extension itself"
  );
  assert_eq!(positions.members.len(), 14);

  // The contribution that wrote `label` says it is an extension; `Point`'s own does not.
  let extensions: Vec<bool> = analysis
    .model()
    .def(POINT)
    .contributions()
    .map(|part: Contribution<'_>| part.is_extension())
    .collect::<Vec<bool>>();
  assert_eq!(extensions, [false, true]);
}

/// Reports an error at every definition it enters, holding the definition's view while it
/// reports.
struct EveryDef;

impl Visitor for EveryDef {
  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let view = cx.model().def(def);
    cx.report(
      Diagnostic::builder(EVERY_DEF, view.location())
        .message(format!("`{}` is defined", view.name()))
        .build(),
    );
    assert_eq!(view.id(), def, "the view outlives the report");
  }
}

/// The ceiling the analysis was made with holds for a consumer's walk too: the least diagnostics
/// are kept, the list says it was truncated, and every error is counted.
#[test]
fn a_walk_keeps_the_least_diagnostics_under_the_analysis_ceiling_and_counts_every_error() {
  let analysis = analysis_of(fixture(), &Limits::new().with_max_diagnostics(2));
  let diagnostics = visit(&analysis, Packages::All, &mut EveryDef);
  assert!(diagnostics.truncated());
  assert_eq!(diagnostics.error_count(), 10);
  assert!(diagnostics.has_errors());
  let kept: Vec<Location> = diagnostics
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.primary())
    .collect::<Vec<Location>>();
  assert_eq!(kept, [at(CORE_LIB, 7, 13), at(CORE_LIB, 25, 35)]);

  let unbounded = analysis_of(fixture(), &Limits::new());
  let diagnostics = visit(&unbounded, Packages::All, &mut EveryDef);
  assert!(!diagnostics.truncated());
  assert_eq!((diagnostics.len(), diagnostics.error_count()), (10, 10));
  assert!(
    unbounded.diagnostics().is_empty(),
    "the walk's list is its own"
  );
}

/// Tolerance: a walk over a model holding each unresolved shape, and an entity a reported error
/// marks, reaches every one of them, a written argument of an occurrence that did not resolve
/// included.
#[test]
fn a_walk_reaches_every_unresolved_shape_and_every_marked_entity() {
  let analysis = analysis_of(fixture(), &Limits::new());
  let model = analysis.model();
  assert_eq!(
    model.type_expr(MISSING_TYPE).kind(),
    TypeExprKind::Unresolved
  );
  assert_eq!(
    model.type_expr(MISSING_ROOT).kind(),
    TypeExprKind::Unresolved
  );
  assert_eq!(model.value(BLUE).kind(), ValueKind::Unresolved);
  assert_eq!(
    model.application(MISSING).directive(),
    Resolution::Unresolved
  );
  assert!(model.member(FILTER_BAD).has_errors() && model.def(FILTER).has_errors());

  for packages in [Packages::All, Packages::Supplied] {
    let census = census_of(&analysis, packages);
    for occurrence in [MISSING_TYPE, MISSING_ARG, MISSING_ROOT] {
      assert!(
        census.type_exprs.contains(&occurrence.get()),
        "{packages:?}: type occurrence {}",
        occurrence.get()
      );
    }
    assert!(census.values.contains(&BLUE.get()), "{packages:?}");
    assert!(census.applications.contains(&MISSING.get()), "{packages:?}");
    assert!(census.members.contains(&FILTER_BAD.get()), "{packages:?}");
    assert!(census.defs.contains(&FILTER.get()), "{packages:?}");
  }
}

/// Warns at every member it enters.
struct EveryMember;

impl Visitor for EveryMember {
  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    let at = cx.model().member(member).location();
    cx.report(
      Diagnostic::builder(EVERY_MEMBER, at)
        .severity(Severity::Warning)
        .build(),
    );
  }
}

/// The diagnostics a walk over every package answers for `visitor`.
fn reported<V: Visitor + ?Sized>(analysis: &Analysis, visitor: &mut V) -> Diagnostics {
  visit(analysis, Packages::All, visitor)
}

/// A composition reports what its elements report each alone, in either order: one list in the
/// one order, with every error counted.
#[test]
fn a_composition_reports_what_its_elements_report_alone_in_either_order() {
  let analysis = analysis_of(fixture(), &Limits::new());
  let forward = reported(&analysis, &mut (EveryDef, EveryMember));
  let backward = reported(&analysis, &mut (EveryMember, EveryDef));
  let mut separate = reported(&analysis, &mut EveryDef);
  separate.merge(reported(&analysis, &mut EveryMember));
  assert_eq!(forward, backward);
  assert_eq!(forward, separate);
  assert_eq!(forward.len(), 24, "ten definitions and fourteen members");
  assert_eq!(forward.error_count(), 10, "the members' are warnings");
}

/// `None` does no work: a composition with a `None` element reports exactly what the composition
/// without it reports, and `Some` reports what its visitor does. `()` and a one-element tuple are
/// visitors too.
#[test]
fn a_none_element_does_no_work_and_a_some_element_is_its_visitor() {
  let analysis = analysis_of(fixture(), &Limits::new());
  let alone = reported(&analysis, &mut EveryDef);
  let off: Option<EveryMember> = None;
  assert_eq!(reported(&analysis, &mut (EveryDef, off)), alone);
  assert_eq!(reported(&analysis, &mut (EveryDef,)), alone);
  assert_eq!(
    reported(&analysis, &mut (EveryDef, Some(EveryMember))),
    reported(&analysis, &mut (EveryDef, EveryMember))
  );
  assert!(reported(&analysis, &mut None::<EveryDef>).is_empty());
  assert!(reported(&analysis, &mut ()).is_empty());
}

/// A borrowed element and a boxed one, each trait objects included, behave as their visitors do.
#[test]
fn borrowed_and_boxed_elements_behave_as_their_visitors_do() {
  let analysis = analysis_of(fixture(), &Limits::new());
  let by_value = reported(&analysis, &mut (EveryDef, EveryMember));

  let mut defs = EveryDef;
  let mut members = EveryMember;
  assert_eq!(
    reported(&analysis, &mut (&mut defs, &mut members)),
    by_value
  );
  let borrowed: &mut dyn Visitor = &mut defs;
  assert_eq!(reported(&analysis, &mut (borrowed, EveryMember)), by_value);

  let boxed: Box<dyn Visitor> = Box::new(EveryMember);
  assert_eq!(
    reported(&analysis, &mut (Box::new(EveryDef), boxed)),
    by_value
  );
  let mut erased: Box<dyn Visitor> = Box::new((EveryDef, EveryMember));
  assert_eq!(reported(&analysis, &mut erased), by_value);
}

/// The widest tuple composes, and each element reports for itself: the list keeps every copy.
#[test]
fn a_tuple_of_twelve_visitors_composes() {
  let analysis = analysis_of(fixture(), &Limits::new());
  let twelve = reported(
    &analysis,
    &mut (
      EveryDef, EveryDef, EveryDef, EveryDef, EveryDef, EveryDef, EveryDef, EveryDef, EveryDef,
      EveryDef, EveryDef, EveryDef,
    ),
  );
  assert_eq!((twelve.len(), twelve.error_count()), (120, 120));
}

/// Reports an error at each of its locations as the walk begins.
struct ErrorsAt(Vec<Location>);

impl Visitor for ErrorsAt {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    for location in &self.0 {
      cx.report(Diagnostic::builder(REFUSED, *location).build());
    }
  }
}

/// Warns at its location as the walk begins.
struct WarnsAt(Location);

impl Visitor for WarnsAt {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    cx.report(
      Diagnostic::builder(REFUSED, self.0)
        .severity(Severity::Warning)
        .build(),
    );
  }
}

/// Checks, as the walk ends and so after every other rule has reported, that the model shows the
/// phases' mark and no mark of a rule's error.
#[derive(Default)]
struct SeesThePhasesAlone {
  checked: bool,
}

impl Visitor for SeesThePhasesAlone {
  fn end(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    assert!(model.member(FILTER_BAD).has_errors(), "the phases' mark");
    assert!(!model.member(NODE_ARG).has_errors(), "no rule's mark yet");
    assert!(!model.member(QUERY_NODE).has_errors(), "no rule's mark yet");
    assert!(!model.def(QUERY).has_errors(), "no rule's mark yet");
    self.checked = true;
  }
}

/// Where the refusing rule's errors point: inside `node`'s argument `id`, at `app`'s package
/// source, and between two of `app`'s definitions.
fn refused_at() -> Vec<Location> {
  vec![
    at(APP_SCHEMA, 174, 180),
    Location::entire(APP_SOURCE),
    at(APP_SCHEMA, 270, 290),
  ]
}

/// The members and the definitions of `analysis` whose `has_errors()` is set, by index.
fn marked(analysis: &Analysis) -> (Vec<u32>, Vec<u32>) {
  let model = analysis.model();
  let members: Vec<u32> = (0..14)
    .filter(|index: &u32| model.member(MemberId::new(*index)).has_errors())
    .collect::<Vec<u32>>();
  let defs: Vec<u32> = model
    .defs()
    .into_iter()
    .filter(|def: &DefId| model.def(*def).has_errors())
    .map(|def: DefId| def.get())
    .collect::<Vec<u32>>();
  (members, defs)
}

/// Marks: an error one of the analysis's rules reports inside a member marks every member whose
/// extent holds it, the contribution that holds it and that contribution's definition, and nothing
/// else; an error at a package source or between two definitions, and a warning, mark nothing. A
/// rule sees the phases' marks and none of the rules', whichever is composed first.
#[test]
fn an_error_a_rule_reports_marks_what_holds_it_once_the_walk_has_ended() {
  for refusing_first in [true, false] {
    let mut refuses = (ErrorsAt(refused_at()), WarnsAt(at(APP_INNER, 72, 77)));
    let mut checks = SeesThePhasesAlone::default();
    let analysis = if refusing_first {
      conclude(fixture(), &Limits::new(), &mut (&mut refuses, &mut checks))
    } else {
      conclude(fixture(), &Limits::new(), &mut (&mut checks, &mut refuses))
    };
    assert!(checks.checked, "the checking rule ran");

    // `node`'s argument and `node` itself, beside the phases' mark on `bad`; then `Query`, whose
    // one contribution holds both, beside `Filter`.
    assert_eq!(marked(&analysis), (vec![2, 4, 11], vec![3, 8]));
    let query: Vec<bool> = analysis
      .model()
      .def(QUERY)
      .contributions()
      .map(|part: Contribution<'_>| part.has_errors())
      .collect::<Vec<bool>>();
    assert_eq!(query, [true]);

    // The analysis's diagnostics are the rules': three errors and a warning.
    let diagnostics = analysis.diagnostics();
    assert_eq!((diagnostics.len(), diagnostics.error_count()), (4, 3));
    assert!(!analysis.is_ok());
  }
}

/// An error marks what holds it whether the ceiling keeps it or not: its mark is noted as it is
/// reported.
#[test]
fn an_error_the_ceiling_drops_marks_all_the_same() {
  let analysis = conclude(
    fixture(),
    &Limits::new().with_max_diagnostics(0),
    &mut ErrorsAt(refused_at()),
  );
  let diagnostics = analysis.diagnostics();
  assert!(diagnostics.is_empty() && diagnostics.truncated());
  assert_eq!(diagnostics.error_count(), 3);
  assert_eq!(marked(&analysis), (vec![2, 4, 11], vec![3, 8]));
}

/// An error that points at no definition marks nothing: one at a package source, and one between
/// two definitions.
#[test]
fn an_error_that_points_at_no_definition_marks_nothing() {
  let outside = vec![Location::entire(APP_SOURCE), at(APP_SCHEMA, 270, 290)];
  let analysis = conclude(fixture(), &Limits::new(), &mut ErrorsAt(outside));
  assert_eq!(analysis.diagnostics().error_count(), 2);
  assert_eq!(
    marked(&analysis),
    (vec![11], vec![8]),
    "the phases' mark alone"
  );
}

/// A consumer's walk marks nothing, whatever it reports: the analysis is not its to change.
#[test]
fn a_consumers_walk_marks_nothing() {
  let analysis = conclude(fixture(), &Limits::new(), &mut ());
  let before = marked(&analysis);
  assert_eq!(before, (vec![11], vec![8]), "the phases' mark alone");
  let diagnostics = visit(&analysis, Packages::All, &mut ErrorsAt(refused_at()));
  assert_eq!(diagnostics.error_count(), 3);
  assert_eq!(marked(&analysis), before);
  assert!(analysis.is_ok(), "the consumer's errors are its own");
}

// Every type this file's walk adds may cross threads.
const _: () = {
  const fn assert_send_sync<T: Send + Sync>() {}
  let _ = assert_send_sync::<Packages>;
  let _ = assert_send_sync::<Site>;
};
