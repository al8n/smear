//! The GraphQLx analyzer's rules on what is judged of types: a marker has no field, is named only
//! in `implements` and in bounds, and is what a scalar, an enum, an input object or a union
//! implements; a set has one type parameter and a map two; `TypeRef` stands only in a directive's
//! arguments; `NonZero`'s argument is an integer primitive; and a definition's type parameters
//! have distinct names. Each rule says nothing within what a phase refused, and quotes a name only
//! as an excerpt.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Diagnostic, IntegerProvider, Layout, Limits, MemberId, Model, Namespace, Package,
    PackageId, SourceFile, SourceId, Tier, TyId, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// The code of a marker used as a marker may not be.
const MARKER: &str = "smear::graphqlx::marker-misuse";

/// The code of a collection whose count of type parameters it has not.
const COLLECTION: &str = "smear::graphqlx::collection-arity";

/// The code of an extension its package may not write, which the orphan rule reports.
const ORPHAN: &str = "smear::graphqlx::orphan-extension";

/// The code of `TypeRef` where it may not stand.
const TYPEREF: &str = "smear::graphqlx::typeref-misuse";

/// The code of `NonZero` of what is no integer primitive.
const NONZERO: &str = "smear::graphqlx::nonzero-argument";

/// The code of a type parameter with the name of an earlier one.
const DUPLICATE: &str = "smear::graphqlx::duplicate-type-parameter";

/// The code of a count of type arguments that does not match.
const ARITY: &str = "smear::graphqlx::generic-arity";

/// The code of a type name that names nothing.
const UNDEFINED: &str = "smear::schema::undefined-type";

/// The code of a root operation type that names nothing.
const UNDEFINED_ROOT: &str = "smear::schema::undefined-root-operation-type";

/// GraphQL's code of a field's argument typed by what is no input type.
const ARGUMENT_TYPE: &str = "smear::schema::argument-type-not-input-type";

/// GraphQL's code of an input field typed by what is no input type.
const INPUT_FIELD_TYPE: &str = "smear::schema::input-field-type-not-input-type";

/// GraphQL's code of a union member that is no object type.
const UNION_MEMBER: &str = "smear::schema::union-member-not-object";

/// GraphQL's code of a directive applied where its definition does not list.
const LOCATION: &str = "smear::schema::unsupported-directive-location";

/// What the rule on `NonZero`'s argument says, whatever the argument is.
const NONZERO_MESSAGE: &str = "`NonZero` takes an integer primitive at least one bit wide, written \
                               without `!`, as its type argument, and this one is not";

/// A package named `name` of `layout` at tier `Std`, holding `files`, `(path, text)`.
fn package(
  name: &str,
  layout: Layout,
  files: &[(&'static str, &'static str)],
) -> Package<&'static str> {
  Package::new(name, layout, Tier::Std).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'static str, &'static str)| SourceFile::new(path, text)),
  )
}

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`.
fn analyze_one(text: &'static str) -> Analysis {
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
  analyze(&root, &[], &Limits::new())
}

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`, whose
/// text is `text`.
fn analyze_text(text: &str) -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], &Limits::new())
}

/// The id of the file at `path` in the package `package`.
fn file(analysis: &Analysis, package: u32, path: &str) -> SourceId {
  let sources = analysis.sources();
  analysis
    .model()
    .package(PackageId::new(package))
    .sources()
    .into_iter()
    .find(|id: &SourceId| sources.source(*id).path() == path)
    .unwrap_or_else(|| panic!("no file `{path}` in package {package}"))
}

/// The root package's one file, `schema.graphqlx`.
fn schema_file(analysis: &Analysis) -> SourceId {
  file(analysis, 3, "schema.graphqlx")
}

/// Where the `nth` occurrence, counting from 0, of `needle` is in `text`, the file `source`.
fn at(source: SourceId, text: &str, needle: &str, nth: usize) -> Location {
  let start = text
    .match_indices(needle)
    .nth(nth)
    .map(|(start, _): (usize, &str)| start)
    .unwrap_or_else(|| panic!("`{needle}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + needle.len()))
}

/// Where the `nth`, counting from 0, of the names `name` written right after `before` is in `text`,
/// the file `source`: a name, so one followed by no letter, digit or `_`.
fn named(source: SourceId, text: &str, before: &str, name: &str, nth: usize) -> Location {
  let written = format!("{before}{name}");
  let start = text
    .match_indices(written.as_str())
    .map(|(start, _): (usize, &str)| start + before.len())
    .filter(|start: &usize| {
      text[start + name.len()..]
        .chars()
        .next()
        .is_none_or(|next: char| !(next.is_ascii_alphanumeric() || next == '_'))
    })
    .nth(nth)
    .unwrap_or_else(|| panic!("`{written}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + name.len()))
}

/// Each diagnostic's code, primary location, and first label's location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location, Option<Location>)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.code().as_str(),
        diagnostic.primary(),
        diagnostic.label(0).map(|label| label.location()),
      )
    })
    .collect()
}

/// Each diagnostic's code, in the list's order.
fn codes(analysis: &Analysis) -> Vec<&'static str> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.code().as_str())
    .collect()
}

/// Each diagnostic's message, in the list's order.
fn messages(analysis: &Analysis) -> Vec<String> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect()
}

/// The type of the field `field` of the type the canonical name `owner` names.
fn field_type(model: &Model, owner: &str, field: &str) -> TyId {
  let owner = model
    .lookup(Namespace::Type, owner)
    .unwrap_or_else(|| panic!("no type `{owner}`"));
  let member = model
    .def(owner)
    .fields()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == field)
    .unwrap_or_else(|| panic!("no field `{field}`"));
  let written = model.member(member).ty().expect("a field has a type");
  model.type_expr(written).ty()
}

/// A marker, and every place one may stand and may not: an `implements` entry and a bound's
/// interface, then a field's type, a list's element, a field's argument, an input field, a union
/// member, a type argument, an argument of an `implements` entry, a type parameter's default, an
/// application's type argument and a root.
const SITES: &str = "interface M @marker\ninterface I<A> {\n  id: ID\n}\n\
                     scalar S implements M\nscalar B<T> where T: M\ndirective @d<T> on OBJECT\n\
                     type Field {\n  m: M\n}\ntype List {\n  l: [M]\n}\n\
                     type Argument {\n  f(a: M): Int\n}\ninput Input {\n  m: M\n}\nunion U = M\n\
                     type Arg {\n  b: B<M>\n}\ntype X implements I<M> {\n  id: ID\n}\n\
                     scalar P<T = M>\ntype Y @d<M> {\n  id: ID\n}\nschema {\n  query: M\n}\n";

#[test]
fn a_marker_stands_in_implements_and_where_and_nowhere_else() {
  let analysis = analyze_one(SITES);
  let schema = schema_file(&analysis);
  let at_marker = |before: &str, nth: usize| -> (&'static str, Location, Option<Location>) {
    (MARKER, named(schema, SITES, before, "M", nth), None)
  };
  // A marker is an interface, so as a field's argument and as an input field it is also no input
  // type, and as a union member no object type, which GraphQL's own rules say at the same token.
  assert_eq!(
    found(&analysis),
    [
      at_marker("m: ", 0),
      at_marker("[", 0),
      at_marker("a: ", 0),
      (ARGUMENT_TYPE, named(schema, SITES, "a: ", "M", 0), None),
      at_marker("m: ", 1),
      (INPUT_FIELD_TYPE, named(schema, SITES, "m: ", "M", 1), None),
      at_marker("U = ", 0),
      (UNION_MEMBER, named(schema, SITES, "U = ", "M", 0), None),
      at_marker("B<", 0),
      at_marker("I<", 0),
      at_marker("T = ", 0),
      at_marker("@d<", 0),
      at_marker("query: ", 0),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis)[0],
    "the marker `M` is named here, and a marker is named only in an `implements` list or as a \
     bound's interface"
  );
}

#[test]
fn a_marker_has_no_field() {
  // Once for a marker with two fields, at the first; at the field an extension of its own package
  // adds; and nothing for a marker with none.
  let text = "interface Two @marker {\n  a: Int\n  b: Int\n}\ninterface Grown @marker\n\
              extend interface Grown {\n  c: Int\n}\ninterface Empty @marker\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        MARKER,
        named(schema, text, "  ", "a", 0),
        Some(named(schema, text, "interface ", "Two", 0))
      ),
      (
        MARKER,
        named(schema, text, "  ", "c", 0),
        Some(named(schema, text, "interface ", "Grown", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis)[0],
    "the marker `Two` declares a field, and a marker declares none: it says what a type is, and \
     holds nothing"
  );
}

#[test]
fn only_a_marker_is_implemented_by_a_scalar_an_enum_an_input_or_a_union() {
  let text = "interface Node {\n  id: ID\n}\nscalar S implements Node\n\
              enum E implements Node {\n  A\n}\ninput In implements Node {\n  x: Int\n}\n\
              union U implements Node = T\ntype T implements Node {\n  id: ID\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let entry = |owner: &str| -> (&'static str, Location, Option<Location>) {
    (
      MARKER,
      named(schema, text, &format!("{owner} implements "), "Node", 0),
      None,
    )
  };
  assert_eq!(
    found(&analysis),
    [entry("S"), entry("E"), entry("In"), entry("U")],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the scalar `S` implements `Node`, which is no marker, and a scalar implements markers alone",
      "the enum `E` implements `Node`, which is no marker, and an enum implements markers alone",
      "the input object `In` implements `Node`, which is no marker, and an input object implements \
       markers alone",
      "the union `U` implements `Node`, which is no marker, and a union implements markers alone",
    ]
  );
}

#[test]
fn a_type_parameter_is_no_marker_to_implement() {
  // A scalar and an input object that implement their own type parameter are each refused at the
  // entry. An object that does is not this rule's: whether an object's entry names an interface is
  // GraphQL's own rule, which refuses it with its own code.
  let text = "scalar S<T> implements T\ninput In<T> implements T {\n  x: Int\n}\n\
              type O<T> implements T {\n  id: ID\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        MARKER,
        named(schema, text, "S<T> implements ", "T", 0),
        None
      ),
      (
        MARKER,
        named(schema, text, "In<T> implements ", "T", 0),
        None
      ),
      (
        "smear::schema::implements-non-interface",
        named(schema, text, "O<T> implements ", "T", 0),
        None
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the scalar `S` implements its type parameter `T`, which is no marker, and a scalar \
       implements markers alone",
      "the input object `In` implements its type parameter `T`, which is no marker, and an input \
       object implements markers alone",
      "the type `O` implements its type parameter `T`, and a type parameter is no interface",
    ]
  );
}

#[test]
fn a_generic_marker_is_a_marker() {
  // `Tagged<T>` is a marker: an `implements` entry and a bound may name it, and `S`, which
  // declares `Tagged<Int>`, satisfies the bound; a field may not.
  let text = "interface Tagged<T> @marker\nscalar S implements Tagged<Int>\n\
              scalar D<X> where X: Tagged<Int>\ntype Q {\n  d: D<S>\n  f: Tagged<Int>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(MARKER, at(schema, text, "Tagged<Int>", 2), None)],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_set_has_one_type_parameter_and_a_map_two() {
  // A set of one and a map of two say nothing; a set of none and of two, and a map of one and of
  // three, are refused at the application; a scalar that applies both is refused once, at the
  // later; an object is not this rule's, and `@set` on one is the rule on directive locations'; and
  // the root package's `@set` on `alloc`'s `string` is the orphan rule's alone.
  let text = "scalar S1<T> @set\nscalar M2<K, V> @map\nscalar S0 @set\nscalar S2<A, B> @set\n\
              scalar M1<K> @map\nscalar M3<A, B, C> @map\nscalar Both<K, V> @set @map\n\
              type Obj @set {\n  x: Int\n}\nextend scalar String @set\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let refused = |directive: &str, nth: usize, scalar: &str| {
    (
      COLLECTION,
      named(schema, text, "@", directive, nth),
      Some(named(schema, text, "scalar ", scalar, 0)),
    )
  };
  assert_eq!(
    found(&analysis),
    [
      refused("set", 1, "S0"),
      refused("set", 2, "S2"),
      refused("map", 1, "M1"),
      refused("map", 2, "M3"),
      refused("map", 3, "Both"),
      (
        LOCATION,
        named(schema, text, "@", "set", 4),
        Some(model.def(model.builtins().set()).location())
      ),
      (
        ORPHAN,
        named(schema, text, "@", "set", 5),
        Some(model.def(model.builtins().string()).location())
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis)[..5],
    [
      "the scalar `S0` applies `@set`, which makes a scalar of 1 type parameter a set, and it \
       declares 0",
      "the scalar `S2` applies `@set`, which makes a scalar of 1 type parameter a set, and it \
       declares 2",
      "the scalar `M1` applies `@map`, which makes a scalar of 2 type parameters a map, and it \
       declares 1",
      "the scalar `M3` applies `@map`, which makes a scalar of 2 type parameters a map, and it \
       declares 3",
      "the scalar `Both` applies both `@set` and `@map`, and a scalar is a set or a map, not both",
    ]
  );

  // The root package's own `String` replaces the built-in, so its `@set` is its own package's, no
  // orphan, and judged here: the replacement declares no type parameter.
  let text = "scalar String\nextend scalar String @set\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      COLLECTION,
      named(schema, text, "@", "set", 0),
      Some(named(schema, text, "scalar ", "String", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the scalar `string` applies `@set`, which makes a scalar of 1 type parameter a set, and it \
      declares 0"
    ]
  );
}

#[test]
fn another_supplied_packages_extension_is_the_orphan_rules_alone() {
  // The root package's `@set` on a dependency's scalar of two type parameters, and the root
  // package's field on a dependency's marker: what another package writes gives a definition
  // nothing, so each is the orphan rule's alone, and neither the collection's nor the marker's.
  for (theirs, ours) in [
    ("scalar Pair<A, B>\n", "extend scalar ^::geo::Pair @set\n"),
    (
      "interface Mark @marker\n",
      "extend interface ^::geo::Mark {\n  f: Int\n}\n",
    ),
  ] {
    let root =
      package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
    let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
    let analysis = analyze(&root, &[geo], &Limits::new());
    assert_eq!(
      codes(&analysis),
      [ORPHAN],
      "{ours}: {:#?}",
      analysis.diagnostics()
    );
  }
}

/// `TypeRef` where it may stand, a directive definition's argument, alone and in a list, and an
/// input field; then where it may not: a type argument of a directive's argument, a collection's
/// and another's, a field's type, a field's argument, an application's type argument, a union
/// member and a default; and an application's type argument where the application stands on a
/// directive definition's argument and on an input field, whose member alone would admit it.
const TYPE_REFS: &str = "directive @d(of: TypeRef, many: [TypeRef!]) on OBJECT\n\
                         input Binding {\n  type: TypeRef!\n}\nscalar Box<T>\n\
                         directive @t<T> on OBJECT | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION\n\
                         directive @m(map: HashMap<string, TypeRef>) on OBJECT\n\
                         directive @b(boxed: [Box<TypeRef>]) on OBJECT\n\
                         type Field {\n  f: TypeRef\n}\ntype Argument {\n  f(a: TypeRef): Int\n}\n\
                         type Applied @t<TypeRef> {\n  id: ID\n}\nunion U = TypeRef\n\
                         scalar P<T = TypeRef>\ndirective @e(x: Int @t<TypeRef>) on OBJECT\n\
                         input In {\n  y: Int @t<TypeRef>\n}\n";

#[test]
fn type_ref_is_a_directive_arguments_type() {
  let analysis = analyze_one(TYPE_REFS);
  let schema = schema_file(&analysis);
  let refused = |before: &str| -> (&'static str, Location, Option<Location>) {
    (
      TYPEREF,
      named(schema, TYPE_REFS, before, "TypeRef", 0),
      None,
    )
  };
  assert_eq!(
    found(&analysis),
    [
      refused("string, "),
      refused("Box<"),
      // The field's, indented: `of: TypeRef`, the argument of `@d`, holds `f: TypeRef` too.
      refused("  f: "),
      refused("a: "),
      refused("@t<"),
      refused("U = "),
      // `TypeRef` is a scalar, so as a union member it is also no object type.
      (
        UNION_MEMBER,
        named(schema, TYPE_REFS, "U = ", "TypeRef", 0),
        None
      ),
      refused("T = "),
      (
        TYPEREF,
        named(schema, TYPE_REFS, "Int @t<", "TypeRef", 0),
        None
      ),
      (
        TYPEREF,
        named(schema, TYPE_REFS, "Int @t<", "TypeRef", 1),
        None
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis)[0],
    "`TypeRef` names a definition, and stands only as the type of a directive definition's \
     argument or of an input field, a list of it included"
  );
}

/// `NonZero` of integer primitives, written and through an alias; then of a float, `string`, `u0`,
/// a list, an object, an integer written with `!` and a type parameter; and with no argument.
const NON_ZEROS: &str = "type User {\n  id: ID\n}\n\
                         type Good {\n  a: NonZero<u32>\n  b: NonZero<i8>\n  c: NonZeroU16\n}\n\
                         type Bad {\n  float: NonZero<Float>\n  text: NonZero<string>\n  \
                         zero: NonZero<u0>\n  list: NonZero<[u8]>\n  user: NonZero<User>\n  \
                         sure: NonZero<u32!>\n}\n\
                         type Gen<T> {\n  n: NonZero<T>\n}\ntype Bare {\n  n: NonZero\n}\n";

#[test]
fn non_zeros_argument_is_an_integer_primitive() {
  // `u0` is a width beyond `core`'s, which a provider gives.
  let root = package("app", Layout::Flat, &[("schema.graphqlx", NON_ZEROS)])
    .with_integers(IntegerProvider::numo());
  let analysis = analyze(&root, &[], &Limits::new());
  let schema = schema_file(&analysis);
  let model = analysis.model();
  // At the occurrence, with a label at the argument as written, its `!` included.
  let refused = |argument: &str| -> (&'static str, Location, Option<Location>) {
    let written = format!("NonZero<{argument}>");
    let start = NON_ZEROS
      .find(written.as_str())
      .unwrap_or_else(|| panic!("`{written}` is not written"));
    let inner = start + "NonZero<".len();
    (
      NONZERO,
      Location::new(schema.get(), SimpleSpan::new(start, start + written.len())),
      Some(Location::new(
        schema.get(),
        SimpleSpan::new(inner, inner + argument.len()),
      )),
    )
  };
  assert_eq!(
    found(&analysis),
    [
      refused("Float"),
      refused("string"),
      refused("u0"),
      refused("[u8]"),
      refused("User"),
      refused("u32!"),
      refused("T"),
      (
        ARITY,
        named(schema, NON_ZEROS, "n: ", "NonZero", 1),
        Some(model.def(model.builtins().non_zero()).location())
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(messages(&analysis)[..7], [NONZERO_MESSAGE; 7]);
  let integer = "write `NonZero` of an integer primitive, `iN` or `uN` with `N` at least 1, as \
                 `NonZero<u32>` and its second name `NonZeroU32` do";
  let nullable = "drop the `!` after the type argument: `NonZero<u32>` is never zero, and \
                  `NonZero<u32>!` is never null either";
  let helps: Vec<Option<&str>> = analysis
    .diagnostics()
    .iter()
    .take(7)
    .map(|diagnostic: &Diagnostic| diagnostic.help())
    .collect();
  assert_eq!(
    helps,
    [
      Some(integer),
      Some(integer),
      Some(integer),
      Some(integer),
      Some(integer),
      Some(nullable),
      Some(integer),
    ]
  );
  let labels: Vec<Option<&str>> = analysis
    .diagnostics()
    .iter()
    .take(7)
    .map(|diagnostic: &Diagnostic| diagnostic.label(0).map(|label| label.text()))
    .collect();
  let not_an_integer = Some("no integer primitive at least one bit wide");
  assert_eq!(
    labels,
    [
      not_an_integer,
      not_an_integer,
      not_an_integer,
      not_an_integer,
      not_an_integer,
      Some("written with `!`"),
      not_an_integer,
    ]
  );

  // The root package's `@set` on `NonZero` is the orphan rule's alone, and makes `NonZero` no set:
  // its argument keeps the nullness it is written with, so the alias and the form it spells stay
  // one type, and neither is refused.
  let text = "extend scalar NonZero @set\ntype Query {\n  a: NonZeroU32\n  b: NonZero<u32>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  assert_eq!(
    found(&analysis),
    [(
      ORPHAN,
      named(schema, text, "@", "set", 0),
      Some(model.def(model.builtins().non_zero()).location())
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    field_type(model, "^::app::Query", "a"),
    field_type(model, "^::app::Query", "b")
  );
}

#[test]
fn a_type_parameter_is_named_once() {
  // A second `T`, once; a third and a fourth `A`, each against the first `A`; a directive's; and
  // the names of one definition in another say nothing.
  let text = "type P<T, T> {\n  id: ID\n}\ntype Q<A, B, A, A> {\n  id: ID\n}\n\
              directive @d<X, X> on OBJECT\ntype R<A, B, T> {\n  id: ID\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let first_a = named(schema, text, "Q<", "A", 0);
  assert_eq!(
    found(&analysis),
    [
      (
        DUPLICATE,
        named(schema, text, ", ", "T", 0),
        Some(named(schema, text, "P<", "T", 0))
      ),
      (DUPLICATE, named(schema, text, ", ", "A", 0), Some(first_a)),
      (DUPLICATE, named(schema, text, ", ", "A", 1), Some(first_a)),
      (
        DUPLICATE,
        named(schema, text, ", ", "X", 0),
        Some(named(schema, text, "@d<", "X", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let refused = |param: &str, def: &str| -> String {
    format!(
      "the type parameter `{param}` of `{def}` has the name of an earlier one, and a definition's \
       type parameters have distinct names"
    )
  };
  assert_eq!(
    messages(&analysis),
    [
      refused("T", "P"),
      refused("A", "Q"),
      refused("A", "Q"),
      refused("X", "@d"),
    ]
  );
}

#[test]
fn an_extensions_list_names_each_type_parameter_once() {
  // An extension's own list names its target's type parameters by position: a second `U` is
  // refused, labelled at the first.
  let text = "type P<A, B> {\n  a: A\n}\nextend type P<U, U> {\n  b: U\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DUPLICATE,
      named(schema, text, ", ", "U", 0),
      Some(named(schema, text, "P<", "U", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the type parameter `U` this extension of `P` names has the name of an earlier one of its \
       list, and an extension's list gives its target's type parameters distinct names"
    ]
  );

  // A list of another count is the phase's alone: the rule says nothing beside it.
  let analysis = analyze_one("type P<A, B> {\n  a: A\n}\nextend type P<U, U, V> {\n  b: U\n}\n");
  assert_eq!(codes(&analysis), [ARITY], "{:#?}", analysis.diagnostics());

  // The root package's extension of a built-in, which adds a directive and nothing else, is
  // judged as any other.
  let text = "directive @x on SCALAR\nextend scalar HashMap<K, K> @x\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DUPLICATE,
      named(schema, text, ", ", "K", 0),
      Some(named(schema, text, "HashMap<", "K", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // An extension a dependency writes of its own type, in a file of its own.
  let theirs = "extend type P<U, U> {\n  b: U\n}\n";
  let geo = package(
    "geo",
    Layout::Flat,
    &[
      ("a.graphqlx", "type P<A, B> {\n  a: A\n}\n"),
      ("b.graphqlx", theirs),
    ],
  );
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "type Query {\n  ok: Int\n}\n")],
  )
  .with_dependencies(["geo"]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let extension = file(&analysis, 4, "b.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      DUPLICATE,
      named(extension, theirs, ", ", "U", 0),
      Some(named(extension, theirs, "P<", "U", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_rule_says_nothing_where_a_phase_spoke() {
  // Each refusal beside a phase's error in the same definition: the phase's diagnostic alone.
  for (text, expected) in [
    (
      "interface M @marker\ntype F {\n  m: M\n  x: Missing\n}\n",
      UNDEFINED,
    ),
    ("interface Mk @marker {\n  a: Missing\n}\n", UNDEFINED),
    (
      "interface Node {\n  id: ID\n}\nscalar S implements Node @missing\n",
      "smear::schema::undefined-directive",
    ),
    (
      "scalar S0 @set @missing\n",
      "smear::schema::undefined-directive",
    ),
    ("type F {\n  f: TypeRef\n  x: Missing\n}\n", UNDEFINED),
    (
      "type F {\n  n: NonZero<Float>\n  x: Missing\n}\n",
      UNDEFINED,
    ),
    ("type F<T, T> {\n  x: Missing\n}\n", UNDEFINED),
    // A written root, which no contribution holds, beside a phase's error in the schema's own
    // contribution, and in another of the schema's contributions.
    (
      "interface M @marker\nschema {\n  query: M\n  mutation: Missing\n}\n",
      UNDEFINED_ROOT,
    ),
    (
      "interface M @marker\nschema {\n  query: M\n}\nextend schema {\n  mutation: Missing\n}\n",
      UNDEFINED_ROOT,
    ),
  ] {
    let analysis = analyze_one(text);
    assert_eq!(codes(&analysis), [expected], "{text}");
  }
  // Beside one in a sibling definition: both.
  for (text, rule) in [
    (
      "interface M @marker\ntype F {\n  m: M\n}\ntype G {\n  x: Missing\n}\n",
      MARKER,
    ),
    (
      "interface Mk @marker {\n  a: Int\n}\ntype G {\n  x: Missing\n}\n",
      MARKER,
    ),
    (
      "interface Node {\n  id: ID\n}\nscalar S implements Node\ntype G {\n  x: Missing\n}\n",
      MARKER,
    ),
    ("scalar S0 @set\ntype G {\n  x: Missing\n}\n", COLLECTION),
    (
      "type F {\n  f: TypeRef\n}\ntype G {\n  x: Missing\n}\n",
      TYPEREF,
    ),
    (
      "type F {\n  n: NonZero<Float>\n}\ntype G {\n  x: Missing\n}\n",
      NONZERO,
    ),
    (
      "type F<T, T> {\n  id: ID\n}\ntype G {\n  x: Missing\n}\n",
      DUPLICATE,
    ),
    // A written root beside a phase's error in a definition, which is none of the schema's
    // contributions.
    (
      "interface M @marker\nschema {\n  query: M\n}\ntype G {\n  x: Missing\n}\n",
      MARKER,
    ),
  ] {
    let analysis = analyze_one(text);
    assert_eq!(codes(&analysis), [rule, UNDEFINED], "{text}");
  }
}

#[test]
fn a_wide_root_beside_many_schema_extensions_is_judged() {
  // A root of many markers, beside many extensions of the schema that hold no error: each marker
  // is reported. With one extension more that holds an error, the root says nothing but the
  // phase's diagnostic. That each occurrence of the root costs one look, and not one for each of
  // the schema's contributions, holds by construction, and is checked by reading: a test sees the
  // diagnostics, not the work.
  const WIDE: usize = 64;
  let params: Vec<String> = (0..WIDE).map(|index: usize| format!("A{index}")).collect();
  let mut text = format!(
    "interface M @marker\ndirective @d repeatable on SCHEMA\ntype Root<{}> {{\n  id: ID\n}}\n\
     schema {{\n  query: Root<{}>\n}}\n",
    params.join(", "),
    ["M"; WIDE].join(", ")
  );
  for _ in 0..WIDE {
    text.push_str("extend schema @d\n");
  }
  let analysis = analyze_text(&text);
  assert_eq!(codes(&analysis), [MARKER; WIDE]);

  text.push_str("extend schema {\n  mutation: Missing\n}\n");
  let analysis = analyze_text(&text);
  assert_eq!(codes(&analysis), [UNDEFINED_ROOT]);
}

#[test]
fn long_names_in_the_rules_are_quoted_as_excerpts() {
  let quoted =
    |letter: &str| -> String { format!("{}…{}", letter.repeat(64), letter.repeat(64)) };
  // A marker named with four kilobytes, named where it may not be.
  let name = "M".repeat(4096);
  let analysis = analyze_text(&format!(
    "interface {name} @marker\ntype Query {{\n  m: {name}\n}}\n"
  ));
  assert_eq!(
    messages(&analysis),
    [format!(
      "the marker `{}` is named here, and a marker is named only in an `implements` list or as a \
       bound's interface",
      quoted("M")
    )]
  );

  // A set named with four kilobytes, of no type parameter.
  let name = "S".repeat(4096);
  let analysis = analyze_text(&format!("scalar {name} @set\n"));
  assert_eq!(
    messages(&analysis),
    [format!(
      "the scalar `{}` applies `@set`, which makes a scalar of 1 type parameter a set, and it \
       declares 0",
      quoted("S")
    )]
  );

  // `NonZero` of a scalar named with four kilobytes: the message names no type.
  let name = "N".repeat(4096);
  let analysis = analyze_text(&format!(
    "scalar {name}\ntype Query {{\n  n: NonZero<{name}>\n}}\n"
  ));
  assert_eq!(messages(&analysis), [NONZERO_MESSAGE]);

  // A definition named with four kilobytes, of two type parameters each named with four kilobytes
  // too.
  let name = "D".repeat(4096);
  let param = "P".repeat(4096);
  let analysis = analyze_text(&format!("scalar {name}<{param}, {param}>\n"));
  assert_eq!(
    messages(&analysis),
    [format!(
      "the type parameter `{}` of `{}` has the name of an earlier one, and a definition's type \
       parameters have distinct names",
      quoted("P"),
      quoted("D")
    )]
  );
}
