//! The GraphQLx analyzer's rules on what is judged of types: a marker has no field, is named only
//! in `implements` and in bounds, and is what a scalar, an enum, an input object or a union
//! implements; a set has one type parameter and a map two; `TypeRef` stands only in a directive's
//! arguments; and `NonZero`'s argument is an integer primitive. Each rule says nothing within what
//! a phase refused, and quotes a name only as an excerpt.
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

/// The code of a count of type arguments that does not match.
const ARITY: &str = "smear::graphqlx::generic-arity";

/// The code of a type name that names nothing.
const UNDEFINED: &str = "smear::schema::undefined-type";

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
  assert_eq!(
    found(&analysis),
    [
      at_marker("m: ", 0),
      at_marker("[", 0),
      at_marker("a: ", 0),
      at_marker("m: ", 1),
      at_marker("U = ", 0),
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
  // later; an object is not this rule's; and the root package's `@set` on `alloc`'s `string` is the
  // orphan rule's alone.
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

/// `TypeRef` where it may stand, a directive definition's argument, alone and in a list, and an
/// input field; then where it may not: a type argument of a directive's argument, a collection's
/// and another's, a field's type, a field's argument, an application's type argument, a union
/// member and a default.
const TYPE_REFS: &str = "directive @d(of: TypeRef, many: [TypeRef!]) on OBJECT\n\
                         input Binding {\n  type: TypeRef!\n}\nscalar Box<T>\n\
                         directive @t<T> on OBJECT\n\
                         directive @m(map: HashMap<string, TypeRef>) on OBJECT\n\
                         directive @b(boxed: [Box<TypeRef>]) on OBJECT\n\
                         type Field {\n  f: TypeRef\n}\ntype Argument {\n  f(a: TypeRef): Int\n}\n\
                         type Applied @t<TypeRef> {\n  id: ID\n}\nunion U = TypeRef\n\
                         scalar P<T = TypeRef>\n";

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
      refused("f: "),
      refused("a: "),
      refused("@t<"),
      refused("U = "),
      refused("T = "),
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
  ] {
    let analysis = analyze_one(text);
    assert_eq!(codes(&analysis), [rule, UNDEFINED], "{text}");
  }
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
}
