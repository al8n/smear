//! The GraphQLx analyzer's implementation phase: what each definition says it is with its
//! `implements` entries, and a union with its members, read of what its own package writes. No
//! two entries of a definition, nor two members of a union, are one type; a definition implements
//! what the interfaces it implements implement, compared as types under the type arguments its
//! entries give them; and an object or an interface declares their fields, of subtypes of their
//! types, with their arguments of exactly their types. All within a budget of its own.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Diagnostic, Layout, Limits, Package, PackageId, SourceFile, SourceId, Tier, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// GraphQL's code for an `implements` entry of a type an earlier entry has.
const DUPLICATE: &str = "smear::schema::duplicate-implements-interface";
/// GraphQL's code for a union member of a type an earlier member has.
const MEMBER: &str = "smear::schema::duplicate-union-member";
/// The resolver's code for an `implements` entry that names nothing.
const UNDEFINED_ENTRY: &str = "smear::schema::undefined-implements-interface";
/// The orphan rule's code.
const ORPHAN: &str = "smear::graphqlx::orphan-extension";
/// GraphQL's code for an interface a definition's interfaces implement and it does not.
const TRANSITIVE: &str = "smear::schema::missing-transitive-interface";
/// A bound of the limits passed.
const LIMIT: &str = "smear::graphqlx::resource-limit";
/// GraphQL's code for an interface's field a definition does not declare.
const MISSING_FIELD: &str = "smear::schema::missing-interface-field";
/// GraphQL's code for a field whose type is no subtype of the interface's.
const FIELD_TYPE: &str = "smear::schema::invalid-interface-field-type";
/// GraphQL's code for a deprecated field implementing one that is not.
const NOT_DEPRECATED: &str = "smear::schema::interface-field-not-deprecated";
/// GraphQL's code for an interface field's argument a field does not take.
const MISSING_ARGUMENT: &str = "smear::schema::missing-interface-field-argument";
/// GraphQL's code for an argument not of exactly the interface's type.
const ARGUMENT_TYPE: &str = "smear::schema::invalid-interface-field-argument-type";
/// GraphQL's code for a required argument the interface's field does not declare.
const UNEXPECTED: &str = "smear::schema::unexpected-required-argument";
/// The resolver's code for a type name that names nothing.
const UNDEFINED_TYPE: &str = "smear::schema::undefined-type";
/// L3's code for a marker misused.
const MARKER_MISUSE: &str = "smear::graphqlx::marker-misuse";

/// The codes this phase reports. A program another rule refuses is asserted on these alone.
const PHASE: [&str; 10] = [
  DUPLICATE,
  MEMBER,
  TRANSITIVE,
  MISSING_FIELD,
  FIELD_TYPE,
  NOT_DEPRECATED,
  MISSING_ARGUMENT,
  ARGUMENT_TYPE,
  UNEXPECTED,
  LIMIT,
];

/// What labels a missing interface's report at the entry that implements it.
const IMPLEMENTED_HERE: &str = "implemented here, by an interface it implements";

/// A package named `name`, `Flat`, at tier `Std`, holding `files`, `(path, text)`.
fn package<'a>(name: &'a str, files: &[(&'a str, &'a str)]) -> Package<&'a str> {
  Package::new(name, Layout::Flat, Tier::Std).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'a str, &'a str)| SourceFile::new(path, text)),
  )
}

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`.
fn analyze_text(text: &str, limits: &Limits) -> Analysis {
  analyze(&package("app", &[("schema.graphqlx", text)]), &[], limits)
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

/// Where the `nth` occurrence, counting from 0, of the name `name` written right after `before` is
/// in `text`, the file `source`: a name that a longer name begins with is not one.
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

/// Each diagnostic's code and primary location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect()
}

/// The rows of `found` whose code is one of `codes`, in the list's order.
fn found_of(analysis: &Analysis, codes: &[&str]) -> Vec<(&'static str, Location)> {
  found(analysis)
    .into_iter()
    .filter(|row: &(&'static str, Location)| codes.contains(&row.0))
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

/// Asserts that `analysis` says nothing.
fn quiet(analysis: &Analysis) {
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// The diagnostic with the code `code` at `at`.
fn the<'a>(analysis: &'a Analysis, code: &str, at: Location) -> &'a Diagnostic {
  analysis
    .diagnostics()
    .iter()
    .find(|diagnostic: &&Diagnostic| {
      diagnostic.code().as_str() == code && diagnostic.primary() == at
    })
    .unwrap_or_else(|| panic!("no `{code}` at {at:?}: {:#?}", analysis.diagnostics()))
}

/// Each label of `diagnostic`, where it is and what it says, in order.
fn labels(diagnostic: &Diagnostic) -> Vec<(Location, &str)> {
  (0..diagnostic.labels())
    .filter_map(|index: usize| diagnostic.label(index))
    .map(|label| (label.location(), label.text()))
    .collect()
}

/// GraphQL's own document: the second `I` is refused, labelled at the first, and nothing else is
/// said. Refuses a report at the first.
#[test]
fn an_interface_implemented_twice_is_refused_at_the_later_entry() {
  let text = "type Query { ok: Int } interface I { a: Int } type T implements I & I { a: Int }";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let second = named(schema, text, "& ", "I", 0);
  assert_eq!(found(&analysis), [(DUPLICATE, second)]);
  let report = the(&analysis, DUPLICATE, second);
  assert_eq!(report.to_string(), "`T` implements `I` twice");
  assert_eq!(
    labels(report),
    [(
      named(schema, text, "implements ", "I", 0),
      "first implemented here"
    )]
  );
}

/// Every later entry of one type is reported, each labelled at the first: `I & I & I` is two. An
/// entry the definition's own package writes in an extension, in another file, is the
/// definition's own, and reported there. Refuses a report at the second entry alone.
#[test]
fn every_later_entry_of_one_type_is_a_duplicate() {
  let text = "type Query {\n  x: Int\n}\ninterface I {\n  a: Int\n}\n\
              type T implements I & I & I {\n  a: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let first = named(schema, text, "implements ", "I", 0);
  let later = [
    named(schema, text, "& ", "I", 0),
    named(schema, text, "& ", "I", 1),
  ];
  assert_eq!(
    found(&analysis),
    [(DUPLICATE, later[0]), (DUPLICATE, later[1])]
  );
  for at in later {
    assert_eq!(
      labels(the(&analysis, DUPLICATE, at)),
      [(first, "first implemented here")]
    );
  }

  let a =
    "type Query {\n  x: Int\n}\ninterface I {\n  a: Int\n}\ntype T implements I {\n  a: Int\n}\n";
  let b = "extend type T implements I\n";
  let root = package("app", &[("a.graphqlx", a), ("b.graphqlx", b)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let (a_file, b_file) = (
    file(&analysis, 3, "a.graphqlx"),
    file(&analysis, 3, "b.graphqlx"),
  );
  let extended = named(b_file, b, "implements ", "I", 0);
  assert_eq!(found(&analysis), [(DUPLICATE, extended)]);
  assert_eq!(
    labels(the(&analysis, DUPLICATE, extended)),
    [(
      named(a_file, a, "implements ", "I", 0),
      "first implemented here"
    )]
  );
}

/// Two instantiations of one interface are two types. Refuses duplicates told by definition.
#[test]
fn two_instantiations_of_one_interface_are_two_types() {
  quiet(&analyze_text(
    "type Query {\n  x: Int\n}\ninterface Cmp<X> {\n  id: ID\n}\n\
     type T implements Cmp<Int> & Cmp<String> {\n  id: ID\n}\n",
    &Limits::new(),
  ));
}

/// GraphQL's own document: the second `A` is refused, labelled at the first; and so is a member
/// the union's own extension writes again.
#[test]
fn a_union_member_written_twice_is_refused_at_the_later() {
  let text = "type Query { ok: Int } type A { a: Int } union U = A | A";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let second = named(schema, text, "| ", "A", 0);
  assert_eq!(found(&analysis), [(MEMBER, second)]);
  let report = the(&analysis, MEMBER, second);
  assert_eq!(
    report.to_string(),
    "the union `U` has `A` as a member twice"
  );
  assert_eq!(
    labels(report),
    [(named(schema, text, "= ", "A", 0), "first a member here")]
  );

  let text = "type Query {\n  ok: Int\n}\ntype A {\n  a: Int\n}\nunion U = A\nextend union U = A\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let extended = named(schema, text, "U = ", "A", 1);
  assert_eq!(found(&analysis), [(MEMBER, extended)]);
  assert_eq!(
    labels(the(&analysis, MEMBER, extended)),
    [(named(schema, text, "U = ", "A", 0), "first a member here")]
  );
}

/// An entry that names nothing is the resolver's alone: compared with nothing, `Missing & Missing`
/// is two `undefined-implements-interface` and no duplicate.
#[test]
fn unresolved_entries_are_no_duplicates() {
  let text = "type Query {\n  x: Int\n}\ntype T implements Missing & Missing {\n  a: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        UNDEFINED_ENTRY,
        named(schema, text, "implements ", "Missing", 0)
      ),
      (UNDEFINED_ENTRY, named(schema, text, "& ", "Missing", 0)),
    ]
  );
}

/// What another package writes into a definition is the orphan rule's: the root package's
/// extension that adds `^::dep::I` to the dependency's `P`, which implements `I` already, is
/// `orphan-extension` alone, and no duplicate. Refuses an orphan's entry read as the definition's.
#[test]
fn what_another_package_writes_is_not_read() {
  let theirs = "interface I {\n  a: Int\n}\ntype P implements I {\n  a: Int\n}\n";
  let ours = "type Query {\n  x: Int\n}\nextend type ^::dep::P implements ^::dep::I\n";
  let root = package("app", &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  assert_eq!(codes(&analysis), [ORPHAN]);
}

/// L3's gate example: `Lazy` implements `Ord<Lazy>`, and `Ord` implements `Comparable<X>`, so
/// `Lazy` must implement `Comparable<Lazy>`, which it omits. One report, at `Lazy`'s `Ord<Lazy>`,
/// labelled at `Ord`'s `Comparable<X>`, and nothing else: the derived `Sorted<Lazy>`'s bound is not
/// judged, and the definition that would hold it is refused instead. Refuses the interim, which
/// said nothing; this test closes the premise satisfaction by declaration rests on.
#[test]
fn the_gate_example_is_refused_at_the_type_that_omits_an_interface() {
  let text = "interface Comparable<X> {\n  compare(other: X): Int\n}\n\
              interface Ord<X> implements Comparable<X> {\n  compare(other: X): Int\n}\n\
              type Lazy implements Ord<Lazy> {\n  compare(other: Lazy): Int\n}\n\
              type Sorted<T> where T: Comparable<T> {\n  items: [T]\n}\n\
              type Wrap<U, S = Sorted<U>> where U: Ord<U> {\n  s: S\n}\n\
              type Query {\n  w: Wrap<Lazy>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let entry = at(schema, text, "Ord<Lazy>", 0);
  assert_eq!(found(&analysis), [(TRANSITIVE, entry)]);
  let report = the(&analysis, TRANSITIVE, entry);
  assert_eq!(
    report.to_string(),
    "`Lazy` implements `Ord`, which implements `Comparable`, so `Lazy` must implement \
     `Comparable` too"
  );
  assert_eq!(
    labels(report),
    [(at(schema, text, "Comparable<X>", 1), IMPLEMENTED_HERE)]
  );
}

/// GraphQL's own document: one report, at `T`'s `B`, labelled at `B`'s `A`.
#[test]
fn graphqls_transitive_fixture_is_refused_once() {
  let text = "type Query { ok: Int }
     interface A { a: Int }
     interface B implements A { a: Int }
     type T implements B { a: Int }";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let entry = named(schema, text, "implements ", "B", 0);
  assert_eq!(found(&analysis), [(TRANSITIVE, entry)]);
  assert_eq!(
    labels(the(&analysis, TRANSITIVE, entry)),
    [(named(schema, text, "implements ", "A", 0), IMPLEMENTED_HERE)]
  );
}

/// What a missing interface implements is owed too: with `D <- C <- B <- A`, each implementing its
/// parent alone, `B` omits `D`, and `A` omits `C` and, through it, `D`: three reports, as
/// `smear-schema` gives. Refuses one hop, which gives two.
#[test]
fn a_missing_interface_is_followed_to_what_it_implements() {
  let text = "type Query {\n  x: Int\n}\ninterface D {\n  id: ID\n}\n\
              interface C implements D {\n  id: ID\n}\ninterface B implements C {\n  id: ID\n}\n\
              type A implements B {\n  id: ID\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let b_entry = named(schema, text, "implements ", "C", 0);
  let a_entry = named(schema, text, "implements ", "B", 0);
  assert_eq!(
    found(&analysis),
    [
      (TRANSITIVE, b_entry),
      (TRANSITIVE, a_entry),
      (TRANSITIVE, a_entry)
    ]
  );
  let c_entry_d = named(schema, text, "implements ", "D", 0);
  let firsts: Vec<(String, Option<Location>)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.to_string(),
        diagnostic.label(0).map(|label| label.location()),
      )
    })
    .collect();
  assert_eq!(
    firsts,
    [
      (
        String::from("`B` implements `C`, which implements `D`, so `B` must implement `D` too"),
        Some(c_entry_d)
      ),
      (
        String::from("`A` implements `B`, which implements `C`, so `A` must implement `C` too"),
        Some(b_entry)
      ),
      (
        String::from("`A` implements `B`, which implements `D`, so `A` must implement `D` too"),
        Some(c_entry_d)
      ),
    ]
  );
}

/// An interface's own entries are read under the root's type arguments: `Box<T>` writes both
/// `Ord<Box<T>>` and the `Comparable<Box<T>>` it implements, and says nothing; `Bad<T>` writes
/// `Comparable<T>`, which is not the `Comparable<Bad<T>>` its `Ord<Bad<T>>` implements, and is
/// refused at that entry. `Bad`'s `other` is also refused once the field's arguments are compared,
/// since one type cannot be both `Bad<T>` and `T`: only this phase's transitive code is asserted.
/// Refuses a check by definition, which accepts `Bad`.
#[test]
fn a_substituted_interface_needs_its_substituted_supertype() {
  let text = "type Query {\n  x: Int\n}\ninterface Comparable<X> {\n  compare(other: X): Int\n}\n\
              interface Ord<X> implements Comparable<X> {\n  compare(other: X): Int\n}\n\
              type Box<T> implements Ord<Box<T>> & Comparable<Box<T>> {\n  \
              compare(other: Box<T>): Int\n}\n\
              type Bad<T> implements Ord<Bad<T>> & Comparable<T> {\n  \
              compare(other: Bad<T>): Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found_of(&analysis, &[TRANSITIVE]),
    [(TRANSITIVE, at(schema, text, "Ord<Bad<T>>", 0))]
  );
}

/// Every kind that may write `implements` is checked, markers alone on a scalar, an enum, an input
/// object and a union: each that writes `M2` and not `M1`, which `M2` implements, is refused, five
/// in all; each that writes both says nothing. Refuses a check of objects and interfaces alone.
#[test]
fn every_kind_that_writes_implements_is_checked() {
  let kinds = |entries: &str| -> String {
    format!(
      "type Query {{\n  x: Int\n}}\ninterface M1 @marker\ninterface M2 implements M1 @marker\n\
       scalar S implements {entries}\nenum E implements {entries} {{\n  A\n}}\n\
       input In implements {entries} {{\n  a: Int\n}}\nunion U implements {entries} = O\n\
       type O implements {entries} {{\n  a: Int\n}}\n"
    )
  };
  let text = kinds("M2");
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  let entries: Vec<(&str, Location)> = (0..5)
    .map(|nth: usize| (TRANSITIVE, named(schema, &text, "implements ", "M2", nth)))
    .collect();
  assert_eq!(found(&analysis), entries);
  let implemented = named(schema, &text, "implements ", "M1", 0);
  for diagnostic in analysis.diagnostics() {
    assert_eq!(labels(diagnostic), [(implemented, IMPLEMENTED_HERE)]);
  }
  quiet(&analyze_text(&kinds("M2 & M1"), &Limits::new()));
}

/// What another rule refuses is passed by: an object implementing an object; a scalar implementing
/// an interface that is no marker, which implements another; an interface implementing itself; a
/// ring of two interfaces; and an object implementing its type parameter. No code of this phase
/// in any. Refuses a second diagnostic for a rule's mistake.
#[test]
fn what_a_rule_refuses_is_passed_by() {
  for text in [
    "type Query {\n  x: Int\n}\ntype X {\n  a: Int\n}\ntype Y implements X {\n  a: Int\n}\n",
    "type Query {\n  x: Int\n}\ninterface K {\n  a: Int\n}\ninterface J implements K {\n  a: Int\n}\n\
     scalar S implements J\n",
    "type Query { ok: Int } interface I implements I { a: Int }",
    "type Query {\n  x: Int\n}\ninterface A implements B {\n  a: Int\n}\n\
     interface B implements A {\n  a: Int\n}\n",
    "type Query {\n  x: Int\n}\ntype O<T> implements T {\n  a: Int\n}\n",
  ] {
    let analysis = analyze_text(text, &Limits::new());
    assert!(
      found_of(&analysis, &PHASE).is_empty(),
      "{text}: {:#?}",
      analysis.diagnostics()
    );
  }
}

/// A ring of three interfaces: each omits the one its interface implements, and is refused at its
/// own entry, three reports, as `smear-schema` gives; what each reaches back is itself, and passed
/// by.
#[test]
fn a_ring_of_three_reports_what_each_omits() {
  let text = "type Query {\n  x: Int\n}\ninterface A implements B {\n  a: Int\n}\n\
              interface B implements C {\n  a: Int\n}\ninterface C implements A {\n  a: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let (a_b, b_c, c_a) = (
    named(schema, text, "implements ", "B", 0),
    named(schema, text, "implements ", "C", 0),
    named(schema, text, "implements ", "A", 0),
  );
  assert_eq!(
    found_of(&analysis, &PHASE),
    [(TRANSITIVE, a_b), (TRANSITIVE, b_c), (TRANSITIVE, c_a)]
  );
  assert_eq!(
    labels(the(&analysis, TRANSITIVE, a_b)),
    [(b_c, IMPLEMENTED_HERE)]
  );
  assert_eq!(
    labels(the(&analysis, TRANSITIVE, b_c)),
    [(c_a, IMPLEMENTED_HERE)]
  );
  assert_eq!(
    labels(the(&analysis, TRANSITIVE, c_a)),
    [(a_b, IMPLEMENTED_HERE)]
  );
}

/// A ring of definitions that grows its arguments ends at once: `T`'s `A<Int>` omits
/// `B<Box<Int>>`, which `A` implements, and `A<Box<Box<Int>>>`, which `B` implements under it, two
/// reports at `T`'s entry; `A`'s definition was read already, and is not read again. `A` and `B`
/// each meet themselves, which is the self-implementing rule's. Refuses an expansion by type,
/// which grows until the depth limit.
#[test]
fn a_growing_ring_ends() {
  let text = "type Query {\n  x: Int\n}\nscalar Box<Z>\n\
              interface A<X> implements B<Box<X>> {\n  a: Int\n}\n\
              interface B<Y> implements A<Box<Y>> {\n  a: Int\n}\n\
              type T implements A<Int> {\n  a: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let entry = at(schema, text, "A<Int>", 0);
  assert_eq!(found(&analysis), [(TRANSITIVE, entry), (TRANSITIVE, entry)]);
  let mut implemented: Vec<Location> = analysis
    .diagnostics()
    .iter()
    .filter_map(|diagnostic: &Diagnostic| diagnostic.label(0).map(|label| label.location()))
    .collect();
  implemented
    .sort_by_key(|location: &Location| location.span().map(|span: SimpleSpan| span.start()));
  assert_eq!(
    implemented,
    [
      at(schema, text, "B<Box<X>>", 0),
      at(schema, text, "A<Box<Y>>", 0)
    ]
  );
}

/// Putting an entry's type arguments in place of an interface's parameters draws on the
/// implementation budget alone: under no substitution step at all, `Item`'s `Ord<Item>` is
/// rebuilt into `Cmp<Item>`, which `Item` omits, and nothing is a limit. Refuses the rebuild
/// charged to `Limits::max_substitutions`.
#[test]
fn a_substitution_draws_on_the_implementation_budget_alone() {
  let text = "type Query {\n  x: Int\n}\ninterface Cmp<X> @marker\n\
              interface Ord<X> implements Cmp<X> @marker\ntype Item implements Ord<Item> {\n  x: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_substitutions(0));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(TRANSITIVE, at(schema, text, "Ord<Item>", 0))]
  );
}

/// A complete chain of 16 interfaces, each implementing every one before it, beside eight objects
/// that each implement all 16 and a query root: markers, or each with the one field `id: ID`, which
/// every object declares too.
fn chain(markers: bool) -> String {
  let mut text = String::from("type Query {\n  x: Int\n}\n");
  let body = if markers {
    " @marker\n"
  } else {
    " {\n  id: ID\n}\n"
  };
  for j in 1..=16 {
    let before: Vec<String> = (1..j).map(|i: usize| format!("I{i}")).collect();
    if before.is_empty() {
      text.push_str(&format!("interface I{j}{body}"));
    } else {
      text.push_str(&format!(
        "interface I{j} implements {}{body}",
        before.join(" & ")
      ));
    }
  }
  let all: Vec<String> = (1..=16).map(|i: usize| format!("I{i}")).collect();
  let field = if markers { "x: Int" } else { "id: ID" };
  for k in 1..=8 {
    text.push_str(&format!(
      "type T{k} implements {} {{\n  {field}\n}}\n",
      all.join(" & ")
    ));
  }
  text
}

/// Checking that `implements` is transitively closed costs the square of a chain, and the budget
/// pays for every look. 16 markers in a complete chain, each looking at the entries of each one it
/// implements: `I(j)` costs the sum of `i - 1` over its `j - 1` roots, 560 in all; and each of the
/// eight objects 120: 1,520. Nothing is said under 1,520; under 1,519 `T8`'s last root, `I16`, is
/// `resource-limit`, naming the limit; and nothing under the default. Refuses a closure computed
/// for each object, and a look unpaid.
#[test]
fn the_transitive_check_costs_the_square_of_a_chain() {
  let text = chain(true);
  quiet(&analyze_text(
    &text,
    &Limits::new().with_max_implementation_steps(1520),
  ));
  let short = analyze_text(&text, &Limits::new().with_max_implementation_steps(1519));
  let schema = schema_file(&short);
  let last = named(schema, &text, "& ", "I16", 7);
  assert_eq!(found(&short), [(LIMIT, last)]);
  let message = the(&short, LIMIT, last).to_string();
  assert!(
    message.contains("`Limits::max_implementation_steps`, 1519"),
    "{message}"
  );
  quiet(&analyze_text(&text, &Limits::new()));
}

/// An entry written twice is one root, checked once: `M2 & M1 & M2` looks at `M2`'s one entry, a
/// step, so one step pays for it and the written duplicate is all that is said; with none, the
/// first `M2` is `resource-limit` beside the duplicate. Refuses a root per entry, which needs two.
#[test]
fn an_entry_written_twice_is_checked_once() {
  let text = "type Query {\n  x: Int\n}\ninterface M1 @marker\ninterface M2 implements M1 @marker\n\
              type T implements M2 & M1 & M2 {\n  x: Int\n}\n";
  let one = analyze_text(text, &Limits::new().with_max_implementation_steps(1));
  let schema = schema_file(&one);
  let later = named(schema, text, "& ", "M2", 0);
  assert_eq!(found(&one), [(DUPLICATE, later)]);
  let none = analyze_text(text, &Limits::new().with_max_implementation_steps(0));
  assert_eq!(
    found(&none),
    [
      (LIMIT, named(schema, text, "implements ", "M2", 0)),
      (DUPLICATE, later)
    ]
  );
}

/// A definition's own entries and members are written, and comparing them costs nothing: under no
/// step at all, the two written duplicates are all that is said.
#[test]
fn duplicates_as_written_cost_nothing() {
  let text = "type Query {\n  x: Int\n}\ninterface M @marker\ntype T implements M & M {\n  x: Int\n}\n\
              type A {\n  a: Int\n}\nunion U = A | A\n";
  let analysis = analyze_text(text, &Limits::new().with_max_implementation_steps(0));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (DUPLICATE, named(schema, text, "& ", "M", 0)),
      (MEMBER, named(schema, text, "| ", "A", 0))
    ]
  );
}

/// GraphQL's own documents: a field the type omits, at its `I`, labelled at the interface's `b`; a
/// field of another type, at the type's field; and a deprecated field implementing one that is not,
/// labelled at the interface's. One code each.
#[test]
fn graphqls_field_fixtures_are_refused_once() {
  let text = "type Query { ok: Int } interface I { a: Int b: Int } type T implements I { a: Int }";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let entry = named(schema, text, "implements ", "I", 0);
  assert_eq!(found(&analysis), [(MISSING_FIELD, entry)]);
  let report = the(&analysis, MISSING_FIELD, entry);
  assert_eq!(
    report.to_string(),
    "`T` implements `I`, which declares the field `b`, and `T` declares no field `b`"
  );
  assert_eq!(
    labels(report),
    [(named(schema, text, "Int ", "b", 0), "the interface's field")]
  );

  let text = "type Query { ok: Int } interface I { a: Int } type T implements I { a: String }";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let field = named(schema, text, "{ ", "a", 1);
  assert_eq!(found(&analysis), [(FIELD_TYPE, field)]);
  let report = the(&analysis, FIELD_TYPE, field);
  assert_eq!(
    report.to_string(),
    "the type of `T`'s field `a` is no subtype of the type `I` declares for it"
  );
  assert_eq!(
    labels(report),
    [
      (named(schema, text, "a: ", "String", 0), "this type"),
      (
        named(schema, text, "{ a: ", "Int", 0),
        "the type the interface declares"
      ),
    ]
  );

  let text =
    "type Query { ok: Int } interface I { a: Int } type T implements I { a: Int @deprecated }";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let field = named(schema, text, "{ ", "a", 1);
  assert_eq!(found(&analysis), [(NOT_DEPRECATED, field)]);
  assert_eq!(
    labels(the(&analysis, NOT_DEPRECATED, field)),
    [(
      named(schema, text, "{ ", "a", 0),
      "the interface's field, not deprecated"
    )]
  );
}

/// A field is found by its name, whatever the order, in the definition's own extension too; a field
/// written twice is found as the first of its name, which is the one compared: `V`'s fits and `W`'s
/// does not. Refuses fields paired by position, and the last of a name compared.
#[test]
fn a_field_is_found_by_its_name() {
  let text = "type Query {\n  x: Int\n}\ninterface I {\n  a: Int\n  b: String\n}\n\
              type T implements I {\n  b: String\n  a: Int\n}\n\
              type U implements I {\n  a: Int\n}\nextend type U {\n  b: String\n}\n\
              type V implements I {\n  a: Int\n  b: String\n  a: String\n}\n\
              type W implements I {\n  a: String\n  b: String\n  a: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found_of(&analysis, &PHASE),
    [(
      FIELD_TYPE,
      named(schema, text, "W implements I {\n  ", "a", 0)
    )]
  );
}

/// A field's type is a subtype of the interface's as GraphQL reads it: `[User!]!` of `[Node]`,
/// `User!` of `Node`, the interface `Named` that implements `Node` of `Node`, and the member `User`
/// of the union `U` hold; `Node` of `Node!`, `[Node]` of `Node`, `Post`, which implements nothing,
/// of `Node`, and `Node` of `U`, which is no member, each do not. Refuses non-null or lists read the
/// wrong way, and a union told by what is no member.
#[test]
fn field_types_are_covariant_as_in_graphql() {
  let text = "type Query {\n  x: Int\n}\ninterface Node {\n  id: ID\n}\n\
              interface Named implements Node {\n  id: ID\n}\n\
              type User implements Node {\n  id: ID\n}\ntype Post {\n  id: ID\n}\n\
              union U = User | Post\n\
              interface I {\n  f1: [Node]\n  f2: Node\n  f3: Node\n  f4: U\n}\n\
              type T implements I {\n  f1: [User!]!\n  f2: User!\n  f3: Named\n  f4: User\n}\n\
              interface J {\n  g1: Node!\n  g2: Node\n  g3: Node\n  g4: U\n}\n\
              type S implements J {\n  g1: Node\n  g2: [Node]\n  g3: Post\n  g4: Node\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let fields: Vec<(&str, Location)> = ["g1", "g2", "g3", "g4"]
    .into_iter()
    .map(|name: &str| (FIELD_TYPE, named(schema, text, "  ", name, 1)))
    .collect();
  assert_eq!(found(&analysis), fields);
}

/// A named type is covariant in its type arguments, through collections too: `Page<User>` of
/// `Page<Node>` and `HashMap<string, User>` of `HashMap<string, Node>` hold; `Page<Int>` of
/// `Page<Node>`, and `Page<User>` of `Page<User!>`, do not. Refuses invariance, and a `!` of an
/// argument read through.
#[test]
fn an_instantiation_is_covariant_in_its_arguments() {
  let text = "type Query {\n  x: Int\n}\ninterface Node {\n  id: ID\n}\n\
              type User implements Node {\n  id: ID\n}\ntype Page<T> {\n  items: [T]\n}\n\
              interface I {\n  a: Page<Node>\n  b: HashMap<string, Node>\n  c: Page<Node>\n  \
              d: Page<User!>\n}\n\
              type T implements I {\n  a: Page<User>\n  b: HashMap<string, User>\n  c: Page<Int>\n  \
              d: Page<User>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (FIELD_TYPE, named(schema, text, "  ", "c", 1)),
      (FIELD_TYPE, named(schema, text, "  ", "d", 1))
    ]
  );
}

/// What a type declares is read under its own arguments: `Box<T>` implements `Container<T>`, so
/// `Box<User>` is a subtype of `Container<Node>` and `Box<Int>` is not. Refuses a type's entries
/// read as written.
#[test]
fn a_declared_interface_is_read_under_the_types_arguments() {
  let text = "type Query {\n  x: Int\n}\ninterface Node {\n  id: ID\n}\n\
              type User implements Node {\n  id: ID\n}\ninterface Container<X> {\n  item: X\n}\n\
              type Box<T> implements Container<T> {\n  item: T\n}\n\
              interface I {\n  a: Container<Node>\n  b: Container<Node>\n}\n\
              type T implements I {\n  a: Box<User>\n  b: Box<Int>\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(FIELD_TYPE, named(schema, text, "  ", "b", 1))]
  );
}

/// A generic union's members are read under its arguments: `User` is a member of
/// `Either<User, Post>`, and `Node` is none. Refuses the members read as written.
#[test]
fn a_generic_unions_members_are_read_under_its_arguments() {
  let text = "type Query {\n  x: Int\n}\ninterface Node {\n  id: ID\n}\n\
              type User implements Node {\n  id: ID\n}\ntype Post {\n  id: ID\n}\n\
              union Either<A, B> = A | B\n\
              interface I {\n  a: Either<User, Post>\n  b: Either<User, Post>\n}\n\
              type T implements I {\n  a: User\n  b: Node\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(FIELD_TYPE, named(schema, text, "  ", "b", 1))]
  );
}

/// A generic definition is judged once, where it is written: a type parameter is a subtype of
/// itself and of its bounds alone. K's `Sorted`, `first: T` for `first: Comparable<T>`, holds on
/// the strength of its bound; `Ordered`'s holds through its bound's interface `Ord`, which
/// implements `Comparable<X>`; `Bare`, with no bound, is refused at `first`; and `Two`'s `x: T` is
/// no subtype of `U`. Refuses a parameter compared with no bound, and one bound step too few.
#[test]
fn a_type_parameter_is_a_subtype_of_itself_and_its_bounds() {
  let text = "type Query {\n  x: Int\n}\ninterface Comparable<X> {\n  compare(other: X): Int\n}\n\
              interface Ord<X> implements Comparable<X> {\n  compare(other: X): Int\n}\n\
              interface HasFirst<X> {\n  first: Comparable<X>\n}\n\
              type Sorted<T> implements HasFirst<T> where T: Comparable<T> {\n  first: T\n}\n\
              type Ordered<T> implements HasFirst<T> where T: Ord<T> {\n  first: T\n}\n\
              type Bare<T> implements HasFirst<T> {\n  first: T\n}\n\
              interface HasX<X> {\n  x: X\n}\ntype Two<T, U> implements HasX<U> {\n  x: T\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found_of(&analysis, &PHASE),
    [
      (
        FIELD_TYPE,
        named(
          schema,
          text,
          "Bare<T> implements HasFirst<T> {\n  ",
          "first",
          0
        )
      ),
      (FIELD_TYPE, named(schema, text, "HasX<U> {\n  ", "x", 0)),
    ]
  );
}

/// An interface's field is read under the entry's arguments: `Node<ID>`'s `id` is `ID`, which
/// `User`'s is, and `Node<String>`'s is `String`, which `S`'s `ID` is not.
#[test]
fn a_member_is_read_under_the_entrys_arguments() {
  let text = "type Query {\n  x: Int\n}\ninterface Node<T> {\n  id: T\n}\n\
              type User implements Node<ID> {\n  id: ID\n}\n\
              type S implements Node<String> {\n  id: ID\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      FIELD_TYPE,
      named(schema, text, "Node<String> {\n  ", "id", 0)
    )]
  );
}

/// Putting the entry's arguments into an interface's field draws on the implementation budget
/// alone: under no substitution step, nothing is said. `User`'s check takes the field, one step;
/// rebuilding `T` under `Node<ID>`, one; and the pair of `ID` and `ID`, one: three. At three
/// nothing is said, and at two the check stops at `Node<ID>`. Refuses the rebuild charged to
/// `Limits::max_substitutions`.
#[test]
fn substituting_a_member_draws_on_the_implementation_budget_alone() {
  let text = "type Query {\n  x: Int\n}\ninterface Node<T> {\n  id: T\n}\n\
              type User implements Node<ID> {\n  id: ID\n}\n";
  quiet(&analyze_text(
    text,
    &Limits::new().with_max_substitutions(0),
  ));
  quiet(&analyze_text(
    text,
    &Limits::new().with_max_implementation_steps(3),
  ));
  let short = analyze_text(text, &Limits::new().with_max_implementation_steps(2));
  let schema = schema_file(&short);
  assert_eq!(found(&short), [(LIMIT, at(schema, text, "Node<ID>", 0))]);
}

/// A pair of types is compared once for the whole phase, and each look at it is a step. `User`'s
/// check takes its field and the pair of `ID` and `ID`, two; `T`'s `a` its field, the pair of
/// `[User]` and `[Node]`, the pair of their elements and `User`'s one entry, four; and `b` its field
/// and a look at the pair answered already, two: eight. At eight nothing is said, and at seven `T`'s
/// check stops at `I`. Refuses a pair answered again, which passes eight.
#[test]
fn a_pair_is_compared_once() {
  let text = "type Query {\n  x: Int\n}\ninterface Node {\n  id: ID\n}\n\
              type User implements Node {\n  id: ID\n}\n\
              interface I {\n  a: [Node]\n  b: [Node]\n}\n\
              type T implements I {\n  a: [User]\n  b: [User]\n}\n";
  quiet(&analyze_text(
    text,
    &Limits::new().with_max_implementation_steps(8),
  ));
  let short = analyze_text(text, &Limits::new().with_max_implementation_steps(7));
  let schema = schema_file(&short);
  assert_eq!(
    found(&short),
    [(LIMIT, named(schema, text, "implements ", "I", 0))]
  );
}

/// An interface's field's type is rebuilt once for each entry type: `User`'s check takes two; `A`'s
/// the field, rebuilding `X` under `Container<User>` and the pair, three; and `B`'s the field and
/// the pair, the rebuild remembered, two: seven. At seven nothing is said, and at six `B`'s check
/// stops at its `Container<User>`. Refuses a rebuild made again for each implementing type.
#[test]
fn a_member_type_is_rebuilt_once_for_each_entry_type() {
  let text = "type Query {\n  x: Int\n}\ninterface Node {\n  id: ID\n}\n\
              type User implements Node {\n  id: ID\n}\ninterface Container<X> {\n  item: X\n}\n\
              type A implements Container<User> {\n  item: User\n}\n\
              type B implements Container<User> {\n  item: User\n}\n";
  quiet(&analyze_text(
    text,
    &Limits::new().with_max_implementation_steps(7),
  ));
  let short = analyze_text(text, &Limits::new().with_max_implementation_steps(6));
  let schema = schema_file(&short);
  assert_eq!(
    found(&short),
    [(LIMIT, at(schema, text, "Container<User>", 1))]
  );
}

/// A valid GraphQL document can meet the budget, which checking implementations needs on accepted
/// programs: 16 interfaces of one field in a complete chain, `I(j)` costing each of its `j - 1`
/// roots' entries and the root's field and pair, `(j - 1)(j + 2) / 2`, 800 in all; and eight objects
/// that implement all 16, 152 each: 2,016. At 2,016 nothing is said, and at 2,015 `T8`'s last root,
/// `I16`, is `resource-limit`. Refuses a field compared for nothing.
#[test]
fn a_valid_graphql_document_can_meet_the_implementation_budget() {
  let text = chain(false);
  quiet(&analyze_text(
    &text,
    &Limits::new().with_max_implementation_steps(2016),
  ));
  let short = analyze_text(&text, &Limits::new().with_max_implementation_steps(2015));
  let schema = schema_file(&short);
  assert_eq!(
    found(&short),
    [(LIMIT, named(schema, &text, "& ", "I16", 7))]
  );
  quiet(&analyze_text(&text, &Limits::new()));
}

/// A type that names nothing is the resolver's alone: `f: Missing` for `f: Node`, and an
/// interface's `g: Gone` for `g: Node`, are each compared with nothing.
#[test]
fn an_unresolved_type_is_judged_by_nothing_here() {
  let text = "type Query {\n  x: Int\n}\ninterface Node {\n  id: ID\n}\n\
              interface I {\n  f: Node\n  g: Gone\n}\n\
              type T implements I {\n  f: Missing\n  g: Node\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (UNDEFINED_TYPE, at(schema, text, "Gone", 0)),
      (UNDEFINED_TYPE, at(schema, text, "Missing", 0))
    ]
  );
}

/// A marker's fields are `marker-misuse`'s, at the marker: a type that implements it declares none
/// of them, and nothing of this phase is said.
#[test]
fn a_marker_has_no_field_to_implement() {
  let text = "type Query {\n  x: Int\n}\ninterface M @marker {\n  a: Int\n}\n\
              type T implements M {\n  x: Int\n}\n";
  assert_eq!(codes(&analyze_text(text, &Limits::new())), [MARKER_MISUSE]);
}

/// Deprecation is told by the directive an application resolved to: the root package's own
/// `@deprecated` replaces `core`'s and keeps its definition, and deprecates; a dependency's own
/// directive named `deprecated` does not. Refuses a name compared.
#[test]
fn deprecation_is_told_by_the_directive() {
  let text = "directive @deprecated(reason: String) on FIELD_DEFINITION\n\
              type Query {\n  x: Int\n}\ninterface I {\n  a: Int\n}\n\
              type T implements I {\n  a: Int @deprecated\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      NOT_DEPRECATED,
      named(schema, text, "T implements I {\n  ", "a", 0)
    )]
  );

  let theirs = "directive @deprecated on FIELD_DEFINITION\n";
  let ours = "type Query {\n  x: Int\n}\ninterface I {\n  a: Int\n}\n\
              type T implements I {\n  a: Int @^::dep::deprecated\n}\n";
  let root = package("app", &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", &[("dep.graphqlx", theirs)]);
  quiet(&analyze(&root, &[dep], &Limits::new()));
}

/// A definition and a field, each named with four kilobytes, are quoted as excerpts: the message
/// stays short.
#[test]
fn long_names_are_quoted_as_excerpts() {
  let long = "N".repeat(4096);
  let text = format!(
    "type Query {{\n  x: Int\n}}\ninterface I{long} {{\n  f{long}: Int\n}}\n\
     type T implements I{long} {{\n  x: Int\n}}\n"
  );
  let analysis = analyze_text(&text, &Limits::new());
  assert_eq!(codes(&analysis), [MISSING_FIELD]);
  for diagnostic in analysis.diagnostics() {
    let message = diagnostic.to_string();
    assert!(message.len() < 1024, "{message}");
    assert!(message.contains('…'), "{message}");
  }
}

/// A pair that fails only because a pair it asks is open, being answered further up, is not
/// remembered as failing: it is judged afresh when asked again. `D` is a subtype of `I<U>` through
/// its `I<Good>`; asked first, `f` meets `W<D>` against `U`, which asks `D` against `I<U>` again
/// while that is open, and fails there, so `g`, asked next, judges `W<D>` against `U` afresh and
/// finds that it holds. Either order says nothing. Refuses a failure remembered under an open
/// assumption, which refuses `g` when `f` is written first.
#[test]
fn a_pair_that_held_by_another_route_is_judged_again() {
  for fields in ["f: I<U>\n  g: U", "g: U\n  f: I<U>"] {
    let text = format!(
      "type Query {{\n  x: Int\n}}\ninterface I<X> {{\n  a: Int\n}}\ntype W<Z> {{\n  a: Int\n}}\n\
       type Good {{\n  a: Int\n}}\nunion U = W<I<U>> | Good\n\
       type D implements I<W<D>> & I<Good> {{\n  a: Int\n}}\n\
       interface J {{\n  {fields}\n}}\ntype T implements J {{\n  f: D\n  g: W<D>\n}}\n"
    );
    quiet(&analyze_text(&text, &Limits::new()));
  }
}

/// GraphQL's own documents: an argument the field does not take, at the field, labelled at the
/// interface's argument; an argument of another type, at the argument; and a required argument the
/// interface's field does not declare, at the argument. One code each.
#[test]
fn graphqls_argument_fixtures_are_refused_once() {
  let text = "type Query { ok: Int } interface I { a(x: Int): Int } type T implements I { a: Int }";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let field = named(schema, text, "{ ", "a", 1);
  assert_eq!(found(&analysis), [(MISSING_ARGUMENT, field)]);
  let report = the(&analysis, MISSING_ARGUMENT, field);
  assert_eq!(
    report.to_string(),
    "`I`'s field `a` takes the argument `x`, and `T`'s field `a` does not"
  );
  assert_eq!(
    labels(report),
    [(named(schema, text, "(", "x", 0), "the interface's argument")]
  );

  let text = "type Query { ok: Int }
     interface I { a(x: Int): Int }
     type T implements I { a(x: String): Int }";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let argument = named(schema, text, "(", "x", 1);
  assert_eq!(found(&analysis), [(ARGUMENT_TYPE, argument)]);
  assert_eq!(
    the(&analysis, ARGUMENT_TYPE, argument).to_string(),
    "the argument `x` of `T`'s field `a` must have exactly the type `I` declares for it"
  );

  let text =
    "type Query { ok: Int } interface I { a: Int } type T implements I { a(x: Int!): Int }";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let argument = named(schema, text, "(", "x", 0);
  assert_eq!(found(&analysis), [(UNEXPECTED, argument)]);
  assert_eq!(
    labels(the(&analysis, UNEXPECTED, argument)),
    [(named(schema, text, "{ ", "a", 0), "the interface's field")]
  );
}

/// An argument's type is the interface's exactly, after substitution: `f(a: User)` for
/// `f(a: Node)` is refused, though `User` implements `Node`; under `I<Int!>`, `f(a: X)` asks
/// `Int!`, which `P` gives and `Q`'s `Int` does not. Refuses covariant arguments, and an argument
/// read as written.
#[test]
fn argument_types_are_exact_after_substitution() {
  let text = "type Query {\n  x: Int\n}\ninterface Node {\n  id: ID\n}\n\
              type User implements Node {\n  id: ID\n}\n\
              interface J {\n  f(a: Node): Int\n}\ntype S implements J {\n  f(a: User): Int\n}\n\
              interface I<X> {\n  f(a: X): Int\n}\n\
              type P implements I<Int!> {\n  f(a: Int!): Int\n}\n\
              type Q implements I<Int!> {\n  f(a: Int): Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let q = named(schema, text, "f(", "a", 4);
  assert_eq!(
    found(&analysis),
    [
      (ARGUMENT_TYPE, named(schema, text, "f(", "a", 1)),
      (ARGUMENT_TYPE, q)
    ]
  );
  assert_eq!(
    labels(the(&analysis, ARGUMENT_TYPE, q)),
    [
      (named(schema, text, "f(a: ", "Int", 1), "this type"),
      (
        named(schema, text, "f(a: ", "X", 0),
        "the type the interface declares"
      ),
      (at(schema, text, "I<Int!>", 1), "under these type arguments"),
    ]
  );
}

/// An extra argument is required when it is written non-null with no default, as GraphQL reads
/// it: `a: Int!` is refused; `a: Int! = 1`, `a: Int` and a bare `a: T` are not, whatever `T`
/// stands for; and `a: Missing!` is, beside the resolver's report of `Missing`.
#[test]
fn an_extra_argument_is_required_by_its_written_bang() {
  let text = "type Query {\n  x: Int\n}\ninterface I {\n  f: Int\n}\n\
              type A implements I {\n  f(a: Int!): Int\n}\n\
              type B implements I {\n  f(a: Int! = 1): Int\n}\n\
              type C implements I {\n  f(a: Int): Int\n}\n\
              type D<T> implements I {\n  f(a: T): Int\n}\n\
              type E implements I {\n  f(a: Missing!): Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (UNEXPECTED, named(schema, text, "f(", "a", 0)),
      (UNEXPECTED, named(schema, text, "f(", "a", 4)),
      (UNDEFINED_TYPE, at(schema, text, "Missing", 0)),
    ]
  );
}

/// Arguments are found by their names, whatever the order; one written twice on the implementing
/// field is found as the first of its name, which is the one compared: `U`'s fits and `V`'s does
/// not. Refuses arguments paired by position, and the last of a name compared.
#[test]
fn arguments_are_found_by_their_names() {
  let text = "type Query {\n  x: Int\n}\ninterface I {\n  f(a: Int, b: String): Int\n}\n\
              type T implements I {\n  f(b: String, a: Int): Int\n}\n\
              type U implements I {\n  f(a: Int, b: String, a: String): Int\n}\n\
              type V implements I {\n  f(a: String, b: String, a: Int): Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found_of(&analysis, &PHASE),
    [(
      ARGUMENT_TYPE,
      named(schema, text, "V implements I {\n  f(", "a", 0)
    )]
  );
}

/// Each argument looked at is a step, on either side: `T`'s check takes the field, one; the pair of
/// `Int` and `Int`, one; the interface's two arguments, two; and `T`'s three, three: seven. At seven
/// nothing is said, and at six the check stops at `T`'s `I`.
#[test]
fn each_argument_is_a_step() {
  let text = "type Query {\n  x: Int\n}\ninterface I {\n  f(a: Int, b: Int): Int\n}\n\
              type T implements I {\n  f(a: Int, b: Int, c: Int): Int\n}\n";
  quiet(&analyze_text(
    text,
    &Limits::new().with_max_implementation_steps(7),
  ));
  let short = analyze_text(text, &Limits::new().with_max_implementation_steps(6));
  let schema = schema_file(&short);
  assert_eq!(
    found(&short),
    [(LIMIT, named(schema, text, "implements ", "I", 0))]
  );
}
