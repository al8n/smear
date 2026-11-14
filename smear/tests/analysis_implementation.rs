//! The GraphQLx analyzer's implementation phase: what each definition says it is with its
//! `implements` entries, and a union with its members, read of what its own package writes. No
//! two entries of a definition, nor two members of a union, are one type; and a definition
//! implements what the interfaces it implements implement, compared as types under the type
//! arguments its entries give them, within a budget of its own.
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

/// The codes this phase reports. A program another rule refuses is asserted on these alone.
const PHASE: [&str; 4] = [DUPLICATE, MEMBER, TRANSITIVE, LIMIT];

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
