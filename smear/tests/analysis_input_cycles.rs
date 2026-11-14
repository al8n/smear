//! The GraphQLx analyzer's rule on input objects that require a value of themselves, GraphQL's own
//! with GraphQL's codes: the draft's unbreakable-cycle algorithm over every input object of every
//! package, a OneOf breaking a cycle only through a field that can be given a finite value, and,
//! for a generic input object, what its type arguments must hold. The rule reports the field that
//! closes a cycle once for its input object, says nothing within what a phase refused, and quotes a
//! name only as an excerpt.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Diagnostic, Layout, Limits, Package, PackageId, SourceFile, SourceId, Tier, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// GraphQL's code for an input object that requires a value of itself through non-null fields.
const NON_NULL: &str = "smear::schema::circular-non-null-input-field";

/// GraphQL's code for an input object that requires a value of itself by way of a OneOf.
const ONE_OF: &str = "smear::schema::circular-one-of-input-field";

/// The code of a type name that names nothing, a phase's.
const UNDEFINED: &str = "smear::schema::undefined-type";

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

/// Each diagnostic's code and primary location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
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

/// The census rows, an input object that requires itself, a ring of three, a figure eight, and a
/// OneOf whose one field leads back.
const GRAPHQL_CYCLES: &str = "type Query {\n  ok: Int\n}\ninput A {\n  b: B!\n}\ninput B {\n  a: A!\n}\n\
                              input Me {\n  me: Me!\n}\ninput R1 {\n  next: R2!\n}\n\
                              input R2 {\n  next: R3!\n}\ninput R3 {\n  next: R1!\n}\n\
                              input E {\n  f: F!\n  g: G!\n}\ninput F {\n  e: E!\n}\n\
                              input G {\n  e: E!\n}\ninput First @oneOf {\n  second: Second\n}\n\
                              input Second {\n  first: First!\n}\n";

#[test]
fn an_input_object_that_requires_itself_is_refused() {
  let text = GRAPHQL_CYCLES;
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let field = |code: &'static str, name: &str, nth: usize| -> (&'static str, Location) {
    (code, named(schema, text, "  ", name, nth))
  };
  // Each cycle once, at the field that closes it, as the walk from the first input object meets
  // it: the figure eight is two cycles, each closed by its own field.
  assert_eq!(
    found(&analysis),
    [
      field(NON_NULL, "a", 0),
      field(NON_NULL, "me", 0),
      field(NON_NULL, "next", 2),
      field(NON_NULL, "e", 0),
      field(NON_NULL, "e", 1),
      field(ONE_OF, "first", 0),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let messages = messages(&analysis);
  assert_eq!(
    messages[0],
    "the input object `B` requires a value of itself through its field `a`: every field on the \
     way is non-null and no list"
  );
  assert_eq!(
    messages[5],
    "the input object `Second` requires a value of itself through its field `first`, by way of a \
     `@oneOf` input object none of whose fields can be given a finite value"
  );
  // GraphQL's phrases and helps, and no label.
  let words: Vec<(Option<&str>, Option<&str>, usize)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.primary_label(),
        diagnostic.help(),
        diagnostic.labels(),
      )
    })
    .collect();
  assert_eq!(
    [words[0], words[5]],
    [
      (
        Some("input object cycle has no nullable or list link"),
        Some(
          "break the cycle with a nullable link or a list; as written, no finite value satisfies \
           it."
        ),
        0
      ),
      (
        Some("input object cycle through a @oneOf input object has no finite value"),
        Some(
          "a `@oneOf` input object is given exactly one field, so it breaks a cycle only through a \
           field that can be given a finite value; give it one, such as a scalar or a list."
        ),
        0
      ),
    ]
  );

  // The draft's own OneOf, a nullable link, and a list link break a cycle; and a field whose type
  // names nothing is the phase's alone.
  let analysis = analyze_one(
    "type Query {\n  ok: Int\n}\ninput Filter @oneOf {\n  not: Filter\n  eq: String\n}\n\
     input N {\n  m: M\n}\ninput M {\n  n: N!\n}\ninput L {\n  l: [L!]!\n}\n",
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let analysis = analyze_one("type Query {\n  ok: Int\n}\ninput U {\n  u: Missing!\n}\n");
  assert_eq!(codes(&analysis), [UNDEFINED]);
}

/// Generic input objects whose type parameters each carry what their arguments require: directly,
/// forwarded through another input object, when non-null under a non-null argument, through a
/// requirement on the walk's path, and through a slot a default fills; OneOfs whose every
/// alternative leads back, directly and through an input object that holds one; and those whose
/// arguments are not required: a nullable field under a nullable argument, a OneOf with a field
/// that is no link, and a OneOf of bare parameters given a finite alternative, directly and
/// through an input object that holds one.
const GENERIC_CYCLES: &str = "type Query {\n  ok: Int\n}\n\
                              input Wrap<T> {\n  v: T!\n}\ninput A1 {\n  w: Wrap<A1>!\n}\n\
                              input Inner<U> {\n  u: U!\n}\ninput Outer<T> {\n  i: Inner<T>!\n}\n\
                              input A2 {\n  o: Outer<A2>!\n}\n\
                              input Choice<T> @oneOf {\n  a: T\n}\ninput A3 {\n  c: Choice<A3>!\n}\n\
                              input Wrap2<T> {\n  v: T\n}\ninput A5 {\n  w: Wrap2<A5>!\n}\n\
                              input Choice2<T> @oneOf {\n  a: T\n  b: Int\n}\n\
                              input A6 {\n  c: Choice2<A6>!\n}\ninput A8 {\n  w: Wrap2<A8!>!\n}\n\
                              input Outer3<T> {\n  i: Wrap2<T!>!\n}\ninput A10 {\n  o: Outer3<A10>!\n}\n\
                              input D1 {\n  a: Wrap<D2>!\n}\ninput D2 {\n  b: Wrap<D2>!\n}\n\
                              input Either<L, R> @oneOf {\n  left: L\n  right: R\n}\n\
                              input End {\n  ok: Boolean\n}\ninput Node {\n  next: Either<Node, End>!\n}\n\
                              input Node2 {\n  next: Either<Node2, Int>!\n}\n\
                              input N2 {\n  e: Either<N2, N2>!\n}\n\
                              input Box3<T> {\n  e: Either<T, End>!\n}\ninput N3 {\n  b: Box3<N3>!\n}\n\
                              input Fwd<X, Y = Inner<X>> {\n  y: Y!\n}\ninput A7 {\n  f: Fwd<A7>!\n}\n\
                              input Box2<T> {\n  e: Either<T, T>!\n}\ninput N4 {\n  b: Box2<N4>!\n}\n";

#[test]
fn a_parameter_carries_what_its_input_object_requires() {
  let text = GENERIC_CYCLES;
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let field = |code: &'static str, owner: &str, name: &str| -> (&'static str, Location) {
    (
      code,
      named(schema, text, &format!("input {owner} {{\n  "), name, 0),
    )
  };
  assert_eq!(
    found(&analysis),
    [
      field(NON_NULL, "A1", "w"),
      // Forwarded: `Outer`'s `T` is carried because `Inner`'s `U` is.
      field(NON_NULL, "A2", "o"),
      // A OneOf of one bare parameter: its one alternative is the argument.
      field(ONE_OF, "A3", "c"),
      // `Wrap2`'s `v` is nullable, and the non-null argument makes it a link.
      field(NON_NULL, "A8", "w"),
      // Passed on written `T!`, carried when non-null is carried.
      field(NON_NULL, "A10", "o"),
      // The cycle closes at `Wrap<D2>`, a requirement on the path: at `D2.b`, none at `D1.a`.
      field(NON_NULL, "D2", "b"),
      // An alternative whose every argument leads back.
      field(ONE_OF, "N2", "e"),
      // A true cycle through a slot a default fills: `Fwd<A7>` is `{ y: Inner<A7>! }`.
      field(NON_NULL, "A7", "f"),
      // Through `Box2`, which holds `Either<T, T>`: its group's one alternative leads back. `N3`,
      // whose `Box3` gives the finite `End` as the other alternative, has a value,
      // `{ b: { e: { right: { ok: true } } } }`.
      field(ONE_OF, "N4", "b"),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_one_of_is_finite_through_any_one_alternative() {
  // A OneOf is given one field, any one: `Choice<A>` has a value through `stop` whatever `A` is,
  // `{ choice: { stop: {} } }`; and through the input objects that hold it the choice stays one,
  // `V<A>` holding `W<A>`, which holds `Choice<A>`: `{ v: { w: { c: { stop: {} } } } }`.
  for text in [
    "type Query {\n  ok: Int\n}\ninput Stop {\n  n: Int\n}\n\
     input Choice<T> @oneOf {\n  again: T\n  stop: Stop\n}\ninput A {\n  choice: Choice<A>!\n}\n",
    "type Query {\n  ok: Int\n}\ninput Stop {\n  n: Int\n}\n\
     input Choice<T> @oneOf {\n  again: T\n  stop: Stop\n}\ninput W<T> {\n  c: Choice<T>!\n}\n\
     input V<T> {\n  w: W<T>!\n}\ninput A {\n  v: V<A>!\n}\n",
  ] {
    let analysis = analyze_text(text);
    assert!(
      analysis.diagnostics().is_empty(),
      "{text}: {:#?}",
      analysis.diagnostics()
    );
  }
  // With `Stop` requiring an `A`, every alternative is infinite: GraphQL's code, at the field
  // that leaves `A` and at the one that leads back from `Stop`.
  for (text, link) in [
    (
      "type Query {\n  ok: Int\n}\ninput A {\n  choice: Choice<A>!\n}\n\
       input Choice<T> @oneOf {\n  again: T\n  stop: Stop\n}\ninput Stop {\n  back: A!\n}\n",
      "choice",
    ),
    (
      "type Query {\n  ok: Int\n}\ninput A {\n  v: V<A>!\n}\ninput V<T> {\n  w: W<T>!\n}\n\
       input W<T> {\n  c: Choice<T>!\n}\n\
       input Choice<T> @oneOf {\n  again: T\n  stop: Stop\n}\ninput Stop {\n  back: A!\n}\n",
      "v",
    ),
  ] {
    let analysis = analyze_text(text);
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis),
      [
        (ONE_OF, named(schema, text, "input A {\n  ", link, 0)),
        (ONE_OF, named(schema, text, "input Stop {\n  ", "back", 0)),
      ],
      "{text}: {:#?}",
      analysis.diagnostics()
    );
  }
}

/// `choices` OneOfs `C1<T>` to `Cn<T>`, each `{ recur: T, stop: <stop> }`, which `W<T>` holds, one
/// in each field, and `input A { w: W<A>! }`: each OneOf gives `W` one group, "`A`, or `stop`".
fn capped(choices: usize, stop: &str) -> String {
  let mut text = String::from("type Query {\n  ok: Int\n}\ninput End {\n  n: Int\n}\n");
  for index in 1..=choices {
    text.push_str(&format!(
      "input C{index}<T> @oneOf {{\n  recur: T\n  stop: {stop}\n}}\n"
    ));
  }
  text.push_str("input W<T> {\n");
  for index in 1..=choices {
    text.push_str(&format!("  f{index}: C{index}<T>!\n"));
  }
  text.push_str("}\ninput A {\n  w: W<A>!\n}\n");
  text
}

#[test]
fn a_group_past_the_cap_requires_nothing() {
  // `W`'s one type parameter allows it four groups, and each `Ci<T>` it holds gives it one. With
  // `stop: End` each group has a value whatever `A` is, so `A` has one,
  // `{ w: { f1: { stop: {} }, … } }`, below the cap and past it: past it, `W` is judged as if it
  // had no group, and the cap never becomes a cycle.
  for choices in [4, 5] {
    let text = capped(choices, "End");
    let analysis = analyze_text(&text);
    assert!(
      analysis.diagnostics().is_empty(),
      "{text}: {:#?}",
      analysis.diagnostics()
    );
  }
  // With `stop: A`, every alternative of every group leads back to `A`, and `A` has no value.
  // Below the cap the groups require `A`, and the cycle is reported at `A.w`.
  let text = capped(4, "A");
  let analysis = analyze_text(&text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(ONE_OF, named(schema, &text, "input A {\n  ", "w", 0))],
    "{:#?}",
    analysis.diagnostics()
  );
  // Past the cap the same cycle is not reported: the recorded gap. `W` is judged as if it had no
  // group, which requires less than any group would, so the rule misses a cycle only a group past
  // its cap would close, in a program with type parameters, and invents none.
  let text = capped(5, "A");
  let analysis = analyze_text(&text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{text}: {:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn an_alternative_only_a_substitution_would_judge_requires_nothing() {
  // `Choice`'s one field is `Soft<T>`, whose finiteness under a use's argument only a substitution
  // would say: it is judged with `T` taken as finite, and `Soft<T>` has a value whatever `T` is,
  // `{}`. So neither `W<A>`, which holds `Choice<A>`, nor `Choice<B>` requires anything of its
  // argument: `A` has a value, `{ w: { c: { a: {} } } }`, and `B` one, `{ c: { a: {} } }`. And
  // `Pick<Soft<T>>`'s alternative `a` stands beside `b`, which leads back to `G` through `H<Int>`:
  // `G` has a value through `a`, `{ h: { p: { a: {} } } }`. Each was refused before, an
  // alternative only a substitution would judge being dropped beside another, and every argument
  // of the type being required when none stood.
  for text in [
    "type Query {\n  ok: Int\n}\ninput Soft<T> {\n  s: T\n}\n\
     input Choice<T> @oneOf {\n  a: Soft<T>\n}\ninput W<T> {\n  c: Choice<T>!\n}\n\
     input A {\n  w: W<A>!\n}\ninput B {\n  c: Choice<B>!\n}\n",
    "type Query {\n  ok: Int\n}\ninput Soft<T> {\n  s: T\n}\n\
     input Pick<T> @oneOf {\n  a: T\n  b: G\n}\ninput H<T> {\n  p: Pick<Soft<T>>!\n}\n\
     input G {\n  h: H<Int>!\n}\n",
  ] {
    let analysis = analyze_text(text);
    assert!(
      analysis.diagnostics().is_empty(),
      "{text}: {:#?}",
      analysis.diagnostics()
    );
  }
  // With `Hard`, whose `h` is non-null, `Choice<B>` is `{ a: { h: B } }`, and `B` has no value;
  // the rule, taking `T` as finite, does not report it. The recorded gap: what only a substitution
  // would say is never required, so the rule misses this cycle, and invents none.
  let text = "type Query {\n  ok: Int\n}\ninput Hard<T> {\n  h: T!\n}\n\
              input Choice<T> @oneOf {\n  a: Hard<T>\n}\ninput B {\n  c: Choice<B>!\n}\n";
  let analysis = analyze_text(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_slot_a_default_fills_carries_as_a_written_one() {
  // A filled argument is read as a written one, with the nullness its default gives it, and a slot
  // whose parameter is carried nowhere carries nothing, whatever fills it. `Use`'s `P` stands only
  // inside the slot `Fwd`'s default fills, as `Soft<P>`, whose `u` is nullable: `A9` has a value,
  // `{ u: { f: { y: {} } } }`. `Wrap`'s `T` fills `Cell`'s `U`, whose `x` is nullable: `A` has a
  // value, `{ w: { c: {} } }`.
  for text in [
    "type Query {\n  ok: Int\n}\ninput Soft<U> {\n  u: U\n}\n\
     input Fwd<X, Y = Soft<X>> {\n  y: Y!\n}\ninput Use<P> {\n  f: Fwd<P>!\n}\n\
     input A9 {\n  u: Use<A9>!\n}\n",
    "type Query {\n  ok: Int\n}\ninput Cell<T, U = T> {\n  x: U\n}\n\
     input Wrap<T> {\n  c: Cell<T>!\n}\ninput A {\n  w: Wrap<A>!\n}\n",
  ] {
    let analysis = analyze_text(text);
    assert!(
      analysis.diagnostics().is_empty(),
      "{text}: {:#?}",
      analysis.diagnostics()
    );
  }
  // With each field non-null, what the default fills is required, and each is a cycle.
  for (text, owner, link) in [
    (
      "type Query {\n  ok: Int\n}\ninput Soft<U> {\n  u: U!\n}\n\
       input Fwd<X, Y = Soft<X>> {\n  y: Y!\n}\ninput Use<P> {\n  f: Fwd<P>!\n}\n\
       input A9 {\n  u: Use<A9>!\n}\n",
      "A9",
      "u",
    ),
    (
      "type Query {\n  ok: Int\n}\ninput Cell<T, U = T> {\n  x: U!\n}\n\
       input Wrap<T> {\n  c: Cell<T>!\n}\ninput A {\n  w: Wrap<A>!\n}\n",
      "A",
      "w",
    ),
  ] {
    let analysis = analyze_text(text);
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis),
      [(
        NON_NULL,
        named(schema, text, &format!("input {owner} {{\n  "), link, 0)
      )],
      "{text}: {:#?}",
      analysis.diagnostics()
    );
  }
}

#[test]
fn a_type_many_link_fields_hold_is_read_once() {
  // `Wide`'s many parameters are all defaulted, and each of many fields of `Holder` is typed by
  // the closed `Wide!`, and of `Open` by the open `Wide<T>!`: what a complete type holds is read
  // once for the type, whichever fields hold it, and a closed type, which holds no type
  // parameter, is passed over at once. A test sees the diagnostics, not the work, which holds by
  // construction: each open type is one node, its arguments read once. Read for each field, this
  // program's 4,000 link fields would each read 2,000 arguments.
  const PARAMS: usize = 2_000;
  const FIELDS: usize = 2_000;
  let mut text = String::from("type Query {\n  ok: Int\n}\ninput Wide<T0 = Int");
  for index in 1..PARAMS {
    text.push_str(&format!(", T{index} = Int"));
  }
  text.push_str("> {\n  x: Int\n}\ninput Holder {\n");
  for index in 0..FIELDS {
    text.push_str(&format!("  w{index}: Wide!\n"));
  }
  text.push_str("  back: Holder!\n}\ninput Open<T> {\n");
  for index in 0..FIELDS {
    text.push_str(&format!("  w{index}: Wide<T>!\n"));
  }
  text.push_str("}\ninput Uses {\n  o: Open<Uses>!\n}\n");
  let analysis = analyze_text(&text);
  let schema = schema_file(&analysis);
  // `Holder` requires itself through `back`; `Uses` does not, `Wide`'s `T0` standing nowhere.
  assert_eq!(
    found(&analysis),
    [(NON_NULL, named(schema, &text, "  ", "back", 0))],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_dependencys_generic_input_object_closes_the_roots_cycle() {
  const OURS: &str = "type Query {\n  ok: Int\n}\ninput A4 {\n  w: ^::geo::Wrap<A4>!\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", OURS)]).with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Flat,
    &[("geo.graphqlx", "input Wrap<T> {\n  v: T!\n}\n")],
  );
  let analysis = analyze(&root, &[geo], &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(NON_NULL, named(schema, OURS, "  ", "w", 0))],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_cycle_through_an_extension_and_a_dependency_is_seen() {
  // The link that closes the cycle is a field another file's extension adds.
  const A: &str = "type Query {\n  ok: Int\n}\ninput A {\n  x: Int\n}\n";
  const B: &str = "extend input A {\n  b: B!\n}\ninput B {\n  a: A!\n}\n";
  let root = package("app", Layout::Flat, &[("a.graphqlx", A), ("b.graphqlx", B)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let b = file(&analysis, 3, "b.graphqlx");
  assert_eq!(
    found(&analysis),
    [(NON_NULL, named(b, B, "  ", "a", 0))],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_cycle_is_reported_once_at_a_field_that_closes_it() {
  let text = "type Query {\n  ok: Int\n}\ninput R1 {\n  next: R2!\n}\ninput R2 {\n  next: R3!\n}\n\
              input R3 {\n  next: R1!\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(NON_NULL, named(schema, text, "  ", "next", 2))],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_long_chain_of_required_input_objects_does_not_recurse() {
  // Twenty thousand input objects, each requiring the next, the last the first: one cycle, found
  // and reported with explicit stacks.
  const COUNT: usize = 20_000;
  let mut text = String::from("type Query {\n  ok: Int\n}\n");
  for index in 0..COUNT {
    let next = (index + 1) % COUNT;
    text.push_str(&format!("input I{index} {{\n  n: I{next}!\n}}\n"));
  }
  let analysis = analyze_text(&text);
  assert_eq!(
    codes(&analysis),
    [NON_NULL],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn the_cycle_rule_says_nothing_where_a_phase_spoke() {
  // A phase's error in the input object that owns the field silences it.
  let analysis = analyze_one("type Query {\n  ok: Int\n}\ninput A {\n  a: A!\n  x: Missing\n}\n");
  assert_eq!(
    codes(&analysis),
    [UNDEFINED],
    "{:#?}",
    analysis.diagnostics()
  );
  let analysis =
    analyze_one("type Query {\n  ok: Int\n}\ninput A {\n  a: A!\n}\ninput U {\n  x: Missing\n}\n");
  let mut reported = codes(&analysis);
  reported.sort_unstable();
  assert_eq!(reported, [NON_NULL, UNDEFINED]);
  // One in another input object of the cycle does not: the field that closes it is `B.a`.
  let text =
    "type Query {\n  ok: Int\n}\ninput A {\n  b: B!\n  x: Missing\n}\ninput B {\n  a: A!\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (UNDEFINED, named(schema, text, "x: ", "Missing", 0)),
      (NON_NULL, named(schema, text, "  ", "a", 0)),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn long_names_in_the_cycle_rule_are_quoted_as_excerpts() {
  let owner = "I".repeat(4096);
  let field = "f".repeat(4096);
  for text in [
    format!("type Query {{\n  ok: Int\n}}\ninput {owner} {{\n  {field}: {owner}!\n}}\n"),
    format!(
      "type Query {{\n  ok: Int\n}}\ninput Back @oneOf {{\n  b: {owner}\n}}\n\
       input {owner} {{\n  {field}: Back!\n}}\n"
    ),
  ] {
    let analysis = analyze_text(&text);
    let messages = messages(&analysis);
    assert_eq!(messages.len(), 1, "{:#?}", analysis.diagnostics());
    assert!(messages[0].len() < 1024, "{}", messages[0]);
  }
}
