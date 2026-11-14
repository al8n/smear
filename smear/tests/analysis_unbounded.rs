//! The rule a backend composes, `UnboundedInstantiation`: a generic definition that reaches itself
//! with a type argument that grows is refused at the occurrence that grows it, and `analyze` never
//! runs the rule. Each test walks the public surface with the rule, over small packages.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Diagnostic, Diagnostics, Layout, Limits, Package, PackageId, Packages, SourceFile,
    SourceId, Tier, UnboundedInstantiation, Visitor, analyze, visit,
  },
  diagnostic::{Diagnose, Label, Location, Severity},
  lexer::tokora::SimpleSpan,
};

/// The rule's code.
const UNBOUNDED: &str = "smear::graphqlx::unbounded-instantiation";

/// W's `Nest`: `Nest<i32>` needs `Nest<[i32]>`, which needs `Nest<[[i32]]>`, without end.
const NEST: &str = "type Nest<T> { value: T  deeper: Nest<[T]> }\ntype Query { nest: Nest<i32> }\n";

/// A package named `name` of `layout`, at tier `Std`, holding `files`, `(path, text)`.
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

/// An analysis of the root package `app`'s one file `text`, which `analyze` accepts: a program it
/// refuses fails here and not at what a test expects of the rule, so that the rule is never pinned
/// on a program `analyze` turns away.
fn accepted(text: &'static str) -> Analysis {
  let analysis = analyze_one(text);
  assert!(analysis.is_ok(), "{text}\n{:#?}", analysis.diagnostics());
  analysis
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

/// Where `name` is written within the first occurrence of `written` in `text`, the file `source`.
fn within(source: SourceId, text: &str, written: &str, name: &str) -> Location {
  let start = text
    .find(written)
    .and_then(|found: usize| written.find(name).map(|offset: usize| found + offset))
    .unwrap_or_else(|| panic!("`{name}` is not written within `{written}`"));
  Location::new(source.get(), SimpleSpan::new(start, start + name.len()))
}

/// What the rule reports walking `packages` of `analysis`, one fresh value of it: each
/// diagnostic's primary location and its first label's location, once every diagnostic is seen to
/// be the rule's error.
fn unbounded(analysis: &Analysis, packages: Packages) -> Vec<(Location, Option<Location>)> {
  let diagnostics = visit(analysis, packages, &mut UnboundedInstantiation::new());
  diagnostics
    .iter()
    .map(|diagnostic: &Diagnostic| {
      assert_eq!(diagnostic.code().as_str(), UNBOUNDED, "{diagnostic:#?}");
      assert_eq!(diagnostic.severity(), Severity::Error, "{diagnostic:#?}");
      (
        diagnostic.primary(),
        diagnostic.label(0).map(|label: Label<'_>| label.location()),
      )
    })
    .collect()
}

/// Asserts that the rule, walking `packages` of `analysis`, reports nothing.
fn silent(analysis: &Analysis, packages: Packages) {
  let reported = unbounded(analysis, packages);
  assert!(reported.is_empty(), "{reported:#?}");
}

/// One diagnostic, at the occurrence that writes the growth, labelled at its list, and nothing at
/// the list, at the field or at the definition: a report anywhere but the occurrence, and a second
/// one inside it, fail here.
#[test]
fn a_type_argument_that_wraps_its_parameter_is_refused_where_it_is_written() {
  let analysis = analyze_one(NEST);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      at(schema, NEST, "Nest<[T]>", 0),
      Some(at(schema, NEST, "[T]", 0))
    )]
  );

  let diagnostics = visit(&analysis, Packages::All, &mut UnboundedInstantiation::new());
  let diagnostic = diagnostics.iter().next().expect("one diagnostic");
  assert_eq!(
    diagnostic.to_string(),
    "an instantiation of `Nest` that grows without end"
  );
  assert_eq!(diagnostic.primary_label(), Some("unbounded instantiation"));
  assert_eq!(
    diagnostic.label(0).map(|label: Label<'_>| label.text()),
    Some("this type argument wraps a type parameter that it reaches again")
  );
  assert_eq!(
    diagnostic.help(),
    Some(
      "every instantiation of `Nest` needs this one, with a larger type argument, so a backend \
       that makes one copy per instantiation cannot finish; pass the type parameter itself, or a \
       type that names none"
    )
  );
}

/// The program the rule refuses is one `analyze` accepts, and no diagnostic of the analysis has the
/// rule's code: the rule in `analyze`'s own set fails here.
#[test]
fn analyze_never_runs_the_rule() {
  let analysis = analyze_one(NEST);
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert!(
    analysis
      .diagnostics()
      .iter()
      .all(|diagnostic: &Diagnostic| diagnostic.code().as_str() != UNBOUNDED),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// The same instantiation again is one instantiation: "any self-reference" fails here.
#[test]
fn the_same_instantiation_again_is_not_growth() {
  let analysis =
    analyze_one("type Nest<T> { value: T  deeper: Nest<T> }\ntype Query { nest: Nest<i32> }\n");
  silent(&analysis, Packages::All);
}

/// Parameters passed along in another order give two instantiations: "any cycle" fails here.
#[test]
fn swapped_parameters_are_not_growth() {
  let analysis = analyze_one("type Swap<T, U> { s: Swap<U, T> }\ntype Query { x: Int }\n");
  silent(&analysis, Packages::All);
}

/// A growth on a cycle through two definitions is reported where it grows, and nowhere else on the
/// cycle: a report at every edge of a cycle fails here.
#[test]
fn a_cycle_through_two_definitions_is_refused_at_the_growth_alone() {
  let text = "type A<T> { b: B<[T]> }\ntype B<U> { a: A<U> }\ntype Query { x: Int }\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      at(schema, text, "B<[T]>", 0),
      Some(at(schema, text, "[T]", 0))
    )]
  );
}

/// A growth into a parameter that reaches no cycle is one more instantiation, once: "any growing
/// edge" fails here.
#[test]
fn growth_that_reaches_no_cycle_is_accepted() {
  let analysis =
    analyze_one("type A<T> { b: B<[T]> }\ntype B<U> { v: U }\ntype Query { x: Int }\n");
  silent(&analysis, Packages::All);
}

/// `!` does not nest, and a collection's key, which its holder keeps non-null while the written
/// argument's own type is nullable, passes its parameter along: `!` read as growth, and an argument
/// read as growth because it differs from the written argument's own type, fail here. The key's
/// cycle, `A.T` to `Bag.T` to `d.U` to `A.T`, is plain.
#[test]
fn non_null_and_a_collections_key_are_not_growth() {
  let analysis =
    analyze_one("type Nest<T> { value: T  deeper: Nest<T!> }\ntype Query { x: Int }\n");
  silent(&analysis, Packages::All);

  let analysis = analyze_one(
    "directive @d<U>(x: A<U>) on SCALAR\nscalar Bag<T> @set @d<T>\ninput A<T> { m: Bag<T> }\n\
     type Query { x: Int }\n",
  );
  silent(&analysis, Packages::All);
}

/// A growth into the second parameter, whose cycle runs through the first, is found; and the near
/// miss, whose second parameter reaches nothing, is not. Each parameter tested against its own slot
/// alone fails the first, and components taken per definition, where `[T]` would reach itself
/// through `A`, fail the second.
#[test]
fn a_cycle_through_two_parameters_is_found() {
  let text = "type A<T, U> { f: A<U, [T]> }\ntype Query { x: Int }\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      at(schema, text, "A<U, [T]>", 0),
      Some(at(schema, text, "[T]", 0))
    )]
  );

  let analysis = analyze_one("type A<T, U> { f: A<i32, [T]> }\ntype Query { x: Int }\n");
  silent(&analysis, Packages::All);
}

/// A collection keeps its element non-null, so the written element's own type is another type than
/// the one its holder holds: the report is at the element where the growth is written, once, and
/// not at the collection. Entering only the roots' complete types fails here, reporting at the
/// collection with no default to label.
#[test]
fn a_collections_element_is_reported_where_the_growth_is_written() {
  let text = "input Node<T> implements MapKey { value: T  children: HashSet<Node<[T]>> }\n\
              type Query { f(n: Node<i32>): Int }\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      at(schema, text, "Node<[T]>", 0),
      Some(at(schema, text, "[T]", 0))
    )]
  );
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  // An object is no key, so `analyze` refuses the set, which keeps its type.
  let text = "type Nest<T> { value: T  set: HashSet<Nest<[T]>> }\ntype Query { n: Nest<i32> }\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      at(schema, text, "Nest<[T]>", 0),
      Some(at(schema, text, "[T]", 0))
    )]
  );
}

/// An application that writes more type arguments than its directive has parameters gives no edge,
/// and nothing panics. With the directive written first, the node after its last parameter is
/// `G`'s `T`, so reading past the directive's parameters closes a false cycle with `[T]` and fails
/// here.
#[test]
fn type_arguments_past_a_directives_parameters_give_no_edge() {
  for text in [
    "directive @d<A> on FIELD_DEFINITION\ntype G<T> { f: Int @d<T, [T]> }\n",
    "directive @e on FIELD_DEFINITION\ntype G<T> { f: Int @e<[T]> }\n",
  ] {
    let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
    let analysis = analyze(&root, &[], &Limits::new());
    silent(&analysis, Packages::All);
  }
}

/// A growth in each followed position is reported where it is written, labelled at its list: an
/// `implements` entry, a union member, a field's argument, an input field, a directive
/// definition's argument, and a field an extension writes. A followed position that is not
/// followed fails here.
#[test]
fn each_followed_position_is_followed() {
  for (text, growth, list) in [
    (
      "interface I<T> implements J<[T]> { a: Int }\ninterface J<U> implements I<U> { a: Int }\n\
       type Query { x: Int }\n",
      "J<[T]>",
      "[T]",
    ),
    (
      "union U<T> = B<[T]>\ntype B<S> { u: U<S> }\ntype Query { x: Int }\n",
      "B<[T]>",
      "[T]",
    ),
    (
      "type G<T> { f(a: G<[T]>): Int }\ntype Query { x: Int }\n",
      "G<[T]>",
      "[T]",
    ),
    (
      "input N<T> { v: T  deeper: N<[T]> }\ntype Query { x: Int }\n",
      "N<[T]>",
      "[T]",
    ),
    (
      "directive @d<U>(x: G<[U]>) on INPUT_OBJECT\ninput G<T> @d<T> { v: T }\n\
       type Query { x: Int }\n",
      "G<[U]>",
      "[U]",
    ),
    (
      "type Nest<T> { value: T }\nextend type Nest<T> { deeper: Nest<[T]> }\n\
       type Query { x: Int }\n",
      "Nest<[T]>",
      "[T]",
    ),
  ] {
    let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
    let analysis = analyze(&root, &[], &Limits::new());
    let schema = schema_file(&analysis);
    assert_eq!(
      unbounded(&analysis, Packages::All),
      [(at(schema, text, growth, 0), Some(at(schema, text, list, 0)))],
      "{text}"
    );
  }
}

/// The `implements` entry of the test above, in a program `analyze` accepts: an object implements
/// the interface and the interface's field names the object, so no interface implements itself
/// through another. The entry `I<[U]>` and `G`'s own field `G<[U]>` each write a growth, and each
/// is reported where it is written, labelled at its list.
#[test]
fn an_implements_entry_that_grows_is_refused_in_an_accepted_program() {
  let text = "interface I<T> { f: G<T> }\ntype G<U> implements I<[U]> { f: G<[U]> }\n\
              type Query { x: Int }\n";
  let analysis = accepted(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [
      (
        at(schema, text, "I<[U]>", 0),
        Some(at(schema, text, "[U]", 0))
      ),
      (
        at(schema, text, "G<[U]>", 0),
        Some(at(schema, text, "[U]", 1))
      ),
    ]
  );
}

/// The field's argument of the test above, in a program `analyze` accepts: the argument's type is
/// an input object, where an object is `argument-type-not-input-type`, and the input object's field
/// applies `@e<G<U>>`, which names `G` again. Reported at the argument's type, labelled at its
/// list.
#[test]
fn a_fields_argument_that_grows_is_refused_in_an_accepted_program() {
  let text = "directive @e<W> on INPUT_FIELD_DEFINITION\ninput X<U> { v: U @e<G<U>> }\n\
              type G<T> { f(a: X<[T]>): Int }\ntype Query { x: Int }\n";
  let analysis = accepted(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      at(schema, text, "X<[T]>", 0),
      Some(at(schema, text, "[T]", 0))
    )]
  );
}

/// The directive definition's argument of the test above, in a program `analyze` accepts: `d`'s
/// argument is an input object whose field applies `@e<G<V>>`, and `G`'s field applies `@d<T>`. An
/// object's field is no part of the graph `self-referential-directive` reads, so `d` is on no cycle
/// there. Reported at the argument's type, labelled at its list.
#[test]
fn a_directive_definitions_argument_that_grows_is_refused_in_an_accepted_program() {
  let text = "directive @e<W> on INPUT_FIELD_DEFINITION\n\
              directive @d<U>(x: X<[U]>) on FIELD_DEFINITION\n\
              input X<V> { v: V @e<G<V>> }\ntype G<T> { f: Int @d<T> }\ntype Query { x: Int }\n";
  let analysis = accepted(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      at(schema, text, "X<[U]>", 0),
      Some(at(schema, text, "[U]", 0))
    )]
  );
}

/// A bound's interface and a type parameter's default as it is written write the same growing type
/// as a field beside them, and only the field's is reported: a hook that reads no site fails here.
#[test]
fn a_bound_and_a_written_default_are_not_followed() {
  for (text, growth) in [
    (
      "interface Cmp<X> { c: X }\ntype G<T> where T: Cmp<G<[T]>> { deeper: G<[T]> }\n\
       type Query { x: Int }\n",
      "G<[T]>",
    ),
    (
      "type H<T, U = H<[T], T>> { deeper: H<[T], T> }\ntype Query { x: Int }\n",
      "H<[T], T>",
    ),
  ] {
    let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
    let analysis = analyze(&root, &[], &Limits::new());
    let schema = schema_file(&analysis);
    assert_eq!(
      unbounded(&analysis, Packages::All),
      [(
        at(schema, text, growth, 1),
        Some(at(schema, text, "[T]", 1))
      )],
      "{text}"
    );
  }
}

/// An interface's instantiation is not followed into the types that implement it. `G<i32>` needs
/// `I<i32>`, `H<i32>` and `I<[i32]>`, and the rule is silent. A backend that also emits the
/// implementors of `I<[i32]>` needs `G<[i32]>`, then `H<[i32]>`, then `I<[[i32]]>`, without end,
/// and checks that itself: an implementor followed from its interface's instantiation fails here.
#[test]
fn an_interfaces_instantiation_is_not_followed_into_its_implementors() {
  let analysis = accepted(
    "interface I<T> { a: Int }\ntype G<U> implements I<U> { a: Int  h: H<U> }\n\
     type H<V> { i: I<[V]> }\ntype Query { g: G<i32> }\n",
  );
  silent(&analysis, Packages::All);
}

/// `E<B>` is `E<B, [B]>`: the growth is the default `[A]`, filled for `B`, and the label is at the
/// default where it is declared. Reading the written arguments in place of the type fails here.
#[test]
fn a_default_that_wraps_its_parameter_is_labelled_where_it_is_declared() {
  let text = "type E<A, B = [A]> { next: E<B> }\ntype Query { x: Int }\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      at(schema, text, "E<B>", 0),
      Some(at(schema, text, "[A]", 0))
    )]
  );
}

/// A growth that only a filled default holds, one level in and two, is reported at the occurrence
/// that leaves the argument out, labelled at that default. Reporting only a type's own arguments
/// fails here.
#[test]
fn growth_inside_a_filled_default_is_reported_at_the_use() {
  for (text, default) in [
    (
      "type P<T, U = P<[T], i32>> { f: P<T> }\ntype Query { x: Int }\n",
      "P<[T], i32>",
    ),
    (
      "type Box<V> { v: V }\ntype P<T, U = Box<P<[T], i32>>> { f: P<T> }\ntype Query { x: Int }\n",
      "Box<P<[T], i32>>",
    ),
  ] {
    let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
    let analysis = analyze(&root, &[], &Limits::new());
    let schema = schema_file(&analysis);
    assert_eq!(
      unbounded(&analysis, Packages::All),
      [(
        at(schema, text, "P<T>", 0),
        Some(at(schema, text, default, 0))
      )],
      "{text}"
    );
  }
}

/// `P<X>` is `P<X, Q<[X]>>`. `P` needs nothing, its two fields being bare parameters, and the
/// growth is `Q`'s, which the default fills in. The report is at the occurrence that leaves the
/// argument out, labelled at the default, and its help says what the author can do: write the type
/// argument the default fills, or change the default. The marked case's help, to pass the type
/// parameter itself, is no advice here, since the occurrence passes it already: a held case that
/// keeps it fails here.
#[test]
fn a_held_growth_is_helped_by_writing_the_argument_or_changing_the_default() {
  let text = "type P<T, U = Q<[T]>> { v: T  w: U }\ntype Q<X> { p: P<X> }\n\
              type Query { x: Int }\n";
  let analysis = accepted(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      at(schema, text, "P<X>", 0),
      Some(at(schema, text, "Q<[T]>", 0))
    )]
  );

  let diagnostics = visit(&analysis, Packages::All, &mut UnboundedInstantiation::new());
  let diagnostic = diagnostics.iter().next().expect("one diagnostic");
  assert_eq!(
    diagnostic.to_string(),
    "an instantiation of `P` whose default grows without end"
  );
  assert_eq!(
    diagnostic.label(0).map(|label: Label<'_>| label.text()),
    Some("this default holds a type argument that wraps a type parameter it reaches again")
  );
  assert_eq!(
    diagnostic.help(),
    Some(
      "this occurrence leaves out a type argument of `P` whose default holds an instantiation \
       that grows without end, so a backend that makes one copy per instantiation cannot finish; \
       write the type argument the default fills, or change the default"
    )
  );
}

/// With two parameters left out, the label is at the default that filled the first argument that
/// holds the growth, `V`'s, and not at `U`'s, the first left out: a label at the first parameter
/// left out fails here.
#[test]
fn the_label_is_at_the_default_that_holds_the_growth() {
  let text = "type P<T, U = i32, V = P<[T], i32, i32>> { f: P<T> }\ntype Query { x: Int }\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      at(schema, text, "P<T>", 0),
      Some(at(schema, text, "P<[T], i32, i32>", 0))
    )]
  );
}

/// An application whose type argument closes a growing cycle is reported at the application's
/// path, the directive's name after its `@`, labelled at the type argument, and nothing is
/// reported at the type argument itself: applications not followed, and a second report at the
/// type argument, fail here.
#[test]
fn an_application_that_grows_a_cycle_is_refused_at_the_application() {
  let text = "directive @d<U>(x: G<U>) on INPUT_OBJECT\ninput G<T> @d<[T]> { v: T }\n\
              type Query { x: Int }\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      within(schema, text, "@d<[T]>", "d"),
      Some(at(schema, text, "[T]", 0))
    )]
  );
}

/// The growth of the test above, in a program `analyze` accepts: `G`'s field applies `@d<[T]>`, and
/// an object's field is no part of the graph `self-referential-directive` reads, so `d`, whose
/// argument names `G<U>` through `@e`, is on no cycle there. Reported as above, at the
/// application's path, labelled at the type argument.
#[test]
fn an_application_that_grows_a_cycle_is_refused_in_an_accepted_program() {
  let text = "directive @e<W> on ARGUMENT_DEFINITION\n\
              directive @d<U>(x: Int @e<G<U>>) on FIELD_DEFINITION\n\
              type G<T> { f: Int @d<[T]> }\ntype Query { x: Int }\n";
  let analysis = accepted(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      within(schema, text, "@d<[T]>", "d"),
      Some(at(schema, text, "[T]", 0))
    )]
  );
}

/// `@d<T>` is `@d<T, [T]>`: the growth is the default `[A]`, filled for `B`, which the application
/// leaves out, so the report is at the application's path, labelled at the default where it is
/// declared. A rule that reads the written type arguments alone sees nothing and fails here.
#[test]
fn an_application_that_grows_through_a_filled_default_is_refused_at_the_application() {
  let text = "directive @d<A, B = [A]>(x: G<B>) on INPUT_OBJECT\ninput G<T> @d<T> { v: T }\n\
              type Query { x: Int }\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      within(schema, text, "@d<T>", "d"),
      Some(at(schema, text, "[A]", 0))
    )]
  );

  let diagnostics = visit(&analysis, Packages::All, &mut UnboundedInstantiation::new());
  let diagnostic = diagnostics.iter().next().expect("one diagnostic");
  assert_eq!(
    diagnostic.to_string(),
    "an application of `@d` that grows without end"
  );
  assert_eq!(
    diagnostic.label(0).map(|label: Label<'_>| label.text()),
    Some("this default fills a type argument that wraps a type parameter it reaches again")
  );
}

/// The growth of the test above, in a program `analyze` accepts: `G`'s field applies `@d<T>`, which
/// is `@d<T, [T]>`, and `d`'s argument names `G<B>` through `@e`, so `d` is on no cycle of
/// `self-referential-directive`'s graph, which leaves out an object's field. Reported as above, at
/// the application's path, labelled at the default.
#[test]
fn an_application_that_grows_through_a_filled_default_is_refused_in_an_accepted_program() {
  let text = "directive @e<W> on ARGUMENT_DEFINITION\n\
              directive @d<A, B = [A]>(x: Int @e<G<B>>) on FIELD_DEFINITION\n\
              type G<T> { f: Int @d<T> }\ntype Query { x: Int }\n";
  let analysis = accepted(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      within(schema, text, "@d<T>", "d"),
      Some(at(schema, text, "[A]", 0))
    )]
  );

  let diagnostics = visit(&analysis, Packages::All, &mut UnboundedInstantiation::new());
  let diagnostic = diagnostics.iter().next().expect("one diagnostic");
  assert_eq!(
    diagnostic.to_string(),
    "an application of `@d` that grows without end"
  );
  assert_eq!(
    diagnostic.label(0).map(|label: Label<'_>| label.text()),
    Some("this default fills a type argument that wraps a type parameter it reaches again")
  );
}

/// `@d<T>` is `@d<T, H<[T]>>`, which does not grow and holds `H<[T]>`, which does: `G<Int>` needs
/// `@d<Int>`, which holds `H<[Int]>`, whose field is `G<[Int]>`, and so on without end. No
/// occurrence is written with the type `H<[T]>`, so the report is at the application's path,
/// labelled at the default that holds it, and nothing is reported at `G<U>`. A rule that reports
/// only an application whose type is itself marked fails here. The application passes the type
/// parameter itself already, so its help is not the marked case's advice to pass it: it says to
/// write the type argument the default fills, or to change the default.
#[test]
fn an_application_whose_filled_default_holds_a_growth_is_refused_at_the_application() {
  let text = "directive @d<A, B = H<[A]>> on OBJECT\ntype H<U> { g: G<U> }\n\
              type G<T> @d<T> { v: T }\ntype Query { x: Int }\n";
  let analysis = accepted(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      within(schema, text, "@d<T>", "d"),
      Some(at(schema, text, "H<[A]>", 0))
    )]
  );

  let diagnostics = visit(&analysis, Packages::All, &mut UnboundedInstantiation::new());
  let diagnostic = diagnostics.iter().next().expect("one diagnostic");
  assert_eq!(
    diagnostic.to_string(),
    "an application of `@d` whose default grows without end"
  );
  assert_eq!(
    diagnostic.label(0).map(|label: Label<'_>| label.text()),
    Some("this default holds a type argument that wraps a type parameter it reaches again")
  );
  assert_eq!(
    diagnostic.help(),
    Some(
      "this application leaves out a type argument of `@d` whose default holds an \
       instantiation that grows without end, so a backend that makes one copy per instantiation \
       cannot finish; write the type argument the default fills, or change the default"
    )
  );
}

/// The rule judges every definition, and reports only what the walk covers: a dependency's growth
/// is not reported under `Packages::Root`, though the root instantiates it, and is reported in the
/// dependency's file under `Packages::Supplied` and `Packages::All`. A report made outside the
/// cover, such as one made in `begin`, fails here.
#[test]
fn a_dependencys_growth_is_reported_only_where_the_walk_covers_it() {
  let nest = "type Nest<T> { value: T  deeper: Nest<[T]> }\n";
  let root = package(
    "app",
    Layout::Flat,
    &[(
      "schema.graphqlx",
      "type Query { nest: ^::geo::Nest<i32> }\n",
    )],
  )
  .with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", nest)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  silent(&analysis, Packages::Root);
  let reported = [(
    at(geo_file, nest, "Nest<[T]>", 0),
    Some(at(geo_file, nest, "[T]", 0)),
  )];
  assert_eq!(unbounded(&analysis, Packages::Supplied), reported);
  assert_eq!(unbounded(&analysis, Packages::All), reported);
}

/// A path that did not resolve, with a growth written on it, and a growing definition instantiated
/// with one, are skipped, and the growth beside them is reported: reading a type the analysis
/// refused fails here.
#[test]
fn what_did_not_resolve_is_skipped() {
  let text = "type Nest<T> { deeper: Nest<[T]>  lost: Missing<[T]>  other: Nest<Missing> }\n\
              type Query { x: Int }\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unbounded(&analysis, Packages::All),
    [(
      at(schema, text, "Nest<[T]>", 0),
      Some(at(schema, text, "[T]", 0))
    )]
  );
}

/// One value walks three analyses in turn, and each walk answers for its own: a `begin` that does
/// not reset fails here.
#[test]
fn one_value_serves_many_walks() {
  let growing = analyze_one(NEST);
  let same =
    analyze_one("type Nest<T> { value: T  deeper: Nest<T> }\ntype Query { nest: Nest<i32> }\n");
  let mut rule = UnboundedInstantiation::new();
  assert_eq!(visit(&growing, Packages::All, &mut rule).len(), 1);
  assert!(visit(&same, Packages::All, &mut rule).is_empty());
  assert_eq!(visit(&growing, Packages::All, &mut rule).len(), 1);
}

/// The rule alone, beside `()`, borrowed and boxed as a trait object, and made by `default()`,
/// reports the same; and so does an analysis of the same packages with the files and the
/// dependencies passed in the other order. State shared between walks, and a result that depends
/// on the order of the inputs, fail here.
#[test]
fn the_rule_composes_and_does_not_depend_on_order() {
  fn assert_send_sync<T: Send + Sync>() {}
  assert_send_sync::<UnboundedInstantiation>();

  let analysis = analyze_one(NEST);
  let alone: Diagnostics = visit(&analysis, Packages::All, &mut UnboundedInstantiation::new());
  assert_eq!(alone.len(), 1);
  assert_eq!(
    visit(
      &analysis,
      Packages::All,
      &mut (UnboundedInstantiation::new(), ())
    ),
    alone
  );
  let mut rule = UnboundedInstantiation::new();
  let borrowed: &mut dyn Visitor = &mut rule;
  assert_eq!(visit(&analysis, Packages::All, borrowed), alone);
  let mut boxed: Box<dyn Visitor> = Box::new(UnboundedInstantiation::new());
  assert_eq!(visit(&analysis, Packages::All, &mut boxed), alone);
  assert_eq!(
    visit(
      &analysis,
      Packages::All,
      &mut UnboundedInstantiation::default()
    ),
    alone
  );

  let files = [
    ("a.graphqlx", "type A<T> { value: T  deeper: A<[T]> }\n"),
    (
      "b.graphqlx",
      "type Query { a: A<i32>  g: ^::geo::G<i32>  m: ^::map::M<i32> }\n",
    ),
  ];
  let geo = [("geo.graphqlx", "type G<T> { value: T  deeper: G<[T]> }\n")];
  let map = [("map.graphqlx", "type M<T> { value: T  deeper: M<[T]> }\n")];
  let forward = analyze(
    &package("app", Layout::Flat, &files).with_dependencies(["geo", "map"]),
    &[
      package("geo", Layout::Flat, &geo),
      package("map", Layout::Flat, &map),
    ],
    &Limits::new(),
  );
  let backward = analyze(
    &package("app", Layout::Flat, &[files[1], files[0]]).with_dependencies(["map", "geo"]),
    &[
      package("map", Layout::Flat, &map),
      package("geo", Layout::Flat, &geo),
    ],
    &Limits::new(),
  );
  let reported = visit(&forward, Packages::All, &mut UnboundedInstantiation::new());
  assert_eq!(reported.len(), 3, "{reported:#?}");
  assert_eq!(
    visit(&backward, Packages::All, &mut UnboundedInstantiation::new()),
    reported
  );
}

/// A definition named with four kilobytes is quoted by an excerpt of its name, so the message is
/// under a kilobyte: a name written whole fails here.
#[test]
fn a_long_name_is_quoted_as_an_excerpt() {
  let name = "N".repeat(4096);
  let text = format!(
    "type {name}<T> {{ value: T  deeper: {name}<[T]> }}\ntype Query {{ nest: {name}<i32> }}\n"
  );
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text.as_str())]);
  let analysis = analyze(&root, &[], &Limits::new());
  let diagnostics = visit(&analysis, Packages::All, &mut UnboundedInstantiation::new());
  let messages: Vec<String> = diagnostics
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect();
  assert_eq!(
    messages,
    [format!(
      "an instantiation of `{}…{}` that grows without end",
      "N".repeat(64),
      "N".repeat(64)
    )]
  );
  assert!(messages.iter().all(|message: &String| message.len() < 1024));
}
