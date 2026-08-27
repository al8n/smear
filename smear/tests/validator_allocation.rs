//! The steady state allocates nothing, measured rather than asserted.
//!
//! # What is being claimed
//!
//! `validate_executable` owns no storage. The working set is the caller's [`Scratch`] and the
//! diagnostics are the caller's sink, so once both have seen a request the size of the ones to
//! come, a validation performs **zero heap allocations** — not "few", not "amortised".
//!
//! # How it is measured
//!
//! A counting global allocator, and a thread-local counter so that a test running beside this one
//! on another thread cannot perturb the reading. The corpus is warmed once — which is where the
//! `Scratch`'s buffers and the collecting sink's `Vec` reach their high-water marks — and every
//! validation after that is measured with the counter.
//!
//! [`the_gate_counts`] is the discrimination check: it shows the allocator is installed and the
//! counter moves, so a green reading below means "nothing allocated" rather than "nothing was
//! looking".

// Every fixture and assertion below calls into `smear::validator`, which does not exist in the
// crate's API surface with the feature off. This file did not gate itself to that dependency —
// the same defect `validator_schema.rs` carried until #100 — so it was a hard `E0433` compile
// error under any feature selection that excludes `validator`, including the crate's own default
// features. `cargo test -p smear --no-run` in CI now compiles exactly that selection.
#![cfg(feature = "validator")]
#![allow(missing_docs)]

use std::{
  alloc::{GlobalAlloc, Layout, System},
  cell::Cell,
  vec::Vec,
};

use smear::{
  diagnostic::{Diagnose, DiagnoseExt},
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  validator::{
    Budget, Collect, Count, Diagnostic, First, Ignore, Schema, Scratch, validate_executable,
  },
};

// ---------------------------------------------------------------------------------------------
// the counting allocator
// ---------------------------------------------------------------------------------------------

thread_local! {
  /// Allocation events on this thread. `alloc`, `alloc_zeroed` and a growing `realloc` all count.
  static ALLOCATIONS: Cell<u64> = const { Cell::new(0) };
}

struct Counting;

/// Counts every allocation event and forwards to the system allocator.
///
/// The counter is thread-local and updated through `try_with`, so an allocation made while the
/// thread's local storage is being set up or torn down is simply not counted rather than
/// re-entering it.
unsafe impl GlobalAlloc for Counting {
  unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
    bump();
    unsafe { System.alloc(layout) }
  }

  unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
    bump();
    unsafe { System.alloc_zeroed(layout) }
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
    bump();
    unsafe { System.realloc(ptr, layout, new_size) }
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
    unsafe { System.dealloc(ptr, layout) }
  }
}

#[global_allocator]
static ALLOCATOR: Counting = Counting;

fn bump() {
  let _ = ALLOCATIONS.try_with(|count| count.set(count.get() + 1));
}

/// Runs `body` and returns how many allocation events it caused on this thread.
fn allocations(body: impl FnOnce()) -> u64 {
  let before = ALLOCATIONS.with(Cell::get);
  body();
  ALLOCATIONS.with(Cell::get) - before
}

// ---------------------------------------------------------------------------------------------
// the corpus
// ---------------------------------------------------------------------------------------------

const SCHEMA: &str = r#"
type Query {
  hero(episode: Episode): Character
  droid(id: ID!): Droid
  search(text: String, filter: SearchFilter): [SearchResult]
}

interface Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
}

type Human implements Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
  homePlanet: String
  height(unit: LengthUnit = METER): Float
}

type Droid implements Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
  primaryFunction: String
}

union SearchResult = Human | Droid

enum Episode { NEWHOPE EMPIRE JEDI }
enum LengthUnit { METER FOOT }

input SearchFilter {
  episode: Episode
  nameContains: String
  limit: Int = 10
}
"#;

/// Documents a server would actually see: valid ones, and ones that fail at the first thing a
/// server would notice.
const CORPUS: &[&str] = &[
  // A realistic request with variables, fragments, aliases, directives and input objects.
  r#"query HeroComparison($ep: Episode, $withFriends: Boolean!, $filter: SearchFilter) {
       left: hero(episode: $ep) { ...characterFields }
       right: hero { ...characterFields }
       results: search(text: "r2", filter: $filter) {
         ... on Droid { primaryFunction }
         ... on Human { homePlanet height(unit: FOOT) }
       }
     }
     fragment characterFields on Character {
       id
       name
       appearsIn
       friends @include(if: $withFriends) { name }
     }"#,
  // The smallest useful request.
  "{ hero { name } }",
  // Introspection, which is an ordinary document against the injected meta-schema.
  "{ __schema { queryType { name } types { name kind } } }",
  // A deeply reused fragment graph, so the reachability and visited bitsets are exercised.
  "query Deep { hero { ...a } }
   fragment a on Character { name ...b ...c }
   fragment b on Character { id friends { ...c } }
   fragment c on Character { appearsIn }",
  // Invalid, and invalid early: an unknown field on the first selection.
  "{ heroo { name } }",
  // Invalid deeper in, past the fragment machinery.
  "query Bad($ep: Episode) { hero(episode: $ep) { ...frag } }
   fragment frag on Character { name midichlorians }",
  // Invalid in a value literal, so the value walk is on the measured path.
  r#"{ search(text: 1, filter: { limit: "ten", nope: true }) { __typename } }"#,
  // Draft 5.3.2's engine, which has a working set of its own and is the only rule that does.
  // Repeated response names give it groups to compare, an abstract parent beside a concrete one
  // gives it a partition to build, and identical arguments send it back to the syntax tree to
  // compare two value literals — the three places it could have allocated.
  r#"query Merging($ep: Episode) {
       hero(episode: $ep) { name }
       hero(episode: $ep) { id friends { name } }
       search(text: "r2", filter: { episode: JEDI, limit: 10 }) {
         ... on Droid { name primaryFunction }
         ... on Human { name homePlanet }
       }
       again: search(text: "r2", filter: { episode: JEDI, limit: 10 }) { __typename }
     }"#,
  // The same, refused: two selections behind one response name that cannot merge, so the
  // diagnostic path is measured too.
  "{ hero { conflict: name } hero { conflict: appearsIn } }",
];

fn parse_sdl(sdl: &str) -> TypeSystemDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .expect("the corpus SDL parses")
}

fn build() -> Schema {
  let document = parse_sdl(SCHEMA);
  Schema::build(&document).expect("the corpus SDL is a schema")
}

fn parse(source: &'static str) -> ExecutableDocument<&'static str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(source)
  .unwrap_or_else(|errors| panic!("corpus query does not parse: {errors:?}\n---\n{source}"))
}

// ---------------------------------------------------------------------------------------------
// the gate
// ---------------------------------------------------------------------------------------------

/// The measurement discriminates: an allocation inside the window is seen.
///
/// Without this, a zero reading below would be indistinguishable from an allocator that was never
/// installed.
#[test]
fn the_gate_counts() {
  // Warm the thread-local itself, so its own setup is not what is being measured.
  let _ = allocations(|| {});
  let counted = allocations(|| {
    let buffer: Vec<u8> = Vec::with_capacity(4096);
    std::hint::black_box(&buffer);
  });
  assert!(
    counted >= 1,
    "the counting allocator saw nothing; the gate below would pass vacuously"
  );
}

/// The smallest document that is a schema, so the reading is the builder's own cost.
const MINIMAL: &str = "type Query { ok: Int }";

/// What a warm `Schema::build` of [`MINIMAL`] may allocate.
///
/// Measured at 215 and pinned with slack, not fitted to a projection. The slack is deliberately
/// smaller than one eager path per element: `Schema::build` merges roughly a hundred and thirty
/// built-in items into that one user type, so a single `to_owned()` or `owner_path(…)` put back on
/// a success path in `resolve_type_refs` or `validate_directive_usages` — the pattern this number
/// exists to keep out — costs about that many and crosses this line at once.
#[cfg(feature = "std")]
const BUILD_CEILING: u64 = 240;

/// The same reading where there is no `OnceLock` to cache the built-in parse in.
///
/// `SchemaBuilder::built_ins` documents the split: without `std` every build re-parses the three
/// constant SDL documents, which is 48 allocations the cached path does not pay. Measured at 263.
///
/// Pinned rather than skipped, because a gate that disappears under a feature selection is a gate
/// that stops noticing — and because this is the arm no `cargo test` currently reaches: `smear`'s
/// dev-dependency on itself does not say `default-features = false`, so every test build resolves
/// `std` on whatever the command line asks for. Reading this number needs the `cfg` inverted by
/// hand, or that manifest edge narrowed.
#[cfg(not(feature = "std"))]
const BUILD_CEILING: u64 = 288;

/// A warm `Schema::build` allocates for the schema it returns, not for diagnostics it does not
/// emit.
///
/// # What this pins
///
/// The defect class this number exists against is an owner path, a copied name or a deep-cloned
/// argument list built for **every** type, field, argument, input field and enum value the builder
/// merges — the ninety-odd built-in and introspection items included — so that an error arm which
/// almost never runs has a `String` to hand. It reaches 917 allocations for a twenty-two byte
/// document, and it is invisible to every other gate: same diagnostics, same verdicts, only the
/// clock and the allocator notice.
///
/// The reading is taken on the **second** build so the once-per-process parse of the built-in SDL
/// is not in it; that parse is the first build's cost and is not what this gate is about.
#[test]
fn a_warm_schema_build_allocates_only_for_the_schema() {
  let document = parse_sdl(MINIMAL);
  // The first build settles the process-wide built-in parse; the second is the steady state.
  let warm = Schema::build(&document).expect("the minimal SDL is a schema");
  std::hint::black_box(&warm);
  let _ = allocations(|| {});

  let counted = allocations(|| {
    let schema = Schema::build(&document).expect("the minimal SDL is a schema");
    std::hint::black_box(&schema);
  });
  assert!(
    counted <= BUILD_CEILING,
    "`Schema::build` allocated {counted} times for `{MINIMAL}`, over the {BUILD_CEILING} ceiling; \
     if that is an intended cost, say why and move the ceiling"
  );
}

/// Steady-state validation allocates nothing, for every sink that is not asked to grow storage.
#[test]
fn steady_state_validation_allocates_nothing() {
  let schema = build();
  let budget = Budget::default();
  let documents: Vec<_> = CORPUS
    .iter()
    .map(|source| (*source, parse(source)))
    .collect();

  let mut scratch = Scratch::new();
  let mut collected: Vec<Diagnostic<&'static str>> = Vec::with_capacity(64);

  // The corpus reaches draft 5.3.2's diagnostic path, not only its happy one — otherwise the
  // reading below would be about a rule that never fired.
  {
    let (_, document) = documents.last().expect("a corpus");
    let mut seen = Vec::new();
    let mut sink = Collect::new(&mut seen);
    let _ = validate_executable(&schema, document, &mut scratch, &budget, &mut sink);
    assert!(
      seen
        .iter()
        .any(|d| d.rule() == smear::validator::Rule::FieldSelectionMerging),
      "the merge engine's diagnostic path is not on the measured corpus: {:?}",
      seen.iter().map(Diagnostic::rule).collect::<Vec<_>>()
    );
  }

  // One warm pass over the whole corpus: this is where the working set and the caller's sink
  // reach the sizes the steady state reuses.
  for (_, document) in &documents {
    let mut first = First::new();
    let _ = validate_executable(&schema, document, &mut scratch, &budget, &mut first);
    collected.clear();
    let mut collect = Collect::new(&mut collected);
    let _ = validate_executable(&schema, document, &mut scratch, &budget, &mut collect);
    let _ = validate_executable(&schema, document, &mut scratch, &budget, &mut Count::new());
    let _ = validate_executable(&schema, document, &mut scratch, &budget, &mut Ignore);
  }
  let _ = allocations(|| {});

  // Ten rounds, so a "first call after the warm-up" fluke cannot pass.
  for _ in 0..10 {
    for (source, document) in &documents {
      let mut first = First::new();
      let counted = allocations(|| {
        let _ = validate_executable(&schema, document, &mut scratch, &budget, &mut first);
      });
      assert_eq!(counted, 0, "`First` allocated {counted} times on\n{source}");

      collected.clear();
      let counted = allocations(|| {
        let mut collect = Collect::new(&mut collected);
        let _ = validate_executable(&schema, document, &mut scratch, &budget, &mut collect);
      });
      assert_eq!(
        counted, 0,
        "a pre-sized `Collect` allocated {counted} times on\n{source}"
      );

      let counted = allocations(|| {
        let _ = validate_executable(&schema, document, &mut scratch, &budget, &mut Count::new());
      });
      assert_eq!(counted, 0, "`Count` allocated {counted} times on\n{source}");

      let counted = allocations(|| {
        let _ = validate_executable(&schema, document, &mut scratch, &budget, &mut Ignore);
      });
      assert_eq!(
        counted, 0,
        "`Ignore` allocated {counted} times on\n{source}"
      );
    }
  }
}

/// The claim survives a fresh `Scratch` meeting the corpus in a different order.
///
/// The high-water mark is what the warm-up establishes, so the largest document must be the one
/// that sets it — whichever position it is in.
#[test]
fn the_order_of_the_warm_up_does_not_matter() {
  let schema = build();
  let budget = Budget::default();
  let mut documents: Vec<_> = CORPUS
    .iter()
    .map(|source| (*source, parse(source)))
    .collect();
  documents.reverse();

  let mut scratch = Scratch::new();
  for (_, document) in &documents {
    let _ = validate_executable(&schema, document, &mut scratch, &budget, &mut Ignore);
  }
  let _ = allocations(|| {});

  for (source, document) in &documents {
    let counted = allocations(|| {
      let _ = validate_executable(&schema, document, &mut scratch, &budget, &mut Ignore);
    });
    assert_eq!(counted, 0, "allocated {counted} times on\n{source}");
  }
}

/// Rendering a diagnostic into a caller's buffer is the only thing that allocates, and it is the
/// caller's decision.
///
/// The point of the split: the validation path formats nothing, so a server that only needs a
/// verdict never pays for a message it will not read.
#[test]
fn only_rendering_allocates() {
  use core::fmt::Write as _;

  let schema = build();
  let budget = Budget::default();
  let document = parse("{ heroo { name } }");
  let mut scratch = Scratch::new();

  let mut first = First::new();
  let _ = validate_executable(&schema, &document, &mut scratch, &budget, &mut first);
  let mut rendered = std::string::String::with_capacity(256);
  let _ = write!(
    rendered,
    "{}",
    first.get().expect("a diagnostic").display(&schema)
  );
  rendered.clear();
  let _ = allocations(|| {});

  let mut first = First::new();
  let validating = allocations(|| {
    let _ = validate_executable(&schema, &document, &mut scratch, &budget, &mut first);
  });
  assert_eq!(validating, 0);

  // Into a pre-sized buffer, even rendering allocates nothing.
  let diagnostic = *first.get().expect("a diagnostic");
  let rendering = allocations(|| {
    let _ = write!(rendered, "{}", diagnostic.display(&schema));
  });
  assert_eq!(rendering, 0, "rendering into a pre-sized buffer allocated");
  assert!(rendered.contains("5.3.1"));

  // And into a fresh `String`, it does — which is the discrimination for this test.
  let rendering = allocations(|| {
    let owned = diagnostic.display(&schema).to_string();
    std::hint::black_box(&owned);
  });
  assert!(rendering >= 1, "`to_string` should allocate");

  // -------------------------------------------------------------------------------------------
  // the same claim for the whole diagnostic contract, read through `&dyn Diagnose`
  // -------------------------------------------------------------------------------------------
  //
  // The claim `smear::diagnostic` makes is stronger than "rendering is the caller's decision": it
  // is that a renderer can read a diagnostic's ENTIRE structure — code, severity, primary,
  // primary label, every secondary label, every response-path segment, help — and write the
  // message out, without touching the allocator. That is what makes `&dyn Diagnose` usable in a
  // request path at all, and it holds only because every vocabulary type is `Copy` and every
  // label and help string is `&'static`.
  //
  // Three things about the measurement below, each load-bearing:
  //
  //   * **No warm-up.** Nothing here is read before the window opens. A warm-up pass is what the
  //     validation gates above need, because the caller's buffers have a high-water mark to
  //     reach; the contract has no working set to fill, so needing one would itself be the
  //     finding.
  //   * **Erased.** Every subject goes in as `&dyn Diagnose`, because a concrete call could be
  //     inlined into nothing and prove less than it appears to. Vtable dispatch is what a server
  //     rendering three families in one pass actually executes.
  //   * **The gate discriminates.** `the_gate_counts` at the top of this file is what says a zero
  //     here means "nothing allocated" rather than "nothing was looking", and the `to_string`
  //     assertion immediately above shows the same counter reporting a real allocation a few
  //     lines earlier in this very test.
  let duplicate = parse_sdl("type Query { ok: Int } type Query { ok: Int }");
  let refusals = Schema::build(&duplicate).expect_err("a duplicate type name is a refusal");
  let refusal = refusals.errors().first().expect("a refusal");
  let view = diagnostic.display(&schema);

  #[cfg(feature = "introspection")]
  let response = {
    let refusal = Schema::from_introspection("{ not a response").expect_err("not JSON");
    let smear::validator::IntrospectionError::Response(error) = refusal else {
      panic!("expected a shape refusal");
    };
    error
  };

  // Assembled outside the window: the storage is the harness's, not the contract's.
  let mut contract: Vec<&dyn Diagnose> = Vec::with_capacity(4);
  contract.push(refusal);
  contract.push(&view);
  #[cfg(feature = "introspection")]
  contract.push(&response);
  let mut sink = StackBuffer::<512>::new();

  let counted = allocations(|| {
    for subject in &contract {
      read_the_whole_contract(*subject, &mut sink);
    }
  });
  assert_eq!(
    counted,
    0,
    "reading {} diagnostics through `&dyn Diagnose` and rendering them into a stack buffer \
     allocated {counted} times",
    contract.len()
  );
  assert!(sink.len() > 0, "the last message rendered to nothing");
}

/// A `core::fmt::Write` sink over a fixed stack buffer.
///
/// A `String` would allocate once and then never again, which is the wrong instrument: it would
/// make a contract accessor that allocated on every call indistinguishable from one that did not,
/// as long as the buffer was warm. This one cannot allocate at all, so anything the counter sees
/// came from the contract.
struct StackBuffer<const N: usize> {
  bytes: [u8; N],
  len: usize,
}

impl<const N: usize> StackBuffer<N> {
  const fn new() -> Self {
    Self {
      bytes: [0; N],
      len: 0,
    }
  }

  const fn len(&self) -> usize {
    self.len
  }

  const fn clear(&mut self) {
    self.len = 0;
  }
}

impl<const N: usize> core::fmt::Write for StackBuffer<N> {
  fn write_str(&mut self, text: &str) -> core::fmt::Result {
    let end = self.len + text.len();
    if end > N {
      return Err(core::fmt::Error);
    }
    self.bytes[self.len..end].copy_from_slice(text.as_bytes());
    self.len = end;
    Ok(())
  }
}

/// Reads every method of the contract, including the indices one past each collection, and writes
/// the message out.
///
/// Exhaustive by hand rather than by macro: a method added to `Diagnose` and not read here would
/// leave the claim covering less than it says, and there is nothing that could notice.
fn read_the_whole_contract(subject: &dyn Diagnose, sink: &mut StackBuffer<512>) {
  use core::{fmt::Write as _, hint::black_box};

  black_box(subject.code());
  black_box(subject.severity());
  black_box(subject.primary());
  black_box(subject.primary_label());
  black_box(subject.help());

  let labels = subject.labels();
  black_box(labels);
  for index in 0..=labels {
    black_box(subject.label(index));
  }
  let segments = subject.path_segments();
  black_box(segments);
  for index in 0..=segments {
    black_box(subject.path_segment(index));
  }

  // The provided iterators too: they are the door most renderers will actually use, and one that
  // allocated would leave the indexed measurement above technically true and practically wrong.
  for label in subject.labels_iter() {
    black_box(label);
  }
  for segment in subject.path_segments_iter() {
    black_box(segment);
  }

  sink.clear();
  write!(sink, "{subject}").expect("the message fits the buffer");
  black_box(sink.len());
}

/// The lossless door's whole-root check allocates nothing, and the red tree is what it used to be.
///
/// # The gate that was not looking
///
/// Everything above measures `validate_executable` — the syntactic door. The **lossless** door was
/// never in this file's population, which is why `verify_parse` could spend one rowan cursor
/// allocation per element on every call for a round without any gate noticing: same diagnostics,
/// same verdicts, only the allocator sees it. That is the shape
/// `a_warm_schema_build_allocates_only_for_the_schema` exists against, one door over.
///
/// The subject is `verify_parse` itself rather than the door, because the door legitimately
/// allocates — it projects an AST — while this helper is public, holds no budget, and must not.
///
/// # What the zero means now that the walk is not a recursion
///
/// The comparison holds an explicit stack of borrowed child iterators rather than a native frame
/// per level, and the first `Descent::INLINE` of those live in its own frame. So the reading below
/// is *no allocation up to that many branching levels*, and not *no allocation at any shape*: a
/// tree nested deeper than the inline capacity spills into a `Vec` and this fixture would read one.
/// The fixture's own branching nesting is three and the repository's deepest corpus green tree is
/// twelve levels of any kind, so what the zero covers is every tree either of them contains. The
/// per-element cost this exists against — one allocation per element walked past — is what the
/// discrimination below still reads off the red tree.
#[cfg(all(feature = "graphql", feature = "rowan"))]
#[test]
fn the_whole_root_check_allocates_nothing() {
  use smear::parser::graphql::lossless::{parse_executable_document, verify_parse};

  // Token-dense on purpose: the cost is one cursor per element the comparison walks past, so the
  // reading has to be taken where there are many elements.
  let mut source = String::from("{ hero {");
  for index in 0..2_000 {
    source.push_str(&std::format!(" f{index}: name"));
  }
  source.push_str(" } }");
  let parse = parse_executable_document(&source);

  let _ = allocations(|| {});
  let checked = allocations(|| {
    assert!(std::hint::black_box(verify_parse(&parse, &source)).is_ok());
  });
  assert_eq!(
    checked, 0,
    "the whole-root check allocated {checked} times for a comparison over borrowed green nodes"
  );

  // Discrimination, and the defect itself: the same answer read off the **red** tree, which is what
  // `parse.syntax().text() == source` does — materialise the root cursor, then allocate and drop a
  // node's worth of cursor data for each element the walk passes. A zero above is a reading rather
  // than a blind gate because this is not zero.
  let red = allocations(|| {
    assert!(std::hint::black_box(
      parse.syntax().text() == source.as_str()
    ));
  });
  println!("whole-root check: green {checked} allocations, red {red}");
  assert!(
    red > 2_000,
    "the red-cursor comparison allocated only {red} times, so it is not the contrast this claims"
  );
}
