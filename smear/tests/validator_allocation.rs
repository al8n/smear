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

#![allow(missing_docs)]

use std::{
  alloc::{GlobalAlloc, Layout, System},
  cell::Cell,
  vec::Vec,
};

use smear::{
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
];

fn build() -> Schema {
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SCHEMA)
  .expect("the corpus SDL parses");
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
}
