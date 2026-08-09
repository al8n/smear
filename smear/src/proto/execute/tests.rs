//! What draft §6.4.4's discard does to the slots, rather than to the response.
//!
//! `smear/tests/proto_execute.rs` watches the same behaviour from outside, with a `Drop` counter,
//! and that is where the released-values cases belong. One property is not visible from there:
//! that a discard above an already-drained subtree *stops* at it instead of walking back in. A
//! second walk over a drained subtree releases nothing, changes no state and alters no response, so
//! no counter and no assertion on the response can tell the two apart — only a slot the walk would
//! have overwritten can, and reaching one means reaching into the executor.

use crate::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  proto::{Leaf, Node, Values},
  validator::Schema,
};

// `proto` implies `validator` and neither implies `std`, so this module also compiles under
// `--no-default-features --features proto`, where `crate::std` is `alloc` and `ToString` is not in
// the prelude. The crate's other in-crate test modules that render a value spell it the same way.
use std::string::ToString;

use core::num::NonZeroU32;

use super::{Executor, Limits, State};

const SDL: &str = r#"
type Query {
  nest: Wrap
}
type Wrap {
  boom: String!
  bulk: [Cell]
}
type Cell {
  text: String
  boom: String!
}
"#;

/// How many cells the list has: one that is discarded on its own, and two the discard above it
/// still has to reach.
const CELLS: usize = 3;

/// The smallest value space that can build a subtree.
#[derive(Debug)]
enum Value {
  Obj,
  List(usize),
  Text,
}

struct Space;

impl Values for Space {
  type Value = Value;

  fn is_null(&self, _: &Value) -> bool {
    false
  }

  fn as_bool(&self, _: &Value) -> Option<bool> {
    None
  }

  fn list_len(&self, value: &Value) -> Option<usize> {
    match value {
      Value::List(len) => Some(*len),
      _ => None,
    }
  }

  fn list_item(&mut self, _: &Value, _: usize) -> Value {
    Value::Obj
  }

  fn type_name<'a>(&'a self, _: &'a Value) -> Option<&'a str> {
    None
  }

  fn coerce_leaf(&mut self, value: Value, _: Leaf<'_>) -> Option<Value> {
    Some(value)
  }

  fn variable(&mut self, _: &str) -> Option<Value> {
    None
  }
}

fn compile(query: &str) -> (Schema, ExecutableDocument<&str>) {
  compile_against(SDL, query)
}

fn compile_against<'q>(sdl: &str, query: &'q str) -> (Schema, ExecutableDocument<&'q str>) {
  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("the SDL is a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(query)
  .expect("the query parses");
  (schema, document)
}

/// A discard above an already-drained subtree does not walk back into it.
///
/// This is the property the amortised bound in [`Executor::discard`] rests on: without it, one
/// error per cell over a list of a million would re-walk the cells already discarded and the total
/// would be quadratic in the response rather than linear.
///
/// A value planted in the drained subtree is how the walk is caught. Every slot `discard` reaches
/// is overwritten with [`State::Null`], so a slot that still holds what the test put there after
/// the second discard is a slot the second discard did not reach — an assertion on *absence of
/// work*, which nothing observable from outside the crate can make.
#[test]
fn a_drained_subtree_is_not_walked_again() {
  let (schema, document) = compile("{ nest { bulk { text boom } boom } }");
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, Value::Obj);
  let bulk = executor.poll_resolve(&mut space).expect("bulk").id();
  executor.handle_resolved(&mut space, bulk, Value::List(CELLS));
  let outer = executor.poll_resolve(&mut space).expect("nest.boom").id();

  // The first cell completes its `text` and then fails its `String!`, which discards the cell and
  // stops there: an element of `[Cell]` is a nullable position.
  let text = executor.poll_resolve(&mut space).expect("text").id();
  executor.handle_resolved(&mut space, text, Value::Text);
  let inner = executor
    .poll_resolve(&mut space)
    .expect("the cell's boom")
    .id();
  executor.handle_field_error(inner, "boom");

  let root = 0;
  let nest_slot = executor.slots[root].first_child;
  let bulk_slot = executor.slots[nest_slot as usize].first_child;
  let drained = executor.slots[bulk_slot as usize].first_child;
  let planted = executor.slots[drained as usize].first_child;
  assert!(
    executor.slots[drained as usize].discarded,
    "the failing cell is discarded"
  );
  assert!(
    executor.slots[planted as usize].discarded,
    "and so is every slot under it, which is what a later discard stops at"
  );
  assert!(
    matches!(executor.slots[planted as usize].state, State::Null),
    "the cell's own discard emptied it"
  );

  // The remaining cells complete normally, and then `nest` is nulled over the top of the drained
  // one.
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    executor.handle_resolved(&mut space, id, Value::Text);
  }
  executor.slots[planted as usize].state = State::Leaf(Value::Text);
  executor.handle_field_error(outer, "boom");

  assert!(
    matches!(executor.slots[planted as usize].state, State::Leaf(_)),
    "nulling `nest` walked over the drained cell and not through it"
  );
  let live = executor.slots[drained as usize].next_sibling;
  assert!(
    matches!(executor.slots[live as usize].state, State::Null),
    "while the cells that still held values were emptied"
  );

  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 2);
  let Node::Object(mut fields) = response.data() else {
    panic!("the root is an object")
  };
  let (key, value) = fields.next().expect("`nest`");
  assert_eq!(key.to_string(), "nest");
  assert!(
    matches!(value, Node::Null),
    "and the planted value is not readable, because `nest` answers null above it"
  );
}

/// The collection scratch cannot grow past what the ceiling that refuses the request permits.
///
/// # What was wrong, and why it needed a test from inside
///
/// Collection stages every surviving selection into `scratch_fields` before `expand` checks
/// `max_response_metadata`, so the buffer used to grow under the **visit** budget — the loosest
/// ceiling on the path — while the operation was refused by the metadata budget. `reset` reuses the
/// scratch rather than shrinking it, so one wide refused request left that capacity resident on a
/// long-lived executor, and a wide enough one could exhaust memory before producing the very budget
/// error that was going to refuse it.
///
/// Capacity is not observable through any public API — a leak and a limit produce the same response
/// — so this is the same reason `a_drained_subtree_is_not_walked_again` lives in here.
///
/// # The numbers, and which one makes it fire
///
/// `max_response_metadata` is **8**, so the root's collection may stage four selections: each
/// becomes two metadata entries when committed, and `(4 + 1) * 2 > 8` refuses the fifth. The query
/// asks for **4096** aliases of one field, all valid.
///
/// **4096 against a bound of 64 is what makes this discriminate.** Before the charge the buffer
/// reached the full width and its capacity with it, so the assertion fails by roughly sixty-four
/// times; after it, the buffer never holds more than four. A narrower query — anything under the
/// bound — would pass against the defect and prove nothing, which is the trap a sibling line hit
/// tonight with a fixture that was wide but not wide enough.
#[test]
fn collection_scratch_cannot_outgrow_the_ceiling_that_refuses_it() {
  const WIDTH: usize = 4096;
  const CAPACITY_BOUND: usize = 64;

  let mut query = std::string::String::from("{");
  for i in 0..WIDTH {
    query.push_str(&std::format!(" k{i}: nest {{ boom }}"));
  }
  query.push_str(" }");

  let (schema, document) = compile(&query);
  let limits = Limits {
    max_response_metadata: NonZeroU32::new(8).expect("eight is not zero"),
    ..Limits::default()
  };
  let mut space = Space;
  let mut executor = Executor::with_limits(&schema, &document, limits);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");

  // Scoped, so the borrow ends before the scratch is read — `Response` borrows the executor.
  let errors = {
    let response = executor.poll_response().expect("nothing is outstanding");
    response.error_count()
  };
  assert_eq!(
    errors, 1,
    "the root's selection set is refused for metadata, which is the point of the fixture"
  );

  assert!(
    executor.scratch.fields.capacity() <= CAPACITY_BOUND,
    "the staging buffer grew to {} entries against a ceiling that admits four; before the charge \
     it followed the {WIDTH}-wide selection set instead of the ceiling that refused it",
    executor.scratch.fields.capacity()
  );
  assert!(
    executor.scratch.groups_capacity() <= CAPACITY_BOUND,
    "groups grows at most one per selection, so bounding the selections bounds it — {} says \
     otherwise",
    executor.scratch.groups_capacity()
  );
}

// ------------------------------------------------------------------------------------------
// Collection costs what it charges, and it charges what it costs
// ------------------------------------------------------------------------------------------
//
// al8n/smear#141 and the flat fragment chain are one defect wearing three faces: the response
// key's interner entry, the key's group and a spread's fragment were each found by a linear scan,
// once per selection, so `n` selections over `n` names cost `n²` before the first field request
// existed. Measured on this tree beforehand: 8,000 distinct keys spent 61 ms inside `start()` and
// a 50,000-link chain spent 2.1 s.
//
// # Why the gate is two-sided, and why it lives in here
//
// A quadratic collection and a linear one return the same response, so nothing outside the crate
// can tell them apart except a clock. What *is* observable from inside is the visit budget, whose
// unit is now one per selection examined, one per entry a name lookup compares, and one per
// definition and fragment the index pass handles — so the counter is the work count, and the
// fixtures below pin it from both directions:
//
// - the **upper** bound is red if a lookup goes back to scanning, because a scan charges what it
//   compares and a scan compares the document;
// - the **lower** bound is red if a lookup or a table's *population* stops charging — including a
//   scan that was reintroduced *and* left uncharged, which is the regression an upper bound alone
//   cannot see, since removing the count is the cheapest way to satisfy it.
//
// # Charging a lookup is half the question; the other half is where the table came from
//
// The fragment table was filled at executor construction, from names the document chooses, outside
// the budget entirely — so a charged lookup bounded a run an adversary had built for free, and the
// bound was hollow. Three of the fixtures below are about the population rather than the lookup:
// that indexing is charged, that its cost does not depend on the names, and that a run is
// abandoned at the ceiling instead of after it.
//
// The one lookup no bound reaches is the group, which is a direct index with no loop to charge. No
// counter can see work that declines to count itself; that one is held by review and by the
// timings, not by a gate.

/// A schema with one field, so a document can be as wide or as deep as the fixture wants.
const ONE_FIELD: &str = "type Query { a: String }";

/// Collects `query`'s root selection set and returns what it charged the visit budget.
fn collection_work(sdl: &str, query: &str) -> u32 {
  let (schema, document) = compile_against(sdl, query);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");
  executor.collection_work()
}

/// `n` repeats of one response key charge exactly `2n - 1`, and the second term is the interner.
///
/// The exact-value case, and it is exact rather than bounded because nothing about it depends on
/// the hash: one key means one bucket with one entry, so the first selection interns after
/// comparing nothing and every later one matches on its first comparison. `n` selections examined
/// plus `n - 1` comparisons is the whole of it.
///
/// **This is the plant against an uncharged lookup.** Deleting the charge leaves `n`, and nothing
/// about the response changes.
#[test]
fn a_repeated_response_key_charges_one_comparison_each_time() {
  const REPEATS: u32 = 1024;

  let mut query = std::string::String::from("{");
  for _ in 0..REPEATS {
    query.push_str(" a");
  }
  query.push_str(" }");

  assert_eq!(
    collection_work(ONE_FIELD, &query),
    2 * REPEATS - 1,
    "{REPEATS} selections examined, and {} comparisons to find the one interned key each time \
     after the first; a smaller total means a name lookup is not charging what it compares",
    REPEATS - 1
  );
}

/// `n` *distinct* response keys stay linear, which is the whole of al8n/smear#141.
///
/// Distinct on purpose, and a fixture edited to repeat a key destroys the case. A repeated key
/// interns once and then finds a one-entry table, so it measures like a fix whether or not
/// anything was fixed — against all three scans at once.
#[test]
fn distinct_response_keys_are_linear() {
  const KEYS: u32 = 4096;

  let mut query = std::string::String::from("{");
  for i in 0..KEYS {
    query.push_str(&std::format!(" k{i}: a"));
  }
  query.push_str(" }");

  let work = collection_work(ONE_FIELD, &query);
  assert!(
    work > KEYS,
    "a successful or failing probe still compares something, so {KEYS} keys cannot cost only \
     {work}; that total says the interner lookup is not charged"
  );
  assert!(
    work <= 3 * KEYS,
    "{work} units for {KEYS} distinct keys. Scanning the names instead of probing them costs \
     about {} — the ceiling here is a linear multiple, so a quadratic lookup misses it by three \
     orders of magnitude",
    u64::from(KEYS) * u64::from(KEYS) / 2
  );
}

/// A flat fragment chain stays linear, which is the other half of the same defect.
///
/// The chain's *depth* is already pinned by `a_flat_fragment_chain_no_longer_ends_the_process` in
/// `smear/tests/proto_execute.rs`, which is the regression for the abort. This is its *cost*: the
/// walk stopped spending native frames but still scanned every definition in the document once per
/// spread, so a chain that no longer killed the process still took quadratic time to answer.
///
/// Four terms, all linear in the chain: the index pass over the definitions, one push per fragment,
/// one visit per selection, and about one comparison per spread.
#[test]
fn a_flat_fragment_chain_is_linear() {
  const LINKS: u32 = 4096;

  let work = collection_work(ONE_FIELD, &fragment_chain(LINKS));
  assert!(
    work >= 3 * LINKS,
    "the index pass alone is a definition and a fragment each, and every spread then compares at \
     least the entry it returns; {work} units for {LINKS} links is short of that, which is what an \
     unindexed table or an uncharged one reads as"
  );
  assert!(
    work <= 6 * LINKS,
    "{work} units for a {LINKS}-link chain. Scanning the definitions per spread costs about {} \
     instead",
    u64::from(LINKS) * u64::from(LINKS) / 2
  );
}

/// A second operation on one executor charges exactly what the first charged.
///
/// # Why this is the general form of the escape, and the response-level fixture is not
///
/// `a_refused_request_is_refused_again_on_the_same_executor` in `tests/proto_execute.rs` is the
/// regression for the defect that was found: the fragment index survived `reset` and so did the
/// charge for building it, so the second `start` skipped the pass and a refused request was served.
/// That fixture compares *answers*, which means it can only fire when the budget happens to sit
/// between the two totals — it is tuned to one discrepancy, and it had to be, because the answer is
/// all a client can see.
///
/// This compares the totals. A charge that moves between operations turns it red whether or not any
/// ceiling is close enough for a caller to notice today, which is the difference between pinning
/// the bug that was found and pinning the class it belongs to: everything an operation pays for has
/// to be paid again by the next one, and the tables that *survive* `reset` are exactly where that
/// stops being automatic. Phases 2–8 will add more of them.
///
/// # It reads every cumulative ceiling, because reading one of them was a claim about four
///
/// The first version of this compared `collection_work()` — `max_selection_visits`, and nothing
/// else — under a residual that claimed the whole class. Three ceilings were outside it:
/// `max_response_slots`, `max_response_metadata` and `max_interned_bytes`. A structure kept across
/// `reset` that let a second operation reuse *positions*, *metadata* or *arena bytes* it had not
/// paid for was a defect of exactly the shape this fixture is named after, and every gate in the
/// suite was green on it. Comparing one number and asserting a property over four is the residual
/// form this program has lost the most rounds to.
///
/// So the comparison is `Charges`, which is one field per ceiling that accumulates, each read as
/// the quantity that ceiling's own check tests. `max_in_flight` is the one that is absent, and
/// `collect::Visits` says why and what else is left to review.
///
/// # Every row has to be able to move
///
/// A row whose value is zero in both runs cannot fail, so widening the comparison would otherwise
/// buy three assertions that always hold. The lower bounds below are what keep them live, and they
/// are also the fixture's maintenance obligation: this document is what decides which populations
/// the gate reaches, and a structure phases 2–8 add that no selection here exercises needs a
/// selection adding, or a gate of its own.
///
/// The document exercises the index pass over the definitions, a fragment lookup per spread, and
/// both a distinct and a repeated response key per selection — so every position, every metadata
/// entry and every arena byte the two runs compare is one this query asked for.
#[test]
fn a_second_operation_charges_what_the_first_did() {
  const LINKS: u32 = 64;
  const KEYS: u32 = 64;

  let mut query = std::string::String::from("{ ...F0 a a");
  for index in 0..KEYS {
    query.push_str(&std::format!(" k{index}: a"));
  }
  query.push_str(" }\n");
  for index in 0..LINKS {
    query.push_str(&std::format!(
      "fragment F{index} on Query {{ ...F{} }}\n",
      index + 1
    ));
  }
  query.push_str(&std::format!("fragment F{LINKS} on Query {{ a }}\n"));

  let (schema, document) = compile_against(ONE_FIELD, &query);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);

  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");
  let first = executor.charges();
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves a second time");
  let second = executor.charges();

  assert!(
    first.visits > 3 * LINKS,
    "the fixture has to cost something: {first:?}"
  );
  assert!(
    first.slots > 0,
    "no position was created, so the slots row cannot tell two operations apart: {first:?}"
  );
  assert!(
    first.metadata > 0,
    "no metadata was committed, so that row cannot tell two operations apart: {first:?}"
  );
  assert!(
    first.interned > 0,
    "no name was interned, so the arena row cannot tell two operations apart: {first:?}"
  );
  assert_eq!(
    second, first,
    "the second operation charged {second:?} where the first charged {first:?}. Something this \
     executor kept from the first run is being reused by the second without being charged for, \
     which makes the ceiling a request meets depend on what the executor was asked before it"
  );
}

/// `{ ...F0 }` with `F0 → F1 → … → Fn`, every definition at nesting depth one.
fn fragment_chain(links: u32) -> std::string::String {
  let mut query = std::string::String::from("{ ...F0 }\n");
  for i in 0..links {
    query.push_str(&std::format!(
      "fragment F{i} on Query {{ ...F{} }}\n",
      i + 1
    ));
  }
  query.push_str(&std::format!("fragment F{links} on Query {{ a }}\n"));
  query
}

// ------------------------------------------------------------------------------------------
// The fragment table's population, which is where a charged lookup was resting on nothing
// ------------------------------------------------------------------------------------------

/// A refused index pass reserves no storage, which is a different property from refusing.
///
/// # The verdict is not what moves, so a fixture keyed on the verdict cannot see this
///
/// `Fragments::build` charges twice — the definitions, then the fragments — and a document can put
/// a ceiling between them. When the second charge is refused the answer is a refusal, on this call
/// and on every retry: `reset` rebuilds `Visits`, so the same document under the same limits pays
/// the same two amounts in the same order and hears the same no. Every assertion about the
/// *response* is therefore already satisfied by the defect, including
/// `a_refused_request_is_refused_again_on_the_same_executor`, which was written for the escape one
/// round earlier and passes over this one.
///
/// What moved was memory. `build` used to learn the fragment count by **populating** the table and
/// charging for what it had populated, clearing the vector when the charge was refused — and a
/// cleared vector keeps its capacity. The table is owned by the executor and kept across `reset` by
/// design, so a refused operation left a fragment-sized allocation that no ceiling had admitted,
/// no later operation could use, and no retry would free. An adversary could not change the answer;
/// it could make a refusal cost memory that outlived it.
///
/// So this reads capacity, and it reads it on both sides of the ceiling. The refused half is the
/// regression. **The served half is what keeps the refused half from being vacuous**: it says
/// `fragment_reserved()` is an observable that moves, so `== 0` is a statement about this run and
/// not about an accessor that always answers zero.
///
/// # The numbers
///
/// `fragment_chain(LINKS)` is `LINKS + 1` fragments in `LINKS + 2` definitions, spread from a root
/// selection set of one selection. The walk charges that selection, then the pass charges
/// `LINKS + 2` for the definitions and `LINKS + 1` for the fragments. A ceiling of `LINKS + 3`
/// admits the selection and the definitions **exactly**, leaving nothing for the population — which
/// is the one refusal this fixture is about. `spent()` is asserted to have reached the definitions
/// charge, so a fixture mis-tuned low enough to be refused at the *first* charge fails instead of
/// passing for the wrong reason.
#[test]
fn a_refused_index_pass_reserves_no_fragment_storage() {
  const LINKS: u32 = 256;
  /// One selection, then one unit per definition walked. The fragment charge is what will not fit.
  const UP_TO_THE_POPULATION: u32 = 1 + LINKS + 2;

  let query = fragment_chain(LINKS);
  let (schema, document) = compile_against(ONE_FIELD, &query);

  let mut space = Space;
  let mut refused = Executor::with_limits(
    &schema,
    &document,
    Limits {
      max_selection_visits: NonZeroU32::new(UP_TO_THE_POPULATION).expect("not zero"),
      ..Limits::default()
    },
  );
  refused
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");

  let spent = refused.collection_work();
  assert!(
    spent >= LINKS + 2,
    "the fixture only says anything if the definitions charge was accepted and the fragment charge \
     was the one refused; {spent} units spent is short of the {} the definitions cost, so this run \
     was refused before the population was ever priced",
    LINKS + 2
  );
  let errors = {
    let response = refused.poll_response().expect("nothing is outstanding");
    response.error_count()
  };
  assert_eq!(
    errors, 1,
    "and the request is refused, which is the state whose cost is under test"
  );
  assert_eq!(
    refused.fragment_reserved(),
    0,
    "the refused pass left room for {} fragment-table entries on an executor that keeps this table \
     across `reset`. The verdict is right and stays right — the retry is refused too — so nothing \
     about the response can show this: what a refusal costs is what moved, and it outlives every \
     retry",
    refused.fragment_reserved()
  );

  // The other side of the same ceiling, so that the zero above is a fact about a refusal rather
  // than about an accessor that cannot report anything else.
  let mut served = Executor::new(&schema, &document);
  served
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");
  assert!(
    served.fragment_reserved() >= (LINKS + 1) as usize,
    "an admitted pass indexes {} fragments, so it has to reserve at least that much; {} says the \
     capacity this fixture watches is not the capacity the pass allocates",
    LINKS + 1,
    served.fragment_reserved()
  );
}

/// How many fragments the colliding fixtures define.
///
/// Small because finding them costs about `COLLIDING × buckets` trial hashes, and the property
/// under test does not get truer with more of them: one bucket holding every name is the worst
/// case at any size.
const COLLIDING: usize = 512;

/// Definitions in a colliding fixture: its fragments, plus the operation that spreads one.
const COLLIDING_DEFINITIONS: u32 = COLLIDING as u32 + 1;

/// The index pass's charge for a colliding fixture: one per definition walked, one per fragment
/// pushed.
const COLLIDING_INDEX: u32 = COLLIDING_DEFINITIONS + COLLIDING as u32;

/// `COLLIDING` fragment names that the index puts in **one** bucket, in the order it will see them.
fn colliding_fragment_names() -> std::vec::Vec<std::string::String> {
  colliding_names("f", (COLLIDING.next_power_of_two() * 2 - 1) as u32)
}

/// `COLLIDING` names beginning `prefix` that all land in one bucket of a table masked by `mask`.
///
/// Derived from the index's own hash rather than hardcoded, so it stays a colliding set if the hash
/// changes. That is the point: a list of literal names would silently stop colliding and the
/// fixtures would go on passing against a table they no longer stress.
///
/// **A set colliding under `mask` collides under every smaller one**, since a narrower mask keeps a
/// subset of the same bits. That is what lets the interner fixture pick one `mask` and have it hold
/// through every rehash the table does on its way to that size.
fn colliding_names(prefix: &str, mask: u32) -> std::vec::Vec<std::string::String> {
  let mut by_bucket: std::vec::Vec<std::vec::Vec<std::string::String>> =
    std::vec::from_elem(std::vec::Vec::new(), mask as usize + 1);
  for candidate in 0u64.. {
    let name = std::format!("{prefix}{candidate}");
    let at =
      crate::validator::schema::bucket(crate::validator::schema::hash_bytes(name.as_bytes()), mask)
        as usize;
    by_bucket[at].push(name);
    if by_bucket[at].len() == COLLIDING {
      return core::mem::take(&mut by_bucket[at]);
    }
  }
  unreachable!("the search is over an unbounded range")
}

/// A document defining every name in `names` as a fragment, whose operation spreads `spread`.
fn colliding_document(names: &[std::string::String], spread: &str) -> std::string::String {
  let mut query = std::format!("{{ ...{spread} }}\n");
  for name in names {
    query.push_str(&std::format!("fragment {name} on Query {{ a }}\n"));
  }
  query
}

/// Indexing the document's fragments is charged, so a budget too small to hold it refuses.
///
/// **The plant for the finding this section exists for.** The table used to be filled in
/// `Executor::with_limits`, outside every ceiling, which meant a service building an executor per
/// execution paid `executions × definitions` unbudgeted — and, worse, that the charged *lookup*
/// below was bounding a probe run an adversary had been allowed to build for free.
///
/// Delete the charge in `Fragments::build` and the first half goes green while the document is
/// indexed for nothing, which is exactly the state this closed.
#[test]
fn indexing_the_documents_fragments_is_charged() {
  let names = colliding_fragment_names();
  // The last name defined is the head of the bucket, so the one spread below finds it on its first
  // comparison and the total is the index pass plus a constant.
  let head = names.last().expect("the set is not empty").clone();
  let query = colliding_document(&names, &head);
  let (schema, document) = compile_against(ONE_FIELD, &query);

  let refused = collected_under(&schema, &document, COLLIDING_INDEX - 1);
  assert!(
    refused.is_some(),
    "a budget one unit short of the index pass must refuse rather than index for free"
  );

  let served = collected_under(&schema, &document, COLLIDING_INDEX + 8);
  assert_eq!(
    served, None,
    "and eight units past it is enough for the spread, its one comparison and the field it reaches"
  );
}

/// The index pass costs the same whatever the names are, which is what chaining buys.
///
/// Open addressing made the *build* the sharper half of the same defect: inserting `n` colliding
/// names probes `n²/2` slots, in a constructor no ceiling watched. Chaining pushes at a bucket head
/// and never probes, so this total is exact and every term in it is a count of something the
/// document has — not of something the names did to each other.
#[test]
fn a_colliding_fragment_table_costs_one_unit_per_definition_and_fragment() {
  let names = colliding_fragment_names();
  let head = names.last().expect("the set is not empty").clone();
  let query = colliding_document(&names, &head);

  // The index pass, the root's one selection, the one comparison that finds the bucket head, and
  // the one field inside the fragment. Interning that field's key compares nothing: it is the first
  // name in an empty arena.
  let expected = COLLIDING_INDEX + 3;
  assert_eq!(
    collection_work(ONE_FIELD, &query),
    expected,
    "{COLLIDING} fragment names in one bucket must cost one unit each to index and no more; a \
     total above this is an insertion whose cost depends on the names"
  );
}

/// A probe run that runs out of budget stops where the budget did, not at the end of the bucket.
///
/// **The plant against charging after the fact.** The charge used to be taken once, for the whole
/// run, after the run had finished — so a single valid spread into the tail of a 512-entry bucket
/// compared all 512 and only then heard it had no budget. Charging before each comparison abandons
/// it at the ceiling.
///
/// The budget cannot show this: comparisons are charged one for one, so the charge and the count
/// agree by construction under either version. Only a count taken independently of the charge —
/// `Fragments::compares` — can separate them, which is what it exists for.
#[test]
fn a_refused_probe_run_stops_at_the_refusal() {
  /// Units left for probing once the index pass and the root's selection are paid for.
  const SLACK: u32 = 8;

  let names = colliding_fragment_names();
  // The *first* name defined sits at the tail of the bucket's chain, so finding it needs every
  // comparison the bucket can offer.
  let tail = names.first().expect("the set is not empty").clone();
  let query = colliding_document(&names, &tail);
  let (schema, document) = compile_against(ONE_FIELD, &query);

  let mut space = Space;
  let limits = Limits {
    max_selection_visits: NonZeroU32::new(COLLIDING_INDEX + 1 + SLACK).expect("not zero"),
    ..Limits::default()
  };
  let mut executor = Executor::with_limits(&schema, &document, limits);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");

  let compares = executor.fragment_compares();
  assert!(
    compares <= u64::from(SLACK),
    "the run compared {compares} entries against a budget with {SLACK} units left for it; a run \
     charged at its end walks the whole {COLLIDING}-entry bucket before the refusal arrives"
  );
  let errors = {
    let response = executor.poll_response().expect("nothing is outstanding");
    response.error_count()
  };
  assert_eq!(
    errors, 1,
    "and the request is refused, rather than served by a run nobody paid for"
  );
}

/// A name interned out of the *document* is charged like any other, so a colliding set of them
/// cannot outrun the budget.
///
/// **The regression for the residual that was wrong in its justification.** The uncharged interner
/// entry point was documented as being reached only by schema and driver bytes — a universal over
/// callers, and two callers falsified it: both spellings of "the variable this argument wanted"
/// come out of the executable document. A client that names its variables into one bucket, then
/// makes every sibling field fail draft §6.4.1 with a distinct one, walks that bucket once per
/// failure. None of it is collection, so `max_selection_visits` never saw it.
///
/// The repair is not a better sentence. `Interner::intern` takes `&mut Visits`, so there is no
/// uncharged path for a future caller to find — and this fixture is what says the charge is
/// actually taken on the path that was missed.
///
/// Charging every insertion's probe run bounds the chain as well as the walk: an `L`th name into a
/// bucket first walks the `L - 1` there, so the run this document can build is `√(2 · budget)`
/// rather than one per field.
#[test]
fn a_colliding_set_of_document_variables_cannot_outrun_the_budget() {
  /// Wide enough that the uncharged version's `n²/2` is two orders of magnitude past the ceiling.
  const BUDGET: u32 = 4096;

  // Masked for a table twice the size this document can grow — `COLLIDING` response keys plus
  // `COLLIDING` variable spellings — so the set still shares a bucket at the end.
  let names = colliding_names("v", (4 * COLLIDING.next_power_of_two() - 1) as u32);
  let mut query = std::string::String::from("query (");
  for name in &names {
    query.push_str(&std::format!("${name}: String! "));
  }
  query.push_str(") {");
  for (index, name) in names.iter().enumerate() {
    query.push_str(&std::format!(" k{index}: arg(to: ${name})"));
  }
  query.push_str(" }");

  let (schema, document) = compile_against("type Query { arg(to: String!): String }", &query);
  let mut space = Space;
  let limits = Limits {
    max_selection_visits: NonZeroU32::new(BUDGET).expect("not zero"),
    ..Limits::default()
  };
  let mut executor = Executor::with_limits(&schema, &document, limits);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");
  // No variable is supplied, so every field fails draft §6.4.1 step 5.f and interns the spelling it
  // could not find. `poll_resolve` moves to the next ready slot after each, which is what made this
  // a per-field cost rather than a one-off.
  while executor.poll_resolve(&mut space).is_some() {}

  let compares = executor.interner_compares();
  assert!(
    u64::from(BUDGET) >= compares,
    "the name table compared {compares} entries against a ceiling of {BUDGET}. Interning a \
     document-derived name without charging lets {COLLIDING} colliding spellings cost about {} \
     comparisons, none of them collection and none of them seen by any ceiling",
    COLLIDING * COLLIDING / 2
  );
}

/// Runs `document` under a chosen visit budget, returning the refusal message if there was one.
fn collected_under(
  schema: &Schema,
  document: &ExecutableDocument<&str>,
  visits: u32,
) -> Option<std::string::String> {
  let mut space = Space;
  let limits = Limits {
    max_selection_visits: NonZeroU32::new(visits).expect("not zero"),
    ..Limits::default()
  };
  let mut executor = Executor::with_limits(schema, document, limits);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");
  // Drained, because the served case reaches a field and offers it: a response is only available
  // once nothing is outstanding, and a refused collection simply offers nothing to drain.
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    executor.handle_resolved(&mut space, id, Value::Text);
  }
  let response = executor.poll_response().expect("nothing is outstanding");
  response.errors().next().map(|error| error.to_string())
}
