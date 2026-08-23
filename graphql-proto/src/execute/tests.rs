//! What draft §6.4.4's discard does to the slots, rather than to the response.
//!
//! `smear/tests/proto_execute.rs` watches the same behaviour from outside, with a `Drop` counter,
//! and that is where the released-values cases belong. One property is not visible from there:
//! that a discard above an already-drained subtree *stops* at it instead of walking back in. A
//! second walk over a drained subtree releases nothing, changes no state and alters no response, so
//! no counter and no assertion on the response can tell the two apart — only a slot the walk would
//! have overwritten can, and reaching one means reaching into the executor.

use smear_parser::{
  graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  lexer::tokora::{Parse as _, Parser},
};
use smear_schema::Schema;

use crate::{Extensions, Leaf, Node, ReqId, ResponseStream, SourceEventError, StartError, Values};

// Neither this crate's `std` nor `smear-schema`'s is implied by anything, so this module also
// compiles under `--no-default-features`, where `crate::std` is `alloc` and `ToString` is not in
// the prelude. The crate's other in-crate test modules that render a value spell it the same way.
use std::string::ToString;

use core::num::NonZeroU32;

use super::{Executor, Limits, NONE, State, node};

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

/// `n` repeats of one response key charge an exact total, and every term of it is named.
///
/// Exact rather than bounded because nothing about it depends on the hash: one key means one bucket
/// with one entry, so the first selection interns after comparing nothing and every later one
/// matches on its first comparison.
///
/// The terms, and the arithmetic is written out rather than folded so a moved number says which one
/// moved. The key is `a`, one byte, so every [`byte_units`] below is `1`.
///
/// - draft §6.1's lookup over the document's one definition;
/// - one visit per selection examined;
/// - the **first** intern: one pass to hash the key, no entry to compare, one pass to copy it into
///   the arena;
/// - each **later** intern: one pass to hash, one entry compared, and one pass to `memcmp` it —
///   reached only because the stored hash and the length agreed, which on a repeat of the same key
///   they always do;
/// - `expand`'s probe of the field's own spelling, once for the one group the repeats collapse to.
///
/// **The plants.** Delete the entry charge and the `n - 1` comparisons go. Delete either
/// `take_bytes` and the total stops moving with the key's length — which
/// `distinct_response_keys_are_linear_however_they_are_spelled` measures across lengths and this
/// one pins at one.
#[test]
fn a_repeated_response_key_charges_one_comparison_each_time() {
  use crate::collect::byte_units;

  const REPEATS: u32 = 1024;
  /// The document is one shorthand operation, which is what draft §6.1's lookup reads.
  const LOOKUP: u32 = 1;
  /// `a`, the one response key and also the one field name `expand` probes.
  const KEY: usize = 1;

  let mut query = std::string::String::from("{");
  for _ in 0..REPEATS {
    query.push_str(" a");
  }
  query.push_str(" }");

  let pass = byte_units(KEY);
  let first_intern = 2 * pass;
  let later_intern = (REPEATS - 1) * (2 * pass + 1);
  let expand_probe = pass;

  assert_eq!(
    collection_work(ONE_FIELD, &query),
    LOOKUP + REPEATS + first_intern + later_intern + expand_probe,
    "one definition read by draft §6.1, {REPEATS} selections examined, {} comparisons to find the \
     one interned key each time after the first, and {pass} unit(s) for every pass any of them \
     makes over the key's bytes; a smaller total means a name lookup is not charging what it \
     compares, or is charging entries where the work is bytes",
    REPEATS - 1
  );
}

/// `n` *distinct* response keys stay linear **however they are spelled**: al8n/smear#141,
/// al8n/smear#172 and al8n/smear#196 together.
///
/// Distinct on purpose, and a fixture edited to repeat a key destroys the case. A repeated key
/// interns once and then finds a one-entry table, so it measures like a fix whether or not
/// anything was fixed — against all three scans at once.
///
/// # One spelling is not a case, and five of them are not either
///
/// The version al8n/smear#141 shipped named its keys `k0 … k4095` and bounded the total by
/// `3 * KEYS`. It passed for a reason with nothing to do with the property under test: `k{i}` is
/// the one ordinary spelling the unfinalized multiply-fold happened to scatter. Substituting
/// `field{i}` — same count, same shape — measured **169,785** against that same 12,288 ceiling.
/// Honest input, no collision search, red by 13.8×.
///
/// al8n/smear#172 replaced it with five *named* spellings and an absolute two-sided count, and the
/// second half of that was right: a **ratio** cannot do this job, because the quantity it would
/// divide by is the one that depends on the spelling, and a denominator that grows with the defect
/// hides it. Time ratios are worse still — five interleaved passes spread 35%, while these integer
/// counts reproduce exactly.
///
/// The first half was the same mistake one round later. Five exemplars are five points, and what
/// the hash does is **positional**: it turns on how many values a byte position takes and on where
/// a chunk boundary falls among the varying bytes. All five named rows sat on one side of that
/// plane, and 4,096 eight-digit base-36 aliases — `x00000000 … x0000035r`, a document a generator
/// writes — charged **11,943** units against this ceiling of 8,192, with base-63 at **18,401**.
/// Two of the five named rows were already emitting duplicate hashes (4,004 and 3,996 for 4,096
/// names) and stayed green because the duplicates were few. al8n/smear#196.
///
/// # So the rows are generated by crossing the two axes that decide it
///
/// Radix — 10, 16, 36 and 63, the alphabets a counter is written in — against width, chosen so the
/// varying bytes land before, on and after **both** chunk boundaries. Twenty-four rows for six
/// lines of code, and the coordinates of a failure name the mechanism instead of naming a fixture:
/// the rows that fail without the fold between rounds are exactly widths 8 and 16 at radices 36 and
/// 63, which is "one byte past a boundary" written down.
///
/// The five named spellings stay above them. `k{i}` is the row that stayed green under every plant
/// this gate has had, and deleting it would delete the evidence for why one row is not a gate.
///
/// # The plants, and they land on different rows
///
/// Delete the `finalize` call from `smear_schema::hash_bytes`: `field{i}` reads 12,388 and the four
/// width-6 rows read 44,687, 114,177, 360,209 and **776,036** — short names, where the fold between
/// rounds cannot help because for eight bytes or fewer its loop body never runs. Delete
/// `h ^= h >> 32` instead: the width-8 and width-16 rows at radices 36 and 63 read 11,943, 11,766,
/// 18,401 and 17,568, and every width-6 row stays green. Each defect leaves the other one's rows
/// passing, which is the whole reason both axes are here.
#[test]
fn distinct_response_keys_are_linear_however_they_are_spelled() {
  const KEYS: u32 = 4096;

  /// The **comparisons**, over and above every term of the total that is not one.
  ///
  /// That subtraction is al8n/smear#172's doing and it makes the gate sharper, not looser. The
  /// ledger now also charges a pass over every key's bytes — hashing it and copying it into the
  /// arena — and those terms grow with the *width* axis this fixture varies on purpose, so a
  /// ceiling over the whole total would have had to be loose enough to admit the widest row and
  /// would then have admitted a clustering hash on the narrowest. `row` computes what a row owes
  /// before a single comparison, and these two bound what is left.
  ///
  /// The comparison term is what this gate is about, and it is under one per key: with the whole
  /// 64-bit hash mixed, every row below costs about three quarters of a comparison per key — summed
  /// over every table size the interner grows through, not just the last. One per key is that with
  /// room, and still three orders of magnitude under the scan.
  const CEILING: u32 = KEYS;

  /// A lookup that compares nothing costs exactly the overhead, so a floor above it catches a
  /// charge deleted outright — which `work > KEYS` alone did not.
  const FLOOR: u32 = KEYS / 2;

  const NAMED: [&str; 5] = [
    "k{i}",
    "field{i}",
    "h{i:0>8}",
    "user{i:0>4}Name",
    "pppp{i:04x}",
  ];
  const RADICES: [&[u8]; 4] = [
    b"0123456789",
    b"0123456789abcdef",
    b"0123456789abcdefghijklmnopqrstuvwxyz",
    b"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
  ];
  /// A name is `x` plus the counter, so these are lengths 7, 8, 9, 16, 17 and 18.
  const WIDTHS: [usize; 6] = [6, 7, 8, 15, 16, 17];

  fn named(scheme: usize, index: u32) -> std::string::String {
    match scheme {
      0 => std::format!("k{index}"),
      1 => std::format!("field{index}"),
      2 => std::format!("h{index:0>8}"),
      3 => std::format!("user{index:0>4}Name"),
      _ => std::format!("pppp{index:04x}"),
    }
  }

  fn generated(radix: &[u8], width: usize, index: u32) -> std::string::String {
    let mut digits = std::vec![radix[0]; width];
    let mut rest = index as usize;
    for slot in (0..width).rev() {
      digits[slot] = radix[rest % radix.len()];
      rest /= radix.len();
    }
    let mut name = std::string::String::from("x");
    name.push_str(core::str::from_utf8(&digits).expect("ascii"));
    name
  }

  /// The query for one spelling, and everything its total costs that is **not** a comparison.
  ///
  /// Derived from the row's own names rather than written down, for the reason the collision search
  /// in `smear_compiler::scratch` gives: a term computed from the fixture moves when the fixture
  /// does, and a term copied out of a measurement goes stale silently.
  fn row(spell: impl Fn(u32) -> std::string::String) -> (std::string::String, u32) {
    use crate::collect::byte_units;

    let mut query = std::string::String::from("{");
    // Draft §6.1's one definition.
    let mut overhead = 1;
    for index in 0..KEYS {
      let key = spell(index);
      // One visit for the selection, two passes over the key's bytes to intern it — hashing it and
      // copying it into the arena, since a distinct key never reaches the `memcmp` — and one pass
      // over the field's own one-byte spelling for `expand`'s schema probe.
      overhead += 1 + 2 * byte_units(key.len()) + byte_units(1);
      query.push_str(&std::format!(" {key}: a"));
    }
    query.push_str(" }");
    (query, overhead)
  }

  let mut rows: std::vec::Vec<(std::string::String, std::string::String, u32)> =
    std::vec::Vec::new();
  for (scheme, label) in NAMED.iter().enumerate() {
    let (query, overhead) = row(|index| named(scheme, index));
    rows.push(((*label).into(), query, overhead));
  }
  for radix in RADICES {
    for width in WIDTHS {
      let (query, overhead) = row(|index| generated(radix, width, index));
      rows.push((
        std::format!("x{{i:radix {} width {width}}}", radix.len()),
        query,
        overhead,
      ));
    }
  }

  for (label, query, overhead) in &rows {
    let (floor, ceiling) = (overhead + FLOOR, overhead + CEILING);
    let work = collection_work(ONE_FIELD, query);
    assert!(
      work > floor,
      "{work} units for {KEYS} distinct keys spelled {label}, under a floor of {floor}. A probe \
       that succeeds or fails still compares something, and {overhead} is what interning them free \
       of charge reads as",
    );
    assert!(
      work <= ceiling,
      "{work} units for {KEYS} distinct keys spelled {label}, against a ceiling of {ceiling} — \
       {overhead} of which is the walk, the interning passes and the schema probes, so the \
       comparisons are {}. Scanning the names instead of probing them costs about {}; a total \
       between the two is the hash dropping an honest document's names into a handful of buckets, \
       which is what the avalanche step and the fold between rounds in `smear_schema::hash_bytes` \
       exist to stop",
      work.saturating_sub(*overhead),
      u64::from(KEYS) * u64::from(KEYS) / 2
    );
  }
}

/// A flat fragment chain stays linear, which is the other half of the same defect.
///
/// The chain's *depth* is already pinned by `a_flat_fragment_chain_no_longer_ends_the_process` in
/// `smear/tests/proto_execute.rs`, which is the regression for the abort. This is its *cost*: the
/// walk stopped spending native frames but still scanned every definition in the document once per
/// spread, so a chain that no longer killed the process still took quadratic time to answer.
///
/// Eight terms, all linear in the chain: draft §6.1's lookup over the definitions, the index pass
/// over them again, one push per fragment, one visit per selection, about one comparison per
/// spread, and — al8n/smear#172 — a pass over the bytes of each spread's name to hash it, another
/// over the one it matches, and one over each fragment's type condition before the schema is probed
/// with it. Every name here is short enough for a pass to be one unit, so the total is about eight
/// units a link.
///
/// The bound is two-sided and both sides are what matters: eight a link against the `LINKS² / 2`
/// that scanning the definitions per spread costs is three orders of magnitude, so a ceiling with
/// room in it still separates linear from quadratic.
#[test]
fn a_flat_fragment_chain_is_linear() {
  const LINKS: u32 = 4096;

  let work = collection_work(ONE_FIELD, &fragment_chain(LINKS));
  assert!(
    work >= 6 * LINKS,
    "the index pass alone is a definition and a fragment each, every spread is a visit, and \
     resolving one hashes its name and compares at least the entry it returns; {work} units for \
     {LINKS} links is short of that, which is what an unindexed table, an uncharged one, or one \
     charging entries where the work is bytes reads as"
  );
  assert!(
    work <= 10 * LINKS,
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
  /// Definitions in `fragment_chain(LINKS)`: the operation and `LINKS + 1` fragments.
  const DEFINITIONS: u32 = LINKS + 2;
  /// Draft §6.1's lookup, which reads every definition because no operation name is given.
  const LOOKUP: u32 = DEFINITIONS;
  /// The lookup, one selection, then one unit per definition the index pass walks. The fragment
  /// charge is what will not fit.
  const UP_TO_THE_POPULATION: u32 = LOOKUP + 1 + DEFINITIONS;

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
  assert_eq!(
    spent, UP_TO_THE_POPULATION,
    "the fixture only says anything if the index pass's definitions charge was accepted and the \
     fragment charge was the one refused. {spent} units against the {UP_TO_THE_POPULATION} this \
     ceiling admits means the run was refused somewhere earlier — at draft §6.1's own lookup, \
     which now spends {LOOKUP} of them before collection begins, or at the definitions charge \
     itself"
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

/// The index pass reads each definition once, so a document of operations is not scanned twice.
///
/// # The totals cannot see this, for the same reason the refusal above could not
///
/// `Fragments::build` spends `definitions` for the walk that finds the fragments and `fragments`
/// for the pushes that index them, and it spends both **up front** — out of a slice length and a
/// count taken before either is used. The total is therefore a function of the document alone: it
/// reads the same whether the pass walks the definitions once or twice. The population used to walk
/// them a second time, `fill` taking the slice and sieving the fragments back out of it, and every
/// gate here was green over that — including the two exact-total ones, which are exact about a
/// number that did not move.
///
/// What moved is the work, and the second walk is the half of it that no charge bounds. A document
/// is free to be nearly all operations, so `definitions` and `fragments` can be as far apart as the
/// client likes: below, `DEFINITIONS + FRAGMENTS` units admit a pass that reads `2 × DEFINITIONS`
/// definitions — more definition reads than the entire operation has budget for.
///
/// `Fragments::walked` is the count taken at the read, and it is the only thing that separates the
/// two versions. That is the role `Fragments::compares` plays for a probe run charged after the
/// fact, one step further out: there, the charge and the count agreed under both versions; here the
/// charge is not even taken per item, so it agrees with itself twice over.
///
/// # The document and the ceiling
///
/// `OPERATIONS` named operations and one fragment, spread by the operation the run selects — which
/// is the **first**, so draft §6.1's lookup stops at it, reads one definition and contributes
/// nothing to the walk under test. The ceiling is what the request costs to the unit: that one
/// definition, the root's one spread, the pass, the one comparison that finds the fragment, and
/// the one field inside it. It sits in the window the defect lives in — above
/// `DEFINITIONS + FRAGMENTS`, so the pass is admitted and the request is served, and far below
/// `2 × DEFINITIONS + FRAGMENTS`, so the second walk is one nobody paid for.
///
/// **The `LOOKUP` term is one and not `DEFINITIONS`, which is the second thing this fixture pins.**
/// `Executor::operation_definition` charges before each definition it reads rather than for the
/// slice it may read, so a named lookup that matches on the first definition costs one — and a
/// version charging the length up front needs `DEFINITIONS` more units than this ceiling has and
/// refuses a request it had the budget to serve.
///
/// **The plant.** Give `Paid` the definitions slice back and let `fill` filter it into `defs`
/// instead of moving the selection in, counting what it reads as every reader in that module does.
/// `walked` doubles, and the response, all four totals in `Charges` and `fragment_reserved()` stay
/// exactly as they are.
#[test]
fn the_index_pass_reads_each_definition_once() {
  /// Named operations, so that what the pass walks is overwhelmingly not fragments.
  const OPERATIONS: u32 = 512;
  /// The one fragment the selected operation spreads.
  const FRAGMENTS: u32 = 1;
  /// Hashing that fragment's name, which `Table::fill` does to decide its bucket. `F` is one byte,
  /// so the pass over it is one unit. al8n/smear#196.
  const FRAGMENT_NAMES: u32 = 1;
  /// What one walk of the document reads.
  const DEFINITIONS: u32 = OPERATIONS + FRAGMENTS;
  /// Draft §6.1's lookup, which matches `Op0` on the document's first definition and stops.
  const LOOKUP: u32 = 1;
  /// Everything the request costs that is not the lookup or the index pass, and every name in it
  /// is short enough that a pass over its bytes is one unit.
  ///
  /// The root's spread and the field inside the fragment are a visit each. Resolving the spread
  /// hashes `F` (one), compares the one entry it finds (one) and `memcmp`s it (one). The fragment's
  /// `on Query` condition is hashed before the schema is probed with it (one). Interning `a` hashes
  /// it and copies it (two), and `expand` hashes the same spelling again to resolve the field
  /// against the schema (one).
  const REST: u32 = 2 + 3 + 1 + 2 + 1;
  /// The whole request.
  const BUDGET: u32 = LOOKUP + DEFINITIONS + FRAGMENTS + FRAGMENT_NAMES + REST;

  let mut query = std::string::String::from("query Op0 { ...F }\n");
  for index in 1..OPERATIONS {
    query.push_str(&std::format!("query Op{index} {{ a }}\n"));
  }
  query.push_str("fragment F on Query { a }\n");

  let (schema, document) = compile_against(ONE_FIELD, &query);
  let mut space = Space;
  let mut executor = Executor::with_limits(
    &schema,
    &document,
    Limits {
      max_selection_visits: NonZeroU32::new(BUDGET).expect("not zero"),
      ..Limits::default()
    },
  );
  executor
    .start(&mut space, Some("Op0"), Value::Obj)
    .expect("the operation resolves");
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    executor.handle_resolved(&mut space, id, Value::Text);
  }

  let spent = executor.collection_work();
  let walked = executor.fragment_definitions_walked();
  assert_eq!(
    spent, BUDGET,
    "the ceiling is meant to be this request's own cost to the unit, so that being served says the \
     budget was enough and not that it had slack; {spent} spent against {BUDGET} means the fixture \
     is no longer tuned to what it is watching"
  );
  let errors = {
    let response = executor.poll_response().expect("nothing is outstanding");
    response.error_count()
  };
  assert_eq!(
    errors, 0,
    "the request costs exactly what it was given, so it has to be served; a refusal here would \
     leave the count below measuring a pass that was abandoned rather than one that walked once"
  );
  assert_eq!(
    walked,
    u64::from(DEFINITIONS),
    "the pass read {walked} definitions under a ceiling of {BUDGET} units, every one of which this \
     request spent somewhere. One walk is what the `definitions` charge buys; a second is a scan of \
     the document's {OPERATIONS} operations, priced by nothing and bounded by nothing the fragment \
     count can express"
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
/// pushed, and one `byte_units` for each name `Table::fill` hashes to decide its bucket.
///
/// Derived from the names rather than written down, for the reason `colliding_spread_cost` gives:
/// the search that produces them does not promise how long they are, and a term over their bytes
/// has to move when they do. A push is a constant and the hash in front of it is not — the count
/// charges alone left `fill` reading every spelling for free, which is the wrong-dimension defect
/// this module's other rows are about, at the one site where the charge in front was a count.
/// al8n/smear#196.
fn colliding_index_cost(names: &[std::string::String]) -> u32 {
  use crate::collect::byte_units;

  COLLIDING_DEFINITIONS
    + COLLIDING as u32
    + names.iter().map(|name| byte_units(name.len())).sum::<u32>()
}

/// Draft §6.1's lookup over a colliding fixture, which every one of them enters with no operation
/// name — so it reads every definition, because ambiguity is only decidable at the end.
const COLLIDING_LOOKUP: u32 = COLLIDING_DEFINITIONS;

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
  colliding_names_of(prefix, mask, COLLIDING, 0)
}

/// `count` names that land in one bucket of `mask + 1`: `prefix`, then a counter zero-padded to at
/// least `width` digits.
///
/// The padding is what lets the *same* collision structure be searched for at several **lengths**,
/// which is the axis a ledger over entries cannot see. Searched again per width rather than padded
/// after the fact, because padding a name changes its hash and so changes its bucket.
fn colliding_names_of(
  prefix: &str,
  mask: u32,
  count: usize,
  width: usize,
) -> std::vec::Vec<std::string::String> {
  let mut by_bucket: std::vec::Vec<std::vec::Vec<std::string::String>> =
    std::vec::from_elem(std::vec::Vec::new(), mask as usize + 1);
  for candidate in 0u64.. {
    let name = std::format!("{prefix}{candidate:0width$}");
    let at = smear_schema::bucket(smear_schema::hash_bytes(name.as_bytes()), mask) as usize;
    by_bucket[at].push(name);
    if by_bucket[at].len() == count {
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

/// Everything a colliding fixture costs beyond draft §6.1's lookup and the index pass, when the
/// one spread finds its fragment on the **first** entry it compares.
///
/// Derived from `spread`'s own length rather than written down, because half of it is a charge over
/// bytes and the search that produces these names does not promise how long they are.
///
/// The terms: the root's spread and the field inside the fragment are a visit each; resolving the
/// spread hashes the spelling, compares the bucket head and `memcmp`s it; the fragment's `on Query`
/// condition is hashed before the schema is probed with it; interning the field's key `a` hashes it
/// and copies it into an empty arena, comparing nothing; and `expand` hashes `a` again to resolve
/// the field against the schema.
fn colliding_spread_cost(spread: &str) -> u32 {
  use crate::collect::byte_units;

  let visits = 2;
  let lookup = 2 * byte_units(spread.len()) + 1;
  let condition = byte_units("Query".len());
  let key = 2 * byte_units("a".len());
  let probe = byte_units("a".len());
  visits + lookup + condition + key + probe
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

  let index = colliding_index_cost(&names);
  let refused = collected_under(&schema, &document, COLLIDING_LOOKUP + index - 1);
  assert!(
    refused.is_some(),
    "a budget one unit short of the index pass must refuse rather than index for free"
  );

  let rest = colliding_spread_cost(&head);
  let served = collected_under(&schema, &document, COLLIDING_LOOKUP + index + rest);
  assert_eq!(
    served, None,
    "and {rest} units past it is enough for the spread, its one comparison, the passes each of \
     those makes over a name, and the field it reaches"
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

  // Draft §6.1's lookup over every definition, the index pass, and the constant every one of these
  // fixtures pays to spread the bucket's head once. The index term is what this gate watches: it is
  // one unit per definition and one per fragment, and `COLLIDING` names in a single bucket must not
  // move it, because chaining pushes at a head and never probes.
  let expected = COLLIDING_LOOKUP + colliding_index_cost(&names) + colliding_spread_cost(&head);
  assert_eq!(
    collection_work(ONE_FIELD, &query),
    expected,
    "{COLLIDING} fragment names in one bucket must cost one unit each to index and no more; a \
     total above this is an insertion whose cost depends on the names"
  );
}

/// The collection ledger tracks the **bytes** a document-chosen name costs, not merely the entries
/// it walks past.
///
/// # Why a third gate, when the two above already price the pile-up and the chain
///
/// Those price *entries*: `k` colliding names walk `k²/2` of them and the budget records `k²/2`.
/// Neither says anything about `L`, the length of the names, and draft §2.1.9 puts no local ceiling
/// on one. Charging entries while running bytes recorded `O(k²)` and ran `O(k² · L)` — about 512
/// aliases of thirty-two kilobytes fit under the default `max_interned_bytes`, and their 130,816
/// charged comparisons moved roughly four gigabytes. It does not even need the pile-up: a single
/// long key looked up once per object position hashes and `memcmp`s its whole length for the one or
/// two units the entry costs, and positions are a factor the query never pays for. al8n/smear#172.
///
/// Four sites, one row each, so a repair reaching one and not the others cannot be green:
///
/// - the **response-key interner**, whose keys are the document's aliases;
/// - the **fragment index**, whose keys are the document's fragment names;
/// - the **schema probe** a type condition goes through, whose key is the document's spelling and
///   whose table is the schema's — the residual that cleared that one cleared the *run length* and
///   said nothing about the hash;
/// - the **schema probe `expand` makes with a field's name**, which collection cannot have charged
///   for, because what collection interned is the alias.
///
/// Each row is the same structure at three lengths: the same collision, the same number of entries
/// compared, the same number of selections. Only the spelling grows. A ledger over entries reads
/// the same number three times, which is the assertion below.
///
/// # And the totals are exact, which says *which* passes were charged
///
/// The interner row's total is written out term by term. The `k²/2` walk contributes no byte term
/// at all: the whole hash is stored beside each entry, so a bucket collision is rejected on two
/// integers and reads nothing. That absence is the second half of the repair and the reason the
/// first half is affordable.
///
/// **The plants.** Delete any one `take_bytes`/`spend_bytes` and that row's three totals collapse
/// onto each other. Drop the stored hash and compare bytes on every chain step instead: the
/// interner row's exact total gains a `k²/2 · byte_units` term and fails on the first width.
#[test]
fn the_collection_charge_tracks_the_bytes_a_name_costs() {
  use crate::collect::byte_units;

  /// Names in one bucket. Small because the search costs about `RUN × buckets` trial hashes and is
  /// repeated at every width, and because one bucket holding every name is the worst case at any
  /// size.
  const RUN: usize = 64;
  /// One bucket of 128, and a set colliding under this mask collides under every narrower one the
  /// table grows through.
  const MASK: u32 = 127;
  /// Zero-padding widths, so the names are 5, 37 and 261 bytes: inside one hash chunk, several,
  /// and many.
  const WIDTHS: [usize; 3] = [4, 36, 260];

  let mut interner = std::vec::Vec::new();
  let mut fragments = std::vec::Vec::new();
  let mut conditions = std::vec::Vec::new();
  let mut field_names = std::vec::Vec::new();

  for width in WIDTHS {
    let names = colliding_names_of("k", MASK, RUN, width);
    let length = names[0].len();
    assert!(
      names.iter().all(|name| name.len() == length),
      "the search must produce names of one length, or the rows are not the same structure"
    );

    // Response keys: every name is interned, so the `RUN`th walks the `RUN - 1` already in its
    // bucket and rejects each on the stored hash without reading a byte.
    let mut query = std::string::String::from("{");
    for name in &names {
      query.push_str(&std::format!(" {name}: a"));
    }
    query.push_str(" }");
    let work = collection_work(ONE_FIELD, &query);
    // Draft §6.1's one definition; one visit per selection; two passes over each key — hashing it
    // and copying it into the arena — with no third, because a distinct key never reaches the
    // `memcmp`; the `RUN(RUN - 1)/2` entries the chains compare; and `expand`'s probe of the field's
    // own one-byte spelling, once per group.
    let walk = (RUN * (RUN - 1) / 2) as u32;
    let expected =
      1 + RUN as u32 + 2 * RUN as u32 * byte_units(length) + walk + RUN as u32 * byte_units(1);
    assert_eq!(
      work, expected,
      "{RUN} keys of {length} bytes in one bucket: each hashed once and copied once, {walk} \
       entries compared and none of them read"
    );
    interner.push((length, work));

    // Fragment names: one spread of the chain's *tail*, so the lookup walks every entry in the
    // bucket and `memcmp`s exactly the one that matches.
    let tail = names.first().expect("the set is not empty").clone();
    let query = colliding_document(&names, &tail);
    fragments.push((length, collection_work(ONE_FIELD, &query)));

    // Type conditions: inline fragments on a type the schema does not define, so each is hashed,
    // missed and skipped. The spelling is the only thing that grows.
    let mut query = std::string::String::from("{");
    for name in &names {
      query.push_str(&std::format!(" ... on {name} {{ a }}"));
    }
    query.push_str(" }");
    conditions.push((length, collection_work(ONE_FIELD, &query)));

    // Field names, which `expand` probes the schema with once per group and per object position.
    // The **alias** is what collection interns, so a short key beside a long field name is a charge
    // of one or two units in front of a hash of whatever the client wrote — and none of these names
    // being a field the schema defines changes nothing about what hashing one costs.
    let mut query = std::string::String::from("{");
    for (index, name) in names.iter().enumerate() {
      query.push_str(&std::format!(" k{index}: {name}"));
    }
    query.push_str(" }");
    field_names.push((length, collection_work(ONE_FIELD, &query)));
  }

  for (label, row) in [
    ("response keys", &interner),
    ("fragment names", &fragments),
    ("type conditions", &conditions),
    ("field names", &field_names),
  ] {
    assert!(
      row[0].1 < row[1].1 && row[1].1 < row[2].1,
      "{label}: the charge does not move with the length: {row:?}. The same collision at three \
       lengths costs three different amounts of work, and a ledger that counts entries reads the \
       same number three times"
    );
  }
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
  let index = colliding_index_cost(&names);
  let budget = COLLIDING_LOOKUP + index + 1 + SLACK;
  let limits = Limits {
    max_selection_visits: NonZeroU32::new(budget).expect("not zero"),
    ..Limits::default()
  };
  let mut executor = Executor::with_limits(&schema, &document, limits);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");

  // Everything but `SLACK` has to have been spent before the probe run begins, or the run is not
  // the thing the ceiling refused and the bound below holds for a reason the fixture is not about.
  let spent = executor.collection_work();
  assert!(
    spent > COLLIDING_LOOKUP + index,
    "{spent} units spent means this run was refused before it reached the bucket — at draft §6.1's \
     lookup or at the index pass — so the comparison bound below is vacuous"
  );

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
#[cfg_attr(
  miri,
  ignore = "A COST GATE, AND ITS FIXTURE CANNOT FIT IN A 32-BIT INTERPRETED ADDRESS SPACE. What \
            is asserted below is `interner_compares()` against a ceiling — a claim about how much \
            work a colliding document can buy, not about whether that work has undefined \
            behaviour, and the interner's MIR is interpreted by every sibling in this file that \
            names a variable at all. What it costs is the search: `colliding_names` walks \
            candidates until 512 of them share one bucket of 2048, holding EVERY candidate it \
            rejected in `by_bucket` until it returns, which is on the order of a million live \
            `String` allocations at once. Under `i686-unknown-linux-gnu` that is `resource \
            exhaustion: there are no more free addresses in the address space`, and the whole \
            `-p graphql-proto --lib` binary dies with it — `ci/miri_sb.sh` already carries \
            `-Zmiri-address-reuse-rate=1.0` for that target and records it as measured \
            insufficient, and no reuse is possible inside one call where nothing has been freed \
            yet. The three `colliding_fragment_names` gates above run the same search at half the \
            mask and DO complete in this binary, which is what puts the peak in this call rather \
            than in the accumulation. Declared in `ci/miri_scope.py`'s ignore table, which is \
            what stops this from being a coverage cut nobody chose."
)]
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

// ------------------------------------------------------------------------------------------
// Draft §6.1's own walk, which was the last uncharged one
// ------------------------------------------------------------------------------------------
//
// al8n/smear#144. `Executor::operation_definition` walks the document's definitions once per
// `start` to find the operation, over a count the client chooses, and it used to be charged to
// nothing at all — outside both fences #143 built, in the one site those fences do not reach.
//
// It is charged now, one unit before each definition it reads. Two things about that cannot be
// seen from a verdict and are therefore pinned by a count taken at the read:
//
// - **the charge is per definition read, not for the slice it might read**, so a named lookup that
//   matches early is not refused for definitions it never touches — a version charging
//   `definitions.len()` up front refuses requests it had the budget to serve, and
//   `the_index_pass_reads_each_definition_once` is tuned to catch exactly that;
// - **the charge is taken before the read**, so a refused lookup stops at the ceiling. A version
//   that walked first and charged afterwards produces the same `OperationLookupRefused` for the
//   same document, having read the whole of it.

/// A document of `count` named operations, `Op0` first.
fn many_operations(count: u32) -> std::string::String {
  let mut query = std::string::String::new();
  for index in 0..count {
    query.push_str(&std::format!("query Op{index} {{ a }}\n"));
  }
  query
}

/// Draft §6.1's lookup charges one unit per definition it reads, and reads only what it needs.
///
/// Three exact totals over one document, which is what makes them a statement about the *walk*
/// rather than about a constant: the same 512 definitions cost 1, 512 and 512 units depending only
/// on which operation is asked for and whether ambiguity has to be decided.
///
/// **The plant.** Delete the `visits.take(1)` and the first total falls to one, the second to two
/// and the third to two, while every response in the file is unchanged.
#[test]
fn the_operation_lookup_charges_one_unit_per_definition_read() {
  const OPERATIONS: u32 = 512;
  /// What collecting the one field `a` costs once the lookup has finished: one visit for the
  /// selection, one pass over its one-byte key to hash it and one to copy it into the arena, and
  /// one over the same spelling for `expand`'s schema probe.
  const FIELD: u32 = 4;

  let query = many_operations(OPERATIONS);
  let (schema, document) = compile_against(ONE_FIELD, &query);
  let mut space = Space;

  // The first operation, by name: the walk stops on the definition that answers.
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, Some("Op0"), Value::Obj)
    .expect("the operation resolves");
  assert_eq!(
    executor.operation_definitions_walked(),
    1,
    "a named lookup that matches the first definition read one definition and no more"
  );
  assert_eq!(
    executor.collection_work(),
    1 + FIELD,
    "and it cost one unit for that definition plus {FIELD} for the field it collects"
  );

  // The last operation, by name: every definition before it has to be read.
  let wanted = std::format!("Op{}", OPERATIONS - 1);
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, Some(&wanted), Value::Obj)
    .expect("the operation resolves");
  assert_eq!(
    executor.operation_definitions_walked(),
    u64::from(OPERATIONS),
    "the last operation is behind every other definition, so the walk reads all of them"
  );
  assert_eq!(
    executor.collection_work(),
    OPERATIONS + FIELD,
    "and pays one unit for each, plus the {FIELD} the field costs"
  );

  // No name: the walk cannot stop early, because ambiguity is only decidable at the second
  // operation — which here is the second definition.
  let mut executor = Executor::new(&schema, &document);
  let refused = executor
    .start(&mut space, None, Value::Obj)
    .expect_err("the document holds more than one operation");
  assert_eq!(refused, StartError::AmbiguousOperation);
  assert_eq!(
    executor.operation_definitions_walked(),
    2,
    "ambiguity is decided by the second operation, so the walk stops there rather than at the end"
  );
}

/// With no operation name the walk reads the whole document, because it must.
///
/// The other of the lookup's two modes, and the one an index has to answer as carefully as the
/// named one: a single operation followed by fragments is only *unambiguous* once every definition
/// has been read, so there is nothing to stop early on and the charge is the document.
#[test]
fn an_unnamed_lookup_reads_every_definition_before_it_can_say_the_operation_is_the_only_one() {
  const LINKS: u32 = 64;
  /// `fragment_chain` is one operation and `LINKS + 1` fragments.
  const DEFINITIONS: u32 = LINKS + 2;

  let query = fragment_chain(LINKS);
  let (schema, document) = compile_against(ONE_FIELD, &query);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");

  assert_eq!(
    executor.operation_definitions_walked(),
    u64::from(DEFINITIONS),
    "one operation and {} fragments, and the lookup has to read every one of them to know the \
     operation is the only one",
    LINKS + 1
  );
}

/// A refused lookup stops at the ceiling instead of reading the document and refusing afterwards.
///
/// # The verdict cannot see this, which is the whole reason the count exists
///
/// `OperationLookupRefused` is the answer under both versions and on every retry: `reset` rebuilds
/// `Visits`, so the same document under the same ceiling reads the same no. What moves is how much
/// of the document a refusal reads — the work an adversary gets for a request that is going to be
/// turned away regardless, which is the same property `Fragments::compares` was added for one
/// table over.
///
/// **The plant.** Take the charge after the read instead of before it, or spend
/// `definitions.len()` up front. The first leaves `walked` at `OPERATIONS`, the second at zero;
/// the refusal, the message and every response in this file are identical under both.
#[test]
fn a_refused_operation_lookup_reads_nothing_past_the_ceiling() {
  const OPERATIONS: u32 = 512;
  /// Definitions the ceiling has room for, which is what the walk must stop at.
  const BUDGET: u32 = 8;

  let query = many_operations(OPERATIONS);
  let (schema, document) = compile_against(ONE_FIELD, &query);
  let wanted = std::format!("Op{}", OPERATIONS - 1);
  let mut space = Space;
  let mut executor = Executor::with_limits(
    &schema,
    &document,
    Limits {
      max_selection_visits: NonZeroU32::new(BUDGET).expect("not zero"),
      ..Limits::default()
    },
  );

  let refused = executor
    .start(&mut space, Some(&wanted), Value::Obj)
    .expect_err("the document has more definitions than the ceiling admits");
  assert_eq!(refused, StartError::OperationLookupRefused);
  assert_eq!(
    executor.operation_definitions_walked(),
    u64::from(BUDGET),
    "the walk read {} definitions against a ceiling of {BUDGET}; a charge taken after the read \
     lets the whole document be walked before the refusal arrives",
    executor.operation_definitions_walked()
  );
  assert_eq!(
    executor.collection_work(),
    BUDGET,
    "and it spent exactly what it read, which is what makes the two counts separable at all"
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

// ------------------------------------------------------------------------------------------
// draft §6.2.2's serial release: the product it would have been
// ------------------------------------------------------------------------------------------
//
// The spec's fourth sweep says a budget on one factor of a product bounds nothing, and names this
// path as one of the two places the shape was expected to recur. It is here to be built: the serial
// rule has to decide, once per offer, whether the previous top-level field is done with and which
// field comes next. Scanning the root's children for the answer makes that `offers × M`, with
// `offers` the driver's quantity — one document sub-selection becomes as many requests as
// `list_len` claims elements — and `M` the document's. `max_response_slots` bounds the first,
// itself and `max_response_metadata` bound the second, and neither reaches their product.
//
// The remedy here is not a fourth ceiling, because the cost is in a mechanism rather than in a
// population: withholding removes it. The withheld fields are the ready chain's own tail, so
// "which is next" is one link, and nothing that can still affect the response is on the chain or
// counted `live` outside the running subtree, so "is it done with" is two counters — the question
// being the weaker one, because draft §6.4.4 moves a discarded subtree's requests out of `live`
// and the release steps over them. `Executor::release_serial` carries that boundary.
//
// Which is unobservable from a response — a scanning implementation answers every mutation exactly
// as this one does — so the gate is the count, as it is for collection three sections up. The
// second fixture is the one that matters: it holds the *query* factor fixed and multiplies the
// *driver's*, which is the product's own shape.

/// A mutation root with one field, so a document can name it as often as it likes, and a list of
/// **objects**, so the driver decides how many requests the document's one sub-selection becomes.
///
/// Also the fixture for `a_withheld_top_level_field_is_in_the_tree_and_reads_as_null`, which needs
/// nothing from a schema but two top-level mutation fields.
///
/// The element type is an object and not a leaf, and that is the fixture's whole discriminating
/// power. A list of leaves grows *positions* with the driver's answer but not **offers** — the
/// elements are completed inside one `handle_resolved` and never handed out — so a `[String]`
/// version runs the same number of `poll_resolve` calls at every length, holds the wrong factor
/// fixed, and cannot see the product at all. That version was written first, and the planted scan
/// passed it.
const SERIAL_SDL: &str = r#"
type Query { a: String }
type Mutation { m: Cell }
type Cell { items: [Cell] text: String }
"#;

/// Runs a mutation of `fields` top-level aliases, each over a list of `elements` objects, and
/// returns what the serial release cost and how many requests the driver was offered.
///
/// Two numbers rather than one, because "the cost did not change" is only evidence if the thing it
/// was supposed to change with did.
fn serial_run(fields: usize, elements: usize) -> (u64, usize) {
  let mut query = std::string::String::from("mutation {");
  for field in 0..fields {
    query.push_str(&std::format!(" f{field}: m {{ items {{ text }} }}"));
  }
  query.push_str(" }");

  let (schema, document) = compile_against(SERIAL_SDL, &query);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");
  let mut offers = 0usize;
  while let Some(request) = executor.poll_resolve(&mut space) {
    offers += 1;
    let id = request.id();
    let answer = match request.name() {
      "items" => Value::List(elements),
      "text" => Value::Text,
      _ => Value::Obj,
    };
    executor.handle_resolved(&mut space, id, answer);
  }
  assert!(
    executor.poll_response().is_some(),
    "the mutation runs to a response"
  );
  (executor.serial_steps(), offers)
}

/// The release costs one link per top-level field, and the first field costs none.
///
/// Exact rather than bounded: the cut leaves the head on the chain and parks the other `M - 1`, and
/// each of them is spliced back by following exactly one link.
///
/// **Measured against the plant.** Replacing the cursor with a walk of the root's children on every
/// call — the obvious implementation, and a correct one — takes this from 63 to **6,112**.
#[test]
fn a_serial_release_costs_one_step_per_top_level_field() {
  const FIELDS: usize = 64;

  let (steps, _) = serial_run(FIELDS, 1);
  assert_eq!(
    steps,
    FIELDS as u64 - 1,
    "{FIELDS} top-level fields, the first of which is never withheld; a larger total means the \
     release is searching for the next field rather than being handed it"
  );
}

/// **The constraint the spec predicted this path would trip.** The driver's answers do not multiply
/// the serial mechanism's cost.
///
/// The query factor is held fixed and the driver's is multiplied, which is the product's own shape:
/// an implementation that decided serial eligibility per offer grows with the second run and this
/// one does not move at all.
///
/// **Measured against the same plant**, and this is the number that says the shape is a product
/// rather than merely a cost. The two runs offer 192 and 4,224 requests over the same 64 top-level
/// fields; the cursor spends 63 links on both, and the planted walk spends **6,112 and 133,120** —
/// twenty-two times the driver's requests bought twenty-two times the serial gate's work, for a
/// document that did not change.
#[test]
fn a_serial_release_does_not_grow_with_the_response() {
  const FIELDS: usize = 64;
  const ELEMENTS: usize = 64;

  let (narrow_steps, narrow_offers) = serial_run(FIELDS, 1);
  let (wide_steps, wide_offers) = serial_run(FIELDS, ELEMENTS);

  assert!(
    wide_offers > narrow_offers * 8,
    "the fixture has to make the driver's factor big before it can say the cost ignores it: \
     {narrow_offers} requests against {wide_offers}"
  );
  assert_eq!(
    narrow_steps, wide_steps,
    "{wide_offers} requests against {narrow_offers}, and the same {narrow_steps} links; a total \
     that moved would be the serial gate charging the driver's quantity for the document's"
  );
}

// ------------------------------------------------------------------------------------------
// What a response would say about a field draft §6.2.2 is still withholding
// ------------------------------------------------------------------------------------------

/// A withheld top-level mutation field is **in** the response tree, and it reads as `null`.
///
/// The fact under [`Executor::release_serial`]'s account of what gating the release on `abandoned`
/// would cost a release build. That symptom is not a *missing* field, and the difference matters
/// because absence is something a driver can notice: §6.2.2's cut takes the field off the ready
/// chain and off nothing else, so its slot is still the root's child and still carries its
/// response key, and [`node`] renders [`State::Ready`] as [`Node::Null`]. What a gated
/// `release_serial` would hand back is the key present with a null under it and no error — a wrong
/// answer shaped like a legitimate one.
///
/// The two halves are asserted apart because either can go without the other. A later phase that
/// built the response tree lazily, or one that unlinked a withheld field rather than only
/// unchaining it, would drop the key; one that gave `Ready` a rendering of its own would keep the
/// key and change the value. Only the pair makes "null, not absent" true.
///
/// Read from the tree rather than from a delivered response, because the executor never delivers
/// one in this state — `poll_response` splices the cursor before it tests whether it is finished.
/// That is the same reason `a_drained_subtree_is_not_walked_again` is in here.
#[test]
fn a_withheld_top_level_field_is_in_the_tree_and_reads_as_null() {
  let (schema, document) = compile_against(
    SERIAL_SDL,
    "mutation { first: m { text } second: m { text } }",
  );
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");

  // `first` is answered and its own sub-selection is left outstanding, which is what holds the
  // release and keeps `second` withheld for the read below.
  let first = executor.poll_resolve(&mut space).expect("`first`").id();
  executor.handle_resolved(&mut space, first, Value::Obj);
  assert!(
    executor.poll_resolve(&mut space).is_some(),
    "`first.text`, never answered"
  );

  let withheld = executor.serial_next;
  assert_ne!(withheld, NONE, "`second` is parked on the withheld cursor");
  assert!(
    matches!(executor.slots[withheld as usize].state, State::Ready),
    "and it is still `Ready`: withholding is a cut on `next_ready` and nothing else"
  );

  const ROOT: u32 = 0;
  let Node::Object(mut fields) = node(
    &executor.slots,
    executor.interner.names(),
    executor.interner.spans(),
    ROOT,
  ) else {
    panic!("the root is an object")
  };
  let (key, value) = fields.next().expect("`first`");
  assert_eq!(key.to_string(), "first");
  assert!(
    matches!(value, Node::Object(_)),
    "the field that was released is an object, so the two keys below are distinguishable"
  );
  let (key, value) = fields
    .next()
    .expect("the withheld field is still the root's child");
  assert_eq!(
    key.to_string(),
    "second",
    "under its response key, which is what makes the symptom a null and not an absence"
  );
  assert!(
    matches!(value, Node::Null),
    "and it reads as `null`, with nothing in `errors` to account for it"
  );
  assert!(fields.next().is_none(), "and the root has no third child");
}

// ------------------------------------------------------------------------------------------
// the entry-point protocol, derived from this file rather than remembered
// ------------------------------------------------------------------------------------------

/// What discharges the entry-point obligation for one public `&mut self` method.
///
/// An enum rather than a boolean because the point of the table is that a method may not simply be
/// *omitted*: the only way out of `on_entry` is to name the alternative that does the same work.
/// `refactor/crate-split` learned the same lesson one level up, where an exemption table of pairs
/// to skip hid a real finding and became a table of twins to declare instead.
///
/// **It had a second variant, `Reset`, and losing it is a finding rather than a tidy.** `start`
/// opened with `self.reset()`, which discharges strictly more than `on_entry` — and that was
/// exactly the defect: the reset destroyed the draft §6.2.3.3 obligation an open response stream
/// was publishing, before `start` could refuse on it. The refusal has to read the phase first, so
/// `start`'s opening statement is now `on_entry` like every other entry point's, and every path
/// that goes on to begin an operation still resets. One variant, therefore — and a method that ever
/// needs a different discharge adds its own rather than being omitted.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Discharge {
  /// Calls `on_entry` first, which is every entry point.
  OnEntry,
}

impl Discharge {
  const fn statement(self) -> &'static str {
    match self {
      Self::OnEntry => "self.on_entry();",
    }
  }
}

/// Where one **execution** is when a driver calls in.
///
/// **The second axis of this table, and it was missing.** The first version enumerated what each
/// entry point *releases* and stopped, which is one attribute of the answer; a method can discharge
/// the release obligation perfectly and still be meaningless — or worse, silently lossy — in the
/// phase it was called from. `set_extensions` was both: it accepted a map before `start`, which the
/// next `start` then dropped, and after delivery, where no response could ever carry it. Neither is
/// a release bug and the release axis could not see either.
///
/// It is one *execution* and not one operation, which draft §6.2.3 made a distinction rather than a
/// synonym: a subscription runs one execution per source event, so its `Idle` is "between events"
/// and its `Delivered` is "this event's result has been taken". Which operation kind is running is
/// the third axis — see [`EntryPoint::stream`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Phase {
  /// `!started`: before the first `start`, after one that refused, or between two draft §6.2.3.2
  /// events.
  Idle,
  /// `started && !delivered`: an execution is under way.
  Running,
  /// `started && delivered`: `poll_response` has yielded, and it yields at most once per execution.
  Delivered,
}

const ALL_PHASES: [Phase; 3] = [Phase::Idle, Phase::Running, Phase::Delivered];

/// A phase test a body performs on the executor's own state.
///
/// The derivation reads *tests* rather than mentions: `start` writes `self.started = true` and
/// `poll_response` writes `self.delivered = true`, and an assignment is not a guard.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Guard {
  /// `self.started` is read, which is what keeps [`Phase::Idle`] out.
  Started,
  /// `self.delivered` is read, which is what keeps [`Phase::Delivered`] out.
  Delivered,
  /// `self.phase` is read: the operation-kind slot, which is what keeps a draft §6.2.3 call out of
  /// an operation that has no response stream, and out of a response stream in the wrong state.
  ///
  /// It excludes no [`Phase`], and that is not a hole — it is a *different axis*. The two flags say
  /// where one execution is; this says which operation is running. A row declaring it must name the
  /// [`ResponseStream`] states its test admits, and a row that does not must name none, which is
  /// what makes the column a check rather than a caption.
  Stream,
}

impl Guard {
  /// Every guard, which is what the reader scans for.
  ///
  /// A `const` rather than a literal at the scan site, because the scan site is where a third kind
  /// was forgotten: adding [`Stream`](Guard::Stream) to the enum left the loop iterating the two it
  /// already knew, so every declared `Stream` read as unperformed — loudly, as it happens, but only
  /// because a row claimed it. A guard added to the enum and to no row would have been silent.
  const ALL: [Self; 3] = [Self::Started, Self::Delivered, Self::Stream];

  const fn field(self) -> &'static str {
    match self {
      Self::Started => "self.started",
      Self::Delivered => "self.delivered",
      Self::Stream => "self.phase",
    }
  }

  /// The [`Phase`] this guard excludes, when it excludes one at all.
  const fn excludes(self) -> Option<Phase> {
    match self {
      Self::Started => Some(Phase::Idle),
      Self::Delivered => Some(Phase::Delivered),
      Self::Stream => None,
    }
  }
}

/// One public `&mut self` method, on all three axes.
struct EntryPoint {
  name: &'static str,
  /// How it settles the previous call's lends.
  discharge: Discharge,
  /// The execution phases in which a call can still affect or report state.
  meaningful: &'static [Phase],
  /// The draft §7.1.2 response-stream states this method's own `self.phase` test admits.
  ///
  /// **The third column, added with draft §6.2.3's five entry points rather than after them.**
  /// Empty means the method performs no operation-kind test at all: its legality is decided by the
  /// two execution flags, and it means the same thing under a query, a mutation and a
  /// subscription. Non-empty means it reads [`Guard::Stream`], and these are the states its test
  /// acts in.
  ///
  /// "Acts in" and not "admits", because two rows read the slot without gating on it. `start`
  /// **refuses** the states it does not name, so for it the column is the admitted set; and
  /// `poll_response` performs a *transition* in the state it names — a recorded §6.2.3.2 ending is
  /// discharged by the very call that takes the result it was queued behind. Both are the same
  /// claim the column exists to make: this method does not mean the same thing under every
  /// operation kind, and here is where it differs.
  ///
  /// The two claims are checked against each other, in both directions, for the reason the guard
  /// column is checked against the source: a column nothing compares to the code is a caption. What
  /// it cannot check is that the *set* is right — a row admitting `Streaming` when the body admits
  /// `Creating` passes here — so the behavioural half is the per-state cases below.
  stream: &'static [ResponseStream],
  /// The state tests the body performs, checked against the source.
  guards: &'static [Guard],
  /// Why an excluded phase that no guard covers needs none.
  ///
  /// Required whenever a phase is excluded without a guard, and held to the same minimum length
  /// `ci/source_census`'s exemption table uses, for the same reason: a reason that says nothing is
  /// an omission with punctuation.
  structural: &'static str,
}

/// The shortest structural reason this gate will accept, matching `ci/source_census`.
const MIN_REASON: usize = 40;

/// Every `pub fn` in `execute.rs` that takes `&mut self`, on both axes.
///
/// Kept in step with the source by the test below, in both directions: a method missing from here
/// fails, and an entry here matching no method fails too. The second direction is the live canary —
/// if the reader ever stops finding the surface, this table goes stale in the same instant rather
/// than reporting a clean file.
const ENTRY_PROTOCOL: &[EntryPoint] = &[
  // `on_entry` and not `reset`, and the difference is one refusal. Draft §6.2.3.3's obligation
  // travels on the phase, so the open-stream refusal has to be read *before* the reset that would
  // erase it — and a refusal that mutates nothing is not "doing something else" ahead of the
  // discharge. Every path that actually begins an operation still resets, which is strictly more.
  EntryPoint {
    name: "start",
    discharge: Discharge::OnEntry,
    meaningful: &ALL_PHASES,
    // The three endings, and no open one: a source stream the driver has not been told to cancel
    // is the one thing `start` cannot silently replace.
    stream: &[
      ResponseStream::Completed,
      ResponseStream::Failed,
      ResponseStream::Cancelled,
    ],
    guards: &[Guard::Stream],
    structural: "",
  },
  EntryPoint {
    name: "poll_resolve",
    discharge: Discharge::OnEntry,
    meaningful: &[Phase::Running],
    stream: &[],
    guards: &[Guard::Started],
    structural: "Delivered needs no test: `poll_response` only yields once the ready chain is \
                 empty and `live` is zero, so after it has there is nothing to offer and this \
                 answers `None` by construction rather than by check.",
  },
  EntryPoint {
    name: "handle_resolved",
    discharge: Discharge::OnEntry,
    meaningful: &[Phase::Running],
    stream: &[],
    guards: &[],
    structural: "Both excluded phases are covered by the `ReqId` itself: an id is validated by \
                 epoch and generation, `reset` moves the epoch, and delivery happens only once \
                 every entry is free — so a call in either phase carries an id that is already \
                 stale and is ignored.",
  },
  EntryPoint {
    name: "handle_field_error",
    discharge: Discharge::OnEntry,
    meaningful: &[Phase::Running],
    stream: &[],
    guards: &[],
    structural: "The same `ReqId` validation as `handle_resolved`: epoch and generation make an id \
                 from before a `reset`, or from after delivery, stale and ignored without any \
                 phase flag being read.",
  },
  EntryPoint {
    name: "poll_abandoned",
    discharge: Discharge::OnEntry,
    // Delivered on purpose: `poll_response` deliberately does not withhold on abandoned entries,
    // so retiring them afterwards is the channel working rather than a call out of phase.
    meaningful: &[Phase::Running, Phase::Delivered],
    stream: &[],
    guards: &[],
    structural: "Idle needs no test: `reset` sets the abandoned count to zero and empties the \
                 slab, so the count is the guard and the loop is never entered.",
  },
  EntryPoint {
    name: "set_extensions",
    discharge: Discharge::OnEntry,
    meaningful: &[Phase::Running],
    stream: &[],
    guards: &[Guard::Started, Guard::Delivered],
    structural: "",
  },
  EntryPoint {
    name: "take_extensions",
    discharge: Discharge::OnEntry,
    // Every phase, and truthfully in each: before an operation and after delivery there is no map,
    // so `None` is the right answer rather than an accepted call that cannot be honoured.
    meaningful: &ALL_PHASES,
    stream: &[],
    guards: &[],
    structural: "",
  },
  // The one row whose `self.phase` read is a *transition* rather than a legality test, and the
  // column is right to demand it: under a subscription whose source stream has already ended, this
  // call is what completes the response stream, so it no longer means the same thing under all
  // three operation kinds. The state named is the published one it acts in — a recorded ending is
  // still `Streaming` until the result it is queued behind is taken here.
  EntryPoint {
    name: "poll_response",
    discharge: Discharge::OnEntry,
    meaningful: &[Phase::Running],
    stream: &[ResponseStream::Streaming],
    guards: &[Guard::Started, Guard::Delivered, Guard::Stream],
    structural: "",
  },
  // ── draft §6.2.3, the five that read the operation-kind slot ─────────────────────────────────
  //
  // Every one of them names a stream state, and none of them tests `started` except the intake —
  // which is the shape to expect rather than a coincidence. A subscription's legality question is
  // "where is the response stream", and only §6.2.3.2's ordering also needs to know where *this
  // event* is.
  EntryPoint {
    name: "handle_source_stream",
    discharge: Discharge::OnEntry,
    meaningful: &[Phase::Idle],
    stream: &[ResponseStream::Creating],
    guards: &[Guard::Stream],
    structural: "Running and Delivered are unreachable while the stream is Creating, and the \
                 stream test is what keeps them out: draft §6.2.3.1 begins no execution, so \
                 `start` leaves `started` false for a subscription and only `handle_source_event` \
                 — which requires Streaming — ever sets it.",
  },
  EntryPoint {
    name: "handle_source_event",
    discharge: Discharge::OnEntry,
    // Idle is between events and after `handle_source_stream`; Delivered is the ordinary case, the
    // previous event's result having been taken.
    meaningful: &[Phase::Idle, Phase::Delivered],
    stream: &[ResponseStream::Streaming],
    guards: &[Guard::Started, Guard::Delivered, Guard::Stream],
    structural: "Running is excluded by the conjunction of both flags rather than by either alone, \
                 and that conjunction is draft §6.2.3.2's ordering: an execution begun and not \
                 delivered is the previous event's, so accepting this event would discard an \
                 execution result the specification requires to be emitted. It is reported as \
                 `SourceEventError::Outstanding`, with the event handed back.",
  },
  // The two endings that read *both* execution flags, and that pair is the whole of the repair.
  // Running is not excluded — a source stream ends when it ends, mid-event included — it is the
  // phase in which the ending is recorded rather than performed, so the accepted event's execution
  // result survives to be emitted.
  EntryPoint {
    name: "handle_source_complete",
    discharge: Discharge::OnEntry,
    // Every execution phase, because a source stream ends when it ends: between events, mid-event
    // with requests outstanding, and after a result has been taken are all reachable.
    meaningful: &ALL_PHASES,
    stream: &[ResponseStream::Streaming],
    guards: &[Guard::Started, Guard::Delivered, Guard::Stream],
    structural: "",
  },
  EntryPoint {
    name: "handle_source_error",
    discharge: Discharge::OnEntry,
    meaningful: &ALL_PHASES,
    stream: &[ResponseStream::Streaming],
    guards: &[Guard::Started, Guard::Delivered, Guard::Stream],
    structural: "",
  },
  EntryPoint {
    name: "unsubscribe",
    discharge: Discharge::OnEntry,
    // Draft §6.2.3.3 arrives when a client disconnects, which is to say at any moment.
    meaningful: &ALL_PHASES,
    stream: &[ResponseStream::Creating, ResponseStream::Streaming],
    guards: &[Guard::Stream],
    structural: "",
  },
];

/// This module's own source, which is where the method set comes from.
const SOURCE: &str = include_str!("../execute.rs");

/// One public method taking `&mut self`: its name, and its body.
struct Entry<'s> {
  name: &'s str,
  body: &'s str,
}

/// Returns every `pub fn` in `text` whose receiver is `&mut self`.
///
/// Text rather than `syn`, and the trade is stated because it is the risky half of this gate. What
/// keeps it honest is not the parser's cleverness, it is that the test plants a drift form for each
/// branch and requires every one to be caught — a checker proven against the one example it was
/// written for is the failure this repository has already met once.
///
/// Three things make the scan safe rather than merely lucky. It anchors on a newline followed by
/// exactly two spaces and `pub `, which is an inherent-impl method and cannot be reached from
/// inside a `///` line, whose prefix is `  /// `. The parameter list is found by paren depth. And
/// the body ends at the first line that is exactly two spaces and a closing brace, which is where
/// rustfmt puts the end of a method in an impl block and nowhere else — `cargo fmt --check` is a
/// gate in this repository, so that is a guarantee rather than a hope, and it avoids brace
/// balancing over string and char literals entirely.
fn entry_points(text: &str) -> std::vec::Vec<Entry<'_>> {
  /// A newline, the two-space indent of an inherent-impl method, and `pub `.
  const ANCHOR: &str = "\n  pub ";
  /// The line rustfmt closes such a method with.
  const CLOSE: &str = "\n  }";

  let mut found = std::vec::Vec::new();
  let mut cursor = 0usize;
  while let Some(at) = text[cursor..].find(ANCHOR) {
    // The index of the `p`, which is the anchor's end less the width of `pub `.
    let start = cursor + at + ANCHOR.len() - "pub ".len();
    cursor = start + 1;
    let after = &text[start..];

    // `pub fn`, `pub const fn`, `pub unsafe fn`, `pub async fn` — anything else at this indent is
    // not a method and is skipped. A form this list does not know about would be missed silently,
    // so the selftest plants a `pub const fn` to prove the list is wide enough.
    let Some(signature) = after.strip_prefix("pub ").and_then(|rest| {
      ["fn ", "const fn ", "unsafe fn ", "async fn "]
        .iter()
        .find_map(|prefix| rest.strip_prefix(prefix))
    }) else {
      continue;
    };
    let name_len = signature
      .find(|c: char| !c.is_alphanumeric() && c != '_')
      .unwrap_or(signature.len());
    let name = &signature[..name_len];
    let tail = &signature[name_len..];

    // The parameter list, by paren depth. Generic parameters come before it and open no paren.
    let Some(open) = tail.find('(') else { continue };
    let mut depth = 0usize;
    let mut close = None;
    for (index, character) in tail[open..].char_indices() {
      match character {
        '(' => depth += 1,
        ')' => {
          depth -= 1;
          if depth == 0 {
            close = Some(open + index);
            break;
          }
        }
        _ => {}
      }
    }
    let Some(close) = close else { continue };

    let receiver: std::string::String = tail[open + 1..close]
      .split_whitespace()
      .collect::<std::vec::Vec<_>>()
      .join(" ");
    if !receiver.starts_with("&mut self") {
      continue;
    }

    // The body starts at the first brace after the signature; a return type and a `where` clause
    // hold none.
    let Some(brace) = tail[close..].find('{') else {
      continue;
    };
    let body = &tail[close + brace + 1..];
    let body = match body.find(CLOSE) {
      Some(end) => &body[..end],
      None => body,
    };
    found.push(Entry { name, body });
  }
  found
}

/// Returns whether `body`'s first statement is `statement`.
///
/// "Before it does anything else" is the obligation's actual wording, so first is what is checked
/// rather than merely present. Line comments are skipped because the release call is often
/// introduced by one.
fn opens_with(body: &str, statement: &str) -> bool {
  for line in body.lines() {
    let line = line.trim();
    if line.is_empty() || line.starts_with("//") {
      continue;
    }
    return line == statement;
  }
  false
}

/// Returns the phase flags `body` **reads**, which is not the same as the ones it mentions.
///
/// `start` writes `self.started = true` and `poll_response` writes `self.delivered = true`, and an
/// assignment is the opposite of a guard — it is the transition the guard exists to notice. So an
/// occurrence followed by a single `=` is skipped and one followed by anything else, `==` included,
/// is a read.
fn guards_read(body: &str) -> std::vec::Vec<Guard> {
  let mut found = std::vec::Vec::new();
  for guard in Guard::ALL {
    let field = guard.field();
    let mut cursor = 0usize;
    while let Some(at) = body[cursor..].find(field) {
      let rest = body[cursor + at + field.len()..].trim_start();
      cursor += at + field.len();
      let assignment = rest.starts_with('=') && !rest.starts_with("==");
      if !assignment {
        found.push(guard);
        break;
      }
    }
  }
  found.sort_unstable();
  found
}

/// Every public `&mut self` entry point declares how it settles the previous call and which phases
/// it is legal in, and the source agrees with both.
///
/// **Two axes, because one round of review found a defect on each and they were the same mistake.**
/// The release axis came first: `set_extensions` and `take_extensions` shipped without `on_entry`,
/// so a driver could drop a `FieldRequest`, call one of them, and leave the argument values and a
/// parked object value held. The phase axis came next: `set_extensions` also accepted a map before
/// `start` — which the next `start` then dropped — and after delivery, where no response could ever
/// carry it. Neither of those is a release bug, and a table that ranged only over releases was
/// complete along its own axis and blind along the other.
///
/// That is the shape to keep in mind when a ninth entry point arrives: this table is an enumeration
/// of *attributes of the answer*, and the failure mode is not a missing row, it is a missing
/// column.
#[test]
fn every_public_entry_point_declares_its_discharge_and_its_phases() {
  let entries = entry_points(SOURCE);
  assert!(
    entries.len() >= ENTRY_PROTOCOL.len(),
    "the reader found {} public `&mut self` methods and the table names {} — a reader that has \
     stopped finding the surface reports a clean file, so this fails instead",
    entries.len(),
    ENTRY_PROTOCOL.len()
  );

  for entry in &entries {
    let Some(declared) = ENTRY_PROTOCOL.iter().find(|row| row.name == entry.name) else {
      panic!(
        "`{}` is a public `&mut self` entry point that `ENTRY_PROTOCOL` does not name. Every call \
         in must settle the previous one's lends before doing anything else, and must say which \
         phases it is legal in — see `Executor`'s header. Add the row, do not omit it.",
        entry.name
      );
    };

    // Axis 1: the release.
    assert!(
      opens_with(entry.body, declared.discharge.statement()),
      "`{}` declares `{:?}` but its body does not open with `{}`. The obligation is \"before it \
       does anything else\", so the call has to be first.",
      entry.name,
      declared.discharge,
      declared.discharge.statement()
    );

    // Axis 2: the phase. The declared guards must be exactly the flags the body reads.
    let read = guards_read(entry.body);
    let mut named = declared.guards.to_vec();
    named.sort_unstable();
    assert_eq!(
      read, named,
      "`{}` declares the phase guards {:?} and its body reads {:?}. A guard the table claims and \
       the code does not perform is a phase nothing keeps out.",
      entry.name, named, read
    );

    // Every excluded phase is either guarded or argued for.
    for phase in ALL_PHASES {
      if declared.meaningful.contains(&phase) {
        continue;
      }
      let guarded = declared
        .guards
        .iter()
        .any(|guard| guard.excludes() == Some(phase));
      assert!(
        guarded || declared.structural.len() >= MIN_REASON,
        "`{}` is not meaningful in {phase:?}, performs no guard that excludes it, and gives no \
         structural reason of at least {MIN_REASON} bytes saying why none is needed. A phase kept \
         out by nothing and explained by nothing is the `set_extensions` defect again.",
        entry.name
      );
    }

    // Axis 3: the operation kind. Reading `self.phase` and naming the stream states it admits are
    // two halves of one claim, and each is the other's check — a row that names states while the
    // body tests nothing is a caption, and a body that tests the phase while the row names nothing
    // is a guard no column records.
    let reads_phase = declared.guards.contains(&Guard::Stream);
    assert_eq!(
      reads_phase,
      !declared.stream.is_empty(),
      "`{}` declares the stream states {:?} and {} `self.phase`. A draft §6.2.3 entry point is \
       exactly the one that reads the operation-kind slot, so the two have to agree.",
      entry.name,
      declared.stream,
      if reads_phase {
        "reads"
      } else {
        "does not read"
      }
    );

    // A method legal everywhere on *both* axes has nothing to guard and nothing to argue. The
    // stream column is part of the condition rather than beside it: draft §6.2.3's terminations are
    // legal in every execution phase and still refuse three of the five stream states, so a row
    // naming any stream state is performing a guard whatever its `meaningful` says.
    if declared.meaningful.len() == ALL_PHASES.len() && declared.stream.is_empty() {
      assert!(
        declared.guards.is_empty() && declared.structural.is_empty(),
        "`{}` is declared meaningful in every phase, under every operation kind, and yet carries a \
         guard or a structural reason. One of the two claims is wrong.",
        entry.name
      );
    }
  }

  for row in ENTRY_PROTOCOL {
    assert!(
      entries.iter().any(|entry| entry.name == row.name),
      "`ENTRY_PROTOCOL` names `{}`, which is no longer a public `&mut self` method. Either it was \
       renamed and the table is stale, or the reader stopped finding the surface.",
      row.name
    );
  }
}

/// A driver value that says how many of itself are alive.
///
/// The in-crate twin of `smear/tests/proto_execute.rs`'s `Counted`, and it is here rather than only
/// there because this file is where the entry-point table lives: the behavioural half has to read
/// the same list as the derived half, and a list crossed with itself in two crates is a list that
/// drifts.
#[derive(Debug)]
struct Tracked {
  live: std::rc::Rc<core::cell::Cell<usize>>,
  payload: Value,
}

impl Tracked {
  fn new(live: &std::rc::Rc<core::cell::Cell<usize>>, payload: Value) -> Self {
    live.set(live.get() + 1);
    Self {
      live: std::rc::Rc::clone(live),
      payload,
    }
  }
}

impl Drop for Tracked {
  fn drop(&mut self) {
    self.live.set(self.live.get() - 1);
  }
}

/// A space whose variable table hands each value **over**, so a live count is the executor's.
struct Counting {
  mint: std::rc::Rc<core::cell::Cell<usize>>,
  variables: std::vec::Vec<(&'static str, Tracked)>,
}

impl Values for Counting {
  type Value = Tracked;

  fn is_null(&self, _: &Tracked) -> bool {
    false
  }
  fn as_bool(&self, _: &Tracked) -> Option<bool> {
    None
  }
  fn list_len(&self, value: &Tracked) -> Option<usize> {
    match value.payload {
      Value::List(len) => Some(len),
      _ => None,
    }
  }
  fn list_item(&mut self, _: &Tracked, _: usize) -> Tracked {
    Tracked::new(&self.mint, Value::Obj)
  }
  fn type_name<'a>(&'a self, _: &'a Tracked) -> Option<&'a str> {
    None
  }
  fn coerce_leaf(&mut self, value: Tracked, _: Leaf<'_>) -> Option<Tracked> {
    Some(value)
  }
  fn variable(&mut self, name: &str) -> Option<Tracked> {
    let index = self
      .variables
      .iter()
      .position(|(declared, _)| *declared == name)?;
    Some(self.variables.remove(index).1)
  }
}

const LEND_SDL: &str = r#"
type Query {
  nest: Wrap
}
type Wrap {
  echo(text: String): String
}
"#;

/// The one query that opens **both** lends at once.
///
/// `nest` has exactly one child, so offering `echo` is the last enqueued child departing and parks
/// `nest` in `deferred`; and `echo` takes a variable argument, so draft §6.4.1's checked value is
/// sitting in `scratch_args` at the same moment.
const LEND_QUERY: &str = r#"query ($text: String) { nest { echo(text: $text) } }"#;

/// Drives an executor to the point where a dropped `FieldRequest` has left both lends open, then
/// hands it to `call`.
///
/// Returns `(arguments still held, tree values still held)` after `call` returned.
fn with_both_lends_open(
  call: impl FnOnce(&mut Executor<'_, &str, Counting>, ReqId, &std::rc::Rc<core::cell::Cell<usize>>),
) -> (usize, usize) {
  let (schema, document) = compile_against(LEND_SDL, LEND_QUERY);
  let arguments = std::rc::Rc::new(core::cell::Cell::new(0usize));
  let tree = std::rc::Rc::new(core::cell::Cell::new(0usize));
  let mut space = Counting {
    mint: std::rc::Rc::clone(&tree),
    variables: std::vec::from_elem((), 1)
      .into_iter()
      .map(|()| ("text", Tracked::new(&arguments, Value::Text)))
      .collect(),
  };
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Tracked::new(&tree, Value::Obj))
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("`nest`").id();
  executor.handle_resolved(&mut space, nest, Tracked::new(&tree, Value::Obj));
  let echo = executor.poll_resolve(&mut space).expect("`echo`").id();

  assert_eq!(
    arguments.get(),
    1,
    "the checked argument value is lent to the request just offered"
  );
  let parked = tree.get();
  assert!(
    parked > 0,
    "`nest` is parked in `deferred` and still holds its object value"
  );

  call(&mut executor, echo, &tree);
  (arguments.get(), tree.get())
}

/// Every public `&mut self` entry point closes the lends the previous call left open.
///
/// The behavioural half of the protocol `Executor`'s header states, and the counterpart to
/// `every_public_entry_point_declares_its_discharge`: that one proves the *table* names every
/// method, this one proves the discharge each row claims actually happens. Neither is sufficient
/// alone — a table can be complete and wrong, and a release can be real and unlisted.
///
/// Both lends are opened at once so that a method settling only one of them fails here. That is not
/// hypothetical: `on_entry` is two calls, and the defect this whole gate exists for —
/// `set_extensions` shipping without it — left *both* held.
#[test]
fn every_public_entry_point_settles_the_previous_call() {
  // Each case drops the offered `FieldRequest` before calling in, which is what makes the two
  // lends stale rather than live.
  let covered: std::vec::Vec<&str> = std::vec::Vec::from([
    "start",
    "poll_resolve",
    "handle_resolved",
    "handle_field_error",
    "poll_abandoned",
    "set_extensions",
    "take_extensions",
    "poll_response",
    "handle_source_stream",
    "handle_source_event",
    "handle_source_complete",
    "handle_source_error",
    "unsubscribe",
  ]);
  let mut names: std::vec::Vec<&str> = ENTRY_PROTOCOL.iter().map(|row| row.name).collect();
  let mut expected = covered.clone();
  names.sort_unstable();
  expected.sort_unstable();
  assert_eq!(
    names, expected,
    "this test's cases and `ENTRY_PROTOCOL` must name the same methods, or one of the two halves \
     is watching a surface the other is not"
  );

  let after = |label: &str, held: (usize, usize)| {
    assert_eq!(
      held.0, 0,
      "`{label}` left the previous request's argument value held"
    );
  };

  after(
    "start",
    with_both_lends_open(|executor, _, tree| {
      let mut space = Counting {
        mint: std::rc::Rc::clone(tree),
        variables: std::vec::Vec::new(),
      };
      executor
        .start(&mut space, None, Tracked::new(tree, Value::Obj))
        .expect("a second operation resolves");
    }),
  );
  after(
    "poll_resolve",
    with_both_lends_open(|executor, _, tree| {
      let mut space = Counting {
        mint: std::rc::Rc::clone(tree),
        variables: std::vec::Vec::new(),
      };
      let _ = executor.poll_resolve(&mut space);
    }),
  );
  after(
    "handle_resolved",
    with_both_lends_open(|executor, echo, tree| {
      let mut space = Counting {
        mint: std::rc::Rc::clone(tree),
        variables: std::vec::Vec::new(),
      };
      executor.handle_resolved(&mut space, echo, Tracked::new(tree, Value::Text));
    }),
  );
  after(
    "handle_field_error",
    with_both_lends_open(|executor, echo, _| {
      executor.handle_field_error(echo, "no");
    }),
  );
  after(
    "poll_abandoned",
    with_both_lends_open(|executor, _, _| {
      let _ = executor.poll_abandoned();
    }),
  );
  after(
    "set_extensions",
    with_both_lends_open(|executor, _, _| {
      let _ = executor.set_extensions(Extensions::new(executor.limits()));
    }),
  );
  after(
    "take_extensions",
    with_both_lends_open(|executor, _, _| {
      let _ = executor.take_extensions();
    }),
  );
  after(
    "poll_response",
    with_both_lends_open(|executor, _, _| {
      let _ = executor.poll_response();
    }),
  );

  // Draft §6.2.3's five, driven on the same *query* fixture as the other eight and on purpose. Each
  // of them refuses here — the phase is `Query`, so there is no response stream — and the release
  // has to happen anyway, because `on_entry` runs before the refusal for the reason
  // `set_extensions` does. A method that tested its phase first and released afterwards would pass
  // every subscription test and fail exactly this one.
  after(
    "handle_source_stream",
    with_both_lends_open(|executor, _, _| {
      assert!(
        !executor.handle_source_stream(),
        "a query has no draft §6.2.3.1 in progress"
      );
    }),
  );
  after(
    "handle_source_event",
    with_both_lends_open(|executor, _, tree| {
      let mut space = Counting {
        mint: std::rc::Rc::clone(tree),
        variables: std::vec::Vec::new(),
      };
      let refused = executor
        .handle_source_event(&mut space, Tracked::new(tree, Value::Obj))
        .expect_err("a query has no response stream to emit on");
      assert!(matches!(refused, SourceEventError::NotStreaming(_)));
      // The refusal hands the event back, so the value dies with this binding rather than inside
      // the executor — which is the whole reason the variant carries it.
      drop(refused.into_value());
    }),
  );
  after(
    "handle_source_complete",
    with_both_lends_open(|executor, _, _| {
      assert!(!executor.handle_source_complete(), "a query has no stream");
    }),
  );
  after(
    "handle_source_error",
    with_both_lends_open(|executor, _, _| {
      assert!(!executor.handle_source_error(), "a query has no stream");
    }),
  );
  after(
    "unsubscribe",
    with_both_lends_open(|executor, _, _| {
      assert!(
        !executor.unsubscribe(),
        "a query has no response stream to cancel"
      );
    }),
  );
}

/// The deferred object value, specifically, and not only the arguments.
///
/// Split out because `on_entry` is two releases and a method could plausibly do one. The count is
/// read against the same call made through a known-good entry point, so the assertion is "these two
/// agree" rather than a number written down by hand.
#[test]
fn the_extensions_entry_points_settle_the_deferred_object_too() {
  let through_poll = with_both_lends_open(|executor, _, _| {
    let _ = executor.poll_abandoned();
  });
  for (label, held) in [
    (
      "set_extensions",
      with_both_lends_open(|executor, _, _| {
        let _ = executor.set_extensions(Extensions::new(executor.limits()));
      }),
    ),
    (
      "take_extensions",
      with_both_lends_open(|executor, _, _| {
        let _ = executor.take_extensions();
      }),
    ),
  ] {
    assert_eq!(
      held, through_poll,
      "`{label}` must leave exactly what `poll_abandoned` leaves; it settled less"
    );
  }
}

/// How many entries the lax map is grown to before it is emptied.
///
/// Chosen so the spine's capacity lands on a power of two exactly — `Vec` doubles, so 4,096
/// pushes leave 4,096 slots — and so the overshoot against the strict ceiling below is a number
/// rather than an argument. `insert` scans for a duplicate on every call, so growing a map is
/// quadratic in its own ceiling; that cost is why this is 4,096 and not a million, and it is a
/// cost the *map's* ceilings bound and the executor's do not.
const GROWN_ENTRIES: usize = 4096;

/// A map grown under a lax `Limits` and emptied does not carry its allocation into a strict
/// executor.
///
/// The two ceilings `set_extensions` re-checks bound what the map *reports* — `len` and
/// `key_bytes` — and the resource being bounded is what it *holds*. `remove` gives the key bytes
/// back to the ceiling and gives no slots back to the allocator, so an empty map can arrive
/// carrying a spine no `Limits` this executor was built with ever authorised.
///
/// Read off the executor's own field rather than through
/// [`take_extensions`](Executor::take_extensions), because the claim is about what is *retained*:
/// taking the map back is one of the three things that ends the retention.
#[test]
fn an_accepted_map_retains_no_capacity_the_executor_never_authorised() {
  let lax = Limits {
    max_extension_entries: NonZeroU32::new(GROWN_ENTRIES as u32).expect("not zero"),
    max_extension_key_bytes: NonZeroU32::new(1 << 20).expect("not zero"),
    ..Limits::default()
  };
  // The key-byte ceiling is deliberately *above* the capacity being planted, and not a small
  // number chosen to look strict. There are two ceilings a repair could read here and only one of
  // them bounds entry slots; with the other one set low, a repair reading it would shrink anyway
  // and this case would pass a bound it never checked.
  let strict = Limits {
    max_extension_entries: NonZeroU32::new(4).expect("not zero"),
    max_extension_key_bytes: NonZeroU32::new(GROWN_ENTRIES as u32 * 2).expect("not zero"),
    ..Limits::default()
  };

  let mut map: Extensions<Value> = Extensions::new(&lax);
  for index in 0..GROWN_ENTRIES {
    map
      .insert(&std::format!("k{index}"), Value::Text)
      .expect("the lax map has room");
  }
  let grown = map.capacity();
  assert!(
    grown >= GROWN_ENTRIES,
    "the map did not actually grow: {grown} slots"
  );
  for index in 0..GROWN_ENTRIES {
    map
      .remove(&std::format!("k{index}"))
      .expect("every key was inserted");
  }
  assert_eq!(map.len(), 0, "empty by every ceiling the executor reads");
  assert_eq!(map.key_bytes(), 0, "and by the other one");
  assert_eq!(
    map.capacity(),
    grown,
    "and still holding every slot: `remove` refunds the budget and never the allocation, which is \
     the whole gap this case is about"
  );

  let (schema, document) = compile("{ nest { boom } }");
  let mut space = Space;
  let mut executor = Executor::with_limits(&schema, &document, strict);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");
  executor
    .set_extensions(map)
    .expect("nothing it reports is over a ceiling");

  let retained = executor
    .extensions
    .as_ref()
    .expect("the map was accepted")
    .capacity();
  let slot = core::mem::size_of::<(std::boxed::Box<str>, Value)>();
  assert!(
    retained as u64 <= u64::from(strict.max_extension_entries.get()),
    "the executor is holding {retained} entry slots ({} bytes) under a ceiling of {}, grown to \
     {grown} under a `Limits` it never agreed to",
    retained * slot,
    strict.max_extension_entries.get()
  );
}

/// The map [`take_extensions`](Executor::take_extensions) hands back re-enters through the same
/// gate.
///
/// A returned map still carries the ceilings it was *created* under — the lax ones, because
/// nothing rewrites them on acceptance — so a driver can grow it again well past anything this
/// executor authorised and attach it a second time. What closes that is not a second check: it is
/// that `set_extensions` is the only site in this file that writes `self.extensions`, so the round
/// trip has exactly one way back in and it is the normalizing one.
#[test]
fn a_map_taken_back_and_regrown_re_enters_through_the_same_gate() {
  /// Smaller than the first case's, because this half is about the path and not the size.
  const REGROWN: usize = 256;

  // One over, because the map keeps the entry it was first attached with while it regrows.
  let lax = Limits {
    max_extension_entries: NonZeroU32::new(REGROWN as u32 + 1).expect("not zero"),
    max_extension_key_bytes: NonZeroU32::new(1 << 20).expect("not zero"),
    ..Limits::default()
  };
  // Above the regrown capacity, for the reason the case above states.
  let strict = Limits {
    max_extension_entries: NonZeroU32::new(4).expect("not zero"),
    max_extension_key_bytes: NonZeroU32::new(REGROWN as u32 * 8).expect("not zero"),
    ..Limits::default()
  };

  let (schema, document) = compile("{ nest { boom } }");
  let mut space = Space;
  let mut executor = Executor::with_limits(&schema, &document, strict);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");

  let mut map: Extensions<Value> = Extensions::new(&lax);
  map.insert("first", Value::Text).expect("room");
  executor.set_extensions(map).expect("one entry, under four");

  let mut back = executor.take_extensions().expect("the map comes back");
  for index in 0..REGROWN {
    back
      .insert(&std::format!("k{index}"), Value::Text)
      .expect("the ceilings that came back with it are still the lax ones");
  }
  assert!(
    back.capacity() > strict.max_extension_entries.get() as usize,
    "the second growth has to actually exceed the executor's ceiling, or this case proves nothing"
  );
  for index in 0..REGROWN {
    back
      .remove(&std::format!("k{index}"))
      .expect("every key was inserted");
  }

  executor
    .set_extensions(back)
    .expect("one entry again, under four");
  let retained = executor
    .extensions
    .as_ref()
    .expect("the map was accepted")
    .capacity();
  assert!(
    retained as u64 <= u64::from(strict.max_extension_entries.get()),
    "a second pass through the gate retained {retained} slots under a ceiling of {}",
    strict.max_extension_entries.get()
  );
}

// ---------------------------------------------------------------------------------------
// draft §6.2.3 Subscription, and draft §7.1.2 Response Stream
// ---------------------------------------------------------------------------------------

const SUB_SDL: &str = r#"
type Query {
  a: String
}
type Subscription {
  newMessage(roomId: ID!): Message
  other: String
}
type Message {
  sender: String
  text: String
}
"#;

/// The chat subscription draft §6.2.3 itself uses as its example, with a literal argument.
const SUB_QUERY: &str = r#"subscription NewMessages { newMessage(roomId: "123") { sender text } }"#;

/// A subscription whose event can be made as large as the ceilings admit, for the retained-buffer
/// census.
///
/// A list under the source field is what lets a *driver* quantity drive the response size, which is
/// the only way to reach the position and metadata ceilings from an event; `feed`'s two arguments
/// are the schema's widest list and therefore the bound on `scratch_args`.
const CENSUS_SDL: &str = r#"
type Query {
  a: String
}
type Subscription {
  feed(topic: ID!, since: String): Cell
}
type Cell {
  rows: [Row]
}
type Row {
  text: String
  other: String
}
"#;

/// Two fragments, so draft §6.3's `visitedFragments` and the fragment index are populated rather
/// than left at zero — a census row that is always empty checks nothing.
const CENSUS_QUERY: &str = r#"
subscription Feed { feed(topic: "t") { rows { ...Body } } }
fragment Body on Row { text ...Tail }
fragment Tail on Row { other }
"#;

/// One field with arguments, aliased enough times that a buffer accumulating across candidates
/// leaves the schema's widest argument list behind.
///
/// Twelve rather than three, because a `Vec`'s first allocation is four elements: a bound of two
/// and an accumulation of three are the same capacity, so a smaller fixture would go green under
/// the very fault this is for.
const ARGUMENTS_SDL: &str = r#"
type Query {
  a: String
}
type Subscription {
  feed(topic: ID!, since: String): Cell
}
type Cell {
  bad(pad: String, must: String!): String
}
"#;

/// Every alias supplies `must` from a variable [`Space`] does not answer, so draft §6.4.1 step 5.f
/// raises **after** `pad` has been pushed — which is the one shape that leaves anything behind.
const ARGUMENTS_QUERY: &str = r#"
subscription Feed($missing: String!) {
  feed(topic: "t", since: "s") {
    f1: bad(pad: "p", must: $missing)
    f2: bad(pad: "p", must: $missing)
    f3: bad(pad: "p", must: $missing)
    f4: bad(pad: "p", must: $missing)
    f5: bad(pad: "p", must: $missing)
    f6: bad(pad: "p", must: $missing)
    f7: bad(pad: "p", must: $missing)
    f8: bad(pad: "p", must: $missing)
    f9: bad(pad: "p", must: $missing)
    f10: bad(pad: "p", must: $missing)
    f11: bad(pad: "p", must: $missing)
    f12: bad(pad: "p", must: $missing)
  }
}
"#;

/// Runs one draft §6.2.3.2 event to its execution result, answering every field, and returns what
/// the event charged against the four cumulative ceilings.
fn drive_event(executor: &mut Executor<'_, &str, Space>, space: &mut Space) -> super::Charges {
  executor
    .handle_source_event(space, Value::Obj)
    .expect("the stream is open and the previous result was taken");
  while let Some(request) = executor.poll_resolve(space) {
    let id = request.id();
    executor.handle_resolved(space, id, Value::Text);
  }
  // Read before the response is taken, because a `Response` borrows the executor.
  let charges = executor.charges();
  let response = executor
    .poll_response()
    .expect("every field of the event was answered");
  assert_eq!(response.error_count(), 0, "the event resolved cleanly");
  charges
}

/// Draft §6.2.3.1 leaves the machine holding a source field and no execution.
#[test]
fn creating_a_source_event_stream_begins_no_execution() {
  let (schema, document) = compile_against(SUB_SDL, SUB_QUERY);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the subscription resolves");

  assert_eq!(executor.response_stream(), Some(ResponseStream::Creating));
  let source = executor.source_field().expect("§6.2.3.1 chose a field");
  assert_eq!(source.name(), "newMessage");
  assert_eq!(source.parent_type(), "Subscription");
  assert_eq!(source.arguments().len(), 1);
  assert_eq!(source.arguments()[0].name(), "roomId");

  // §6.2.3.1 runs `CollectFields` and `CoerceArgumentValues` and stops. Nothing is resolvable and
  // nothing is deliverable, because the first execution belongs to the first *event*.
  assert!(executor.poll_resolve(&mut space).is_none());
  assert!(executor.poll_response().is_none());
}

/// A no-op call in while draft §6.2.3.1 is still running does not empty the source field's
/// arguments.
///
/// **The fault this names is a silent wrong answer, not a leak.** Those arguments are step 8's,
/// coerced by `start` and lent by `source_field` for as long as the stream is `Creating`; every
/// entry point opens with `on_entry`, and `on_entry`'s ordinary job is to retire the arguments of
/// the request the *previous* call was answering. In `Creating` there is no such request, so an
/// unconditional retirement would empty `scratch_args` on the way through any of six calls that
/// otherwise do nothing at all — and `source_field` would then hand the driver a source field with
/// **no arguments**, indistinguishable from a field that has none. The driver would call
/// `ResolveFieldEventStream` for the wrong room.
///
/// Every one of the six is driven, because the condition lives in `on_entry` rather than in any of
/// them: a repair that special-cased one call would pass a one-call test.
#[test]
fn a_call_in_while_creating_keeps_the_source_field_arguments() {
  let (schema, document) = compile_against(SUB_SDL, SUB_QUERY);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the subscription resolves");

  let stale = ReqId {
    epoch: 0,
    index: 0,
    generation: 0,
  };
  let _ = executor.poll_resolve(&mut space);
  let _ = executor.poll_response();
  let _ = executor.poll_abandoned();
  let _ = executor.take_extensions();
  let _ = executor.set_extensions(Extensions::new(executor.limits()));
  executor.handle_resolved(&mut space, stale, Value::Text);
  executor.handle_field_error(stale, "no");

  let source = executor
    .source_field()
    .expect("draft §6.2.3.1 is still running");
  assert_eq!(
    source.arguments().len(),
    1,
    "six calls that do nothing must not have retired draft §6.2.3.1 step 8's answer"
  );
  assert_eq!(source.arguments()[0].name(), "roomId");
}

/// Draft §6.2.3.2 maps each source event to one execution result, and the intake is what makes that
/// true.
///
/// **The fault: a lost execution result and a stream out of order.** Without the gate, an event
/// pushed while the previous event's result is still undelivered resets over it — the response the
/// specification requires on the stream is discarded, and nothing anywhere says so. A driver
/// pulling from a source faster than it serialises would silently drop payloads.
#[test]
fn a_source_event_is_refused_while_the_previous_result_is_undelivered() {
  let (schema, document) = compile_against(SUB_SDL, SUB_QUERY);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the subscription resolves");
  assert!(executor.handle_source_stream());
  assert_eq!(executor.response_stream(), Some(ResponseStream::Streaming));

  executor
    .handle_source_event(&mut space, Value::Obj)
    .expect("the first event is taken");

  // Mid-execution: a request is outstanding.
  let message = executor.poll_resolve(&mut space).expect("newMessage").id();
  let refused = executor
    .handle_source_event(&mut space, Value::Obj)
    .expect_err("the first event's result has not been taken");
  assert!(matches!(refused, SourceEventError::Outstanding(_)));

  // Still refused once every field is answered, because "answered" is not "delivered": the result
  // exists and no one has taken it.
  executor.handle_resolved(&mut space, message, Value::Obj);
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    executor.handle_resolved(&mut space, id, Value::Text);
  }
  assert!(matches!(
    executor.handle_source_event(&mut space, Value::Obj),
    Err(SourceEventError::Outstanding(_))
  ));

  assert!(executor.poll_response().is_some());
  executor
    .handle_source_event(&mut space, Value::Obj)
    .expect("the previous result has been taken");
}

/// Every draft §6.2.3.2 event pays exactly what the first one did.
///
/// `a_second_operation_charges_what_the_first_did` over a stream, reading the same four cumulative
/// quantities, and it is the bound a subscription needs and a query does not: a stream is unbounded
/// in time, so a ceiling that accumulated across it would fail event *N* for work event 1 did —
/// which is a bound a client clears by reconnecting rather than a bound at all.
///
/// **The fault: `handle_source_event` beginning an event without resetting.** The cumulative
/// ceilings are per operation and §6.2.3.2 makes each event one whole `ExecuteRootSelectionSet`, so
/// the reset is what re-arms them; it is also what releases the previous event's driver values.
/// Three events rather than two, so that a repair which merely halved the growth still fails.
#[test]
fn each_source_event_charges_what_the_first_did() {
  let (schema, document) = compile_against(SUB_SDL, SUB_QUERY);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the subscription resolves");
  assert!(executor.handle_source_stream());

  let first = drive_event(&mut executor, &mut space);
  let second = drive_event(&mut executor, &mut space);
  let third = drive_event(&mut executor, &mut space);
  assert_eq!(
    first, second,
    "the second event must charge what the first did"
  );
  assert_eq!(second, third, "and so must every event after it");
}

/// Draft §6.2.3.1 step 5's request error, over every shape that reaches it.
///
/// **The fault: taking the first collected entry instead of requiring exactly one.** That is what
/// `graphql-js` 16.11.0 does — `[...rootFields.entries()][0]` — and the draft added the check
/// precisely because it silently streams one field of a subscription that asked for two. The
/// undefined-field row is the one a count of *positions* rather than of collected entries would
/// pass: `expand` creates no slot for a field the schema does not define, so `{ nope newMessage }`
/// is one position and two entries.
#[test]
fn a_subscription_root_must_name_exactly_one_field_with_an_event_stream() {
  for (query, why) in [
    (
      "subscription { newMessage(roomId: \"1\") { sender } other }",
      "two entries",
    ),
    (
      "subscription { nope }",
      "a field the schema does not define",
    ),
    (
      "subscription { nope newMessage(roomId: \"1\") { sender } }",
      "two entries, one of which creates no position",
    ),
    (
      "subscription { __typename }",
      "draft §4.4's meta-field, which has no event stream",
    ),
  ] {
    let (schema, document) = compile_against(SUB_SDL, query);
    let mut space = Space;
    let mut executor = Executor::new(&schema, &document);
    assert_eq!(
      executor.start(&mut space, None, Value::Obj),
      Err(StartError::NoSourceField),
      "{why}: {query}"
    );
    assert_eq!(
      executor.response_stream(),
      None,
      "a refused `start` runs no subscription, so there is no stream to report"
    );
  }
}

/// Draft §6.2.3.1 step 8's failure is a *request* error, and refusing leaves nothing behind.
///
/// **The fault: letting §6.4.1's ordinary field error stand.** Everywhere else in draft §6 an
/// argument refusal nulls the field and lands in the response's `errors`; here there is no response
/// yet, so the same code path would produce a §7.1.1 execution result with `"data": null` where
/// §7.1.3 requires a map with **no `data` entry at all**. `graphql-js` returns `{errors: [...]}`
/// from `createSourceEventStream` for exactly this case.
#[test]
fn a_source_field_argument_error_refuses_the_subscription() {
  let (schema, document) = compile_against(
    SUB_SDL,
    "subscription ($room: ID!) { newMessage(roomId: $room) { sender } }",
  );
  // `Space::variable` supplies nothing, which is draft §6.4.1 step 5.f's `hasValue` false at a
  // non-null argument with no default.
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  assert_eq!(
    executor.start(&mut space, None, Value::Obj),
    Err(StartError::SourceFieldArguments)
  );
  assert_eq!(executor.response_stream(), None);
  assert!(
    executor.source_field().is_none(),
    "a refused §6.2.3.1 has no source field"
  );
  assert!(
    executor.poll_response().is_none(),
    "and builds no response, because §7.1.3's result has no `data` to build"
  );
}

/// Collecting the subscription's root selection set can be refused by a ceiling, and that is a
/// different refusal from a root that names the wrong number of fields.
#[test]
fn a_refused_root_collection_is_its_own_request_error() {
  let (schema, document) = compile_against(SUB_SDL, SUB_QUERY);
  let mut space = Space;
  let one = NonZeroU32::new(1).expect("one is not zero");
  // One position is the root's, so there is no room for the source field's — a ceiling refusal
  // rather than a document that names the wrong number of fields.
  let mut executor = Executor::with_limits(
    &schema,
    &document,
    Limits {
      max_response_slots: one,
      ..Limits::default()
    },
  );
  assert_eq!(
    executor.start(&mut space, None, Value::Obj),
    Err(StartError::SourceSelectionRefused),
    "the remedy is a larger budget, not a different document"
  );
  assert_eq!(executor.response_stream(), None);
}

/// A schema with no `subscription` root refuses before anything is collected.
#[test]
fn a_schema_with_no_subscription_root_refuses() {
  let (schema, document) = compile_against("type Query { a: String }", "subscription { a }");
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  assert_eq!(
    executor.start(&mut space, None, Value::Obj),
    Err(StartError::NoSubscriptionRoot)
  );
}

/// Draft §6.2.3.2's three completions are terminal, and each is its own state.
///
/// **The fault: a completed response stream that still takes events.** §6.2.3.2 completes the
/// response stream on each of the three, and an execution result emitted afterwards is one on a
/// stream the specification says has ended. The `Cancelled`/`Completed` distinction is the second
/// half: §6.2.3.2's cancellation arm also says "Cancel {sourceStream}", which is the driver's to
/// do and which it can only know it owes by reading a state that is not `Completed`.
#[test]
fn every_completion_is_terminal_and_says_which_one_it_was() {
  // The three are named rather than passed as function pointers: a `&dyn Fn` over a method of a
  // type with two lifetime parameters is not general enough for the loop's borrow, and the `match`
  // is exhaustive over the states that end a stream anyway.
  let ends = |executor: &mut Executor<'_, &str, Space>, end: ResponseStream| match end {
    ResponseStream::Completed => executor.handle_source_complete(),
    ResponseStream::Failed => executor.handle_source_error(),
    ResponseStream::Cancelled => executor.unsubscribe(),
    ResponseStream::Creating | ResponseStream::Streaming => {
      unreachable!("the two open states are not completions")
    }
  };
  for end in [
    ResponseStream::Completed,
    ResponseStream::Failed,
    ResponseStream::Cancelled,
  ] {
    let (schema, document) = compile_against(SUB_SDL, SUB_QUERY);
    let mut space = Space;
    let mut executor = Executor::new(&schema, &document);
    executor
      .start(&mut space, None, Value::Obj)
      .expect("the subscription resolves");
    assert!(executor.handle_source_stream());
    let _ = drive_event(&mut executor, &mut space);

    assert!(ends(&mut executor, end), "{end:?} ends an open stream");
    assert_eq!(executor.response_stream(), Some(end));
    assert!(!end.is_open());
    assert!(!ends(&mut executor, end), "{end:?} does not end twice");

    let refused = executor
      .handle_source_event(&mut space, Value::Obj)
      .expect_err("the response stream has completed");
    assert!(matches!(refused, SourceEventError::NotStreaming(_)));
  }
}

/// Draft §6.2.3.3 is legal before the source stream exists, and the two live states are the only
/// ones it accepts.
#[test]
fn unsubscribing_is_legal_while_the_source_stream_is_still_being_created() {
  let (schema, document) = compile_against(SUB_SDL, SUB_QUERY);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the subscription resolves");
  assert_eq!(executor.response_stream(), Some(ResponseStream::Creating));
  assert!(executor.unsubscribe());
  assert_eq!(executor.response_stream(), Some(ResponseStream::Cancelled));
  assert!(
    executor.source_field().is_none(),
    "a cancelled §6.2.3.1 has released the field it was offering"
  );
  assert!(
    !executor.handle_source_stream(),
    "and a stream reported afterwards belongs to nothing"
  );
}

/// A field error inside an event lands in that event's execution result and the stream goes on.
///
/// §6.2.3.2's note is the whole of it: `ExecuteSubscriptionEvent` "handles all *execution error*",
/// and the only condition that completes the response stream with an error is an internal one this
/// executor cannot raise. So an event that fails is an execution result with `errors`, exactly as a
/// query's would be, and the next event is taken normally.
#[test]
fn a_field_error_in_an_event_does_not_end_the_response_stream() {
  let (schema, document) = compile_against(SUB_SDL, SUB_QUERY);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the subscription resolves");
  assert!(executor.handle_source_stream());

  executor
    .handle_source_event(&mut space, Value::Obj)
    .expect("the stream is open");
  let message = executor.poll_resolve(&mut space).expect("newMessage").id();
  executor.handle_field_error(message, "the room went away");
  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 1);
  assert!(matches!(response.data(), Node::Object(_)));

  assert_eq!(
    executor.response_stream(),
    Some(ResponseStream::Streaming),
    "an execution error is carried by the event, not by the stream"
  );
  let clean = drive_event(&mut executor, &mut space);
  assert!(clean.slots > 0, "and the next event executes normally");
}

/// Reporting the source stream releases everything draft §6.2.3.1 was holding.
///
/// **The fault: a driver handle held for the life of the subscription.** The `initialValue` is dead
/// the moment the source stream exists — each event brings its own root — and a subscription lasts
/// as long as its client does, so waiting for the first event to release it holds a wasm handle, a
/// pooled buffer or a database cursor open across a stream that may legitimately never produce one.
#[test]
fn reporting_the_source_stream_releases_the_initial_value() {
  let (schema, document) = compile_against(SUB_SDL, SUB_QUERY);
  let live = std::rc::Rc::new(core::cell::Cell::new(0usize));
  let mut space = Counting {
    mint: std::rc::Rc::clone(&live),
    variables: std::vec::Vec::new(),
  };
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Tracked::new(&live, Value::Obj))
    .expect("the subscription resolves");
  assert_eq!(
    live.get(),
    1,
    "draft §6.2.3.1's `initialValue` is held while the source field is offered"
  );

  assert!(executor.handle_source_stream());
  assert_eq!(
    live.get(),
    0,
    "and released the instant the source stream exists"
  );
}

/// A source stream that ends while an event's result is undelivered still emits that result.
/// An ending reported mid-event is a state the terminator cannot skip past.
///
/// **The fault: draft §6.2.3.2's one-result-per-event mapping, broken by the ordinary behaviour of
/// a push source.** A source that emits its final value and completes back to back reaches the
/// terminator with an event accepted and its execution result untaken, and the shipped draft
/// `reset` over it — data, errors, the §7.1.7 map and both execution flags — so `poll_response`
/// could never emit the result the specification requires. The ordering was a sentence in a
/// comment claiming the compiler enforced it; the borrow it appealed to never exists for a driver
/// that simply does not call `poll_response`.
///
/// So this drives the states the repair adds rather than only the result: the ending refuses to
/// happen twice, the intake still refuses, the event is still *running* enough to accept a §7.1.7
/// map, and the recorded ending arrives exactly when the result is taken. Both §6.2.3.2 arms,
/// because they are two bodies and a repair to one is not a repair to the other.
#[test]
fn an_ending_reported_mid_event_still_emits_that_events_result() {
  for end in [ResponseStream::Completed, ResponseStream::Failed] {
    let (schema, document) = compile_against(SUB_SDL, SUB_QUERY);
    let mut space = Space;
    let mut executor = Executor::new(&schema, &document);
    executor
      .start(&mut space, None, Value::Obj)
      .expect("the subscription resolves");
    assert!(executor.handle_source_stream());

    executor
      .handle_source_event(&mut space, Value::Obj)
      .expect("the stream is open");
    while let Some(request) = executor.poll_resolve(&mut space) {
      let id = request.id();
      executor.handle_resolved(&mut space, id, Value::Text);
    }

    let ended = match end {
      ResponseStream::Completed => executor.handle_source_complete(),
      _ => executor.handle_source_error(),
    };
    assert!(ended, "{end:?} ends an open stream");
    assert_eq!(
      executor.response_stream(),
      Some(ResponseStream::Streaming),
      "a stream that still owes an execution result has not completed"
    );
    assert!(
      !executor.handle_source_complete() && !executor.handle_source_error(),
      "and the ending does not happen twice"
    );
    // `NotStreaming` and not `Outstanding`, and the difference is the driver's remedy: the source
    // has ended, so no retry of this event can ever succeed and the thing to do is take the last
    // result. It is the one case where the refusal says "not streaming" while the stream reads
    // `Streaming`, and `SourceEventError::NotStreaming` records why the two agree.
    assert!(matches!(
      executor.handle_source_event(&mut space, Value::Obj),
      Err(SourceEventError::NotStreaming(_))
    ));

    // The event is still Running, which is what makes it able to owe a result at all — so draft
    // §7.1.7's setter still accepts, and the map reaches the response the ending was queued behind.
    let mut extensions = Extensions::new(executor.limits());
    extensions
      .insert("seq", Value::Text)
      .expect("well under the ceiling");
    executor
      .set_extensions(extensions)
      .expect("the event is running and its result is not delivered");

    {
      let response = executor
        .poll_response()
        .expect("the accepted event owes an execution result");
      assert!(matches!(response.data(), Node::Object(_)));
      assert!(response.extensions().is_some());
    }
    assert_eq!(
      executor.response_stream(),
      Some(end),
      "and taking it is the transition into the ending that was recorded"
    );
    assert!(matches!(
      executor.handle_source_event(&mut space, Value::Obj),
      Err(SourceEventError::NotStreaming(_))
    ));
  }
}

/// `start` over an open response stream refuses rather than orphaning the driver's source stream.
///
/// **The fault: one leaked pub-sub subscription, cursor or task per restart, with no observable.**
/// `start` reset before it looked at the phase, so an open `Streaming` was replaced without ever
/// passing through `Cancelled` — and `Cancelled` read off `response_stream()` is the only channel
/// draft §6.2.3.3's driver-side obligation travels on. The refusal is inert on purpose: the phase,
/// the event and the obligation are all exactly where they were, and `unsubscribe` is the way
/// through.
#[test]
fn a_start_refuses_while_a_response_stream_is_open() {
  let query = "query Plain { a } subscription NewMessages { newMessage(roomId: \"1\") { sender } }";
  let (schema, document) = compile_against(SUB_SDL, query);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, Some("NewMessages"), Value::Obj)
    .expect("the subscription resolves");
  assert!(executor.handle_source_stream());
  executor
    .handle_source_event(&mut space, Value::Obj)
    .expect("the stream is open");

  let refused = executor
    .start(&mut space, Some("Plain"), Value::Obj)
    .expect_err("a live source stream would be orphaned");
  assert_eq!(refused, StartError::ResponseStreamOpen);
  assert_eq!(
    executor.response_stream(),
    Some(ResponseStream::Streaming),
    "the refusal changes nothing"
  );
  assert!(executor.unsubscribe());
  assert_eq!(executor.response_stream(), Some(ResponseStream::Cancelled));
  executor
    .start(&mut space, Some("Plain"), Value::Obj)
    .expect("the obligation was published and the query starts");

  // `Creating` is open too, and it is the leg a narrower repair would miss: the driver may already
  // have called its resolver and be holding the stream it has not reported yet, which is exactly
  // the window in which nothing else knows the stream exists.
  executor
    .start(&mut space, Some("NewMessages"), Value::Obj)
    .expect("the subscription resolves");
  assert_eq!(executor.response_stream(), Some(ResponseStream::Creating));
  assert_eq!(
    executor.start(&mut space, Some("Plain"), Value::Obj),
    Err(StartError::ResponseStreamOpen)
  );
  assert!(
    executor.source_field().is_some(),
    "and draft §6.2.3.1 is exactly where it was, arguments included"
  );
}

/// Draft §6.4.1's coerced arguments never hold more than one field's declared arguments.
///
/// **The bound the census names for `scratch_args`, at the mechanism that produces it.** No
/// [`Limits`] ceiling charges `coerce_arguments`; what bounds the buffer is that the function
/// *clears* and then pushes at most one entry per argument the schema declares on the one field it
/// is coercing, so the peak is the schema's widest argument list and nothing a request sends moves
/// it.
///
/// One `poll_resolve` call is where that could go wrong, and it is the only place: the loop walks
/// past every candidate draft §6.4.1 refuses, so without the clear each refusal's already-checked
/// arguments would stay and the buffer would grow with the *document's* field count instead. The
/// census gate cannot see it — its document has one field with arguments, which is one push either
/// way — and a bound with no gate at its own mechanism is a sentence.
#[test]
fn coerced_arguments_never_outgrow_one_fields_declared_arguments() {
  /// `feed` and `bad` both declare two, which is the whole schema's widest.
  const WIDEST: usize = 2;

  let (schema, document) = compile_against(ARGUMENTS_SDL, ARGUMENTS_QUERY);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the subscription resolves");
  assert!(executor.handle_source_stream());
  executor
    .handle_source_event(&mut space, Value::Obj)
    .expect("the stream is open");

  let feed = executor.poll_resolve(&mut space).expect("feed").id();
  executor.handle_resolved(&mut space, feed, Value::Obj);
  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "every alias refuses coercion, so one call walks all twelve"
  );
  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(
    response.error_count(),
    12,
    "one draft §6.4.1 refusal per alias, which is what put the twelve candidates in one call"
  );

  let held = executor.scratch_args.capacity();
  assert!(
    held <= 2 * WIDEST.max(4),
    "`scratch_args` is holding {held} after twelve refused coercions, against a schema whose \
     widest argument list is {WIDEST} — so it is accumulating across candidates rather than \
     clearing at each one, and its only bound is gone"
  );
}

/// A subscription that never ends retains no buffer that grows with it.
///
/// **The fault this names is one a charge model structurally cannot see.**
/// `each_source_event_charges_what_the_first_did` reads what an event *spent* — and `reset` puts
/// every spend back to zero while deliberately shrinking nothing, so a buffer that kept growing
/// would leave those four quantities identical event after event. A subscription is unbounded in
/// time, so "what is still allocated" is the question that decides whether this phase is safe, and
/// `Retained` is the only thing that asks it.
///
/// Two halves, and neither is sufficient. **Bounded**: every buffer is inside a quantity the client
/// cannot move — a [`Limits`] ceiling for all but three, and for those three the document's or the
/// schema's own size, which is the row `scratch_args` needed and did not have. **Not growing**: the
/// whole census is identical at event 2 and event 20, over events that deliberately vary, so a
/// buffer creeping upward by a constant per event fails here even though every ceiling still admits
/// it.
///
/// The `2 *` in the bounds is a `Vec`'s doubling and nothing else: a push past capacity reserves
/// twice what is there, so a length bound bounds the capacity within a constant factor. A constant
/// factor is what "bounded" means here; a *client* factor is what it must not be.
#[test]
fn no_buffer_a_subscription_retains_grows_with_the_stream() {
  const SLOTS: u32 = 48;
  const METADATA: u32 = 64;
  const IN_FLIGHT: u32 = 8;
  const INTERNED: u32 = 512;
  const VISITS: u32 = 4_096;
  /// Fragment definitions in `CENSUS_QUERY`, which is what `visited` and the index are sized by.
  const FRAGMENTS: usize = 2;
  /// The widest argument list the schema declares on any one field, which is what bounds
  /// `scratch_args` — no ceiling does.
  const SCHEMA_ARGUMENTS: usize = 2;

  let limits = Limits {
    max_in_flight: NonZeroU32::new(IN_FLIGHT).expect("not zero"),
    max_response_slots: NonZeroU32::new(SLOTS).expect("not zero"),
    max_response_metadata: NonZeroU32::new(METADATA).expect("not zero"),
    max_interned_bytes: NonZeroU32::new(INTERNED).expect("not zero"),
    max_selection_visits: NonZeroU32::new(VISITS).expect("not zero"),
    ..Limits::default()
  };
  let (schema, document) = compile_against(CENSUS_SDL, CENSUS_QUERY);
  let mut space = Space;
  let mut executor = Executor::with_limits(&schema, &document, limits);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the subscription resolves");
  assert!(executor.handle_source_stream());

  // Event 1 is the one that reaches every peak: its list is long enough to be refused by the
  // position ceiling, which is the largest response this executor can be made to build.
  let mut settled = None;
  for event in 0..20u32 {
    let rows = if event == 0 { usize::from(u8::MAX) } else { 1 };
    executor
      .handle_source_event(&mut space, Value::Obj)
      .expect("the stream is open and the last result was taken");
    while let Some(request) = executor.poll_resolve(&mut space) {
      let id = request.id();
      let value = match request.name() {
        "rows" => Value::List(rows),
        "feed" => Value::Obj,
        _ => Value::Text,
      };
      executor.handle_resolved(&mut space, id, value);
    }
    while executor.poll_abandoned().is_some() {}
    assert!(executor.poll_response().is_some(), "event {event} resolved");
    if event == 1 {
      settled = Some(executor.retained());
    }
  }
  let settled = settled.expect("event 1 ran");
  assert_eq!(
    settled,
    executor.retained(),
    "eighteen more events, and not one buffer moved"
  );

  // And every one of them is inside something the client cannot choose. The comment on each row is
  // the bound's source, which is the half a number alone does not carry.
  let held = executor.retained();
  let ceiling = |bound: u64| -> u64 { 2 * bound.max(4) };
  for (what, capacity, bound) in [
    // `push_child`, the sole creator of a position, charged against the position ceiling.
    ("slots", held.slots, u64::from(SLOTS)),
    // One per position, pushed on the same line.
    ("meta", held.meta, u64::from(SLOTS)),
    // `expand`'s commit, charged against the metadata ceiling.
    ("merged", held.merged, u64::from(METADATA)),
    // `expand` and `fail_at`, both charged against the metadata ceiling.
    ("locations", held.locations, u64::from(METADATA)),
    // `Interner::insert`, refused past the arena ceiling. A name is at least one byte, so entries
    // cannot outrun bytes.
    ("interner entries", held.interner.0, u64::from(INTERNED)),
    ("interner bytes", held.interner.1, u64::from(INTERNED)),
    // `fail`/`fail_at`, derived: at most one row per position.
    ("errors", held.errors, u64::from(SLOTS)),
    // `poll_resolve`, which withholds at the in-flight ceiling.
    ("inflight", held.inflight, u64::from(IN_FLIGHT)),
    // The walk's staging buffer, charged against the metadata ceiling before each push.
    ("scratch.fields", held.scratch.fields, u64::from(METADATA)),
    // One per distinct response key, so at most one per staged field.
    ("scratch.groups", held.scratch.groups, u64::from(METADATA)),
    // Sparse over interner ids, and an id is only minted by an insertion the arena admitted.
    ("scratch.keys", held.scratch.keys, u64::from(INTERNED)),
    // Draft §6.3's `visitedFragments`: a bitset word per 64 fragment ordinals, and the ordinals
    // set. **The document's size, not a ceiling** — every ordinal comes from the fragment index.
    ("scratch.visited words", held.scratch.visited.0, 1),
    (
      "scratch.visited seen",
      held.scratch.visited.1,
      FRAGMENTS as u64,
    ),
    // One frame per fragment spread or inline fragment, each charged a visit before it is taken.
    ("scratch.stack", held.scratch.stack, u64::from(VISITS)),
    // The sub-selection sets of one merged group, which is a slice of `merged`.
    ("scratch_sets", held.scratch_sets, u64::from(METADATA)),
    // **The one buffer no ceiling bounds.** `coerce_arguments` clears and then pushes at most one
    // entry per argument the *schema* declares on the one field being coerced, so the peak is the
    // schema's widest argument list — a deployment constant, like `MAX_WRAPPERS`.
    ("scratch_args", held.scratch_args, SCHEMA_ARGUMENTS as u64),
    // The fragment index, kept across every reset on purpose. **The document's size**, and its
    // pass is charged: `defs` and `chain` are one entry per fragment and `heads` is a power-of-two
    // bucket table at a load factor of a half.
    ("fragments", held.fragments, 4 * FRAGMENTS as u64),
  ] {
    assert!(
      capacity as u64 <= ceiling(bound),
      "`{what}` is holding {capacity} against a bound of {bound}, which nothing a client sends can \
       move — so either the bound is wrong or the buffer has escaped it"
    );
  }
}

/// Draft §6.2.3.3 releases the event's values too, which is what §6.2.3.3 calls the point of it.
#[test]
fn unsubscribing_releases_the_events_values() {
  let (schema, document) = compile_against(SUB_SDL, SUB_QUERY);
  let live = std::rc::Rc::new(core::cell::Cell::new(0usize));
  let mut space = Counting {
    mint: std::rc::Rc::clone(&live),
    variables: std::vec::Vec::new(),
  };
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Tracked::new(&live, Value::Obj))
    .expect("the subscription resolves");
  assert!(executor.handle_source_stream());

  executor
    .handle_source_event(&mut space, Tracked::new(&live, Value::Obj))
    .expect("the stream is open");
  let message = executor.poll_resolve(&mut space).expect("newMessage").id();
  executor.handle_resolved(&mut space, message, Tracked::new(&live, Value::Obj));
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    executor.handle_resolved(&mut space, id, Tracked::new(&live, Value::Text));
  }
  assert!(
    executor.poll_response().is_some(),
    "the event produced a result"
  );
  assert!(live.get() > 0, "whose leaves the executor is holding");

  assert!(executor.unsubscribe());
  assert_eq!(
    live.get(),
    0,
    "draft §6.2.3.3 is where the subscription's resources are cleaned up"
  );
}

/// The saturation value is refused, so the public maximum does not restore an unbounded byte pass.
///
/// # Arithmetic, because the input is thirty-two gibibytes
///
/// `byte_units` is `len / 8 + 1` and saturates at [`u32::MAX`], which a length of about thirty-two
/// gibibytes reaches — and **every larger length produces the same number**, so past that point the
/// charge has stopped being a function of the length at all. `Visits::take` used to be
/// `left.checked_sub(work)` and nothing else, which is a complete bound for every charge a *count*
/// can produce and not for that one: `Limits::max_selection_visits` is a `NonZeroU32` an operator
/// may set to `u32::MAX`, `left` therefore starts at `u32::MAX`, and the saturated charge fits
/// exactly once. `Schema::sym` then hashed the whole thing on a ledger that believed it had paid.
///
/// The name is not constructible in a test, so what is pinned here is the arithmetic that decides
/// it — the shipped expression against the replacement over the same inputs, exactly as
/// `an_overflowing_charge_refuses_at_the_largest_limit_too` pins `Work::take` one crate over. This
/// is that finding, in the sibling ledger: the byte charges that made a poison necessary were added
/// to both, and the poison to one. al8n/smear#196.
///
/// **The plant.** Delete the `work == u32::MAX` arm from `Visits::take` and the first assertion
/// reads `true` — the saturated charge accepted, and every larger name accepted with it.
#[test]
fn the_saturated_byte_charge_is_refused_at_the_largest_limit_too() {
  use crate::collect::{Visits, byte_units};

  /// The smallest length whose `byte_units` saturates: `len / 8 + 1 > u32::MAX`.
  const SATURATING: usize = (u32::MAX as usize) * 8;

  assert_eq!(
    byte_units(SATURATING),
    u32::MAX,
    "the fixture is aimed at the saturation and this length no longer reaches it"
  );
  assert_eq!(
    byte_units(SATURATING * 4),
    u32::MAX,
    "and four times the name costs the same, which is the whole reason the value is poison rather \
     than a quantity"
  );

  // The largest budget `Limits::max_selection_visits` accepts, which is where the defect lived.
  let mut visits = Visits::new(u32::MAX);
  assert!(
    !visits.take_bytes(SATURATING),
    "a saturated byte charge fits `checked_sub` exactly once at this limit, and admitting it is \
     admitting a pass whose size the ledger has stopped tracking"
  );
  assert!(
    !visits.take(u32::MAX),
    "the amount is refused however it is spelled"
  );
  assert_eq!(
    visits.spent(),
    0,
    "and the refusal spends nothing: the callers that degrade rather than raise depend on a \
     refused charge leaving the budget where it was, which is where this ledger differs from \
     `Work` and must go on differing"
  );

  // One unit below the poison is an ordinary charge and is still admitted. This ledger counts
  // *down*, so the whole budget is spendable and only the poison amount is not — which is the one
  // way it differs from `Work`, whose ceiling is a total it may not rest on.
  let mut visits = Visits::new(u32::MAX);
  assert!(
    visits.take(u32::MAX - 1),
    "one unit short of the ceiling is inside it"
  );
  assert!(visits.take(1), "and the last unit is still a unit");
  assert!(!visits.take(1), "and then there is nothing left");

  // Under an ordinary ceiling nothing moves: the limit refuses exactly where it always did.
  let mut visits = Visits::new(10);
  assert!(visits.take(10));
  assert!(!visits.take(1));
  let mut visits = Visits::new(10);
  assert!(!visits.take(u32::MAX));
  assert!(visits.take(10), "and the refusal left the budget intact");
}

/// A count charge in front of a byte pass does not pay for it: the fragment name is priced.
///
/// # The one site where the charge in front was still a count
///
/// `Table::fill` hashes every fragment name to decide its bucket, and the two charges before it
/// counted *definitions* and *fragments*. The comment on that pass defended the count on the
/// grounds that it reads every name exactly once for the executor's whole life, with no factor a
/// client can apply to it. No factor is true; it is also not the question. Draft §2.1.9 puts no
/// ceiling on a name, so one pass over one name is still a pass whose length the client chose — and
/// a **valid** document of one long fragment and the spread that selects it exhausts those two
/// counts exactly, after which `fill` hashed the whole spelling before the metered lookup behind it
/// could refuse anything.
///
/// The row below is the same document at two name lengths. Only the spelling grows, so a ledger
/// over counts reads the same number twice. al8n/smear#196.
///
/// **The plants.** Delete the `name_units` charge in `Table::charge` and the two boundaries become
/// equal, and the exhaustion case hashes its whole name on a budget that had none for it.
#[test]
fn a_fragment_names_hashing_pass_is_charged_for_its_bytes() {
  use crate::collect::byte_units;

  /// One fragment of the given name, and the spread that selects it.
  fn document(name: &str) -> std::string::String {
    std::format!("{{ ...{name} }}\nfragment {name} on Query {{ a }}\n")
  }

  let short = "F";
  let long = "F".repeat(4_000);
  let long = long.as_str();

  // Every count in front of `fill`, to the unit: draft §6.1's lookup reads both definitions
  // because no operation name is given, the root's one spread is a selection examined, the index
  // pass walks both definitions, and one fragment is pushed. At exactly this budget the counts are
  // exhausted and nothing is left for the spelling — which is the construction, and it is one unit
  // rather than a comfortable margin because a fixture that stops short of `fill` proves nothing
  // about `fill`. Verified in both directions: with the byte charge deleted this same budget hashes
  // the whole name.
  const COUNTS: u32 = 2 + 1 + 2 + 1;

  for name in [short, long] {
    let source = document(name);
    let (schema, document) = compile_against(ONE_FIELD, &source);
    let mut space = Space;
    let mut executor = Executor::with_limits(
      &schema,
      &document,
      Limits {
        max_selection_visits: NonZeroU32::new(COUNTS).expect("not zero"),
        ..Limits::default()
      },
    );
    let _ = executor.start(&mut space, None, Value::Obj);
    let hashed = executor.fragment_name_bytes_hashed();
    assert_eq!(
      hashed,
      0,
      "a budget that covers the counts and nothing else hashed {hashed} bytes of a \
       {}-byte fragment name",
      name.len()
    );
  }

  // And the total a served request pays moves with the spelling.
  let least = |name: &str| -> u32 {
    let source = document(name);
    let (schema, parsed) = compile_against(ONE_FIELD, &source);
    let clears = |limit: u32| collected_under(&schema, &parsed, limit).is_none();
    let (mut lo, mut hi) = (1u32, 1u32 << 20);
    assert!(clears(hi), "the fixture does not clear {hi} visits");
    while lo < hi {
      let mid = lo + (hi - lo) / 2;
      if clears(mid) {
        hi = mid;
      } else {
        lo = mid + 1;
      }
    }
    assert!(clears(lo) && !clears(lo - 1), "{lo} is not a boundary");
    lo
  };
  let narrow = least(short);
  let wide = least(long);
  std::println!(
    "fragment name: {narrow} visits at 1 byte, {wide} at {}",
    long.len()
  );
  assert_eq!(
    wide - narrow,
    // The name is read three times over a served request — hashed into the index, hashed again by
    // the lookup, and `memcmp`d once the bucket agrees — and only the first of those was missing.
    3 * (byte_units(long.len()) - byte_units(short.len())),
    "the whole difference between {narrow} and {wide} is the passes over the spelling, and one of \
     the three is the index pass this fixture is about"
  );
}

/// The fragment index's charge does not depend on whether this executor already built it.
///
/// # A cached table saves the walking, and must not save the verdict
///
/// `Fragments::build` re-charges the pass when the table is already there, in the same amounts and
/// the same order, precisely so that the budget's remainder is a function of the operation and
/// never of the executor's history. The byte charge had to join that. Charging it on the build path
/// alone would refuse a request on an executor's first `start` and serve the identical request on
/// its second — the table having been left behind by the first — which is the defect
/// `Names::reset` closed one crate over, arriving by a different route. A ceiling a client clears
/// by sending the request twice is not a ceiling. al8n/smear#196.
///
/// **The plant.** Drop the `name_units` re-charge from the cached branch of `Fragments::build` and
/// the second boundary falls below the first by the fragment names' own bytes.
#[test]
fn a_warm_fragment_table_charges_what_a_cold_one_charges() {
  let name = "F".repeat(2_000);
  let source = std::format!("{{ ...{name} }}\nfragment {name} on Query {{ a }}\n");
  let (schema, document) = compile_against(ONE_FIELD, &source);

  // `runs` is how many times the same executor is asked for the same operation. The first leaves
  // the table behind whenever it got far enough to build it; the second is the one that must not
  // profit from it.
  let clears = |limit: u32, runs: usize| -> bool {
    let mut space = Space;
    let mut executor = Executor::with_limits(
      &schema,
      &document,
      Limits {
        max_selection_visits: NonZeroU32::new(limit).expect("not zero"),
        ..Limits::default()
      },
    );
    let mut served = false;
    for _ in 0..runs {
      served = executor.start(&mut space, None, Value::Obj).is_ok();
      if !served {
        continue;
      }
      while let Some(request) = executor.poll_resolve(&mut space) {
        let id = request.id();
        executor.handle_resolved(&mut space, id, Value::Text);
      }
      served = executor
        .poll_response()
        .expect("nothing is outstanding")
        .error_count()
        == 0;
    }
    served
  };

  let least = |runs: usize| -> u32 {
    let (mut lo, mut hi) = (1u32, 1u32 << 20);
    assert!(clears(hi, runs), "the fixture does not clear {hi} visits");
    while lo < hi {
      let mid = lo + (hi - lo) / 2;
      if clears(mid, runs) {
        hi = mid;
      } else {
        lo = mid + 1;
      }
    }
    assert!(
      clears(lo, runs) && !clears(lo - 1, runs),
      "{lo} is not a boundary"
    );
    lo
  };

  let cold = least(1);
  let warm = least(2);
  std::println!("fragment index: {cold} visits on the first start, {warm} on the second");
  assert_eq!(
    cold, warm,
    "the same document needs {cold} visits the first time this executor is asked and {warm} the \
     second; what a cached table saves is the walking, and it must not save the verdict"
  );

  // And the table really is being reused, so the equality above is not the equality of two cold
  // runs. At a limit that serves, the first start hashes the names and the second does not.
  let mut space = Space;
  let mut executor = Executor::with_limits(
    &schema,
    &document,
    Limits {
      max_selection_visits: NonZeroU32::new(cold).expect("not zero"),
      ..Limits::default()
    },
  );
  assert!(executor.start(&mut space, None, Value::Obj).is_ok());
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    executor.handle_resolved(&mut space, id, Value::Text);
  }
  let _ = executor.poll_response();
  let after_first = executor.fragment_name_bytes_hashed();
  assert_eq!(
    after_first,
    name.len() as u64,
    "the first start must index the one fragment, or there is no warm state to test"
  );
  assert!(executor.start(&mut space, None, Value::Obj).is_ok());
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    executor.handle_resolved(&mut space, id, Value::Text);
  }
  let _ = executor.poll_response();
  assert_eq!(
    executor.fragment_name_bytes_hashed(),
    after_first,
    "the second start hashed the names again, so it is not the cached path this fixture is about"
  );
}

/// A name the arena cannot hold is not charged for the copy nobody makes.
///
/// # A charge in front of a step that can decline is a bill for work that may not happen
///
/// "Charge before the work" is this branch's whole posture, and it is right for work that *will*
/// happen. `intern` took it one step too far: after a complete lookup miss it deducted the copy's
/// bytes and **then** called `insert`, whose cap and `u32` endpoints can refuse the name before
/// `push_str` runs. The callers that meet `Unstored::Arena` degrade and carry on —
/// `handle_field_error` loses the message text and keeps the error — so an unstorable
/// thirty-two-megabyte driver message under the default sixteen-megabyte arena spent about 4.2
/// million visits it never used, and short fields behind it then failed with `CollectionBudget`
/// for a copy that never happened.
///
/// Preflight, refuse, charge, then perform a step that cannot decline. al8n/smear#196.
///
/// **The plant.** Move the `fit` call back below the copy's `take_bytes` — or restore the fallible
/// `insert` and its `ok_or` — and the refused half below spends two passes over the name instead
/// of one.
#[test]
fn a_name_the_arena_refuses_is_not_charged_for_its_copy() {
  use crate::collect::{Interner, Unstored, Visits, byte_units};

  /// An arena with room for a handful of short keys and nothing like the name below.
  const CAP: u32 = 16;

  let long = "n".repeat(4_096);
  let units = byte_units(long.len());

  // Refused by the arena: one pass over the name to hash it, and no second one.
  let mut interner = Interner::new(CAP);
  let mut visits = Visits::new(u32::MAX - 1);
  let refused = interner.intern(&long, &mut visits);
  assert!(
    matches!(refused, Err(Unstored::Arena { limit }) if limit == CAP),
    "the arena is what refuses a {}-byte name under a {CAP}-byte cap",
    long.len()
  );
  assert_eq!(
    visits.spent(),
    units,
    "a refused insertion copies nothing, so it owes one pass over the name and not two"
  );

  // And the accepted half, which is what keeps the assertion above from being a statement about a
  // name nothing reads: a name the arena *does* hold pays for the hash and for the copy.
  let short = "n";
  let mut interner = Interner::new(CAP);
  let mut visits = Visits::new(u32::MAX - 1);
  interner
    .intern(short, &mut visits)
    .expect("one byte fits a sixteen-byte arena");
  assert_eq!(
    visits.spent(),
    2 * byte_units(short.len()),
    "an insertion that happens is charged for the hash and for the copy"
  );
}
