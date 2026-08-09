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
  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
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
    executor.scratch_fields.capacity() <= CAPACITY_BOUND,
    "the staging buffer grew to {} entries against a ceiling that admits four; before the charge \
     it followed the {WIDTH}-wide selection set instead of the ceiling that refused it",
    executor.scratch_fields.capacity()
  );
  assert!(
    executor.scratch_groups.capacity() <= CAPACITY_BOUND,
    "groups grows at most one per selection, so bounding the selections bounds it — {} says \
     otherwise",
    executor.scratch_groups.capacity()
  );
}
