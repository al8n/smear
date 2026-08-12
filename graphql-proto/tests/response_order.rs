//! Draft §7.1.5's map entries are the **document's** order, whatever order the driver answers in.
//!
//! # The property, and the defect it is here to notice
//!
//! A response map's entries must appear in the order the fields appear in the document. Parallel
//! *execution* is allowed and expected; the *assembly* is positional. The two are independent, and
//! an implementation that conflates them is correct until something completes late.
//!
//! async-graphql 7.2.1 conflates them: `do_resolve_container` drives the root field futures through
//! a `FuturesUnordered` and inserts each result into the response map **as it completes**. A future
//! that finishes on its first poll comes back in hand-over order, so document order survives right
//! up until one of them yields — which is why the defect stayed latent for months behind a wire
//! suite whose fifteen response assertions were fourteen single-field documents and one that never
//! awaited.
//!
//! # Why the driver's supply order is the faithful translation, and not a timer
//!
//! This crate is Sans-I/O. There is no runtime to make a future finish late in, and reaching for
//! one would test the runtime rather than the executor. Out-of-order completion here has an exact
//! and *deterministic* expression: the driver takes every offer the executor makes and answers them
//! in an order other than the document's. [`Executor::handle_resolved`] is the moment a value lands
//! in the response, so permuting the calls to it is precisely permuting completion — with none of a
//! race's flakiness, and covering the interleaving a timer would only reach by luck.
//!
//! [`drive_backwards`] therefore answers **each batch of outstanding offers in reverse**. Reversal
//! is chosen over a hand-written permutation because it is the maximum displacement available and
//! because it needs no maintenance when a fixture grows a field; [`assert_permuted`] then checks,
//! against the document's order rather than the response's, that the driver really did answer out
//! of order, so this file cannot decay into supplying in document order and passing for the wrong
//! reason.
//!
//! That premise is read **one response map at a time**, and a global timeline cannot stand in for
//! it. A whole-run supply sequence differs from the document's serialized key sequence whatever the
//! driver does, because the serialization is depth-first while the run is level-by-level — a parent
//! has to resolve before its children exist to be offered. A whole-run `!=` is therefore satisfied
//! by the shape of execution rather than by any permutation, and it stays satisfied when every
//! sibling set arrives in perfect document order: it cannot tell a permuted run from a driver that
//! has drifted back into answering each offer as it takes it. Per map there is no such slack. A
//! map's children are offered together, the document gives them exactly one order, and requiring
//! the arrival order to differ from it is a statement about permutation and nothing else.
//!
//! # Aliases, and depth
//!
//! The response key is the *alias* when a field has one, and the alias is what a consumer compares
//! against, so each fixture here aliases one field three ways — `a: item(id: 1)`, `b: item(id: 2)`,
//! `c: item(id: 3)`. Three positions that differ only by response key and argument also rule out a
//! merge: draft §6.3 groups by response key, so anything grouping by field name instead would
//! collapse the three rather than reorder them.
//!
//! The property is recursive, and a root-only pin says nothing about children. The second case
//! therefore permutes an inner selection set inside an outer field that is itself permuted, and
//! asserts the whole flattened key sequence rather than only the top level.
//!
//! # What this file catches that the suite around it did not
//!
//! Assembly here is positional by construction — `push_child` is the sole creator of a position and
//! appends, `handle_resolved` writes a state into a position that already exists, and rendering
//! walks the sibling chain — but *by construction* is not *unrepresentable*, so the gate was
//! calibrated by planting the defect rather than by reading the code.
//!
//! The plant is `handle_resolved` moving the position to the end of its parent's child chain before
//! completing it, which is async-graphql's insert-on-completion expressed against this tree; when
//! every child of a position completes through that path exactly once, the resulting sibling order
//! *is* the completion order. Measured against it, with the plant applied at **every** depth:
//!
//! | suite | result |
//! |---|---|
//! | this file | 2 of 2 red, each naming the key sequence it got |
//! | `smear/tests/proto_execute.rs` | 145 of 146 green |
//! | `proto_mutation_oracle.rs` | 3 of 5 green |
//! | `proto_nonnull_oracle.rs` | 5 of 5 green |
//!
//! And with the plant applied **below the root only** — the recursive half of the property:
//!
//! | suite | result |
//! |---|---|
//! | this file | the nested case red, the root case green |
//! | `smear/tests/proto_execute.rs` | **146 of 146 green** |
//! | `proto_mutation_oracle.rs`, `proto_nonnull_oracle.rs` | all green |
//!
//! Not one of the incumbent detections is about response key order. Every one of them is a mutation
//! or `__typename` fixture, and it fires for the same incidental reason in each: a position that
//! never passes through [`Executor::handle_resolved`] — draft §4.4's `__typename`, answered at
//! collection, or a field the driver failed — is the one the plant leaves behind, so the *others*
//! move around it. The one in-crate failure, `a_drained_subtree_is_not_walked_again`, reads
//! `next_sibling` directly to find a slot and trips on the relink rather than on the order.
//!
//! Which is the same latency that hid the defect upstream, one level down: a driver that answers
//! each offer as it takes it completes in offer order, offer order is document order, and a
//! completion-ordered assembly then reproduces document order exactly. Every incumbent fixture
//! drives that way, so the plant is invisible to all of them but the handful holding a position
//! that never completes at all.

use graphql_proto::{Executor, Leaf, Node, ReqId, Values};
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

/// One field at a leaf type and one at an object type, each of which a fixture below aliases three
/// ways.
///
/// `id` is here so that the three aliases of a field are distinguishable to a reader as something
/// other than a typo: they are the same field with different arguments, which is the shape a client
/// writes when it wants three rows back in one request.
const SDL: &str = r#"
type Query {
  label(id: Int): String
  item(id: Int): Item
}

type Item {
  p: String
  q: String
  r: String
}
"#;

/// The driver's values.
///
/// A leaf carries the response path the executor asked at, so the rendered response pins the
/// key/value *pairing* as well as the key order. A permutation that carried values along with their
/// keys and one that crossed them are different defects, and a sequence of bare `"x"`s could not
/// tell them apart.
#[derive(Debug)]
enum Value {
  Object,
  Text(String),
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

  fn list_len(&self, _: &Value) -> Option<usize> {
    None
  }

  fn list_item(&mut self, _: &Value, _: usize) -> Value {
    // Draft §6.4.3 step 3 is the only caller and it is reached only from a list position, which
    // `SDL` declares none of. Answering with a value instead would hide a fixture that grew one.
    unreachable!("no position in this fixture has a list type")
  }

  fn type_name<'a>(&'a self, _: &'a Value) -> Option<&'a str> {
    // `ResolveAbstractType` asks only at an interface or a union position, and `SDL` has neither:
    // an object position's runtime type is the one the schema already declared.
    None
  }

  fn coerce_leaf(&mut self, value: Value, _: Leaf<'_>) -> Option<Value> {
    Some(value)
  }

  fn variable(&mut self, _: &str) -> Option<Value> {
    None
  }
}

/// What one run of a fixture answered, read three ways.
struct Run {
  /// Every response key, as a dotted draft §7.1.2 path, in the order the finished response yields
  /// them. This is the serialized key sequence the property is about.
  keys: Vec<String>,
  /// `data`, serialised — keys *and* the values sitting under them.
  data: String,
  /// Per response map — the root's under the empty path — the response keys of that map's children
  /// in the order the driver supplied their results. The premise of every case here, and asserted
  /// rather than assumed.
  ///
  /// A map is the unit because it is the unit the property is about, and because it is the only
  /// unit a premise can be phrased in: see the module header on why one flat sequence for the whole
  /// run says nothing.
  supplied: Vec<(String, Vec<String>)>,
}

/// Runs `query` to completion against [`SDL`], answering each batch of offers in reverse.
///
/// A *batch* is everything [`Executor::poll_resolve`] will hand out before it withholds — for these
/// fixtures, every position at one depth. Taking the whole batch before answering any of it is what
/// makes the permutation possible at all: a driver that answers each offer as it takes it can only
/// ever complete in the order the executor offered.
fn drive_backwards(query: &str) -> Run {
  let (schema, document) = compile(SDL, query);
  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Object)
    .expect("the operation resolves");

  let mut supplied: Vec<(String, Vec<String>)> = Vec::new();
  loop {
    // Take every offer that is available right now. The `FieldRequest` borrows the executor, so
    // each one is reduced to the three owned facts the answer needs before the next poll.
    let mut batch: Vec<(ReqId, String, bool)> = Vec::new();
    while let Some(request) = executor.poll_resolve(&mut space) {
      batch.push((
        request.id(),
        request.path().to_string(),
        request.selection_set().is_some(),
      ));
    }
    if batch.is_empty() {
      break;
    }
    batch.reverse();
    for (id, path, object) in batch {
      let value = if object {
        Value::Object
      } else {
        Value::Text(path.clone())
      };
      let (parent, key) = parent_and_key(&path);
      record(&mut supplied, parent, key);
      executor.handle_resolved(&mut space, id, value);
    }
  }

  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(
    response.error_count(),
    0,
    "the fixture resolves every field, so an error here means the run measured something else"
  );
  let data = response.data();
  let mut keys = Vec::new();
  collect_keys(&data, "", &mut keys);
  Run {
    keys,
    data: render(&data),
    supplied,
  }
}

/// Every response key under `node`, depth-first, each as a dotted path.
fn collect_keys(node: &Node<'_, Value>, prefix: &str, out: &mut Vec<String>) {
  let children = match node {
    Node::List(children) | Node::Object(children) => children.clone(),
    Node::Null | Node::Leaf(_) | Node::TypeName(_) => return,
  };
  for (key, child) in children {
    let path = if prefix.is_empty() {
      key.to_string()
    } else {
      format!("{prefix}.{key}")
    };
    out.push(path.clone());
    collect_keys(&child, &path, out);
  }
}

fn render(node: &Node<'_, Value>) -> String {
  match node {
    Node::Null => "null".to_owned(),
    Node::Leaf(Value::Text(text)) => format!("\"{text}\""),
    // Unreachable for these fixtures, and rendered distinctly rather than as `null` so that a run
    // that reached it says so instead of looking like a nulled position.
    Node::Leaf(Value::Object) => "<object in a leaf position>".to_owned(),
    Node::TypeName(name) => format!("\"{name}\""),
    Node::List(children) => {
      let mut out = String::from("[");
      for (index, (_, child)) in children.clone().enumerate() {
        if index > 0 {
          out.push(',');
        }
        out.push_str(&render(&child));
      }
      out.push(']');
      out
    }
    Node::Object(children) => {
      let mut out = String::from("{");
      for (index, (key, child)) in children.clone().enumerate() {
        if index > 0 {
          out.push(',');
        }
        out.push_str(&format!("\"{key}\":{}", render(&child)));
      }
      out.push('}');
      out
    }
  }
}

/// The response map a path sits in, and its response key within that map: `a.p` is `p` under `a`,
/// and `a` is `a` under the root, whose path is empty.
fn parent_and_key(path: &str) -> (&str, &str) {
  path.rsplit_once('.').unwrap_or(("", path))
}

/// Appends `key` to `parent`'s map, creating that map the first time the parent is seen.
fn record(maps: &mut Vec<(String, Vec<String>)>, parent: &str, key: &str) {
  match maps.iter().position(|(name, _)| name == parent) {
    Some(index) => maps[index].1.push(key.to_owned()),
    None => maps.push((parent.to_owned(), vec![key.to_owned()])),
  }
}

/// Asserts the run's premise against the **document's** order, one response map at a time, before
/// anything is asserted about the response's.
///
/// Deliberately not a comparison with `run.keys`. Under a correct executor the two questions have
/// the same answer, and under a completion-ordered one they do not — the response order becomes the
/// supply order, so a premise phrased against the response would fire first and report a broken
/// fixture where the truth is a broken product. Phrased against the document it stays green exactly
/// when the fixture is sound, and the case's own assertion is left to say what went wrong.
///
/// `supply_order` names, per map, the order that map's children were expected to arrive in, and the
/// run's table has to equal it exactly. What is deliberately *not* pinned is the interleaving
/// between maps: the parents are matched by name rather than by position, because when one map's
/// children arrive is a fact about depth scheduling, while the order they arrive in is the fact
/// this file is about. A driver that finished one map before polling for the next would have
/// permuted every sibling set just as thoroughly.
///
/// The loop is what stops `supply_order` from being repaired into a lie. A hand-written table is a
/// non-vacuity premise only because a reader knows what the document said, so the same sentence is
/// asked of the machine: per map, the keys that arrived must be exactly that map's children — which
/// is where a dropped or duplicated position is caught — and must not be in the document's order.
/// A later round that meets a red premise by copying the run's new order into the table therefore
/// cannot make it green if that new order is the document's, which is the one outcome the table
/// exists to exclude.
fn assert_permuted(run: &Run, document_order: &[&str], supply_order: &[(&str, &[&str])]) {
  // The document's own reading of each map, derived from the sequence the case already declares
  // rather than written out a second time and left to drift from it.
  let mut declared: Vec<(String, Vec<String>)> = Vec::new();
  for path in document_order {
    let (parent, key) = parent_and_key(path);
    record(&mut declared, parent, key);
  }

  let mut supplied = run.supplied.clone();
  let mut expected: Vec<(String, Vec<String>)> = supply_order
    .iter()
    .map(|(parent, keys)| {
      (
        (*parent).to_owned(),
        keys.iter().map(|key| (*key).to_owned()).collect(),
      )
    })
    .collect();
  supplied.sort_by(|left, right| left.0.cmp(&right.0));
  expected.sort_by(|left, right| left.0.cmp(&right.0));
  assert_eq!(
    supplied, expected,
    "each response map's children arrived in the order this case names"
  );

  assert_eq!(
    expected.len(),
    declared.len(),
    "the case names a supply order for every map the document has, and for no map it does not"
  );
  for (parent, children) in &declared {
    let Some((_, arrival)) = expected.iter().find(|(name, _)| name == parent) else {
      panic!("the case names no supply order for the `{parent}` map");
    };
    let mut sorted_arrival = arrival.clone();
    let mut sorted_children = children.clone();
    sorted_arrival.sort_unstable();
    sorted_children.sort_unstable();
    assert_eq!(
      sorted_arrival, sorted_children,
      "the `{parent}` map was supplied exactly the children the document gives it"
    );
    assert_ne!(
      arrival, children,
      "the `{parent}` map must be supplied out of document order, or it asserts nothing"
    );
  }
}

/// Parses an SDL and a query into the two borrows an [`Executor`] is built from.
fn compile<'q>(sdl: &str, query: &'q str) -> (Schema, ExecutableDocument<&'q str>) {
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

/// Three root fields answered backwards still serialise in the document's order.
///
/// The narrowest statement of the property: one depth, three response keys, and the only difference
/// between this run and a correct one is *when* each value arrived.
#[test]
fn root_response_keys_are_the_document_s_order_and_not_the_driver_s() {
  const DOCUMENT_ORDER: [&str; 3] = ["a", "b", "c"];
  // The one map this fixture has, and the driver hands it back to front.
  const SUPPLY_ORDER: [(&str, &[&str]); 1] = [("", &["c", "b", "a"])];

  let run = drive_backwards(
    r"{
      a: label(id: 1)
      b: label(id: 2)
      c: label(id: 3)
    }",
  );

  assert_permuted(&run, &DOCUMENT_ORDER, &SUPPLY_ORDER);
  assert_eq!(
    run.keys, DOCUMENT_ORDER,
    "the response key sequence is the document's"
  );
  assert_eq!(run.data, r#"{"a":"a","b":"b","c":"c"}"#);
}

/// The same property one level down, inside an outer field that is itself out of order.
///
/// Assembly being positional at the root says nothing about a selection set assembled later: the
/// root's positions are created once, by `start`, while an object's children are created when that
/// object's own value comes back — a different call, at a different time, with the driver's
/// permutation already applied above it. This is the case that separates the two.
#[test]
fn nested_response_keys_are_the_document_s_order_at_every_depth() {
  const DOCUMENT_ORDER: [&str; 12] = [
    "a", "a.p", "a.q", "a.r", "b", "b.p", "b.q", "b.r", "c", "c.p", "c.q", "c.r",
  ];
  // Four maps, every one of them handed back to front — which is the whole of what this case needs
  // to be true before its own assertions mean anything, and which no single sequence can say.
  const SUPPLY_ORDER: [(&str, &[&str]); 4] = [
    ("", &["c", "b", "a"]),
    ("a", &["r", "q", "p"]),
    ("b", &["r", "q", "p"]),
    ("c", &["r", "q", "p"]),
  ];

  let run = drive_backwards(
    r"{
      a: item(id: 1) { p q r }
      b: item(id: 2) { p q r }
      c: item(id: 3) { p q r }
    }",
  );

  assert_permuted(&run, &DOCUMENT_ORDER, &SUPPLY_ORDER);
  assert_eq!(
    run.keys, DOCUMENT_ORDER,
    "the response key sequence is the document's, outer and inner alike"
  );
  assert_eq!(
    run.data,
    concat!(
      r#"{"a":{"p":"a.p","q":"a.q","r":"a.r"},"#,
      r#""b":{"p":"b.p","q":"b.q","r":"b.r"},"#,
      r#""c":{"p":"c.p","q":"c.q","r":"c.r"}}"#,
    )
  );
}
