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
//! # Where "the document's order" comes from, and why it cannot be written down
//!
//! From the document. [`drive_backwards`] derives it — per response map, that map's response keys
//! in the order draft §6.3 `CollectFields` produces them — out of the very [`ExecutableDocument`]
//! value it then hands [`Executor::new`], and both the premise and the expected response read that
//! one derivation. A sequence written by hand beside the query cannot be an *anchor*, because the
//! two drift, and a drifted constant does not make this file go quiet. It makes it go **wrong**.
//! Leave `a, b, c` written down beside a query that says `c, b, a` and every premise check passes
//! against the constant — including the inequality, which then reads "the arrivals differ from
//! `a, b, c`" and is satisfied by a driver that permuted nothing at all — and the response is then
//! required to be `a, b, c`, so an executor that assembles its maps in some order of its own is
//! *blessed* by the assertion written to catch it. Only the driver's intended **supply** order is
//! still written by hand, because that is an input to the run rather than a fact about the
//! document.
//!
//! Deriving it is not a walk over the selections. Draft §6.3 groups by **response key**: a key that
//! appears twice is one entry, at the position of its *first* selection, and draft §6.4's
//! `MergeSelectionSets` concatenates that group's sub-selection sets into the one map underneath
//! it. So `b { q p } a b { r q }` declares `b, a` with `b` holding `q, p, r` — not `b, a, b`, and
//! not two `b` maps, and not `q, p, r, q`. Neither execution fixture repeats a response key, so the
//! grouping is stated against documents that do: [`document_maps`] has its own cases at the end of
//! this file, and they are the ones a walk over selections fails.
//!
//! What the derivation will not do is guess. A fragment's selections belong to a map only if
//! `DoesFragmentTypeApply` says so, which reads the schema and the position's runtime type, and
//! `@skip`/`@include` read variable values; none of the three is a fact about the document, so
//! [`group_by_response_key`] refuses those selections instead of answering for them.
//!
//! # Two documents, one key set
//!
//! [`QUERY`] and [`MIRROR`] are the same four root selections, and the same inner ones, written in
//! the opposite order. They declare the same three response maps — the root, `item` and `a` — each
//! holding the same response keys, and each map's order in one document is the other's reversed.
//! [`the_pair_is_one_key_set_in_two_orders`] asserts that out of the two documents themselves, so
//! the pair cannot quietly stop being a pair.
//!
//! ## What the pair proves, and why it is a proof and not two more samples
//!
//! The defect this file exists to catch is a response map whose order comes from somewhere other
//! than the document. One large family of "somewhere else" is the *keys*: a `BTreeMap` keyed by
//! response key, a sort by key length, a partition on "was this key an alias", by hash, by first
//! byte, by anything at all that a comparator can read off the key and nothing else.
//!
//! Enumerating that family is hopeless, and this file tried twice. A fixture whose keys were in
//! alphabetical order could not tell positional assembly from a lexical sort; naming the keys out
//! of alphabetical order fixed that and left a fixture whose response keys were every one of them a
//! single character, with every root key an alias and no child key one — which could not tell it
//! from a stable sort by length or a stable partition on alias-ness, since both tie on such a map
//! and leave it exactly as written. Each round named an ordering and the round after it named the
//! next, because there is no last one.
//!
//! The pair ends the family in one move. An assembly that reads only the keys is a *function from
//! key set to sequence*: hand it the set both fixtures share and it answers once. Two documents,
//! two different orders, one answer — it matches at most one of them, and the other fixture is red.
//! No ordering has to be named for that to hold, which is the whole point of it.
//!
//! One refinement, because the two orderings that survived the previous rounds were **stable**
//! sorts, and a stable sort is not purely a function of the key set — it reads the document order
//! too, and leaves a map alone whenever that order already agrees with the comparator. A stable
//! `cmp`-sort is the identity on a map exactly when the map's keys are non-decreasing under `cmp`.
//! [`MIRROR`] is [`QUERY`] reversed, so if one map's order is non-decreasing the other's is
//! non-increasing, and both hold at once only if `cmp` calls **every** pair of that map's keys
//! equal. So the statement survives the refinement: every key-only comparator that can move a key
//! in these maps at all moves one of the two fixtures out of its document order.
//!
//! What the pair says nothing about is an order derived from the *run* rather than from the keys —
//! completion order, arrival index, insertion order. Those are a different family and they die to a
//! different mechanism: the driver answers every map backwards, so any of them reproduces the
//! reversal in the response. The plants at the end of this file measure both halves.
//!
//! ## The belt: what the shared key set has to look like
//!
//! One comparator still escapes the argument above, by being inert: a `cmp` that calls every pair
//! of a map's keys equal leaves both orders alone. That is not luck — such a comparator cannot
//! reorder *this* map under any document whatsoever — but it can be a live defect on some other
//! document, and that is precisely what happened here. Every response key in the previous round was
//! one character long, so "sort by length" tied all of them and passed, while reordering any real
//! document with mixed-length keys.
//!
//! So the shared key set is varied along every axis a cheap comparator might read, and no map's
//! keys are monotone along any of them:
//!
//! | map | keys, in [`QUERY`]'s order | length | first byte | from an alias |
//! |---|---|---|---|---|
//! | root | `bee, item, a, label` | 3, 4, 1, 5 | `b, i, a, l` | yes, no, yes, no |
//! | `item` and `a` | `zed, q, rho` | 3, 1, 3 | `z, q, r` | yes, no, yes |
//!
//! Non-monotone rather than merely "not all equal", and that buys the second document for free:
//! reversing a sequence that is neither non-decreasing nor non-increasing yields another such
//! sequence, so the one table covers [`MIRROR`] too, and each fixture *individually* survives a
//! stable sort on each axis without needing its twin.
//! [`the_pair_is_one_key_set_in_two_orders`] asserts all four axes — the keys themselves, their
//! lengths, their first bytes and their alias flags — against both documents. Three of the four can
//! fail on their own; the first-byte column cannot, and its call site says why it is kept anyway.
//!
//! Not one of these spellings is cosmetic and none is free to be tidied. `a` is one character and
//! `label` is five so the length column moves; `item` sits second rather than first so no column is
//! sorted; two of the four root keys are the field's own name so the alias column holds both
//! values. A later round that renames a key or straightens an order deletes a detection, and the
//! pair test is what goes red instead of nothing — measured by straightening each column in turn,
//! which reddens it and names the column it straightened.
//!
//! # Aliases, depth, and leaves beside objects
//!
//! The response key is the *alias* when a field has one, and the alias is what a consumer compares
//! against. `label` appears twice at the root, once as `bee` and once under its own name, and
//! `item` likewise, once as `a` and once under its own name. Two positions that differ only by
//! response key and argument rule out a merge: draft §6.3 groups by response key, so anything
//! grouping by field *name* would collapse the four root keys into two rather than reorder them.
//!
//! The property is recursive, and a root-only pin says nothing about children. Both documents put
//! two object positions at the root, each with a three-key map of its own, and both assert the
//! whole flattened key sequence rather than only the top level. Both also hold leaf positions at
//! the root beside those objects — a root map created by `start`'s expansion is the same code path
//! whether its children are leaves or objects, but a defect that moved only one kind would have
//! somewhere to hide in a fixture that held only the other.
//!
//! # Measured, not read off the code
//!
//! Assembly here is positional by construction — `push_child` is the sole creator of a position and
//! appends, `handle_resolved` writes a state into a position that already exists, and rendering
//! walks the sibling chain — but *by construction* is not *unrepresentable*, so every claim above
//! was calibrated by planting the defect rather than by reading the code.
//!
//! ## The key-derived family, one plant per comparator
//!
//! Three product-only plants, each reordering a response map by reading only its keys, each run
//! against the fixtures below exactly as they ship:
//!
//! | plant | this file | `proto_execute.rs` | `proto_mutation_oracle.rs` | `proto_nonnull_oracle.rs` |
//! |---|---|---|---|---|
//! | `push_child` sorts the chain lexically by response key | 2 of 2 red | 140 of 146 | 2 of 5 | 2 of 5 |
//! | `push_child` stable-sorts the chain by response-key **length** | 2 of 2 red | 136 of 146 | 2 of 5 | 5 of 5 |
//! | `expand` stable-partitions the finished map, **aliases first** | 2 of 2 red | 144 of 146 | 5 of 5 | 3 of 5 |
//!
//! Each of the three fails at the *property*, not at the premise: reordering the sibling chain
//! moves the response without touching the `next_ready` queue the offers come off, so the driver
//! supplied exactly what the case named.
//!
//! The lexical row is the argument made visible, because the two cases fail with the **same** key
//! sequence in hand — `a, a.q, a.rho, a.zed, bee, item, …` out of both documents. One key set, one
//! answer, two documents to be right about: the plant cannot be right about the second without
//! being wrong about the first. The length row shows the refinement doing its work. `zed` and `rho`
//! are both three characters, so that comparator ties them and the stable sort hands each document
//! back its own relative order for that pair — the inner maps come out *differently* in the two
//! cases. The root map, where all four lengths differ, comes out identically in both. What the tie
//! costs is only that the case is settled by the reversal rather than by counting: `q` is one
//! character where the others are three, so a map whose document order was `zed, q, rho` cannot be
//! non-decreasing in length and neither can its reverse.
//!
//! What the right-hand columns are *not* is a claim that these defects are invisible elsewhere. An
//! assembly that reorders every response map reorders plenty of fixtures that never meant to assert
//! an order, and all three plants redden neighbours. The relevant number is this file's own column
//! against the *previous* round's fixture, where the length plant and the alias-first plant were
//! **5 of 5 green** while the incumbents around them went red — the one file whose subject is
//! §7.1.5's key order was the file that could not see either defect. That is what the pair fixes,
//! and it fixes it for the comparators nobody has named yet as well as for these two.
//!
//! The same shape of defect can live in the **oracle**: a derivation that sorted its own groups
//! would agree with a sorting executor about every document there is, and the two would be right
//! about each other and wrong together. So `group_by_response_key` was planted with a lexical sort
//! of its own, and it turns five of this file's six cases red — both grouping cases, both execution
//! cases, and [`the_pair_is_one_key_set_in_two_orders`], which notices first and most directly,
//! because a derivation that sorts reports the *same* order for two documents that differ only in
//! order.
//!
//! ## The history-derived family
//!
//! The plant is `handle_resolved` moving the position to the end of its parent's child chain before
//! completing it, which is async-graphql's insert-on-completion expressed against this tree; when
//! every child of a position completes through that path exactly once, the resulting sibling order
//! *is* the completion order. Measured against it, with the plant applied at **every** depth:
//!
//! | suite | result |
//! |---|---|
//! | this file's two execution cases | 2 of 2 red, each naming the key sequence it got |
//! | `smear/tests/proto_execute.rs` | 145 of 146 green |
//! | `proto_mutation_oracle.rs` | 3 of 5 green |
//! | `proto_nonnull_oracle.rs` | 5 of 5 green |
//!
//! And with the plant applied **below the root only** — the recursive half of the property:
//!
//! | suite | result |
//! |---|---|
//! | this file's two execution cases | 2 of 2 red, each naming an inner map's reversal |
//! | `smear/tests/proto_execute.rs` | **146 of 146 green** |
//! | `proto_mutation_oracle.rs`, `proto_nonnull_oracle.rs` | all green |
//!
//! Not one of the incumbent detections is about response key order. Every one of them is a mutation
//! or `__typename` fixture, and it fires for the same incidental reason in each: a position that
//! never passes through [`Executor::handle_resolved`] — draft §4.4's `__typename`, answered at
//! collection, a field the driver failed, or one withheld and never offered — is the one the plant
//! leaves behind, so the *others* move around it. `graphql-proto`'s own unit tests say the same
//! thing twice: `a_withheld_top_level_field_is_in_the_tree_and_reads_as_null` holds exactly such a
//! position at the root and goes red under the relink at every depth but not under the below-root
//! one, and `a_drained_subtree_is_not_walked_again` reads `next_sibling` directly to find a slot,
//! so it trips on the relink rather than on the order and goes red under both.
//!
//! Which is the same latency that hid the defect upstream, one level down: a driver that answers
//! each offer as it takes it completes in offer order, offer order is document order, and a
//! completion-ordered assembly then reproduces document order exactly. Every incumbent fixture
//! drives that way, so the plant is invisible to all of them but the handful holding a position
//! that never completes at all.
//!
//! Both relink plants redden both execution cases, so which depth broke is not read off the test
//! name. It is read off the failure, which prints the whole dotted key sequence: under the relink
//! below the root the root's keys are still the document's and only the inner maps have turned
//! round, and under the relink at every depth the whole sequence is the supply order.

use graphql_proto::{Executor, Leaf, Node, ReqId, Values};
use smear_parser::{
  graphql::{
    GraphQL,
    ast::{
      ExecutableDefinition, ExecutableDocument, OperationDefinition, Selection, TypeSystemDocument,
    },
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  lexer::tokora::{Parse as _, Parser},
};
use smear_schema::Schema;

/// One field at a leaf type and one at an object type, each of which the documents below take
/// twice: once under an alias and once under its own name.
///
/// `id` is here so that the two spellings of a field are distinguishable to a reader as something
/// other than a typo: they are the same field with different arguments, which is the shape a client
/// writes when it wants two rows back in one request.
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

/// The first of the pair.
///
/// Two leaves and two objects at the root, each object holding the same three keys. Every response
/// key here is in [`MIRROR`] as well; what differs is only the order, and the module header is why
/// that is the entire mechanism of this file.
const QUERY: &str = r"{
  bee: label(id: 1)
  item(id: 2) { zed: p q rho: r }
  a: item(id: 3) { zed: p q rho: r }
  label(id: 4)
}";

/// The second of the pair: [`QUERY`]'s selections, and every inner selection set, written in the
/// opposite order.
///
/// Reversal rather than some other permutation for the reason [`drive_backwards`] reverses: it is
/// the maximum displacement available, it needs no maintenance when a fixture grows a field, and it
/// is what makes the stable-sort half of the header's argument go through — a stable sort leaves
/// both a sequence and its reverse alone only if it ties every key in the map.
const MIRROR: &str = r"{
  label(id: 4)
  a: item(id: 3) { rho: r q zed: p }
  item(id: 2) { rho: r q zed: p }
  bee: label(id: 1)
}";

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

/// What one run of a fixture answered, read three ways, and what its document said it should be.
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
  /// The same table as the document declares it, derived by [`document_maps`] from the very
  /// [`ExecutableDocument`] this run's [`Executor`] was built from.
  ///
  /// Not a parameter, and not a constant. It travels with the run because the run is the only thing
  /// that knows which document it executed, and a second copy anywhere else is a copy that can
  /// disagree with the first.
  declared: Vec<(String, Vec<String>)>,
}

impl Run {
  /// The dotted response paths [`Self::declared`] yields when it is walked depth-first — what
  /// [`Self::keys`] has to be.
  ///
  /// The walk turns a table of *selection sets* into a sequence of *response paths*, which is exact
  /// only where every object position holds exactly one map. `SDL` declares no list type and
  /// [`Space`] nulls nothing, so it is exact here; a fixture that grew a list would produce
  /// `a.0.p` where this produces `a.p` and the comparison would fail, which is the direction a
  /// wrong assumption has to fail in.
  fn document_keys(&self) -> Vec<String> {
    fn walk(maps: &[(String, Vec<String>)], path: &str, out: &mut Vec<String>) {
      let Some((_, keys)) = maps.iter().find(|(name, _)| name == path) else {
        return;
      };
      for key in keys {
        let child = join(path, key);
        out.push(child.clone());
        walk(maps, &child, out);
      }
    }

    let mut keys = Vec::new();
    walk(&self.declared, "", &mut keys);
    keys
  }

  /// The `data` [`Self::declared`] implies under [`drive_backwards`]'s answering convention — what
  /// [`Self::data`] has to be.
  ///
  /// The convention is the whole of the extra input: the driver answers every leaf with the
  /// response path the executor asked it at, so the value under a key is that key's own path and
  /// nothing here has to know what the driver did. A permutation that crossed keys and values puts
  /// some other path under a key and this is what notices.
  fn document_data(&self) -> String {
    fn walk(maps: &[(String, Vec<String>)], path: &str) -> String {
      let (_, keys) = maps
        .iter()
        .find(|(name, _)| name == path)
        .expect("the walk descends only into paths the table names");
      let mut out = String::from("{");
      for (index, key) in keys.iter().enumerate() {
        if index > 0 {
          out.push(',');
        }
        let child = join(path, key);
        let value = if maps.iter().any(|(name, _)| *name == child) {
          walk(maps, &child)
        } else {
          format!("\"{child}\"")
        };
        out.push_str(&format!("\"{key}\":{value}"));
      }
      out.push('}');
      out
    }

    walk(&self.declared, "")
  }
}

/// Runs `query` to completion against [`SDL`], answering each batch of offers in reverse.
///
/// A *batch* is everything [`Executor::poll_resolve`] will hand out before it withholds — for these
/// fixtures, every position at one depth. Taking the whole batch before answering any of it is what
/// makes the permutation possible at all: a driver that answers each offer as it takes it can only
/// ever complete in the order the executor offered.
fn drive_backwards(query: &str) -> Run {
  let (schema, document) = compile(SDL, query);
  // Before the executor exists, and from the same value it is about to be handed. This is the one
  // place the document's order is read, and there is nowhere for a second reading to drift from it.
  let declared = document_maps(&document);
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
    declared,
  }
}

/// Every response key under `node`, depth-first, each as a dotted path.
fn collect_keys(node: &Node<'_, Value>, prefix: &str, out: &mut Vec<String>) {
  let children = match node {
    Node::List(children) | Node::Object(children) => children.clone(),
    Node::Null | Node::Leaf(_) | Node::TypeName(_) => return,
  };
  for (key, child) in children {
    let path = join(prefix, &key.to_string());
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

/// A child's dotted path: `key` under the response map at `parent`, whose own path is empty at the
/// root.
fn join(parent: &str, key: &str) -> String {
  if parent.is_empty() {
    key.to_owned()
  } else {
    format!("{parent}.{key}")
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

/// One draft §6.3 group: the response key, whether the document reached it through an alias, and
/// the concatenation draft §6.4's `MergeSelectionSets` makes of the group's sub-selection sets.
struct Group<'a> {
  key: &'a str,
  /// The key differs from the name of the field its *first* selection named.
  ///
  /// The first selection rather than any of them, because that is the one whose field the executor
  /// records against the position, and this has to be the executor's notion of an alias rather than
  /// a second one: `q: q` is an alias to the parser and is indistinguishable from a bare `q` to
  /// everything downstream, so the comparison is against the name and not against `alias().is_some()`.
  aliased: bool,
  merged: Vec<&'a Selection<&'a str>>,
}

/// Draft §6.3 `CollectFields`, as much of it as a document alone can decide: one entry per response
/// key, in the order of each key's *first* selection, carrying the concatenation of every
/// sub-selection set the selections under that key declared.
///
/// # This is not the order of the selections, and that is the whole of it
///
/// A response key is a field's alias or, failing that, its name, and draft §6.3 collects the
/// selections that share one into a single group at the position the first of them took. Draft
/// §6.4's `MergeSelectionSets` then runs the group's sub-selection sets together as the one
/// selection set of the one position underneath. Two consequences a walk over selections gets
/// wrong: a repeated key must not appear twice, and it must not move to where its last selection
/// sits; and the map under it holds the *concatenation* of the group's sub-selections, re-grouped
/// by the same rule one level down.
///
/// # What it refuses, and why refusing is the honest answer
///
/// Whether a fragment's selections belong to this group depends on `DoesFragmentTypeApply`, which
/// reads the schema and the position's runtime type, and whether a selection is collected at all
/// depends on `@skip`/`@include`, which read variable values. Neither is a fact about the document,
/// and this derivation has only the document. A guess would be an oracle that is right about the
/// fixtures it was written for and quietly wrong past them, which is the shape of defect it exists
/// to remove — so it panics instead, and a fixture that grows either construct gets a red test with
/// the reason in it rather than an answer.
fn group_by_response_key<'a>(selections: &[&'a Selection<&'a str>]) -> Vec<Group<'a>> {
  let mut groups: Vec<Group<'a>> = Vec::new();
  for selection in selections {
    let field = match selection {
      Selection::Field(field) => field,
      Selection::FragmentSpread(_) | Selection::InlineFragment(_) => panic!(
        "a fragment's selections join a map only if `DoesFragmentTypeApply` says so, which the \
         document does not: derive this order some other way, or teach the schema to this helper"
      ),
    };
    assert!(
      field.directives().is_none(),
      "`@skip`/`@include` decide membership from variable values, which the document does not carry"
    );
    let name: &'a str = field.name().source();
    let key: &'a str = field.alias().map_or(name, |alias| alias.name().source());
    let sub = match field.selection_set() {
      Some(set) => set.selections(),
      None => &[],
    };
    match groups.iter().position(|group| group.key == key) {
      Some(index) => groups[index].merged.extend(sub.iter()),
      None => groups.push(Group {
        key,
        aliased: key != name,
        merged: sub.iter().collect(),
      }),
    }
  }
  groups
}

/// One response map, as the document declares it.
struct Declared {
  /// The map's dotted response path; the root's is empty.
  path: String,
  /// Its response keys, in document order.
  keys: Vec<String>,
  /// Whether each of those keys came through an alias, in the same order.
  ///
  /// Read by [`the_pair_is_one_key_set_in_two_orders`] and nothing else. Alias-ness is not a
  /// function of the key, so unlike the key's length or its first byte it cannot be recovered from
  /// [`Self::keys`] after the fact — and it is one of the axes a cheap comparator reads, so the
  /// derivation has to carry it rather than let the header's table be prose.
  aliased: Vec<bool>,
}

/// Every response map `selections` declares at `path` and below.
fn declare_maps_under<'a>(
  selections: &[&'a Selection<&'a str>],
  path: &str,
  out: &mut Vec<Declared>,
) {
  let groups = group_by_response_key(selections);
  out.push(Declared {
    path: path.to_owned(),
    keys: groups.iter().map(|group| group.key.to_owned()).collect(),
    aliased: groups.iter().map(|group| group.aliased).collect(),
  });
  for group in groups {
    // A key with no sub-selection is a leaf position, and a leaf holds no map. The recursion runs
    // on the *merged* list, so a key repeated inside one of these groups regroups by the same rule.
    if !group.merged.is_empty() {
      declare_maps_under(&group.merged, &join(path, group.key), out);
    }
  }
}

/// The document's own answer to "which response keys does each response map hold, in what order,
/// and which of them are aliases", for the operation draft §6.1 `GetOperation` selects when it is
/// given no operation name.
///
/// No operation name because that is the `None` every fixture here hands [`Executor::start`], and
/// the two have to be reading the same operation for any of this to mean anything. `GetOperation`
/// requires the document to hold exactly one operation in that case, so a fixture that grew a
/// second one is a fixture whose `start` would have failed too, and it panics here rather than
/// silently describing the first.
///
/// The schema is not a parameter, and it is not an oversight: which keys a response map holds and
/// what order they come in is settled by draft §6.3 out of the document alone.
fn document_declares(document: &ExecutableDocument<&str>) -> Vec<Declared> {
  let operations: Vec<_> = document
    .definitions()
    .iter()
    .filter_map(|described| match described.node() {
      ExecutableDefinition::Operation(operation) => Some(operation),
      ExecutableDefinition::Fragment(_) => None,
    })
    .collect();
  assert_eq!(
    operations.len(),
    1,
    "draft §6.1 `GetOperation` selects an operation without a name only when the document defines \
     exactly one"
  );
  let selection_set = match operations[0] {
    OperationDefinition::Named(named) => named.selection_set(),
    OperationDefinition::Shorthand(shorthand) => shorthand,
  };

  let selections: Vec<&Selection<&str>> = selection_set.selections().iter().collect();
  let mut maps = Vec::new();
  declare_maps_under(&selections, "", &mut maps);
  maps
}

/// [`document_declares`] projected onto the pair every other reader here needs: each map's path and
/// its response keys in document order.
fn document_maps(document: &ExecutableDocument<&str>) -> Vec<(String, Vec<String>)> {
  document_declares(document)
    .into_iter()
    .map(|map| (map.path, map.keys))
    .collect()
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
/// asked of the machine — against [`Run::declared`], which *is* what the document said: per map,
/// the keys that arrived must be exactly that map's children — which is where a dropped or
/// duplicated position is caught — and must not be in the document's order. A later round that
/// meets a red premise by copying the run's new order into the table therefore cannot make it green
/// if that new order is the document's, which is the one outcome the table exists to exclude.
fn assert_permuted(run: &Run, supply_order: &[(&str, &[&str])]) {
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
    run.declared.len(),
    "the case names a supply order for every map the document has, and for no map it does not"
  );
  for (parent, children) in &run.declared {
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

/// Asserts that `values` is neither non-decreasing nor non-increasing.
///
/// A stable sort by some comparator is the identity on a sequence exactly when that sequence is
/// already non-decreasing under it, so a map whose keys are monotone along an axis is a map that
/// axis's sort leaves alone. Requiring non-monotone rather than merely "the axis distinguishes two
/// of these keys" is what makes each document catch such a sort on its own, without the other half
/// of the pair — and because reversing a non-monotone sequence yields another one, asserting it on
/// one document would already have covered the other.
fn assert_not_monotone<T: Ord + core::fmt::Debug>(axis: &str, path: &str, values: &[T]) {
  assert!(
    values.windows(2).any(|pair| pair[0] > pair[1]),
    "the `{path}` map's keys are non-decreasing in {axis} — {values:?} — so a stable sort on that \
     axis would leave this map exactly as the document wrote it"
  );
  assert!(
    values.windows(2).any(|pair| pair[0] < pair[1]),
    "the `{path}` map's keys are non-increasing in {axis} — {values:?} — so a stable sort on that \
     axis would leave this map exactly as the document wrote it"
  );
}

/// Parses an executable document, which is all [`document_declares`] needs and the second of the
/// two borrows an [`Executor`] is built from.
fn parse_query(query: &str) -> ExecutableDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(query)
  .expect("the query parses")
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
  (schema, parse_query(query))
}

/// A map table as `&str`s, so a case can be written and a failure read without a wall of
/// `to_owned`.
fn borrowed(maps: &[(String, Vec<String>)]) -> Vec<(&str, Vec<&str>)> {
  maps
    .iter()
    .map(|(name, keys)| (name.as_str(), keys.iter().map(String::as_str).collect()))
    .collect()
}

/// [`QUERY`] answered backwards still serialises in [`QUERY`]'s order.
///
/// One half of the pair the module header is about, and on its own the plain statement of the
/// property: two depths, ten response keys, and the only difference between this run and a correct
/// one is *when* each value arrived.
#[test]
fn response_keys_are_the_document_s_order_at_every_depth_and_not_the_driver_s() {
  // Every one of the three maps handed back to front — which is the whole of what this case needs
  // to be true before its own assertions mean anything, and which no single sequence can say.
  const SUPPLY_ORDER: [(&str, &[&str]); 3] = [
    ("", &["label", "a", "item", "bee"]),
    ("a", &["rho", "q", "zed"]),
    ("item", &["rho", "q", "zed"]),
  ];

  let run = drive_backwards(QUERY);

  assert_permuted(&run, &SUPPLY_ORDER);
  assert_eq!(
    run.keys,
    run.document_keys(),
    "the response key sequence is the document's, outer and inner alike"
  );
  assert_eq!(run.data, run.document_data());
}

/// [`MIRROR`] answered backwards serialises in [`MIRROR`]'s order — the opposite one, out of the
/// same keys.
///
/// This is the other half, and it is not a second sample of the same thing. Together with the case
/// above it rules out every assembly whose order is a function of the response keys, because such
/// an assembly answers one order for the key set the two documents share and there are two orders
/// to match. See the module header; [`the_pair_is_one_key_set_in_two_orders`] is what keeps the two
/// documents in that relationship.
#[test]
fn the_mirror_document_s_opposite_order_is_served_by_the_same_executor() {
  const SUPPLY_ORDER: [(&str, &[&str]); 3] = [
    ("", &["bee", "item", "a", "label"]),
    ("a", &["zed", "q", "rho"]),
    ("item", &["zed", "q", "rho"]),
  ];

  let run = drive_backwards(MIRROR);

  assert_permuted(&run, &SUPPLY_ORDER);
  assert_eq!(
    run.keys,
    run.document_keys(),
    "the response key sequence is the document's, outer and inner alike"
  );
  assert_eq!(run.data, run.document_data());
}

/// The two execution documents declare one key set in two orders, and each order is varied along
/// every axis a comparator could cheaply read.
///
/// Without this, the pair is a coincidence of how the two queries happen to be spelled. A round
/// that renamed `zed` to `s`, or moved `item` to the front, or dropped the one unaliased key from a
/// map, would delete a detection and leave five green tests behind it — which is exactly the way
/// the previous two rounds of this file lost theirs. The header's table is asserted here rather
/// than merely written down.
#[test]
fn the_pair_is_one_key_set_in_two_orders() {
  let document = parse_query(QUERY);
  let mirror = parse_query(MIRROR);
  let declared = document_declares(&document);
  let mirrored = document_declares(&mirror);

  assert_eq!(
    declared.len(),
    mirrored.len(),
    "the two documents declare the same number of response maps"
  );
  for map in &declared {
    let Some(other) = mirrored.iter().find(|other| other.path == map.path) else {
      panic!("the mirror declares no `{}` map", map.path);
    };
    assert!(
      map.keys.len() > 1,
      "the `{}` map holds one key, so it has only one order and the pair says nothing about it",
      map.path
    );
    let reversed: Vec<&String> = other.keys.iter().rev().collect();
    let forward: Vec<&String> = map.keys.iter().collect();
    assert_eq!(
      forward, reversed,
      "the mirror's `{}` map must hold the same keys in the reverse order",
      map.path
    );

    // Both documents, though one would do: a reversed non-monotone sequence is non-monotone, so
    // this is a second reading of the same fact rather than a second fact. It is here because the
    // header's table names both documents and a reader should be able to see it checked for both.
    for side in [map, other] {
      assert_not_monotone("the keys themselves", &side.path, &side.keys);
      let lengths: Vec<usize> = side.keys.iter().map(String::len).collect();
      assert_not_monotone("key length", &side.path, &lengths);
      // Redundant today and deliberately kept. No two keys in either map begin with the same byte,
      // and where first bytes are distinct the first-byte order *is* the lexical order, so the
      // check above already covers this one and nothing can reach the panic below. It stops being
      // redundant the moment a map holds two keys with a shared first byte, which is exactly when a
      // reader would otherwise have to notice on their own that an axis had gone uncovered.
      let first: Vec<u8> = side
        .keys
        .iter()
        .map(|key| *key.as_bytes().first().expect("a response key is not empty"))
        .collect();
      assert_not_monotone("first byte", &side.path, &first);
      assert_not_monotone("alias-ness", &side.path, &side.aliased);
    }
  }
}

/// A repeated response key is one entry, where the *first* of its selections put it.
///
/// Neither execution fixture repeats a key, so this is where the derivation's grouping is stated
/// against a document that needs it, and it is a document draft §5.3.2 allows: both `b` selections
/// name the same field with the same argument, so they merge rather than conflict.
///
/// Three answers a walk over selections gets wrong, all in this one case. `b` stays ahead of `a`
/// rather than moving to where its second selection sits; it appears once rather than twice; and
/// the map under it is the two sub-selection sets run together *and re-grouped*, so the `q` that
/// both of them name is one key at its first position and the map reads `q, p, r` rather than
/// `q, p, r, q`.
///
/// Both orders it asserts are out of alphabetical order, and that is not cosmetic either. Written
/// as `a, b` over `p, q, r` — which is the natural spelling of this document — the case would be
/// answered identically by a derivation that sorted each group, and would report nothing when one
/// did. The execution cases are protected from that by the pair; a derivation case has no pair, so
/// it is protected the old way.
#[test]
fn a_repeated_response_key_is_one_entry_at_its_first_position() {
  let document = parse_query(
    r"{
      b: item(id: 1) { q p }
      a: label(id: 2)
      b: item(id: 1) { r q }
    }",
  );

  assert_eq!(
    borrowed(&document_maps(&document)),
    [("", vec!["b", "a"]), ("b", vec!["q", "p", "r"])]
  );
}

/// Grouping is by response key, so one field name under two aliases is two maps and one alias over
/// two selections is one.
///
/// The complement of the case above, and the reason the derivation reads `alias()` before `name()`:
/// `item` and `x: item` are the same field and must not merge, while the two `x`s are the same
/// response key and must. Getting the key from the field's name instead would collapse all four
/// selections into one map of `q, r, p`, in a file whose execution documents take each of their two
/// fields twice.
///
/// `x` is written ahead of `item` so that all three orders asserted here are out of alphabetical
/// order. With `item` first — where a reader would naturally put it — the root map's document order
/// and its sorted order are the same list, and a derivation that sorted its groups would be green on
/// a map it had reordered.
#[test]
fn a_response_key_is_the_alias_and_grouping_follows_it() {
  let document = parse_query(
    r"{
      x: item(id: 2) { q }
      item(id: 1) { r }
      x: item(id: 2) { p }
      item(id: 1) { p }
    }",
  );

  assert_eq!(
    borrowed(&document_maps(&document)),
    [
      ("", vec!["x", "item"]),
      ("x", vec!["q", "p"]),
      ("item", vec!["r", "p"]),
    ]
  );
}

/// A construct the document cannot settle is refused, not guessed at.
///
/// The boundary is worth a test of its own because the failure it prevents is the silent one. Draft
/// §6.3 expands this spread in place, so the root map is `a, b`; a derivation that walked past a
/// spread it could not resolve would answer `a`, and a response missing `b` entirely would then be
/// the response it required. Refusing is the only answer that cannot be quietly wrong, and it is
/// only an answer while nothing weakens it into a skip.
#[test]
#[should_panic(expected = "`DoesFragmentTypeApply`")]
fn a_fragment_is_refused_rather_than_guessed_at() {
  let document = parse_query(
    r"{
      a: label(id: 1)
      ...Rest
    }

    fragment Rest on Query {
      b: label(id: 2)
    }",
  );

  let _ = document_declares(&document);
}
