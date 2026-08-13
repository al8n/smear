//! The draft §7.1 response map, written out.
//!
//! # The shape is closed, so this module is a transcription and not a design
//!
//! Draft §7.1.8 *Additional Entries* closes the execution result — it "must not contain any
//! entries other than those described above" — so the writer here has three keys and no
//! extension point. §7.1.3's *request error result* is a second closed map with two, and it is a
//! second function rather than a flag, exactly as it is a second type in `graphql-proto`.
//!
//! # Entry order
//!
//! `errors`, then `data`, then `extensions`. §7.1 asks for the first of those and not the rest:
//! "If the *errors* entry is present, it should be first". The other two follow the reference
//! implementation's order, which is what a differential against it compares against. Key order is
//! not semantic in JSON, so the round-trip gate compares parsed *values*; the byte gate below it
//! is what pins this choice.
//!
//! # `locations` needs the document, so every door asks for it
//!
//! `graphql-proto` records a field error's position as a byte span, deliberately: "turning one
//! into the other needs the source text, which the executor does not hold". Draft §7.1.2 wants a
//! line and a column, so the source text arrives here instead — as the `document` argument, which
//! must be the same text the executor's `ExecutableDocument` was parsed from. Handing over a
//! different one produces positions that point at nothing, which is why the parameter is required
//! rather than optional: an `Option<&str>` would make silently dropping `locations` the easy path.
//!
//! # The three walks a remote client shapes, and what each of them runs on
//!
//! Everything else in this file is proportional to the response by construction. These three are
//! not, because their cost is chosen by whoever wrote the query and whoever broke the resolvers:
//!
//! - **Every location resolved against the document.** [`Locations`] indexes the document once and
//!   answers each offset in bounded work. Deriving a line and column by walking the prefix is the
//!   same primitive `smear-schema`'s introspection decoder was repaired for one release earlier,
//!   met again from the writing side, and the shapes differ in the way that decides the repair:
//!   there one refusal escapes, so the walk is *staged* and performed once; here every location is
//!   kept, so there is no single walk to defer and the document is indexed instead.
//! - **Every container in `data`.** [`write_node`] runs on an explicit work stack.
//! - **Every error's response path, up the response tree.** The links point the wrong way for
//!   draft §7.1.2's order, so the chain has to be reversed; reversing it per segment is quadratic
//!   in the depth. That reversal belongs to `graphql-proto`, which owns the links, and it is done
//!   once per path there — [`Path::collect_into`](graphql_proto::Path::collect_into). What this
//!   file owns is the buffer, hoisted beside [`Locations`] so that one serves the whole response;
//!   `graphql-proto` owns how it *grows*, and reserves it against a depth the executor recorded
//!   while building the tree, so filling it costs one allocation rather than a doubling sequence
//!   with the old buffer and the new one both live at the peak.
//!
//! All three are stated as bounds and all three are measured — see this module's `visited` and
//! `allocations` counters and the gates in `super::tests`, and `graphql-proto`'s own slot-traversal
//! **and allocation** gate for the third — because a cost claim settled by reading the code is an
//! opinion. That third gate needs both of its axes and would pass on either one alone, which is why
//! the counter it reads says so at length.
//!
//! # The three buffers THIS FILE grows, which are three rows of an enumeration one level up
//!
//! **And it is worth saying why this file keeps attracting them.** It is the only place that
//! touches the whole document and the whole response tree once per error, so every decision made
//! elsewhere on the grounds that an item is cheap meets its worst case here, all at once and with
//! the count chosen by the client. Four findings, four different mechanisms — a prefix rescan, a
//! native recursion, a parent-link re-walk, and the *repair* for the second one growing
//! geometrically — and none of the four was cheap by accident.
//!
//! The fourth is why this table exists rather than a fifth repair. A structure that arrives as the
//! answer to a finding reads as already designed, and the frame stack was: for the one property it
//! repaired. Nobody priced its own growth.
//!
//! | buffer | what bounds its size | reserved from | growth |
//! |---|---|---|---|
//! | [`write_node`]'s frame stack | one frame per open container: the response's depth | [`Response::depth`](graphql_proto::Response::depth), recorded at each position's creation | one `try_reserve_exact`, [`Error::Allocation`] |
//! | `write_response_with`'s `segments` | the deepest error path in the response | [`Path::len`](graphql_proto::Path::len), the same recorded number | one `try_reserve` per path, [`Error::Allocation`] |
//! | [`Locations`]'s checkpoints | one per [`STRIDE`] bytes of the document, plus one | the document's length | one `try_reserve_exact`, [`Error::Allocation`] |
//!
//! **THE TABLE IS SCOPED TO THIS FILE, AND SAYING SO IS THE REPAIR THE ROUND AFTER IT ASKED FOR.**
//! It used to be headed "every growable allocation on this path", and that claim was wrong in two
//! directions at once. The path leaves this file — a response leaf and an `extensions` value are
//! written by [`super::materialized`] or by the caller — and an allocation is not the only thing a
//! serialiser can exhaust. The defect that survived this table grew the **native stack**, in that
//! sibling module, over a value whose nesting [`Response::depth`](graphql_proto::Response::depth)
//! does not describe. A file-local table asserting a whole-path property is the shape of that
//! mistake, so the closed list lives in [`super`]'s header, where both modules and both exhaustible
//! resources can be named, and these three are three of its rows.
//!
//! And the rows that are *not* allocations, listed because "there is nothing else" is the claim and
//! an absence has to be enumerated to be checked: the sink is the caller's `fmt::Write` and this
//! writer never buffers into one of its own; [`Json::number`], `int_leaf` and `double` render into
//! stack buffers; a leaf, a `__typename`, an error message, an `extensions` key and a path segment
//! are all written straight through, escaped as they stream; and every iterator this file drives —
//! [`Response::errors`](graphql_proto::Response::errors), [`Children`],
//! [`Extensions`]'s — is a cursor over a slice the executor already owns.
//! What this file writes an error *message* with is [`Json::display`](super::Json::display) over
//! the error's own `Display`, which renders no path and is a `match` over one error row, so it adds
//! no native frames either.
//!
//! # A row this table crossed off, and the criterion that was wrong
//!
//! It used to end with a sentence naming the one door in `graphql-proto` that allocated infallibly
//! — the iterator behind [`Path`](graphql_proto::Path)'s `Debug` and `Display` — and dismissing it
//! as "deliberately not on this path". **The row was right and the dismissal was not.** It is not
//! on this path. It is also public, both standard formatters went through it, and the depth is the
//! client's, so any driver that logged a field error's path could abort the process on a buffer
//! this file never touches. `graphql-proto` now turns a path around in a fixed window of its own
//! frame and *truncates* a deeper one behind a marker — it does not allocate at any depth, and it
//! does not answer `fmt::Error`, which `core` reserves for a failure the sink reported and which
//! `format!` and `ToString` therefore `expect` on. The same re-audit found [`Node`]'s `Debug`
//! recursing through the response tree, on the other fatal axis, and that is fixed beside it.
//!
//! The lesson is about the *shape* of a dismissal and belongs here rather than in a report, because
//! this table will attract more of them. **A row crossed off because "the writer does not reach it"
//! has been checked against the narrowest caller there is.** What a dismissal has to answer is what
//! the *widest* caller of that item is, and for anything `pub` the widest caller is `{}` and
//! `{:?}`.

use core::fmt;

use std::vec::Vec;

use graphql_proto::{
  Children, Error as FieldError, Extensions, Node, RequestErrorResult, Response, Segment,
};

use super::{Error, Json, WriteJson};

/// Writes a finished draft §7.1 execution result as JSON.
///
/// `document` is the source text the operation was parsed from; see the module header for why it
/// is not optional.
///
/// # Errors
///
/// Whatever the sink or a leaf reports — see [`Error`]. A refusal leaves whatever had already been
/// written in the sink, which is a partial document: this writer streams, so there is no point at
/// which the whole response exists to be discarded. A caller that needs all-or-nothing writes into
/// its own buffer and keeps it only on `Ok`.
pub fn write_response<W, V>(out: W, response: &Response<'_, V>, document: &str) -> Result<(), Error>
where
  W: fmt::Write,
  V: WriteJson,
{
  write_response_with(out, response, document, |value, json| {
    value.write_json(json)
  })
}

/// Writes a finished draft §7.1 execution result, with the caller saying how a leaf is written.
///
/// The door for a driver whose value is a handle into a table it owns: `write_leaf` can close over
/// the table, which is the writing side of the argument
/// [`Values`](crate::proto::Values) makes for being a trait on the *space* rather than on
/// the value.
///
/// # Errors
///
/// As [`write_response`].
pub fn write_response_with<W, V, F>(
  out: W,
  response: &Response<'_, V>,
  document: &str,
  mut write_leaf: F,
) -> Result<(), Error>
where
  W: fmt::Write,
  F: FnMut(&V, &mut Json<W>) -> Result<(), Error>,
{
  let mut json = Json::new(out);
  let mut root = json.object()?;

  // §7.1: the `errors` entry is present only when it is non-empty — "if the response contains no
  // errors, it must not contain this entry" — and an empty list would be a different response.
  if response.error_count() > 0 {
    // ONE index for the whole response, and not one per error: that is the whole of the repair,
    // so it is a value created here rather than inside the loop where a reader could not see the
    // difference. A response whose errors carry no locations never builds it.
    let mut locations = Locations::new(document);
    // ONE buffer for the whole response, for the same reason and out of the same round. It is
    // hoisted to exactly where the index is because it answers the same question about the other
    // walk: a path is discovered leaf-first and §7.1.2 wants it root-first, so something has to
    // hold the turned-around chain, and a `Vec` created inside the loop is an allocation per error
    // that no assertion about the *output* could see. Empty until the first path needs it.
    //
    // ── AND WHY IT IS NOT KEPT ACROSS RESPONSES, WHICH IS THE NEXT QUESTION ────────────────────
    //
    // A subscription writes a response per event through this same function, so a scratch that
    // lived in a writer object would amortise this allocation over a stream instead of paying it
    // per event. It is one allocation per response with errors — none at all for a clean one — and
    // buying it back is not free: a `Segment<'r>` borrows the response's name table, so a buffer
    // outliving `'r` cannot be typed. Reusing the ALLOCATION across responses means laundering the
    // element lifetime through `unsafe`, and the price of that is one `malloc` per response, on a
    // path that has already decided to build a `Locations` index — a strictly larger allocation on
    // exactly the same schedule. Per event the writer pays two, and both are proportional to
    // something the event actually has.
    let mut segments = Vec::new();
    let mut list = root.key("errors")?.array()?;
    for error in response.errors() {
      write_field_error(list.element()?, &error, &mut locations, &mut segments)?;
    }
    list.end()?;
  }

  // The depth travels with the node because the frame stack is sized from it — see `write_node`.
  write_node(
    root.key("data")?,
    response.data(),
    response.depth(),
    &mut write_leaf,
  )?;

  if let Some(extensions) = response.extensions() {
    write_extensions(root.key("extensions")?, extensions, &mut write_leaf)?;
  }

  root.end()
}

/// Writes a draft §7.1.3 request error result as JSON.
///
/// No `data` key, ever: §7.1.3 reserves the absence for a request that never ran, and the type
/// this takes has no `data` accessor to write. That is the distinction being kept — a present
/// `"data": null` says execution began and nulled the root.
///
/// # Errors
///
/// As [`write_response`].
pub fn write_request_error_result<W, V>(out: W, result: &RequestErrorResult<V>) -> Result<(), Error>
where
  W: fmt::Write,
  V: WriteJson,
{
  write_request_error_result_with(out, result, |value, json| value.write_json(json))
}

/// Writes a draft §7.1.3 request error result, with the caller saying how a leaf is written.
///
/// Only `extensions` can hold a driver value here, and it is the same door
/// [`write_response_with`] opens for the same reason.
///
/// # Errors
///
/// As [`write_response`].
pub fn write_request_error_result_with<W, V, F>(
  out: W,
  result: &RequestErrorResult<V>,
  mut write_leaf: F,
) -> Result<(), Error>
where
  W: fmt::Write,
  F: FnMut(&V, &mut Json<W>) -> Result<(), Error>,
{
  let mut json = Json::new(out);
  let mut root = json.object()?;

  // §7.1.3 makes `errors` non-empty by definition, and the type makes it exactly one.
  let mut list = root.key("errors")?.array()?;
  for error in result.errors() {
    let mut entry = list.element()?.object()?;
    entry.key("message")?.display(&error)?;
    entry.end()?;
  }
  list.end()?;

  if let Some(extensions) = result.extensions() {
    write_extensions(root.key("extensions")?, extensions, &mut write_leaf)?;
  }

  root.end()
}

/// Writes one entry of `errors` (draft §7.1.2).
///
/// `message` always, `locations` when the error has any, `path` when it is non-empty. The last is
/// not a tidying: a `@skip`/`@include` condition read over the *root* selection set is raised at
/// the root, where §7.1.2 says the entry is absent because the error "cannot be associated to a
/// particular field" — so an empty array would assert a position the specification says there is
/// none of.
///
/// No `extensions` on an error entry. §7.1.2 permits one and `graphql-proto` carries none, so
/// there is nothing to write; writing an empty map would be this writer inventing an entry.
///
/// `segments` is the caller's scratch, cleared and refilled per error — see the call site for why
/// it is the caller's.
fn write_field_error<'r, W, V>(
  json: &mut Json<W>,
  error: &FieldError<'r, V>,
  locations: &mut Locations<'_>,
  segments: &mut Vec<Segment<'r>>,
) -> Result<(), Error>
where
  W: fmt::Write,
{
  let mut entry = json.object()?;

  entry.key("message")?.display(error)?;

  let spans = error.locations();
  if !spans.is_empty() {
    let mut list = entry.key("locations")?.array()?;
    for span in spans {
      let (line, column) = locations.resolve(span.start())?;
      let mut position = list.element()?.object()?;
      position.key("line")?.number(line)?;
      position.key("column")?.number(column)?;
      position.end()?;
    }
    list.end()?;
  }

  // One climb of the response tree per error, into the buffer the caller lends, and the whole
  // path is in hand before the first bracket is written. `graphql-proto` owns the climb because
  // `graphql-proto` owns the links; what would be wrong here is asking it for the segments one at
  // a time, which is a request it cannot answer in constant work.
  //
  // It owns the buffer's GROWTH for the same reason — it is the side that knows how deep the path
  // is before walking it — and that is where the refusal comes from: the reservation is fallible,
  // so a response too deep for the allocator ends this write instead of aborting the process
  // holding it.
  let path = error
    .path()
    .collect_into(segments)
    .map_err(|_| Error::Allocation)?;
  if !path.is_empty() {
    let mut list = entry.key("path")?.array()?;
    for &segment in path {
      let slot = list.element()?;
      match segment {
        Segment::Field(name) => slot.string(name)?,
        Segment::Index(index) => slot.number(i64::from(index))?,
      }
    }
    list.end()?;
  }

  entry.end()
}

/// One container `write_node` has opened and not yet closed.
///
/// [`Children`] is the frame's whole content because it is the whole of what a level has left to
/// do: it holds the executor's slabs and the index of the next sibling, which is a handful of
/// words and no borrow of the sink. That is what makes an explicit stack possible here at all — a
/// stack of
/// [`Array`](super::Array)/[`Object`](super::Object) scoped writers is not expressible, because
/// each one borrows the [`Json`] the enclosing one has already borrowed.
struct Frame<'r, V> {
  children: Children<'r, V>,
  /// Whether the children carry response keys, which is also which bracket closes the frame.
  object: bool,
  /// Whether the next child is the first, and so is not preceded by a `,`.
  first: bool,
}

/// Writes the `data` entry, or any node inside it.
///
/// # On an explicit stack, because the depth is not this crate's to bound
///
/// One frame per open container, on the heap. It used to recurse, and the reason that was wrong is
/// one layer down rather than here: `graphql-proto` **deliberately** represents and walks response
/// state on the heap — draft §6.3's collection runs on an explicit stack, and its own
/// documentation records that the recursion it replaced aborted the process with `SIGABRT` at a
/// few thousand links — so the depth of a response is bounded by
/// [`Limits::max_response_slots`](crate::proto::Limits::max_response_slots) and by nothing that
/// looks at a native frame. A recursive serialiser spends a native frame per level of a shape
/// three configurable ceilings admit and no ceiling counts, which turns a response that *executed*
/// into a process that dies while writing it out, after the status line and part of the body have
/// already gone to the client.
///
/// So the bound is: **the native stack does not grow with the response**, and the frames that do
/// are one allocation whose length is the response's depth. `Vec::new` allocates nothing, so a
/// `data` that is a leaf, a `null` or a `__typename` still costs nothing on the heap.
///
/// # And the frames are one allocation because they are reserved, which they were not
///
/// "One allocation whose length is the response's depth" is what this walk was introduced claiming,
/// and moving the depth off the native stack is not what makes it true. An empty `Vec` pushed once
/// per container reallocates and copies its way up in powers of two, with the old buffer and the
/// new one live together at the peak, and `Vec::push` calls the allocation-error handler on a
/// refusal — so the repair for a process that died of the *native* stack was a process that dies of
/// the *heap*, at roughly 64 MiB of frames for a response at the default 2²⁰-slot ceiling, after
/// the status line and part of the body have gone to the client. That is the same defect one
/// structure over from the path buffer, and it survived a round because a structure that arrives as
/// the answer to a finding reads as already designed. It was — for the one property it repaired.
///
/// The fix is the one that buffer already got, from the number the executor already records:
/// `depth` is [`Response::depth`](graphql_proto::Response::depth), the deepest position in the
/// tree, so `depth + 1` frames is everything this stack can reach and one `try_reserve_exact`
/// buys it. **Exact and not amortised**, which is the opposite of the choice
/// [`Path::collect_into`](graphql_proto::Path::collect_into) makes and for the reason that one
/// gives: that buffer is shared by every error in the response, so its reservation is almost never
/// the last one made on it, and this one is filled once and freed with the walk.
///
/// The `+ 1` is the root's own frame — the root is at depth zero and is a container — and the
/// bound is slack by at most one more when the deepest position is a leaf, which takes no frame.
/// `Response::depth` says where the rest of the slack comes from.
///
/// The separators are placed here rather than by [`Array`](super::Array) and
/// [`Object`](super::Object) for the reason [`Frame`] gives; `Json::punct` is private to keep that
/// exception to this one walk.
///
/// # Errors
///
/// [`Error::Allocation`] when the allocator refuses room for the frames, and whatever the sink or a
/// leaf reports.
fn write_node<'r, W, V, F>(
  json: &mut Json<W>,
  node: Node<'r, V>,
  depth: usize,
  write_leaf: &mut F,
) -> Result<(), Error>
where
  W: fmt::Write,
  F: FnMut(&V, &mut Json<W>) -> Result<(), Error>,
{
  let mut stack: Vec<Frame<'r, V>> = Vec::new();
  if let Some(frame) = open(json, node, write_leaf)? {
    // After the root is written and only if it opened one: a `data` that is a leaf or a null has no
    // frames to reserve for, and reserving before `open` would charge it for the response's depth
    // anyway. `depth + 1` cannot overflow — a depth is at most one less than the number of slots
    // and that count is a `u32`.
    stack
      .try_reserve_exact(depth + 1)
      .map_err(|_| Error::Allocation)?;
    stack.push(frame);
  }

  loop {
    let Some(top) = stack.last_mut() else {
      return Ok(());
    };
    let object = top.object;
    // Everything the frame is asked for is taken in one step, so the borrow of the stack ends
    // before the body below can push onto it.
    let step = top
      .children
      .next()
      .map(|child| (core::mem::replace(&mut top.first, false), child));

    let Some((first, (key, child))) = step else {
      stack.pop();
      json.punct(if object { '}' } else { ']' })?;
      continue;
    };

    if !first {
      json.punct(',')?;
    }
    if object {
      match key {
        Segment::Field(name) => json.string(name)?,
        // Unreachable: an object's children are keyed by response key, and only a list's are
        // keyed by index. Rendered rather than asserted so the walk stays total.
        Segment::Index(index) => {
          let mut buffer = itoa::Buffer::new();
          json.string(buffer.format(index))?;
        }
      }
      json.punct(':')?;
    }
    if let Some(frame) = open(json, child, write_leaf)? {
      // The reservation above is the whole of this stack's growth, and it is an invariant rather
      // than a hope: a frame for the position at depth `d` sits at index `d`, and `d` is at most
      // the depth the reservation came from. A `push` past the capacity would still write a correct
      // response — which is why this is an assertion and not a refusal, since truncating a response
      // is a worse answer to a broken bound than paying for a reallocation — but it would be the
      // finding this reservation exists for, so the gate in `super::tests` fails on it too.
      debug_assert!(
        stack.len() < stack.capacity(),
        "the frame stack reserved {} frames and a {}th container was opened, so the depth it was \
         sized from does not bound this walk",
        stack.capacity(),
        stack.len() + 1
      );
      stack.push(frame);
    }
  }
}

/// Writes as much of `node` as can be written without descending, and returns the frame to descend
/// into when there is one.
///
/// A scalar is written whole; a container is opened, and closing it is the walk's business. Both
/// entrances to a node go through here — the root of `data`, and every child — so a value that a
/// list writes and an object does not is unrepresentable.
fn open<'r, W, V, F>(
  json: &mut Json<W>,
  node: Node<'r, V>,
  write_leaf: &mut F,
) -> Result<Option<Frame<'r, V>>, Error>
where
  W: fmt::Write,
  F: FnMut(&V, &mut Json<W>) -> Result<(), Error>,
{
  Ok(match node {
    Node::Null => {
      json.null()?;
      None
    }
    Node::Leaf(value) => {
      write_leaf(value, json)?;
      None
    }
    // Draft §4.4's `__typename` is the executor's own answer and is a `String` in the response,
    // written the same way a response key is.
    Node::TypeName(name) => {
      json.string(name)?;
      None
    }
    Node::List(children) => {
      json.punct('[')?;
      Some(Frame {
        children,
        object: false,
        first: true,
      })
    }
    Node::Object(children) => {
      json.punct('{')?;
      Some(Frame {
        children,
        object: true,
        first: true,
      })
    }
  })
}

/// Writes a draft §7.1.7 `extensions` map.
///
/// The keys are the driver's, under no lexical restriction at all, so they are escaped like any
/// other string; the values are the driver's and are written by the same leaf writer `data` uses.
fn write_extensions<W, V, F>(
  json: &mut Json<W>,
  extensions: &Extensions<V>,
  write_leaf: &mut F,
) -> Result<(), Error>
where
  W: fmt::Write,
  F: FnMut(&V, &mut Json<W>) -> Result<(), Error>,
{
  let mut map = json.object()?;
  for (key, value) in extensions {
    write_leaf(value, map.key(key)?)?;
  }
  map.end()
}

// ---------------------------------------------------------------------------------------------
// what the writer spends — THREE axes, and no two of them can see each other's defect
// ---------------------------------------------------------------------------------------------

// How many document bytes the writer has looked at, and how many times it went to the allocator.
// Both counted only when this crate's own tests are built. The third axis — native stack bytes —
// needs no counter here: `super::tests` reads it as a difference of two addresses.
//
// ── WHY BYTES ─────────────────────────────────────────────────────────────────────────────────
//
// The location property is about *scanning* and about nothing else: deriving a line and column by
// walking the prefix allocates nothing, so an allocation pin is structurally incapable of seeing
// it, and a clock sees it along with the machine it ran on. This is `smear-schema`'s introspection
// reader's instrument, reached for rather than reinvented — that module's own header carries the
// argument at length, and it learned it by writing the wrong gate first.
//
// A byte looked at twice counts twice, because that is precisely the defect.
//
// ── AND WHY ALLOCATIONS, BESIDE IT RATHER THAN INSTEAD OF IT ──────────────────────────────────
//
// Because each of the other two is blind to the frame stack's own growth, and this file's history
// is that blindness four times over:
//
//   | round   | the gate measured  | it was blind to                                  |
//   |---------|--------------------|--------------------------------------------------|
//   | #162 R2 | document bytes     | a walk spending the NATIVE stack                 |
//   | #162 R2 | native stack bytes | the heap the walk that replaced it grew           |
//   | #162 R3 | slot traversals    | reallocation and copying (`graphql-proto`)        |
//   | #162 R4 | those two, jointly | a THIRD buffer, one structure over                |
//
// The native-stack probe is the sharpest case. It was written for this very walk, it passes on a
// stack that reallocates its way to 64 MiB, and it would go on passing: moving a cost off the
// native stack is exactly what makes an allocation counter the instrument that can see what
// happened to it. A gate needs every axis its subject can grow along, and the answer to "is two
// enough" is the enumeration in this module's header rather than another round.
//
// `realloc` counts, and counts as one event. Geometric growth is mostly `realloc`, so an allocator
// that forwarded it silently would report the defect this instrument exists for as one allocation.
#[cfg(test)]
thread_local! {
  /// Document bytes the writer has looked at on this thread.
  static VISITED: core::cell::Cell<u64> = const { core::cell::Cell::new(0) };
  /// Allocating events on this thread, whoever made them.
  static ALLOCATED: core::cell::Cell<u64> = const { core::cell::Cell::new(0) };
  /// The request size, in bytes, at and above which the allocator answers no on this thread.
  static REFUSE_AT: core::cell::Cell<usize> = const { core::cell::Cell::new(usize::MAX) };
}

/// Records that the writer looked at `count` bytes of the document.
///
/// Compiles to nothing outside this crate's own tests.
#[inline]
fn visit(count: usize) {
  #[cfg(test)]
  VISITED.with(|visited| visited.set(visited.get() + count as u64));
  #[cfg(not(test))]
  let _ = count;
}

/// Returns how many document bytes the writer has looked at on this thread.
#[cfg(test)]
pub(super) fn visited() -> u64 {
  VISITED.with(core::cell::Cell::get)
}

/// A pass-through global allocator that tallies every allocating event on the calling thread.
///
/// `graphql-proto`'s shape, one crate down, and here for the reason that one is in a *library* test
/// rather than an integration target: the counter has to be read beside `visited`, which is private
/// to this module. A `#[global_allocator]` belongs to a binary and an integration target's is not
/// reachable from a lib test, so there is nothing to import.
///
/// It counts the whole thread and not this module, which is what the gate wants: the claim is about
/// how many times *writing a response* goes to the allocator, and a buffer some other layer grows
/// on the writer's behalf would be exactly the kind of thing a module-scoped counter would miss.
/// The gate's fixture writes into a sink that allocates nothing, so what it reads is the writer's.
#[cfg(test)]
struct Counting;

#[cfg(test)]
// SAFETY: every method forwards to `System` with the layout it was given, unchanged, and returns
// exactly what `System` returned — or a null pointer, which is the spelling of "the allocator said
// no" and is what every caller of this trait already has to handle. The tally is a `Cell<u64>` in
// thread-local storage, reached through `try_with` so that an allocation made while that storage is
// being set up or torn down is left uncounted rather than re-entering it. A refused `realloc`
// returns null and does **not** forward, so the block it was given is untouched and still the
// caller's, which is what `GlobalAlloc::realloc` requires of a failure.
unsafe impl core::alloc::GlobalAlloc for Counting {
  unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
    allocate();
    if refused(layout.size()) {
      return core::ptr::null_mut();
    }
    unsafe { std::alloc::System.alloc(layout) }
  }

  unsafe fn alloc_zeroed(&self, layout: core::alloc::Layout) -> *mut u8 {
    allocate();
    if refused(layout.size()) {
      return core::ptr::null_mut();
    }
    unsafe { std::alloc::System.alloc_zeroed(layout) }
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: core::alloc::Layout, new_size: usize) -> *mut u8 {
    allocate();
    if refused(new_size) {
      return core::ptr::null_mut();
    }
    unsafe { std::alloc::System.realloc(ptr, layout, new_size) }
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: core::alloc::Layout) {
    unsafe { std::alloc::System.dealloc(ptr, layout) }
  }
}

/// Makes this thread's allocator answer no to any single request of `bytes` or more, until
/// [`allow_allocations`] puts it back.
///
/// # Why an allocator that refuses is the only instrument for the door
///
/// `Error::Allocation`'s documentation makes the strongest claim in this module — that **every**
/// allocation the writer makes is behind that variant, so a refusal is something a server says
/// rather than something it dies of. Nothing measured it. An allocation *counter* cannot: it sees
/// how many requests were made and not what happens when one is answered no, and `Vec::push` and
/// `Vec::with_capacity` register on it identically to `try_reserve` right up to the moment they
/// abort the process instead of returning.
///
/// A size threshold rather than a countdown, because it is what makes the door **attributable**:
/// each of the four buffers can be put on its own fixture where it is the only request above the
/// threshold, so the refusal that comes back names one buffer rather than "one of four".
///
/// # Nothing may panic between this and [`allow_allocations`]
///
/// Not a style rule. Rust's panic machinery allocates — the message, and the backtrace printer's
/// own buffer — so a panic while the allocator is armed meets a refusal inside the code that is
/// handling the first one. Observed on this tree while checking one of these fixtures: an unwrap
/// inside the window printed `memory allocation of 6144 bytes failed`, could not print its
/// backtrace, and then hung instead of aborting. So every fixture arms the allocator immediately
/// before the call under test, disarms immediately after, and asserts afterwards.
#[cfg(test)]
pub(super) fn refuse_allocations_of_at_least(bytes: usize) {
  REFUSE_AT.with(|refuse| refuse.set(bytes));
}

/// Puts this thread's allocator back to answering every request.
#[cfg(test)]
pub(super) fn allow_allocations() {
  REFUSE_AT.with(|refuse| refuse.set(usize::MAX));
}

/// Returns whether a request of this size is one the armed allocator refuses.
///
/// `try_with` and a default of "allowed" for the reason the tally uses it: this runs inside the
/// global allocator, so a thread whose local storage is being set up or torn down must get an
/// answer rather than a recursive initialisation.
#[cfg(test)]
fn refused(size: usize) -> bool {
  REFUSE_AT
    .try_with(|refuse| size >= refuse.get())
    .unwrap_or(false)
}

#[cfg(test)]
#[global_allocator]
static ALLOCATOR: Counting = Counting;

/// Records one allocating event.
#[cfg(test)]
fn allocate() {
  let _ = ALLOCATED.try_with(|allocated| allocated.set(allocated.get() + 1));
}

/// Returns how many allocating events this thread has caused.
#[cfg(test)]
pub(super) fn allocations() -> u64 {
  ALLOCATED.with(core::cell::Cell::get)
}

mod document {
  //! The document, behind the one accessor that counts what it reads.
  //!
  //! **A module, and not merely a convention.** `smear-schema`'s reader routes every read through
  //! one function and says so in a comment, and that is as strong as the next person to add a
  //! derivation — a snippet, a caret, whatever the next field on an error entry turns out to be.
  //! The measurement is only a measurement of the writer if there is no second way in, so the text
  //! is a private field of a private module: reaching it without being counted is not a slip, it
  //! is an edit to this file that has to delete a door to get at another one.
  //!
  //! Nothing here is about correctness. Every method would be a one-liner on `&str`; what the
  //! module buys is that `super`'s ~200 lines of position arithmetic cannot spell `.text`.

  /// The source text a response's positions are resolved against.
  pub(super) struct Document<'d> {
    text: &'d str,
  }

  impl<'d> Document<'d> {
    /// Wraps the text, reading none of it.
    pub(super) const fn new(text: &'d str) -> Self {
      Self { text }
    }

    /// Returns how many bytes the document has.
    ///
    /// Not a look: a length is metadata the `&str` already carries and no byte is read to answer
    /// it.
    pub(super) const fn len(&self) -> usize {
      self.text.len()
    }

    /// Returns the document's bytes in `from..to`, and records that the writer looked at them.
    pub(super) fn look(&self, from: usize, to: usize) -> &'d [u8] {
      let bytes = &self.text.as_bytes()[from..to];
      super::visit(bytes.len());
      bytes
    }

    /// Rounds an offset down to the character boundary at or before it, clamped to the document.
    ///
    /// At most three bytes are looked at, and they are counted like any other.
    pub(super) fn char_boundary(&self, at: usize) -> usize {
      let mut at = at.min(self.len());
      while at > 0 && at < self.len() && !super::is_char_start(self.look(at, at + 1)[0]) {
        at -= 1;
      }
      at
    }
  }
}

use document::Document;

// ---------------------------------------------------------------------------------------------
// draft §7.1.2's line and column, once per document rather than once per error
// ---------------------------------------------------------------------------------------------

/// How many document bytes one [`Checkpoint`] covers.
///
/// It is the two costs traded against each other, so it is one number rather than two: the index
/// is one checkpoint per this many bytes, and resolving one location scans at most this many
/// twice. 256 puts the index at three words per 256 bytes — under a tenth of the document on a
/// 64-bit target — and the scan at a few cache lines' worth of work per error.
const STRIDE: usize = 256;

/// What the writer knows about the document at a multiple of [`STRIDE`] bytes, so that it does not
/// have to walk there.
#[derive(Clone, Copy)]
struct Checkpoint {
  /// The one-based line this offset is on.
  line: usize,
  /// Where that line starts.
  line_start: usize,
  /// How many characters of the document lie before this offset.
  ///
  /// A *character* count and not a byte count, because the column is one — see [`Locations`].
  chars: usize,
}

/// Draft §7.1.2's `line` and `column` for every location in one response.
///
/// # Why an index, when the walk it replaces was two dozen lines
///
/// Every error entry carries at least one location and a response may carry as many errors as it
/// has fields, so deriving each position by walking the prefix is Θ(n²) over a response a remote
/// client shapes — a valid flat query selects tens of thousands of uniquely aliased fields, a
/// degraded resolver fails every one of them, and the offsets increase. The cost lands **precisely
/// when the dependencies are already failing**, which is the worst moment for a service to also
/// become quadratic. `smear-schema`'s introspection decoder answers the same primitive by staging
/// the walk and performing it once, because there exactly one refusal escapes; here every location
/// is kept, so there is nothing to defer and the document is indexed instead.
///
/// # Why checkpoints at a fixed stride, and not one entry per line
///
/// A line index is the obvious shape and it is the one a document can amplify: it costs a word per
/// line, so a megabyte of nothing but line terminators becomes a multiple of the document. An
/// attacker chooses the document. A checkpoint every [`STRIDE`] bytes costs the same three words
/// per 256 whatever the text is — no shape makes it larger — and it bounds the per-location scan at
/// the same time, which a line index does not: a single line the length of the document leaves the
/// column to be counted from its start.
///
/// # What it is built lazily for
///
/// A response with no `errors` never reaches this type at all, and one whose errors carry no
/// locations builds nothing: the pass over the document happens on the first
/// [`resolve`](Locations::resolve) and never again.
///
/// # The two choices the positions themselves embody, unchanged by any of the above
///
/// **Line terminators are draft §2.1.1's three**: a line feed, a carriage return, and the pair,
/// which counts as one. A document written on Windows and a document written on Unix therefore
/// report the same line for the same token, which they would not if only `\n` were counted.
///
/// **The column counts characters, not bytes.** The specification does not say which, and the
/// reference implementation counts UTF-16 code units because it is written in JavaScript — so on
/// a line of ASCII, which is every case in this repository's `graphql-js` differential, all three
/// readings agree, and they diverge only where the line already contains non-ASCII. Counting
/// characters is the reading a human counting along the line would give, and it is the only one of
/// the three that does not depend on an encoding the document is not stored in.
pub(super) struct Locations<'d> {
  document: Document<'d>,
  /// One entry per [`STRIDE`] bytes of the document, plus one for its end. Empty until the first
  /// location is resolved, and never empty afterwards — the offset-zero entry always exists — so
  /// emptiness is the "not built yet" flag and there is no second field to keep in step with it.
  checkpoints: Vec<Checkpoint>,
}

impl<'d> Locations<'d> {
  /// Prepares to resolve positions against `document`, reading none of it.
  pub(super) const fn new(document: &'d str) -> Self {
    Self {
      document: Document::new(document),
      checkpoints: Vec::new(),
    }
  }

  /// Returns draft §7.1.2's one-based line and column for a byte offset.
  ///
  /// Total on any offset. One past the end, or past it by a mile, is the end of the document; one
  /// interior to a character is that character's own position, which is the walk this replaced
  /// clamping the same way.
  ///
  /// # Errors
  ///
  /// [`Error::Allocation`] when the allocator refuses room for the index, which is the first
  /// location's cost and no later one's.
  pub(super) fn resolve(&mut self, offset: usize) -> Result<(i64, i64), Error> {
    if self.checkpoints.is_empty() {
      self.build()?;
    }

    let end = self.document.char_boundary(offset);

    // The checkpoint at or before `end`, and then the at most `STRIDE` bytes between them. No
    // line terminator in that stretch is missed and none is counted twice: the checkpoint holds
    // the state a walk from zero would have been in, and the one shape that cannot be recovered
    // from the state alone — a `\r\n` split by the checkpoint — is what `after_cr` carries.
    let at = end - end % STRIDE;
    let checkpoint = self.checkpoints[at / STRIDE];
    let mut line = checkpoint.line;
    let mut line_start = checkpoint.line_start;
    let mut chars = checkpoint.chars;
    let mut after_cr = at > 0 && self.document.look(at - 1, at)[0] == b'\r';

    let mut cursor = at;
    for &byte in self.document.look(at, end) {
      cursor += 1;
      if is_char_start(byte) {
        chars += 1;
      }
      match byte {
        // A line feed after a carriage return is the second half of one terminator, so the line
        // was already counted; what it still does is move the start of the line past it.
        b'\n' => {
          if !after_cr {
            line += 1;
          }
          line_start = cursor;
          after_cr = false;
        }
        b'\r' => {
          line += 1;
          line_start = cursor;
          after_cr = true;
        }
        _ => after_cr = false,
      }
    }

    // The column is a *character* count from the start of the line, and the line may have begun
    // before the checkpoint — a long line is exactly the case a line index would have handled and
    // this one has to answer, so the count is a difference of two absolute character counts and
    // not a walk from wherever the line happens to start.
    let column = chars - self.chars_before(line_start) + 1;
    Ok((as_i64(line), as_i64(column)))
  }

  /// Returns how many characters of the document lie before `at`, which must be a checkpointed or
  /// character-boundary offset.
  fn chars_before(&self, at: usize) -> usize {
    let base = at - at % STRIDE;
    let checkpoint = self.checkpoints[base / STRIDE];
    checkpoint.chars
      + self
        .document
        .look(base, at)
        .iter()
        .filter(|&&byte| is_char_start(byte))
        .count()
  }

  /// Walks the document once, recording what a walk from zero would know at every [`STRIDE`]
  /// bytes.
  ///
  /// # Errors
  ///
  /// [`Error::Allocation`]. The whole size is a function of the document's length, so it is one
  /// exact request made before the walk — `try_reserve_exact` and not `try_reserve`, because
  /// unlike the `segments` buffer this one is filled once and never added to again, and unlike the
  /// frame stack the fill reaches the reservation exactly rather than at most.
  ///
  /// **Fallible at all** because it is the third buffer, and the point of the table in this
  /// module's header is that there is no fourth: the document is a client's and the index is a
  /// fixed fraction of it, so an allocator refusing one is an answer this writer can give.
  /// `Vec::with_capacity` was the ordinary Rust behaviour here and would abort the process instead.
  fn build(&mut self) -> Result<(), Error> {
    let bytes = self.document.look(0, self.document.len());
    let mut checkpoints = Vec::new();
    checkpoints
      .try_reserve_exact(bytes.len() / STRIDE + 1)
      .map_err(|_| Error::Allocation)?;

    let mut line = 1usize;
    let mut line_start = 0usize;
    let mut chars = 0usize;
    let mut after_cr = false;

    for (at, &byte) in bytes.iter().enumerate() {
      if at.is_multiple_of(STRIDE) {
        checkpoints.push(Checkpoint {
          line,
          line_start,
          chars,
        });
      }
      if is_char_start(byte) {
        chars += 1;
      }
      match byte {
        b'\n' => {
          if !after_cr {
            line += 1;
          }
          line_start = at + 1;
          after_cr = false;
        }
        b'\r' => {
          line += 1;
          line_start = at + 1;
          after_cr = true;
        }
        _ => after_cr = false,
      }
    }
    // The end of the document is a resolvable offset like any other, and on an exact multiple of
    // the stride it is the entry nothing else would have pushed.
    if bytes.len().is_multiple_of(STRIDE) {
      checkpoints.push(Checkpoint {
        line,
        line_start,
        chars,
      });
    }

    self.checkpoints = checkpoints;
    Ok(())
  }
}

/// Returns whether a byte begins a character, which is every byte that is not a UTF-8
/// continuation.
///
/// Counting these is counting characters, and it is what lets the column be a difference of two
/// running totals rather than a `chars()` walk from the start of a line.
#[inline]
const fn is_char_start(byte: u8) -> bool {
  byte & 0xc0 != 0x80
}

/// Narrows a count to the width draft §7.1.2's numbers are written at.
///
/// Saturating rather than wrapping or panicking: a line or column past `i64::MAX` needs a document
/// of nine exabytes, and the honest answer for one is a number that is merely wrong rather than a
/// negative position or a dead process.
#[inline]
fn as_i64(count: usize) -> i64 {
  i64::try_from(count).unwrap_or(i64::MAX)
}
