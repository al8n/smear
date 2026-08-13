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
//!   file owns is the buffer, hoisted beside [`Locations`] so that one serves the whole response.
//!
//! All three are stated as bounds and all three are measured — see this module's `visited` counter
//! and the gates in `super::tests`, and `graphql-proto`'s own slot-traversal gate for the third —
//! because a cost claim settled by reading the code is an opinion.
//!
//! **And it is worth saying why this file keeps attracting them.** It is the only place that
//! touches the whole document and the whole response tree once per error, so every decision made
//! elsewhere on the grounds that an item is cheap meets its worst case here, all at once and with
//! the count chosen by the client. Three findings, three different mechanisms — a prefix rescan, a
//! native recursion, and a parent-link re-walk — and none of the three was cheap by accident.

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
    let mut segments = Vec::new();
    let mut list = root.key("errors")?.array()?;
    for error in response.errors() {
      write_field_error(list.element()?, &error, &mut locations, &mut segments)?;
    }
    list.end()?;
  }

  write_node(root.key("data")?, response.data(), &mut write_leaf)?;

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
      let (line, column) = locations.resolve(span.start());
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
  let path = error.path().collect_into(segments);
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
/// The separators are placed here rather than by [`Array`](super::Array) and
/// [`Object`](super::Object) for the reason [`Frame`] gives; `Json::punct` is private to keep that
/// exception to this one walk.
fn write_node<'r, W, V, F>(
  json: &mut Json<W>,
  node: Node<'r, V>,
  write_leaf: &mut F,
) -> Result<(), Error>
where
  W: fmt::Write,
  F: FnMut(&V, &mut Json<W>) -> Result<(), Error>,
{
  let mut stack: Vec<Frame<'r, V>> = Vec::new();
  if let Some(frame) = open(json, node, write_leaf)? {
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
// what the writer looks at
// ---------------------------------------------------------------------------------------------

// How many document bytes the writer has looked at, counted only when this crate's own tests are
// built.
//
// ── WHY BYTES, AND NOT ALLOCATIONS OR A CLOCK ─────────────────────────────────────────────────
//
// The property below is about *scanning* and about nothing else: deriving a line and column by
// walking the prefix allocates nothing, so an allocation pin is structurally incapable of seeing
// it, and a clock sees it along with the machine it ran on. This is `smear-schema`'s introspection
// reader's instrument, reached for rather than reinvented — that module's own header carries the
// argument at length, and it learned it by writing the wrong gate first.
//
// A byte looked at twice counts twice, because that is precisely the defect.
#[cfg(test)]
thread_local! {
  /// Document bytes the writer has looked at on this thread.
  static VISITED: core::cell::Cell<u64> = const { core::cell::Cell::new(0) };
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
  pub(super) fn resolve(&mut self, offset: usize) -> (i64, i64) {
    if self.checkpoints.is_empty() {
      self.build();
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
    (as_i64(line), as_i64(column))
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
  fn build(&mut self) {
    let bytes = self.document.look(0, self.document.len());
    let mut checkpoints = Vec::with_capacity(bytes.len() / STRIDE + 1);

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
