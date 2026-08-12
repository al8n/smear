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

use core::fmt;

use graphql_proto::{Error as FieldError, Extensions, Node, RequestErrorResult, Response, Segment};

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
    let mut list = root.key("errors")?.array()?;
    for error in response.errors() {
      write_field_error(list.element()?, &error, document)?;
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
fn write_field_error<W, V>(
  json: &mut Json<W>,
  error: &FieldError<'_, V>,
  document: &str,
) -> Result<(), Error>
where
  W: fmt::Write,
{
  let mut entry = json.object()?;

  entry.key("message")?.display(error)?;

  let locations = error.locations();
  if !locations.is_empty() {
    let mut list = entry.key("locations")?.array()?;
    for span in locations {
      let (line, column) = line_column(document, span.start());
      let mut position = list.element()?.object()?;
      position.key("line")?.number(line)?;
      position.key("column")?.number(column)?;
      position.end()?;
    }
    list.end()?;
  }

  let path = error.path();
  let mut segments = path.iter().peekable();
  if segments.peek().is_some() {
    let mut list = entry.key("path")?.array()?;
    for segment in segments {
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

/// Writes the `data` entry, or any node inside it.
///
/// Recursive, for the reason the module header gives: the depth is the response's, which is the
/// document's shape and not a driver's answer.
fn write_node<W, V, F>(
  json: &mut Json<W>,
  node: Node<'_, V>,
  write_leaf: &mut F,
) -> Result<(), Error>
where
  W: fmt::Write,
  F: FnMut(&V, &mut Json<W>) -> Result<(), Error>,
{
  match node {
    Node::Null => json.null(),
    Node::Leaf(value) => write_leaf(value, json),
    // Draft §4.4's `__typename` is the executor's own answer and is a `String` in the response,
    // written the same way a response key is.
    Node::TypeName(name) => json.string(name),
    Node::List(children) => {
      let mut list = json.array()?;
      for (_, child) in children {
        write_node(list.element()?, child, write_leaf)?;
      }
      list.end()
    }
    Node::Object(children) => {
      let mut object = json.object()?;
      for (key, child) in children {
        let slot = match key {
          Segment::Field(name) => object.key(name)?,
          // Unreachable: an object's children are keyed by response key, and only a list's are
          // keyed by index. Rendered rather than asserted so the walk stays total.
          Segment::Index(index) => {
            let mut buffer = itoa::Buffer::new();
            object.key(buffer.format(index))?
          }
        };
        write_node(slot, child, write_leaf)?;
      }
      object.end()
    }
  }
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

/// Turns a byte offset into draft §7.1.2's one-based line and column.
///
/// # Two choices, both of which a reader is entitled to know
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
///
/// Linear in the offset, and deliberately: an index would be a per-response allocation to make a
/// per-error walk cheaper, and a response with errors has few of them relative to its size. It is
/// the same trade `smear-schema`'s introspection decoder records.
pub(super) fn line_column(document: &str, offset: usize) -> (i64, i64) {
  let mut end = offset.min(document.len());
  while !document.is_char_boundary(end) {
    end -= 1;
  }

  let mut line = 1i64;
  let mut column = 1i64;
  let mut chars = document[..end].chars();
  while let Some(ch) = chars.next() {
    match ch {
      '\n' => {
        line += 1;
        column = 1;
      }
      '\r' => {
        // A carriage return followed by a line feed is one terminator, so the feed is consumed
        // here rather than counted again on the next turn.
        if chars.clone().next() == Some('\n') {
          chars.next();
        }
        line += 1;
        column = 1;
      }
      _ => column += 1,
    }
  }
  (line, column)
}
