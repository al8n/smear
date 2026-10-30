//! The canonical text of a constant value, in graphql-js's `print` form.
//!
//! # What canonical means here
//!
//! Draft §4's `defaultValue` is "a GraphQL-formatted string", and the schema keeps one per input
//! value that declares a default. It is printed once, at build, the way graphql-js prints a value
//! node: a list as `[a, b]`, an object as `{a: 1, b: 2}`, a string re-encoded as a quoted string —
//! a block string included, since graphql-js prints what it decoded rather than how it was spelled —
//! and an integer, a float, an enum value, a boolean and `null` as written. Two spellings of one
//! literal therefore print the same, and printing a printed value gives it back unchanged.
//!
//! What it is not is graphql-js's `defaultValue` for the same schema. graphql-js prints the value
//! *after coercing it* to the argument's type, so `[Int] = 1` reports `[1]`, a `Float` written `1.0`
//! reports `1`, and an input object reports the fields its own defaults fill in. This prints the
//! literal the document wrote, because coercion is execution's and the schema keeps no coerced
//! values.
//!
//! # A walk with a stack of its own
//!
//! The literal's depth is the caller's — every carrier on the route to a default has a public
//! constructor — so the walk holds one frame per open container on the heap and none on the native
//! stack, and stops where [`SchemaBuilder::const_value`](super::SchemaBuilder::const_value) stops:
//! past [`MAX_CONST_VALUE_DEPTH`] open containers. That literal is already refused by then, so
//! stopping prints nothing and reports nothing.
//!
//! # Measured, then written
//!
//! Every print runs twice: into a [`Measure`] that only counts, and then into the arena, which is
//! reserved from the count in between. The first pass is also where an unreadable string is
//! found, so the arena never holds half a value.

use std::{string::String, vec::Vec};

use smear_parser::{
  graphql::ast::{ConstInputValue, ConstObjectField},
  lexer::{error::StringValueError, string_value_pieces},
};
use tokora::{SimpleSpan, span::AsSpan};

use super::MAX_CONST_VALUE_DEPTH;

/// Where printed text goes: the text arena, or a count of what would have gone there.
pub(crate) trait Sink {
  /// Takes the next piece of text.
  fn put(&mut self, text: &str);
}

impl Sink for String {
  #[inline]
  fn put(&mut self, text: &str) {
    self.push_str(text);
  }
}

/// What a print would write, counted so the arena can be reserved before anything is written.
#[derive(Debug, Default)]
pub(super) struct Measure(pub(super) usize);

impl Sink for Measure {
  #[inline]
  fn put(&mut self, text: &str) {
    self.0 += text.len();
  }
}

/// Why a constant value has no canonical text.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Unprinted {
  /// The value opens more than [`MAX_CONST_VALUE_DEPTH`] containers, which
  /// [`SchemaBuilder::const_value`](super::SchemaBuilder::const_value) has already refused it for.
  TooDeep,
  /// A string in it is not text, or a spelling in it is not UTF-8; the span is that literal's.
  Unreadable(SimpleSpan),
}

/// Prints a constant value in graphql-js's `print` form. See the module header for the form.
///
/// The introspection door prints a response's default with this too, where it has to compare a
/// specified directive's argument with the one the build injects.
pub(crate) fn canonical<S, K>(value: &ConstInputValue<S>, sink: &mut K) -> Result<(), Unprinted>
where
  S: AsRef<[u8]>,
  K: Sink,
{
  /// A container whose children are still being printed, and whether the next one is its first.
  enum Frame<'a, S> {
    List(core::slice::Iter<'a, ConstInputValue<S>>, bool),
    Object(core::slice::Iter<'a, ConstObjectField<S>>, bool),
  }

  let mut frames: Vec<Frame<'_, S>> = Vec::new();
  let mut current = Some(value);
  loop {
    let Some(value) = current.take() else {
      // Nothing in hand: take the next child of the innermost open container, or close it.
      match frames.last_mut() {
        None => return Ok(()),
        Some(Frame::List(rest, first)) => match rest.next() {
          Some(item) => {
            if !core::mem::replace(first, false) {
              sink.put(", ");
            }
            current = Some(item);
          }
          None => {
            sink.put("]");
            frames.pop();
          }
        },
        Some(Frame::Object(rest, first)) => match rest.next() {
          Some(field) => {
            if !core::mem::replace(first, false) {
              sink.put(", ");
            }
            let name = field.name();
            spelling(name.source().as_ref(), *name.as_span(), sink)?;
            sink.put(": ");
            current = Some(field.value());
          }
          None => {
            sink.put("}");
            frames.pop();
          }
        },
      }
      continue;
    };

    match value {
      ConstInputValue::Null(_) => sink.put("null"),
      ConstInputValue::Boolean(boolean) => sink.put(if boolean.value() { "true" } else { "false" }),
      ConstInputValue::Int(int) => spelling(int.source().as_ref(), *int.span(), sink)?,
      ConstInputValue::Float(float) => spelling(float.source().as_ref(), *float.span(), sink)?,
      ConstInputValue::Enum(member) => spelling(member.source().as_ref(), *member.span(), sink)?,
      ConstInputValue::String(string) => quote_spelling(string.source().as_ref(), sink)
        .map_err(|_| Unprinted::Unreadable(*string.span()))?,
      // The ceiling is read before the frame is pushed, as `const_value` reads it, so the two stop
      // at the same container. An empty container needs no frame — `[]` and `{}` are the commonest
      // defaults there are — so it costs the walk no allocation at all.
      ConstInputValue::List(list) => {
        if frames.len() >= MAX_CONST_VALUE_DEPTH {
          return Err(Unprinted::TooDeep);
        }
        match list.values() {
          [] => sink.put("[]"),
          values => {
            sink.put("[");
            frames.push(Frame::List(values.iter(), true));
          }
        }
      }
      ConstInputValue::Object(object) => {
        if frames.len() >= MAX_CONST_VALUE_DEPTH {
          return Err(Unprinted::TooDeep);
        }
        match object.fields() {
          [] => sink.put("{}"),
          fields => {
            sink.put("{");
            frames.push(Frame::Object(fields.iter(), true));
          }
        }
      }
    }
  }
}

/// Writes a lexeme as it was spelled: an integer, a float, an enum value or an object field's
/// name.
///
/// All four are ASCII by the grammar that lexed them, so the UTF-8 check is the arena's admission
/// rule rather than a reading the parse leaves open.
fn spelling<K: Sink>(bytes: &[u8], span: SimpleSpan, sink: &mut K) -> Result<(), Unprinted> {
  let text = core::str::from_utf8(bytes).map_err(|_| Unprinted::Unreadable(span))?;
  sink.put(text);
  Ok(())
}

/// Writes a `StringValue` spelling as the quoted string graphql-js prints for its value.
fn quote_spelling<K: Sink>(spelling: &[u8], sink: &mut K) -> Result<(), StringValueError> {
  sink.put("\"");
  string_value_pieces(spelling, |piece| escape(piece, sink))?;
  sink.put("\"");
  Ok(())
}

/// The hex digits graphql-js's `\u00XX` escapes spell, upper case.
const HEX: [&str; 16] = [
  "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F",
];

/// `printString`'s escaping, without the quotes: the short forms for `\b` `\t` `\n` `\f` `\r` `"`
/// and `\`, and `\u00XX` for every other code point in U+0000–U+001F and U+007F–U+009F. `/` is left
/// alone, as graphql-js leaves it.
///
/// The introspection door writes a response's descriptions and reasons through this too, between
/// two quotes, which is what makes its SDL safe to hand the parser: a quote or a line terminator in
/// the text comes out escaped, so no text can close the literal it sits in.
pub(crate) fn escape<K: Sink>(text: &str, sink: &mut K) {
  let mut clean = 0;
  for (at, ch) in text.char_indices() {
    let short = match ch {
      '"' => Some("\\\""),
      '\\' => Some("\\\\"),
      '\u{8}' => Some("\\b"),
      '\t' => Some("\\t"),
      '\n' => Some("\\n"),
      '\u{c}' => Some("\\f"),
      '\r' => Some("\\r"),
      _ if ch <= '\u{1f}' || ('\u{7f}'..='\u{9f}').contains(&ch) => None,
      _ => continue,
    };
    sink.put(&text[clean..at]);
    match short {
      Some(short) => sink.put(short),
      None => {
        // At most U+009F, so the high digit is at most 9.
        let code = u32::from(ch) as usize;
        sink.put("\\u00");
        sink.put(HEX[code >> 4]);
        sink.put(HEX[code & 0xf]);
      }
    }
    clean = at + ch.len_utf8();
  }
  sink.put(&text[clean..]);
}

#[cfg(test)]
mod tests;
