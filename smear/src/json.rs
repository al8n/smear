//! Draft §7.2.1 JSON serialisation, for the draft §7.1 response `graphql-proto` builds.
//!
//! # Why this is here and not one layer down
//!
//! `graphql-proto`'s header says it does not write a response out, and that refusal was never
//! arbitrary: writing one means writing the driver's `V`, and
//! [`Values`](crate::proto::Values) asks seven structural questions of which none is
//! *write this*. A crate that cannot render a leaf cannot render the tree the leaves are in.
//!
//! What unblocked it is the materialised value layer — with
//! [`parser::graphql::ast::materialized`](crate::parser::graphql::ast::materialized) there is a
//! concrete tree to write, so the writer lives *above* both and neither layer has to grow an
//! eighth question. [`materialized`] holds the two implementations, one per width.
//!
//! # What the writer refuses to guess
//!
//! Three of JSON's edges are decisions rather than mechanics, and each one is taken here on the
//! record rather than inherited:
//!
//! - **Integers beyond draft §3.5.1's 32-bit `Int` are written as JSON strings**, which is what
//!   `BigInt`-style custom scalars already do. [`Json::int_leaf`] is the rule; [`Json::number`] is
//!   the plain JSON number a `line`, a `column` or a response-path index needs, and the two are
//!   separate methods so that neither can be reached by accident.
//! - **A non-finite `Float` is refused**, not written. Draft §3.5.2 makes a `Float` a *finite*
//!   double and JSON has no spelling for the others; see [`Json::double`] for why this is not
//!   symmetrical with the integer rule.
//! - **No `\u` escape is ever emitted above U+001F**, so no surrogate escape — paired or lone —
//!   can appear in this writer's output at all. [`Json::string`] has the argument.
//!
//! # What it costs
//!
//! Every entry point writes into a caller's [`core::fmt::Write`], strings are escaped as they
//! stream past, and the two number formatters ([`itoa`] and [`zmij`], both `#![no_std]` with zero
//! dependencies) render into stack buffers. Nothing here buffers the response, and no leaf, no key
//! and no message is ever assembled before it is written.
//!
//! **Two `Vec`s, and both are the answer to a cost a remote client chooses.** This module claimed
//! "nothing on the heap" when it was first written, and the claim was kept by spending the two
//! resources a client can drive instead — the native stack, and a walk of the document per error —
//! which is the wrong trade in both cases:
//!
//! - `data` is written by an **explicit work stack**, one frame per open container, so the native
//!   stack does not grow with the response. It used to recurse, which put the depth of an
//!   attacker-shaped response on the native stack at the last stage of a pipeline that had
//!   deliberately kept it off — draft §6.3's collection walk runs on an explicit stack for exactly
//!   this reason, and a budget on what the executor *retains* says nothing about what a serialiser
//!   *recurses through*. The frames cost one allocation per response, sized by its depth, and a
//!   response whose `data` is a leaf or a null allocates nothing.
//! - Draft §7.1.2's `line` and `column` are resolved through a **checkpoint index over the
//!   document**, built once on the first location and never rebuilt. Deriving them by walking the
//!   prefix made a response with *k* errors quadratic in the document, which is a remotely
//!   triggerable cost precisely when a service is already degraded. The index is three words per
//!   256 bytes of document — 24 bytes per 256 on a 64-bit target, and flat, so that no shape a
//!   client can send amplifies it — and a response with no locations never builds it.
//!
//! Both bounds are measured rather than argued: `json::response`'s own tests count the document
//! bytes the writer looks at per location, and the native stack it uses per level of response
//! depth.

use core::fmt;

pub use response::{
  write_request_error_result, write_request_error_result_with, write_response, write_response_with,
};

mod response;

#[cfg(feature = "materialized-numbers")]
#[cfg_attr(docsrs, doc(cfg(feature = "materialized-numbers")))]
pub mod materialized;

#[cfg(test)]
mod tests;

/// What stopped a value from being written.
///
/// Deliberately **not** `#[non_exhaustive]`. The set is closed by what the writer can produce, and
/// each variant below is a branch of a total function rather than a placeholder for a case nobody
/// has met yet; a census over it should be exhaustive and checked, which
/// `#[non_exhaustive]` would quietly stop being possible outside this crate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Error {
  /// The sink refused the write.
  ///
  /// [`core::fmt::Error`] carries nothing, so neither does this: a `fmt::Write` implementation
  /// reports only *that* it failed, and inventing a cause here would be a claim the sink never
  /// made.
  Sink,
  /// A `Float` that is not finite reached [`Json::double`].
  ///
  /// Draft §3.5.2's `Float` is a finite IEEE 754 double and JSON has no number literal for a NaN
  /// or an infinity.
  NonFiniteFloat,
  /// A GraphQL `\u` escape named a UTF-16 surrogate code point.
  ///
  /// Both spellings reach it — a lone `\uD800`, and the braced `\u{D800}` — because neither names
  /// a Unicode scalar value and so neither has a character to write. A *paired* `𐀀` is
  /// not this: it names U+10000 and is written as that one character.
  SurrogateEscape,
  /// A GraphQL string literal carried an escape the lexer would not have produced.
  ///
  /// Unreachable from a literal this workspace lexed — `smear-lexer` validates every escape before
  /// a `StringValue` exists — and present because the cooking walk must be total. A `panic!` here
  /// would be the same walk with the failure mode of the two crates it was written to avoid.
  MalformedEscape,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::Sink => "the output sink refused the write",
      Self::NonFiniteFloat => "a Float that is not finite has no JSON number literal",
      Self::SurrogateEscape => "a \\u escape named a UTF-16 surrogate, which is not a character",
      Self::MalformedEscape => "a string literal carried a malformed escape",
    })
  }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl From<fmt::Error> for Error {
  #[inline]
  fn from(_: fmt::Error) -> Self {
    Self::Sink
  }
}

/// A value that can write itself as one JSON value.
///
/// Implemented here for both materialised value trees (see [`materialized`]); a driver whose
/// values are self-describing implements it for its own type and gets
/// [`write_response`] for free.
///
/// **A handle is not self-describing**, and that is why [`write_response_with`] exists: a driver
/// whose `V` is an index into a table it owns cannot answer from `&self` alone, so it passes a
/// closure that has the table in scope. This is the same division
/// [`Values`](crate::proto::Values) draws by taking `&self` alongside every value it is
/// asked about, arrived at from the writing side.
pub trait WriteJson {
  /// Writes `self` as exactly one JSON value.
  ///
  /// "Exactly one" is the whole contract, and it is what lets a caller place the value anywhere a
  /// JSON value belongs. An implementation that writes two, or none, produces a document no
  /// bracket count can rescue.
  fn write_json<W: fmt::Write>(&self, out: &mut Json<W>) -> Result<(), Error>;
}

/// A JSON sink.
///
/// Structure is written through [`Json::array`] and [`Json::object`], which return scoped writers
/// that place the separators. Nothing here tracks whether a *value* was written where one was
/// promised: the type stops a missing comma, not a missing value, and each entry point in this
/// module is written so the two cannot come apart.
#[derive(Debug)]
pub struct Json<W> {
  out: W,
}

impl<W> Json<W> {
  /// Wraps a sink.
  #[inline]
  pub const fn new(out: W) -> Self {
    Self { out }
  }

  /// Returns the sink, which has whatever was written to it.
  #[inline]
  pub fn into_inner(self) -> W {
    self.out
  }
}

impl<W: fmt::Write> Json<W> {
  /// Writes `null`.
  #[inline]
  pub fn null(&mut self) -> Result<(), Error> {
    self.out.write_str("null")?;
    Ok(())
  }

  /// Writes `true` or `false`.
  #[inline]
  pub fn bool(&mut self, value: bool) -> Result<(), Error> {
    self.out.write_str(if value { "true" } else { "false" })?;
    Ok(())
  }

  /// Writes a JSON number.
  ///
  /// **Not** the door for a GraphQL `Int` leaf — that one is [`int_leaf`](Json::int_leaf), and the
  /// two are separate because the response has integers of both kinds in it. A `line`, a `column`
  /// and a response-path index are `proto`'s own counts and are always numbers; a driver's `Int`
  /// is a value the reader may be a JavaScript client, and draft §3.5.1 bounds it.
  #[inline]
  pub fn number(&mut self, value: i64) -> Result<(), Error> {
    let mut buffer = itoa::Buffer::new();
    self.out.write_str(buffer.format(value))?;
    Ok(())
  }

  /// Writes a GraphQL `Int` leaf, as a number when draft §3.5.1 can hold it and as a JSON string
  /// when it cannot.
  ///
  /// Draft §3.5.1's `Int` is a signed 32-bit integer. A value outside that range is a well-formed
  /// `IntValue` under §2.9.1's grammar and is not an `Int`, so it is written the way GraphQL
  /// services already write one — as a string, which is what every `BigInt`-style custom scalar
  /// does, and for the reason they do it: JSON has one number type and a consumer that reads it as
  /// an IEEE 754 double silently loses the value.
  ///
  /// **This is where the two materialised widths meet the writer.** The `i32` tree
  /// ([`materialized32`](crate::parser::graphql::ast::materialized32)) cannot reach the string
  /// branch at all — its parser refused the literal — and the `i64` tree can, which is the
  /// difference between the permissive reading and the specification's made visible in the
  /// response rather than only in the parse.
  ///
  /// No escaping on the string branch: a decimal integer's spelling is `-` and ASCII digits.
  pub fn int_leaf(&mut self, value: i64) -> Result<(), Error> {
    let mut buffer = itoa::Buffer::new();
    let rendered = buffer.format(value);
    if i64::from(i32::MIN) <= value && value <= i64::from(i32::MAX) {
      self.out.write_str(rendered)?;
    } else {
      self.out.write_char('"')?;
      self.out.write_str(rendered)?;
      self.out.write_char('"')?;
    }
    Ok(())
  }

  /// Writes a GraphQL `Float` leaf.
  ///
  /// # A non-finite double is refused, and that is not the integer rule inverted
  ///
  /// [`int_leaf`](Json::int_leaf) renders an out-of-range integer as a string because a string
  /// *is* how GraphQL already carries an integer JSON cannot hold — `BigInt` scalars are declared
  /// as strings in schemas that exist, and a client reading one knows to parse it. There is no
  /// such convention for `"NaN"`: a client would read a string where the schema promised a
  /// `Float`, and the response would be wrong in a way no reader could detect. Draft §3.5.2 says a
  /// `Float` is finite, so the honest answer is that the value cannot be serialised, which is
  /// [`Error::NonFiniteFloat`].
  ///
  /// It is also not reachable from a parsed document: a literal naming no finite double is
  /// `FloatOverflow` at the point the materialising parser reads it.
  pub fn double(&mut self, value: f64) -> Result<(), Error> {
    if !value.is_finite() {
      return Err(Error::NonFiniteFloat);
    }
    let mut buffer = zmij::Buffer::new();
    self.out.write_str(buffer.format_finite(value))?;
    Ok(())
  }

  /// Writes a Rust string as a quoted, escaped JSON string.
  ///
  /// # What is escaped, and the one class that is unreachable
  ///
  /// RFC 8259 §7 requires exactly three things to be escaped — `"`, `\`, and the control
  /// characters below U+0020 — and permits, without requiring, a `\uXXXX` escape for anything
  /// else. This writer takes the minimum: the two-character escapes for `"` `\` `\b` `\f` `\n`
  /// `\r` `\t`, `\u00XX` for the remaining control characters, and every other character as its
  /// own UTF-8 bytes.
  ///
  /// **So no `\u` escape is ever emitted above U+001F, and therefore no surrogate escape is ever
  /// emitted at all.** That is the deliberate answer to the question a JSON writer has to answer
  /// somewhere: a lone surrogate cannot appear in this output, not because a check rejects one but
  /// because the only construct that could spell one is never used. The alternative — escaping
  /// non-ASCII as `\uXXXX` — puts the writer one arithmetic slip away from emitting `\uD83D`
  /// without its pair, which is the defect filed against tokora's own JSON example as
  /// `al8n/tokora#272`, seen from the writing side. A `&str` cannot carry a surrogate either, so
  /// the input side of the same question is closed by the type.
  ///
  /// `/` is **not** escaped. RFC 8259 permits `\/` and nothing requires it; the convention exists
  /// to keep `</script>` out of an HTML document, which is an obligation of whatever embeds the
  /// response, not of the response. U+2028 and U+2029 are likewise written as themselves: they are
  /// legal JSON string content, and it is a JavaScript parser predating ES2019 that could not
  /// read them back.
  pub fn string(&mut self, value: &str) -> Result<(), Error> {
    self.out.write_char('"')?;
    escape(&mut self.out, value)?;
    self.out.write_char('"')?;
    Ok(())
  }

  /// Writes a [`core::fmt::Display`] rendering as a quoted, escaped JSON string.
  ///
  /// Escaped as it streams: the rendering is never assembled in a buffer, so a response with four
  /// hundred field errors allocates four hundred times nothing here too, which is the property
  /// `graphql-proto`'s error table exists to keep.
  pub fn display(&mut self, value: &dyn fmt::Display) -> Result<(), Error> {
    use fmt::Write as _;

    self.out.write_char('"')?;
    write!(Escaping { out: &mut self.out }, "{value}")?;
    self.out.write_char('"')?;
    Ok(())
  }

  /// Writes the *value* of a GraphQL string literal — source spelling in, JSON string out.
  ///
  /// `literal` is the literal as the document spells it, delimiters included: either
  /// `"…"` or `"""…"""`. The two are told apart by the delimiter, which is unambiguous — `""` is
  /// the empty inline string and does not begin with three quotes, and the shortest block string
  /// is `""""""`.
  ///
  /// # Why the writer cooks the literal itself
  ///
  /// The materialised value tree keeps a string leaf as its **source slice, escapes and all** —
  /// that is what makes materialisation allocate nothing — so something has to apply draft §2.9.1's
  /// escapes and draft §2.9.4's block-string algorithm before a JSON string can be written.
  ///
  /// `smear-lexer` has a second implementation of that, as `Cow<str>` conversions on its literal
  /// types, and this writer deliberately does **not** route through them: measured on this tree,
  /// `Cow::from(LitInlineStr::from(…))` **panics** on `\u{1F600}` — a braced escape its own lexer
  /// accepts and its `normalize_str_to_string` has no arm for. Inheriting that would put a panic
  /// on the response path for a legal document. The conversions are used as a differential oracle
  /// in this module's tests instead, where a second implementation is worth having and a panic is
  /// a finding rather than an outage.
  ///
  /// # Escapes
  ///
  /// Inline: `\"` `\\` `\/` `\b` `\f` `\n` `\r` `\t`, the fixed-width `\uXXXX` including a
  /// surrogate **pair**, and the braced `\u{X…}`. Block: `\"""`, and nothing else — a backslash in
  /// a block string is a backslash.
  pub fn graphql_string(&mut self, literal: &str) -> Result<(), Error> {
    self.out.write_char('"')?;
    if let Some(raw) = block_body(literal) {
      cook_block(&mut self.out, raw)?;
    } else {
      cook_inline(&mut self.out, inline_body(literal))?;
    }
    self.out.write_char('"')?;
    Ok(())
  }

  /// Writes one of JSON's structural characters.
  ///
  /// **Not public, and not a general escape hatch.** [`Array`] and [`Object`] are how structure is
  /// written, and the whole point of them is that the separators are not the caller's to remember.
  /// There is exactly one walk that cannot use them — `response::write_node`, which
  /// runs on an explicit stack so the native one does not grow with the response, and a scoped
  /// writer cannot be *kept* on that stack because each one borrows the [`Json`] the enclosing one
  /// already borrows. That walk places its own separators, and this is the door it does it
  /// through; being private is what keeps the exception to the one caller that argued for it.
  #[inline]
  fn punct(&mut self, ch: char) -> Result<(), Error> {
    self.out.write_char(ch)?;
    Ok(())
  }

  /// Opens an array, which the caller closes with [`Array::end`].
  #[inline]
  #[must_use = "an array that is never `end`ed is never closed"]
  pub fn array(&mut self) -> Result<Array<'_, W>, Error> {
    self.out.write_char('[')?;
    Ok(Array {
      json: self,
      empty: true,
    })
  }

  /// Opens an object, which the caller closes with [`Object::end`].
  #[inline]
  #[must_use = "an object that is never `end`ed is never closed"]
  pub fn object(&mut self) -> Result<Object<'_, W>, Error> {
    self.out.write_char('{')?;
    Ok(Object {
      json: self,
      empty: true,
    })
  }
}

/// An array being written. Returned by [`Json::array`].
#[derive(Debug)]
pub struct Array<'a, W> {
  json: &'a mut Json<W>,
  empty: bool,
}

impl<W: fmt::Write> Array<'_, W> {
  /// Begins one element, returning the sink its value is written to.
  pub fn element(&mut self) -> Result<&mut Json<W>, Error> {
    if !self.empty {
      self.json.out.write_char(',')?;
    }
    self.empty = false;
    Ok(self.json)
  }

  /// Closes the array.
  #[inline]
  pub fn end(self) -> Result<(), Error> {
    self.json.out.write_char(']')?;
    Ok(())
  }
}

/// An object being written. Returned by [`Json::object`].
#[derive(Debug)]
pub struct Object<'a, W> {
  json: &'a mut Json<W>,
  empty: bool,
}

impl<W: fmt::Write> Object<'_, W> {
  /// Begins one member, returning the sink its value is written to.
  ///
  /// The key is escaped exactly as [`Json::string`] escapes any other string. Duplicate keys are
  /// not this type's to refuse: the response's keys are draft §6.3's response keys, which
  /// collection already made unique, and a driver's `extensions` map holds unique keys by
  /// construction.
  pub fn key(&mut self, key: &str) -> Result<&mut Json<W>, Error> {
    if !self.empty {
      self.json.out.write_char(',')?;
    }
    self.empty = false;
    self.json.string(key)?;
    self.json.out.write_char(':')?;
    Ok(self.json)
  }

  /// Closes the object.
  #[inline]
  pub fn end(self) -> Result<(), Error> {
    self.json.out.write_char('}')?;
    Ok(())
  }
}

/// A sink that escapes everything written through it, for [`Json::display`].
struct Escaping<'a, W> {
  out: &'a mut W,
}

impl<W: fmt::Write> fmt::Write for Escaping<'_, W> {
  #[inline]
  fn write_str(&mut self, s: &str) -> fmt::Result {
    escape(self.out, s)
  }
}

/// Writes `value` with JSON's escapes applied and no surrounding quotes.
///
/// Byte-wise rather than character-wise, and that is sound rather than a shortcut: every byte this
/// escapes is ASCII, and no byte of a multi-byte UTF-8 sequence is, so a split at one of these
/// positions is always at a character boundary.
fn escape<W: fmt::Write>(out: &mut W, value: &str) -> fmt::Result {
  let mut flushed = 0;
  for (at, byte) in value.bytes().enumerate() {
    let two = match byte {
      b'"' => '"',
      b'\\' => '\\',
      0x08 => 'b',
      0x0C => 'f',
      b'\n' => 'n',
      b'\r' => 'r',
      b'\t' => 't',
      0x00..=0x1F => {
        out.write_str(&value[flushed..at])?;
        write_control(out, byte)?;
        flushed = at + 1;
        continue;
      }
      _ => continue,
    };
    out.write_str(&value[flushed..at])?;
    out.write_char('\\')?;
    out.write_char(two)?;
    flushed = at + 1;
  }
  out.write_str(&value[flushed..])
}

/// Writes one already-cooked character with JSON's escapes applied.
fn escape_char<W: fmt::Write>(out: &mut W, ch: char) -> fmt::Result {
  let two = match ch {
    '"' => '"',
    '\\' => '\\',
    '\u{8}' => 'b',
    '\u{c}' => 'f',
    '\n' => 'n',
    '\r' => 'r',
    '\t' => 't',
    '\0'..='\u{1f}' => return write_control(out, ch as u8),
    other => return out.write_char(other),
  };
  out.write_char('\\')?;
  out.write_char(two)
}

/// Writes a control character below U+0020 as `\u00XX`.
fn write_control<W: fmt::Write>(out: &mut W, byte: u8) -> fmt::Result {
  const HEX: [u8; 16] = *b"0123456789abcdef";
  out.write_str("\\u00")?;
  out.write_char(HEX[usize::from(byte >> 4)] as char)?;
  out.write_char(HEX[usize::from(byte & 0x0f)] as char)
}

/// Returns a block string's raw body, or `None` when the literal is an inline string.
fn block_body(literal: &str) -> Option<&str> {
  let bytes = literal.as_bytes();
  if bytes.len() >= 6 && bytes.starts_with(b"\"\"\"") && bytes.ends_with(b"\"\"\"") {
    Some(&literal[3..literal.len() - 3])
  } else {
    None
  }
}

/// Returns an inline string's raw body.
///
/// Total on a malformed literal — a caller that hands over something that is not a literal at all
/// gets its own bytes back rather than a panic — because the delimiters are the lexer's guarantee
/// and this module does not re-check the lexer.
fn inline_body(literal: &str) -> &str {
  literal
    .strip_prefix('"')
    .and_then(|rest| rest.strip_suffix('"'))
    .unwrap_or(literal)
}

/// Applies draft §2.9.1's escapes and writes the result JSON-escaped.
fn cook_inline<W: fmt::Write>(out: &mut W, raw: &str) -> Result<(), Error> {
  let mut rest = raw;
  while let Some(at) = rest.find('\\') {
    escape(out, &rest[..at])?;
    let after = &rest[at + 1..];
    let (ch, consumed) = read_escape(after)?;
    escape_char(out, ch)?;
    rest = &after[consumed..];
  }
  escape(out, rest)?;
  Ok(())
}

/// Reads one escape body — everything after the backslash — returning its character and how many
/// bytes it spans.
fn read_escape(after: &str) -> Result<(char, usize), Error> {
  let mut chars = after.chars();
  let ch = chars.next().ok_or(Error::MalformedEscape)?;
  let simple = match ch {
    '"' => '"',
    '\\' => '\\',
    '/' => '/',
    'b' => '\u{8}',
    'f' => '\u{c}',
    'n' => '\n',
    'r' => '\r',
    't' => '\t',
    'u' => return read_unicode_escape(&after[1..]).map(|(ch, span)| (ch, span + 1)),
    _ => return Err(Error::MalformedEscape),
  };
  Ok((simple, ch.len_utf8()))
}

/// Reads a `\u` escape body — everything after the `u` — in either spelling.
fn read_unicode_escape(after: &str) -> Result<(char, usize), Error> {
  if let Some(rest) = after.strip_prefix('{') {
    let close = rest.find('}').ok_or(Error::MalformedEscape)?;
    let scalar = hex(&rest[..close])?;
    return char::from_u32(scalar)
      .map(|ch| (ch, close + 2))
      .ok_or(surrogate_or_malformed(scalar));
  }

  let leading = hex4(after)?;
  // A leading surrogate is only a character with its trailing half, which draft §2.9.1 spells as a
  // second `\u` escape immediately after this one. Anything else is not a character at all.
  if (0xD800..0xDC00).contains(&leading) {
    let tail = after.get(4..).ok_or(Error::SurrogateEscape)?;
    let tail = tail.strip_prefix("\\u").ok_or(Error::SurrogateEscape)?;
    let trailing = hex4(tail)?;
    if !(0xDC00..0xE000).contains(&trailing) {
      return Err(Error::SurrogateEscape);
    }
    let combined = 0x1_0000 + ((leading - 0xD800) << 10) + (trailing - 0xDC00);
    return char::from_u32(combined)
      .map(|ch| (ch, 10))
      .ok_or(Error::MalformedEscape);
  }

  char::from_u32(leading)
    .map(|ch| (ch, 4))
    .ok_or(surrogate_or_malformed(leading))
}

/// Says which refusal a non-scalar code point earned.
const fn surrogate_or_malformed(scalar: u32) -> Error {
  if 0xD800 <= scalar && scalar < 0xE000 {
    Error::SurrogateEscape
  } else {
    Error::MalformedEscape
  }
}

/// Reads exactly four hex digits.
fn hex4(digits: &str) -> Result<u32, Error> {
  let digits = digits.get(..4).ok_or(Error::MalformedEscape)?;
  hex(digits)
}

/// Reads one to six hex digits as a code point.
fn hex(digits: &str) -> Result<u32, Error> {
  if digits.is_empty() || digits.len() > 6 {
    return Err(Error::MalformedEscape);
  }
  let mut scalar = 0u32;
  for digit in digits.chars() {
    scalar = (scalar << 4) | digit.to_digit(16).ok_or(Error::MalformedEscape)?;
  }
  Ok(scalar)
}

/// Applies draft §2.9.4's `BlockStringValue` and writes the result JSON-escaped.
///
/// Two passes over the body and no buffer: the first measures the common indentation and which
/// lines survive, the second writes. The specification replaces `\"""` with `"""` *before* the
/// dedent, and doing it during the write is equivalent — a backslash is not whitespace, so the
/// replacement cannot change any line's indentation or whether it is blank.
fn cook_block<W: fmt::Write>(out: &mut W, raw: &str) -> Result<(), Error> {
  let mut indent = usize::MAX;
  let mut keep_from = 0usize;
  let mut keep_to = 0usize;
  for (seen, line) in Lines::new(raw).enumerate() {
    if blank(line) {
      continue;
    }
    // Step 3.a: the common indent is measured over every line but the first of the RAW split,
    // which is not the same as the first line that survives steps 5 and 6.
    if seen > 0 {
      indent = indent.min(leading_whitespace(line));
    }
    if keep_to == 0 {
      keep_from = seen;
    }
    keep_to = seen + 1;
  }
  let indent = if indent == usize::MAX { 0 } else { indent };

  let mut written = 0usize;
  for (at, line) in Lines::new(raw).enumerate() {
    if at < keep_from || at >= keep_to {
      continue;
    }
    if written > 0 {
      escape_char(out, '\n')?;
    }
    let body = if at == 0 { line } else { chop(line, indent) };
    write_block_line(out, body)?;
    written += 1;
  }
  Ok(())
}

/// Writes one block-string line, turning `\"""` into `"""` on the way past.
fn write_block_line<W: fmt::Write>(out: &mut W, line: &str) -> Result<(), Error> {
  let mut rest = line;
  while let Some(at) = rest.find("\\\"\"\"") {
    escape(out, &rest[..at])?;
    escape(out, "\"\"\"")?;
    rest = &rest[at + 4..];
  }
  escape(out, rest)?;
  Ok(())
}

/// Splits on draft §2.1.1's line terminators: a line feed, a carriage return, or the pair.
struct Lines<'a> {
  rest: Option<&'a str>,
}

impl<'a> Lines<'a> {
  #[inline]
  const fn new(text: &'a str) -> Self {
    Self { rest: Some(text) }
  }
}

impl<'a> Iterator for Lines<'a> {
  type Item = &'a str;

  fn next(&mut self) -> Option<&'a str> {
    let rest = self.rest?;
    match rest.find(['\n', '\r']) {
      None => {
        self.rest = None;
        Some(rest)
      }
      Some(at) => {
        let width = if rest.as_bytes()[at] == b'\r' && rest.as_bytes().get(at + 1) == Some(&b'\n') {
          2
        } else {
          1
        };
        self.rest = Some(&rest[at + width..]);
        Some(&rest[..at])
      }
    }
  }
}

/// Returns whether a line is draft §2.9.4's "only whitespace", which is spaces and tabs.
fn blank(line: &str) -> bool {
  line.bytes().all(|byte| byte == b' ' || byte == b'\t')
}

/// Counts a line's leading spaces and tabs.
fn leading_whitespace(line: &str) -> usize {
  line
    .bytes()
    .take_while(|&byte| byte == b' ' || byte == b'\t')
    .count()
}

/// Removes up to `indent` leading whitespace characters.
fn chop(line: &str, indent: usize) -> &str {
  let taken = line
    .bytes()
    .take(indent)
    .take_while(|&byte| byte == b' ' || byte == b'\t')
    .count();
  &line[taken..]
}
