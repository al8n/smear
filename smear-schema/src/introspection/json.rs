//! The JSON a draft §4 response is written in, read as a **scan** rather than as a document.
//!
//! # There is no `Value` here, and that is the whole point
//!
//! A general JSON reader answers "what is this?" for every node it meets, and to answer it has to
//! materialise the node: a `String` per string, a map per object, a vector per array. The door
//! never asks that question — at every point it already knows which member of which draft §4 type
//! it expects — so this file provides the *tokens* of JSON and nothing above them, and
//! [`decode`](super::decode) supplies the shape.
//!
//! # Two string paths, and the difference is a promise about allocation
//!
//! [`Scanner::string`] hands back the literal's own bytes **borrowed from the response**, together
//! with whether a backslash occurred inside it. Everything the door reads except one field is a
//! GraphQL `Name`, a `__TypeKind` or a `__DirectiveLocation` — closed vocabularies of ASCII
//! identifiers that never need an escape — so those are matched against the borrowed bytes and
//! never copied. The one exception is `__InputValue.defaultValue`, which is a *value* a server
//! printed and may legitimately contain a `"` or a `\`; [`Text`] is what carries it, and it
//! allocates **only when a backslash is actually present**.
//!
//! # Two failures, kept apart
//!
//! [`ResponseErrorKind::MalformedJson`] is *these bytes are not JSON*; [`MalformedResponse`] is
//! *they are JSON and not the shape draft §4 describes*. Every primitive here draws that line the
//! same way: a byte that begins no JSON value at all is a syntax failure, and a well-formed value
//! of the wrong type is a shape failure. That is the distinction
//! [`serde_json::error::Category`](https://docs.rs/serde_json/latest/serde_json/error/enum.Category.html)
//! used to draw for this door, kept because the two have different audiences.
//!
//! [`MalformedResponse`]: ResponseErrorKind::MalformedResponse

use std::{format, string::String};

use super::error::{ResponseError, ResponseErrorKind};

/// The result of every primitive in this file.
pub(super) type Scanned<T = ()> = Result<T, ResponseError>;

/// How deeply containers may nest before the reader refuses.
///
/// This is `serde_json`'s own limit, kept rather than chosen: the previous reader enforced it
/// before a `TypeRef` was ever constructed, and both [`decode`](super::decode) and the renderer
/// walk a `ofType` chain recursively, so the bound is what keeps a hostile response from driving
/// either off the stack. The representation's own limit on wrapper depth is much lower —
/// `MAX_WRAPPERS` is 15 — and is left to the build, which reports `TypeReferenceTooDeep` for the
/// SDL that says the same thing.
const MAX_DEPTH: u32 = 128;

/// A response string, borrowed when it can be and expanded when it must be.
///
/// # The name is the contract
///
/// `Borrowed` is the response's own bytes. `Unescaped` is a copy, and it exists for exactly one
/// field: `__InputValue.defaultValue` is a literal a server *printed*, so `"\"world\""` is a
/// perfectly ordinary thing for it to contain. Nothing else the door reads can hold an escape
/// without being refused for a different reason — a `Name`, a `__TypeKind` and a
/// `__DirectiveLocation` are all matched against the literal's own bytes — so this type appears
/// once in the model and the borrow appears everywhere else.
///
/// Calling the decoder "zero-copy" without saying that would be a name that misdescribes its
/// contents.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Text<'a> {
  /// The literal held no backslash, so its decoded form *is* the response's bytes.
  Borrowed(&'a str),
  /// The literal held a backslash, so the escapes were expanded into one new allocation.
  Unescaped(String),
}

impl Text<'_> {
  /// Returns the decoded text.
  #[inline]
  pub(super) fn as_str(&self) -> &str {
    match self {
      Self::Borrowed(text) => text,
      Self::Unescaped(text) => text,
    }
  }

  /// Returns whether the text is the response's own bytes rather than a copy of them.
  ///
  /// Read only by the allocation gate — a corpus whose default values carry no escape must answer
  /// `true` for every one of them — because nothing in the door's own work depends on which side
  /// of the enum it is holding. That is the property, stated as a method rather than as a
  /// sentence.
  #[cfg(test)]
  #[inline]
  pub(super) fn is_borrowed(&self) -> bool {
    matches!(self, Self::Borrowed(_))
  }
}

/// Whether a `,` is still owed before the next member of the container being read.
///
/// Handed out by [`Scanner::enter_object`] and [`Scanner::enter_array`] rather than kept on the
/// scanner, because containers nest and a flag on the scanner would belong to whichever one
/// touched it last.
#[derive(Debug)]
pub(super) struct Seq {
  first: bool,
}

/// A cursor over a response, and the JSON grammar it can recognise at that cursor.
#[derive(Debug)]
pub(super) struct Scanner<'a> {
  response: &'a str,
  pos: usize,
  depth: u32,
}

impl<'a> Scanner<'a> {
  /// Opens a scan at the start of a response.
  #[inline]
  pub(super) const fn new(response: &'a str) -> Self {
    Self {
      response,
      pos: 0,
      depth: 0,
    }
  }

  /// Opens a scan at a byte offset a previous pass recorded.
  ///
  /// The offset always names the first byte of a value the earlier pass already scanned whole, so
  /// this cannot start in the middle of a literal.
  #[inline]
  pub(super) const fn at(response: &'a str, pos: usize) -> Self {
    Self {
      response,
      pos,
      depth: 0,
    }
  }

  /// Returns the whole response, for the error path that reads an owner's name back out of it.
  #[inline]
  pub(super) const fn response(&self) -> &'a str {
    self.response
  }

  /// Returns the offset of the next byte, which is the start of a value after [`Self::skip_ws`].
  #[inline]
  pub(super) const fn position(&self) -> usize {
    self.pos
  }

  #[inline]
  fn peek(&self) -> Option<u8> {
    self.response.as_bytes().get(self.pos).copied()
  }

  /// Positions the cursor on the next value and reports whether it is an object.
  ///
  /// The envelope asks this and nothing else: which of the three shapes a response is depends on
  /// which keys its root object has, and a root that is not an object has none of them.
  #[inline]
  pub(super) fn at_object(&mut self) -> bool {
    self.skip_ws();
    self.peek() == Some(b'{')
  }

  /// Advances past whitespace. JSON's four characters, and no others.
  pub(super) fn skip_ws(&mut self) {
    let bytes = self.response.as_bytes();
    while let Some(&byte) = bytes.get(self.pos) {
      match byte {
        b' ' | b'\t' | b'\n' | b'\r' => self.pos += 1,
        _ => break,
      }
    }
  }

  // -------------------------------------------------------------------------------------------
  // refusals
  // -------------------------------------------------------------------------------------------

  /// *These bytes are not JSON.*
  pub(super) fn syntax(&self, what: &str) -> ResponseError {
    self.refuse(ResponseErrorKind::MalformedJson, what)
  }

  /// *They are JSON, and not the shape draft §4 describes.*
  fn shape(&self, what: &str) -> ResponseError {
    self.refuse(ResponseErrorKind::MalformedResponse, what)
  }

  /// *A member draft §4 declares non-null is not there.*
  ///
  /// Reported at the closing brace of the object that should have carried it, which is where the
  /// reader learned it was absent.
  pub(super) fn missing(&self, field: &str) -> ResponseError {
    self.shape(&format!("missing field `{field}`"))
  }

  /// *A well-formed JSON value, of a type the shape does not admit here.*
  fn invalid_type(&self, expected: &str, what: &str) -> ResponseError {
    self.shape(&format!(
      "invalid type: {}, expected {expected} for `{what}`",
      self.found()
    ))
  }

  /// Names the JSON type at the cursor, in `serde`'s vocabulary.
  fn found(&self) -> &'static str {
    match self.peek() {
      Some(b'{') => "a map",
      Some(b'[') => "a sequence",
      Some(b'"') => "a string",
      Some(b't') | Some(b'f') => "a boolean",
      Some(b'n') => "null",
      Some(b'-') | Some(b'0'..=b'9') => "a number",
      _ => "no value",
    }
  }

  /// Attaches the position every refusal carries.
  ///
  /// A line and column rather than a byte offset: the response is machine-generated, but a human
  /// is the one reading the message, and every JSON tool they have counts the same way.
  fn refuse(&self, kind: ResponseErrorKind, what: &str) -> ResponseError {
    let (line, column) = self.line_column();
    ResponseError::new(kind, format!("{what} at line {line} column {column}"))
  }

  fn line_column(&self) -> (usize, usize) {
    let upto = &self.response.as_bytes()[..self.pos.min(self.response.len())];
    let line = 1 + upto.iter().filter(|&&byte| byte == b'\n').count();
    let column = match upto.iter().rposition(|&byte| byte == b'\n') {
      Some(newline) => upto.len() - newline,
      None => upto.len() + 1,
    };
    (line, column)
  }

  // -------------------------------------------------------------------------------------------
  // containers
  // -------------------------------------------------------------------------------------------

  /// Enters an object, or refuses whatever is there instead.
  pub(super) fn enter_object(&mut self, what: &str) -> Scanned<Seq> {
    self.skip_ws();
    match self.peek() {
      Some(b'{') => {}
      Some(_) => return Err(self.invalid_type("an object", what)),
      None => return Err(self.syntax("unexpected end of input")),
    }
    self.descend()?;
    self.pos += 1;
    Ok(Seq { first: true })
  }

  /// Reads the next member's key, or closes the object and returns `None`.
  ///
  /// The key comes back as the literal's own bytes. A key spelled with a JSON escape therefore
  /// matches no member the door knows and is skipped as an unknown one — which is what a response
  /// carrying `"name"` deserves, since draft §4 has no such member and every real key is an
  /// ASCII identifier.
  pub(super) fn next_key(&mut self, seq: &mut Seq) -> Scanned<Option<&'a str>> {
    self.skip_ws();
    match self.peek() {
      Some(b'}') => {
        self.pos += 1;
        self.depth -= 1;
        return Ok(None);
      }
      Some(b',') if !seq.first => {
        self.pos += 1;
        self.skip_ws();
      }
      Some(_) if seq.first => {}
      Some(_) => return Err(self.syntax("expected `,` or `}`")),
      None => return Err(self.syntax("unexpected end of input")),
    }
    seq.first = false;
    if self.peek() != Some(b'"') {
      return Err(self.syntax("key must be a string"));
    }
    let (key, _) = self.string("an object key")?;
    self.skip_ws();
    if self.peek() != Some(b':') {
      return Err(self.syntax("expected `:`"));
    }
    self.pos += 1;
    Ok(Some(key))
  }

  /// Enters an array, or refuses whatever is there instead.
  pub(super) fn enter_array(&mut self, what: &str) -> Scanned<Seq> {
    self.skip_ws();
    match self.peek() {
      Some(b'[') => {}
      Some(_) => return Err(self.invalid_type("a sequence", what)),
      None => return Err(self.syntax("unexpected end of input")),
    }
    self.descend()?;
    self.pos += 1;
    Ok(Seq { first: true })
  }

  /// Positions the cursor on the next element, or closes the array and returns `false`.
  pub(super) fn next_element(&mut self, seq: &mut Seq) -> Scanned<bool> {
    self.skip_ws();
    match self.peek() {
      Some(b']') => {
        self.pos += 1;
        self.depth -= 1;
        return Ok(false);
      }
      Some(b',') if !seq.first => {
        self.pos += 1;
      }
      Some(_) if seq.first => {}
      Some(_) => return Err(self.syntax("expected `,` or `]`")),
      None => return Err(self.syntax("unexpected end of input")),
    }
    seq.first = false;
    Ok(true)
  }

  fn descend(&mut self) -> Scanned {
    if self.depth == MAX_DEPTH {
      return Err(self.syntax("recursion limit exceeded"));
    }
    self.depth += 1;
    Ok(())
  }

  // -------------------------------------------------------------------------------------------
  // scalars
  // -------------------------------------------------------------------------------------------

  /// Reads a string literal and returns its **raw** contents, with whether it holds a backslash.
  ///
  /// The whole literal is validated here — control characters, escape spellings and surrogate
  /// pairing — so a caller that only wants the bytes gets the same refusals as one that wants the
  /// text, and [`Self::text`] can expand a literal this already proved well-formed.
  pub(super) fn string(&mut self, what: &str) -> Scanned<(&'a str, bool)> {
    self.skip_ws();
    match self.peek() {
      Some(b'"') => {}
      Some(_) => return Err(self.invalid_type("a string", what)),
      None => return Err(self.syntax("unexpected end of input")),
    }
    self.pos += 1;
    let start = self.pos;
    let mut escaped = false;
    loop {
      let Some(byte) = self.peek() else {
        return Err(self.syntax("unexpected end of input"));
      };
      match byte {
        b'"' => {
          let raw = &self.response[start..self.pos];
          self.pos += 1;
          return Ok((raw, escaped));
        }
        b'\\' => {
          escaped = true;
          self.pos += 1;
          self.escape()?;
        }
        0x00..=0x1F => return Err(self.syntax("control character in a string")),
        _ => self.pos += 1,
      }
    }
  }

  /// Reads a string literal as text, expanding its escapes only if it has any.
  pub(super) fn text(&mut self, what: &str) -> Scanned<Text<'a>> {
    let at = self.pos;
    let (raw, escaped) = self.string(what)?;
    if !escaped {
      return Ok(Text::Borrowed(raw));
    }
    match unescape(raw) {
      Some(text) => Ok(Text::Unescaped(text)),
      // Unreachable by construction: `string` validated every escape in `raw` before returning
      // it. An error rather than a panic because "unreachable" is a claim, and a claim that costs
      // a `Result` arm is cheaper than one that costs a panic in a server.
      None => Err(Scanner::at(self.response, at).syntax("unreadable escape in a string")),
    }
  }

  /// Validates one escape sequence, the cursor sitting on the character after the backslash.
  fn escape(&mut self) -> Scanned {
    let Some(byte) = self.peek() else {
      return Err(self.syntax("unexpected end of input"));
    };
    self.pos += 1;
    match byte {
      b'"' | b'\\' | b'/' | b'b' | b'f' | b'n' | b'r' | b't' => Ok(()),
      b'u' => {
        let leading = self.hex4()?;
        if (0xDC00..0xE000).contains(&leading) {
          return Err(self.syntax("lone trailing surrogate in a `\\u` escape"));
        }
        if !(0xD800..0xDC00).contains(&leading) {
          return Ok(());
        }
        if self.peek() != Some(b'\\') || self.response.as_bytes().get(self.pos + 1) != Some(&b'u') {
          return Err(self.syntax("lone leading surrogate in a `\\u` escape"));
        }
        self.pos += 2;
        let trailing = self.hex4()?;
        if !(0xDC00..0xE000).contains(&trailing) {
          return Err(self.syntax("lone leading surrogate in a `\\u` escape"));
        }
        Ok(())
      }
      _ => Err(self.syntax("unknown escape")),
    }
  }

  fn hex4(&mut self) -> Scanned<u32> {
    let bytes = self.response.as_bytes();
    let mut value = 0u32;
    for _ in 0..4 {
      let Some(&digit) = bytes.get(self.pos) else {
        return Err(self.syntax("unexpected end of input"));
      };
      let nibble = match digit {
        b'0'..=b'9' => u32::from(digit - b'0'),
        b'a'..=b'f' => u32::from(digit - b'a') + 10,
        b'A'..=b'F' => u32::from(digit - b'A') + 10,
        _ => return Err(self.syntax("invalid `\\u` escape")),
      };
      value = value * 16 + nibble;
      self.pos += 1;
    }
    Ok(value)
  }

  /// Reads `true` or `false`.
  pub(super) fn boolean(&mut self, what: &str) -> Scanned<bool> {
    self.skip_ws();
    match self.peek() {
      Some(b't') => {
        self.keyword("true")?;
        Ok(true)
      }
      Some(b'f') => {
        self.keyword("false")?;
        Ok(false)
      }
      Some(_) => Err(self.invalid_type("a boolean", what)),
      None => Err(self.syntax("unexpected end of input")),
    }
  }

  /// Consumes a `null` if one is there, and reports whether it was.
  ///
  /// Draft §4 makes most members nullable and servers return `null` for the ones that do not
  /// apply to a kind, so every nullable slot asks this before it asks anything else.
  pub(super) fn null(&mut self) -> Scanned<bool> {
    self.skip_ws();
    if self.peek() != Some(b'n') {
      return Ok(false);
    }
    self.keyword("null")?;
    Ok(true)
  }

  fn keyword(&mut self, word: &str) -> Scanned {
    if self.response.as_bytes()[self.pos..].starts_with(word.as_bytes()) {
      self.pos += word.len();
      return Ok(());
    }
    Err(self.syntax("expected a JSON value"))
  }

  // -------------------------------------------------------------------------------------------
  // everything the door does not read
  // -------------------------------------------------------------------------------------------

  /// Walks one value without interpreting it, and refuses it if it is not JSON.
  ///
  /// Unknown members are ignored on purpose — a response carries whatever the *query* asked for,
  /// servers add fields ahead of the specification, and gateways add their own — but "ignored" is
  /// not "unread": the whole document is still proved to be JSON, exactly as a reader that built
  /// a tree of it would have proved.
  pub(super) fn skip_value(&mut self) -> Scanned {
    self.skip_ws();
    match self.peek() {
      Some(b'{') => {
        let mut members = self.enter_object("a value")?;
        while self.next_key(&mut members)?.is_some() {
          self.skip_value()?;
        }
        Ok(())
      }
      Some(b'[') => {
        let mut elements = self.enter_array("a value")?;
        while self.next_element(&mut elements)? {
          self.skip_value()?;
        }
        Ok(())
      }
      Some(b'"') => self.string("a value").map(|_| ()),
      Some(b't') => self.keyword("true"),
      Some(b'f') => self.keyword("false"),
      Some(b'n') => self.keyword("null"),
      Some(b'-') | Some(b'0'..=b'9') => self.number(),
      Some(_) => Err(self.syntax("expected a JSON value")),
      None => Err(self.syntax("unexpected end of input")),
    }
  }

  /// Walks a number without converting it.
  ///
  /// **No number in a draft §4 response is read.** `isRepeatable` and `isOneOf` are booleans and
  /// every other numeric member — a `line`, a gateway's own counter — belongs to a field the door
  /// ignores, so the reader validates the grammar and never needs a parse, a rounding rule or a
  /// range.
  fn number(&mut self) -> Scanned {
    if self.peek() == Some(b'-') {
      self.pos += 1;
    }
    match self.peek() {
      Some(b'0') => self.pos += 1,
      Some(b'1'..=b'9') => self.digits(),
      _ => return Err(self.syntax("expected a digit")),
    }
    if self.peek() == Some(b'.') {
      self.pos += 1;
      if !matches!(self.peek(), Some(b'0'..=b'9')) {
        return Err(self.syntax("expected a digit"));
      }
      self.digits();
    }
    if matches!(self.peek(), Some(b'e') | Some(b'E')) {
      self.pos += 1;
      if matches!(self.peek(), Some(b'+') | Some(b'-')) {
        self.pos += 1;
      }
      if !matches!(self.peek(), Some(b'0'..=b'9')) {
        return Err(self.syntax("expected a digit"));
      }
      self.digits();
    }
    Ok(())
  }

  fn digits(&mut self) {
    while matches!(self.peek(), Some(b'0'..=b'9')) {
      self.pos += 1;
    }
  }

  /// Refuses anything after the document's one value.
  pub(super) fn finish(&mut self) -> Scanned {
    self.skip_ws();
    match self.peek() {
      None => Ok(()),
      Some(_) => Err(self.syntax("trailing characters")),
    }
  }
}

/// Expands a literal [`Scanner::string`] has already validated.
///
/// One allocation, sized once: an escape never expands — `\n` is two bytes in and one out, a
/// `\uXXXX` is six in and at most three out, a surrogate pair twelve in and four out — so the
/// literal's own length is an exact upper bound and the `String` never grows.
///
/// `None` is the validator and this walk disagreeing, which cannot happen; the caller reports it
/// rather than panicking.
fn unescape(raw: &str) -> Option<String> {
  let bytes = raw.as_bytes();
  let mut out = String::with_capacity(raw.len());
  let mut copied = 0;
  let mut at = 0;
  while at < bytes.len() {
    if bytes[at] != b'\\' {
      at += 1;
      continue;
    }
    out.push_str(raw.get(copied..at)?);
    at += 1;
    let expanded = match *bytes.get(at)? {
      b'"' => '"',
      b'\\' => '\\',
      b'/' => '/',
      b'b' => '\u{8}',
      b'f' => '\u{c}',
      b'n' => '\n',
      b'r' => '\r',
      b't' => '\t',
      b'u' => {
        at += 1;
        let leading = hex4(bytes, at)?;
        at += 4;
        if (0xD800..0xDC00).contains(&leading) {
          // The validator proved a trailing surrogate follows, so this reads the pair it left.
          at += 2;
          let trailing = hex4(bytes, at)?;
          at += 4;
          let combined = 0x1_0000 + ((leading - 0xD800) << 10) + (trailing - 0xDC00);
          out.push(char::from_u32(combined)?);
        } else {
          out.push(char::from_u32(leading)?);
        }
        copied = at;
        continue;
      }
      _ => return None,
    };
    out.push(expanded);
    at += 1;
    copied = at;
  }
  out.push_str(raw.get(copied..)?);
  Some(out)
}

fn hex4(bytes: &[u8], at: usize) -> Option<u32> {
  let mut value = 0u32;
  for offset in 0..4 {
    let digit = *bytes.get(at + offset)?;
    let nibble = match digit {
      b'0'..=b'9' => u32::from(digit - b'0'),
      b'a'..=b'f' => u32::from(digit - b'a') + 10,
      b'A'..=b'F' => u32::from(digit - b'A') + 10,
      _ => return None,
    };
    value = value * 16 + nibble;
  }
  Some(value)
}

#[cfg(test)]
mod tests;
