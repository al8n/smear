//! Utilities for [`smear`](https://crates.io/crates/smear) and its ecosystem.
#![cfg_attr(not(feature = "std"), no_std)]
#![deny(missing_docs, warnings)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

use chumsky::{input::StrInput, text::Char as ChumskyChar};

pub use convert::{FromMapExtra, IntoComponents, IntoSpan};
pub use with_source::WithSource;

pub use ascii::AsciiChar;

mod convert;
mod with_source;

/// General utilities for working with the lexer.
pub mod lexer;

#[doc(hidden)]
pub mod __private {
  pub use super::lexer::{self, Require, Tokenizer, token};
  pub use chumsky;
}

/// Extension trait providing standardized character constants for GraphQL parsing.
///
/// This trait extends Chumsky's `Char` trait with a comprehensive set of ASCII
/// character constants and utility methods specifically designed for GraphQL
/// parsing. It provides a character-agnostic interface that works with both
/// `char` and `u8` tokens, enabling efficient parsing of both UTF-8 strings
/// and byte arrays.
#[allow(non_upper_case_globals)]
pub trait Char: ChumskyChar {
  /// Returns a token sequence representing the Byte Order Mark (BOM).
  ///
  /// The BOM is used to identify the encoding and byte order of text files.
  /// This method returns the appropriate BOM sequence for the character type:
  /// - For `char`: Returns `['\u{FEFF}']` (Unicode BOM)
  /// - For `u8`: Returns `[0xEF, 0xBB, 0xBF]` (UTF-8 BOM)
  fn bom<'a>() -> &'a [Self];

  // Control and Special Characters
  /// The ASCII null character (`\0`, U+0000).
  ///
  /// Used in some text processing contexts, though not typically
  /// encountered in GraphQL documents.
  const NULL: Self;

  // Punctuation Characters (GraphQL Syntax)
  /// The ASCII exclamation mark character (`!`, U+0021).
  ///
  /// Used in GraphQL for non-null type indicators: `String!`, `[Int!]!`
  const EXCLAMATION: Self;

  /// The ASCII quotation mark character (`"`, U+0022).
  ///
  /// Used for string literal delimiters: `"hello world"`
  const QUOTATION: Self;

  /// The ASCII hash character (`#`, U+0023).
  ///
  /// Used for GraphQL comments: `# This is a comment`
  const HASH: Self;

  /// The ASCII dollar sign character (`$`, U+0024).
  ///
  /// Used for variable references: `$userId`, `$filter`
  const DOLLAR: Self;

  /// The ASCII percent character (`%`, U+0025).
  ///
  /// Not used in standard GraphQL syntax but included for completeness.
  const PERCENT: Self;

  /// The ASCII ampersand character (`&`, U+0026).
  ///
  /// Not used in standard GraphQL syntax but included for completeness.
  const AMPERSAND: Self;

  /// The ASCII apostrophe character (`'`, U+0027).
  ///
  /// Not used in GraphQL (which uses double quotes for strings).
  const APOSTROPHE: Self;

  /// The ASCII left parenthesis character (`(`, U+0028).
  ///
  /// Used for grouping in field arguments: `user(id: 123)`
  const PAREN_OPEN: Self;

  /// The ASCII right parenthesis character (`)`, U+0029).
  ///
  /// Used for closing argument groups: `user(id: 123)`
  const PAREN_CLOSE: Self;

  /// The ASCII left square bracket character (`[`, U+005B).
  ///
  /// Used for list literals and list types: `[1, 2, 3]`, `[String]`
  const BRACKET_OPEN: Self;

  /// The ASCII right square bracket character (`]`, U+005D).
  ///
  /// Used for closing lists: `[1, 2, 3]`, `[String]`
  const BRACKET_CLOSE: Self;

  /// The ASCII asterisk character (`*`, U+002A).
  ///
  /// Not used in standard GraphQL syntax but included for completeness.
  const ASTERISK: Self;

  /// The ASCII plus character (`+`, U+002B).
  ///
  /// Used in numeric literals for positive exponents: `1.23e+4`
  const PLUS: Self;

  /// The ASCII comma character (`,`, U+002C).
  ///
  /// Used for separating list elements and field arguments: `[1, 2, 3]`
  const COMMA: Self;

  /// The ASCII minus character (`-`, U+002D).
  ///
  /// Used for negative numbers and exponents: `-123`, `1.23e-4`
  const MINUS: Self;

  /// The ASCII period/dot character (`.`, U+002E).
  ///
  /// Used in float literals for decimal points: `3.14`, `0.5`
  const DOT: Self;

  /// The ASCII forward slash character (`/`, U+002F).
  ///
  /// Used in escape sequences: `\"`, `\/` and potentially in custom scalars.
  const SLASH: Self;

  /// The ASCII colon character (`:`, U+003A).
  ///
  /// Used for field-value separation and type annotations: `name: "John"`, `$id: Int`
  const COLON: Self;

  /// The ASCII semicolon character (`;`, U+003B).
  ///
  /// Not used in standard GraphQL syntax but included for completeness.
  const SEMICOLON: Self;

  /// The ASCII less-than character (`<`, U+003C).
  ///
  /// Not used in standard GraphQL syntax but included for completeness.
  const LESS_THAN: Self;

  /// The ASCII equals character (`=`, U+003D).
  ///
  /// Used for default values in variable declarations: `$limit: Int = 10`
  const EQUAL: Self;

  /// The ASCII greater-than character (`>`, U+003E).
  ///
  /// Not used in standard GraphQL syntax but included for completeness.
  const GREATER_THAN: Self;

  /// The ASCII question mark character (`?`, U+003F).
  ///
  /// Not used in standard GraphQL syntax but included for completeness.
  const QUESTION: Self;

  /// The ASCII at symbol character (`@`, U+0040).
  ///
  /// Used for directive applications: `@include(if: $showDetails)`
  const AT: Self;

  /// The ASCII backslash character (`\`, U+005C).
  ///
  /// Used for escape sequences in strings: `\"`, `\\`, `\n`, `\uXXXX`
  const BACKSLASH: Self;

  /// The ASCII left curly brace character (`{`, U+007B).
  ///
  /// Used for object literals and selection sets: `{ name age }`, `{ id: 1 }`
  const CURLY_BRACE_OPEN: Self;

  /// The ASCII right curly brace character (`}`, U+007D).
  ///
  /// Used for closing objects and selection sets: `{ name age }`
  const CURLY_BRACE_CLOSE: Self;

  /// The ASCII tilde character (`~`, U+007E).
  ///
  /// Not used in standard GraphQL syntax but included for completeness.
  const TILDE: Self;

  /// The ASCII vertical bar character (`|`, U+007C).
  ///
  /// Not used in standard GraphQL syntax but included for completeness.
  const VERTICAL_BAR: Self;

  /// The ASCII underscore character (`_`, U+005F).
  ///
  /// Used in identifiers and names: `user_name`, `_private`, `__typename`
  const UNDERSCORE: Self;

  // Whitespace Characters
  /// The ASCII space character (` `, U+0020).
  ///
  /// Standard whitespace for separating tokens in GraphQL.
  const SPACE: Self;

  /// The ASCII tab character (`\t`, U+0009).
  ///
  /// Horizontal tab, treated as whitespace in GraphQL.
  const TAB: Self;

  /// The ASCII line feed character (`\n`, U+000A).
  ///
  /// Line terminator, used to end lines and terminate comments.
  const LINE_FEED: Self;

  /// The ASCII carriage return character (`\r`, U+000D).
  ///
  /// Line terminator (Windows-style), often paired with line feed.
  const CARRIAGE_RETURN: Self;

  // Digit Characters (0-9)
  /// The ASCII '0' character (U+0030).
  const ZERO: Self;
  /// The ASCII '1' character (U+0031).
  const ONE: Self;
  /// The ASCII '2' character (U+0032).
  const TWO: Self;
  /// The ASCII '3' character (U+0033).
  const THREE: Self;
  /// The ASCII '4' character (U+0034).
  const FOUR: Self;
  /// The ASCII '5' character (U+0035).
  const FIVE: Self;
  /// The ASCII '6' character (U+0036).
  const SIX: Self;
  /// The ASCII '7' character (U+0037).
  const SEVEN: Self;
  /// The ASCII '8' character (U+0038).
  const EIGHT: Self;
  /// The ASCII '9' character (U+0039).
  const NINE: Self;

  // Uppercase Letters (A-Z)
  /// The ASCII 'A' character (U+0041).
  const A: Self;
  /// The ASCII 'B' character (U+0042).
  const B: Self;
  /// The ASCII 'C' character (U+0043).
  const C: Self;
  /// The ASCII 'D' character (U+0044).
  const D: Self;
  /// The ASCII 'E' character (U+0045).
  const E: Self;
  /// The ASCII 'F' character (U+0046).
  const F: Self;
  /// The ASCII 'G' character (U+0047).
  const G: Self;
  /// The ASCII 'H' character (U+0048).
  const H: Self;
  /// The ASCII 'I' character (U+0049).
  const I: Self;
  /// The ASCII 'J' character (U+004A).
  const J: Self;
  /// The ASCII 'K' character (U+004B).
  const K: Self;
  /// The ASCII 'L' character (U+004C).
  const L: Self;
  /// The ASCII 'M' character (U+004D).
  const M: Self;
  /// The ASCII 'N' character (U+004E).
  const N: Self;
  /// The ASCII 'O' character (U+004F).
  const O: Self;
  /// The ASCII 'P' character (U+0050).
  const P: Self;
  /// The ASCII 'Q' character (U+0051).
  const Q: Self;
  /// The ASCII 'R' character (U+0052).
  const R: Self;
  /// The ASCII 'S' character (U+0053).
  const S: Self;
  /// The ASCII 'T' character (U+0054).
  const T: Self;
  /// The ASCII 'U' character (U+0055).
  const U: Self;
  /// The ASCII 'V' character (U+0056).
  const V: Self;
  /// The ASCII 'W' character (U+0057).
  const W: Self;
  /// The ASCII 'X' character (U+0058).
  const X: Self;
  /// The ASCII 'Y' character (U+0059).
  const Y: Self;
  /// The ASCII 'Z' character (U+005A).
  const Z: Self;

  // Lowercase Letters (a-z)
  /// The ASCII 'a' character (U+0061).
  const a: Self;
  /// The ASCII 'b' character (U+0062).
  const b: Self;
  /// The ASCII 'c' character (U+0063).
  const c: Self;
  /// The ASCII 'd' character (U+0064).
  const d: Self;
  /// The ASCII 'e' character (U+0065).
  const e: Self;
  /// The ASCII 'f' character (U+0066).
  const f: Self;
  /// The ASCII 'g' character (U+0067).
  const g: Self;
  /// The ASCII 'h' character (U+0068).
  const h: Self;
  /// The ASCII 'i' character (U+0069).
  const i: Self;
  /// The ASCII 'j' character (U+006A).
  const j: Self;
  /// The ASCII 'k' character (U+006B).
  const k: Self;
  /// The ASCII 'l' character (U+006C).
  const l: Self;
  /// The ASCII 'm' character (U+006D).
  const m: Self;
  /// The ASCII 'n' character (U+006E).
  const n: Self;
  /// The ASCII 'o' character (U+006F).
  const o: Self;
  /// The ASCII 'p' character (U+0070).
  const p: Self;
  /// The ASCII 'q' character (U+0071).
  const q: Self;
  /// The ASCII 'r' character (U+0072).
  const r: Self;
  /// The ASCII 's' character (U+0073).
  const s: Self;
  /// The ASCII 't' character (U+0074).
  const t: Self;
  /// The ASCII 'u' character (U+0075).
  const u: Self;
  /// The ASCII 'v' character (U+0076).
  const v: Self;
  /// The ASCII 'w' character (U+0077).
  const w: Self;
  /// The ASCII 'x' character (U+0078).
  const x: Self;
  /// The ASCII 'y' character (U+0079).
  const y: Self;
  /// The ASCII 'z' character (U+007A).
  const z: Self;
}

impl Char for char {
  #[inline]
  fn bom<'a>() -> &'a [Self] {
    &['\u{feff}']
  }

  const NULL: Self = '\0';
  const EXCLAMATION: Self = '!';
  const QUOTATION: Self = '"';
  const HASH: Self = '#';
  const DOLLAR: Self = '$';
  const PERCENT: Self = '%';
  const AMPERSAND: Self = '&';
  const APOSTROPHE: Self = '\'';
  const PAREN_OPEN: Self = '(';
  const PAREN_CLOSE: Self = ')';
  const BRACKET_OPEN: Self = '[';
  const BRACKET_CLOSE: Self = ']';
  const ASTERISK: Self = '*';
  const PLUS: Self = '+';
  const COMMA: Self = ',';
  const MINUS: Self = '-';
  const DOT: Self = '.';
  const SLASH: Self = '/';
  const COLON: Self = ':';
  const SEMICOLON: Self = ';';
  const LESS_THAN: Self = '<';
  const EQUAL: Self = '=';
  const GREATER_THAN: Self = '>';
  const QUESTION: Self = '?';
  const AT: Self = '@';
  const BACKSLASH: Self = '\\';
  const CURLY_BRACE_OPEN: Self = '{';
  const CURLY_BRACE_CLOSE: Self = '}';
  const TILDE: Self = '~';
  const VERTICAL_BAR: Self = '|';
  const UNDERSCORE: Self = '_';
  const SPACE: Self = ' ';
  const TAB: Self = '\t';
  const LINE_FEED: Self = '\n';
  const CARRIAGE_RETURN: Self = '\r';
  const ZERO: Self = '0';
  const ONE: Self = '1';
  const TWO: Self = '2';
  const THREE: Self = '3';
  const FOUR: Self = '4';
  const FIVE: Self = '5';
  const SIX: Self = '6';
  const SEVEN: Self = '7';
  const EIGHT: Self = '8';
  const NINE: Self = '9';
  const A: Self = 'A';
  const B: Self = 'B';
  const C: Self = 'C';
  const D: Self = 'D';
  const E: Self = 'E';
  const F: Self = 'F';
  const G: Self = 'G';
  const H: Self = 'H';
  const I: Self = 'I';
  const J: Self = 'J';
  const K: Self = 'K';
  const L: Self = 'L';
  const M: Self = 'M';
  const N: Self = 'N';
  const O: Self = 'O';
  const P: Self = 'P';
  const Q: Self = 'Q';
  const R: Self = 'R';
  const S: Self = 'S';
  const T: Self = 'T';
  const U: Self = 'U';
  const V: Self = 'V';
  const W: Self = 'W';
  const X: Self = 'X';
  const Y: Self = 'Y';
  const Z: Self = 'Z';
  const a: Self = 'a';
  const b: Self = 'b';
  const c: Self = 'c';
  const d: Self = 'd';
  const e: Self = 'e';
  const f: Self = 'f';
  const g: Self = 'g';
  const h: Self = 'h';
  const i: Self = 'i';
  const j: Self = 'j';
  const k: Self = 'k';
  const l: Self = 'l';
  const m: Self = 'm';
  const n: Self = 'n';
  const o: Self = 'o';
  const p: Self = 'p';
  const q: Self = 'q';
  const r: Self = 'r';
  const s: Self = 's';
  const t: Self = 't';
  const u: Self = 'u';
  const v: Self = 'v';
  const w: Self = 'w';
  const x: Self = 'x';
  const y: Self = 'y';
  const z: Self = 'z';
}

impl Char for u8 {
  #[inline]
  fn bom<'a>() -> &'a [Self] {
    const UTF8_BOM: &[u8] = b"\xEF\xBB\xBF";
    UTF8_BOM
  }

  const NULL: Self = b'\0';
  const EXCLAMATION: Self = b'!';
  const QUOTATION: Self = b'"';
  const HASH: Self = b'#';
  const DOLLAR: Self = b'$';
  const PERCENT: Self = b'%';
  const AMPERSAND: Self = b'&';
  const APOSTROPHE: Self = b'\'';
  const PAREN_OPEN: Self = b'(';
  const PAREN_CLOSE: Self = b')';
  const BRACKET_OPEN: Self = b'[';
  const BRACKET_CLOSE: Self = b']';
  const ASTERISK: Self = b'*';
  const PLUS: Self = b'+';
  const COMMA: Self = b',';
  const MINUS: Self = b'-';
  const DOT: Self = b'.';
  const SLASH: Self = b'/';
  const COLON: Self = b':';
  const SEMICOLON: Self = b';';
  const LESS_THAN: Self = b'<';
  const EQUAL: Self = b'=';
  const GREATER_THAN: Self = b'>';
  const QUESTION: Self = b'?';
  const AT: Self = b'@';
  const BACKSLASH: Self = b'\\';
  const CURLY_BRACE_OPEN: Self = b'{';
  const CURLY_BRACE_CLOSE: Self = b'}';
  const TILDE: Self = b'~';
  const VERTICAL_BAR: Self = b'|';
  const UNDERSCORE: Self = b'_';
  const SPACE: Self = b' ';
  const TAB: Self = b'\t';
  const LINE_FEED: Self = b'\n';
  const CARRIAGE_RETURN: Self = b'\r';
  const ZERO: Self = b'0';
  const ONE: Self = b'1';
  const TWO: Self = b'2';
  const THREE: Self = b'3';
  const FOUR: Self = b'4';
  const FIVE: Self = b'5';
  const SIX: Self = b'6';
  const SEVEN: Self = b'7';
  const EIGHT: Self = b'8';
  const NINE: Self = b'9';
  const A: Self = b'A';
  const B: Self = b'B';
  const C: Self = b'C';
  const D: Self = b'D';
  const E: Self = b'E';
  const F: Self = b'F';
  const G: Self = b'G';
  const H: Self = b'H';
  const I: Self = b'I';
  const J: Self = b'J';
  const K: Self = b'K';
  const L: Self = b'L';
  const M: Self = b'M';
  const N: Self = b'N';
  const O: Self = b'O';
  const P: Self = b'P';
  const Q: Self = b'Q';
  const R: Self = b'R';
  const S: Self = b'S';
  const T: Self = b'T';
  const U: Self = b'U';
  const V: Self = b'V';
  const W: Self = b'W';
  const X: Self = b'X';
  const Y: Self = b'Y';
  const Z: Self = b'Z';
  const a: Self = b'a';
  const b: Self = b'b';
  const c: Self = b'c';
  const d: Self = b'd';
  const e: Self = b'e';
  const f: Self = b'f';
  const g: Self = b'g';
  const h: Self = b'h';
  const i: Self = b'i';
  const j: Self = b'j';
  const k: Self = b'k';
  const l: Self = b'l';
  const m: Self = b'm';
  const n: Self = b'n';
  const o: Self = b'o';
  const p: Self = b'p';
  const q: Self = b'q';
  const r: Self = b'r';
  const s: Self = b's';
  const t: Self = b't';
  const u: Self = b'u';
  const v: Self = b'v';
  const w: Self = b'w';
  const x: Self = b'x';
  const y: Self = b'y';
  const z: Self = b'z';
}

/// Trait for types that can be treated as token slices in parsing.
///
/// This trait provides a unified interface for working with different slice
/// types that contain sequences of tokens. It enables generic parsing code
/// that works with various input representations while maintaining efficient
/// iteration and comparison operations.
pub trait Slice {
  /// The type of individual tokens contained in this slice.
  ///
  /// This determines what kind of elements the slice contains:
  /// - `char` for string slices
  /// - `u8` for byte slices
  /// - Custom types for specialized parsing scenarios
  type Token;

  /// Returns an iterator over the tokens in this slice.
  ///
  /// The iterator should provide efficient access to each token without
  /// unnecessary copying or allocation. Implementations should prefer
  /// zero-copy iteration when possible.
  fn iter<'a>(&'a self) -> impl Iterator<Item = Self::Token> + 'a;

  /// Returns `true` if this slice is equivalent to a sequence of tokens.
  ///
  /// This method provides an efficient way to compare the slice contents
  /// with an iterator of tokens, typically used for pattern matching in
  /// parsing contexts. The comparison should short-circuit on the first
  /// mismatch for optimal performance.
  fn equivalent(&self, other: impl Iterator<Item = Self::Token>) -> bool;
}

impl Slice for &str {
  type Token = char;

  #[inline]
  fn iter<'a>(&'a self) -> impl Iterator<Item = Self::Token> + 'a {
    self.chars()
  }

  #[inline]
  fn equivalent(&self, other: impl Iterator<Item = Self::Token>) -> bool {
    self.chars().eq(other)
  }
}

impl Slice for &[u8] {
  type Token = u8;

  #[inline]
  fn iter<'a>(&'a self) -> impl Iterator<Item = Self::Token> + 'a {
    <[u8]>::iter(self).copied()
  }

  #[inline]
  fn equivalent(&self, other: impl Iterator<Item = Self::Token>) -> bool {
    self.iter().eq(other)
  }
}

impl<const N: usize> Slice for &[u8; N] {
  type Token = u8;

  #[inline]
  fn iter<'a>(&'a self) -> impl Iterator<Item = Self::Token> + 'a {
    <[u8]>::iter(self.as_slice()).copied()
  }

  #[inline]
  fn equivalent(&self, other: impl Iterator<Item = Self::Token>) -> bool {
    self.iter().eq(other)
  }
}

#[cfg(feature = "bytes")]
impl Slice for bytes::Bytes {
  type Token = u8;

  #[inline]
  fn iter<'a>(&'a self) -> impl Iterator<Item = Self::Token> + 'a {
    <[u8]>::iter(self.as_ref()).copied()
  }

  #[inline]
  fn equivalent(&self, other: impl Iterator<Item = Self::Token>) -> bool {
    self.iter().eq(other)
  }
}

/// Extension trait providing GraphQL-specific functionality for input sources.
///
/// This trait extends Chumsky's [`StrInput`] with predefined character sets and
/// utility constants specifically designed for GraphQL parsing. It provides
/// efficient access to commonly used character groups, enabling fast and
/// readable parser construction.
pub trait Source<'src>: StrInput<'src>
where
  Self::Token: Char + 'src,
  Self::Slice: Slice<Token = Self::Token>,
{
  /// Character tokens for all decimal digits (0-9).
  ///
  /// This array contains all ten decimal digit characters in ascending order.
  /// Used for parsing any numeric content in GraphQL literals.
  const DIGITS: [Self::Token; 10] = [
    Self::Token::ZERO,
    Self::Token::ONE,
    Self::Token::TWO,
    Self::Token::THREE,
    Self::Token::FOUR,
    Self::Token::FIVE,
    Self::Token::SIX,
    Self::Token::SEVEN,
    Self::Token::EIGHT,
    Self::Token::NINE,
  ];

  /// Character tokens for non-zero digits (1-9).
  ///
  /// This array excludes '0' and is specifically used for GraphQL's "no leading
  /// zeros" rule in integer literals. According to the GraphQL specification,
  /// multi-digit numbers cannot start with '0' (except for the literal '0' itself).
  const NON_ZERO_DIGITS: [Self::Token; 9] = [
    Self::Token::ONE,
    Self::Token::TWO,
    Self::Token::THREE,
    Self::Token::FOUR,
    Self::Token::FIVE,
    Self::Token::SIX,
    Self::Token::SEVEN,
    Self::Token::EIGHT,
    Self::Token::NINE,
  ];

  /// Character tokens for all ASCII letters (A-Z, a-z).
  ///
  /// This array contains all 52 ASCII letters, with uppercase letters first
  /// followed by lowercase letters. Used for parsing GraphQL identifiers,
  /// names, and keywords.
  const LETTERS: [Self::Token; 52] = [
    Self::Token::A,
    Self::Token::B,
    Self::Token::C,
    Self::Token::D,
    Self::Token::E,
    Self::Token::F,
    Self::Token::G,
    Self::Token::H,
    Self::Token::I,
    Self::Token::J,
    Self::Token::K,
    Self::Token::L,
    Self::Token::M,
    Self::Token::N,
    Self::Token::O,
    Self::Token::P,
    Self::Token::Q,
    Self::Token::R,
    Self::Token::S,
    Self::Token::T,
    Self::Token::U,
    Self::Token::V,
    Self::Token::W,
    Self::Token::X,
    Self::Token::Y,
    Self::Token::Z,
    // Lowercase letters
    Self::Token::a,
    Self::Token::b,
    Self::Token::c,
    Self::Token::d,
    Self::Token::e,
    Self::Token::f,
    Self::Token::g,
    Self::Token::h,
    Self::Token::i,
    Self::Token::j,
    Self::Token::k,
    Self::Token::l,
    Self::Token::m,
    Self::Token::n,
    Self::Token::o,
    Self::Token::p,
    Self::Token::q,
    Self::Token::r,
    Self::Token::s,
    Self::Token::t,
    Self::Token::u,
    Self::Token::v,
    Self::Token::w,
    Self::Token::x,
    Self::Token::y,
    Self::Token::z,
  ];

  /// Character tokens for lowercase ASCII letters (a-z).
  ///
  /// This array contains only the 26 lowercase ASCII letters. Useful for
  /// case-sensitive parsing where only lowercase letters are expected,
  /// such as GraphQL keywords and reserved words.
  const LOWERCASE_LETTERS: [Self::Token; 26] = [
    Self::Token::a,
    Self::Token::b,
    Self::Token::c,
    Self::Token::d,
    Self::Token::e,
    Self::Token::f,
    Self::Token::g,
    Self::Token::h,
    Self::Token::i,
    Self::Token::j,
    Self::Token::k,
    Self::Token::l,
    Self::Token::m,
    Self::Token::n,
    Self::Token::o,
    Self::Token::p,
    Self::Token::q,
    Self::Token::r,
    Self::Token::s,
    Self::Token::t,
    Self::Token::u,
    Self::Token::v,
    Self::Token::w,
    Self::Token::x,
    Self::Token::y,
    Self::Token::z,
  ];

  /// Character tokens for uppercase ASCII letters (A-Z).
  ///
  /// This array contains only the 26 uppercase ASCII letters. Useful for
  /// case-sensitive parsing where only uppercase letters are expected.
  const UPPERCASE_LETTERS: [Self::Token; 26] = [
    Self::Token::A,
    Self::Token::B,
    Self::Token::C,
    Self::Token::D,
    Self::Token::E,
    Self::Token::F,
    Self::Token::G,
    Self::Token::H,
    Self::Token::I,
    Self::Token::J,
    Self::Token::K,
    Self::Token::L,
    Self::Token::M,
    Self::Token::N,
    Self::Token::O,
    Self::Token::P,
    Self::Token::Q,
    Self::Token::R,
    Self::Token::S,
    Self::Token::T,
    Self::Token::U,
    Self::Token::V,
    Self::Token::W,
    Self::Token::X,
    Self::Token::Y,
    Self::Token::Z,
  ];
}

impl<'src> Source<'src> for &'src str {}

impl<'src> Source<'src> for &'src [u8] {}

impl<'src, const N: usize> Source<'src> for &'src [u8; N] {}

#[cfg(feature = "bytes")]
impl<'src> Source<'src> for bytes::Bytes {}

const _: () = {
  const fn assert_impls_source<'src, T: Source<'src>>()
  where
    T::Token: Char + 'src,
    T::Slice: Slice<Token = T::Token>,
  {
  }

  assert_impls_source::<&'static str>();
  assert_impls_source::<&'static [u8]>();
  assert_impls_source::<&'static [u8; 100]>();

  #[cfg(feature = "bytes")]
  assert_impls_source::<bytes::Bytes>();
};
