use chumsky::{input::StrInput, text::Char as ChumskyChar};

/// Providing some extra methods for working with `Char`.
#[allow(non_upper_case_globals)]
pub trait Char: ChumskyChar {
  /// Returns a sequence representing the BOM
  fn bom<'a>() -> &'a [Self];

  /// The ASCII null character (`\0`)
  const NULL: Self;
  /// The ASCII exclamation mark character (`!`).
  const EXCLAMATION: Self;
  /// The ASCII quotation mark character (`"`).
  const QUOTATION: Self;
  /// The ASCII hash character (`#`).
  const HASH: Self;
  /// The ASCII dollar sign character (`$`).
  const DOLLAR: Self;
  /// The ASCII percent character (`%`).
  const PERCENT: Self;
  /// The ASCII ampersand character (`&`).
  const AMPERSAND: Self;
  /// The ASCII apostrophe character (`'`).
  const APOSTROPHE: Self;
  /// The ASCII left paren character (`(`).
  const PAREN_OPEN: Self;
  /// The ASCII right paren character (`)`).
  const PAREN_CLOSE: Self;
  /// The ASCII left bracket character (`[`).
  const BRACKET_OPEN: Self;
  /// The ASCII right bracket character (`]`).
  const BRACKET_CLOSE: Self;
  /// The ASCII asterisk character (`*`).
  const ASTERISK: Self;
  /// The ASCII plus character (`+`).
  const PLUS: Self;
  /// The ASCII comma character (`,`).
  const COMMA: Self;
  /// The ASCII minus character (`-`).
  const MINUS: Self;
  /// The ASCII dot character (`.`).
  const DOT: Self;
  /// The ASCII slash character (`/`).
  const SLASH: Self;
  /// The ASCII colon character (`:`).
  const COLON: Self;
  /// The ASCII semicolon character (`;`).
  const SEMICOLON: Self;
  /// The ASCII less than character (`<`).
  const LESS_THAN: Self;
  /// The ASCII equal character (`=`).
  const EQUAL: Self;
  /// The ASCII greater than character (`>`).
  const GREATER_THAN: Self;
  /// The ASCII question mark character (`?`).
  const QUESTION: Self;
  /// The ASCII at symbol character (`@`).
  const AT: Self;
  /// The ASCII backslash character (`\`).
  const BACKSLASH: Self;
  /// The ASCII curly brace open character (`{`)
  const CURLY_BRACE_OPEN: Self;
  /// The ASCII curly brace close character (`}`)
  const CURLY_BRACE_CLOSE: Self;
  /// The ASCII tilde character (`~`).
  const TILDE: Self;
  /// The ASCII vertical bar character (`|`).
  const VERTICAL_BAR: Self;
  /// The ASCII underscore character (`_`).
  const UNDERSCORE: Self;

  /// The ASCII space character (` `).
  const SPACE: Self;
  /// The ASCII tab character (`\t`).
  const TAB: Self;
  /// The ASCII line feed character (`\n`).
  const LINE_FEED: Self;
  /// The ASCII carriage return character (`\r`).
  const CARRIAGE_RETURN: Self;

  /// The ASCII '0' character.
  const ZERO: Self;
  /// The ASCII '1' character.
  const ONE: Self;
  /// The ASCII '2' character.
  const TWO: Self;
  /// The ASCII '3' character.
  const THREE: Self;
  /// The ASCII '4' character.
  const FOUR: Self;
  /// The ASCII '5' character.
  const FIVE: Self;
  /// The ASCII '6' character.
  const SIX: Self;
  /// The ASCII '7' character.
  const SEVEN: Self;
  /// The ASCII '8' character.
  const EIGHT: Self;
  /// The ASCII '9' character.
  const NINE: Self;

  /// The ASCII 'A' character.
  const A: Self;
  /// The ASCII 'B' character.
  const B: Self;
  /// The ASCII 'C' character.
  const C: Self;
  /// The ASCII 'D' character.
  const D: Self;
  /// The ASCII 'E' character.
  const E: Self;
  /// The ASCII 'F' character.
  const F: Self;
  /// The ASCII 'G' character.
  const G: Self;
  /// The ASCII 'H' character.
  const H: Self;
  /// The ASCII 'I' character.
  const I: Self;
  /// The ASCII 'J' character.
  const J: Self;
  /// The ASCII 'K' character.
  const K: Self;
  /// The ASCII 'L' character.
  const L: Self;
  /// The ASCII 'M' character.
  const M: Self;
  /// The ASCII 'N' character.
  const N: Self;
  /// The ASCII 'O' character.
  const O: Self;
  /// The ASCII 'P' character.
  const P: Self;
  /// The ASCII 'Q' character.
  const Q: Self;
  /// The ASCII 'R' character.
  const R: Self;
  /// The ASCII 'S' character.
  const S: Self;
  /// The ASCII 'T' character.
  const T: Self;
  /// The ASCII 'U' character.
  const U: Self;
  /// The ASCII 'V' character.
  const V: Self;
  /// The ASCII 'W' character.
  const W: Self;
  /// The ASCII 'X' character.
  const X: Self;
  /// The ASCII 'Y' character.
  const Y: Self;
  /// The ASCII 'Z' character.
  const Z: Self;
  /// The ASCII 'a' character.
  const a: Self;
  /// The ASCII 'b' character.
  const b: Self;
  /// The ASCII 'c' character.
  const c: Self;
  /// The ASCII 'd' character.
  const d: Self;
  /// The ASCII 'e' character.
  const e: Self;
  /// The ASCII 'f' character.
  const f: Self;
  /// The ASCII 'g' character.
  const g: Self;
  /// The ASCII 'h' character.
  const h: Self;
  /// The ASCII 'i' character.
  const i: Self;
  /// The ASCII 'j' character.
  const j: Self;
  /// The ASCII 'k' character.
  const k: Self;
  /// The ASCII 'l' character.
  const l: Self;
  /// The ASCII 'm' character.
  const m: Self;
  /// The ASCII 'n' character.
  const n: Self;
  /// The ASCII 'o' character.
  const o: Self;
  /// The ASCII 'p' character.
  const p: Self;
  /// The ASCII 'q' character.
  const q: Self;
  /// The ASCII 'r' character.
  const r: Self;
  /// The ASCII 's' character.
  const s: Self;
  /// The ASCII 't' character.
  const t: Self;
  /// The ASCII 'u' character.
  const u: Self;
  /// The ASCII 'v' character.
  const v: Self;
  /// The ASCII 'w' character.
  const w: Self;
  /// The ASCII 'x' character.
  const x: Self;
  /// The ASCII 'y' character.
  const y: Self;
  /// The ASCII 'z' character.
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

pub trait Slice {
  type Token;

  fn iter<'a>(&'a self) -> impl Iterator<Item = Self::Token> + 'a;

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

/// An extension trait over [`chumsky::StrInput`]
pub trait Source<'src>: StrInput<'src>
where
  Self::Token: Char + 'src,
  Self::Slice: Slice<Token = Self::Token>,
{
  /// The character tokens for the digits 0-9.
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

  /// The character tokens for the digits 1-9.
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

  /// The character tokens for the letters
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

  /// The character tokens for the lower case letters
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

  /// The character tokens for the upper case letters
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
