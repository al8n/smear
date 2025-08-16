use chumsky::{
  extra::ParserExtra,
  input::{SliceInput, StrInput},
  label::LabelError,
  prelude::*,
  text::{Char, TextExpected},
  util::MaybeRef,
};

mod ignored;
mod input_value;

/// The source span
mod span;

/// The punctuators
pub mod punct;

type Error<'a> = chumsky::extra::Err<chumsky::prelude::Rich<'a, char>>;

/// A name
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Name<Src, Span>(Spanned<Src, Span>);

impl<Src, Span> Name<Src, Span> {
  /// Creates a name from a spanned
  #[inline]
  pub const fn new(span: Spanned<Src, Span>) -> Self {
    Self(span)
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.0
  }

  /// Returns the parser to parse a name.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Name<I::Slice, I::Span>, E> + Clone
  where
    I: StrInput<'src>,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    // [_A-Za-z]
    let start = one_of([
      // underscore
      I::Token::UNDERSCORE,

      // A-Z
      I::Token::A, I::Token::B, I::Token::C, I::Token::D, I::Token::E, I::Token::F, I::Token::G,
      I::Token::H, I::Token::I, I::Token::J, I::Token::K, I::Token::L, I::Token::M, I::Token::N,
      I::Token::O, I::Token::P, I::Token::Q, I::Token::R, I::Token::S, I::Token::T, I::Token::U,
      I::Token::V, I::Token::W, I::Token::X, I::Token::Y, I::Token::Z,

      // a-z
      I::Token::a, I::Token::b, I::Token::c, I::Token::d, I::Token::e, I::Token::f, I::Token::g,
      I::Token::h, I::Token::i, I::Token::j, I::Token::k, I::Token::l, I::Token::m, I::Token::n,
      I::Token::o, I::Token::p, I::Token::q, I::Token::r, I::Token::s, I::Token::t, I::Token::u,
      I::Token::v, I::Token::w, I::Token::x, I::Token::y, I::Token::z,
    ])
    .ignored();

    let cont = one_of([
    // underscore
    I::Token::UNDERSCORE,

    // 0-9
    I::Token::ZERO, I::Token::ONE, I::Token::TWO, I::Token::THREE, I::Token::FOUR,
    I::Token::FIVE, I::Token::SIX, I::Token::SEVEN, I::Token::EIGHT, I::Token::NINE,

    // A-Z
    I::Token::A, I::Token::B, I::Token::C, I::Token::D, I::Token::E, I::Token::F, I::Token::G,
    I::Token::H, I::Token::I, I::Token::J, I::Token::K, I::Token::L, I::Token::M, I::Token::N,
    I::Token::O, I::Token::P, I::Token::Q, I::Token::R, I::Token::S, I::Token::T, I::Token::U,
    I::Token::V, I::Token::W, I::Token::X, I::Token::Y, I::Token::Z,

    // a-z
    I::Token::a, I::Token::b, I::Token::c, I::Token::d, I::Token::e, I::Token::f, I::Token::g,
    I::Token::h, I::Token::i, I::Token::j, I::Token::k, I::Token::l, I::Token::m, I::Token::n,
    I::Token::o, I::Token::p, I::Token::q, I::Token::r, I::Token::s, I::Token::t, I::Token::u,
    I::Token::v, I::Token::w, I::Token::x, I::Token::y, I::Token::z,
  ])
  .ignored()
  .repeated();

  start
    .then(cont)
    .map_with(|_, sp| Name::new(Spanned::from(sp)))
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spanned<Src, Span> {
  src: Src,
  span: Span,
}

impl<Src, Span> Spanned<Src, Span> {
  /// Create a new `Spanned` value.
  pub const fn new(src: Src, span: Span) -> Self {
    Self { src, span }
  }

  /// Returns the source
  pub const fn source(&self) -> &Src {
    &self.src
  }

  /// Returns the span
  pub const fn span(&self) -> &Span {
    &self.span
  }
}

impl<'src, 'b, I: SliceInput<'src>, E: ParserExtra<'src, I>>
  From<&mut chumsky::input::MapExtra<'src, 'b, I, E>> for Spanned<I::Slice, I::Span>
{
  fn from(value: &mut chumsky::input::MapExtra<'src, 'b, I, E>) -> Self {
    Self::new(value.slice(), value.span())
  }
}

/// Providing some extra methods for working with `Char`.
#[allow(non_upper_case_globals)]
pub trait SmearChar: Char {
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

impl SmearChar for char {
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

impl SmearChar for u8 {
  fn bom<'a>() -> &'a [Self] {
    "\u{feff}".as_bytes()
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
