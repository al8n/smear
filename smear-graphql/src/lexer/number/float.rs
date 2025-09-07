// pub use exponent::{ExponentError, UnexpectedEndOfExponent, ExponentHint, ExponentInput, lex_exponent, lex_exponent_with_identifier};
// pub use fractional::{FractionalError, FractionalHint, UnexpectedFractionalCharacter, FractionalInput, lex_fractional, lex_fractional_with_dot};


// use logos::Logos;
// use core::ops::Range;

// mod exponent;
// mod fractional;

// #[derive(Logos, Debug, Clone)]
// enum FindLongestInvalidSequence {
//   #[regex(r"[a-zA-Z_]+", |lexer| lexer.span())]
//   Span(Range<usize>),
//   #[regex(r"[ \t\r\n,\ufeff]+")]
//   WhiteSpaces,
// }

// #[derive(Logos, Debug, Clone)]
// #[logos(source = [u8])]
// enum FindLongestInvalidBytesSequence {
//   #[regex(r"[a-zA-Z_]+", |lexer| lexer.span())]
//   Span(Range<usize>),
//   #[regex(r"[ \t\r\n,\ufeff]+")]
//   WhiteSpaces,
// }
