use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use super::{char::Char, source::Source, spanned::Spanned};

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
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    // [_A-Za-z]
    let start = one_of([
      I::Token::UNDERSCORE,
      I::Token::A,
      I::Token::B,
      I::Token::C,
      I::Token::D,
      I::Token::E,
      I::Token::F,
      I::Token::G,
      I::Token::H,
      I::Token::I,
      I::Token::J,
      I::Token::K,
      I::Token::L,
      I::Token::M,
      I::Token::N,
      I::Token::O,
      I::Token::P,
      I::Token::Q,
      I::Token::R,
      I::Token::S,
      I::Token::T,
      I::Token::U,
      I::Token::V,
      I::Token::W,
      I::Token::X,
      I::Token::Y,
      I::Token::Z,
      I::Token::a,
      I::Token::b,
      I::Token::c,
      I::Token::d,
      I::Token::e,
      I::Token::f,
      I::Token::g,
      I::Token::h,
      I::Token::i,
      I::Token::j,
      I::Token::k,
      I::Token::l,
      I::Token::m,
      I::Token::n,
      I::Token::o,
      I::Token::p,
      I::Token::q,
      I::Token::r,
      I::Token::s,
      I::Token::t,
      I::Token::u,
      I::Token::v,
      I::Token::w,
      I::Token::x,
      I::Token::y,
      I::Token::z,
    ])
    .labelled("name start")
    .ignored();

    // [_0-9A-Za-z]*
    let cont = one_of([
      I::Token::UNDERSCORE,
      I::Token::ZERO,
      I::Token::ONE,
      I::Token::TWO,
      I::Token::THREE,
      I::Token::FOUR,
      I::Token::FIVE,
      I::Token::SIX,
      I::Token::SEVEN,
      I::Token::EIGHT,
      I::Token::NINE,
      I::Token::A,
      I::Token::B,
      I::Token::C,
      I::Token::D,
      I::Token::E,
      I::Token::F,
      I::Token::G,
      I::Token::H,
      I::Token::I,
      I::Token::J,
      I::Token::K,
      I::Token::L,
      I::Token::M,
      I::Token::N,
      I::Token::O,
      I::Token::P,
      I::Token::Q,
      I::Token::R,
      I::Token::S,
      I::Token::T,
      I::Token::U,
      I::Token::V,
      I::Token::W,
      I::Token::X,
      I::Token::Y,
      I::Token::Z,
      I::Token::a,
      I::Token::b,
      I::Token::c,
      I::Token::d,
      I::Token::e,
      I::Token::f,
      I::Token::g,
      I::Token::h,
      I::Token::i,
      I::Token::j,
      I::Token::k,
      I::Token::l,
      I::Token::m,
      I::Token::n,
      I::Token::o,
      I::Token::p,
      I::Token::q,
      I::Token::r,
      I::Token::s,
      I::Token::t,
      I::Token::u,
      I::Token::v,
      I::Token::w,
      I::Token::x,
      I::Token::y,
      I::Token::z,
    ])
    .ignored()
    .repeated();

    start
      .then(cont)
      .map_with(|_, sp| Name::new(Spanned::from(sp)))
      .labelled("name")
  }
}
