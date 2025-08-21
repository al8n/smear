use chumsky::input::StrInput;

use super::char::Char;

/// An extension trait over [`chumsky::StrInput`]
pub trait Source<'src>: StrInput<'src>
where
  Self::Token: Char + 'src,
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

  /// Returns an iterator over the associated type `Slice` which yields tokens of the input.
  fn iter_slice(src: &'src Self::Slice) -> impl Iterator<Item = Self::Token> + 'src;

  /// Returns `true` if the given slice contains a value `true`.
  fn is_true_slice(src: &Self::Slice) -> bool;

  /// Returns `true` if the given slice contains a value `false`.
  fn is_false_slice(src: &Self::Slice) -> bool;

  /// Returns `true` if the given slice contains a value `null`.
  fn is_null_slice(src: &Self::Slice) -> bool;
}

impl<'src> Source<'src> for &'src str {
  #[inline]
  fn iter_slice(src: &'src Self::Slice) -> impl Iterator<Item = Self::Token> + 'src {
    src.chars()
  }

  #[inline]
  fn is_true_slice(src: &Self::Slice) -> bool {
    src.eq(&"true")
  }

  #[inline]
  fn is_false_slice(src: &Self::Slice) -> bool {
    src.eq(&"false")
  }

  #[inline]
  fn is_null_slice(src: &Self::Slice) -> bool {
    src.eq(&"null")
  }
}

impl<'src> Source<'src> for &'src [u8] {
  #[inline]
  fn iter_slice(src: &'src Self::Slice) -> impl Iterator<Item = Self::Token> + 'src {
    src.iter().copied()
  }

  #[inline]
  fn is_true_slice(src: &Self::Slice) -> bool {
    src == b"true"
  }

  #[inline]
  fn is_false_slice(src: &Self::Slice) -> bool {
    src == b"false"
  }

  #[inline]
  fn is_null_slice(src: &Self::Slice) -> bool {
    src == b"null"
  }
}

impl<'src, const N: usize> Source<'src> for &'src [u8; N] {
  #[inline]
  fn iter_slice(src: &'src Self::Slice) -> impl Iterator<Item = Self::Token> + 'src {
    src.iter().copied()
  }

  #[inline]
  fn is_true_slice(src: &Self::Slice) -> bool {
    <&[u8] as Source<'src>>::is_true_slice(src)
  }

  #[inline]
  fn is_false_slice(src: &Self::Slice) -> bool {
    <&[u8] as Source<'src>>::is_false_slice(src)
  }

  #[inline]
  fn is_null_slice(src: &Self::Slice) -> bool {
    <&[u8] as Source<'src>>::is_null_slice(src)
  }
}

#[cfg(feature = "bytes")]
impl<'src> Source<'src> for bytes::Bytes {
  #[inline]
  fn iter_slice(src: &'src Self::Slice) -> impl Iterator<Item = Self::Token> + 'src {
    src.iter().copied()
  }

  #[inline]
  fn is_true_slice(src: &Self::Slice) -> bool {
    src.eq(&b"true"[..])
  }

  #[inline]
  fn is_false_slice(src: &Self::Slice) -> bool {
    src.eq(&b"false"[..])
  }

  #[inline]
  fn is_null_slice(src: &Self::Slice) -> bool {
    src.eq(&b"null"[..])
  }
}

const _: () = {
  const fn assert_impls_source<'src, T: Source<'src>>()
  where
    T::Token: Char + 'src,
  {
  }

  assert_impls_source::<&'static str>();
  assert_impls_source::<&'static [u8]>();
  assert_impls_source::<&'static [u8; 100]>();

  #[cfg(feature = "bytes")]
  assert_impls_source::<bytes::Bytes>();
};
