// use derive_more::{From, Into, IsVariant, Unwrap, TryUnwrap, Deref, DerefMut, AsMut, AsRef};
// use logosky::{Lexed, Token, TokenStream, Tokenizer, utils::Span};
// use chumsky::{DefaultExpected, error::{self, LabelError}, util::MaybeRef};

// use crate::error::Errors;

// #[derive(Debug, Clone)]
// pub enum LexerErrorData<'a, T>
// where
//   T: Tokenizer<'a>,
//   T::Token: Token<'a>,
// {
//   Lex(Errors<<T::Token as Token<'a>>::Char, <T::State as logosky::State>::Error>),
//   UnexpectedToken {
//     found: T::Token,
//     expected: <T::Token as Token<'a>>::Kind,
//   },
// }

// #[derive(Debug, Clone)]
// pub struct Error<'a, T>
// where
//   T: Tokenizer<'a>,
//   T::Token: Token<'a>,
// {
//   span: Span,
//   data: LexerErrorData<'a, T>,
// }

// impl<'a, T> From<DefaultExpected<'a, Lexed<'a, T::Token>>> for Error<'a, T>
// where
//   T: Tokenizer<'a>,
//   T::Token: Token<'a>,
// {
//   fn from(value: DefaultExpected<'a, Lexed<'a, T::Token>>) -> Self {
//     match value {
//       DefaultExpected::Token(maybe) => todo!(),
//       DefaultExpected::Any => todo!(),
//       DefaultExpected::SomethingElse => todo!(),
//       DefaultExpected::EndOfInput => todo!(),
//       _ => todo!(),
//     }
//   }
// }

// impl<'a, T> Error<'a, T>
// where
//   T: Tokenizer<'a>,
//   T::Token: Token<'a>,
// {
//   /// Creates an unexpected token error.
//   #[inline]
//   pub const fn unexpected_token(
//     found: T::Token,
//     expected: <T::Token as Token<'a>>::Kind,
//     span: Span,
//   ) -> Self {
//     Self {
//       span,
//       data: LexerErrorData::UnexpectedToken { found, expected },
//     }
//   }
// }


// #[cfg(feature = "smallvec")]
// type DefaultErrorsContainer<'a, T> = smallvec::SmallVec<[Error<'a, T>; 1]>;

// #[cfg(not(feature = "smallvec"))]
// type DefaultErrorsContainer<'a, T> = std::vec::Vec<Error<'a, T>>;

// /// A container for storing multiple lexer errors.
// #[derive(Debug, Clone, From, Into, Deref, DerefMut, AsMut, AsRef)]
// pub struct Errors<'a, T>(DefaultErrorsContainer<'a, T>) where
//   T: Tokenizer<'a>,
//   T::Token: Token<'a>;

// impl<'a, T> Default for Errors<'a, T>
// where
//   T: Tokenizer<'a>,
//   T::Token: Token<'a>,
// {
//   #[inline(always)]
//   fn default() -> Self {
//     Self(DefaultErrorsContainer::default())
//   }
// }

// impl<'a, T> From<Error<'a, T>> for Errors<'a, T>
// where
//   T: Tokenizer<'a>,
//   T::Token: Token<'a>,
// {
//   #[inline]
//   fn from(error: Error<'a, T>) -> Self {
//     Self(core::iter::once(error).collect())
//   }
// }

// impl<'a, T> Errors<'a, T>
// where
//   T: Tokenizer<'a>,
//   T::Token: Token<'a>,
// {
//   /// Create a new empty errors container with given capacity.
//   #[inline]
//   pub fn with_capacity(capacity: usize) -> Self {
//     Self(DefaultErrorsContainer::with_capacity(capacity))
//   }
// }

// impl<'a, T> LabelError<'a, TokenStream<'a, T>, DefaultExpected<'a, Lexed<'a, T>>> for Errors<'a, TokenStream<'a, T>>
// where
//   T: Token<'a>,
//   T::Extras: Copy,
// {
//   fn expected_found<E: IntoIterator<Item = DefaultExpected<'a, Lexed<'a, T>>>>(
//     expected: E,
//     found: Option<MaybeRef<'a, Lexed<'a, T>>>,
//     span: logosky::Span<<T as logos::Logos<'a>>::Extras>,
//   ) -> Self {
//     for exp in expected {
      
//     }
//   }

//   fn merge_expected_found<E: IntoIterator<Item = DefaultExpected<'a, Lexed<'a, T>>>>(
//     self,
//     expected: E,
//     found: Option<MaybeRef<'a, Lexed<'a, T>>>,
//     span: logosky::Span<<T as logos::Logos<'a>>::Extras>,
//   ) -> Self
//   where
//     Self: error::Error<'a, TokenStream<'a, T>>,
//   {
//     todo!() 
//   }
// }
