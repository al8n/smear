use chumsky::{label::LabelError, prelude::*};
use smear_parser::lexer::{self, DisplayText, Lexer, LexerError, Span, State, Text};

mod fast_token;
mod full_token;
mod token;

const BOM: &[u8] = b"\xef\xbb\xbf";

/// Float literal lexing
pub mod number;

// /// Returns a parser that produces a stream of tokens.
// pub fn lexer<'a, I, S, C, E>() -> impl Parser<'a, Lexer<'a, I, Token<I>, S>, C, E>
// where
//   S: State,
//   I: Text<'a>,
//   Lexer<'a, I, Token<I>, S>: Input<'a>,
//   Token<I>: smear_parser::lexer::Token<'a, I, S>,
//   E: extra::ParserExtra<'a, Lexer<'a, I, Token<I>, S>>,
//   // E::Error: LabelError<'a, Lexer<'a, I, Token<I>, S>, LexerError<TokenError<I>, S::Error>>,
//   C: chumsky::container::Container<Result<Token<I>, LexerError<TokenError<I>, S::Error>>>,
// {
//   any()
//     .map(|tok| {
//       match tok {
//         Ok(Some(tok)) => Ok(tok),
//         Ok(None) => Err(LexerError::EndOfInput),
//         Err(e) => Err(e),
//       }
//     })
//     .repeated()
//     .collect::<C>()
// }

#[cfg(test)]
mod tests {
  use core::convert::Infallible;

  use smear_parser::{
    // lang::{comment, comment1, comment2, ignored1, name},
    lexer::Lexer,
  };

  // use crate::lexer::full_token::Token;

  use super::*;

  #[test]
  fn t() {
    let input = r#" what
    
    
    
    
    # asdasdasdas
    ,,,,
    "#;

    // let parser = name::<Lexer<&[u8], Token<&[u8], ()>, ()>, extra::Err<Simple<Token<&[u8], ()>>>>()
    //   .padded_by(ignored1());
    // let res = parser
    //   .parse(Lexer::new(input.as_bytes()))
    //   .into_result()
    //   .unwrap();
    // println!("{}", res);

    // println!("input: {}n", core::str::from_utf8(&input.as_bytes()[14..15]).unwrap());
    // let parser = comment::<&str, extra::Err<Rich<char>>>();
    // let res = parser
    //   .parse(input)
    //   .into_result()
    //   .unwrap();
    // println!("{:#?}", res);

    // let parser = any::<
    //   Lexer<'_, &str, Token<&str>, ()>,
    //   extra::Err<Simple<_, <Lexer<'_, &str, Token<&str>, ()> as Input<'_>>::Span>>,
    // >()
    // .try_map(|tok, sp| match tok {
    //   Ok(Some(tok)) => Ok(tok),
    //   Ok(None) => Err(LexerError::EndOfInput),
    //   Err(e) => Err(e),
    // })
    // .repeated()
    // .collect::<Vec<_>>();

    // let res: Result<
    //   Vec<Result<Token<&str>, LexerError<TokenError<&str>, Infallible>>>,
    //   Vec<
    //     Simple<
    //       '_,
    //       Result<Option<Token<&str>>, LexerError<TokenError<&str>, Infallible>>,
    //       SimpleSpan<usize, Context<&str, ()>>,
    //     >,
    //   >,
    // > = parser
    //   .parse(Lexer::<&str, Token<&str>>::new(input))
    //   .into_result();
    // let lexer = lexer::<&str, (), Vec<_>, extra::Err<LexerError<Token<&str>, Infallible>>>();
    // let tokens = lexer.parse(Lexer::<&str, Token<&str>>::new(input));
  }
}
