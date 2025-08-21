use super::super::{char::Char, spanned::Spanned};

use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

macro_rules! punct {
  ($token_name:ident:$token:literal:$tokens: expr) => {
    paste::paste! {
      #[doc = "The `" $token "`" " punctuator."]
      #[derive(Debug, Clone, Copy, PartialEq, Eq)]
      pub struct [< $token_name:upper:camel >]<Src, Span>(Spanned<Src, Span>);

      impl<Src, Span> [< $token_name:upper:camel >]<Src, Span> {
        /// Returns the raw string literal of the punctuator.
        #[inline]
        pub const fn raw() -> &'static str {
          $token
        }

        #[doc = "Returns the span of the " $token_name " punctuator."]
        #[inline]
        pub const fn span(&self) -> &Spanned<Src, Span> {
          &self.0
        }

        /// Returns the parser.
        pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
        where
          I: StrInput<'src, Slice = Src, Span = Span>,
          I::Token: Char + 'src,
          E: ParserExtra<'src, I>,
          E::Error:
            LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
        {
          just($tokens)
            .map_with(|_, sp| Self(Spanned::from(sp)))
        }
      }
    }
  };
}

punct!(bang: "!": I::Token::EXCLAMATION);
punct!(dollar: "$": I::Token::DOLLAR);
punct!(ampersand: "&": I::Token::AMPERSAND);
punct!(l_paren: "(": I::Token::PAREN_OPEN);
punct!(r_paren: ")": I::Token::PAREN_CLOSE);
punct!(ellipsis: "...": [I::Token::DOT, I::Token::DOT, I::Token::DOT]);
punct!(colon: ":": I::Token::COLON);
punct!(equal: "=": I::Token::EQUAL);
punct!(at: "@": I::Token::AT);
punct!(l_bracket: "[": I::Token::BRACKET_OPEN);
punct!(r_bracket: "]": I::Token::BRACKET_CLOSE);
punct!(l_brace: "{": I::Token::CURLY_BRACE_OPEN);
punct!(pipe: "|": I::Token::VERTICAL_BAR);
punct!(r_brace: "}": I::Token::CURLY_BRACE_CLOSE);
punct!(l_angle: "<": I::Token::LESS_THAN);
punct!(r_angle: ">": I::Token::GREATER_THAN);
punct!(triple_quote: "\"\"\"": [I::Token::QUOTATION, I::Token::QUOTATION, I::Token::QUOTATION]);
punct!(quote: "\"": I::Token::QUOTATION);
