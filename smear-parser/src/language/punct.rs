macro_rules! punct {
  ($token_name:ident:$token:literal:$tokens: expr) => {
    paste::paste! {
      #[doc = "The `" $token "`" " punctuator."]
      #[derive(Debug, Clone, Copy, PartialEq, Eq)]
      pub struct [< $token_name:upper:camel >]<Span>(Span);

      impl<Span> ::core::convert::AsRef<Span> for [< $token_name:upper:camel >]<Span> {
        #[inline]
        fn as_ref(&self) -> &Span {
          self.span()
        }
      }

      impl<Span> $crate::__private::IntoSpanned<Span> for [< $token_name:upper:camel >]<Span> {
        #[inline]
        fn into_spanned(self) -> Span {
          self.0
        }
      }

      impl<Span> $crate::__private::IntoComponents for [< $token_name:upper:camel >]<Span> {
        type Components = Span;

        #[inline]
        fn into_components(self) -> Self::Components {
          <Self as $crate::__private::IntoSpanned<Span>>::into_spanned(self)
        }
      }

      impl<Span> [< $token_name:upper:camel >]<Span> {
        #[doc = "Returns the raw string literal of the `" $token "` punctuator."]
        #[inline]
        pub const fn raw() -> &'static ::core::primitive::str {
          $token
        }

        #[doc = "Returns the span of the `" $token "` punctuator."]
        #[inline]
        pub const fn span(&self) -> &Span {
          &self.0
        }

        #[doc = "Returns the parser for the `" $token "` punctuator."]
        pub fn parser<'src, I, E>() -> impl $crate::__private::chumsky::prelude::Parser<'src, I, Self, E> + ::core::clone::Clone
        where
          I: $crate::__private::Source<'src>,
          I::Token: $crate::__private::Char + 'src,
          E: $crate::__private::chumsky::extra::ParserExtra<'src, I>,
          Span: $crate::__private::Spanned<'src, I, E>,
        {
          use $crate::__private::{chumsky::prelude::*, Char};

          just($tokens)
            .map_with(|_, sp| Self($crate::__private::Spanned::from_map_extra(sp)))
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
punct!(minus: "-": I::Token::MINUS);
punct!(plus: "+": I::Token::PLUS);
punct!(dot: ".": I::Token::DOT);
