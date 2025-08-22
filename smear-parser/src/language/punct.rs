macro_rules! punct {
  ($token_name:ident:$token:literal:$tokens: expr) => {
    paste::paste! {
      #[doc = "The `" $token "`" " punctuator."]
      #[derive(Debug, Clone, Copy, PartialEq, Eq)]
      pub struct [< $token_name:upper:camel >]<Src, Span>($crate::__private::Spanned<Src, Span>);

      impl<Src, Span> $crate::__private::AsSpanned<Src, Span> for [< $token_name:upper:camel >]<Src, Span> {
        #[inline]
        fn as_spanned(&self) -> &$crate::__private::Spanned<Src, Span> {
          self.span()
        }
      }

      impl<Src, Span> $crate::__private::IntoSpanned<Src, Span> for [< $token_name:upper:camel >]<Src, Span> {
        #[inline]
        fn into_spanned(self) -> $crate::__private::Spanned<Src, Span> {
          self.0
        }
      }

      impl<Src, Span> $crate::__private::IntoComponents for [< $token_name:upper:camel >]<Src, Span> {
        type Components = $crate::__private::Spanned<Src, Span>;

        #[inline]
        fn into_components(self) -> Self::Components {
          <Self as $crate::__private::IntoSpanned<Src, Span>>::into_spanned(self)
        }
      }

      impl<Src, Span> [< $token_name:upper:camel >]<Src, Span> {
        #[doc = "Returns the raw string literal of the `" $token "` punctuator."]
        #[inline]
        pub const fn raw() -> &'static ::core::primitive::str {
          $token
        }

        #[doc = "Returns the span of the `" $token "` punctuator."]
        #[inline]
        pub const fn span(&self) -> &$crate::__private::Spanned<Src, Span> {
          &self.0
        }

        #[doc = "Returns the parser for the `" $token "` punctuator."]
        pub fn parser<'src, I, E>() -> impl $crate::__private::chumsky::prelude::Parser<'src, I, Self, E> + ::core::clone::Clone
        where
          I: $crate::__private::Source<'src, Slice = Src, Span = Span>,
          I::Token: $crate::__private::Char + 'src,
          E: $crate::__private::chumsky::extra::ParserExtra<'src, I>,
        {
          use $crate::__private::{chumsky::prelude::*, Char};

          just($tokens)
            .map_with(|_, sp| Self($crate::__private::Spanned::from(sp)))
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
