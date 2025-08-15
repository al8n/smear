use chumsky::span::Span;

macro_rules! punct {
  ($token_name:ident:$token:literal) => {
    paste::paste! {
      #[doc = "Error returned when parsing an " $token_name " punctuator fails."]
      #[derive(Debug, Clone, Copy)]
      pub struct [< Parse $token_name:upper:camel Error >]<S>(S);

      impl<S> [< Parse $token_name:upper:camel Error >] <S> {
        #[doc = "Returns the raw span of the " $token_name " token"]
        #[inline]
        pub const fn span(&self) -> &S {
          &self.0
        }

        #[doc = "Consumes the error and returns the raw span of the " $token_name " token"]
        #[inline]
        pub fn into_span(self) -> S {
          self.0
        }
      }

      impl<S: core::fmt::Display> core::fmt::Display for [< Parse $token_name:upper:camel Error >] <S> {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
          write!(f, concat!("invalid ", stringify!($token_name), " token: {}"), self.0)
        }
      }

      impl<S: core::fmt::Debug + core::fmt::Display> core::error::Error for [< Parse $token_name:upper:camel Error >] <S> {}

      #[doc = "The `" $token "`" " punctuator."]
      #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
      pub struct [< $token_name:upper:camel >]<S> {
        span: S,
      }

      impl<S> [< $token_name:upper:camel >]<S> {
        /// Returns the raw string literal of the punctuator.
        #[inline]
        pub const fn raw() -> &'static str {
          $token
        }

        #[doc = "Returns the span of the " $token_name " punctuator."]
        #[inline]
        pub const fn span(&self) -> &S {
          &self.span
        }
      }

      impl<S> [< $token_name:upper:camel >]<S>
      where
        S: Span,
        S::Context: AsRef<[u8]>,
      {
        #[doc = "Creates a new " $token_name " punctuator."]
        ///
        /// # Panics
        /// - Panics if the span is invalid
        #[inline]
        pub fn new(span: S) -> Self {
          assert_eq!(span.context().as_ref(), $token.as_bytes(), stringify!("invalid " $token_name " tokens"));

          Self { span }
        }

        #[doc = "Tries to create a new " $token_name " punctuator."]
        ///
        /// # Errors
        /// - Returns an error if the span is invalid
        #[inline]
        pub fn try_new(span: S) -> Result<Self, [< Parse $token_name:upper:camel Error >]<S>> {
          if span.context().as_ref() == $token.as_bytes() {
            Ok(Self { span })
          } else {
            Err([< Parse $token_name:upper:camel Error >](span))
          }
        }
      }
    }
  };
}

punct!(bang: "!");
punct!(dollar: "$");
punct!(ampersand: "&");
punct!(l_paren: "(");
punct!(r_paren: ")");
punct!(ellipsis: "...");
punct!(colon: ":");
punct!(equal: "=");
punct!(at: "@");
punct!(l_bracket: "[");
punct!(r_bracket: "]");
punct!(l_brace: "{");
punct!(pipe: "|");
punct!(r_brace: "}");

punct!(l_angle: "<");
punct!(r_angle: ">");

punct!(triple_quote: "\"\"\"");
punct!(quote: "\"");

/// Punctuator
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Punctuator<S> {
  /// The exclamation mark `!`
  Bang(Bang<S>),
  /// The dollar sign `$`
  Dollar(Dollar<S>),
  /// The ampersand `&`
  Ampersand(Ampersand<S>),
  /// The left parenthesis `(`
  LParen(LParen<S>),
  /// The right parenthesis `)`
  RParen(RParen<S>),
  /// The ellipsis `...`
  Ellipsis(Ellipsis<S>),
  /// The colon `:`
  Colon(Colon<S>),
  /// The equal sign `=`
  Equal(Equal<S>),
  /// The at sign `@`
  At(At<S>),
  /// The left bracket `[`
  LBracket(LBracket<S>),
  /// The right bracket `]`
  RBracket(RBracket<S>),
  /// The left brace `{`
  LBrace(LBrace<S>),
  /// The pipe `|`
  Pipe(Pipe<S>),
  /// The right brace `}`
  RBrace(RBrace<S>),

  /// The left angle bracket `<`
  LAngle(LAngle<S>),
  /// The right angle bracket `>`
  RAngle(RAngle<S>),
  /// The triple quote `"""``
  TripleQuote(TripleQuote<S>),
  /// The quote `"`
  Quote(Quote<S>),
}

#[test]
fn t() {}
