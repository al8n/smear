macro_rules! punct {
  ($token_name:ident:$token:literal) => {
    paste::paste! {
      #[doc = "The `" $token "`" " punctuator."]
      #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
      pub struct [< $token_name:upper:camel >]<S> {
        span: S,
      }

      impl<S> [< $token_name:upper:camel >]<S> {
        #[doc = "Creates a new " $token_name " punctuator."]
        #[inline]
        pub const fn new(span: S) -> Self {
          Self { span }
        }

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
