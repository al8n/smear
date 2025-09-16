use logosky::utils::{Span, human_display::DisplayHuman, syntax_tree_display::DisplaySyntaxTree};

macro_rules! punctuator {
  ($(($name:ident, $syntax_tree_display: literal, $punct:literal)),+$(,)?) => {
    paste::paste! {
      $(
        #[doc = $name " punctuator"]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $name(Span);

        impl $name {
          /// Creates a new punctuator.
          #[inline(always)]
          pub(crate) const fn new(span: Span) -> Self {
            Self(span)
          }

          /// Returns the span of the punctuator.
          #[inline(always)]
          pub const fn span(&self) -> Span {
            self.0
          }
        }

        impl core::fmt::Display for $name {
          #[inline(always)]
          fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            write!(f, $punct)
          }
        }

        impl DisplayHuman for $name {
          #[inline]
          fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            core::fmt::Display::fmt(self, f)
          }
        }

        impl DisplaySyntaxTree for $name {
          #[inline]
          fn fmt(
            &self,
            level: usize,
            indent: usize,
            f: &mut core::fmt::Formatter<'_>,
          ) -> core::fmt::Result {
            let padding = level * indent;
            write!(f, "{:indent$}", "", indent = padding)?;
            writeln!(f, concat!("- ", $syntax_tree_display, "@{}..{}"), self.span().start(), self.span().end())
          }
        }
      )*
    }
  };
}

punctuator!(
  (Comma, "COMMA", ","),
  (Colon, "COLON", ":"),
  (Dollar, "DOLAR", "$"),
  (Equal, "EQUAL", "="),
  (Pipe, "PIPE", "|"),
  (Spread, "SPREAD", "..."),
  (LBracket, "L_BRACKET", "["),
  (RBracket, "R_BRACKET", "]"),
  (LBrace, "L_BRACE", "{{"),
  (RBrace, "R_BRACE", "}}"),
);
