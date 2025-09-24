/// Defines the punctuators.
///
/// # Examples
/// ```rust
/// use smear_parser::punctuator;
///
/// punctuator! {
///   (LAngle, "L_ANGLE", "<"),
///   (RAngle, "R_ANGLE", ">"),
/// }
/// ```
#[macro_export]
macro_rules! punctuator {
  ($(($name:ident, $syntax_tree_display: literal, $punct:literal)),+$(,)?) => {
    paste::paste! {
      $(
        #[doc = "The `" $punct "` punctuator"]
        #[derive(::core::fmt::Debug, ::core::clone::Clone, ::core::marker::Copy, ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::hash::Hash)]
        pub struct $name {
          span: $crate::__private::logosky::utils::Span,
        }

        impl $name {
          /// Creates a new punctuator with the given span.
          #[inline(always)]
          pub const fn new(span: $crate::__private::logosky::utils::Span) -> Self {
            Self { span }
          }

          #[doc = "Returns the raw string literal of the `" $punct "` punctuator."]
          #[inline]
          pub const fn raw() -> &'static ::core::primitive::str {
            $punct
          }

          #[doc = "Returns the raw string literal of the `" $punct "` punctuator."]
          #[inline]
          pub const fn as_str(&self) -> &'static ::core::primitive::str {
            Self::raw()
          }

          #[doc = "Returns the span of the `" $punct "` punctuator."]
          #[inline]
          pub const fn span(&self) -> &$crate::__private::logosky::utils::Span {
            &self.span
          }
        }

        impl ::core::cmp::PartialEq<::core::primitive::str> for $name {
          #[inline]
          fn eq(&self, other: &::core::primitive::str) -> bool {
            self.as_str().eq(other)
          }
        }

        impl ::core::cmp::PartialOrd<::core::primitive::str> for $name {
          #[inline]
          fn partial_cmp(&self, other: &::core::primitive::str) -> ::core::option::Option<::core::cmp::Ordering> {
            self.as_str().partial_cmp(other)
          }
        }

        impl ::core::cmp::PartialEq<$name> for ::core::primitive::str {
          #[inline]
          fn eq(&self, other: &$name) -> bool {
            self.eq(other.as_str())
          }
        }

        impl ::core::cmp::PartialOrd<$name> for ::core::primitive::str {
          #[inline]
          fn partial_cmp(&self, other: &$name) -> ::core::option::Option<::core::cmp::Ordering> {
            self.partial_cmp(other.as_str())
          }
        }

        impl ::core::borrow::Borrow<::core::primitive::str> for $name {
          #[inline]
          fn borrow(&self) -> &::core::primitive::str {
            self.as_str()
          }
        }

        impl ::core::convert::AsRef<::core::primitive::str> for $name {
          #[inline]
          fn as_ref(&self) -> &::core::primitive::str {
            self.as_str()
          }
        }

        impl ::core::convert::AsRef<$crate::__private::logosky::utils::Span> for $name {
          #[inline]
          fn as_ref(&self) -> &$crate::__private::logosky::utils::Span {
            self.span()
          }
        }

        impl $crate::__private::IntoSpan<$crate::__private::logosky::utils::Span> for $name {
          #[inline]
          fn into_span(self) -> $crate::__private::logosky::utils::Span {
            self.span
          }
        }

        impl $crate::__private::IntoComponents for $name {
          type Components = $crate::__private::logosky::utils::Span;

          #[inline]
          fn into_components(self) -> Self::Components {
            <Self as $crate::__private::IntoSpan<$crate::__private::logosky::utils::Span>>::into_span(self)
          }
        }

        impl ::core::fmt::Display for $name {
          #[inline(always)]
          fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
            write!(f, $punct)
          }
        }

        impl $crate::__private::logosky::utils::human_display::DisplayHuman for $name {
          #[inline]
          fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
            ::core::fmt::Display::fmt(self, f)
          }
        }

        impl $crate::__private::logosky::utils::sdl_display::DisplaySDL for $name {
          #[inline]
          fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
            ::core::fmt::Display::fmt(self, f)
          }
        }

        impl $crate::__private::logosky::utils::syntax_tree_display::DisplaySyntaxTree for $name {
          #[inline]
          fn fmt(
            &self,
            level: ::core::primitive::usize,
            indent: ::core::primitive::usize,
            f: &mut ::core::fmt::Formatter<'_>,
          ) -> ::core::fmt::Result {
            let padding = level * indent;
            ::core::write!(f, "{:indent$}", "", indent = padding)?;
            ::core::writeln!(f, ::core::concat!("- ", $syntax_tree_display, "@{}..{} \"", $punct, "\""), self.span().start(), self.span().end())
          }
        }
      )*
    }
  };
}

punctuator!(
  (At, "AT", "@"),
  (Ampersand, "AMPERSAND", "&"),
  (Bang, "BANG", "!"),
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
  (LParen, "L_PAREN", "("),
  (RParen, "R_PAREN", ")"),
  (LAngle, "L_ANGLE", "<"),
  (RAngle, "R_ANGLE", ">"),
);
