/// Defines the punctuators.
///
/// # Examples
/// ```rust
/// use smear_lexer::punctuator;
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
        pub struct $name<C = ()> {
          span: $crate::__private::logosky::utils::Span,
          source: C,
        }

        impl $name {
          /// Creates a new punctuator with the given span.
          #[inline(always)]
          pub const fn new(span: $crate::__private::logosky::utils::Span) -> Self {
            Self { span, source: () }
          }
        }

        impl<C> $name<C> {
          #[doc = "Creates a new punctuator with the given span and source."]
          #[inline(always)]
          pub const fn with_source(span: $crate::__private::logosky::utils::Span, source: C) -> Self {
            Self { span, source }
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

          #[doc = "Returns a reference to the source of the `" $punct "` punctuator."]
          #[inline]
          pub const fn source(&self) -> &C {
            &self.source
          }
        }

        impl<S> ::core::cmp::PartialEq<::core::primitive::str> for $name<S> {
          #[inline]
          fn eq(&self, other: &::core::primitive::str) -> bool {
            self.as_str().eq(other)
          }
        }

        impl<S> ::core::cmp::PartialOrd<::core::primitive::str> for $name<S> {
          #[inline]
          fn partial_cmp(&self, other: &::core::primitive::str) -> ::core::option::Option<::core::cmp::Ordering> {
            self.as_str().partial_cmp(other)
          }
        }

        impl<S> ::core::cmp::PartialEq<$name<S>> for ::core::primitive::str {
          #[inline]
          fn eq(&self, other: &$name<S>) -> bool {
            self.eq(other.as_str())
          }
        }

        impl<S> ::core::cmp::PartialOrd<$name<S>> for ::core::primitive::str {
          #[inline]
          fn partial_cmp(&self, other: &$name<S>) -> ::core::option::Option<::core::cmp::Ordering> {
            self.partial_cmp(other.as_str())
          }
        }

        impl<S> ::core::borrow::Borrow<::core::primitive::str> for $name<S> {
          #[inline]
          fn borrow(&self) -> &::core::primitive::str {
            self.as_str()
          }
        }

        impl<S> ::core::convert::AsRef<::core::primitive::str> for $name<S> {
          #[inline]
          fn as_ref(&self) -> &::core::primitive::str {
            self.as_str()
          }
        }

        impl<S> $crate::__private::logosky::utils::AsSpan<$crate::__private::logosky::utils::Span> for $name<S> {
          #[inline]
          fn as_span(&self) -> &$crate::__private::logosky::utils::Span {
            self.span()
          }
        }

        impl<S> $crate::__private::logosky::utils::IntoSpan<$crate::__private::logosky::utils::Span> for $name<S> {
          #[inline]
          fn into_span(self) -> $crate::__private::logosky::utils::Span {
            self.span
          }
        }

        impl<S> $crate::__private::logosky::utils::IntoComponents for $name<S> {
          type Components = ($crate::__private::logosky::utils::Span, S);

          #[inline]
          fn into_components(self) -> Self::Components {
            (self.span, self.source)
          }
        }

        impl<S> ::core::fmt::Display for $name<S> {
          #[inline(always)]
          fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
            ::core::fmt::Display::fmt($punct, f)
          }
        }

        impl<S> $crate::__private::logosky::utils::human_display::DisplayHuman for $name<S> {
          #[inline]
          fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
            ::core::fmt::Display::fmt(self, f)
          }
        }

        impl<S> $crate::__private::logosky::utils::sdl_display::DisplayCompact for $name<S> {
          type Options = ();

          #[inline]
          fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>, _: &Self::Options) -> ::core::fmt::Result {
            ::core::fmt::Display::fmt(self, f)
          }
        }

        impl<S> $crate::__private::logosky::utils::sdl_display::DisplayPretty for $name<S> {
          type Options = ();

          #[inline]
          fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>, _: &Self::Options) -> ::core::fmt::Result {
            ::core::fmt::Display::fmt(self, f)
          }
        }
      )*
    }
  };
}

punctuator!(
  (At, "AT", "@"),
  (Ampersand, "AMPERSAND", "&"),
  (Asterisk, "ASTERISK", "*"),
  (Bang, "BANG", "!"),
  (Comma, "COMMA", ","),
  (Colon, "COLON", ":"),
  (Dollar, "DOLAR", "$"),
  (Equal, "EQUAL", "="),
  (Pipe, "PIPE", "|"),
  (Spread, "SPREAD", "..."),
  (LBracket, "L_BRACKET", "["),
  (RBracket, "R_BRACKET", "]"),
  (LBrace, "L_BRACE", "{"),
  (RBrace, "R_BRACE", "}"),
  (LParen, "L_PAREN", "("),
  (RParen, "R_PAREN", ")"),
  (LAngle, "L_ANGLE", "<"),
  (RAngle, "R_ANGLE", ">"),
  (FatArrow, "FAT_ARROW", "=>"),
  (ThinArrow, "THIN_ARROW", "->"),
  (PathSeparator, "PATH_SEPARATOR", "::"),
);
