/// Defines the keyword.
///
/// # Examples
/// ```rust
/// use smear::keyword;
///
/// keyword! {
///   (MyKeyword, "MY_KEYWORD", "my_keyword"),
///   (AnotherKeyword, "ANOTHER_KEYWORD", "another_keyword"),
/// }
/// ```
#[macro_export]
macro_rules! keyword {
  ($(
    $(#[$meta:meta])*
    (
      $name:ident, $syntax_tree_display: literal, $kw:literal
    )
  ),+$(,)?) => {
    paste::paste! {
      $(
        #[doc = "The `" $kw "` keyword"]
        $(#[$meta])*
        #[derive(::core::fmt::Debug, ::core::clone::Clone, ::core::marker::Copy, ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::hash::Hash)]
        pub struct $name {
          span: $crate::__private::logosky::utils::Span,
        }

        impl ::core::convert::AsRef<::core::primitive::str> for $name {
          #[inline]
          fn as_ref(&self) -> &str {
            $kw
          }
        }

        impl ::core::borrow::Borrow<str> for $name {
          #[inline]
          fn borrow(&self) -> &str {
            ::core::convert::AsRef::<str>::as_ref(self)
          }
        }

        impl $name {
          /// Creates a new keyword.
          #[doc = "Creates a new `" $kw "` keyword."]
          #[inline(always)]
          pub const fn new(span: $crate::__private::logosky::utils::Span) -> Self {
            Self { span }
          }

          #[doc = "Returns the raw string literal of the `" $kw "` keyword."]
          #[inline]
          pub const fn raw() -> &'static ::core::primitive::str {
            $kw
          }

          #[doc = "Returns the span of the `" $kw "` keyword."]
          #[inline]
          pub const fn span(&self) -> &$crate::__private::logosky::utils::Span {
            &self.span
          }
        }

        impl $crate::__private::logosky::utils::AsSpan<$crate::__private::logosky::utils::Span> for $name {
          #[inline]
          fn as_span(&self) -> &$crate::__private::logosky::utils::Span {
            self.span()
          }
        }

       impl $crate::__private::logosky::utils::IntoSpan<$crate::__private::logosky::utils::Span> for $name {
          #[inline]
          fn into_span(self) -> $crate::__private::logosky::utils::Span {
            self.span
          }
        }

        impl $crate::__private::logosky::utils::IntoComponents for $name {
          type Components = $crate::__private::logosky::utils::Span;

          #[inline]
          fn into_components(self) -> Self::Components {
            <Self as $crate::__private::logosky::utils::IntoSpan<$crate::__private::logosky::utils::Span>>::into_span(self)
          }
        }

        impl ::core::fmt::Display for $name {
          #[inline(always)]
          fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
            write!(f, $kw)
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
            ::core::writeln!(f, ::core::concat!("- ", $syntax_tree_display, "@{}..{}"), self.span().start(), self.span().end())
          }
        }
      )*
    }
  };
}

keyword!(
  (On, "on_KW", "on"),
  (Input, "input_KW", "input"),
  (Interface, "interface_KW", "interface"),
  (Enum, "enum_KW", "enum"),
  (Union, "union_KW", "union"),
  (Scalar, "scalar_KW", "scalar"),
  (Directive, "directive_KW", "directive"),
  (Implements, "implements_KW", "implements"),
  (Extend, "extend_KW", "extend"),
  (Repeatable, "repeatable_KW", "repeatable"),
  (Schema, "schema_KW", "schema"),
  (Type, "type_KW", "type"),
  (Query, "query_KW", "query"),
  (Mutation, "mutation_KW", "mutation"),
  (Subscription, "subscription_KW", "subscription"),
  (Fragment, "fragment_KW", "fragment"),
);
