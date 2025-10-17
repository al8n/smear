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

        impl $crate::__private::logosky::utils::sdl_display::DisplayCompact for $name {
          type Options = ();

          #[inline]
          fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>, _: &Self::Options) -> ::core::fmt::Result {
            ::core::fmt::Display::fmt(self, f)
          }
        }

        impl $crate::__private::logosky::utils::sdl_display::DisplayPretty for $name {
          type Options = ();

          #[inline]
          fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>, _: &Self::Options) -> ::core::fmt::Result {
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

// GraphQLx contextual keywords
keyword! {
  (Import, "import_KW", "import"),
  (As, "as_KW", "as"),
  (From, "from_KW", "from"),
  (Where, "where_KW", "where"),
  (Set, "set_KW", "set"),
  (Map, "map_KW", "map"),
}

// GraphQL Keywords
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

keyword! {
  /// `QUERY` location - directives can be applied to query operations.
  ///
  /// Used when defining where a directive can be placed. Query directives
  /// affect the entire query operation and can be used for things like
  /// authentication, caching, or operation-level configuration.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @auth on QUERY
  /// directive @cache(ttl: 300) on QUERY
  /// ```
  (QueryLocation, "QUERY_KW", "QUERY"),

  /// `MUTATION` location - directives can be applied to mutation operations.
  ///
  /// Mutation directives affect the entire mutation operation and can be used
  /// for authorization, rate limiting, or transaction control.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @rateLimit(max: 10) on MUTATION
  /// directive @requireAuth on MUTATION
  /// ```
  (MutationLocation, "MUTATION_KW", "MUTATION"),

  /// `SUBSCRIPTION` location - directives can be applied to subscription operations.
  ///
  /// Subscription directives control real-time data flow and can be used
  /// for filtering, authentication, or subscription management.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @requireSubscription on SUBSCRIPTION
  /// directive @throttle(rate: "1/sec") on SUBSCRIPTION
  /// ```
  (SubscriptionLocation, "SUBSCRIPTION_KW", "SUBSCRIPTION"),

  /// `FIELD_DEFINITION` location - directives can be applied to field definitions in schemas.
  ///
  /// Field definition directives control field behavior, validation, authorization,
  /// or provide metadata about fields in type definitions.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on FIELD_DEFINITION
  /// directive @auth(requires: Role) on FIELD_DEFINITION
  /// ```
  (FieldDefinitionLocation, "FIELD_DEFINITION_KW", "FIELD_DEFINITION"),

  /// `FIELD` location - directives can be applied to field selections in queries.
  ///
  /// Field directives control individual field selection behavior, commonly
  /// used for conditional inclusion, skipping, or field-level configuration.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @include(if: Boolean!) on FIELD
  /// directive @skip(if: Boolean!) on FIELD
  /// ```
  (FieldLocation, "FIELD_KW", "FIELD"),

  /// `FRAGMENT_DEFINITION` location - directives can be applied to named fragment definitions.
  ///
  /// Fragment definition directives control fragment behavior and can be used
  /// for conditional fragments, caching, or fragment-level metadata.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @experimental on FRAGMENT_DEFINITION
  /// directive @cache(scope: PRIVATE) on FRAGMENT_DEFINITION
  /// ```
  (FragmentDefinitionLocation, "FRAGMENT_DEFINITION_KW", "FRAGMENT_DEFINITION"),

  /// `FRAGMENT_SPREAD` location - directives can be applied to fragment spreads.
  ///
  /// Fragment spread directives control when and how fragments are included
  /// in selection sets, commonly used for conditional fragment inclusion.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @include(if: Boolean!) on FRAGMENT_SPREAD
  /// directive @defer(label: String) on FRAGMENT_SPREAD
  /// ```
  (FragmentSpreadLocation, "FRAGMENT_SPREAD_KW", "FRAGMENT_SPREAD"),
  /// `INLINE_FRAGMENT` location - directives can be applied to inline fragments.
  ///
  /// Inline fragment directives control conditional type-specific field selections
  /// and can be used for conditional inclusion based on type or other criteria.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @include(if: Boolean!) on INLINE_FRAGMENT
  /// directive @skip(if: Boolean!) on INLINE_FRAGMENT
  /// ```
  (InlineFragmentLocation, "INLINE_FRAGMENT_KW", "INLINE_FRAGMENT"),

  /// `VARIABLE_DEFINITION` location - directives can be applied to variable definitions.
  ///
  /// Variable definition directives control variable behavior, validation,
  /// or provide metadata about operation variables.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on VARIABLE_DEFINITION
  /// directive @validate(pattern: String) on VARIABLE_DEFINITION
  /// ```
  (VariableDefinitionLocation, "VARIABLE_DEFINITION_KW", "VARIABLE_DEFINITION"),

  /// `SCHEMA` location - directives can be applied to the schema definition.
  ///
  /// Schema directives provide global schema-level configuration, metadata,
  /// or behavior that applies to the entire GraphQL schema.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @link(url: String!) on SCHEMA
  /// directive @composeDirective(name: String!) on SCHEMA
  /// ```
  (SchemaLocation, "SCHEMA_KW", "SCHEMA"),

  /// `SCALAR` location - directives can be applied to scalar type definitions.
  ///
  /// Scalar directives provide validation, serialization, or metadata
  /// for custom scalar types in the schema.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @specifiedBy(url: String!) on SCALAR
  /// directive @validate(regex: String) on SCALAR
  /// ```
  (ScalarLocation, "SCALAR_KW", "SCALAR"),

  /// `OBJECT` location - directives can be applied to object type definitions.
  ///
  /// Object type directives control object behavior, provide metadata,
  /// or enable features like interfaces, caching, or authorization.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @key(fields: String!) on OBJECT
  /// directive @cacheControl(maxAge: Int) on OBJECT
  /// ```
  (ObjectLocation, "OBJECT_KW", "OBJECT"),

  /// `ARGUMENT_DEFINITION` location - directives can be applied to argument definitions.
  ///
  /// Argument definition directives control argument validation, transformation,
  /// or provide metadata about field and directive arguments.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on ARGUMENT_DEFINITION
  /// directive @constraint(min: Int, max: Int) on ARGUMENT_DEFINITION
  /// ```
  (ArgumentDefinitionLocation, "ARGUMENT_DEFINITION_KW", "ARGUMENT_DEFINITION"),

  /// `INTERFACE` location - directives can be applied to interface type definitions.
  ///
  /// Interface directives control interface behavior, provide metadata,
  /// or enable features for types that implement the interface.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @key(fields: String!) on INTERFACE
  /// directive @auth(requires: Role) on INTERFACE
  /// ```
  (InterfaceLocation, "INTERFACE_KW", "INTERFACE"),

  /// `UNION` location - directives can be applied to union type definitions.
  ///
  /// Union directives control union behavior, type resolution,
  /// or provide metadata for union types.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @unionMember(type: String!) on UNION
  /// directive @deprecated(reason: String) on UNION
  /// ```
  (UnionLocation, "UNION_KW", "UNION"),

  /// `ENUM_VALUE` location - directives can be applied to enum value definitions.
  ///
  /// Enum value directives provide metadata, deprecation information,
  /// or control the behavior of specific enum values.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on ENUM_VALUE
  /// directive @internal on ENUM_VALUE
  /// ```
  (EnumValueLocation, "ENUM_VALUE_KW", "ENUM_VALUE"),

  /// `ENUM` location - directives can be applied to enum type definitions.
  ///
  /// Enum directives control enum behavior, validation,
  /// or provide metadata for the entire enum type.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on ENUM
  /// directive @oneOf on ENUM
  /// ```
  (EnumLocation, "ENUM_KW", "ENUM"),

  /// `INPUT_OBJECT` location - directives can be applied to input object type definitions.
  ///
  /// Input object directives control input validation, transformation,
  /// or provide metadata for input types used in arguments.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @oneOf on INPUT_OBJECT
  /// directive @validate(schema: String) on INPUT_OBJECT
  /// ```
  (InputObjectLocation, "INPUT_OBJECT_KW", "INPUT_OBJECT"),

  /// `INPUT_FIELD_DEFINITION` location - directives can be applied to input field definitions.
  ///
  /// Input field directives control input field validation, transformation,
  /// or provide metadata for fields within input object types.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on INPUT_FIELD_DEFINITION
  /// directive @constraint(min: Int, max: Int) on INPUT_FIELD_DEFINITION
  /// ```
  (InputFieldDefinitionLocation, "INPUT_FIELD_DEFINITION_KW", "INPUT_FIELD_DEFINITION"),
}
