use chumsky::{extra::ParserExtra, prelude::*};
use derive_more::{AsMut, AsRef, Deref, From, Into, IsVariant, TryUnwrap, Unwrap};

use smear_parser::{
  convert::*,
  definitions,
  lang::{self, *},
  source::{self, Char, Slice, Source},
};

pub use smear_parser::{
  definitions::{
    DirectiveLocation, EnumTypeExtensionContent, ExecutableDirectiveLocation, ImplementInterface,
    ImplementInterfaces, InputObjectTypeExtensionContent, InterfaceTypeExtensionContent,
    LeadingDirectiveLocation, LeadingImplementInterface, LeadingUnionMemberType, ListType,
    Location, NamedType, ObjectTypeExtensionContent, OperationType, SchemaExtensionContent, Type,
    TypeSystemDirectiveLocation, UnionMemberType, UnionMemberTypes, UnionTypeExtensionContent,
  },
  lang::{
    BooleanValue, EnumValue, Exponent, ExponentIdentifier, ExponentSign, FloatValue, Fractional,
    FragmentName, IntValue, Name, NullValue, StringContent, StringDelimiter, StringValue,
    UintValue, Variable,
  },
};

macro_rules! newtype {
  (
    $(#[$meta:meta])*
    struct $outer:ident($inner:ty $(,)?) {
      $(#[$parser_meta:meta])*
      parser: $parser:expr

      $(, remaining: {
        $($item:item)*
      })?
    }
  ) => {
    $(#[$meta:meta])*
    #[derive(Debug, Clone, From, Into, AsMut, AsRef, Deref)]
    #[repr(transparent)]
    pub struct $outer<Span>($inner);

    impl<Span> core::borrow::Borrow<$inner> for $outer<Span> {
      #[inline]
      fn borrow(&self) -> &$inner {
        self
      }
    }

    impl<Span> AsRef<Span> for $outer<Span> {
      #[inline]
      fn as_ref(&self) -> &Span {
        self.span()
      }
    }

    impl<Span> IntoSpan<Span> for $outer<Span> {
      #[inline]
      fn into_span(self) -> Span {
        self.0.into_span()
      }
    }

    impl<Span> IntoComponents for $outer<Span> {
      type Components = <$inner as IntoComponents>::Components;

      #[inline]
      fn into_components(self) -> Self::Components {
        self.0.into_components()
      }
    }

    impl<Span> $outer<Span> {
      $(#[$parser_meta])*
      pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
      where
        I: Source<'src>,
        I::Token: Char + 'src,
        I::Slice: Slice<Token = I::Token>,
        E: ParserExtra<'src, I>,
        Span: source::Span<'src, I, E>,
      {
        $parser
      }

      $($($item)*)?
    }
  };
}

newtype!(struct ObjectField(lang::ObjectField<InputValue<Span>, Span>) {
  parser: {
    lang::ObjectField::parser_with(InputValue::parser()).map(Self)
  }
});

newtype!(struct ConstObjectField(lang::ObjectField<ConstInputValue<Span>, Span>) {
  parser: {
    lang::ObjectField::parser_with(ConstInputValue::parser()).map(Self)
  }
});

newtype!(struct Object(lang::Object<ObjectField<Span>, Span>) {
  parser: {
    lang::Object::parser_with(ObjectField::parser()).map(Self)
  },
  remaining: {
    #[inline]
    pub const fn fields(&self) -> &[ObjectField<Span>] {
      self.0.fields().as_slice()
    }
  }
});

newtype!(struct ConstObject(lang::Object<ConstObjectField<Span>, Span>) {
  parser: {
    lang::Object::parser_with(ConstObjectField::parser()).map(Self)
  },
  remaining: {
    #[inline]
    pub const fn fields(&self) -> &[ConstObjectField<Span>] {
      self.0.fields().as_slice()
    }
  }
});

newtype!(struct List(lang::List<InputValue<Span>, Span>) {
  parser: {
    lang::List::parser_with(InputValue::parser()).map(Self)
  },
  remaining: {
    #[inline]
    pub const fn values(&self) -> &[InputValue<Span>] {
      self.0.values().as_slice()
    }
  }
});

newtype!(struct ConstList(lang::List<ConstInputValue<Span>, Span>) {
  parser: {
    lang::List::parser_with(ConstInputValue::parser()).map(Self)
  },
  remaining: {
    #[inline]
    pub const fn values(&self) -> &[ConstInputValue<Span>] {
      self.0.values().as_slice()
    }
  }
});

/// Input value
///
/// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
///
/// About the generics
/// - `Src`: the corresponding slice of the [`StrInput`](chumsky::input::StrInput)
/// - `Span`: the span type of the input [`StrInput`](chumsky::input::StrInput)
/// - `Container`: the container type for storing the nested type, e.g. list, object, map, set.
#[derive(Debug, Clone, Unwrap, TryUnwrap, IsVariant)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum InputValue<Span> {
  /// Spec: [Variable Value](https://spec.graphql.org/draft/#sec-Variable-Value)
  Variable(Variable<Span>),
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
  ///
  /// Instead of giving a type of number, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Int(IntValue<Span>),
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  ///
  /// Instead of giving a type of float, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Float(FloatValue<Span>),
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  Boolean(BooleanValue<Span>),
  /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
  String(StringValue<Span>),
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  Null(NullValue<Span>),
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  Enum(EnumValue<Span>),
  /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
  List(List<Span>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(Object<Span>),
}

impl<Span> InputValue<Span> {
  /// Returns the span of the input value.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Variable(value) => value.span(),
      Self::Int(value) => value.span(),
      Self::Float(value) => value.span(),
      Self::Boolean(value) => value.span(),
      Self::String(value) => value.span(),
      Self::Null(value) => value.span(),
      Self::Enum(value) => value.span(),
      Self::List(value) => value.0.span(),
      Self::Object(value) => value.0.span(),
    }
  }

  /// Returns a parser for the input value.
  ///
  /// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    recursive(|value| {
      let boolean_value_parser = BooleanValue::parser::<I, E>().map(|v| Self::Boolean(v));
      let null_value_parser = NullValue::parser::<I, E>().map(|v| Self::Null(v));
      let int_value_parser = IntValue::parser::<I, E>().map(|v| Self::Int(v));
      let float_value_parser = FloatValue::parser::<I, E>().map(|v| Self::Float(v));
      let string_value_parser = StringValue::parser::<I, E>().map(|v| Self::String(v));
      let enum_value_parser = EnumValue::parser::<I, E>().map(|v| Self::Enum(v));
      let variable_value_parser = Variable::parser::<I, E>().map(|v| Self::Variable(v));
      let object_value_parser = lang::Object::parser_with::<I, E, _>(
        lang::ObjectField::parser_with(value.clone()).map(ObjectField),
      )
      .map(|v| Self::Object(v.into()));
      let list_value_parser =
        lang::List::parser_with::<I, E, _>(value.clone()).map(|v| Self::List(v.into()));

      choice((
        boolean_value_parser,
        null_value_parser,
        enum_value_parser,
        variable_value_parser,
        string_value_parser,
        float_value_parser,
        int_value_parser,
        list_value_parser,
        object_value_parser,
      ))
    })
  }
}

/// Input value
///
/// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
///
/// About the generics
/// - `Src`: the corresponding slice of the [`StrInput`](chumsky::input::StrInput)
/// - `Span`: the span type of the input [`StrInput`](chumsky::input::StrInput)
/// - `Container`: the container type for storing the nested type, e.g. list, object, map, set.
#[derive(Debug, Clone, Unwrap, TryUnwrap, IsVariant)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum ConstInputValue<Span> {
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
  ///
  /// Instead of giving a type of number, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Int(IntValue<Span>),
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  ///
  /// Instead of giving a type of float, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Float(FloatValue<Span>),
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  Boolean(BooleanValue<Span>),
  /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
  String(StringValue<Span>),
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  Null(NullValue<Span>),
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  Enum(EnumValue<Span>),
  /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
  List(ConstList<Span>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(ConstObject<Span>),
}

impl<Span> ConstInputValue<Span> {
  /// Returns the span of the input value.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Int(value) => value.span(),
      Self::Float(value) => value.span(),
      Self::Boolean(value) => value.span(),
      Self::String(value) => value.span(),
      Self::Null(value) => value.span(),
      Self::Enum(value) => value.span(),
      Self::List(value) => value.0.span(),
      Self::Object(value) => value.0.span(),
    }
  }

  /// Returns a parser for the input value.
  ///
  /// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    recursive(|value| {
      // scalars (whatever you already have)
      let boolean_value_parser = BooleanValue::parser::<I, E>().map(|v| Self::Boolean(v));
      let null_value_parser = NullValue::parser::<I, E>().map(|v| Self::Null(v));
      let int_value_parser = IntValue::parser::<I, E>().map(|v| Self::Int(v));
      let float_value_parser = FloatValue::parser::<I, E>().map(|v| Self::Float(v));
      let string_value_parser = StringValue::parser::<I, E>().map(|v| Self::String(v));
      let enum_value_parser = EnumValue::parser::<I, E>().map(|v| Self::Enum(v));

      let object_value_parser = lang::Object::parser_with::<I, E, _>(
        lang::ObjectField::parser_with(value.clone()).map(ConstObjectField),
      )
      .map(|v| Self::Object(v.into()));
      let list_value_parser =
        lang::List::parser_with::<I, E, _>(value.clone()).map(|v| Self::List(v.into()));

      choice((
        boolean_value_parser,
        null_value_parser,
        float_value_parser,
        int_value_parser,
        string_value_parser,
        enum_value_parser,
        list_value_parser,
        object_value_parser,
      ))
    })
  }
}

newtype!(struct DefaultInputValue(lang::DefaultInputValue<ConstInputValue<Span>, Span>) {
  parser: {
    lang::DefaultInputValue::parser_with(ConstInputValue::parser()).map(Self)
  }
});

newtype!(struct Argument(lang::Argument<InputValue<Span>, Span>) {
  parser: {
    lang::Argument::parser_with(InputValue::parser()).map(|arg| Self(arg))
  }
});

newtype!(struct Arguments(lang::Arguments<Argument<Span>, Span>) {
  parser: {
    lang::Arguments::parser_with(Argument::parser()).map(Self)
  },
  remaining: {
    /// Returns the list of arguments
    #[inline]
    pub const fn arguments(&self) -> &[Argument<Span>] {
      self.0.arguments().as_slice()
    }
  }
});

newtype!(struct ConstArgument(lang::Argument<ConstInputValue<Span>, Span>) {
  parser: {
    lang::Argument::parser_with(ConstInputValue::parser()).map(Self)
  }
});

newtype!(struct ConstArguments(lang::Arguments<ConstArgument<Span>, Span>) {
  parser: {
    lang::Arguments::parser_with(ConstArgument::parser()).map(Self)
  },
  remaining: {
    /// Returns the list of arguments
    #[inline]
    pub const fn arguments(&self) -> &[ConstArgument<Span>] {
      self.0.arguments().as_slice()
    }
  }
});

newtype!(struct Directive(lang::Directive<Arguments<Span>, Span>) {
  parser: {
    lang::Directive::parser_with(Arguments::parser()).map(Self)
  }
});

newtype!(struct Directives(lang::Directives<Directive<Span>, Span>) {
  parser: {
    lang::Directives::parser_with(Directive::parser()).map(Self)
  },
  remaining: {
    #[inline]
    pub const fn directives(&self) -> &[Directive<Span>] {
      self.0.directives().as_slice()
    }

    #[inline]
    pub fn into_directives(self) -> Vec<Directive<Span>> {
      self.0.into_directives()
    }
  }
});

newtype!(struct ConstDirective(lang::Directive<ConstArguments<Span>, Span>) {
  parser: {
    lang::Directive::parser_with(ConstArguments::parser()).map(Self)
  }
});

newtype!(struct ConstDirectives(lang::Directives<ConstDirective<Span>, Span>) {
  parser: {
    lang::Directives::parser_with(ConstDirective::parser()).map(Self)
  },
  remaining: {
    #[inline]
    pub const fn directives(&self) -> &[ConstDirective<Span>] {
      self.0.directives().as_slice()
    }

    #[inline]
    pub fn into_directives(self) -> Vec<ConstDirective<Span>> {
      self.0.into_directives()
    }
  }
});

newtype!(struct FragmentSpread(lang::FragmentSpread<Directives<Span>, Span>) {
  parser: {
    lang::FragmentSpread::parser_with(Directives::parser()).map(Self)
  }
});

newtype!(struct InlineFragment(lang::InlineFragment<Directives<Span>, SelectionSet<Span>, Span>) {
  parser: {
    lang::InlineFragment::parser_with(Directives::parser(), SelectionSet::parser()).map(Self)
  }
});

newtype!(struct FragmentDefinition(definitions::FragmentDefinition<Directives<Span>, SelectionSet<Span>, Span>) {
  parser: {
    definitions::FragmentDefinition::parser_with(Directives::parser(), SelectionSet::parser())
      .map(Self)
  }
});

newtype!(struct Field(lang::Field<Arguments<Span>, Directives<Span>, SelectionSet<Span>, Span>) {
  parser: {
    recursive(|field_parser| {
      // Inner fixpoint: build a `Selection<Span>` parser by using the recursive `field_parser`.
      let selection = recursive(|selection| {
        // SelectionSet needs a `Selection` parser
        let selection_set =
          lang::SelectionSet::parser_with(selection.clone()).map(SelectionSet::<Span>);

        let spread = lang::FragmentSpread::parser_with(Directives::parser())
          .map(|fs| Selection::<Span>::FragmentSpread(FragmentSpread::<Span>(fs)));

        let inline = lang::InlineFragment::parser_with(Directives::parser(), selection_set.clone())
          .map(|f| Selection::<Span>::InlineFragment(InlineFragment::<Span>(f)));

        choice((field_parser.map(Selection::<Span>::Field), spread, inline))
      });

      // Pass the selection parser to the selection set
      let selection_set = lang::SelectionSet::parser_with(selection).map(SelectionSet::<Span>);

      lang::Field::parser_with(Arguments::parser(), Directives::parser(), selection_set).map(Self)
    })
  }
});

#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Selection<Span> {
  Field(Field<Span>),
  FragmentSpread(FragmentSpread<Span>),
  InlineFragment(InlineFragment<Span>),
}

impl<Span> AsRef<Span> for Selection<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for Selection<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Field(f) => f.into_span(),
      Self::FragmentSpread(fs) => fs.into_span(),
      Self::InlineFragment(ifr) => ifr.into_span(),
    }
  }
}

impl<Span> Selection<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Field(f) => f.0.span(),
      Self::FragmentSpread(fs) => fs.0.span(),
      Self::InlineFragment(ifr) => ifr.0.span(),
    }
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    recursive(|selection| {
      let selsetion_set =
        lang::SelectionSet::parser_with(selection.clone()).map(SelectionSet::<Span>);

      let field_p = lang::Field::parser_with(
        Arguments::parser(),
        Directives::parser(),
        selsetion_set.clone(),
      )
      .map(Field::<Span>);

      let inline_p = lang::InlineFragment::parser_with(Directives::parser(), selsetion_set.clone())
        .map(InlineFragment::<Span>);

      let spread_p =
        lang::FragmentSpread::parser_with(Directives::parser()).map(FragmentSpread::<Span>);

      choice((
        field_p.map(Self::Field),
        spread_p.map(Self::FragmentSpread),
        inline_p.map(Self::InlineFragment),
      ))
    })
  }
}

newtype!(struct SelectionSet(lang::SelectionSet<Selection<Span>, Span>) {
  parser: {
    recursive(|selection_set| {
      let field_p = lang::Field::parser_with(
        Arguments::parser(),
        Directives::parser(),
        selection_set.clone(),
      )
      .map(Field::<Span>);

      let inline_p = lang::InlineFragment::parser_with(Directives::parser(), selection_set)
        .map(InlineFragment::<Span>);

      let spread_p =
        lang::FragmentSpread::parser_with(Directives::parser()).map(FragmentSpread::<Span>);

      let selection = choice((
        field_p.map(Selection::<Span>::Field),
        spread_p.map(Selection::<Span>::FragmentSpread),
        inline_p.map(Selection::<Span>::InlineFragment),
      ));

      lang::SelectionSet::parser_with(selection).map(Self)
    })
  },
  remaining: {
    #[inline]
    pub const fn selections(&self) -> &[Selection<Span>] {
      self.0.selections().as_slice()
    }

    #[inline]
    pub fn into_selections(self) -> Vec<Selection<Span>> {
      self.0.into_selections()
    }
  }
});

newtype!(struct InputValueDefinition(definitions::InputValueDefinition<
  Type<Span>,
  DefaultInputValue<Span>,
  ConstDirectives<Span>,
  Span,
>) {
  parser: {
    definitions::InputValueDefinition::parser_with(
      Type::parser(),
      DefaultInputValue::parser(),
      ConstDirectives::parser(),
    )
    .map(Self)
  }
});

newtype!(struct ArgumentsDefinition(definitions::ArgumentsDefinition<InputValueDefinition<Span>, Span>) {
  parser: {
    definitions::ArgumentsDefinition::parser_with(InputValueDefinition::parser()).map(Self)
  },
  remaining: {
    #[inline]
    pub const fn input_value_definitions(&self) -> &[InputValueDefinition<Span>] {
      self.0.input_value_definitions().as_slice()
    }

    #[inline]
    pub fn into_input_value_definitions(self) -> Vec<InputValueDefinition<Span>> {
      self.0.into_input_value_definitions()
    }
  }
});

newtype!(struct DirectiveLocations(definitions::DirectiveLocations<Location<Span>, Span>) {
  parser: {
    definitions::DirectiveLocations::parser_with(Location::parser).map(Self)
  }
});

newtype!(struct DirectiveDefinition(
  definitions::DirectiveDefinition<ConstArguments<Span>, DirectiveLocations<Span>, Span>,
) {
  parser: {
    definitions::DirectiveDefinition::parser_with(
      ConstArguments::parser(),
      DirectiveLocations::parser(),
    )
    .map(Self)
  }
});

newtype!(struct EnumValueDefinition(definitions::EnumValueDefinition<ConstDirectives<Span>, Span>) {
  parser: {
    definitions::EnumValueDefinition::parser_with(ConstDirectives::parser()).map(Self)
  }
});

newtype!(struct EnumValuesDefinition(definitions::EnumValuesDefinition<EnumValueDefinition<Span>, Span>) {
  parser: {
    definitions::EnumValuesDefinition::parser_with(EnumValueDefinition::parser()).map(Self)
  },
  remaining: {
    #[inline]
    pub const fn enum_value_definitions(&self) -> &[EnumValueDefinition<Span>] {
      self.0.enum_value_definitions().as_slice()
    }
    #[inline]
    pub fn into_enum_value_definitions(self) -> Vec<EnumValueDefinition<Span>> {
      self.0.into_enum_value_definitions()
    }
  }
});

newtype!(struct EnumTypeDefinition(definitions::EnumTypeDefinition<ConstDirectives<Span>, EnumValuesDefinition<Span>, Span>) {
  parser: {
    definitions::EnumTypeDefinition::parser_with(
      ConstDirectives::parser(),
      EnumValuesDefinition::parser(),
    )
    .map(Self)
  }
});

newtype!(struct EnumTypeExtension(definitions::EnumTypeExtension<ConstDirectives<Span>, EnumValuesDefinition<Span>, Span>) {
  parser: {
    definitions::EnumTypeExtension::parser_with(
      ConstDirectives::parser,
      EnumValuesDefinition::parser,
    )
    .map(Self)
  }
});

newtype!(struct FieldDefinition(
  definitions::FieldDefinition<ConstArguments<Span>, Type<Span>, ConstDirectives<Span>, Span>,
) {
  parser: {
    definitions::FieldDefinition::parser_with(
      ConstArguments::parser(),
      Type::parser(),
      ConstDirectives::parser(),
    )
    .map(Self)
  }
});

newtype!(struct FieldsDefinition(definitions::FieldsDefinition<FieldDefinition<Span>, Span>) {
  parser: {
    definitions::FieldsDefinition::parser_with(FieldDefinition::parser()).map(Self)
  },
  remaining: {
    #[inline]
    pub const fn field_definitions(&self) -> &[FieldDefinition<Span>] {
      self.0.field_definitions().as_slice()
    }

    #[inline]
    pub fn into_field_definitions(self) -> Vec<FieldDefinition<Span>> {
      self.0.into_field_definitions()
    }
  }
});

newtype!(struct InputFieldsDefinition(
  definitions::InputFieldsDefinition<InputValueDefinition<Span>, Span>,
) {
  parser: {
    definitions::InputFieldsDefinition::parser_with(InputValueDefinition::parser()).map(Self)
  },
  remaining: {
    #[inline]
    pub const fn input_value_definitions(&self) -> &[InputValueDefinition<Span>] {
      self.0.input_value_definitions().as_slice()
    }
    #[inline]
    pub fn into_input_value_definitions(self) -> Vec<InputValueDefinition<Span>> {
      self.0.into_input_value_definitions()
    }
  }
});

newtype!(struct InputObjectTypeDefinition(
  definitions::InputObjectTypeDefinition<ConstDirectives<Span>, InputFieldsDefinition<Span>, Span>,
) {
  parser: {
    definitions::InputObjectTypeDefinition::parser_with(
      ConstDirectives::parser(),
      InputFieldsDefinition::parser(),
    )
    .map(Self)
  }
});

newtype!(struct InputObjectTypeExtension(
  definitions::InputObjectTypeExtension<ConstDirectives<Span>, InputFieldsDefinition<Span>, Span>,
) {
  parser: {
    definitions::InputObjectTypeExtension::parser_with(
      ConstDirectives::parser,
      InputFieldsDefinition::parser,
    )
    .map(Self)
  }
});

newtype!(struct InterfaceTypeDefinition(
  definitions::InterfaceTypeDefinition<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
    Span,
  >,
) {
  parser: {
    definitions::InterfaceTypeDefinition::parser_with(
      ImplementInterfaces::parser(),
      ConstDirectives::parser(),
      FieldsDefinition::parser(),
    )
    .map(Self)
  }
});

newtype!(
  struct InterfaceTypeExtension(
    definitions::InterfaceTypeExtension<
      ImplementInterfaces<Span>,
      ConstDirectives<Span>,
      FieldsDefinition<Span>,
      Span,
    >,
  ) {
    parser: {
      definitions::InterfaceTypeExtension::parser_with(
        ImplementInterfaces::parser,
        ConstDirectives::parser,
        FieldsDefinition::parser,
      )
      .map(Self)
    }
  }
);

newtype!(struct ObjectTypeDefinition(
  definitions::ObjectTypeDefinition<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
    Span,
  >,
) {
  parser: {
    definitions::ObjectTypeDefinition::parser_with(
      ImplementInterfaces::parser(),
      ConstDirectives::parser(),
      FieldsDefinition::parser(),
    )
    .map(Self)
  }
});

newtype!(struct ObjectTypeExtension(
  definitions::ObjectTypeExtension<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
    Span,
  >,
) {
  parser: {
    definitions::ObjectTypeExtension::parser_with(
      ImplementInterfaces::parser,
      ConstDirectives::parser,
      FieldsDefinition::parser,
    )
    .map(Self)
  }
});

newtype!(struct ScalarTypeDefinition(
  definitions::ScalarTypeDefinition<ConstDirectives<Span>, Span>,
) {
  parser: {
    definitions::ScalarTypeDefinition::parser_with(ConstDirectives::parser()).map(Self)
  }
});

newtype!(struct ScalarTypeExtension(
  definitions::ScalarTypeExtension<ConstDirectives<Span>, Span>,
) {
  parser: {
    definitions::ScalarTypeExtension::parser_with(ConstDirectives::parser()).map(Self)
  }
});

newtype!(struct UnionTypeDefinition(
  definitions::UnionTypeDefinition<ConstDirectives<Span>, UnionMemberTypes<Span>, Span>,
) {
  parser: {
    definitions::UnionTypeDefinition::parser_with(
      ConstDirectives::parser(),
      UnionMemberTypes::parser(),
    )
    .map(Self)
  }
});

newtype!(struct UnionTypeExtension(
  definitions::UnionTypeExtension<ConstDirectives<Span>, UnionMemberTypes<Span>, Span>,
) {
  parser: {
    definitions::UnionTypeExtension::parser_with(ConstDirectives::parser, UnionMemberTypes::parser)
      .map(Self)
  }
});

newtype!(struct VariableDefinition(
  definitions::VariableDefinition<Type<Span>, Directives<Span>, DefaultInputValue<Span>, Span>,
) {
  parser: {
    definitions::VariableDefinition::parser(
      Type::parser(),
      Directives::parser(),
      DefaultInputValue::parser(),
    )
    .map(Self)
  }
});

newtype!(struct VariablesDefinition(
  definitions::VariablesDefinition<VariableDefinition<Span>, Span>,
) {
  parser: {
    definitions::VariablesDefinition::parser_with(VariableDefinition::parser()).map(Self)
  },
  remaining: {
    #[inline]
    pub const fn variable_definitions(&self) -> &[VariableDefinition<Span>] {
      self.0.variable_definitions().as_slice()
    }

    #[inline]
    pub fn into_variable_definitions(self) -> Vec<VariableDefinition<Span>> {
      self.0.into_variable_definitions()
    }
  }
});

newtype!(struct NamedOperationDefinition(
  definitions::NamedOperationDefinition<
    OperationType<Span>,
    VariablesDefinition<Span>,
    Directives<Span>,
    SelectionSet<Span>,
    Span,
  >,
) {
  parser: {
    definitions::NamedOperationDefinition::parser_with(
      OperationType::parser(),
      VariablesDefinition::parser(),
      Directives::parser(),
      SelectionSet::parser(),
    )
    .map(Self)
  }
});

#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum OperationDefinition<Span> {
  Named(NamedOperationDefinition<Span>),
  Shorthand(SelectionSet<Span>),
}

impl<Span> AsRef<Span> for OperationDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for OperationDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Named(named) => named.into_span(),
      Self::Shorthand(s) => s.into_span(),
    }
  }
}

impl<Span> OperationDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Named(named) => named.0.span(),
      Self::Shorthand(short) => short.0.span(),
    }
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::OperationDefinition::parser_with(
      OperationType::parser(),
      VariablesDefinition::parser(),
      Directives::parser(),
      SelectionSet::parser(),
    )
    .map(|val| match val {
      definitions::OperationDefinition::Named(named) => Self::Named(named.into()),
      definitions::OperationDefinition::Shorthand(short) => Self::Shorthand(short),
    })
  }
}

newtype!(struct RootOperationTypeDefinition(
  definitions::RootOperationTypeDefinition<OperationType<Span>, Span>,
) {
  parser: {
    definitions::RootOperationTypeDefinition::parser_with(OperationType::parser()).map(Self)
  }
});

newtype!(struct RootOperationTypesDefinition(
  definitions::RootOperationTypesDefinition<RootOperationTypeDefinition<Span>, Span>,
) {
  parser: {
    definitions::RootOperationTypesDefinition::parser_with(RootOperationTypeDefinition::parser())
      .map(Self)
  },
  remaining: {
    #[inline]
    pub const fn root_operation_type_definitions(&self) -> &[RootOperationTypeDefinition<Span>] {
      self.0.root_operation_type_definitions().as_slice()
    }

    #[inline]
    pub fn into_root_operation_type_definitions(self) -> Vec<RootOperationTypeDefinition<Span>> {
      self.0.into_root_operation_type_definitions()
    }
  }
});

newtype!(struct SchemaDefinition(
  definitions::SchemaDefinition<ConstDirectives<Span>, RootOperationTypesDefinition<Span>, Span>,
) {
  parser: {
    definitions::SchemaDefinition::parser_with(
      ConstDirectives::parser(),
      RootOperationTypesDefinition::parser(),
    )
    .map(Self)
  }
});

newtype!(struct SchemaExtension(
  definitions::SchemaExtension<ConstDirectives<Span>, RootOperationTypesDefinition<Span>, Span>,
) {
  parser: {
    definitions::SchemaExtension::parser_with(
      ConstDirectives::parser,
      RootOperationTypesDefinition::parser(),
    )
    .map(Self)
  }
});

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeDefinition<Span> {
  Scalar(ScalarTypeDefinition<Span>),
  Enum(EnumTypeDefinition<Span>),
  Union(UnionTypeDefinition<Span>),
  InputObject(InputObjectTypeDefinition<Span>),
  Object(ObjectTypeDefinition<Span>),
  Interface(InterfaceTypeDefinition<Span>),
}

impl<Span> AsRef<Span> for TypeDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for TypeDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(s) => s.into_span(),
      Self::Enum(e) => e.into_span(),
      Self::Union(u) => u.into_span(),
      Self::InputObject(i) => i.into_span(),
      Self::Object(o) => o.into_span(),
      Self::Interface(i) => i.into_span(),
    }
  }
}

impl<Span> TypeDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Scalar(s) => s.0.span(),
      Self::Enum(e) => e.0.span(),
      Self::Union(u) => u.0.span(),
      Self::InputObject(i) => i.0.span(),
      Self::Object(o) => o.0.span(),
      Self::Interface(i) => i.0.span(),
    }
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    choice((
      ScalarTypeDefinition::parser().map(Self::Scalar),
      EnumTypeDefinition::parser().map(Self::Enum),
      UnionTypeDefinition::parser().map(Self::Union),
      InputObjectTypeDefinition::parser().map(Self::InputObject),
      ObjectTypeDefinition::parser().map(Self::Object),
      InterfaceTypeDefinition::parser().map(Self::Interface),
    ))
  }
}

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDefinition<Span> {
  Type(TypeDefinition<Span>),
  Directive(DirectiveDefinition<Span>),
  Schema(SchemaDefinition<Span>),
}

impl<Span> AsRef<Span> for TypeSystemDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    match self {
      Self::Type(t) => t.span(),
      Self::Directive(d) => d.span(),
      Self::Schema(s) => s.span(),
    }
  }
}

impl<Span> IntoSpan<Span> for TypeSystemDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(t) => t.into_span(),
      Self::Directive(d) => d.into_span(),
      Self::Schema(s) => s.into_span(),
    }
  }
}

impl<Span> TypeSystemDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Type(t) => t.span(),
      Self::Directive(d) => d.0.span(),
      Self::Schema(s) => s.0.span(),
    }
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    choice((
      TypeDefinition::parser().map(Self::Type),
      DirectiveDefinition::parser().map(Self::Directive),
      SchemaDefinition::parser().map(Self::Schema),
    ))
  }
}

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeExtension<Span> {
  Scalar(ScalarTypeExtension<Span>),
  Enum(EnumTypeExtension<Span>),
  Union(UnionTypeExtension<Span>),
  InputObject(InputObjectTypeExtension<Span>),
  Object(ObjectTypeExtension<Span>),
  Interface(InterfaceTypeExtension<Span>),
}

impl<Span> AsRef<Span> for TypeExtension<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for TypeExtension<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(s) => s.into_span(),
      Self::Enum(e) => e.into_span(),
      Self::Union(u) => u.into_span(),
      Self::InputObject(i) => i.into_span(),
      Self::Object(o) => o.into_span(),
      Self::Interface(i) => i.into_span(),
    }
  }
}

impl<Span> TypeExtension<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Scalar(s) => s.0.span(),
      Self::Enum(e) => e.0.span(),
      Self::Union(u) => u.0.span(),
      Self::InputObject(i) => i.0.span(),
      Self::Object(o) => o.0.span(),
      Self::Interface(i) => i.0.span(),
    }
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    choice((
      ScalarTypeExtension::parser().map(Self::Scalar),
      EnumTypeExtension::parser().map(Self::Enum),
      UnionTypeExtension::parser().map(Self::Union),
      InputObjectTypeExtension::parser().map(Self::InputObject),
      ObjectTypeExtension::parser().map(Self::Object),
      InterfaceTypeExtension::parser().map(Self::Interface),
    ))
  }
}

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemExtension<Span> {
  Type(TypeExtension<Span>),
  Schema(SchemaExtension<Span>),
}

impl<Span> AsRef<Span> for TypeSystemExtension<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for TypeSystemExtension<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(t) => t.into_span(),
      Self::Schema(s) => s.into_span(),
    }
  }
}

impl<Span> TypeSystemExtension<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Type(t) => t.span(),
      Self::Schema(s) => s.0.span(),
    }
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    choice((
      TypeExtension::parser().map(Self::Type),
      SchemaExtension::parser().map(Self::Schema),
    ))
  }
}

#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystem<Span> {
  Definition(TypeDefinition<Span>),
  Extension(TypeSystemExtension<Span>),
}

impl<Span> AsRef<Span> for TypeSystem<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for TypeSystem<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Extension(e) => e.into_span(),
      Self::Definition(d) => d.into_span(),
    }
  }
}

impl<Span> TypeSystem<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Extension(e) => e.span(),
      Self::Definition(d) => d.span(),
    }
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    choice((
      TypeDefinition::parser().map(Self::Definition),
      TypeSystemExtension::parser().map(Self::Extension),
    ))
  }
}

#[derive(Debug, Clone)]
pub struct Document<Span> {
  span: Span,
  types: Vec<TypeSystem<Span>>,
}

impl<Span> AsRef<[TypeSystem<Span>]> for Document<Span> {
  #[inline]
  fn as_ref(&self) -> &[TypeSystem<Span>] {
    self.types()
  }
}

impl<Span> AsRef<Span> for Document<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for Document<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for Document<Span> {
  type Components = (Span, Vec<TypeSystem<Span>>);

  fn into_components(self) -> Self::Components {
    (self.span, self.types)
  }
}

impl<Span> Document<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn types(&self) -> &[TypeSystem<Span>] {
    self.types.as_slice()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    TypeSystem::parser()
      .padded_by(ignored())
      .repeated()
      .collect()
      .map_with(|types, sp| Self {
        span: source::Span::from_map_extra(sp),
        types,
      })
  }
}
