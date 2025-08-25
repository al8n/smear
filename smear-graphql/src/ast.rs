use chumsky::{extra::ParserExtra, prelude::*};
use derive_more::{AsMut, AsRef, Deref, From, Into, IsVariant, TryUnwrap, Unwrap};

use smear_parser::{
  convert::*,
  definitions,
  lang::{self, punct::*, *},
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

macro_rules! bail_struct_wrapper {
  (
    $(#[$meta:meta])*
    $(Const<$const:literal>)? struct $outer:ident($inner:ty) {
      $(#[$parser_meta:meta])*
      parser: $parser:expr

      $(, remaining: {
        $($item:item)*
      })?
    }
  ) => {
    $(#[$meta:meta])*
    #[derive(Debug, Clone, From, Into, AsMut, AsRef, Deref)]
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

    $(
      impl<Span> Const<$const> for $outer<Span> {}
    )?

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

bail_struct_wrapper!(Const<false> struct ObjectField(lang::ObjectField<InputValue<Span>, Span>) {
  parser: {
    lang::ObjectField::parser_with(InputValue::parser()).map(Self)
  }
});

bail_struct_wrapper!(Const<true> struct ConstObjectField(lang::ObjectField<ConstInputValue<Span>, Span>) {
  parser: {
    lang::ObjectField::parser_with(ConstInputValue::parser()).map(Self)
  }
});

bail_struct_wrapper!(Const<false> struct Object(lang::Object<ObjectField<Span>, Span>) {
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

bail_struct_wrapper!(Const<true> struct ConstObject(lang::Object<ConstObjectField<Span>, Span>) {
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

bail_struct_wrapper!(Const<false> struct List(lang::List<InputValue<Span>, Span>) {
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

bail_struct_wrapper!(Const<true> struct ConstList(lang::List<ConstInputValue<Span>, Span>) {
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

impl<Span> Const<false> for InputValue<Span> {}

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
      let object_value_parser = lang::Object::parser_with::<I, E, _, false>(
        lang::ObjectField::parser_with(value.clone()).map(ObjectField),
      )
      .map(|v| Self::Object(v.into()));
      let list_value_parser =
        lang::List::parser_with::<I, E, _, false>(value.clone()).map(|v| Self::List(v.into()));

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

impl<Span> Const<true> for ConstInputValue<Span> {}

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

      let object_value_parser = lang::Object::parser_with::<I, E, _, true>(
        lang::ObjectField::parser_with(value.clone()).map(ConstObjectField),
      )
      .map(|v| Self::Object(v.into()));
      let list_value_parser =
        lang::List::parser_with::<I, E, _, true>(value.clone()).map(|v| Self::List(v.into()));

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

bail_struct_wrapper!(Const<true> struct DefaultInputValue(lang::DefaultInputValue<ConstInputValue<Span>, Span>) {
  parser: {
    lang::DefaultInputValue::parser_with(ConstInputValue::parser()).map(Self)
  }
});

bail_struct_wrapper!(Const<false> struct Argument(lang::Argument<InputValue<Span>, Span>) {
  parser: {
    lang::Argument::parser_with(InputValue::parser()).map(|arg| Self(arg))
  }
});

bail_struct_wrapper!(Const<false> struct Arguments(lang::Arguments<Argument<Span>, Span>) {
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

bail_struct_wrapper!(Const<true> struct ConstArgument(lang::Argument<ConstInputValue<Span>, Span>) {
  parser: {
    lang::Argument::parser_with(ConstInputValue::parser()).map(Self)
  }
});

bail_struct_wrapper!(Const<true> struct ConstArguments(lang::Arguments<ConstArgument<Span>, Span>) {
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

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Directive<Span>(lang::Directive<Arguments<Span>, Span>);

impl<Span> Const<false> for Directive<Span> {}

impl<Span> Directive<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }
  #[inline]
  pub const fn at(&self) -> &At<Span> {
    self.0.at()
  }
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<Span>> {
    self.0.arguments()
  }

  #[inline]
  pub fn into_arguments(self) -> Option<Arguments<Span>> {
    self.0.into_arguments()
  }

  /// Returns a parser for the directive.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::Directive::parser_with(Arguments::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstDirective<Span>(lang::Directive<ConstArguments<Span>, Span>);

impl<Span> Const<true> for ConstDirective<Span> {}

impl<Span> ConstDirective<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }
  #[inline]
  pub const fn at(&self) -> &At<Span> {
    self.0.at()
  }
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&ConstArguments<Span>> {
    self.0.arguments()
  }

  #[inline]
  pub fn into_arguments(self) -> Option<ConstArguments<Span>> {
    self.0.into_arguments()
  }

  /// Returns a parser for the directive.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::Directive::parser_with(ConstArguments::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Directives<Span>(lang::Directives<Directive<Span>, Span>);

impl<Span> Const<false> for Directives<Span> {}

impl<Span> Directives<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }
  #[inline]
  pub const fn directives(&self) -> &[Directive<Span>] {
    self.0.directives().as_slice()
  }
  #[inline]
  pub fn into_directives(self) -> Vec<Directive<Span>> {
    self.0.into_directives()
  }

  /// Returns a parser for the directive.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::Directives::parser_with(Directive::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstDirectives<Span>(lang::Directives<ConstDirective<Span>, Span>);

impl<Span> Const<true> for ConstDirectives<Span> {}

impl<Span> ConstDirectives<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }
  #[inline]
  pub const fn directives(&self) -> &[ConstDirective<Span>] {
    self.0.directives().as_slice()
  }
  #[inline]
  pub fn into_directives(self) -> Vec<ConstDirective<Span>> {
    self.0.into_directives()
  }

  /// Returns a parser for the directive.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::Directives::parser_with(ConstDirective::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsRef, AsMut)]
pub struct FragmentSpread<Span>(lang::FragmentSpread<Directives<Span>, Span>);

impl<Span> AsRef<Span> for FragmentSpread<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for FragmentSpread<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for FragmentSpread<Span> {
  type Components = <lang::FragmentSpread<Directives<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> FragmentSpread<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn name(&self) -> &FragmentName<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives<Span>> {
    self.0.directives()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::FragmentSpread::parser_with(Directives::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct InlineFragment<Span>(lang::InlineFragment<Directives<Span>, SelectionSet<Span>, Span>);

impl<Span> AsRef<Span> for InlineFragment<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for InlineFragment<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for InlineFragment<Span> {
  type Components = <lang::InlineFragment<Directives<Span>, SelectionSet<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> InlineFragment<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn ellipsis(&self) -> &Ellipsis<Span> {
    self.0.ellipsis()
  }

  #[inline]
  pub const fn type_condition(&self) -> Option<&TypeCondition<Span>> {
    self.0.type_condition()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives<Span>> {
    self.0.directives()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::InlineFragment::parser_with(Directives::parser(), SelectionSet::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct FragmentDefinition<Span>(
  definitions::FragmentDefinition<Directives<Span>, SelectionSet<Span>, Span>,
);

impl<Span> AsRef<Span> for FragmentDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for FragmentDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for FragmentDefinition<Span> {
  type Components = <definitions::FragmentDefinition<Directives<Span>, SelectionSet<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> FragmentDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn fragment_keyword(&self) -> &keywords::Fragment<Span> {
    self.0.fragment_keyword()
  }

  #[inline]
  pub const fn name(&self) -> &FragmentName<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn type_condition(&self) -> &TypeCondition<Span> {
    self.0.type_condition()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet<Span> {
    self.0.selection_set()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::FragmentDefinition::parser_with(Directives::parser(), SelectionSet::parser())
      .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Field<Span>(lang::Field<Arguments<Span>, Directives<Span>, SelectionSet<Span>, Span>);

impl<Span> AsRef<Span> for Field<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for Field<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for Field<Span> {
  type Components = <lang::Field<Arguments<Span>, Directives<Span>, SelectionSet<Span>, Span> as IntoComponents>::Components;

  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> Field<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn alias(&self) -> Option<&Alias<Span>> {
    self.0.alias()
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<Span>> {
    self.0.arguments()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSet<Span>> {
    self.0.selection_set()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
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
}

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
      Self::Field(f) => f.span(),
      Self::FragmentSpread(fs) => fs.span(),
      Self::InlineFragment(ifr) => ifr.span(),
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

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct SelectionSet<Span>(lang::SelectionSet<Selection<Span>, Span>);

impl<Span> AsRef<Span> for SelectionSet<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for SelectionSet<Span> {
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for SelectionSet<Span> {
  type Components = <lang::SelectionSet<Selection<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> SelectionSet<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    self.0.l_brace()
  }

  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    self.0.r_brace()
  }

  #[inline]
  pub const fn selections(&self) -> &[Selection<Span>] {
    self.0.selections().as_slice()
  }

  /// Consumes the selections.
  #[inline]
  pub fn into_selections(self) -> Vec<Selection<Span>> {
    self.0.into_selections()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
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
  }
}

#[derive(Debug, Clone)]
pub struct InputValueDefinition<Span>(
  definitions::InputValueDefinition<
    Type<Span>,
    DefaultInputValue<Span>,
    ConstDirectives<Span>,
    Span,
  >,
);

impl<Span> Const<true> for InputValueDefinition<Span> {}

impl<Span> AsRef<Span> for InputValueDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for InputValueDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> InputValueDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    self.0.colon()
  }

  #[inline]
  pub const fn ty(&self) -> &Type<Span> {
    self.0.ty()
  }

  #[inline]
  pub const fn default_value(&self) -> Option<&DefaultInputValue<Span>> {
    self.0.default_value()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<Span>> {
    self.0.directives()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::InputValueDefinition::parser_with(
      Type::parser(),
      DefaultInputValue::parser(),
      ConstDirectives::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, AsMut, AsRef, From, Into)]
pub struct ArgumentsDefinition<Span>(
  definitions::ArgumentsDefinition<InputValueDefinition<Span>, Span>,
);

impl<Span> AsRef<Span> for ArgumentsDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for ArgumentsDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for ArgumentsDefinition<Span> {
  type Components = (
    Span,
    LParen<Span>,
    Vec<InputValueDefinition<Span>>,
    RParen<Span>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> ArgumentsDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    self.0.l_paren()
  }

  #[inline]
  pub const fn values(&self) -> &[InputValueDefinition<Span>] {
    self.0.values().as_slice()
  }

  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
    self.0.r_paren()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::ArgumentsDefinition::parser_with(InputValueDefinition::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct DirectiveLocations<Span>(definitions::DirectiveLocations<Location<Span>, Span>);

impl<Span> AsRef<Span> for DirectiveLocations<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for DirectiveLocations<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for DirectiveLocations<Span> {
  type Components = (
    Span,
    LeadingDirectiveLocation<Location<Span>, Span>,
    Vec<DirectiveLocation<Location<Span>, Span>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> DirectiveLocations<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn locations(&self) -> &[DirectiveLocation<Location<Span>, Span>] {
    self.0.remaining_locations().as_slice()
  }

  #[inline]
  pub const fn leading_location(&self) -> &LeadingDirectiveLocation<Location<Span>, Span> {
    self.0.leading_location()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::DirectiveLocations::parser_with(Location::parser).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct DirectiveDefinition<Span>(
  definitions::DirectiveDefinition<ConstArguments<Span>, DirectiveLocations<Span>, Span>,
);

impl<Span> AsRef<Span> for DirectiveDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for DirectiveDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for DirectiveDefinition<Span> {
  type Components = <definitions::DirectiveDefinition<
    ConstArguments<Span>,
    DirectiveLocations<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> DirectiveDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn at(&self) -> &At<Span> {
    self.0.at()
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.0.description()
  }

  #[inline]
  pub const fn directive_keyword(&self) -> &keywords::Directive<Span> {
    self.0.directive_keyword()
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&ConstArguments<Span>> {
    self.0.arguments()
  }

  #[inline]
  pub const fn repeatable(&self) -> Option<&keywords::Repeatable<Span>> {
    self.0.repeatable()
  }

  #[inline]
  pub const fn on_keyword(&self) -> &keywords::On<Span> {
    self.0.on_keyword()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn locations(&self) -> &DirectiveLocations<Span> {
    self.0.locations()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::DirectiveDefinition::parser_with(
      ConstArguments::parser(),
      DirectiveLocations::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct EnumValueDefinition<Span>(definitions::EnumValueDefinition<ConstDirectives<Span>, Span>);

impl<Span> Const<true> for EnumValueDefinition<Span> {}

impl<Span> AsRef<Span> for EnumValueDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for EnumValueDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for EnumValueDefinition<Span> {
  type Components =
    <definitions::EnumValueDefinition<ConstDirectives<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> EnumValueDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.0.description()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn value(&self) -> &EnumValue<Span> {
    self.0.value()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::EnumValueDefinition::parser_with(ConstDirectives::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct EnumValuesDefinition<Span>(
  definitions::EnumValuesDefinition<EnumValueDefinition<Span>, Span>,
);

impl<Span> AsRef<Span> for EnumValuesDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for EnumValuesDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for EnumValuesDefinition<Span> {
  type Components = <definitions::EnumValuesDefinition<EnumValueDefinition<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> EnumValuesDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    self.0.l_brace()
  }

  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    self.0.r_brace()
  }

  #[inline]
  pub const fn values(&self) -> &[EnumValueDefinition<Span>] {
    self.0.values().as_slice()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::EnumValuesDefinition::parser_with(EnumValueDefinition::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct EnumTypeDefinition<Span>(
  definitions::EnumTypeDefinition<ConstDirectives<Span>, EnumValuesDefinition<Span>, Span>,
);

impl<Span> AsRef<Span> for EnumTypeDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for EnumTypeDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for EnumTypeDefinition<Span> {
  type Components = <definitions::EnumTypeDefinition<
    ConstDirectives<Span>,
    EnumValuesDefinition<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> EnumTypeDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.0.description()
  }

  #[inline]
  pub const fn enum_keyword(&self) -> &keywords::Enum<Span> {
    self.0.enum_keyword()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn enum_values_definition(&self) -> Option<&EnumValuesDefinition<Span>> {
    self.0.enum_values_definition()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::EnumTypeDefinition::parser_with(
      ConstDirectives::parser(),
      EnumValuesDefinition::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct EnumTypeExtension<Span>(
  definitions::EnumTypeExtension<ConstDirectives<Span>, EnumValuesDefinition<Span>, Span>,
);

impl<Span> AsRef<Span> for EnumTypeExtension<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for EnumTypeExtension<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for EnumTypeExtension<Span> {
  type Components = <definitions::EnumTypeExtension<
    ConstDirectives<Span>,
    EnumValuesDefinition<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> EnumTypeExtension<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    self.0.extend_keyword()
  }

  #[inline]
  pub const fn enum_keyword(&self) -> &keywords::Enum<Span> {
    self.0.enum_keyword()
  }

  #[inline]
  pub const fn content(
    &self,
  ) -> &EnumTypeExtensionContent<ConstDirectives<Span>, EnumValuesDefinition<Span>> {
    self.0.content()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::EnumTypeExtension::parser_with(
      ConstDirectives::parser,
      EnumValuesDefinition::parser,
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct FieldDefinition<Span>(
  definitions::FieldDefinition<ConstArguments<Span>, Type<Span>, ConstDirectives<Span>, Span>,
);

impl<Span> Const<true> for FieldDefinition<Span> {}

impl<Span> AsRef<Span> for FieldDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for FieldDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for FieldDefinition<Span> {
  type Components = <definitions::FieldDefinition<
    ConstArguments<Span>,
    Type<Span>,
    ConstDirectives<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> FieldDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.0.description()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn arguments_definition(&self) -> Option<&ConstArguments<Span>> {
    self.0.arguments_definition()
  }

  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    self.0.colon()
  }

  #[inline]
  pub const fn ty(&self) -> &Type<Span> {
    self.0.ty()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<Span>> {
    self.0.directives()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::FieldDefinition::parser_with(
      ConstArguments::parser(),
      Type::parser(),
      ConstDirectives::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct FieldsDefinition<Span>(definitions::FieldsDefinition<FieldDefinition<Span>, Span>);

impl<Span> Const<true> for FieldsDefinition<Span> {}

impl<Span> AsRef<Span> for FieldsDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for FieldsDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for FieldsDefinition<Span> {
  type Components =
    <definitions::FieldsDefinition<FieldDefinition<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> FieldsDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    self.0.l_brace()
  }

  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    self.0.r_brace()
  }

  #[inline]
  pub const fn fields(&self) -> &[FieldDefinition<Span>] {
    self.0.fields().as_slice()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::FieldsDefinition::parser_with(FieldDefinition::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct InputFieldsDefinition<Span>(
  definitions::InputFieldsDefinition<InputValueDefinition<Span>, Span>,
);

impl<Span> Const<true> for InputFieldsDefinition<Span> {}

impl<Span> AsRef<Span> for InputFieldsDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for InputFieldsDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for InputFieldsDefinition<Span> {
  type Components = <definitions::InputFieldsDefinition<InputValueDefinition<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> InputFieldsDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    self.0.l_brace()
  }

  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    self.0.r_brace()
  }

  #[inline]
  pub const fn fields(&self) -> &[InputValueDefinition<Span>] {
    self.0.fields().as_slice()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::InputFieldsDefinition::parser_with(InputValueDefinition::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct InputObjectTypeDefinition<Span>(
  definitions::InputObjectTypeDefinition<ConstDirectives<Span>, InputFieldsDefinition<Span>, Span>,
);

impl<Span> AsRef<Span> for InputObjectTypeDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for InputObjectTypeDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for InputObjectTypeDefinition<Span> {
  type Components = <definitions::InputObjectTypeDefinition<
    ConstDirectives<Span>,
    InputFieldsDefinition<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> InputObjectTypeDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.0.description()
  }

  #[inline]
  pub const fn input_keyword(&self) -> &keywords::Input<Span> {
    self.0.input_keyword()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn fields(&self) -> Option<&InputFieldsDefinition<Span>> {
    self.0.fields()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::InputObjectTypeDefinition::parser_with(
      ConstDirectives::parser(),
      InputFieldsDefinition::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct InputObjectTypeExtension<Span>(
  definitions::InputObjectTypeExtension<ConstDirectives<Span>, InputFieldsDefinition<Span>, Span>,
);

impl<Span> AsRef<Span> for InputObjectTypeExtension<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for InputObjectTypeExtension<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for InputObjectTypeExtension<Span> {
  type Components = <definitions::InputObjectTypeExtension<
    ConstDirectives<Span>,
    InputFieldsDefinition<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> InputObjectTypeExtension<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    self.0.extend_keyword()
  }

  #[inline]
  pub const fn input_keyword(&self) -> &keywords::Input<Span> {
    self.0.input_keyword()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn content(
    &self,
  ) -> &InputObjectTypeExtensionContent<ConstDirectives<Span>, InputFieldsDefinition<Span>> {
    self.0.content()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::InputObjectTypeExtension::parser_with(
      ConstDirectives::parser,
      InputFieldsDefinition::parser,
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct InterfaceTypeDefinition<Span>(
  definitions::InterfaceTypeDefinition<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
    Span,
  >,
);

impl<Span> AsRef<Span> for InterfaceTypeDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for InterfaceTypeDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for InterfaceTypeDefinition<Span> {
  type Components = <definitions::InterfaceTypeDefinition<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> InterfaceTypeDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.0.description()
  }

  #[inline]
  pub const fn interface_keyword(&self) -> &keywords::Interface<Span> {
    self.0.interface_keyword()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn implements(&self) -> Option<&ImplementInterfaces<Span>> {
    self.0.implements()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition<Span>> {
    self.0.fields_definition()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::InterfaceTypeDefinition::parser_with(
      ImplementInterfaces::parser(),
      ConstDirectives::parser(),
      FieldsDefinition::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct InterfaceTypeExtension<Span>(
  definitions::InterfaceTypeExtension<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
    Span,
  >,
);

impl<Span> AsRef<Span> for InterfaceTypeExtension<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for InterfaceTypeExtension<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for InterfaceTypeExtension<Span> {
  type Components = <definitions::InterfaceTypeExtension<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> InterfaceTypeExtension<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    self.0.extend_keyword()
  }

  #[inline]
  pub const fn interface_keyword(&self) -> &keywords::Interface<Span> {
    self.0.interface_keyword()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn content(
    &self,
  ) -> &InterfaceTypeExtensionContent<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
  > {
    self.0.content()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::InterfaceTypeExtension::parser_with(
      ImplementInterfaces::parser,
      ConstDirectives::parser,
      FieldsDefinition::parser,
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct ObjectTypeDefinition<Span>(
  definitions::ObjectTypeDefinition<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
    Span,
  >,
);

impl<Span> AsRef<Span> for ObjectTypeDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for ObjectTypeDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for ObjectTypeDefinition<Span> {
  type Components = <definitions::ObjectTypeDefinition<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> ObjectTypeDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.0.description()
  }

  #[inline]
  pub const fn type_keyword(&self) -> &keywords::Type<Span> {
    self.0.type_keyword()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn implements(&self) -> Option<&ImplementInterfaces<Span>> {
    self.0.implements()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition<Span>> {
    self.0.fields_definition()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::ObjectTypeDefinition::parser_with(
      ImplementInterfaces::parser(),
      ConstDirectives::parser(),
      FieldsDefinition::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsRef, AsMut)]
pub struct ObjectTypeExtension<Span>(
  definitions::ObjectTypeExtension<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
    Span,
  >,
);

impl<Span> AsRef<Span> for ObjectTypeExtension<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for ObjectTypeExtension<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for ObjectTypeExtension<Span> {
  type Components = <definitions::ObjectTypeExtension<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> ObjectTypeExtension<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn content(
    &self,
  ) -> &ObjectTypeExtensionContent<
    ImplementInterfaces<Span>,
    ConstDirectives<Span>,
    FieldsDefinition<Span>,
  > {
    self.0.content()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::ObjectTypeExtension::parser_with(
      ImplementInterfaces::parser,
      ConstDirectives::parser,
      FieldsDefinition::parser,
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct ScalarTypeDefinition<Span>(
  definitions::ScalarTypeDefinition<ConstDirectives<Span>, Span>,
);

impl<Span> AsRef<Span> for ScalarTypeDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for ScalarTypeDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for ScalarTypeDefinition<Span> {
  type Components =
    <definitions::ScalarTypeDefinition<ConstDirectives<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> ScalarTypeDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<Span>> {
    self.0.directives()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::ScalarTypeDefinition::parser_with(ConstDirectives::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsRef, AsMut)]
pub struct ScalarTypeExtension<Span>(definitions::ScalarTypeExtension<ConstDirectives<Span>, Span>);

impl<Span> AsRef<Span> for ScalarTypeExtension<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for ScalarTypeExtension<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for ScalarTypeExtension<Span> {
  type Components =
    <definitions::ScalarTypeExtension<ConstDirectives<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> ScalarTypeExtension<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    self.0.extend_keyword()
  }

  #[inline]
  pub const fn scalar_keyword(&self) -> &keywords::Scalar<Span> {
    self.0.scalar_keyword()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn directives(&self) -> &ConstDirectives<Span> {
    self.0.directives()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::ScalarTypeExtension::parser_with(ConstDirectives::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsRef, AsMut)]
pub struct UnionTypeDefinition<Span>(
  definitions::UnionTypeDefinition<ConstDirectives<Span>, UnionMemberTypes<Span>, Span>,
);

impl<Span> AsRef<Span> for UnionTypeDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for UnionTypeDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for UnionTypeDefinition<Span> {
  type Components = <definitions::UnionTypeDefinition<
    ConstDirectives<Span>,
    UnionMemberTypes<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> UnionTypeDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.0.description()
  }

  #[inline]
  pub const fn union_keyword(&self) -> &keywords::Union<Span> {
    self.0.union_keyword()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn members(&self) -> Option<&UnionMemberTypes<Span>> {
    self.0.members()
  }

  #[inline]
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::UnionTypeDefinition::parser_with(
      ConstDirectives::parser(),
      UnionMemberTypes::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsRef, AsMut)]
pub struct UnionTypeExtension<Span>(
  definitions::UnionTypeExtension<ConstDirectives<Span>, UnionMemberTypes<Span>, Span>,
);

impl<Span> AsRef<Span> for UnionTypeExtension<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for UnionTypeExtension<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for UnionTypeExtension<Span> {
  type Components = <definitions::UnionTypeExtension<
    ConstDirectives<Span>,
    UnionMemberTypes<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> UnionTypeExtension<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    self.0.extend_keyword()
  }

  #[inline]
  pub const fn union_keyword(&self) -> &keywords::Union<Span> {
    self.0.union_keyword()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn content(
    &self,
  ) -> &UnionTypeExtensionContent<ConstDirectives<Span>, UnionMemberTypes<Span>> {
    self.0.content()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::UnionTypeExtension::parser_with(ConstDirectives::parser, UnionMemberTypes::parser)
      .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct VariableDefinition<Span>(
  definitions::VariableDefinition<Type<Span>, Directives<Span>, DefaultInputValue<Span>, Span>,
);

impl<Span> AsRef<Span> for VariableDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for VariableDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for VariableDefinition<Span> {
  type Components = <definitions::VariableDefinition<
    Type<Span>,
    Directives<Span>,
    DefaultInputValue<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> VariableDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.0.description()
  }

  #[inline]
  pub const fn variable(&self) -> &Variable<Span> {
    self.0.variable()
  }

  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    self.0.colon()
  }

  #[inline]
  pub const fn ty(&self) -> &Type<Span> {
    self.0.ty()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn default_value(&self) -> Option<&DefaultInputValue<Span>> {
    self.0.default_value()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::VariableDefinition::parser(
      Type::parser(),
      Directives::parser(),
      DefaultInputValue::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct VariablesDefinition<Span>(
  definitions::VariablesDefinition<VariableDefinition<Span>, Span>,
);

impl<Span> AsRef<Span> for VariablesDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for VariablesDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for VariablesDefinition<Span> {
  type Components = <definitions::VariablesDefinition<VariableDefinition<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> VariablesDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    self.0.l_paren()
  }

  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
    self.0.r_paren()
  }

  #[inline]
  pub const fn definitions(&self) -> &[VariableDefinition<Span>] {
    self.0.definitions().as_slice()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::VariablesDefinition::parser_with(VariableDefinition::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
pub struct NamedOperationDefinition<Span>(
  definitions::NamedOperationDefinition<
    OperationType<Span>,
    VariablesDefinition<Span>,
    Directives<Span>,
    SelectionSet<Span>,
    Span,
  >,
);

impl<Span> AsRef<Span> for NamedOperationDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.0.span()
  }
}

impl<Span> IntoSpan<Span> for NamedOperationDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for NamedOperationDefinition<Span> {
  type Components = <definitions::NamedOperationDefinition<
    OperationType<Span>,
    VariablesDefinition<Span>,
    Directives<Span>,
    SelectionSet<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> NamedOperationDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.0.description()
  }

  #[inline]
  pub const fn operation_type(&self) -> &OperationType<Span> {
    self.0.operation_type()
  }

  #[inline]
  pub const fn name(&self) -> Option<&Name<Span>> {
    self.0.name()
  }

  #[inline]
  pub const fn variable_definitions(&self) -> Option<&VariablesDefinition<Span>> {
    self.0.variable_definitions()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet<Span> {
    &self.0.selection_set()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::NamedOperationDefinition::parser_with(
      OperationType::parser(),
      VariablesDefinition::parser(),
      Directives::parser(),
      SelectionSet::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum OperationDefinition<Span> {
  Named(NamedOperationDefinition<Span>),
  Shorten(SelectionSet<Span>),
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
      Self::Shorten(s) => s.into_span(),
    }
  }
}

impl<Span> OperationDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Named(named) => named.span(),
      Self::Shorten(short) => short.span(),
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
      definitions::OperationDefinition::Shorten(short) => Self::Shorten(short),
    })
  }
}

#[derive(Debug, Clone, From, Into, AsRef, AsMut)]
pub struct RootOperationTypeDefinition<Span>(
  definitions::RootOperationTypeDefinition<OperationType<Span>, Span>,
);

impl<Span> AsRef<Span> for RootOperationTypeDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.0.span()
  }
}

impl<Span> IntoSpan<Span> for RootOperationTypeDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for RootOperationTypeDefinition<Span> {
  type Components = <definitions::RootOperationTypeDefinition<OperationType<Span>, Span> as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> RootOperationTypeDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn operation_type(&self) -> &OperationType<Span> {
    self.0.operation_type()
  }

  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    self.0.colon()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::RootOperationTypeDefinition::parser_with(OperationType::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsRef, AsMut)]
pub struct RootOperationTypesDefinition<Span>(
  definitions::RootOperationTypesDefinition<RootOperationTypeDefinition<Span>, Span>,
);

impl<Span> AsRef<Span> for RootOperationTypesDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for RootOperationTypesDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for RootOperationTypesDefinition<Span> {
  type Components = <definitions::RootOperationTypesDefinition<
    RootOperationTypeDefinition<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> RootOperationTypesDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    self.0.l_brace()
  }

  #[inline]
  pub const fn definitions(&self) -> &[RootOperationTypeDefinition<Span>] {
    self.0.definitions().as_slice()
  }

  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    self.0.r_brace()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::RootOperationTypesDefinition::parser_with(RootOperationTypeDefinition::parser())
      .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsRef, AsMut)]
pub struct SchemaDefinition<Span>(
  definitions::SchemaDefinition<ConstDirectives<Span>, RootOperationTypesDefinition<Span>, Span>,
);

impl<Span> AsRef<Span> for SchemaDefinition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for SchemaDefinition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for SchemaDefinition<Span> {
  type Components = <definitions::SchemaDefinition<
    ConstDirectives<Span>,
    RootOperationTypesDefinition<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> SchemaDefinition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.0.description()
  }

  #[inline]
  pub const fn schema_keyword(&self) -> &keywords::Schema<Span> {
    self.0.schema_keyword()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn definitions(&self) -> &RootOperationTypesDefinition<Span> {
    self.0.definitions()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::SchemaDefinition::parser_with(
      ConstDirectives::parser(),
      RootOperationTypesDefinition::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsRef, AsMut)]
pub struct SchemaExtension<Span>(
  definitions::SchemaExtension<ConstDirectives<Span>, RootOperationTypesDefinition<Span>, Span>,
);

impl<Span> AsRef<Span> for SchemaExtension<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for SchemaExtension<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<Span> IntoComponents for SchemaExtension<Span> {
  type Components = <definitions::SchemaExtension<
    ConstDirectives<Span>,
    RootOperationTypesDefinition<Span>,
    Span,
  > as IntoComponents>::Components;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<Span> SchemaExtension<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    self.0.extend_keyword()
  }

  #[inline]
  pub const fn schema_keyword(&self) -> &keywords::Schema<Span> {
    self.0.schema_keyword()
  }

  #[inline]
  pub const fn content(
    &self,
  ) -> &SchemaExtensionContent<ConstDirectives<Span>, RootOperationTypesDefinition<Span>> {
    self.0.content()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    definitions::SchemaExtension::parser_with(
      ConstDirectives::parser,
      RootOperationTypesDefinition::parser(),
    )
    .map(Self)
  }
}

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
      Self::Scalar(s) => s.span(),
      Self::Enum(e) => e.span(),
      Self::Union(u) => u.span(),
      Self::InputObject(i) => i.span(),
      Self::Object(o) => o.span(),
      Self::Interface(i) => i.span(),
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
      Self::Directive(d) => d.span(),
      Self::Schema(s) => s.span(),
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
      Self::Scalar(s) => s.span(),
      Self::Enum(e) => e.span(),
      Self::Union(u) => u.span(),
      Self::InputObject(i) => i.span(),
      Self::Object(o) => o.span(),
      Self::Interface(i) => i.span(),
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
      Self::Schema(s) => s.span(),
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

// #[cfg(test)]
// mod tests {
//   use super::*;
//   use chumsky::{extra::Err, prelude::*};

//   type Error<'a, T, S> = Err<Rich<'a, T, S>>;

//   #[test]
//   fn t() {
//     let input = "nul";

//     let parser = NullValue::parser::<&str, Error<'_, _, _>>();
//   }
// }
