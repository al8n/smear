use chumsky::{extra::ParserExtra, prelude::*};
use derive_more::{AsMut, AsRef, From, Into, IsVariant, TryUnwrap, Unwrap};

use smear_parser::{
  lang::{self, punct::*, *},
  source::{self, Char, Slice, Source},
};

pub type List<Span> = lang::List<InputValue<Span>, Span>;
pub type ConstList<Span> = lang::List<ConstInputValue<Span>, Span>;

pub type Object<Span> = lang::Object<InputValue<Span>, Span>;
pub type ConstObject<Span> = lang::Object<ConstInputValue<Span>, Span>;

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
      Self::List(value) => value.span(),
      Self::Object(value) => value.span(),
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
      let object_value_parser =
        Object::parser_with::<I, E, _, false>(value.clone()).map(|v| Self::Object(v));
      let list_value_parser =
        List::parser_with::<I, E, _, false>(value.clone()).map(|v| Self::List(v));

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
      Self::List(value) => value.span(),
      Self::Object(value) => value.span(),
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

      let object_value_parser =
        ConstObject::parser_with::<I, E, _, true>(value.clone()).map(|v| Self::Object(v));
      let list_value_parser =
        ConstList::parser_with::<I, E, _, true>(value.clone()).map(|v| Self::List(v));

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

/// Default input value
#[derive(Debug, Clone)]
pub struct DefaultInputValue<Span>(lang::DefaultInputValue<ConstInputValue<Span>, Span>);

impl<Span> Const<true> for DefaultInputValue<Span> {}

impl<Span> DefaultInputValue<Span> {
  /// Returns the span of the default input value
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns a reference to the equal token
  #[inline]
  pub const fn eq(&self) -> &Equal<Span> {
    self.0.eq()
  }

  /// Returns a reference to the value of the default input value.
  #[inline]
  pub const fn value(&self) -> &ConstInputValue<Span> {
    self.0.value()
  }

  /// Returns a parser of default input value.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::DefaultInputValue::parser_with(ConstInputValue::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Argument<Span>(lang::Argument<InputValue<Span>, Span>);

impl<Span> Const<false> for Argument<Span> {}

impl<Span> Argument<Span> {
  /// Returns the span of the argument.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the span of the colon
  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    self.0.colon()
  }

  /// Returns the name of the argument.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  /// Returns the value of the argument.
  #[inline]
  pub const fn value(&self) -> &InputValue<Span> {
    self.0.value()
  }

  /// Returns a parser for the argument.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::Argument::parser_with(InputValue::parser()).map(|arg| Self(arg))
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstArgument<Span>(lang::Argument<ConstInputValue<Span>, Span>);

impl<Span> Const<true> for ConstArgument<Span> {}

impl<Span> ConstArgument<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the span of the colon
  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    self.0.colon()
  }

  /// Returns the name of the argument.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  /// Returns the value of the argument.
  #[inline]
  pub const fn value(&self) -> &ConstInputValue<Span> {
    self.0.value()
  }

  /// Returns a parser for the argument.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::Argument::parser_with(ConstInputValue::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Arguments<Span>(lang::Arguments<Argument<Span>, Span>);

impl<Span> Const<false> for Arguments<Span> {}

impl<Span> Arguments<Span> {
  /// Returns the span of the arguments.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the left parenthesis of the arguments.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    self.0.l_paren()
  }

  /// Returns the right parenthesis of the arguments.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
    self.0.r_paren()
  }

  /// Returns the list of arguments
  #[inline]
  pub const fn arguments(&self) -> &[Argument<Span>] {
    self.0.arguments().as_slice()
  }

  /// Returns a parser for the arguments.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::Arguments::parser_with(Argument::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct ConstArguments<Span>(lang::Arguments<ConstArgument<Span>, Span>);

impl<Span> Const<true> for ConstArguments<Span> {}

impl<Span> ConstArguments<Span> {
  /// Returns the span of the arguments.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the left parenthesis of the arguments.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    self.0.l_paren()
  }

  /// Returns the right parenthesis of the arguments.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
    self.0.r_paren()
  }

  /// Returns the list of arguments
  #[inline]
  pub const fn arguments(&self) -> &[ConstArgument<Span>] {
    self.0.arguments().as_slice()
  }

  /// Returns a parser for the arguments.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::Arguments::parser_with(ConstArgument::parser()).map(Self)
  }
}

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
#[repr(transparent)]
pub struct Field<Span>(lang::Field<Arguments<Span>, Directives<Span>, SelectionSet<Span>, Span>);

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

impl<Span> Selection<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Selection::Field(f) => f.span(),
      Selection::FragmentSpread(fs) => fs.span(),
      Selection::InlineFragment(ifr) => ifr.span(),
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
