// use chumsky::{
//   extra::ParserExtra, prelude::*, text::TextExpected,
//   util::MaybeRef,
// };
// use either::Either;

// use super::{spanned::Spanned, source::Source};

// use super::*;

// /// Input value
// ///
// /// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
// ///
// /// About the generics
// /// - `Src`: the corresponding slice of the [`StrInput`](chumsky::input::StrInput)
// /// - `Span`: the span type of the input [`StrInput`](chumsky::input::StrInput)
// /// - `Container`: the container type for storing the nested type, e.g. list, object, map, set.
// #[derive(Debug, Clone, derive_more::Unwrap, derive_more::TryUnwrap, derive_more::IsVariant)]
// #[unwrap(ref, ref_mut)]
// #[try_unwrap(ref, ref_mut)]
// #[non_exhaustive]
// pub enum InputValue<Span> {
//   /// Spec: [Variable Value](https://spec.graphql.org/draft/#sec-Variable-Value)
//   Variable(Variable<Span>),
//   /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
//   ///
//   /// Instead of giving a type of number, keep the raw string representation, let the
//   /// upper layers handle the conversion.
//   Int(IntValue<Span>),
//   /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
//   ///
//   /// Instead of giving a type of float, keep the raw string representation, let the
//   /// upper layers handle the conversion.
//   Float(FloatValue<Span>),
//   /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
//   Boolean(BooleanValue<Span>),
//   /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
//   String(StringValue<Span>),
//   /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
//   Null(NullValue<Span>),
//   /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
//   Enum(EnumValue<Span>),
//   /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
//   List(List<Self, Span>),
//   /// Spec: [Set Value](https://spec.graphql.org/draft/#sec-Set-Value)
//   Set(Set<Self, Span>),
//   /// Spec: [Map Value](https://spec.graphql.org/draft/#sec-Map-Value)
//   Map(Map<Self, Span>),
//   /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
//   Object(Object<Self, Span>),
// }

// impl<Span> InputValue<Span> {
//   /// Returns the span of the input value.
//   #[inline]
//   pub const fn span(&self) -> &Span {
//     match self {
//       Self::Variable(value) => value.span(),
//       Self::Int(value) => value.span(),
//       Self::Float(value) => value.span(),
//       Self::Boolean(value) => value.span(),
//       Self::String(value) => value.span(),
//       Self::Null(value) => value.span(),
//       Self::Enum(value) => value.span(),
//       Self::List(value) => value.span(),
//       Self::Set(value) => value.span(),
//       Self::Map(value) => value.span(),
//       Self::Object(value) => value.span(),
//     }
//   }
// }

// impl<Span> InputValue<Span> {
//   /// Returns a parser for the input value.
//   ///
//   /// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
//   pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
//   where
//     I: Source<'src>,
//     I::Token: Char + 'src,
//     I::Slice: Slice<Token = I::Token>,
//     Src: 'src,
//     Span: 'src,
//     E: ParserExtra<'src, I>,
//     E::Error:
//       LabelError<'src, I, &'static str>,
//   {
//     recursive(|value| {
//       let boolean_value_parser = Boolean::parser::<I, E>().map(|v| Self::Boolean(v));
//       let null_value_parser = Null::parser::<I, E>().map(|v| Self::Null(v));
//       let int_value_parser = Int::parser::<I, E>().map(|v| Self::Int(v));
//       let float_value_parser = Float::parser::<I, E>().map(|v| Self::Float(v));
//       let string_value_parser = String::parser::<I, E>().map(|v| Self::String(v));
//       let enum_value_parser = Enum::parser::<I, E>().map(|v| Self::Enum(v));
//       let variable_value_parser = Variable::parser::<I, E>().map(|v| Self::Variable(v));
//       let object_value_parser =
//         Object::parser_with::<I, E, _>(value.clone()).map(|v| Self::Object(v));
//       let list_value_parser = List::parser_with::<I, E, _>(value.clone()).map(|v| Self::List(v));
//       let angle_value_parser =
//         angle_parser_with::<I, E, _, Self, _, _>(value).map(|v| match v.into() {
//           Either::Left(set) => Self::Set(set),
//           Either::Right(map) => Self::Map(map),
//         });

//       choice((
//         boolean_value_parser,
//         null_value_parser,
//         enum_value_parser,
//         variable_value_parser,
//         string_value_parser,
//         float_value_parser,
//         int_value_parser,
//         list_value_parser,
//         angle_value_parser,
//         object_value_parser,
//       ))
//       .padded_by(super::ignored::ignored())
//     })
//   }
// }

// /// Input value
// ///
// /// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
// ///
// /// About the generics
// /// - `Src`: the corresponding slice of the [`StrInput`](chumsky::input::StrInput)
// /// - `Span`: the span type of the input [`StrInput`](chumsky::input::StrInput)
// /// - `Container`: the container type for storing the nested type, e.g. list, object, map, set.
// #[derive(Debug, Clone, derive_more::Unwrap, derive_more::TryUnwrap, derive_more::IsVariant)]
// #[unwrap(ref, ref_mut)]
// #[try_unwrap(ref, ref_mut)]
// #[non_exhaustive]
// pub enum ConstInputValue<Span> {
//   /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
//   ///
//   /// Instead of giving a type of number, keep the raw string representation, let the
//   /// upper layers handle the conversion.
//   Int(IntValue<Span>),
//   /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
//   ///
//   /// Instead of giving a type of float, keep the raw string representation, let the
//   /// upper layers handle the conversion.
//   Float(FloatValue<Span>),
//   /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
//   Boolean(BooleanValue<Span>),
//   /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
//   String(StringValue<Span>),
//   /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
//   Null(NullValue<Span>),
//   /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
//   Enum(EnumValue<Span>),
//   /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
//   List(List<Self, Span>),
//   /// Spec: [Set Value](https://spec.graphql.org/draft/#sec-Set-Value)
//   Set(Set<Self, Span>),
//   /// Spec: [Map Value](https://spec.graphql.org/draft/#sec-Map-Value)
//   Map(Map<Self, Span>),
//   /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
//   Object(Object<Self, Span>),
// }

// impl<Span> ConstInputValue<Span> {
//   /// Returns the span of the input value.
//   #[inline]
//   pub const fn span(&self) -> &Span {
//     match self {
//       Self::Int(value) => value.span(),
//       Self::Float(value) => value.span(),
//       Self::Boolean(value) => value.span(),
//       Self::String(value) => value.span(),
//       Self::Null(value) => value.span(),
//       Self::Enum(value) => value.span(),
//       Self::List(value) => value.span(),
//       Self::Set(value) => value.span(),
//       Self::Map(value) => value.span(),
//       Self::Object(value) => value.span(),
//     }
//   }
// }

// impl<Span> ConstInputValue<Span> {
//   /// Returns a parser for the input value.
//   ///
//   /// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
//   pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
//   where
//     I: Source<'src>,
//     I::Token: Char + 'src,
//  I::Slice: Slice<Token = I::Token>,
//     Src: 'src,
//     Span: 'src,
//     E: ParserExtra<'src, I>,
//     E::Error:
//       LabelError<'src, I, &'static str>,
//   {
//     recursive(|value| {
//       // scalars (whatever you already have)
//       let boolean_value_parser = Boolean::parser::<I, E>().map(|v| Self::Boolean(v));
//       let null_value_parser = Null::parser::<I, E>().map(|v| Self::Null(v));
//       let int_value_parser = Int::parser::<I, E>().map(|v| Self::Int(v));
//       let float_value_parser = Float::parser::<I, E>().map(|v| Self::Float(v));
//       let string_value_parser = String::parser::<I, E>().map(|v| Self::String(v));
//       let enum_value_parser = Enum::parser::<I, E>().map(|v| Self::Enum(v));

//       let object_value_parser =
//         Object::parser_with::<I, E, _>(value.clone()).map(|v| Self::Object(v));
//       let list_value_parser = List::parser_with::<I, E, _>(value.clone()).map(|v| Self::List(v));
//       let angle_value_parser =
//         angle_parser_with::<I, E, _, Self, _, _>(value).map(|v| match v.into() {
//           Either::Left(set) => Self::Set(set),
//           Either::Right(map) => Self::Map(map),
//         });

//       choice((
//         boolean_value_parser,
//         null_value_parser,
//         float_value_parser,
//         int_value_parser,
//         string_value_parser,
//         enum_value_parser,
//         list_value_parser,
//         object_value_parser,
//         angle_value_parser,
//       ))
//       .padded_by(super::ignored::ignored())
//     })
//   }
// }

// #[test]
// fn test_empty_set() {
//   let input = "<>";

//   let set = ConstInputValue::parser::<&str, crate::Error>()
//     .parse(input)
//     .into_result()
//     .unwrap()
//     .unwrap_set();
//   assert!(set.values().is_empty());
// }

// #[test]
// fn test_non_empty_set() {
//   let input = "<0, 1, 2, 3>";

//   let set = ConstInputValue::parser::<&str, crate::Error>()
//     .parse(input)
//     .into_result()
//     .unwrap()
//     .unwrap_set();
//   assert!(!set.values().is_empty());

//   for (idx, value) in set.values().iter().enumerate() {
//     assert_eq!(
//       value
//         .unwrap_int_ref()
//         .digits()
//         .source()
//         .parse::<usize>()
//         .unwrap(),
//       idx
//     );
//   }
// }

// #[test]
// fn test_empty_map() {
//   let input = "<:>";

//   let map = ConstInputValue::parser::<&str, crate::Error>()
//     .parse(input)
//     .into_result()
//     .unwrap()
//     .unwrap_map();
//   assert!(map.fields().is_empty());
// }

// #[test]
// fn test_non_empty_map() {
//   let input = r##"<"0": 0, "1": 1, "2": 2>"##;

//   let map = ConstInputValue::parser::<&str, crate::Error>()
//     .parse(input)
//     .into_result()
//     .unwrap()
//     .unwrap_map();
//   assert!(!map.fields().is_empty());

//   for (idx, field) in map.fields().iter().enumerate() {
//     assert_eq!(
//       field
//         .key()
//         .unwrap_string_ref()
//         .content()
//         .source()
//         .parse::<usize>()
//         .unwrap(),
//       idx
//     );
//     assert_eq!(
//       field
//         .value()
//         .unwrap_int_ref()
//         .digits()
//         .source()
//         .parse::<usize>()
//         .unwrap(),
//       idx
//     );
//   }
// }

// #[test]
// fn test_empty_list() {
//   let input = "[]";

//   let list = ConstInputValue::parser::<&str, crate::Error>()
//     .parse(input)
//     .into_result()
//     .unwrap()
//     .unwrap_list();
//   assert!(list.values().is_empty());
// }

// #[test]
// fn test_non_empty_list() {
//   let input = "[0, 1, 2]";

//   let list = ConstInputValue::parser::<&str, crate::Error>()
//     .parse(input)
//     .into_result()
//     .unwrap()
//     .unwrap_list();
//   assert!(!list.values().is_empty());

//   for (idx, value) in list.values().iter().enumerate() {
//     assert_eq!(
//       value
//         .unwrap_int_ref()
//         .digits()
//         .source()
//         .parse::<usize>()
//         .unwrap(),
//       idx
//     );
//   }
// }

// #[test]
// fn test_empty_object() {
//   let input = "{}";

//   let object = ConstInputValue::parser::<&str, crate::Error>()
//     .parse(input)
//     .into_result()
//     .unwrap()
//     .unwrap_object();
//   assert!(object.fields().is_empty());
// }

// #[test]
// fn test_non_empty_object() {
//   let input = r##"{ _0: 0, _1: 1, _2: 2 }"##;

//   let object = ConstInputValue::parser::<&str, crate::Error>()
//     .parse(input)
//     .into_result()
//     .unwrap()
//     .unwrap_object();
//   assert!(!object.fields().is_empty());

//   for (idx, field) in object.fields().iter().enumerate() {
//     assert_eq!(
//       field
//         .name()
//         .source()
//         .trim_matches('_')
//         .parse::<usize>()
//         .unwrap(),
//       idx
//     );
//     assert_eq!(
//       field
//         .value()
//         .unwrap_int_ref()
//         .digits()
//         .source()
//         .parse::<usize>()
//         .unwrap(),
//       idx
//     );
//   }
// }
