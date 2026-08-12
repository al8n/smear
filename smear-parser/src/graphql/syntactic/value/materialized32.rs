//! The GraphQL value productions with `Int` materialised as [`i32`] — **the width draft §3.5.1
//! specifies** — and `Float` as [`f64`].
//!
//! [`syntactic::value::materialized`](crate::graphql::syntactic::value::materialized) is this
//! module at the other width, and its header carries every argument the two share: not a second
//! dialect and not a second parser, one implementation of each untouched leaf, strings keeping
//! their slices, free functions instead of a `Node::graphql` method, one bound set for the
//! module. None of it is repeated here. What is different is one type and one sentence.
//!
//! # The sentence
//!
//! **A literal outside `i32` is refused here and accepted next door**, and that is the whole
//! difference between the two modules. `2147483648` is a well-formed `IntValue` under draft
//! §2.9.1 and is not a value draft §3.5.1's `Int` can hold, so:
//!
//! ```text
//! literal               materialized32        materialized
//! 2147483648            IntOverflow / i32     2147483648i64
//! 9223372036854775808   IntOverflow / i32     IntOverflow / i64
//! ```
//!
//! The width on the error is not decoration. Without it the first row and the second are the same
//! report, and they are different facts about the document: one literal is outside the
//! specification, the other is outside any integer this crate reads.
//!
//! # Which width a new consumer should reach for
//!
//! **This one, unless the document is not a GraphQL document.** A consumer that materialises
//! `Int` is asking for the value a GraphQL server would see, and that value is 32-bit; the
//! sibling exists for the caller who has decided that accepting a larger literal is better than
//! refusing it — a gateway logging what a client sent, a formatter, a migration tool reading
//! documents written against a server that never enforced §3.5.1. That is a real need and a
//! deliberate one, which is why both ship, and it is the narrower of the two needs even though it
//! is the wider of the two types.
//!
//! # What this module does *not* change
//!
//! [`float_value`](crate::graphql::syntactic::value::materialized32::float_value) and the five
//! leaves materialisation never touches produce exactly what their twins next door produce, and
//! delegate to the same bodies. They are re-published here rather than left out because a module
//! that carried nine of eleven productions would make its own path mean two different things
//! depending on which node a caller reached for — the argument the sibling's header makes about
//! `Node::graphql`, applied to the module boundary instead of the type.

use tokora::{
  Lexer, SimpleSpan, Slice, Source,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, IntoComponents},
};

use smear_lexer::graphql::ContextualKeyword;

use super::{
  GraphQL, GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken, ParseCtx,
  materialized::materialized_parser,
  numbers::{MaterializedNumbers32, Numbers},
};
use crate::graphql::{
  ast::{
    BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue, VariableValue,
    materialized32::{
      ConstInputValue, ConstList, ConstObject, ConstObjectField, DefaultInputValue, InputValue,
      List, Object, ObjectField,
    },
  },
  error::GraphqlError as DialectGraphqlError,
};

/// Converts a parsed slice-payload integer node in place at `i32`, keeping its span.
///
/// The twin of [`materialized::materialize_int`](super::materialized) at the other marker: the
/// production above it is [`super`]'s, unmodified, and this is everything that is added to it.
#[inline]
fn materialize_int<'inp, Src, Ctx>(
  node: IntValue<GraphqlSlice<'inp, Src>>,
) -> Result<IntValue<i32>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + AsRef<[u8]> + 'inp,
  GraphqlLexer<'inp, Src>: Lexer<'inp>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  let (span, slice) = node.into_components();
  match <MaterializedNumbers32 as Numbers<GraphqlSlice<'inp, Src>>>::int(slice) {
    Ok(payload) => Ok(IntValue::new(span, payload)),
    Err(err) => {
      Err(<MaterializedNumbers32 as Numbers<GraphqlSlice<'inp, Src>>>::report(err, span).into())
    }
  }
}

/// Converts a parsed slice-payload float node in place, keeping its span.
///
/// Identical to its twin next door, and it has to be: `Float` is [`f64`] at both widths, because
/// GraphQL's `Float` *is* IEEE 754 double precision (draft §3.5.2).
#[inline]
fn materialize_float<'inp, Src, Ctx>(
  node: FloatValue<GraphqlSlice<'inp, Src>>,
) -> Result<FloatValue<f64>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + AsRef<[u8]> + 'inp,
  GraphqlLexer<'inp, Src>: Lexer<'inp>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  let (span, slice) = node.into_components();
  match <MaterializedNumbers32 as Numbers<GraphqlSlice<'inp, Src>>>::float(slice) {
    Ok(payload) => Ok(FloatValue::new(span, payload)),
    Err(err) => {
      Err(<MaterializedNumbers32 as Numbers<GraphqlSlice<'inp, Src>>>::report(err, span).into())
    }
  }
}

// ── The two leaves materialisation touches ───────────────────────────────────────────────────

materialized_parser!(
  int_value,
  inp,
  IntValue<i32>,
  "Parses an integer literal and converts it to [`i32`], the width draft §3.5.1 specifies.\n\nSee the [GraphQL Int Value specification](https://spec.graphql.org/draft/#sec-Int-Value).",
  { super::int_value(inp).and_then(materialize_int::<Src, Ctx>) }
);

materialized_parser!(
  float_value,
  inp,
  FloatValue<f64>,
  "Parses a float literal and converts it to [`f64`].\n\nSee the [GraphQL Float Value specification](https://spec.graphql.org/draft/#sec-Float-Value).",
  { super::float_value(inp).and_then(materialize_float::<Src, Ctx>) }
);

materialized_parser!(
  try_int_value,
  inp,
  ParseAttempt<IntValue<i32>>,
  "Attempts an integer literal, converting it to [`i32`], without consuming on a head mismatch.",
  { super::try_int_value(inp)?.and_then(materialize_int::<Src, Ctx>) }
);

materialized_parser!(
  try_float_value,
  inp,
  ParseAttempt<FloatValue<f64>>,
  "Attempts a float literal, converting it to [`f64`], without consuming on a head mismatch.",
  { super::try_float_value(inp)?.and_then(materialize_float::<Src, Ctx>) }
);

// ── The five leaves it does not, delegating rather than repeating ────────────────────────────

materialized_parser!(
  string_value,
  inp,
  StringValue<GraphqlSlice<'inp, Src>>,
  "Parses a string literal, which **keeps its source slice** — see the module header.\n\nSee the [GraphQL String Value specification](https://spec.graphql.org/draft/#sec-String-Value).",
  { super::string_value(inp) }
);

materialized_parser!(
  boolean_value,
  inp,
  BooleanValue<GraphqlSlice<'inp, Src>>,
  "Parses a boolean literal.\n\nSee the [GraphQL Boolean Value specification](https://spec.graphql.org/draft/#sec-Boolean-Value).",
  { super::boolean_value(inp) }
);

materialized_parser!(
  null_value,
  inp,
  NullValue<GraphqlSlice<'inp, Src>>,
  "Parses the `null` literal.\n\nSee the [GraphQL Null Value specification](https://spec.graphql.org/draft/#sec-Null-Value).",
  { super::null_value(inp) }
);

materialized_parser!(
  enum_value,
  inp,
  EnumValue<GraphqlSlice<'inp, Src>>,
  "Parses an enum literal.\n\nSee the [GraphQL Enum Value specification](https://spec.graphql.org/draft/#sec-Enum-Value).",
  { super::enum_value(inp) }
);

materialized_parser!(
  variable_value,
  inp,
  VariableValue<GraphqlSlice<'inp, Src>>,
  "Parses a variable reference.\n\nSee the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).",
  { super::variable_value(inp) }
);

materialized_parser!(
  try_string_value,
  inp,
  ParseAttempt<StringValue<GraphqlSlice<'inp, Src>>>,
  "Attempts a string literal without consuming on a head mismatch.",
  { super::try_string_value(inp) }
);

materialized_parser!(
  try_boolean_value,
  inp,
  ParseAttempt<BooleanValue<GraphqlSlice<'inp, Src>>>,
  "Attempts a boolean literal without consuming on a head mismatch.",
  { super::try_boolean_value(inp) }
);

materialized_parser!(
  try_null_value,
  inp,
  ParseAttempt<NullValue<GraphqlSlice<'inp, Src>>>,
  "Attempts the `null` literal without consuming on a head mismatch.",
  { super::try_null_value(inp) }
);

materialized_parser!(
  try_enum_value,
  inp,
  ParseAttempt<EnumValue<GraphqlSlice<'inp, Src>>>,
  "Attempts an enum literal without consuming on a head mismatch.",
  { super::try_enum_value(inp) }
);

materialized_parser!(
  try_variable_value,
  inp,
  ParseAttempt<VariableValue<GraphqlSlice<'inp, Src>>>,
  "Attempts a variable reference without consuming on a head mismatch.",
  { super::try_variable_value(inp) }
);

// ── The composites, each one [`super`]'s production at the other payload ─────────────────────

materialized_parser!(
  value,
  inp,
  InputValue<GraphqlSlice<'inp, Src>>,
  "Parses any GraphQL input value.\n\nSee the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).",
  { super::value_with::<Src, Ctx, MaterializedNumbers32>(inp) }
);

materialized_parser!(
  const_value,
  inp,
  ConstInputValue<GraphqlSlice<'inp, Src>>,
  "Parses a GraphQL constant input value.\n\nSee the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).",
  { super::const_value_with::<Src, Ctx, MaterializedNumbers32>(inp) }
);

materialized_parser!(
  list_value,
  inp,
  List<GraphqlSlice<'inp, Src>>,
  "Parses a list value.\n\nSee the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value).",
  { super::list_value_with::<Src, Ctx, MaterializedNumbers32>(inp) }
);

materialized_parser!(
  const_list_value,
  inp,
  ConstList<GraphqlSlice<'inp, Src>>,
  "Parses a constant list value.\n\nSee the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value).",
  { super::const_list_value_with::<Src, Ctx, MaterializedNumbers32>(inp) }
);

materialized_parser!(
  object_value,
  inp,
  Object<GraphqlSlice<'inp, Src>>,
  "Parses an object value.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::object_value_with::<Src, Ctx, MaterializedNumbers32>(inp) }
);

materialized_parser!(
  const_object_value,
  inp,
  ConstObject<GraphqlSlice<'inp, Src>>,
  "Parses a constant object value.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::const_object_value_with::<Src, Ctx, MaterializedNumbers32>(inp) }
);

materialized_parser!(
  object_field,
  inp,
  ObjectField<GraphqlSlice<'inp, Src>>,
  "Parses one object field.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::object_field_with::<Src, Ctx, MaterializedNumbers32>(inp) }
);

materialized_parser!(
  const_object_field,
  inp,
  ConstObjectField<GraphqlSlice<'inp, Src>>,
  "Parses one constant object field.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::const_object_field_with::<Src, Ctx, MaterializedNumbers32>(inp) }
);

materialized_parser!(
  try_default_value,
  inp,
  ParseAttempt<DefaultInputValue<GraphqlSlice<'inp, Src>>>,
  "Attempts a default value, declining without consuming when `=` is absent.\n\nSee the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).",
  { super::try_default_value_with::<Src, Ctx, MaterializedNumbers32>(inp) }
);

materialized_parser!(
  default_value,
  inp,
  Option<DefaultInputValue<GraphqlSlice<'inp, Src>>>,
  "Parses an optional default value.\n\nSee the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).",
  { super::default_value_with::<Src, Ctx, MaterializedNumbers32>(inp) }
);

#[cfg(test)]
mod tests;
