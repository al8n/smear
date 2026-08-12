//! The GraphQL value productions with `Int` and `Float` materialised as [`i64`] and [`f64`].
//!
//! This is **not a second dialect and not a second AST**. Every node it builds is a node
//! [`super`] builds, at a different instantiation of the same generic carriers — see
//! [`ast::materialized`](crate::graphql::ast::materialized), which is a set of type aliases and
//! declares no type of its own. Every composite production here is
//! [`super`]'s production monomorphised against a different
//! `Numbers` marker, so the two parsers commit at the same tokens, recover the same way and
//! raise the same errors everywhere except where a number is converted. Each of the five leaves
//! that materialisation does not touch — strings, booleans, `null`, enums and variables —
//! delegates straight to [`super`]'s, so there is one implementation of each and not two.
//!
//! # What is converted
//!
//! `Int` becomes [`i64`] and `Float` becomes [`f64`]. **Strings are not touched**: a
//! [`StringValue`] keeps its source slice, escapes included,
//! because unescaping means an owned buffer per node and the property this module exists to keep
//! is that materialisation allocates nothing the slice parser did not already allocate.
//! `materialization_allocates_nothing` in the tests measures that against the slice parser on the
//! same document rather than asserting it.
//!
//! # The accepted bound
//!
//! A 26-digit integer literal is *syntactically valid GraphQL*. Here it is
//! [`ErrorData::IntOverflow`](crate::graphql::error::ErrorData::IntOverflow) — a **parse** error
//! where the specification would raise a **coercion** error, because this view does the
//! conversion at the point the literal is read. The same holds for a float literal that names no
//! finite double, as [`ErrorData::FloatOverflow`](crate::graphql::error::ErrorData::FloatOverflow).
//!
//! That is a documented bound of this view and not a defect to engineer around: the slice parser
//! in [`super`] is unchanged, still accepts the full grammar, and a consumer that needs
//! `BigInt`-style handling of beyond-range values parses with it instead. Rendering such a value
//! back out as a JSON string is the output side's decision and does not belong here.
//!
//! # Why free functions and no `graphql` method
//!
//! [`super`] hangs its public entry points off the AST types as `Node::graphql`. This module
//! cannot: its nodes are the *same* types at `i64`/`f64`, so a second inherent `graphql` on
//! `IntValue<i64>` would collide with the blanket `impl<S> IntValue<S>` that already carries one.
//! The module path is what distinguishes the two instantiations, which is the same thing the type
//! aliases say.
//!
//! # The bound set
//!
//! Every entry here requires `GraphqlSlice<'inp, Src>: AsRef<[u8]>` — this crate's established
//! spelling for "a slice whose text can be read", satisfied by every source backing it ships. It
//! is one bound set for the whole module rather than the narrowest per production, so the
//! module's contract is one sentence; the slice parser is unchanged and unbounded by it.

use tokora::{
  Lexer, SimpleSpan, Slice, Source,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, IntoComponents},
};

use smear_lexer::graphql::ContextualKeyword;

use super::{
  GraphQL, GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken, ParseCtx,
  numbers::{MaterializedNumbers, Numbers},
};
use crate::graphql::{
  ast::{
    BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue, VariableValue,
    materialized::{
      ConstInputValue, ConstList, ConstObject, ConstObjectField, DefaultInputValue, InputValue,
      List, Object, ObjectField,
    },
  },
  error::GraphqlError as DialectGraphqlError,
};

/// One bound set for the whole module, and one doc line per entry.
///
/// Wider than each production strictly needs — `string_value` reads no bytes and `variable_value`
/// downcasts no keyword — deliberately: a module whose contract is "available wherever the source
/// text can be read" is one sentence, and every backing this crate ships satisfies it.
macro_rules! materialized_parser {
  (
    $name:ident,
    $input:ident,
    $output:ty,
    $doc:literal,
    $body:block
  ) => {
    #[doc = $doc]
    ///
    /// The materialised-number instantiation; see the module header for what that converts.
    pub fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + AsRef<[u8]> + 'inp,
      GraphqlLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
      Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
    $body
  };
}

/// Converts a parsed slice-payload integer node in place, keeping its span.
///
/// This is the design's `.and_then(|x| x.parse())` with the error mapped: the production above it
/// is [`super`]'s, unmodified, and this is everything that is added to it.
#[inline]
fn materialize_int<'inp, Src, Ctx>(
  node: IntValue<GraphqlSlice<'inp, Src>>,
) -> Result<IntValue<i64>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + AsRef<[u8]> + 'inp,
  GraphqlLexer<'inp, Src>: Lexer<'inp>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
{
  let (span, slice) = node.into_components();
  match <MaterializedNumbers as Numbers<GraphqlSlice<'inp, Src>>>::int(slice) {
    Ok(payload) => Ok(IntValue::new(span, payload)),
    Err(err) => {
      Err(<MaterializedNumbers as Numbers<GraphqlSlice<'inp, Src>>>::report(err, span).into())
    }
  }
}

/// Converts a parsed slice-payload float node in place, keeping its span.
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
  match <MaterializedNumbers as Numbers<GraphqlSlice<'inp, Src>>>::float(slice) {
    Ok(payload) => Ok(FloatValue::new(span, payload)),
    Err(err) => {
      Err(<MaterializedNumbers as Numbers<GraphqlSlice<'inp, Src>>>::report(err, span).into())
    }
  }
}

// ── The two leaves materialisation touches ───────────────────────────────────────────────────

materialized_parser!(
  int_value,
  inp,
  IntValue<i64>,
  "Parses an integer literal and converts it to [`i64`].\n\nSee the [GraphQL Int Value specification](https://spec.graphql.org/draft/#sec-Int-Value).",
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
  ParseAttempt<IntValue<i64>>,
  "Attempts an integer literal, converting it to [`i64`], without consuming on a head mismatch.",
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
  { super::value_with::<Src, Ctx, MaterializedNumbers>(inp) }
);

materialized_parser!(
  const_value,
  inp,
  ConstInputValue<GraphqlSlice<'inp, Src>>,
  "Parses a GraphQL constant input value.\n\nSee the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).",
  { super::const_value_with::<Src, Ctx, MaterializedNumbers>(inp) }
);

materialized_parser!(
  list_value,
  inp,
  List<GraphqlSlice<'inp, Src>>,
  "Parses a list value.\n\nSee the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value).",
  { super::list_value_with::<Src, Ctx, MaterializedNumbers>(inp) }
);

materialized_parser!(
  const_list_value,
  inp,
  ConstList<GraphqlSlice<'inp, Src>>,
  "Parses a constant list value.\n\nSee the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value).",
  { super::const_list_value_with::<Src, Ctx, MaterializedNumbers>(inp) }
);

materialized_parser!(
  object_value,
  inp,
  Object<GraphqlSlice<'inp, Src>>,
  "Parses an object value.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::object_value_with::<Src, Ctx, MaterializedNumbers>(inp) }
);

materialized_parser!(
  const_object_value,
  inp,
  ConstObject<GraphqlSlice<'inp, Src>>,
  "Parses a constant object value.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::const_object_value_with::<Src, Ctx, MaterializedNumbers>(inp) }
);

materialized_parser!(
  object_field,
  inp,
  ObjectField<GraphqlSlice<'inp, Src>>,
  "Parses one object field.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::object_field_with::<Src, Ctx, MaterializedNumbers>(inp) }
);

materialized_parser!(
  const_object_field,
  inp,
  ConstObjectField<GraphqlSlice<'inp, Src>>,
  "Parses one constant object field.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::const_object_field_with::<Src, Ctx, MaterializedNumbers>(inp) }
);

materialized_parser!(
  try_default_value,
  inp,
  ParseAttempt<DefaultInputValue<GraphqlSlice<'inp, Src>>>,
  "Attempts a default value, declining without consuming when `=` is absent.\n\nSee the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).",
  { super::try_default_value_with::<Src, Ctx, MaterializedNumbers>(inp) }
);

materialized_parser!(
  default_value,
  inp,
  Option<DefaultInputValue<GraphqlSlice<'inp, Src>>>,
  "Parses an optional default value.\n\nSee the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).",
  { super::default_value_with::<Src, Ctx, MaterializedNumbers>(inp) }
);

#[cfg(test)]
mod tests;
