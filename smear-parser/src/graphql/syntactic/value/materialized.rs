//! The GraphQL value productions with `Int` materialised as `I` and `Float` as [`f64`].
//!
//! This is **not a second dialect and not a second parser**. Every composite production here is
//! [`super`]'s production monomorphised against a different `Numbers` marker, so the two commit
//! at the same tokens, recover the same way and raise the same errors everywhere except where a
//! number is converted. Each of the five leaves that materialisation does not touch — strings,
//! booleans, `null`, enums and variables — delegates straight to [`super`]'s, so there is one
//! implementation of each and not two.
//!
//! It *is* a second value tree: [`ast::materialized`](crate::graphql::ast::materialized) declares
//! two enums whose variants match their slice twins one for one, and whose leaves are the same
//! types but for `Int` and `Float`. The marker chooses which tree a body assembles, the same way
//! it already chose what a numeric leaf carries. That module's header has the argument for two
//! enums over one at two instantiations.
//!
//! # The width is a parameter, and the twelve productions that take it are the ones that carry it
//!
//! `I` is [`i32`] — the width draft §3.5.1 specifies — or [`i64`], the reading that takes draft
//! §2.9.1's unbounded `IntValue` grammar at its word. [`MaterialisedInt`] is the trait that admits
//! them, it is sealed, and its `WIDTH` is what an out-of-range refusal names.
//!
//! Exactly the productions whose *output type* mentions the payload take the parameter:
//! [`int_value`], [`try_int_value`] and the ten composites. [`float_value`] and the ten delegating
//! leaves do not, because nothing in what they return depends on a width — asking a caller of
//! [`string_value`] to name one would be asking them to choose between two answers to a question
//! it does not ask. Where the parameter stops is therefore where the width stops, visibly, in the
//! signatures.
//!
//! # What is converted
//!
//! `Int` becomes `I` and `Float` becomes [`f64`]. **Strings are not touched**: a
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
//! conversion at the point the literal is read. At `i32` the same is true of `2147483648`, which
//! is a well-formed `IntValue` under §2.9.1 and not a value §3.5.1's `Int` can hold. The same
//! holds for a float literal that names no finite double, as
//! [`ErrorData::FloatOverflow`](crate::graphql::error::ErrorData::FloatOverflow).
//!
//! That is a documented bound of this view and not a defect to engineer around: the slice parser
//! in [`super`] is unchanged, still accepts the full grammar, and a consumer that needs
//! `BigInt`-style handling of beyond-range values parses with it instead. Rendering such a value
//! back out as a JSON string is the output side's decision and does not belong here.
//!
//! # Which width a new consumer should reach for
//!
//! **`i32`, unless the document is not a GraphQL document.** A consumer that materialises `Int`
//! is asking for the value a GraphQL server would see, and that value is 32-bit. `i64` is for the
//! caller who has decided that accepting a larger literal is better than refusing it — a gateway
//! logging what a client sent, a formatter, a migration tool reading documents written against a
//! server that never enforced §3.5.1. That is a real need and a deliberate one, which is why both
//! are reachable, and it is the narrower of the two needs even though it is the wider of the two
//! types. The [`IntWidth`](crate::graphql::error::IntWidth) on an
//! [`ErrorData::IntOverflow`](crate::graphql::error::ErrorData::IntOverflow) is how a report says
//! which question was asked:
//!
//! ```text
//! literal               at i32                at i64
//! 2147483648            IntOverflow / i32     2147483648i64
//! 9223372036854775808   IntOverflow / i32     IntOverflow / i64
//! ```
//!
//! The width on the error is not decoration. Without it the first row and the second are the same
//! report, and they are different facts about the document: one literal is outside the
//! specification, the other is outside any integer this crate reads.
//!
//! # Why free functions and no `graphql` method
//!
//! [`super`] hangs its public entry points off the AST types as `Node::graphql`. **The leaves
//! here cannot have one**: `IntValue<i64>` is the very type the blanket `impl<S> IntValue<S>`
//! already carries a `graphql` for, so a second would collide. The composites *could* — they are
//! this module's own enums — and they deliberately do not, because an entry point that exists on
//! nine of eleven productions is worse than one that exists on none: the module path then means
//! two different things depending on which node you reached for. One door per production, all of
//! them here.
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
  GraphQL, GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken, ParseCtx, numbers,
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

use numbers::{Materialized, Numbers};

pub use numbers::MaterialisedInt;

/// One bound set for every entry in this module, and one doc line per entry.
///
/// Wider than each production strictly needs — `string_value` reads no bytes and `variable_value`
/// downcasts no keyword — deliberately: a module whose contract is "available wherever the source
/// text can be read" is one sentence, and every backing this crate ships satisfies it.
///
/// This arm is for the twelve productions whose output carries the width;
/// [`width_free_parser`] is the other twelve. The bound sets differ in `I: MaterialisedInt` and
/// in nothing else, which is what makes the split a statement about where the payload reaches
/// rather than a second contract.
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
    /// The materialised-number instantiation at `I`; see the module header for what that
    /// converts and which width to reach for.
    pub fn $name<'inp, Src, Ctx, I>(
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
      I: MaterialisedInt,
    $body
  };
}

/// The same entry, for a production whose output holds no integer payload.
///
/// [`float_value`] and the ten leaves materialisation never touches produce exactly what
/// [`super`]'s do, at every width, so a width parameter on them would be a parameter no argument
/// and no return type mentions: uninferable, and about nothing. They are published here rather
/// than left out because a module that carried twelve of twenty-four productions would make its
/// own path mean two different things depending on which node a caller reached for — the argument
/// the section on `Node::graphql` makes about the type, applied to the module boundary.
macro_rules! width_free_parser {
  (
    $name:ident,
    $input:ident,
    $output:ty,
    $doc:literal,
    $body:block
  ) => {
    #[doc = $doc]
    ///
    /// Width-free: what it returns is the same type at every `I`, so it takes none.
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

/// Converts a parsed slice-payload integer node in place at `I`, keeping its span.
///
/// This is the design's `.and_then(|x| x.parse())` with the error mapped: the production above it
/// is [`super`]'s, unmodified, and this is everything that is added to it. The width the failure
/// names is `I`'s own — `Materialized<I>` reads it off [`MaterialisedInt::WIDTH`] — so there is no
/// argument here for a caller or a future edit to get wrong.
#[inline]
fn materialize_int<'inp, Src, Ctx, I>(
  node: IntValue<GraphqlSlice<'inp, Src>>,
) -> Result<IntValue<I>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + AsRef<[u8]> + 'inp,
  GraphqlLexer<'inp, Src>: Lexer<'inp>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  I: MaterialisedInt,
{
  let (span, slice) = node.into_components();
  match <Materialized<I> as Numbers<GraphqlSlice<'inp, Src>>>::int(slice) {
    Ok(payload) => Ok(IntValue::new(span, payload)),
    Err(err) => {
      Err(<Materialized<I> as Numbers<GraphqlSlice<'inp, Src>>>::report(err, span).into())
    }
  }
}

/// Converts a parsed slice-payload float node in place, keeping its span.
///
/// It names no marker and no width, and reaches
/// [`numbers::float`](super::numbers::float) directly, because GraphQL's `Float` *is* IEEE 754
/// double precision (draft §3.5.2) at every reading of `Int`. That is why there is nothing here
/// for a per-width test to compare.
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
  match numbers::float(slice) {
    Ok(payload) => Ok(FloatValue::new(span, payload)),
    Err(err) => Err(numbers::report_out_of_range(err, span).into()),
  }
}

// ── The integer leaf, which is the one place the width reaches a leaf ────────────────────────

materialized_parser!(
  int_value,
  inp,
  IntValue<I>,
  "Parses an integer literal and converts it to `I`.\n\nSee the [GraphQL Int Value specification](https://spec.graphql.org/draft/#sec-Int-Value).",
  { super::int_value(inp).and_then(materialize_int::<Src, Ctx, I>) }
);

materialized_parser!(
  try_int_value,
  inp,
  ParseAttempt<IntValue<I>>,
  "Attempts an integer literal, converting it to `I`, without consuming on a head mismatch.",
  { super::try_int_value(inp)?.and_then(materialize_int::<Src, Ctx, I>) }
);

// ── The float leaf, converted and width-free ─────────────────────────────────────────────────

width_free_parser!(
  float_value,
  inp,
  FloatValue<f64>,
  "Parses a float literal and converts it to [`f64`].\n\nSee the [GraphQL Float Value specification](https://spec.graphql.org/draft/#sec-Float-Value).",
  { super::float_value(inp).and_then(materialize_float::<Src, Ctx>) }
);

width_free_parser!(
  try_float_value,
  inp,
  ParseAttempt<FloatValue<f64>>,
  "Attempts a float literal, converting it to [`f64`], without consuming on a head mismatch.",
  { super::try_float_value(inp)?.and_then(materialize_float::<Src, Ctx>) }
);

// ── The five leaves it does not touch, delegating rather than repeating ──────────────────────

width_free_parser!(
  string_value,
  inp,
  StringValue<GraphqlSlice<'inp, Src>>,
  "Parses a string literal, which **keeps its source slice** — see the module header.\n\nSee the [GraphQL String Value specification](https://spec.graphql.org/draft/#sec-String-Value).",
  { super::string_value(inp) }
);

width_free_parser!(
  boolean_value,
  inp,
  BooleanValue<GraphqlSlice<'inp, Src>>,
  "Parses a boolean literal.\n\nSee the [GraphQL Boolean Value specification](https://spec.graphql.org/draft/#sec-Boolean-Value).",
  { super::boolean_value(inp) }
);

width_free_parser!(
  null_value,
  inp,
  NullValue<GraphqlSlice<'inp, Src>>,
  "Parses the `null` literal.\n\nSee the [GraphQL Null Value specification](https://spec.graphql.org/draft/#sec-Null-Value).",
  { super::null_value(inp) }
);

width_free_parser!(
  enum_value,
  inp,
  EnumValue<GraphqlSlice<'inp, Src>>,
  "Parses an enum literal.\n\nSee the [GraphQL Enum Value specification](https://spec.graphql.org/draft/#sec-Enum-Value).",
  { super::enum_value(inp) }
);

width_free_parser!(
  variable_value,
  inp,
  VariableValue<GraphqlSlice<'inp, Src>>,
  "Parses a variable reference.\n\nSee the [GraphQL Variables specification](https://spec.graphql.org/draft/#sec-Language.Variables).",
  { super::variable_value(inp) }
);

width_free_parser!(
  try_string_value,
  inp,
  ParseAttempt<StringValue<GraphqlSlice<'inp, Src>>>,
  "Attempts a string literal without consuming on a head mismatch.",
  { super::try_string_value(inp) }
);

width_free_parser!(
  try_boolean_value,
  inp,
  ParseAttempt<BooleanValue<GraphqlSlice<'inp, Src>>>,
  "Attempts a boolean literal without consuming on a head mismatch.",
  { super::try_boolean_value(inp) }
);

width_free_parser!(
  try_null_value,
  inp,
  ParseAttempt<NullValue<GraphqlSlice<'inp, Src>>>,
  "Attempts the `null` literal without consuming on a head mismatch.",
  { super::try_null_value(inp) }
);

width_free_parser!(
  try_enum_value,
  inp,
  ParseAttempt<EnumValue<GraphqlSlice<'inp, Src>>>,
  "Attempts an enum literal without consuming on a head mismatch.",
  { super::try_enum_value(inp) }
);

width_free_parser!(
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
  InputValue<GraphqlSlice<'inp, Src>, I>,
  "Parses any GraphQL input value.\n\nSee the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).",
  { super::value_with::<Src, Ctx, Materialized<I>>(inp) }
);

materialized_parser!(
  const_value,
  inp,
  ConstInputValue<GraphqlSlice<'inp, Src>, I>,
  "Parses a GraphQL constant input value.\n\nSee the [GraphQL Input Values specification](https://spec.graphql.org/draft/#sec-Input-Values).",
  { super::const_value_with::<Src, Ctx, Materialized<I>>(inp) }
);

materialized_parser!(
  list_value,
  inp,
  List<GraphqlSlice<'inp, Src>, I>,
  "Parses a list value.\n\nSee the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value).",
  { super::list_value_with::<Src, Ctx, Materialized<I>>(inp) }
);

materialized_parser!(
  const_list_value,
  inp,
  ConstList<GraphqlSlice<'inp, Src>, I>,
  "Parses a constant list value.\n\nSee the [GraphQL List Value specification](https://spec.graphql.org/draft/#sec-List-Value).",
  { super::const_list_value_with::<Src, Ctx, Materialized<I>>(inp) }
);

materialized_parser!(
  object_value,
  inp,
  Object<GraphqlSlice<'inp, Src>, I>,
  "Parses an object value.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::object_value_with::<Src, Ctx, Materialized<I>>(inp) }
);

materialized_parser!(
  const_object_value,
  inp,
  ConstObject<GraphqlSlice<'inp, Src>, I>,
  "Parses a constant object value.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::const_object_value_with::<Src, Ctx, Materialized<I>>(inp) }
);

materialized_parser!(
  object_field,
  inp,
  ObjectField<GraphqlSlice<'inp, Src>, I>,
  "Parses one object field.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::object_field_with::<Src, Ctx, Materialized<I>>(inp) }
);

materialized_parser!(
  const_object_field,
  inp,
  ConstObjectField<GraphqlSlice<'inp, Src>, I>,
  "Parses one constant object field.\n\nSee the [GraphQL Input Object Values specification](https://spec.graphql.org/draft/#sec-Input-Object-Values).",
  { super::const_object_field_with::<Src, Ctx, Materialized<I>>(inp) }
);

materialized_parser!(
  try_default_value,
  inp,
  ParseAttempt<DefaultInputValue<GraphqlSlice<'inp, Src>, I>>,
  "Attempts a default value, declining without consuming when `=` is absent.\n\nSee the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).",
  { super::try_default_value_with::<Src, Ctx, Materialized<I>>(inp) }
);

materialized_parser!(
  default_value,
  inp,
  Option<DefaultInputValue<GraphqlSlice<'inp, Src>, I>>,
  "Parses an optional default value.\n\nSee the [GraphQL Input Value Definitions specification](https://spec.graphql.org/draft/#sec-Input-Value-Definitions).",
  { super::default_value_with::<Src, Ctx, Materialized<I>>(inp) }
);

#[cfg(test)]
mod tests;
