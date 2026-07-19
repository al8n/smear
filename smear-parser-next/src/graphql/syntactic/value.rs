//! GraphQL `Value` productions — the worked template of Phase 2S.
//!
//! Every production is a free fn generic over `L: Lexer<'inp>` and the language
//! marker `Lang`, bounded only by tokora capability traits and [`ParseCtx`], with
//! `Span = SimpleSpan`. The productions stay `Lang`-generic like the atom layer and
//! the frozen `smear-parser` crate — the dialect is fixed by the AST types and the
//! error they name, not by the marker — so the entry runner pins the dialect marker
//! when it drives them.
//!
//! # Committed / declining pairs
//!
//! Following the tokora `json.rs` example, every value production comes as a
//! committed form and a declining `try_` twin. The committed form consumes and
//! errors on a wrong head; the `try_` twin obeys the position law — on decline it
//! leaves the cursor at its original position. A fixed multi-token head (`$` + name,
//! `Name ':'`, `'='` + value-head) decides via a non-consuming [`ParseChoice`] peek
//! over the exact window the head needs, then runs the committed form on a match:
//! GraphQL's heads are bounded (at most `U3`), so the decision never reaches for
//! [`InputRef::attempt`](tokora::InputRef)'s checkpoint/rollback — a decline has
//! consumed nothing *by construction*, not by rewinding. Only the two single-token
//! twins whose match turns on an already-consumed identifier's spelling
//! ([`try_boolean_value`], [`try_null_value`]) still reach for `attempt`, to roll
//! back an identifier that turned out not to be the reserved spelling they were
//! checking for. The committed dispatchers [`value`]/[`const_value`] *derive* from
//! their declining twins [`try_value`]/[`try_const_value`] the same way: the twin
//! peeks the head and runs the committed arm, and the committed form maps a decline
//! into the shared unexpected-value error.
//!
//! # Dispatch discipline
//!
//! [`try_value`] and [`try_const_value`] are the house-way alternation: a tuple of
//! committed arms behind a [`ParseChoice`] peek dispatch (plan Amendment 7). The
//! decision peeks the value head via the shared `value_head_kind` classifier — the
//! single source of truth every value-head decision reuses (the list continuation,
//! the object-field continuation, the default-value head, and both dispatchers). A
//! `$` selects the `Variable` arm only when an identifier follows it (its FIRST set
//! is two tokens, so the dispatch peeks `U2`); a `$` in a constant context, or a
//! `$` with no name, declines the choice and falls through to the unexpected-token
//! error the frozen classifier raised.
//!
//! On the identifier arm a slice compare resolves `true` / `false` / `null`
//! **before** the enum fallthrough (frozen `graphql/ast/value.rs` ordering, which
//! the spec blesses): those three spellings are the sole exclusion `EnumValue`
//! carves out of `Name`, so ruling them out first leaves the `EnumValue` arm with
//! exactly the admissible set.
//!
//! # Lists and objects — the canonical delimited-list law
//!
//! `ListValue` and `ObjectValue` are the committed-item / decision-driven-repetition
//! / strict-close shape (plan Amendments 6–7): a committed opener, the committed
//! item driven by [`repeated_while`](tokora::ParseInput::repeated_while) over a
//! positive FIRST-set decision, and a committed close. The brackets/braces are
//! dropped — parser-next's AST keeps no delimiter tokens, only the whole-node span
//! (plan Amendment 7); the frozen `scaffold::List`/`scaffold::Object` already store
//! span + elements only. Empty `[]`/`{}` are accepted (the spec allows them).
//!
//! This is deliberately *not* the many-builder's `.delimited::<D>()` chain: an
//! empirical probe (2026-07-19) showed `repeated_while(..).delimited::<D>().collect()`
//! accepts unterminated input silently (the EOI-no-close branch in tokora
//! `parser/many/delim/repeated_while.rs` is a no-op), so it would accept an
//! unterminated `[ …` against the spec and frozen parity. The committed close atom
//! (`rbracket`/`rbrace`) is used instead until tokora emits the `Unclosed`
//! diagnostic through the emitter.

use smear_lexer::{LitBlockStr, LitInlineStr};
use smear_scaffold::ast as scaffold;
use std::vec::Vec;
use tokora::{
  Accumulator, Branch, InputRef, Lexer, ParseChoice, ParseInput, SimpleSpan, Token, TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  span::AsSpan,
  token::{IdentifierToken, LitToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::{
    IntoComponents,
    typenum::{U1, U2, U3},
  },
};

use crate::{
  combinator::{
    Equivalent, ErrorOf, LiteralValueToken, ParseCtx, SliceOf, StringLiteral, colon, dollar, equal,
    ident, lbrace, lbracket, rbrace, rbracket, try_float, try_ident, try_int, try_lbrace,
    try_lbracket, try_string,
  },
  graphql::ast::{
    BooleanValue, ConstInputValue, ConstObjectField, DefaultInputValue, EnumValue, FloatValue,
    InputValue, IntValue, Name, NullValue, ObjectField, StringValue, VariableValue,
  },
};

// ─── Shared head classification ──────────────────────────────────────────────

/// The kind of value a single peeked token can begin. The one-token classification
/// every value-head decision reuses — the list/object continuation decisions, the
/// default-value head, and the dispatchers' `ParseChoice` peeks — so the admissible
/// heads have exactly one definition. `Dollar` records only that a `$` is next; the
/// dispatchers apply the second-token identifier check that a full `Variable` head
/// needs.
///
/// `pub(crate)` so sibling productions whose FIRST set ends in a value head — the
/// `Argument`/`ObjectField` list decisions — share this one classification.
#[derive(Clone, Copy)]
pub(crate) enum HeadKind {
  Int,
  Float,
  Str,
  Dollar,
  List,
  Object,
  Ident,
}

/// Classifies a single token into the [`HeadKind`] it begins, or `None` when it
/// begins no value. The sole source of truth for a value's FIRST set — reused by
/// the sibling `Argument`/`ObjectField` list decisions (hence `pub(crate)`).
#[inline]
pub(crate) fn value_head_kind<'inp, L>(t: &L::Token) -> Option<HeadKind>
where
  L: Lexer<'inp>,
  L::Token: LitToken<'inp> + PunctuatorToken<'inp> + IdentifierToken<'inp>,
{
  if <L::Token as LitToken>::is_integer_literal(t) {
    Some(HeadKind::Int)
  } else if <L::Token as LitToken>::is_float_literal(t)
    || <L::Token as LitToken>::is_hex_float_literal(t)
  {
    Some(HeadKind::Float)
  } else if <L::Token as LitToken>::is_string_literal(t) {
    Some(HeadKind::Str)
  } else if <L::Token as PunctuatorTokenExt>::is_dollar(t) {
    Some(HeadKind::Dollar)
  } else if <L::Token as PunctuatorTokenExt>::is_open_bracket(t) {
    Some(HeadKind::List)
  } else if <L::Token as PunctuatorTokenExt>::is_open_brace(t) {
    Some(HeadKind::Object)
  } else if <L::Token as IdentifierToken>::is_identifier(t) {
    Some(HeadKind::Ident)
  } else {
    None
  }
}

/// The three spellings the spec excludes from `EnumValue`, resolved on the
/// identifier arm before the enum fallthrough.
enum Reserved {
  True,
  False,
  Null,
}

/// Classifies an identifier's slice into a [`Reserved`] spelling, or `None` for an
/// ordinary enum name. Shared by the identifier dispatch arm and the standalone
/// `boolean`/`null` builders so the reserved set has one definition.
#[inline]
fn reserved_word<S>(slice: &S) -> Option<Reserved>
where
  S: Equivalent<str>,
{
  if slice.equivalent("true") {
    Some(Reserved::True)
  } else if slice.equivalent("false") {
    Some(Reserved::False)
  } else if slice.equivalent("null") {
    Some(Reserved::Null)
  } else {
    None
  }
}

// ─── Committed leaf builders ─────────────────────────────────────────────────

/// Consumes the next token as an [`IntValue`], extracting its raw slice payload.
///
/// Committed: it consumes whatever token is next and errors on anything but an
/// integer literal. The dispatchers only reach it once a peek has classified the
/// next token as an int, so the error paths are unreachable through them; they are
/// spelled out so the leaf is a sound committed parser on its own.
///
/// Spec: [IntValue](https://spec.graphql.org/draft/#IntValue).
pub fn int_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<IntValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<'inp, Int = SliceOf<'inp, L>>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match <L::Token as LiteralValueToken>::into_int(token) {
        Ok(raw) => Ok(IntValue::new(span, raw)),
        Err(token) => Err(UnexpectedToken::of(span).with_found(token).into()),
      }
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Consumes the next token as a [`FloatValue`], extracting its raw slice payload.
///
/// Committed, with the same discipline as [`int_value`].
///
/// Spec: [FloatValue](https://spec.graphql.org/draft/#FloatValue).
pub fn float_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<FloatValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<'inp, Float = SliceOf<'inp, L>>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match <L::Token as LiteralValueToken>::into_float(token) {
        Ok(raw) => Ok(FloatValue::new(span, raw)),
        Err(token) => Err(UnexpectedToken::of(span).with_found(token).into()),
      }
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Consumes the next token as a [`StringValue`] — inline or block — wrapping the
/// carrier into the node's `LitStr` via `into`.
///
/// Committed, with the same discipline as [`int_value`].
///
/// Spec: [StringValue](https://spec.graphql.org/draft/#StringValue).
pub fn string_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<StringValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<
      'inp,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match <L::Token as LiteralValueToken>::into_inline_str(token) {
        Ok(inline) => Ok(StringValue::new(span, inline.into())),
        Err(token) => match <L::Token as LiteralValueToken>::into_block_str(token) {
          Ok(block) => Ok(StringValue::new(span, block.into())),
          Err(token) => Err(UnexpectedToken::of(span).with_found(token).into()),
        },
      }
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Consumes the next token as a [`BooleanValue`] (`true` or `false`).
///
/// Committed: errors on a non-identifier, on end of input, or on an identifier that
/// is neither `true` nor `false`. Value dispatch resolves booleans through
/// `identifier_value`; this standalone builder is the committed leaf the `try_`
/// twin derives from.
///
/// Spec: [BooleanValue](https://spec.graphql.org/draft/#BooleanValue).
pub fn boolean_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<BooleanValue, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let is_ident = spanned.data().is_identifier();
      let slice = inp.slice();
      let (span, token) = spanned.into_components();
      match is_ident.then(|| reserved_word(&slice)).flatten() {
        Some(Reserved::True) => Ok(BooleanValue::new(span, true)),
        Some(Reserved::False) => Ok(BooleanValue::new(span, false)),
        _ => Err(UnexpectedToken::of(span).with_found(token).into()),
      }
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Consumes the next token as a [`NullValue`] (`null`).
///
/// Committed, with the same discipline as [`boolean_value`]: errors on anything but
/// the `null` identifier.
///
/// Spec: [NullValue](https://spec.graphql.org/draft/#NullValue).
pub fn null_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<NullValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let is_ident = spanned.data().is_identifier();
      let slice = inp.slice();
      let (span, token) = spanned.into_components();
      match is_ident.then(|| reserved_word(&slice)).flatten() {
        Some(Reserved::Null) => Ok(NullValue::new(span, slice)),
        _ => Err(UnexpectedToken::of(span).with_found(token).into()),
      }
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Consumes the next token as an [`EnumValue`]: a `Name` that is not `true`,
/// `false`, or `null`.
///
/// Committed; delegates the exclusion rule to the [`enum_value`](crate::combinator::enum_value)
/// atom (which backs both this position and `EnumValueDefinition`) and wraps the
/// admitted name into the AST node.
///
/// Spec: [EnumValue](https://spec.graphql.org/draft/#EnumValue).
pub fn enum_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<EnumValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let (span, src) = crate::combinator::enum_value(inp)?.into_components();
  Ok(EnumValue::new(span, src))
}

/// Parses a [`VariableValue`] (`$name`), committed on the leading `$`.
///
/// Spec: [Variable](https://spec.graphql.org/draft/#Variable).
pub fn variable_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<VariableValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let dollar = dollar(inp)?;
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  let span = SimpleSpan::new(dollar.span().start(), name.span().end());
  Ok(VariableValue::new(span, name))
}

// ─── try_ leaf twins ─────────────────────────────────────────────────────────

/// Declines (no tokens consumed) unless the next token is an int literal, which it
/// then consumes as an [`IntValue`].
///
/// Spec: [IntValue](https://spec.graphql.org/draft/#IntValue).
#[allow(clippy::type_complexity)]
pub fn try_int_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<IntValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<'inp, Int = SliceOf<'inp, L>>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
{
  Ok(try_int(inp)?.map(|(raw, span)| IntValue::new(span, raw)))
}

/// Declines (no tokens consumed) unless the next token is a float literal, which it
/// then consumes as a [`FloatValue`].
///
/// Spec: [FloatValue](https://spec.graphql.org/draft/#FloatValue).
#[allow(clippy::type_complexity)]
pub fn try_float_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<FloatValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<'inp, Float = SliceOf<'inp, L>>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
{
  Ok(try_float(inp)?.map(|(raw, span)| FloatValue::new(span, raw)))
}

/// Declines (no tokens consumed) unless the next token is a string literal — inline
/// or block — which it then consumes as a [`StringValue`].
///
/// Spec: [StringValue](https://spec.graphql.org/draft/#StringValue).
#[allow(clippy::type_complexity)]
pub fn try_string_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<StringValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<
      'inp,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
{
  Ok(try_string(inp)?.map(|(carrier, span)| match carrier {
    StringLiteral::Inline(inline) => StringValue::new(span, inline.into()),
    StringLiteral::Block(block) => StringValue::new(span, block.into()),
  }))
}

/// Declines (no tokens consumed) unless the next token is the identifier `true` or
/// `false`, which it then consumes as a [`BooleanValue`].
///
/// The head is a single identifier token whose spelling decides acceptance; a
/// decline (non-identifier, or an identifier that is not `true`/`false`) leaves the
/// cursor untouched.
#[allow(clippy::type_complexity)]
pub fn try_boolean_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<BooleanValue>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  let mut failed = None;
  let accepted = inp.attempt(|inp| match try_ident(inp) {
    Ok(ParseAttempt::Accept(id)) => {
      let (span, src) = id.into_components();
      match reserved_word(&src) {
        Some(Reserved::True) => Some(BooleanValue::new(span, true)),
        Some(Reserved::False) => Some(BooleanValue::new(span, false)),
        _ => None,
      }
    }
    Ok(ParseAttempt::Decline) => None,
    Err(err) => {
      failed = Some(err);
      None
    }
  });
  match failed {
    Some(err) => Err(err),
    None => Ok(match accepted {
      Some(b) => ParseAttempt::Accept(b),
      None => ParseAttempt::Decline,
    }),
  }
}

/// Declines (no tokens consumed) unless the next token is the identifier `null`,
/// which it then consumes as a [`NullValue`].
#[allow(clippy::type_complexity)]
pub fn try_null_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<NullValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  let mut failed = None;
  let accepted = inp.attempt(|inp| match try_ident(inp) {
    Ok(ParseAttempt::Accept(id)) => {
      let (span, src) = id.into_components();
      match reserved_word(&src) {
        Some(Reserved::Null) => Some(NullValue::new(span, src)),
        _ => None,
      }
    }
    Ok(ParseAttempt::Decline) => None,
    Err(err) => {
      failed = Some(err);
      None
    }
  });
  match failed {
    Some(err) => Err(err),
    None => Ok(match accepted {
      Some(n) => ParseAttempt::Accept(n),
      None => ParseAttempt::Decline,
    }),
  }
}

/// Declines (no tokens consumed) unless the next token is an `EnumValue`: an
/// identifier that is not `true`, `false`, or `null`, which it then consumes.
///
/// Spec: [EnumValue](https://spec.graphql.org/draft/#EnumValue).
#[allow(clippy::type_complexity)]
pub fn try_enum_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<EnumValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  Ok(match crate::combinator::try_enum_value(inp)? {
    ParseAttempt::Accept(id) => {
      let (span, src) = id.into_components();
      ParseAttempt::Accept(EnumValue::new(span, src))
    }
    ParseAttempt::Decline => ParseAttempt::Decline,
  })
}

/// Declines (no tokens consumed) unless the next two tokens are `$` followed by an
/// identifier, which it then consumes as a [`VariableValue`].
///
/// The head is two tokens (`$` and a name): a `U2` [`ParseChoice`] peek decides the
/// match before anything commits, so a `$` with no name (or any other head)
/// declines with nothing consumed *by construction* — no checkpoint, no rollback —
/// never committing to a missing-name error.
///
/// Spec: [Variable](https://spec.graphql.org/draft/#Variable).
#[allow(clippy::type_complexity)]
pub fn try_variable_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<VariableValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  (variable_value::<L, Ctx, Lang>,)
    .peek_then_try_choice::<_, U2>(|mut peeked: Peeked<'_, 'inp, L, U2>, _emitter| {
      let Some(dollar) = peeked.pop_front() else {
        return Ok(None);
      };
      if !dollar.token().is_dollar() {
        return Ok(None);
      }
      Ok(match peeked.pop_front() {
        Some(name) if name.token().is_identifier() => Some(Branch::B0),
        _ => None,
      })
    })
    .try_parse_input(inp)
}

// ─── Identifier dispatch arm (true / false / null / enum) ────────────────────

/// Consumes an identifier already peeked as a value head and resolves it, in order,
/// to `true` / `false` → [`BooleanValue`], `null` → [`NullValue`], otherwise
/// [`EnumValue`]. The single-consume classifier the value dispatcher uses for the
/// identifier arm.
fn identifier_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  let spanned = match inp.next()? {
    Some(spanned) => spanned,
    None => return Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  };
  let slice = inp.slice();
  let (span, _token) = spanned.into_components();
  Ok(match reserved_word(&slice) {
    Some(Reserved::True) => InputValue::Boolean(BooleanValue::new(span, true)),
    Some(Reserved::False) => InputValue::Boolean(BooleanValue::new(span, false)),
    Some(Reserved::Null) => InputValue::Null(NullValue::new(span, slice)),
    None => InputValue::Enum(EnumValue::new(span, slice)),
  })
}

/// The const twin of `identifier_value`, yielding a [`ConstInputValue`].
fn const_identifier_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstInputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  let spanned = match inp.next()? {
    Some(spanned) => spanned,
    None => return Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  };
  let slice = inp.slice();
  let (span, _token) = spanned.into_components();
  Ok(match reserved_word(&slice) {
    Some(Reserved::True) => ConstInputValue::Boolean(BooleanValue::new(span, true)),
    Some(Reserved::False) => ConstInputValue::Boolean(BooleanValue::new(span, false)),
    Some(Reserved::Null) => ConstInputValue::Null(NullValue::new(span, slice)),
    None => ConstInputValue::Enum(EnumValue::new(span, slice)),
  })
}

// ─── List / object continuation decisions ────────────────────────────────────

/// The list-continuation decision: continue ([`Action::Continue`]) while the next
/// token begins a value (`value_head_kind`), stop otherwise — at the closing
/// bracket, at end of input, or on any non-value head. A one-token positive
/// FIRST-set peek; a mid-item failure (e.g. a lone `$`) is the committed item's
/// error, not a decline.
fn decide_value_head<'inp, L, Ctx, Lang>(
  mut peeked: Peeked<'_, 'inp, L, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  L::Token: LitToken<'inp> + PunctuatorToken<'inp> + IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
{
  Ok(match peeked.pop_front() {
    Some(tok) if value_head_kind::<L>(tok.token()).is_some() => Action::Continue,
    _ => Action::Stop,
  })
}

/// The object-field-continuation decision: an `ObjectField`'s FIRST set is
/// `Name ':' Value`, so continue only while the next three tokens are an identifier,
/// a colon, and a value head (plan Amendment 7, U3 name-colon-value window). Stops
/// at the closing brace, at end of input, or on any other head — a bare identifier
/// without a colon included, so `{ a 1 }` stops and is reported by the closing-brace
/// check.
fn decide_object_field_head<'inp, L, Ctx, Lang>(
  mut peeked: Peeked<'_, 'inp, L, U3>,
  _: &mut Ctx::Emitter,
) -> Result<Action, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  L::Token: LitToken<'inp> + PunctuatorToken<'inp> + IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
{
  let Some(name) = peeked.pop_front() else {
    return Ok(Action::Stop);
  };
  if !name.token().is_identifier() {
    return Ok(Action::Stop);
  }
  let Some(colon) = peeked.pop_front() else {
    return Ok(Action::Stop);
  };
  if !colon.token().is_colon() {
    return Ok(Action::Stop);
  }
  Ok(match peeked.pop_front() {
    Some(tok) if value_head_kind::<L>(tok.token()).is_some() => Action::Continue,
    _ => Action::Stop,
  })
}

// ─── List values ─────────────────────────────────────────────────────────────

/// Parses a `ListValue` (`'[' Value* ']'`), committed on the leading `[`.
///
/// The canonical delimited-list law: the committed [`value`] item driven by
/// `decide_value_head`, then a committed [`rbracket`] close (an unterminated `[ …`
/// errors). The brackets are dropped — the AST keeps only the whole-list span (plan
/// Amendment 7); empty `[]` is accepted (the spec allows it).
///
/// Spec: [ListValue](https://spec.graphql.org/draft/#sec-List-Value).
// The `scaffold::List<InputValue<…>>` return is inherent to the node shape;
// factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn list_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<scaffold::List<InputValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  lbracket(inp)?;
  let values: Vec<InputValue<SliceOf<'inp, L>>> = value
    .repeated_while::<_, U1>(decide_value_head::<L, Ctx, Lang>)
    .collect()
    .parse_input(inp)?;
  rbracket(inp)?;
  Ok(scaffold::List::new(inp.span_since(&cursor), values))
}

/// Declines (no tokens consumed) unless the next token is `[`, in which case it
/// commits to the `ListValue` body and a required close (an unterminated `[ …`
/// errors, never declines).
///
/// Spec: [ListValue](https://spec.graphql.org/draft/#sec-List-Value).
#[allow(clippy::type_complexity)]
pub fn try_list_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<scaffold::List<InputValue<SliceOf<'inp, L>>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  match try_lbracket(inp)? {
    ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    ParseAttempt::Accept(_open) => {
      let values: Vec<InputValue<SliceOf<'inp, L>>> = value
        .repeated_while::<_, U1>(decide_value_head::<L, Ctx, Lang>)
        .collect()
        .parse_input(inp)?;
      rbracket(inp)?;
      Ok(ParseAttempt::Accept(scaffold::List::new(
        inp.span_since(&cursor),
        values,
      )))
    }
  }
}

/// The const twin of [`list_value`]: a `ListValue` of [`const_value`] items.
///
/// Spec: [ListValue](https://spec.graphql.org/draft/#sec-List-Value) (const context).
// The `scaffold::List<ConstInputValue<…>>` return is inherent to the node shape;
// factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn const_list_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<scaffold::List<ConstInputValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  lbracket(inp)?;
  let values: Vec<ConstInputValue<SliceOf<'inp, L>>> = const_value
    .repeated_while::<_, U1>(decide_value_head::<L, Ctx, Lang>)
    .collect()
    .parse_input(inp)?;
  rbracket(inp)?;
  Ok(scaffold::List::new(inp.span_since(&cursor), values))
}

/// The const twin of [`try_list_value`].
///
/// Spec: [ListValue](https://spec.graphql.org/draft/#sec-List-Value) (const context).
#[allow(clippy::type_complexity)]
pub fn try_const_list_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<
  ParseAttempt<scaffold::List<ConstInputValue<SliceOf<'inp, L>>>>,
  ErrorOf<'inp, L, Ctx, Lang>,
>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  match try_lbracket(inp)? {
    ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    ParseAttempt::Accept(_open) => {
      let values: Vec<ConstInputValue<SliceOf<'inp, L>>> = const_value
        .repeated_while::<_, U1>(decide_value_head::<L, Ctx, Lang>)
        .collect()
        .parse_input(inp)?;
      rbracket(inp)?;
      Ok(ParseAttempt::Accept(scaffold::List::new(
        inp.span_since(&cursor),
        values,
      )))
    }
  }
}

// ─── Object fields ───────────────────────────────────────────────────────────

/// Parses a (non-const) `ObjectField` (`name : Value`), committed on the name.
///
/// Spec: [ObjectField](https://spec.graphql.org/draft/#ObjectField).
pub fn object_field<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ObjectField<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  colon(inp)?;
  let value = value(inp)?;
  let span = SimpleSpan::new(name.span().start(), value.as_span().end());
  Ok(scaffold::ObjectField::new(span, name, value))
}

/// Declines (no tokens consumed) unless the next two tokens are an identifier
/// followed by a colon, in which case it commits to the field's [`value`].
///
/// The head is `Name ':'`: a `U2` [`ParseChoice`] peek decides the match over both
/// tokens before anything commits, so a name with no colon (or any other head)
/// declines with nothing consumed *by construction*, never committing to a colon
/// error.
///
/// Spec: [ObjectField](https://spec.graphql.org/draft/#ObjectField).
#[allow(clippy::type_complexity)]
pub fn try_object_field<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<ObjectField<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  (object_field::<L, Ctx, Lang>,)
    .peek_then_try_choice::<_, U2>(|mut peeked: Peeked<'_, 'inp, L, U2>, _emitter| {
      let Some(name) = peeked.pop_front() else {
        return Ok(None);
      };
      if !name.token().is_identifier() {
        return Ok(None);
      }
      Ok(match peeked.pop_front() {
        Some(colon) if colon.token().is_colon() => Some(Branch::B0),
        _ => None,
      })
    })
    .try_parse_input(inp)
}

/// Parses a constant `ObjectField` (`name : ConstValue`), committed on the name.
///
/// Spec: [ObjectField](https://spec.graphql.org/draft/#ObjectField) (const context).
pub fn const_object_field<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstObjectField<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  colon(inp)?;
  let value = const_value(inp)?;
  let span = SimpleSpan::new(name.span().start(), value.as_span().end());
  Ok(scaffold::ObjectField::new(span, name, value))
}

/// The const twin of [`try_object_field`].
///
/// Spec: [ObjectField](https://spec.graphql.org/draft/#ObjectField) (const context).
#[allow(clippy::type_complexity)]
pub fn try_const_object_field<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<ConstObjectField<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  (const_object_field::<L, Ctx, Lang>,)
    .peek_then_try_choice::<_, U2>(|mut peeked: Peeked<'_, 'inp, L, U2>, _emitter| {
      let Some(name) = peeked.pop_front() else {
        return Ok(None);
      };
      if !name.token().is_identifier() {
        return Ok(None);
      }
      Ok(match peeked.pop_front() {
        Some(colon) if colon.token().is_colon() => Some(Branch::B0),
        _ => None,
      })
    })
    .try_parse_input(inp)
}

// ─── Object values ───────────────────────────────────────────────────────────

/// Parses an `ObjectValue` (`'{' ObjectField* '}'`), committed on the leading `{`.
///
/// The canonical delimited-list law: the committed [`object_field`] item driven by
/// `decide_object_field_head`, then a committed [`rbrace`] close. The braces are
/// dropped — the AST keeps only the whole-object span; empty `{}` is accepted.
///
/// Spec: [ObjectValue](https://spec.graphql.org/draft/#sec-Input-Object-Values).
// The `scaffold::Object<Name<…>, InputValue<…>>` return is inherent to the node
// shape; factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn object_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<
  scaffold::Object<Name<SliceOf<'inp, L>>, InputValue<SliceOf<'inp, L>>>,
  ErrorOf<'inp, L, Ctx, Lang>,
>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  lbrace(inp)?;
  let fields: Vec<ObjectField<SliceOf<'inp, L>>> = object_field
    .repeated_while::<_, U3>(decide_object_field_head::<L, Ctx, Lang>)
    .collect()
    .parse_input(inp)?;
  rbrace(inp)?;
  Ok(scaffold::Object::new(inp.span_since(&cursor), fields))
}

/// Declines (no tokens consumed) unless the next token is `{`, in which case it
/// commits to the `ObjectValue` body and a required close.
///
/// Spec: [ObjectValue](https://spec.graphql.org/draft/#sec-Input-Object-Values).
#[allow(clippy::type_complexity)]
pub fn try_object_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<
  ParseAttempt<scaffold::Object<Name<SliceOf<'inp, L>>, InputValue<SliceOf<'inp, L>>>>,
  ErrorOf<'inp, L, Ctx, Lang>,
>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  match try_lbrace(inp)? {
    ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    ParseAttempt::Accept(_open) => {
      let fields: Vec<ObjectField<SliceOf<'inp, L>>> = object_field
        .repeated_while::<_, U3>(decide_object_field_head::<L, Ctx, Lang>)
        .collect()
        .parse_input(inp)?;
      rbrace(inp)?;
      Ok(ParseAttempt::Accept(scaffold::Object::new(
        inp.span_since(&cursor),
        fields,
      )))
    }
  }
}

/// The const twin of [`object_value`]: an `ObjectValue` of [`const_object_field`]s.
///
/// Spec: [ObjectValue](https://spec.graphql.org/draft/#sec-Input-Object-Values) (const context).
// The `scaffold::Object<Name<…>, ConstInputValue<…>>` return is inherent to the
// node shape; factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn const_object_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<
  scaffold::Object<Name<SliceOf<'inp, L>>, ConstInputValue<SliceOf<'inp, L>>>,
  ErrorOf<'inp, L, Ctx, Lang>,
>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  lbrace(inp)?;
  let fields: Vec<ConstObjectField<SliceOf<'inp, L>>> = const_object_field
    .repeated_while::<_, U3>(decide_object_field_head::<L, Ctx, Lang>)
    .collect()
    .parse_input(inp)?;
  rbrace(inp)?;
  Ok(scaffold::Object::new(inp.span_since(&cursor), fields))
}

/// The const twin of [`try_object_value`].
///
/// Spec: [ObjectValue](https://spec.graphql.org/draft/#sec-Input-Object-Values) (const context).
#[allow(clippy::type_complexity)]
pub fn try_const_object_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<
  ParseAttempt<scaffold::Object<Name<SliceOf<'inp, L>>, ConstInputValue<SliceOf<'inp, L>>>>,
  ErrorOf<'inp, L, Ctx, Lang>,
>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  match try_lbrace(inp)? {
    ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    ParseAttempt::Accept(_open) => {
      let fields: Vec<ConstObjectField<SliceOf<'inp, L>>> = const_object_field
        .repeated_while::<_, U3>(decide_object_field_head::<L, Ctx, Lang>)
        .collect()
        .parse_input(inp)?;
      rbrace(inp)?;
      Ok(ParseAttempt::Accept(scaffold::Object::new(
        inp.span_since(&cursor),
        fields,
      )))
    }
  }
}

// ─── Dispatch arms ───────────────────────────────────────────────────────────

/// Generates a fn-item dispatch arm that runs a committed leaf and lifts it into the
/// dispatcher's value enum. A plain fn item (not a `.map`ped parser) because `Map`'s
/// `ParseInput` impl requires a `Sized` `Lang`, which these `Lang: ?Sized`
/// productions do not have — the same reason `ty.rs`'s choice arms are fn items.
macro_rules! dispatch_arm {
  ($name:ident, $out:ident :: $variant:ident, $leaf:ident) => {
    fn $name<'inp, L, Ctx, Lang>(
      inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
    ) -> Result<$out<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
    where
      L: Lexer<'inp, Span = SimpleSpan>,
      L::Token: IdentifierToken<'inp>
        + PunctuatorToken<'inp>
        + LiteralValueToken<
          'inp,
          Int = SliceOf<'inp, L>,
          Float = SliceOf<'inp, L>,
          InlineStr = LitInlineStr<SliceOf<'inp, L>>,
          BlockStr = LitBlockStr<SliceOf<'inp, L>>,
        >,
      Ctx: ParseCtx<'inp, L, Lang>,
      Lang: ?Sized,
      SliceOf<'inp, L>: Equivalent<str> + Clone,
      ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
        + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
    {
      $leaf(inp).map($out::$variant)
    }
  };
}

dispatch_arm!(arm_int, InputValue::Int, int_value);
dispatch_arm!(arm_float, InputValue::Float, float_value);
dispatch_arm!(arm_string, InputValue::String, string_value);
dispatch_arm!(arm_variable, InputValue::Variable, variable_value);
dispatch_arm!(arm_list, InputValue::List, list_value);
dispatch_arm!(arm_object, InputValue::Object, object_value);

dispatch_arm!(arm_const_int, ConstInputValue::Int, int_value);
dispatch_arm!(arm_const_float, ConstInputValue::Float, float_value);
dispatch_arm!(arm_const_string, ConstInputValue::String, string_value);
dispatch_arm!(arm_const_list, ConstInputValue::List, const_list_value);
dispatch_arm!(
  arm_const_object,
  ConstInputValue::Object,
  const_object_value
);

// ─── Dispatchers ─────────────────────────────────────────────────────────────

/// The shared error tail of the value dispatchers: reports the offending token as
/// an unexpected token, or end of input.
fn unexpected_value<'inp, L, Ctx, T, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      Err(UnexpectedToken::of(span).with_found(token).into())
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Attempts a (non-const) `Value`, declining (no tokens consumed) when the next
/// token begins no value.
///
/// The house-way dispatch: a tuple of committed arms behind a [`ParseChoice`] peek.
/// The decision peeks the value head via `value_head_kind`; a `$` selects the
/// variable arm only when an identifier follows (the FIRST set is two tokens, so
/// the peek window is `U2`), and any other head — or a `$` with no name — declines.
///
/// Spec: [Value](https://spec.graphql.org/draft/#Value).
#[allow(clippy::type_complexity)]
pub fn try_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<InputValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  (
    arm_int::<L, Ctx, Lang>,
    arm_float::<L, Ctx, Lang>,
    arm_string::<L, Ctx, Lang>,
    arm_variable::<L, Ctx, Lang>,
    arm_list::<L, Ctx, Lang>,
    arm_object::<L, Ctx, Lang>,
    identifier_value::<L, Ctx, Lang>,
  )
    .peek_then_try_choice::<_, U2>(|mut peeked: Peeked<'_, 'inp, L, U2>, _emitter| {
      let Some(head) = peeked.pop_front() else {
        return Ok(None);
      };
      Ok(match value_head_kind::<L>(head.token()) {
        Some(HeadKind::Int) => Some(Branch::B0),
        Some(HeadKind::Float) => Some(Branch::B1),
        Some(HeadKind::Str) => Some(Branch::B2),
        Some(HeadKind::Dollar) => match peeked.pop_front() {
          Some(next) if next.token().is_identifier() => Some(Branch::B3),
          _ => None,
        },
        Some(HeadKind::List) => Some(Branch::B4),
        Some(HeadKind::Object) => Some(Branch::B5),
        Some(HeadKind::Ident) => Some(Branch::B6),
        None => None,
      })
    })
    .try_parse_input(inp)
}

/// Parses a (non-const) `Value`.
///
/// Derives from [`try_value`]: the attempt peeks the head and runs the committed
/// arm; a decline (no value head) maps to the shared unexpected-token / end-of-input
/// error the frozen classifier raised.
///
/// Spec: [Value](https://spec.graphql.org/draft/#Value).
pub fn value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match try_value(inp)? {
    ParseAttempt::Accept(value) => Ok(value),
    ParseAttempt::Decline => unexpected_value(inp),
  }
}

/// Attempts a `Value` in a constant context — the non-variable alternatives —
/// declining when the next token begins no const value.
///
/// Identical dispatch to [`try_value`] minus the `$` arm: a leading `$` declines the
/// choice (the peek window is `U1`; a variable is not a const value), so the
/// committed [`const_value`] reports it as an unexpected token.
///
/// Spec: [Value](https://spec.graphql.org/draft/#Value) (const context).
#[allow(clippy::type_complexity)]
pub fn try_const_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<ConstInputValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  (
    arm_const_int::<L, Ctx, Lang>,
    arm_const_float::<L, Ctx, Lang>,
    arm_const_string::<L, Ctx, Lang>,
    arm_const_list::<L, Ctx, Lang>,
    arm_const_object::<L, Ctx, Lang>,
    const_identifier_value::<L, Ctx, Lang>,
  )
    .peek_then_try_choice::<_, U1>(|mut peeked: Peeked<'_, 'inp, L, U1>, _emitter| {
      let Some(head) = peeked.pop_front() else {
        return Ok(None);
      };
      Ok(match value_head_kind::<L>(head.token()) {
        Some(HeadKind::Int) => Some(Branch::B0),
        Some(HeadKind::Float) => Some(Branch::B1),
        Some(HeadKind::Str) => Some(Branch::B2),
        Some(HeadKind::List) => Some(Branch::B3),
        Some(HeadKind::Object) => Some(Branch::B4),
        Some(HeadKind::Ident) => Some(Branch::B5),
        Some(HeadKind::Dollar) | None => None,
      })
    })
    .try_parse_input(inp)
}

/// Parses a `Value` in a constant context — the non-variable alternatives.
///
/// Derives from [`try_const_value`]: a leading `$` is reported as an unexpected
/// token (a variable is not a const value).
///
/// Spec: [Value](https://spec.graphql.org/draft/#Value) (const context).
pub fn const_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstInputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match try_const_value(inp)? {
    ParseAttempt::Accept(value) => Ok(value),
    ParseAttempt::Decline => unexpected_value(inp),
  }
}

// ─── Default value ───────────────────────────────────────────────────────────

/// The committed arm [`try_default_value`] dispatches to once its `U2` peek
/// confirms `=` followed by a value head: consumes the `=` and the committed
/// [`const_value`], producing the `DefaultInputValue`.
///
/// A plain fn item — like the "Dispatch arms" below — because a `.map()`ped closure
/// would require a `Sized` `Lang`, which these `Lang: ?Sized` productions do not
/// have.
fn committed_default_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<DefaultInputValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  equal(inp)?;
  let value = const_value(inp)?;
  Ok(DefaultInputValue::new(inp.span_since(&cursor), value))
}

/// Attempts a `DefaultValue` (`= ConstValue`), declining (no tokens consumed) unless
/// the next two tokens are `=` followed by a value head (plan Amendment 7, U2
/// `=`-then-value-head window).
///
/// The window is decided by a [`ParseChoice`] peek, not a checkpoint/rollback
/// attempt: an `=` with no value head declines with nothing consumed *by
/// construction*, leaving the cursor at the `=` (position law). Once the head
/// matches, the const value is committed — `= $v` commits and then errors (a
/// variable is not a const value).
///
/// Spec: [DefaultValue](https://spec.graphql.org/draft/#DefaultValue).
#[allow(clippy::type_complexity)]
pub fn try_default_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<DefaultInputValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  (committed_default_value::<L, Ctx, Lang>,)
    .peek_then_try_choice::<_, U2>(|mut peeked: Peeked<'_, 'inp, L, U2>, _emitter| {
      let Some(eq) = peeked.pop_front() else {
        return Ok(None);
      };
      if !eq.token().is_equal() {
        return Ok(None);
      }
      Ok(match peeked.pop_front() {
        Some(tok) if value_head_kind::<L>(tok.token()).is_some() => Some(Branch::B0),
        _ => None,
      })
    })
    .try_parse_input(inp)
}

/// Parses an optional `DefaultValue` (`= ConstValue`).
///
/// Derives from [`try_default_value`]: an `=` followed by a value head yields the
/// default; anything else (no `=`, or `=` with no value head) declines to `None`
/// with the cursor left in place.
///
/// Spec: [DefaultValue](https://spec.graphql.org/draft/#DefaultValue).
// The `Result<Option<…>, …>` return is inherent to an optional generic production.
#[allow(clippy::type_complexity)]
pub fn default_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<DefaultInputValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  Ok(match try_default_value(inp)? {
    ParseAttempt::Accept(default) => Some(default),
    ParseAttempt::Decline => None,
  })
}

#[cfg(test)]
mod tests;
