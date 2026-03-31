//! Generic combinator-based parsers for GraphQL AST nodes.
//!
//! These parsers are generic over the source type `S` and can be used
//! with `&str`, `&[u8]`, `bytes::Bytes`, or any other source type that
//! satisfies the required trait bounds.

use std::boxed::Box;
use std::vec::Vec;

use smear_lexer::tokit::{
  Accumulator, Branch, Emitter, InputRef, Lexer, ParseChoice, ParseContext, ParseInput,
  SimpleSpan as Span,
  cache::Peeked,
  emitter::FullContainerEmitter,
  emitter::{SeparatedEmitter, TooFewEmitter, UnexpectedLeadingSeparatorEmitter, UnexpectedTrailingSeparatorEmitter},
  lexer::FromLogos,
  parser::Action,
  span::Spanned,
  token::{PunctuatorToken, KeywordToken},
  utils::{Maybe, cmp::Equivalent, typenum::{U1, U3}},
};
use smear_scaffold::ast::{self as scaffold, FragmentName};

use super::{
  Expectation, SyntacticTokenError, SyntacticTokenErrors, next_token,
  name::{Name, parse_name},
  value::*,
  keyword::*,
  location::parse_location,
  operation_type::parse_operation_type,
  default::*,
  fragment::*,
  ty::Type,
  type_system::*,
};
use crate::lexer::graphql::syntactic::{
  SyntacticLexer, SyntacticToken, SyntacticTokenKind,
};

// ─── Peek helper ─────────────────────────────────────────────────────────────

/// Helper macro to uniformly access the `&SyntacticToken<S>` from a peeked
/// `Maybe<CachedTokenRef, CachedToken>` element (the `Ref` variant has an extra
/// level of indirection).
macro_rules! with_peeked_token {
  ($tok:expr, |$t:ident| $body:expr) => {
    match $tok {
      Maybe::Ref(cached) => {
        let $t: &SyntacticToken<S> = *cached.token().data();
        $body
      }
      Maybe::Owned(cached) => {
        let $t: &SyntacticToken<S> = cached.token().data();
        $body
      }
    }
  };
}

/// Decision closure: stop when the next token is the given kind (or EOF).
macro_rules! stop_at {
  ($kind:expr) => {
    |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U1>, _: &mut _| {
      Ok(match peeked.pop_front() {
        None => Action::Stop,
        Some(tok) => with_peeked_token!(&tok, |t| {
          if t.kind() == $kind { Action::Stop } else { Action::Continue }
        }),
      })
    }
  };
}

/// Decision closure: continue while the next token is the given kind.
macro_rules! continue_while {
  ($kind:expr) => {
    |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U1>, _: &mut _| {
      Ok(match peeked.pop_front() {
        None => Action::Stop,
        Some(tok) => with_peeked_token!(&tok, |t| {
          if t.kind() == $kind { Action::Continue } else { Action::Stop }
        }),
      })
    }
  };
}

/// Decision closure: continue while there is any token (stop at EOF).
macro_rules! continue_while_some {
  () => {
    |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U1>, _: &mut _| {
      Ok(match peeked.pop_front() {
        None => Action::Stop,
        Some(_) => Action::Continue,
      })
    }
  };
}

/// Decision closure: continue while the next token is a non-keyword identifier.
/// Uses `KeywordToken::is_keyword()` for fast dispatch — no string comparison.
macro_rules! continue_while_name {
  () => {
    |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U1>, _: &mut _| {
      Ok(match peeked.pop_front() {
        None => Action::Stop,
        Some(tok) => with_peeked_token!(&tok, |t| {
          use KeywordToken;
          if t.kind() == SyntacticTokenKind::Identifier && !t.is_keyword() {
            Action::Continue
          } else {
            Action::Stop
          }
        }),
      })
    }
  };
}

// ─── Helper functions ────────────────────────────────────────────────────────

/// Peek at the next token kind without consuming it.
#[inline]
pub fn peek_kind<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Option<SyntacticTokenKind>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let mut kind = None;
  let _ = input.try_expect(|spanned| {
    kind = Some(spanned.data().kind());
    false
  });
  kind
}

/// Check if the peeked identifier matches a keyword.
#[inline]
pub fn peek_keyword<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
  kw: &str,
) -> bool
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let mut found = false;
  let _ = input.try_expect(|spanned| {
    found = match spanned.data() {
      SyntacticToken::Identifier(s) => kw.equivalent(s),
      _ => false,
    };
    false
  });
  found
}

/// Try to consume a specific token kind, return None if not matched.
#[inline]
pub fn try_token<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
  expected: SyntacticTokenKind,
) -> Result<Option<Spanned<SyntacticToken<S>>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) == Some(expected) {
    Ok(Some(next_token(input)?))
  } else {
    Ok(None)
  }
}

/// Expect and consume a specific token kind.
#[inline]
pub fn expect_token<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
  expected: SyntacticTokenKind,
) -> Result<Spanned<SyntacticToken<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let tok = next_token(input)?;
  if tok.data().kind() == expected {
    Ok(tok)
  } else {
    Err(SyntacticTokenError::unexpected_token(
      tok.data().clone(),
      Expectation::from(expected),
      tok.span(),
    ).into())
  }
}

// ─── Description ─────────────────────────────────────────────────────────────

/// Parses an optional description (StringValue).
pub fn parse_description<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<StringValue<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  match peek_kind(input) {
    Some(SyntacticTokenKind::InlineString | SyntacticTokenKind::BlockString) => {
      let tok = next_token(input)?;
      let span = tok.span();
      match tok.into_data() {
        SyntacticToken::LitInlineStr(s) => Ok(Some(StringValue::new(span, s.into()))),
        SyntacticToken::LitBlockStr(s) => Ok(Some(StringValue::new(span, s.into()))),
        _ => unreachable!(),
      }
    }
    _ => Ok(None),
  }
}

// ─── Type ────────────────────────────────────────────────────────────────────

/// Parses a GraphQL type (named, list, non-null).
pub fn parse_type<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Type<Name<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let eot_span = Span::new(0, 0);
  (
    |input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let cursor = input.cursor().clone();
      next_token(input)?; // consume '['
      let inner = parse_type(input)?;
      expect_token(input, SyntacticTokenKind::RBracket)?;
      let required = try_token(input, SyntacticTokenKind::Bang)?.is_some();
      let span = input.span_since(&cursor);
      Ok(Type::List(Box::new(scaffold::ListType::new(span, inner, required))))
    },
    |input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let cursor = input.cursor().clone();
      let name = parse_name(input)?;
      let required = try_token(input, SyntacticTokenKind::Bang)?.is_some();
      let span = input.span_since(&cursor);
      Ok(Type::Name(scaffold::NamedType::new(span, name, required)))
    },
  )
    .peek_then_choice::<_, U1>(
      |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U1>, _emitter| {
        match peeked.pop_front() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(eot_span).into()),
          Some(tok) => with_peeked_token!(&tok, |t| match t.kind() {
            SyntacticTokenKind::LBracket => Ok(Branch::B0),
            SyntacticTokenKind::Identifier => Ok(Branch::B1),
            _ => Err(SyntacticTokenError::unexpected_token(t.clone(), Expectation::Name, eot_span).into()),
          }),
        }
      },
    )
    .parse_input(input)
}

// ─── Const Arguments & Directives ────────────────────────────────────────────

/// Parses a constant argument: `Name ':' ConstValue`.
pub fn parse_const_argument<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<ConstArgument<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let name = parse_name(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let value = parse_const_input_value(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Argument::new(span, name, value))
}

/// Parses optional constant arguments: `'(' ConstArgument+ ')'`.
pub fn parse_const_arguments<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<ConstArguments<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) != Some(SyntacticTokenKind::LParen) { return Ok(None); }
  let cursor = input.cursor().clone();
  let l = smear_lexer::punctuator::LParen::new(next_token(input)?.into_span());
  let args = (parse_const_argument as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(stop_at!(SyntacticTokenKind::RParen))
    .collect()
    .parse_input(input)?;
  let r = smear_lexer::punctuator::RParen::new(next_token(input)?.into_span());
  let span = input.span_since(&cursor);
  Ok(Some(scaffold::Arguments::new(span, l, args, r)))
}

/// Parses a constant directive: `'@' Name ConstArguments?`.
pub fn parse_const_directive<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<ConstDirective<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::At)?;
  let name = parse_name(input)?;
  let args = parse_const_arguments(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Directive::new(span, name, args))
}

/// Parses optional constant directives: `Directive+ (peek '@')`.
pub fn parse_const_directives<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<ConstDirectives<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) != Some(SyntacticTokenKind::At) { return Ok(None); }
  let cursor = input.cursor().clone();
  let ds = (parse_const_directive as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(continue_while!(SyntacticTokenKind::At))
    .collect()
    .parse_input(input)?;
  let span = input.span_since(&cursor);
  Ok(Some(scaffold::Directives::new(span, ds)))
}

// ─── Executable Arguments & Directives ───────────────────────────────────────

/// Parses an argument: `Name ':' Value`.
pub fn parse_argument<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Argument<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let name = parse_name(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let value = parse_input_value(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Argument::new(span, name, value))
}

/// Parses optional arguments: `'(' Argument+ ')'`.
pub fn parse_arguments<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<Arguments<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) != Some(SyntacticTokenKind::LParen) { return Ok(None); }
  let cursor = input.cursor().clone();
  let l = smear_lexer::punctuator::LParen::new(next_token(input)?.into_span());
  let args = (parse_argument as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(stop_at!(SyntacticTokenKind::RParen))
    .collect()
    .parse_input(input)?;
  let r = smear_lexer::punctuator::RParen::new(next_token(input)?.into_span());
  let span = input.span_since(&cursor);
  Ok(Some(scaffold::Arguments::new(span, l, args, r)))
}

/// Parses a directive: `'@' Name Arguments?`.
pub fn parse_directive<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Directive<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::At)?;
  let name = parse_name(input)?;
  let args = parse_arguments(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Directive::new(span, name, args))
}

/// Parses optional directives: `Directive+ (peek '@')`.
pub fn parse_directives<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<Directives<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) != Some(SyntacticTokenKind::At) { return Ok(None); }
  let cursor = input.cursor().clone();
  let ds = (|input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
    let c2 = input.cursor().clone();
    expect_token(input, SyntacticTokenKind::At)?;
    let name = parse_name(input)?;
    let args = parse_arguments(input)?;
    Ok(scaffold::Directive::new(input.span_since(&c2), name, args))
  })
    .repeated_while::<_, U1>(continue_while!(SyntacticTokenKind::At))
    .collect()
    .parse_input(input)?;
  let span = input.span_since(&cursor);
  Ok(Some(scaffold::Directives::new(span, ds)))
}

// ─── Default Value ───────────────────────────────────────────────────────────

/// Parses an optional default value: `'=' ConstValue`.
pub fn parse_default_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<DefaultInputValue<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) != Some(SyntacticTokenKind::Equal) { return Ok(None); }
  let cursor = input.cursor().clone();
  next_token(input)?;
  let value = parse_const_input_value(input)?;
  Ok(Some(scaffold::DefaultInputValue::new(input.span_since(&cursor), value)))
}

// ─── Input Value / Arguments Definitions ─────────────────────────────────────

/// Parses an input value definition: `Description? Name ':' Type Default? Directives?`.
pub fn parse_input_value_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<InputValueDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let desc = parse_description(input)?;
  let name = parse_name(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let ty = parse_type(input)?;
  let dv = parse_default_value(input)?;
  let dirs = parse_const_directives(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Described::new(span, desc, scaffold::InputValueDefinition::new(span, name, ty, dv, dirs)))
}

/// Parses arguments definition: `'(' InputValueDefinition+ ')'`.
pub fn parse_arguments_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<ArgumentsDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LParen)?;
  let defs = (parse_input_value_definition as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(stop_at!(SyntacticTokenKind::RParen))
    .collect()
    .parse_input(input)?;
  expect_token(input, SyntacticTokenKind::RParen)?;
  Ok(scaffold::ArgumentsDefinition::new(input.span_since(&cursor), defs))
}

/// Parses optional arguments definition.
pub fn parse_opt_arguments_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<ArgumentsDefinition<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) == Some(SyntacticTokenKind::LParen) {
    Ok(Some(parse_arguments_definition(input)?))
  } else {
    Ok(None)
  }
}

// ─── Field / Fields Definition ───────────────────────────────────────────────

/// Parses a field definition: `Description? Name ArgumentsDef? ':' Type Directives?`.
pub fn parse_field_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<FieldDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let desc = parse_description(input)?;
  let name = parse_name(input)?;
  let args = parse_opt_arguments_definition(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let ty = parse_type(input)?;
  let dirs = parse_const_directives(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Described::new(span, desc, scaffold::FieldDefinition::new(span, name, args, ty, dirs)))
}

/// Parses optional fields definition: `'{' FieldDefinition+ '}'`.
pub fn parse_fields_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<FieldsDefinition<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) != Some(SyntacticTokenKind::LBrace) { return Ok(None); }
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LBrace)?;
  let fs = (parse_field_definition as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(stop_at!(SyntacticTokenKind::RBrace))
    .collect()
    .parse_input(input)?;
  expect_token(input, SyntacticTokenKind::RBrace)?;
  Ok(Some(scaffold::FieldsDefinition::new(input.span_since(&cursor), fs)))
}

/// Parses optional input fields definition: `'{' InputValueDefinition+ '}'`.
pub fn parse_input_fields_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<InputFieldsDefinition<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) != Some(SyntacticTokenKind::LBrace) { return Ok(None); }
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LBrace)?;
  let fs = (parse_input_value_definition as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(stop_at!(SyntacticTokenKind::RBrace))
    .collect()
    .parse_input(input)?;
  expect_token(input, SyntacticTokenKind::RBrace)?;
  Ok(Some(scaffold::InputFieldsDefinition::new(input.span_since(&cursor), fs)))
}

// ─── Implements / Union Members / Directive Locations / Enum Values ──────────

/// Parses optional implements interfaces: `'implements' '&'? Name ('&' Name)*`.
pub fn parse_implements<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<scaffold::ImplementInterfaces<Name<S>>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if !peek_keyword(input, "implements") { return Ok(None); }
  let cursor = input.cursor().clone();
  next_token(input)?;
  // &? Name (& Name)* — allow optional leading &, stop at keywords
  let ns: Vec<_> = (|input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| parse_name(input))
    .separated_while::<smear_lexer::tokit::punct::Ampersand<(), (), Lang>, _, U1>(continue_while_name!())
    .allow_leading()
    .at_least(1)
    .collect()
    .parse_input(input)?;
  Ok(Some(scaffold::ImplementInterfaces::new(input.span_since(&cursor), ns)))
}

/// Parses optional union member types: `'=' '|'? Name ('|' Name)*`.
pub fn parse_union_members<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<scaffold::UnionMemberTypes<Name<S>>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) != Some(SyntacticTokenKind::Equal) { return Ok(None); }
  let cursor = input.cursor().clone();
  next_token(input)?;
  // |? Name (| Name)* — allow optional leading |, stop at keywords
  let ms: Vec<_> = (|input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| parse_name(input))
    .separated_while::<smear_lexer::tokit::punct::Pipe<(), (), Lang>, _, U1>(continue_while_name!())
    .allow_leading()
    .at_least(1)
    .collect()
    .parse_input(input)?;
  Ok(Some(scaffold::UnionMemberTypes::new(input.span_since(&cursor), ms)))
}

/// Parses directive locations: `'|'? Location ('|' Location)*`.
pub fn parse_directive_locations<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<scaffold::DirectiveLocations<scaffold::Location>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  // |? Location (| Location)+ — allow optional leading |, at least 1 location, stop at keywords
  let locs: Vec<_> = (|input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| parse_location(input))
    .separated_while::<smear_lexer::tokit::punct::Pipe<(), (), Lang>, _, U1>(continue_while_name!())
    .allow_leading()
    .at_least(1)
    .collect()
    .parse_input(input)?;
  Ok(scaffold::DirectiveLocations::new(input.span_since(&cursor), locs))
}

/// Parses an enum value definition: `Description? Name Directives?`.
pub fn parse_enum_value_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<EnumValueDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let desc = parse_description(input)?;
  let name = parse_name(input)?;
  let dirs = parse_const_directives(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Described::new(span, desc, scaffold::EnumValueDefinition::new(span, name, dirs)))
}

/// Parses optional enum values definition: `'{' EnumValueDefinition+ '}'`.
pub fn parse_enum_values_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<EnumValuesDefinition<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) != Some(SyntacticTokenKind::LBrace) { return Ok(None); }
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LBrace)?;
  let vs = (parse_enum_value_definition as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(stop_at!(SyntacticTokenKind::RBrace))
    .collect()
    .parse_input(input)?;
  expect_token(input, SyntacticTokenKind::RBrace)?;
  Ok(Some(scaffold::EnumValuesDefinition::new(input.span_since(&cursor), vs)))
}

// ─── Selection Set ───────────────────────────────────────────────────────────

/// Parses a field: `Alias? Name Arguments? Directives? SelectionSet?`.
pub fn parse_field<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<scaffold::Field<Alias<S>, Name<S>, Arguments<S>, Directives<S>, SelectionSet<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let name1 = parse_name(input)?;
  let (alias, name) = if peek_kind(input) == Some(SyntacticTokenKind::Colon) {
    next_token(input)?;
    let n2 = parse_name(input)?;
    (Some(scaffold::Alias::new(input.span_since(&cursor), name1)), n2)
  } else {
    (None, name1)
  };
  let args = parse_arguments(input)?;
  let dirs = parse_directives(input)?;
  let ss = if peek_kind(input) == Some(SyntacticTokenKind::LBrace) {
    Some(parse_selection_set(input)?)
  } else {
    None
  };
  Ok(scaffold::Field::new(input.span_since(&cursor), alias, name, args, dirs, ss))
}

/// Parses a selection: field, fragment spread, or inline fragment.
///
/// Hand-rolled dispatch for performance — `parse_selection` is the hottest function
/// in executable query parsing. Uses `try_expect` (fast cache check) instead of
/// `peek_then_choice` (which constructs a `Peeked` deque on every call).
#[inline]
pub fn parse_selection<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Selection<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  // Fast path: ~90%+ of selections are fields (Identifier token).
  // Only spread (...) needs further dispatch.
  if peek_kind(input) != Some(SyntacticTokenKind::Spread) {
    return parse_field(input).map(|f| Selection::Field(f.into()));
  }

  // Spread: consume `...` then dispatch on next token
  let cursor = input.cursor().clone();
  next_token(input)?; // consume ...

  if peek_keyword(input, "on") {
    // ... on TypeCondition Directives? SelectionSet
    next_token(input)?; // consume "on"
    let tn = parse_name(input)?;
    let tc = TypeCondition::new(input.span_since(&cursor), tn);
    let dirs = parse_directives(input)?;
    let ss = parse_selection_set(input)?;
    Ok(Selection::InlineFragment(scaffold::InlineFragment::new(input.span_since(&cursor), Some(tc), dirs, ss)))
  } else if peek_kind(input) == Some(SyntacticTokenKind::LBrace) || peek_kind(input) == Some(SyntacticTokenKind::At) {
    // ... Directives? SelectionSet  (inline fragment without type condition)
    let dirs = parse_directives(input)?;
    let ss = parse_selection_set(input)?;
    Ok(Selection::InlineFragment(scaffold::InlineFragment::new(input.span_since(&cursor), None, dirs, ss)))
  } else {
    // ... Name Directives?  (fragment spread)
    let name = parse_name(input)?;
    let fname = FragmentName::new(name.span().clone(), name.source_ref().clone());
    let dirs = parse_directives(input)?;
    Ok(Selection::FragmentSpread(scaffold::FragmentSpread::new(input.span_since(&cursor), fname, dirs)))
  }
}

/// Parses a selection set: `'{' Selection+ '}'`.
pub fn parse_selection_set<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<SelectionSet<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LBrace)?;
  let mut sels = Vec::new();
  while peek_kind(input) != Some(SyntacticTokenKind::RBrace) && peek_kind(input).is_some() {
    sels.push(parse_selection(input)?);
  }
  expect_token(input, SyntacticTokenKind::RBrace)?;
  Ok(scaffold::SelectionSet::new(input.span_since(&cursor), sels))
}

// ─── Variable Definitions ────────────────────────────────────────────────────

/// Parses a variable value: `'$' Name`.
pub fn parse_variable_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<VariableValue<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::Dollar)?;
  let name = parse_name(input)?;
  Ok(VariableValue::new(input.span_since(&cursor), name))
}

/// Parses a variable definition: `Description? '$' Name ':' Type Default? Directives?`.
pub fn parse_variable_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<DescribedVariableDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let desc = parse_description(input)?;
  let var = parse_variable_value(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let ty = parse_type(input)?;
  let dv = parse_default_value(input)?;
  let dirs = parse_directives(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Described::new(span, desc, scaffold::VariableDefinition::new(span, var, ty, dirs, dv)))
}

/// Parses optional variables definition: `'(' VariableDefinition+ ')'`.
pub fn parse_variables_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Option<VariablesDefinition<S>>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  if peek_kind(input) != Some(SyntacticTokenKind::LParen) { return Ok(None); }
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LParen)?;
  let vs = (parse_variable_definition as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(stop_at!(SyntacticTokenKind::RParen))
    .collect()
    .parse_input(input)?;
  expect_token(input, SyntacticTokenKind::RParen)?;
  Ok(Some(scaffold::VariablesDefinition::new(input.span_since(&cursor), vs)))
}

// ─── Root Operation Types ────────────────────────────────────────────────────

/// Parses a root operation type definition: `OperationType ':' Name`.
pub fn parse_root_operation_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<RootOperationTypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let op = parse_operation_type(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let name = parse_name(input)?;
  Ok(scaffold::RootOperationTypeDefinition::new(input.span_since(&cursor), op, name))
}

/// Parses root operation types definition: `'{' RootOperationTypeDefinition+ '}'`.
pub fn parse_root_operation_types_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<RootOperationTypesDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LBrace)?;
  let ds = (parse_root_operation_type_definition as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(stop_at!(SyntacticTokenKind::RBrace))
    .collect()
    .parse_input(input)?;
  expect_token(input, SyntacticTokenKind::RBrace)?;
  Ok(scaffold::RootOperationTypesDefinition::new(input.span_since(&cursor), ds))
}

// ─── Type System Definitions ─────────────────────────────────────────────────

/// Parses a scalar type definition.
pub fn parse_scalar_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<ScalarTypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  parse_scalar(input)?;
  let name = parse_name(input)?;
  let dirs = parse_const_directives(input)?;
  Ok(scaffold::ScalarTypeDefinition::new(input.span_since(&cursor), name, dirs))
}

/// Parses an object type definition.
pub fn parse_object_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<ObjectTypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  parse_type_kw(input)?;
  let name = parse_name(input)?;
  let impls = parse_implements(input)?;
  let dirs = parse_const_directives(input)?;
  let fields = parse_fields_definition(input)?;
  Ok(scaffold::ObjectTypeDefinition::new(input.span_since(&cursor), name, impls, dirs, fields))
}

/// Parses an interface type definition.
pub fn parse_interface_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<InterfaceTypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  parse_interface(input)?;
  let name = parse_name(input)?;
  let impls = parse_implements(input)?;
  let dirs = parse_const_directives(input)?;
  let fields = parse_fields_definition(input)?;
  Ok(scaffold::InterfaceTypeDefinition::new(input.span_since(&cursor), name, impls, dirs, fields))
}

/// Parses a union type definition.
pub fn parse_union_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<UnionTypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  parse_union(input)?;
  let name = parse_name(input)?;
  let dirs = parse_const_directives(input)?;
  let members = parse_union_members(input)?;
  Ok(scaffold::UnionTypeDefinition::new(input.span_since(&cursor), name, dirs, members))
}

/// Parses an enum type definition.
pub fn parse_enum_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<EnumTypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  parse_enum(input)?;
  let name = parse_name(input)?;
  let dirs = parse_const_directives(input)?;
  let vals = parse_enum_values_definition(input)?;
  Ok(scaffold::EnumTypeDefinition::new(input.span_since(&cursor), name, dirs, vals))
}

/// Parses an input object type definition.
pub fn parse_input_object_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<InputObjectTypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  parse_input_kw(input)?;
  let name = parse_name(input)?;
  let dirs = parse_const_directives(input)?;
  let fields = parse_input_fields_definition(input)?;
  Ok(scaffold::InputObjectTypeDefinition::new(input.span_since(&cursor), name, dirs, fields))
}

/// Parses a directive definition.
pub fn parse_directive_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<DirectiveDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  parse_directive_kw(input)?;
  expect_token(input, SyntacticTokenKind::At)?;
  let name = parse_name(input)?;
  let args = parse_opt_arguments_definition(input)?;
  let rep = peek_keyword(input, "repeatable");
  if rep { next_token(input)?; }
  parse_on(input)?;
  let locs = parse_directive_locations(input)?;
  Ok(scaffold::DirectiveDefinition::new(input.span_since(&cursor), name, args, rep, locs))
}

/// Parses a schema definition.
pub fn parse_schema_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<SchemaDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  parse_schema(input)?;
  let dirs = parse_const_directives(input)?;
  let ops = parse_root_operation_types_definition(input)?;
  Ok(scaffold::SchemaDefinition::new(input.span_since(&cursor), dirs, ops))
}

/// Parses a type definition by dispatching on keyword.
pub fn parse_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<TypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let eot_span = Span::new(0, 0);
  (
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| parse_scalar_type_definition(inp).map(TypeDefinition::Scalar)),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| parse_object_type_definition(inp).map(TypeDefinition::Object)),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| parse_interface_type_definition(inp).map(TypeDefinition::Interface)),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| parse_union_type_definition(inp).map(TypeDefinition::Union)),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| parse_enum_type_definition(inp).map(TypeDefinition::Enum)),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| parse_input_object_type_definition(inp).map(TypeDefinition::InputObject)),
  )
    .peek_then_choice::<_, U1>(
      |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U1>, _emitter| {
        match peeked.pop_front() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(eot_span).into()),
          Some(tok) => with_peeked_token!(&tok, |t| match t {
            SyntacticToken::Identifier(s) if "scalar".equivalent(s) => Ok(Branch::B0),
            SyntacticToken::Identifier(s) if "type".equivalent(s) => Ok(Branch::B1),
            SyntacticToken::Identifier(s) if "interface".equivalent(s) => Ok(Branch::B2),
            SyntacticToken::Identifier(s) if "union".equivalent(s) => Ok(Branch::B3),
            SyntacticToken::Identifier(s) if "enum".equivalent(s) => Ok(Branch::B4),
            SyntacticToken::Identifier(s) if "input".equivalent(s) => Ok(Branch::B5),
            _ => Err(SyntacticTokenError::unexpected_token(t.clone(), Expectation::Name, eot_span).into()),
          }),
        }
      },
    )
    .parse_input(input)
}

// ─── Extensions ──────────────────────────────────────────────────────────────

/// Parses a type extension by dispatching on keyword.
pub fn parse_type_extension<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<TypeExtension<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let eot_span = Span::new(0, 0);
  (
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let c = inp.cursor().clone();
      parse_scalar(inp)?;
      let name = parse_name(inp)?;
      let dirs = parse_const_directives(inp)?;
      let span = inp.span_since(&c);
      Ok(TypeExtension::Scalar(scaffold::ScalarTypeExtension::new(span, name, dirs.unwrap_or_else(|| scaffold::Directives::new(span, Vec::new())))))
    }),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let c = inp.cursor().clone();
      parse_type_kw(inp)?;
      let name = parse_name(inp)?;
      let impls = parse_implements(inp)?;
      let dirs = parse_const_directives(inp)?;
      let fields = parse_fields_definition(inp)?;
      let span = inp.span_since(&c);
      let data = match (impls, dirs, fields) {
        (i, d, Some(f)) => scaffold::ObjectTypeExtensionData::Fields { implements: i, directives: d, fields: f },
        (i, Some(d), None) => scaffold::ObjectTypeExtensionData::Directives { implements: i, directives: d },
        _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
      };
      Ok(TypeExtension::Object(scaffold::ObjectTypeExtension::new(span, name, data)))
    }),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let c = inp.cursor().clone();
      parse_interface(inp)?;
      let name = parse_name(inp)?;
      let impls = parse_implements(inp)?;
      let dirs = parse_const_directives(inp)?;
      let fields = parse_fields_definition(inp)?;
      let span = inp.span_since(&c);
      let data = match (impls, dirs, fields) {
        (i, d, Some(f)) => scaffold::InterfaceTypeExtensionData::Fields { implements: i, directives: d, fields: f },
        (i, Some(d), None) => scaffold::InterfaceTypeExtensionData::Directives { implements: i, directives: d },
        _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
      };
      Ok(TypeExtension::Interface(scaffold::InterfaceTypeExtension::new(span, name, data)))
    }),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let c = inp.cursor().clone();
      parse_union(inp)?;
      let name = parse_name(inp)?;
      let dirs = parse_const_directives(inp)?;
      let members = parse_union_members(inp)?;
      let span = inp.span_since(&c);
      let data = match (dirs, members) {
        (d, Some(m)) => scaffold::UnionTypeExtensionData::Members { directives: d, members: m },
        (Some(d), None) => scaffold::UnionTypeExtensionData::Directives(d),
        _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
      };
      Ok(TypeExtension::Union(scaffold::UnionTypeExtension::new(span, name, data)))
    }),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let c = inp.cursor().clone();
      parse_enum(inp)?;
      let name = parse_name(inp)?;
      let dirs = parse_const_directives(inp)?;
      let vals = parse_enum_values_definition(inp)?;
      let span = inp.span_since(&c);
      let data = match (dirs, vals) {
        (d, Some(v)) => scaffold::EnumTypeExtensionData::Values { directives: d, values: v },
        (Some(d), None) => scaffold::EnumTypeExtensionData::Directives(d),
        _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
      };
      Ok(TypeExtension::Enum(scaffold::EnumTypeExtension::new(span, name, data)))
    }),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let c = inp.cursor().clone();
      parse_input_kw(inp)?;
      let name = parse_name(inp)?;
      let dirs = parse_const_directives(inp)?;
      let fields = parse_input_fields_definition(inp)?;
      let span = inp.span_since(&c);
      let data = match (dirs, fields) {
        (d, Some(f)) => scaffold::InputObjectTypeExtensionData::Fields { directives: d, fields: f },
        (Some(d), None) => scaffold::InputObjectTypeExtensionData::Directives(d),
        _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
      };
      Ok(TypeExtension::InputObject(scaffold::InputObjectTypeExtension::new(span, name, data)))
    }),
  )
    .peek_then_choice::<_, U1>(
      |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U1>, _emitter| {
        match peeked.pop_front() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(eot_span).into()),
          Some(tok) => with_peeked_token!(&tok, |t| match t {
            SyntacticToken::Identifier(s) if "scalar".equivalent(s) => Ok(Branch::B0),
            SyntacticToken::Identifier(s) if "type".equivalent(s) => Ok(Branch::B1),
            SyntacticToken::Identifier(s) if "interface".equivalent(s) => Ok(Branch::B2),
            SyntacticToken::Identifier(s) if "union".equivalent(s) => Ok(Branch::B3),
            SyntacticToken::Identifier(s) if "enum".equivalent(s) => Ok(Branch::B4),
            SyntacticToken::Identifier(s) if "input".equivalent(s) => Ok(Branch::B5),
            _ => Err(SyntacticTokenError::unexpected_token(t.clone(), Expectation::Name, eot_span).into()),
          }),
        }
      },
    )
    .parse_input(input)
}

/// Parses a type system extension: `'extend' (SchemaExtension | TypeExtension)`.
pub fn parse_type_system_extension<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<TypeSystemExtension<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  parse_extend(input)?;
  (
    |inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let c = inp.cursor().clone();
      parse_schema(inp)?;
      let dirs = parse_const_directives(inp)?;
      let ops = if peek_kind(inp) == Some(SyntacticTokenKind::LBrace) {
        Some(parse_root_operation_types_definition(inp)?)
      } else {
        None
      };
      let span = inp.span_since(&c);
      let data = match (dirs, ops) {
        (Some(d), Some(o)) => scaffold::SchemaExtensionData::Operations { directives: Some(d), definitions: o },
        (Some(d), None) => scaffold::SchemaExtensionData::Directives(d),
        (None, Some(o)) => scaffold::SchemaExtensionData::Operations { directives: None, definitions: o },
        (None, None) => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
      };
      Ok(TypeSystemExtension::Schema(scaffold::SchemaExtension::new(span, data)))
    },
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      parse_type_extension(inp).map(TypeSystemExtension::Type)
    }),
  )
    .peek_then_choice::<_, U1>(
      |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U1>, _emitter| {
        match peeked.pop_front() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(Span::new(0, 0)).into()),
          Some(tok) => with_peeked_token!(&tok, |t| match t {
            SyntacticToken::Identifier(s) if "schema".equivalent(s) => Ok(Branch::B0),
            _ => Ok(Branch::B1),
          }),
        }
      },
    )
    .parse_input(input)
}

// ─── Operation / Fragment / Document ─────────────────────────────────────────

/// Parses an operation definition.
pub fn parse_operation_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<OperationDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  (
    |inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      Ok(scaffold::OperationDefinition::Shorthand(parse_selection_set(inp)?))
    },
    |inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let cursor = inp.cursor().clone();
      let op = parse_operation_type(inp)?;
      let name = if peek_kind(inp) == Some(SyntacticTokenKind::Identifier) && !peek_keyword(inp, "on") {
        Some(parse_name(inp)?)
      } else {
        None
      };
      let vars = parse_variables_definition(inp)?;
      let dirs = parse_directives(inp)?;
      let ss = parse_selection_set(inp)?;
      Ok(scaffold::OperationDefinition::Named(
        scaffold::NamedOperationDefinition::new(inp.span_since(&cursor), op, name, vars, dirs, ss),
      ))
    },
  )
    .peek_then_choice::<_, U1>(
      |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U1>, _emitter| {
        match peeked.pop_front() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(Span::new(0, 0)).into()),
          Some(tok) => with_peeked_token!(&tok, |t| match t.kind() {
            SyntacticTokenKind::LBrace => Ok(Branch::B0),
            _ => Ok(Branch::B1),
          }),
        }
      },
    )
    .parse_input(input)
}

/// Parses a fragment definition.
pub fn parse_fragment_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<FragmentDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  parse_fragment_kw(input)?;
  let name = parse_name(input)?;
  let fname = FragmentName::new(name.span().clone(), name.source_ref().clone());
  parse_on(input)?;
  let tn = parse_name(input)?;
  let tc = TypeCondition::new(tn.span().clone(), tn);
  let dirs = parse_directives(input)?;
  let ss = parse_selection_set(input)?;
  Ok(scaffold::FragmentDefinition::new(input.span_since(&cursor), fname, tc, dirs, ss))
}

/// Parses an executable definition: fragment or operation.
pub fn parse_executable_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<ExecutableDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  (
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      parse_fragment_definition(inp).map(scaffold::ExecutableDefinition::Fragment)
    }),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      parse_operation_definition(inp).map(scaffold::ExecutableDefinition::Operation)
    }),
  )
    .peek_then_choice::<_, U1>(
      |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U1>, _emitter| {
        match peeked.pop_front() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(Span::new(0, 0)).into()),
          Some(tok) => with_peeked_token!(&tok, |t| match t {
            SyntacticToken::Identifier(s) if "fragment".equivalent(s) => Ok(Branch::B0),
            _ => Ok(Branch::B1),
          }),
        }
      },
    )
    .parse_input(input)
}

/// Parses a definition (type system or executable).
pub fn parse_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Definition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  (
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      parse_schema_definition(inp).map(|d| scaffold::Definition::TypeSystem(scaffold::TypeSystemDefinition::Schema(d)))
    }),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      parse_directive_definition(inp).map(|d| scaffold::Definition::TypeSystem(scaffold::TypeSystemDefinition::Directive(d)))
    }),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      parse_type_definition(inp).map(|d| scaffold::Definition::TypeSystem(scaffold::TypeSystemDefinition::Type(d)))
    }),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      parse_executable_definition(inp).map(scaffold::Definition::Executable)
    }),
  )
    .peek_then_choice::<_, U3>(
      |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U3>, _emitter| {
        match peeked.pop_front() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(Span::new(0, 0)).into()),
          Some(tok) => with_peeked_token!(&tok, |t| match t {
            SyntacticToken::Identifier(s) if "schema".equivalent(s) => Ok(Branch::B0),
            SyntacticToken::Identifier(s) if "directive".equivalent(s) => Ok(Branch::B1),
            SyntacticToken::Identifier(s) if "scalar".equivalent(s)
              || "type".equivalent(s)
              || "interface".equivalent(s)
              || "union".equivalent(s)
              || "enum".equivalent(s)
              || "input".equivalent(s) => Ok(Branch::B2),
            _ => Ok(Branch::B3),
          }),
        }
      },
    )
    .parse_input(input)
}

/// Parses a definition or extension.
pub fn parse_definition_or_extension<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<DefinitionOrExtension<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  (
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      parse_type_system_extension(inp).map(scaffold::DefinitionOrExtension::Extension)
    }),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let cursor = inp.cursor().clone();
      let desc = parse_description(inp)?;
      let def = parse_definition(inp)?;
      let span = inp.span_since(&cursor);
      Ok(scaffold::DefinitionOrExtension::Definition(scaffold::Described::new(span, desc, def)))
    }),
  )
    .peek_then_choice::<_, U3>(
      |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U3>, _emitter| {
        match peeked.pop_front() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(Span::new(0, 0)).into()),
          Some(tok) => with_peeked_token!(&tok, |t| match t {
            SyntacticToken::Identifier(s) if "extend".equivalent(s) => Ok(Branch::B0),
            _ => Ok(Branch::B1),
          }),
        }
      },
    )
    .parse_input(input)
}

/// Parses a type system definition or extension.
pub fn parse_type_system_definition_or_extension<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<TypeSystemDefinitionOrExtension<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  (
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      parse_type_system_extension(inp).map(scaffold::TypeSystemDefinitionOrExtension::Extension)
    }),
    (|inp: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>| {
      let cursor = inp.cursor().clone();
      let desc = parse_description(inp)?;
      let def = if peek_keyword(inp, "schema") { scaffold::TypeSystemDefinition::Schema(parse_schema_definition(inp)?) }
      else if peek_keyword(inp, "directive") { scaffold::TypeSystemDefinition::Directive(parse_directive_definition(inp)?) }
      else { scaffold::TypeSystemDefinition::Type(parse_type_definition(inp)?) };
      let span = inp.span_since(&cursor);
      Ok(scaffold::TypeSystemDefinitionOrExtension::Definition(scaffold::Described::new(span, desc, def)))
    }),
  )
    .peek_then_choice::<_, U1>(
      |mut peeked: Peeked<'_, 'inp, SyntacticLexer<'inp, S>, U1>, _emitter| {
        match peeked.pop_front() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(Span::new(0, 0)).into()),
          Some(tok) => with_peeked_token!(&tok, |t| match t {
            SyntacticToken::Identifier(s) if "extend".equivalent(s) => Ok(Branch::B0),
            _ => Ok(Branch::B1),
          }),
        }
      },
    )
    .parse_input(input)
}

/// Parses a document (repeated definitions until EOF).
pub fn parse_document<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Document<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let defs = (parse_definition_or_extension as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(continue_while_some!())
    .collect()
    .parse_input(input)?;
  Ok(scaffold::Document::new(input.span_since(&cursor), defs))
}

/// Parses an executable document (operations and fragments).
pub fn parse_executable_document<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<ExecutableDocument<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let defs = (parse_executable_definition as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(continue_while_some!())
    .collect()
    .parse_input(input)?;
  Ok(scaffold::Document::new(input.span_since(&cursor), defs))
}

/// Parses a type system document (type system definitions and extensions).
pub fn parse_type_system_document<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<TypeSystemDocument<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  let defs = (parse_type_system_definition_or_extension as fn(&mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>) -> _)
    .repeated_while::<_, U1>(continue_while_some!())
    .collect()
    .parse_input(input)?;
  Ok(scaffold::Document::new(input.span_since(&cursor), defs))
}

// ─── Standalone extension parsers (for individual ParseStr impls) ────────────

/// Parses a described object type definition.
pub fn parse_described_object_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<DescribedObjectTypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let c = input.cursor().clone();
  let d = parse_description(input)?;
  let def = parse_object_type_definition(input)?;
  Ok(scaffold::Described::new(input.span_since(&c), d, def))
}

/// Parses a described interface type definition.
pub fn parse_described_interface_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<DescribedInterfaceTypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let c = input.cursor().clone();
  let d = parse_description(input)?;
  let def = parse_interface_type_definition(input)?;
  Ok(scaffold::Described::new(input.span_since(&c), d, def))
}

/// Parses a described enum type definition.
pub fn parse_described_enum_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<DescribedEnumTypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let c = input.cursor().clone();
  let d = parse_description(input)?;
  let def = parse_enum_type_definition(input)?;
  Ok(scaffold::Described::new(input.span_since(&c), d, def))
}

/// Parses a described input object type definition.
pub fn parse_described_input_object_type_definition<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<DescribedInputObjectTypeDefinition<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let c = input.cursor().clone();
  let d = parse_description(input)?;
  let def = parse_input_object_type_definition(input)?;
  Ok(scaffold::Described::new(input.span_since(&c), d, def))
}

/// Parses an object type extension: `'extend' 'type' ...`.
pub fn parse_object_type_extension<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<ObjectTypeExtension<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  parse_extend(input)?;
  let c = input.cursor().clone();
  parse_type_kw(input)?;
  let name = parse_name(input)?;
  let impls = parse_implements(input)?;
  let dirs = parse_const_directives(input)?;
  let fields = parse_fields_definition(input)?;
  let span = input.span_since(&c);
  let data = match (impls, dirs, fields) {
    (i, d, Some(f)) => scaffold::ObjectTypeExtensionData::Fields { implements: i, directives: d, fields: f },
    (i, Some(d), None) => scaffold::ObjectTypeExtensionData::Directives { implements: i, directives: d },
    _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
  };
  Ok(scaffold::ObjectTypeExtension::new(span, name, data))
}

/// Parses an interface type extension: `'extend' 'interface' ...`.
pub fn parse_interface_type_extension<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<InterfaceTypeExtension<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  parse_extend(input)?;
  let c = input.cursor().clone();
  parse_interface(input)?;
  let name = parse_name(input)?;
  let impls = parse_implements(input)?;
  let dirs = parse_const_directives(input)?;
  let fields = parse_fields_definition(input)?;
  let span = input.span_since(&c);
  let data = match (impls, dirs, fields) {
    (i, d, Some(f)) => scaffold::InterfaceTypeExtensionData::Fields { implements: i, directives: d, fields: f },
    (i, Some(d), None) => scaffold::InterfaceTypeExtensionData::Directives { implements: i, directives: d },
    _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
  };
  Ok(scaffold::InterfaceTypeExtension::new(span, name, data))
}

/// Parses an enum type extension: `'extend' 'enum' ...`.
pub fn parse_enum_type_extension<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<EnumTypeExtension<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  parse_extend(input)?;
  let c = input.cursor().clone();
  parse_enum(input)?;
  let name = parse_name(input)?;
  let dirs = parse_const_directives(input)?;
  let vals = parse_enum_values_definition(input)?;
  let span = input.span_since(&c);
  let data = match (dirs, vals) {
    (d, Some(v)) => scaffold::EnumTypeExtensionData::Values { directives: d, values: v },
    (Some(d), None) => scaffold::EnumTypeExtensionData::Directives(d),
    _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
  };
  Ok(scaffold::EnumTypeExtension::new(span, name, data))
}

/// Parses an input object type extension: `'extend' 'input' ...`.
pub fn parse_input_object_type_extension<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<InputObjectTypeExtension<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  parse_extend(input)?;
  let c = input.cursor().clone();
  parse_input_kw(input)?;
  let name = parse_name(input)?;
  let dirs = parse_const_directives(input)?;
  let fields = parse_input_fields_definition(input)?;
  let span = input.span_since(&c);
  let data = match (dirs, fields) {
    (d, Some(f)) => scaffold::InputObjectTypeExtensionData::Fields { directives: d, fields: f },
    (Some(d), None) => scaffold::InputObjectTypeExtensionData::Directives(d),
    _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
  };
  Ok(scaffold::InputObjectTypeExtension::new(span, name, data))
}

/// Parses a scalar type extension: `'extend' 'scalar' ...`.
pub fn parse_scalar_type_extension<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<ScalarTypeExtension<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  parse_extend(input)?;
  let c = input.cursor().clone();
  parse_scalar(input)?;
  let name = parse_name(input)?;
  let dirs = parse_const_directives(input)?;
  let span = input.span_since(&c);
  Ok(scaffold::ScalarTypeExtension::new(span, name, dirs.unwrap_or_else(|| scaffold::Directives::new(span, Vec::new()))))
}

/// Parses a schema extension: `'extend' 'schema' ...`.
pub fn parse_schema_extension<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<SchemaExtension<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp> + PunctuatorToken<'inp> + KeywordToken<'inp>,
  <SyntacticToken<S> as smear_lexer::tokit::Token<'inp>>::Kind: From<smear_lexer::tokit::punct::Pipe<(), (), ()>> + From<smear_lexer::tokit::punct::Ampersand<(), (), ()>>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>
    + FullContainerEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + SeparatedEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, SyntacticLexer<'inp, S>, Lang>
    + TooFewEmitter<'inp, SyntacticLexer<'inp, S>, Lang>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  parse_extend(input)?;
  let c = input.cursor().clone();
  parse_schema(input)?;
  let dirs = parse_const_directives(input)?;
  let ops = if peek_kind(input) == Some(SyntacticTokenKind::LBrace) {
    Some(parse_root_operation_types_definition(input)?)
  } else {
    None
  };
  let span = input.span_since(&c);
  let data = match (dirs, ops) {
    (Some(d), Some(o)) => scaffold::SchemaExtensionData::Operations { directives: Some(d), definitions: o },
    (Some(d), None) => scaffold::SchemaExtensionData::Directives(d),
    (None, Some(o)) => scaffold::SchemaExtensionData::Operations { directives: None, definitions: o },
    _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
  };
  Ok(scaffold::SchemaExtension::new(span, data))
}
