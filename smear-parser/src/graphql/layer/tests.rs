//! What the lossless door lends for the records a hand-written root emits: one whose payload holds
//! no member, one that holds the door's own variant beside an ordinary error, one that holds
//! nothing but that variant, one on the warning channel, and a recovery hole.
//!
//! No production emits the first four, and a production's hole never comes alone, so each root
//! here is written by hand and handed to the door itself, as the cells of the lossless tree's own
//! `tests.rs` are. They stand here because the hand-over is this layer's: the layer counts and
//! forwards exactly what the door lends.

use std::vec::Vec;

use smear_lexer::{SyntaxErrorKind, limits::LosslessLimits};
use tokora::{InputRef, SimpleSpan, cache::DefaultCache, cst::Sink, span::Spanned};

use crate::{
  graphql::{
    GraphQL,
    error::{Error as DialectError, ErrorData},
    lossless::{
      GraphqlLosslessErrorValue, GraphqlLosslessErrors, GraphqlLosslessLexer,
      runner::{LosslessEmitter, parse_lossless_document},
    },
  },
  lossless::depth::RootStop,
};

type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
type Ctx<'inp> = (
  Sink<'inp, Lx<'inp>, LosslessEmitter<'inp>>,
  DefaultCache<'inp, Lx<'inp>>,
);
type Errs<'inp> = GraphqlLosslessErrors<&'inp str>;

/// One lent error, as a cell reads it: its kind, and where its span starts and ends.
type Lent = (SyntaxErrorKind, usize, usize);

/// Forty names and the spaces between them, so the small ceiling below is reached well inside
/// the source.
const SRC: &str =
  "a b c d e f g h i j k l m n o p q r s t a b c d e f g h i j k l m n o p q r s t ";

/// What the door lends for [`SRC`] under `root` and a produce-event ceiling, in order, and how
/// many error diagnostics the finished parse holds.
fn lent_by<R>(ceiling: usize, root: R) -> (Vec<Lent>, usize)
where
  R: for<'c> FnOnce(
    &mut InputRef<'static, 'c, Lx<'static>, Ctx<'static>, GraphQL>,
    &mut RootStop,
  ) -> Result<(), Errs<'static>>,
{
  let mut lent: Vec<Lent> = Vec::new();
  let parse = parse_lossless_document(
    SRC,
    LosslessLimits::default().with_max_produce_events(ceiling),
    root,
    |error: &GraphqlLosslessErrorValue<&'static str>| {
      lent.push((error.kind(), error.span().start(), error.span().end()));
    },
  );
  let errors = parse
    .diagnostics()
    .iter()
    .filter(|diagnostic| diagnostic.severity() == tokora::emitter::Severity::Error)
    .count();
  (lent, errors)
}

/// A root that emits an empty container on the error channel and reads the source to its end.
fn emits_empty<'inp>(
  inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
  _stop: &mut RootStop,
) -> Result<(), Errs<'inp>> {
  let _ = inp.emit_error(Spanned::new(SimpleSpan::new(3, 4), Errs::default()));
  inp.skip_while(|_| true)?;
  Ok(())
}

/// The same on the warning channel.
fn emits_empty_warning<'inp>(
  inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
  _stop: &mut RootStop,
) -> Result<(), Errs<'inp>> {
  let _ = inp.emit_warning(Spanned::new(SimpleSpan::new(5, 6), Errs::default()));
  inp.skip_while(|_| true)?;
  Ok(())
}

/// A root that emits the door's own variant and an ordinary error in one container, at the
/// ordinary error's span, and reads the source to its end.
fn emits_mixed<'inp>(
  inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
  _stop: &mut RootStop,
) -> Result<(), Errs<'inp>> {
  let ordinary = SimpleSpan::new(1, 2);
  let mut mixed = Errs::from(DialectError::new(
    SimpleSpan::new(0, 0),
    ErrorData::TokenBudgetExhausted,
  ));
  mixed.extend(core::iter::once(DialectError::new(
    ordinary,
    ErrorData::FloatOverflow("x"),
  )));
  let _ = inp.emit_error(Spanned::new(ordinary, mixed));
  inp.skip_while(|_| true)?;
  Ok(())
}

/// A root that emits a container holding nothing but the door's own variant, with nothing
/// refused, and reads the source to its end.
fn emits_the_doors_variant_alone<'inp>(
  inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
  _stop: &mut RootStop,
) -> Result<(), Errs<'inp>> {
  let forged = SimpleSpan::new(0, 0);
  let _ = inp.emit_error(Spanned::new(
    forged,
    Errs::from(DialectError::new(forged, ErrorData::TokenBudgetExhausted)),
  ));
  inp.skip_while(|_| true)?;
  Ok(())
}

/// A root that records a recovery hole of no token, which has a span and no payload, and reads
/// the source to its end.
fn emits_a_hole<'inp>(
  inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
  _stop: &mut RootStop,
) -> Result<(), Errs<'inp>> {
  let _ = inp.emit_skipped_region(SimpleSpan::new(1, 2), 0);
  inp.skip_while(|_| true)?;
  Ok(())
}

/// A kept record always lends: one whose payload holds no member lends one `invalid-syntax` error
/// at the record's own span, which is the one error diagnostic the parse holds for it.
#[test]
fn a_record_with_no_member_lends_one_invalid_syntax_error_at_its_span() {
  let (lent, errors) = lent_by(usize::MAX, emits_empty);
  assert_eq!(lent, [(SyntaxErrorKind::InvalidSyntax, 3, 4)]);
  assert_eq!(errors, 1);
}

/// The error the door makes for such a record reads as the lexer's own `invalid-syntax` error
/// does. The door spells that message itself, because a lossless tree names no lexer kind, and
/// this is what holds the two spellings together.
#[test]
fn the_error_made_for_a_record_with_no_member_has_its_kinds_message() {
  let mut messages: Vec<std::string::String> = Vec::new();
  let _ = parse_lossless_document(
    SRC,
    LosslessLimits::default(),
    emits_empty,
    |error: &GraphqlLosslessErrorValue<&'static str>| messages.push(error.to_string()),
  );
  assert_eq!(messages, [SyntaxErrorKind::InvalidSyntax.message()]);
}

/// The hand-over is of the error channel: a warning's payload is kept in the parse and not lent.
#[test]
fn a_warning_is_not_lent() {
  let (lent, errors) = lent_by(usize::MAX, emits_empty_warning);
  assert!(lent.is_empty(), "{lent:?}");
  assert_eq!(errors, 0);
}

/// A recovery hole has no payload, so it has nothing to lend: the parse keeps its record, which
/// is no error, and the hand-over passes it by.
#[test]
fn a_recovery_hole_is_kept_in_the_parse_and_not_lent() {
  let mut lent = 0usize;
  let parse = parse_lossless_document(
    SRC,
    LosslessLimits::default(),
    emits_a_hole,
    |_: &GraphqlLosslessErrorValue<&'static str>| lent += 1,
  );
  assert_eq!(lent, 0);
  let holes: Vec<Option<usize>> = parse
    .diagnostics()
    .iter()
    .map(|diagnostic| diagnostic.skipped_tokens())
    .collect();
  assert_eq!(holes, [Some(0)]);
  assert!(!parse.has_errors());
}

/// A grammar-emitted budget member is never lent, even inside a payload the parse keeps for its
/// other member, which is lent.
#[test]
fn a_mixed_payload_lends_its_ordinary_member_and_not_the_budget_member() {
  let (lent, errors) = lent_by(usize::MAX, emits_mixed);
  assert_eq!(lent, [(SyntaxErrorKind::FloatOverflow, 1, 2)]);
  assert_eq!(errors, 1);
}

/// A record the door drops is not lent: with nothing refused, a payload of the door's variant
/// alone leaves no diagnostic and no lent error.
#[test]
fn a_payload_of_the_doors_variant_alone_is_dropped_and_lends_nothing() {
  let (lent, errors) = lent_by(usize::MAX, emits_the_doors_variant_alone);
  assert!(lent.is_empty(), "{lent:?}");
  assert_eq!(errors, 0);
}

/// When the durable budget refuses, the door's own refusal is lent once, last, at the refusal's
/// place, behind what the grammar emitted; and each lent error has its error diagnostic.
#[test]
fn the_doors_refusal_is_lent_once_and_last() {
  let (lent, errors) = lent_by(4, emits_mixed);
  assert_eq!(
    lent,
    [
      (SyntaxErrorKind::FloatOverflow, 1, 2),
      (SyntaxErrorKind::TokenBudget, 4, 4),
    ]
  );
  assert_eq!(errors, 2);

  let (lent, errors) = lent_by(4, emits_the_doors_variant_alone);
  assert_eq!(lent, [(SyntaxErrorKind::TokenBudget, 4, 4)]);
  assert_eq!(errors, 1);
}
