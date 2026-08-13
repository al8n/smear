use smear_lexer::graphql::{
  error::{DecimalError, LexerErrorData},
  syntactic::{SyntacticLexer, SyntacticToken},
};
use tokora::Lexer;

use super::{
  MaterialisedInt, Materialized, Numbers, OutOfRange, is_int_literal, overflows, parse_f64,
  parse_i32, parse_i64,
};
use crate::graphql::error::{IntOverflow, IntWidth};

/// Which of [`overflows`]'s two conjuncts decided a row.
///
/// A test that only asks whether the answer was a refusal cannot tell a right answer from a right
/// answer for the wrong reason: `007` and `2147483647` are both "not an overflow" and they are not
/// refused by the same thing, and the difference is the whole of the round that produced this
/// enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Verdict {
  /// The **shape** conjunct refused: the lexer does not call these bytes an `IntValue`, so there
  /// is no width at which they overflow anything and no width a payload may name.
  NotALiteral,
  /// An `IntValue`, and the reader at this width converted it.
  Fits,
  /// An `IntValue`, and the reader at this width refused it — the one case [`overflows`] answers
  /// `true` to.
  Overflows,
}

use Verdict::{Fits, NotALiteral, Overflows};

/// [`overflows`] taken apart into the two questions it asks, in the order it asks them.
fn verdict(literal: &str, width: IntWidth) -> Verdict {
  let bytes = literal.as_bytes();
  let converts = match width {
    IntWidth::I32 => parse_i32(bytes).is_some(),
    IntWidth::I64 => parse_i64(bytes).is_some(),
  };
  match (is_int_literal(bytes), converts) {
    (false, _) => NotALiteral,
    (true, true) => Fits,
    (true, false) => Overflows,
  }
}

/// One row of the boundary table.
struct Row {
  literal: &'static str,
  at_i32: Verdict,
  at_i64: Verdict,
  /// Whether `core`'s own integer reader reaches the same overflow/not-overflow answer at both
  /// widths — see `overflows_agrees_with_cores_own_reader_on_which_failures_are_overflows` for the
  /// oracle and `the_divergences_from_cores_reader_are_ours` for the two classes that do not.
  ///
  /// **A column rather than a curated omission.** Leaving a divergent literal out of the table
  /// would remove it from the boundary property as well, which is the one it is here for; marking
  /// it is what keeps both properties asked over one list.
  core_agrees: bool,
}

impl Row {
  const fn agreeing(literal: &'static str, at_i32: Verdict, at_i64: Verdict) -> Self {
    Self {
      literal,
      at_i32,
      at_i64,
      core_agrees: true,
    }
  }

  const fn diverging(literal: &'static str, at_i32: Verdict, at_i64: Verdict) -> Self {
    Self {
      literal,
      at_i32,
      at_i64,
      core_agrees: false,
    }
  }
}

/// Every literal these tests reason about, with the conjunct that must decide it at each width.
const TABLE: &[Row] = &[
  // Small values, both signs. `-0` is a well-formed `IntValue`: draft §2.9.1's `IntegerPart` is
  // `-? (0 | NonZeroDigit Digit*)`, and the sign is outside the leading-zero rule.
  Row::agreeing("0", Fits, Fits),
  Row::agreeing("-0", Fits, Fits),
  Row::agreeing("7", Fits, Fits),
  Row::agreeing("-7", Fits, Fits),
  // The four range boundaries and the four literals one past each of them.
  Row::agreeing("2147483647", Fits, Fits),
  Row::agreeing("2147483648", Overflows, Fits),
  Row::agreeing("-2147483648", Fits, Fits),
  Row::agreeing("-2147483649", Overflows, Fits),
  Row::agreeing("9223372036854775807", Overflows, Fits),
  Row::agreeing("9223372036854775808", Overflows, Overflows),
  Row::agreeing("-9223372036854775808", Overflows, Fits),
  Row::agreeing("-9223372036854775809", Overflows, Overflows),
  Row::agreeing("99999999999999999999999999", Overflows, Overflows),
  // Leading zeroes. The lexer reports these as `LeadingZeros` and no production ever converts one,
  // so none of them overflows anything — including the four whose *value* is past a width, which
  // are the payloads the finding was about.
  Row::agreeing("00", NotALiteral, NotALiteral),
  Row::agreeing("007", NotALiteral, NotALiteral),
  Row::agreeing("-007", NotALiteral, NotALiteral),
  Row::agreeing("0000000000000000007", NotALiteral, NotALiteral),
  Row::agreeing("02147483647", NotALiteral, NotALiteral),
  Row::agreeing("-02147483648", NotALiteral, NotALiteral),
  Row::diverging("02147483648", NotALiteral, NotALiteral),
  Row::diverging("-02147483649", NotALiteral, NotALiteral),
  Row::diverging("09223372036854775808", NotALiteral, NotALiteral),
  Row::diverging("-09223372036854775809", NotALiteral, NotALiteral),
  // A leading `+`. `core` reads one; draft §2.9.1 has no unary plus.
  Row::agreeing("+", NotALiteral, NotALiteral),
  Row::agreeing("+7", NotALiteral, NotALiteral),
  Row::diverging("+2147483648", NotALiteral, NotALiteral),
  Row::diverging("+99999999999999999999999999", NotALiteral, NotALiteral),
  // Nothing, or nothing but a sign.
  Row::agreeing("", NotALiteral, NotALiteral),
  Row::agreeing("-", NotALiteral, NotALiteral),
  // Whitespace is trivia to the lexer, so each of these *contains* an `Int` without being one.
  Row::agreeing(" 7", NotALiteral, NotALiteral),
  Row::agreeing("7 ", NotALiteral, NotALiteral),
  Row::agreeing("\t7", NotALiteral, NotALiteral),
  Row::agreeing("7\n", NotALiteral, NotALiteral),
  Row::agreeing(" ", NotALiteral, NotALiteral),
  Row::agreeing("7 7", NotALiteral, NotALiteral),
  // Not an integer at all: a separator GraphQL does not have, a suffix, a float, a radix prefix,
  // a name.
  Row::agreeing("1_000", NotALiteral, NotALiteral),
  Row::agreeing("1x", NotALiteral, NotALiteral),
  Row::agreeing("1.0", NotALiteral, NotALiteral),
  Row::agreeing("1e400", NotALiteral, NotALiteral),
  Row::agreeing("0x10", NotALiteral, NotALiteral),
  Row::agreeing("hello", NotALiteral, NotALiteral),
];

/// Does the lexer refuse this spelling *specifically* for a leading zero?
///
/// The shape conjunct answers only yes-or-no, so this is how a test says which grammar rule did
/// the refusing — the difference between "the lexer said no" and "the lexer said `LeadingZeros`".
fn lexer_refuses_for_leading_zeros(literal: &str) -> bool {
  let mut lexer = SyntacticLexer::<'_, [u8]>::new(literal.as_bytes());
  let Some(Err(errors)) = lexer.lex() else {
    return false;
  };
  matches!(
    errors.first().map(|error| error.data()),
    Some(LexerErrorData::Int(DecimalError::LeadingZeros(_)))
  )
}

/// `core`'s reading of the same question: is this literal's failure at this width an out-of-range
/// one, as opposed to it not being a number at all?
fn core_calls_it_an_overflow(literal: &str, width: IntWidth) -> bool {
  use core::num::IntErrorKind;

  let kind = match width {
    IntWidth::I32 => literal.parse::<i32>().err().map(|e| *e.kind()),
    IntWidth::I64 => literal.parse::<i64>().err().map(|e| *e.kind()),
  };
  matches!(
    kind,
    Some(IntErrorKind::PosOverflow | IntErrorKind::NegOverflow)
  )
}

#[test]
fn i64_reads_the_grammar_it_is_given() {
  assert_eq!(parse_i64(b"0"), Some(0));
  assert_eq!(parse_i64(b"-0"), Some(0));
  assert_eq!(parse_i64(b"42"), Some(42));
  assert_eq!(parse_i64(b"-42"), Some(-42));
  assert_eq!(parse_i64(b"9223372036854775807"), Some(i64::MAX));
}

/// `i64::MIN`'s magnitude is one past `i64::MAX`, so a conversion that parses the digits
/// unsigned and negates afterwards overflows on exactly this input and no other. Accumulating in
/// the literal's own sign is what makes it convert; this is the test that distinguishes the two
/// implementations.
#[test]
fn i64_min_converts_rather_than_overflowing() {
  assert_eq!(parse_i64(b"-9223372036854775808"), Some(i64::MIN));
  assert_eq!(parse_i64(b"9223372036854775808"), None);
}

#[test]
fn i64_refuses_what_does_not_fit_and_what_is_not_a_number() {
  assert_eq!(parse_i64(b"99999999999999999999999999"), None);
  assert_eq!(parse_i64(b"-99999999999999999999999999"), None);
  assert_eq!(parse_i64(b""), None);
  assert_eq!(parse_i64(b"-"), None);
  assert_eq!(parse_i64(b"1x"), None);
  assert_eq!(parse_i64(b"1.0"), None);
}

#[test]
fn f64_reads_the_grammar_it_is_given() {
  assert_eq!(parse_f64(b"1.0"), Some(1.0));
  assert_eq!(parse_f64(b"-1.5"), Some(-1.5));
  assert_eq!(parse_f64(b"1e3"), Some(1000.0));
  assert_eq!(parse_f64(b"1.5E-3"), Some(0.0015));
  assert_eq!(parse_f64(b"0.0"), Some(0.0));
}

/// A literal longer than any fixed buffer still converts, and converts *correctly rounded*.
///
/// Each case names a different way the rejected implementations get it wrong.
#[test]
fn f64_is_correctly_rounded_at_any_length() {
  // 302 characters, and the ONE digit that carries the value is the last of them. A conversion
  // that copied into a fixed buffer and truncated would answer `0.0`; one that refused past its
  // buffer would answer `None`.
  let deep = std::format!("0.{}1", "0".repeat(300));
  assert_eq!(parse_f64(deep.as_bytes()), Some(1e-301));

  // 403 characters whose tail is all zeros: the value is `0.1`, and an implementation with a
  // length limit still has to say so rather than decline.
  let long_tail = std::format!("0.1{}", "0".repeat(400));
  assert_eq!(parse_f64(long_tail.as_bytes()), Some(0.1));

  // The smallest positive subnormal, and just under half of it. Only a correctly rounded
  // conversion reaches these: a mantissa-times-power-of-ten shortcut double-rounds and answers a
  // denormal for the second one instead of zero.
  assert_eq!(parse_f64(b"4.9406564584124654e-324"), Some(5e-324));
  assert_eq!(parse_f64(b"2.4703282292062327e-324"), Some(0.0));
}

/// Draft §3.5.2 admits only finite values, and `f64::from_str` reports an overflow as an infinity
/// rather than as an error — so the guard has to be `is_finite`, not the parse's own `Result`.
#[test]
fn f64_refuses_the_non_finite() {
  assert_eq!(parse_f64(b"1e400"), None);
  assert_eq!(parse_f64(b"-1e400"), None);
  assert_eq!(parse_f64(b"1e-400"), Some(0.0));
  assert_eq!(parse_f64(b"nope"), None);
}

/// The failure arm hands the slice back rather than dropping it, which is what lets the caller
/// name the literal in its error without cloning on the path that succeeds — and it tags which
/// leaf failed, because the two report as different variants.
#[test]
fn failure_returns_the_slice_and_which_leaf_it_came_from() {
  let int = <Materialized<i64> as Numbers<&str>>::int("99999999999999999999999999");
  assert!(matches!(
    int,
    Err(OutOfRange::Int {
      value: "99999999999999999999999999",
      width: IntWidth::I64,
    })
  ));

  let float = <Materialized<i64> as Numbers<&str>>::float("1e400");
  assert!(matches!(float, Err(OutOfRange::Float("1e400"))));

  assert_eq!(
    <Materialized<i64> as Numbers<&str>>::int("7").ok(),
    Some(7)
  );
  assert_eq!(
    <Materialized<i64> as Numbers<&str>>::float("7.5").ok(),
    Some(7.5)
  );
}

#[test]
fn i32_reads_the_grammar_it_is_given() {
  assert_eq!(parse_i32(b"0"), Some(0));
  assert_eq!(parse_i32(b"-0"), Some(0));
  assert_eq!(parse_i32(b"42"), Some(42));
  assert_eq!(parse_i32(b"-42"), Some(-42));
  assert_eq!(parse_i32(b"2147483647"), Some(i32::MAX));
  assert_eq!(parse_i32(b"-2147483648"), Some(i32::MIN));
}

/// The literal draft §3.5.1 and draft §2.9.1 disagree about: well-formed, and not an `Int`.
#[test]
fn i32_refuses_what_the_specification_does_not_admit() {
  assert_eq!(parse_i32(b"2147483648"), None);
  assert_eq!(parse_i32(b"-2147483649"), None);
  assert_eq!(parse_i32(b"9223372036854775808"), None);
  assert_eq!(parse_i32(b"99999999999999999999999999"), None);
  assert_eq!(parse_i32(b""), None);
  assert_eq!(parse_i32(b"-"), None);
  assert_eq!(parse_i32(b"1x"), None);
  assert_eq!(parse_i32(b"1.0"), None);
}

/// The claim `parse_i32`'s doc comment makes: reading at `i64` and narrowing *is* the narrower
/// read, so there is one digit loop and not two.
///
/// The interesting inputs are the two boundaries and the two literals just past them, plus the
/// two that overflow `i64` outright — the only place a two-step conversion could differ from a
/// one-step one is where the first step already refused.
#[test]
fn i32_is_i64_narrowed_on_every_boundary() {
  for literal in [
    "0",
    "-0",
    "1",
    "-1",
    "2147483647",
    "2147483648",
    "-2147483648",
    "-2147483649",
    "9223372036854775807",
    "9223372036854775808",
    "-9223372036854775808",
    "-9223372036854775809",
    "99999999999999999999999999",
    "",
    "-",
    "1x",
  ] {
    let bytes = literal.as_bytes();
    let narrowed = parse_i64(bytes).and_then(|wide| i32::try_from(wide).ok());
    assert_eq!(
      parse_i32(bytes),
      narrowed,
      "the two-step reading and the direct one disagree on {literal:?}",
    );
  }

  // Non-vacuity: the loop above compares two expressions that would agree if both were always
  // `None`. These two are the ones that make the comparison mean something.
  assert_eq!(parse_i32(b"2147483647"), Some(i32::MAX));
  assert_eq!(parse_i32(b"2147483648"), None);
}

/// The width travels with the failure, and it is **the payload type's own** rather than a
/// constant written beside it.
///
/// Both instantiations refuse `9223372036854775808` and each says so about a different width; if
/// they ever named the same one, a consumer could not tell "outside the specification's `Int`"
/// from "outside any integer this crate reads".
///
/// This is where a wrong [`MaterialisedInt::WIDTH`] surfaces first: `Materialized<I>::int` builds
/// the failure and the width it names from one type, so an impl answering the other's constant
/// moves every row here for that width and leaves the other width's alone.
#[test]
fn each_instantiation_names_its_own_width() {
  let past_i32 = <Materialized<i32> as Numbers<&str>>::int("2147483648");
  assert!(matches!(
    past_i32,
    Err(OutOfRange::Int {
      value: "2147483648",
      width: IntWidth::I32,
    })
  ));
  assert_eq!(
    <Materialized<i64> as Numbers<&str>>::int("2147483648").ok(),
    Some(2_147_483_648_i64),
    "the permissive width must accept the literal the specified one refuses",
  );

  let past_i64_at_32 = <Materialized<i32> as Numbers<&str>>::int("9223372036854775808");
  let past_i64_at_64 = <Materialized<i64> as Numbers<&str>>::int("9223372036854775808");
  assert!(matches!(
    past_i64_at_32,
    Err(OutOfRange::Int {
      width: IntWidth::I32,
      ..
    })
  ));
  assert!(matches!(
    past_i64_at_64,
    Err(OutOfRange::Int {
      width: IntWidth::I64,
      ..
    })
  ));
}

/// **The join between the two statements of one correspondence.**
///
/// [`overflows`] dispatches on a runtime [`IntWidth`] — because `IntOverflow::checked` takes one
/// as a value — and reaches [`parse_i32`] for `I32` and [`parse_i64`] for `I64`. A production
/// reaches the same readers the other way round, through [`MaterialisedInt`], and names
/// [`MaterialisedInt::WIDTH`] on what it refuses. Those are two spellings of "`i32` is the reader
/// at `I32`", and everything the door promises about the productions rests on them being the same
/// spelling.
///
/// It is the one place a wrong `WIDTH` could otherwise hide: the boundary table below asks
/// [`overflows`], which does not read `WIDTH` at all, so a `WIDTH` that lied would leave all 41
/// rows green while every refusal a production raised named the wrong width. Two lines, and they
/// are the reason that cannot happen quietly.
#[test]
fn the_widths_the_door_dispatches_on_are_the_widths_the_readers_name() {
  assert_eq!(<i32 as MaterialisedInt>::WIDTH, IntWidth::I32);
  assert_eq!(<i64 as MaterialisedInt>::WIDTH, IntWidth::I64);

  // And that each reader really is the one the door reaches for that width, on a literal only
  // one of them converts — so the equalities above are about the readers and not about two names.
  assert!(<i64 as MaterialisedInt>::parse(b"2147483648").is_some());
  assert!(<i32 as MaterialisedInt>::parse(b"2147483648").is_none());
  assert!(overflows(b"2147483648", <i32 as MaterialisedInt>::WIDTH));
  assert!(!overflows(b"2147483648", <i64 as MaterialisedInt>::WIDTH));
}

/// The boundary table, and **which conjunct refused each row** rather than only that it was
/// refused.
///
/// Three assertions per row and width: the conjunct that decided it, that [`overflows`] answers
/// `true` exactly on `Overflows`, and that the public door — `IntOverflow::checked`, the only way
/// an out-of-crate caller can name an [`IntWidth`] — reaches the same answer as its decider.
#[test]
fn overflows_reaches_the_tabulated_verdict_and_the_conjunct_that_decided_it() {
  let (mut shape, mut fits, mut over) = (0usize, 0usize, 0usize);

  for row in TABLE {
    for (width, expected) in [(IntWidth::I32, row.at_i32), (IntWidth::I64, row.at_i64)] {
      assert_eq!(
        verdict(row.literal, width),
        expected,
        "{:?} at {width}: the wrong conjunct decided it",
        row.literal,
      );
      assert_eq!(
        overflows(row.literal.as_bytes(), width),
        expected == Overflows,
        "{:?} at {width}: `overflows` disagrees with its own conjuncts",
        row.literal,
      );
      assert_eq!(
        IntOverflow::checked(row.literal, width).is_ok(),
        expected == Overflows,
        "{:?} at {width}: the public door disagrees with its decider",
        row.literal,
      );
      match expected {
        NotALiteral => shape += 1,
        Fits => fits += 1,
        Overflows => over += 1,
      }
    }
  }

  // Non-vacuity, per verdict: a table that reached only one of the three would satisfy every
  // equality above while testing one arm.
  assert!(shape >= 20, "only {shape} shape refusals");
  assert!(fits >= 8, "only {fits} in-range rows");
  assert!(over >= 8, "only {over} overflow rows");
}

/// The row the round turns on: **in range, and refused anyway — for its shape.**
///
/// `007` is `7`. Both readers convert it, so the range conjunct cannot be what refuses it, and a
/// row asserting only `Err` would be satisfied by an implementation that refused it for the wrong
/// reason — or by one that accepted it at a width and refused it at the other. Each literal here
/// is checked three ways: the reader accepts it at both widths, the verdict is `NotALiteral` at
/// both, and the lexer's reason is `LeadingZeros` and not some other refusal.
#[test]
fn an_in_range_leading_zeroed_literal_is_refused_for_shape_and_not_for_range() {
  for literal in [
    "00",
    "007",
    "-007",
    "0000000000000000007",
    "-02147483648",
    "02147483647",
  ] {
    let bytes = literal.as_bytes();

    assert!(
      parse_i32(bytes).is_some() && parse_i64(bytes).is_some(),
      "{literal:?} has to be in range at both widths, or the range conjunct could be what refuses it",
    );
    assert_eq!(verdict(literal, IntWidth::I32), NotALiteral);
    assert_eq!(verdict(literal, IntWidth::I64), NotALiteral);
    assert!(
      lexer_refuses_for_leading_zeros(literal),
      "{literal:?} must be refused as `LeadingZeros`, not as some other anomaly",
    );
  }

  // And the same shape past a width — the payload the finding named. It is refused for the same
  // reason as `007`, which is the point: its value never enters the question.
  for (literal, width) in [
    ("02147483648", IntWidth::I32),
    ("09223372036854775808", IntWidth::I64),
  ] {
    assert!(lexer_refuses_for_leading_zeros(literal));
    assert_eq!(verdict(literal, width), NotALiteral);
    assert_eq!(IntOverflow::checked(literal, width), Err(literal));
  }
}

/// [`is_int_literal`] with its first-byte gate removed: the lexer's answer and nothing else.
///
/// The oracle for the gate, and the reason a *necessary* condition is safe to add where a second
/// grammar was not. The gate is allowed to be redundant and is not allowed to disagree, so the
/// property is one-sided in form and total in fact: over the corpus below the two answers are
/// **equal**, which is what a necessary condition promises — it refuses early only what the
/// lexer refuses anyway.
fn lexer_alone_calls_it_an_int_literal(bytes: &[u8]) -> bool {
  let mut lexer = SyntacticLexer::<'_, [u8]>::new(bytes);
  match lexer.lex() {
    Some(Ok(SyntacticToken::LitInt(literal))) => literal.len() == bytes.len(),
    _ => false,
  }
}

/// **The gate refuses nothing the lexer admits** — asked over every first byte there is, rather
/// than over the ones worth guessing.
///
/// A wrong necessary condition can only refuse something valid, and this is the test that catches
/// it "at once": all 256 possible first bytes against a set of tails chosen so the corpus
/// contains genuine literals, near-misses, and the shapes the gate is there to cut off. The
/// oracle is [`lexer_alone_calls_it_an_int_literal`] — the same function without the gate — so a
/// gate that ever excluded a byte draft §2.9.1 admits fails here on the first run.
///
/// The 41-row table is folded in as well, so the boundary rows are checked against the gate-free
/// reading and not only against their tabulated verdict.
#[test]
fn the_gate_refuses_nothing_the_lexer_admits() {
  const TAILS: &[&[u8]] = &[
    b"",
    b"7",
    b"0",
    b"0007",
    b"2147483648",
    b"-7",
    b"z",
    b".5",
    b"e9",
    b"\\q",
    b" ",
    b"\"",
    b"+7",
  ];

  let mut admitted = 0usize;
  let mut buffer = [0u8; 32];
  for first in u8::MIN..=u8::MAX {
    for tail in TAILS {
      buffer[0] = first;
      buffer[1..=tail.len()].copy_from_slice(tail);
      let input = &buffer[..=tail.len()];
      let expected = lexer_alone_calls_it_an_int_literal(input);
      assert_eq!(
        is_int_literal(input),
        expected,
        "first byte {first:#04x} with tail {tail:?}: the gate disagrees with the lexer alone",
      );
      admitted += usize::from(expected);
    }
  }

  for row in TABLE {
    assert_eq!(
      is_int_literal(row.literal.as_bytes()),
      lexer_alone_calls_it_an_int_literal(row.literal.as_bytes()),
      "{:?}: the gate disagrees with the lexer alone",
      row.literal,
    );
  }

  // Non-vacuity: a corpus the lexer admitted nothing from would make every equality above hold
  // over a gate that refused everything.
  assert!(
    admitted >= 20,
    "only {admitted} of the generated inputs are `IntValue`s; the agreement is near-vacuous",
  );

  // And the two halves of the condition, named rather than left inside the loop: a digit or `-`
  // is the only first byte that survives it, and `0` survives it — so the leading-zero refusal is
  // still the lexer's to make.
  assert!(!is_int_literal(b"\"7\""));
  assert!(!is_int_literal(b"+7"));
  assert!(is_int_literal(b"-7") && is_int_literal(b"0"));
  assert!(!is_int_literal(b"007") && lexer_refuses_for_leading_zeros("007"));
}

/// What the conjunction needs from its second reader, now that the first one is the lexer:
/// **once the lexer has called a slice an `IntValue`, the only thing left to refuse is its
/// magnitude.**
///
/// That is what makes `parse_i64(b).is_none()` mean "overflow" rather than "not a number" inside
/// [`overflows`]. The oracle is `core`'s own reader, which reports the two failures as different
/// [`IntErrorKind`](core::num::IntErrorKind)s and knows nothing about this file: for every
/// spelling the lexer admits, `core` either converts it or calls it `PosOverflow`/`NegOverflow`,
/// never `InvalidDigit` or `Empty`.
///
/// **The property this replaces was the reverse inclusion** — everything `parse_i64` reads, the
/// shape check admits — and it is now false on purpose. `007` converts at both widths and is not a
/// GraphQL `IntValue`. That inclusion is what a `-?[0-9]+` predicate could promise; it is not what
/// the conjunction needs, and promising it was how the leading-zero payloads got built.
#[test]
fn once_the_lexer_admits_it_the_only_refusal_left_is_range() {
  use core::num::IntErrorKind;

  let (mut admitted, mut converted, mut ranged) = (0usize, 0usize, 0usize);
  for row in TABLE {
    if !is_int_literal(row.literal.as_bytes()) {
      continue;
    }
    admitted += 1;
    for width in [IntWidth::I32, IntWidth::I64] {
      let kind = match width {
        IntWidth::I32 => row.literal.parse::<i32>().err().map(|e| *e.kind()),
        IntWidth::I64 => row.literal.parse::<i64>().err().map(|e| *e.kind()),
      };
      match kind {
        None => converted += 1,
        Some(IntErrorKind::PosOverflow | IntErrorKind::NegOverflow) => ranged += 1,
        other => panic!(
          "{:?} is an `IntValue` to the lexer and `core` refused it as {other:?} — a shape \
           failure has survived the first conjunct",
          row.literal,
        ),
      }
    }
  }

  // Non-vacuity: the loop above is satisfied by admitting nothing, and the disjunction by rows
  // that only ever convert.
  assert!(admitted >= 12, "only {admitted} rows reach the reader");
  assert!(converted >= 8, "only {converted} conversions");
  assert!(ranged >= 8, "only {ranged} range refusals");

  // The reverse inclusion, named as false rather than left to be rediscovered.
  assert!(parse_i64(b"007").is_some() && !is_int_literal(b"007"));
}

/// [`overflows`] is the decider behind the public `IntOverflow::checked`, and it is held against
/// an oracle **this crate did not write**: `core`'s own `str::parse`, which reports an
/// out-of-range integer as `IntErrorKind::PosOverflow`/`NegOverflow` and a non-number as
/// `InvalidDigit` or `Empty`. That distinction is exactly the conjunction this predicate makes,
/// arrived at independently.
///
/// An oracle recomputed from `is_int_literal(bytes) && reader.is_none()` would be the
/// implementation transcribed, and would stay green over a version of the function that dropped
/// either conjunct. This one cannot: `core` has no idea what this file does.
///
/// It is asked over the rows the oracle is *entitled* to answer. `core`'s reader is not GraphQL's
/// grammar in two respects, and `the_divergences_from_cores_reader_are_ours` holds that column
/// down so it cannot become a place to park an inconvenient row.
#[test]
fn overflows_agrees_with_cores_own_reader_on_which_failures_are_overflows() {
  let (mut agreed_overflow, mut agreed_not) = (0usize, 0usize);
  for row in TABLE.iter().filter(|row| row.core_agrees) {
    for width in [IntWidth::I32, IntWidth::I64] {
      let expected = core_calls_it_an_overflow(row.literal, width);
      assert_eq!(
        overflows(row.literal.as_bytes(), width),
        expected,
        "{:?} at {width}: `core` says overflow={expected}",
        row.literal,
      );
      if expected {
        agreed_overflow += 1;
      } else {
        agreed_not += 1;
      }
    }
  }

  // Non-vacuity: the equality above is not two constants agreeing.
  assert!(
    agreed_overflow >= 8,
    "only {agreed_overflow} overflow cases"
  );
  assert!(agreed_not >= 8, "only {agreed_not} non-overflow cases");

  // The pair the earlier round was about, spelled out rather than left to the loop.
  assert!(overflows(b"2147483648", IntWidth::I32));
  assert!(!overflows(b"2147483648", IntWidth::I64));
}

/// The two places [`overflows`] and `core`'s reader are *meant* to disagree, each pinned so the
/// `core_agrees` column is a declaration rather than a curation.
///
/// `core` accepts a leading `+`, and it accepts leading zeroes; draft §2.9.1's `IntegerPart` has
/// neither. So `+99999999999999999999999999` and `02147483648` are overflows to `core` and are
/// not integer literals at all here. Ours is the right answer for a GraphQL `IntValue`: a payload
/// built for either would be quoting a spelling the grammar cannot produce, and the second one is
/// the finding this round repaired.
///
/// The loop is the part that matters. Every row marked as diverging must actually diverge, so a
/// row cannot be excused from the oracle by mislabelling it.
#[test]
fn the_divergences_from_cores_reader_are_ours() {
  let mut diverging = 0usize;
  for row in TABLE.iter().filter(|row| !row.core_agrees) {
    diverging += 1;
    let disagrees = [IntWidth::I32, IntWidth::I64].into_iter().any(|width| {
      overflows(row.literal.as_bytes(), width) != core_calls_it_an_overflow(row.literal, width)
    });
    assert!(
      disagrees,
      "{:?} is marked as diverging from `core` and agrees with it at both widths",
      row.literal,
    );
  }
  assert!(diverging >= 6, "only {diverging} declared divergences");

  // The unary plus.
  assert!("+99999999999999999999999999".parse::<i64>().is_err());
  assert!(!is_int_literal(b"+99999999999999999999999999"));
  assert!(!overflows(b"+99999999999999999999999999", IntWidth::I64));
  assert!(!overflows(b"+7", IntWidth::I32));

  // The leading zero, and the oracle really does read it as an overflow.
  assert!(core_calls_it_an_overflow("02147483648", IntWidth::I32));
  assert!(!is_int_literal(b"02147483648"));
  assert!(!overflows(b"02147483648", IntWidth::I32));
  assert!(core_calls_it_an_overflow(
    "09223372036854775808",
    IntWidth::I64
  ));
  assert!(!overflows(b"09223372036854775808", IntWidth::I64));
}

/// `Float` is `f64` at every width, and this asserts it **once** because there is one conversion.
///
/// What used to be here compared `<MaterializedNumbers32>::float` with `<MaterializedNumbers>::
/// float` and asserted they agreed. They were two bodies then and the comparison could fail; they
/// are one now — [`super::float`] takes no `I`, and each [`Numbers`] impl's `float` is a call to
/// it — so the same comparison would be between a function and itself. The property is held by
/// the call graph, and what is left to test is the conversion itself.
///
/// The type-level half is stated where it cannot rot: `Materialized<I>::Float` is written `f64`,
/// so no instantiation can make it anything else.
#[test]
fn the_float_leaf_takes_no_width() {
  assert_eq!(super::float("7.5").ok(), Some(7.5));
  assert!(matches!(super::float("1e400"), Err(OutOfRange::Float("1e400"))));

  // The markers reach that one function, which is the half a caller sees.
  let at_i32: Result<f64, OutOfRange<&str>> = <Materialized<i32> as Numbers<&str>>::float("7.5");
  let at_i64: Result<f64, OutOfRange<&str>> = <Materialized<i64> as Numbers<&str>>::float("7.5");
  assert_eq!(at_i32.ok(), Some(7.5));
  assert_eq!(at_i64.ok(), Some(7.5));
}
