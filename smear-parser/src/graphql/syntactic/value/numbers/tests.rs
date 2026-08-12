use super::{
  MaterializedNumbers, MaterializedNumbers32, Numbers, OutOfRange, is_integer_literal, overflows,
  parse_f64, parse_i32, parse_i64,
};
use crate::graphql::error::IntWidth;

/// Every literal the two predicate properties below reason about, in one place so both are asked
/// over the same corpus rather than over two lists that happen to agree today.
const CORPUS: &[&str] = &[
  "0",
  "-0",
  "7",
  "-7",
  "007",
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
  "1.0",
  "+7",
  "hello",
];

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
  let int = <MaterializedNumbers as Numbers<&str>>::int("99999999999999999999999999");
  assert!(matches!(
    int,
    Err(OutOfRange::Int {
      value: "99999999999999999999999999",
      width: IntWidth::I64,
    })
  ));

  let float = <MaterializedNumbers as Numbers<&str>>::float("1e400");
  assert!(matches!(float, Err(OutOfRange::Float("1e400"))));

  assert_eq!(
    <MaterializedNumbers as Numbers<&str>>::int("7").ok(),
    Some(7)
  );
  assert_eq!(
    <MaterializedNumbers as Numbers<&str>>::float("7.5").ok(),
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

/// The width travels with the failure, and it is the marker's own width rather than a constant.
///
/// Both markers refuse `9223372036854775808` and each says so about a different width; if they
/// ever named the same one, a consumer could not tell "outside the specification's `Int`" from
/// "outside any integer this crate reads".
#[test]
fn each_marker_names_its_own_width() {
  let past_i32 = <MaterializedNumbers32 as Numbers<&str>>::int("2147483648");
  assert!(matches!(
    past_i32,
    Err(OutOfRange::Int {
      value: "2147483648",
      width: IntWidth::I32,
    })
  ));
  assert_eq!(
    <MaterializedNumbers as Numbers<&str>>::int("2147483648").ok(),
    Some(2_147_483_648_i64),
    "the permissive width must accept the literal the specified one refuses",
  );

  let past_i64_at_32 = <MaterializedNumbers32 as Numbers<&str>>::int("9223372036854775808");
  let past_i64_at_64 = <MaterializedNumbers as Numbers<&str>>::int("9223372036854775808");
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

/// The claim `is_integer_literal`'s doc makes, and the reason it is the digit shape rather than
/// draft §2.9.1's `IntegerPart`: **everything the reader accepts, the shape check accepts.**
///
/// A stricter predicate would refuse literals `parse_i64` reads — `007` is the one in the corpus —
/// and the conjunction in [`overflows`] would then answer "not an overflow" for a literal the
/// production had converted, which is a disagreement between two readers of the same grammar.
#[test]
fn the_digit_shape_admits_everything_the_reader_accepts() {
  let mut accepted = 0usize;
  for literal in CORPUS {
    let bytes = literal.as_bytes();
    if parse_i64(bytes).is_some() {
      accepted += 1;
      assert!(
        is_integer_literal(bytes),
        "{literal:?} converts at `i64` and the shape check refuses it",
      );
    }
  }

  // Non-vacuity twice over: the implication above is satisfied by a corpus the reader rejects
  // wholesale, and the shape check is not the constant `true`.
  assert!(accepted >= 8, "only {accepted} corpus literals convert");
  assert!(!is_integer_literal(b"hello"));
  assert!(!is_integer_literal(b""));
  assert!(!is_integer_literal(b"-"));
  assert!(
    is_integer_literal(b"007"),
    "a leading zero is not this check's business"
  );
}

/// [`overflows`] is the decider behind the public `IntOverflow::checked`, and it is held against
/// an oracle **this crate did not write**: `core`'s own `str::parse`, which reports an
/// out-of-range integer as `IntErrorKind::PosOverflow`/`NegOverflow` and a non-number as
/// `InvalidDigit` or `Empty`. That distinction is exactly the conjunction this predicate makes,
/// arrived at independently.
///
/// An oracle recomputed from `is_integer_literal(bytes) && reader.is_none()` would be the
/// implementation transcribed, and would stay green over a version of the function that dropped
/// either conjunct. This one cannot: `core` has no idea what this file does.
///
/// **One deliberate divergence, and it is not in the corpus.** `core` accepts a leading `+`;
/// draft §2.9.1's `IntegerPart` has no unary plus, so `+99999999999999999999999999` is
/// `PosOverflow` to `core` and not an integer literal here. `the_unary_plus_divergence_is_ours`
/// below pins it rather than leaving the corpus quietly curated around it.
#[test]
fn overflows_agrees_with_cores_own_reader_on_which_failures_are_overflows() {
  use core::num::IntErrorKind;

  fn core_calls_it_an_overflow(literal: &str, width: IntWidth) -> bool {
    let kind = match width {
      IntWidth::I32 => literal.parse::<i32>().err().map(|e| *e.kind()),
      IntWidth::I64 => literal.parse::<i64>().err().map(|e| *e.kind()),
    };
    matches!(
      kind,
      Some(IntErrorKind::PosOverflow | IntErrorKind::NegOverflow)
    )
  }

  let (mut agreed_overflow, mut agreed_not) = (0usize, 0usize);
  for literal in CORPUS {
    for width in [IntWidth::I32, IntWidth::I64] {
      let expected = core_calls_it_an_overflow(literal, width);
      assert_eq!(
        overflows(literal.as_bytes(), width),
        expected,
        "{literal:?} at {width}: `core` says overflow={expected}",
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

  // The pair the whole round is about, spelled out rather than left to the loop.
  assert!(overflows(b"2147483648", IntWidth::I32));
  assert!(!overflows(b"2147483648", IntWidth::I64));
}

/// The one place [`overflows`] and `core`'s reader are *meant* to disagree, named so the corpus
/// above is a corpus rather than a curation.
///
/// `core` accepts a leading `+` and draft §2.9.1 does not, so a `+`-prefixed literal past a width
/// is an overflow to `core` and not an integer literal at all here. Ours is the right answer for
/// a GraphQL `IntValue`, and a payload built for `+99999999999999999999999999` would be quoting
/// something the grammar cannot produce.
#[test]
fn the_unary_plus_divergence_is_ours() {
  assert!(
    "+99999999999999999999999999".parse::<i64>().is_err(),
    "the oracle has to refuse it for the divergence to exist",
  );
  assert!(!is_integer_literal(b"+99999999999999999999999999"));
  assert!(!overflows(b"+99999999999999999999999999", IntWidth::I64));
  assert!(!overflows(b"+7", IntWidth::I32));
}

/// `Float` is `f64` at both markers, so the float conversion must be the same conversion.
#[test]
fn the_float_leaf_is_the_same_at_both_widths() {
  assert_eq!(
    <MaterializedNumbers32 as Numbers<&str>>::float("7.5").ok(),
    <MaterializedNumbers as Numbers<&str>>::float("7.5").ok(),
  );
  assert_eq!(
    <MaterializedNumbers32 as Numbers<&str>>::float("7.5").ok(),
    Some(7.5)
  );

  let float = <MaterializedNumbers32 as Numbers<&str>>::float("1e400");
  assert!(matches!(float, Err(OutOfRange::Float("1e400"))));
}
