use super::{MaterializedNumbers, Numbers, OutOfRange, parse_f64, parse_i64};

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
    Err(OutOfRange::Int("99999999999999999999999999"))
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
