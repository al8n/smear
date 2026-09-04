use super::{LitFloat, LitInt};

/// Every form the number grammar accepts round-trips through the door, and its own text comes back.
///
/// The list is derived from `syntactic/number.rs`'s `logos` grammar rather than from the corpus:
/// four integer radices, two float radices, the optional sign on each, the `_` separator both
/// grammars admit, and the two float shapes with no integer part or no fraction.
#[test]
fn every_form_the_grammar_accepts_round_trips() {
  for (form, text, expected) in [
    ("decimal", "16", LitInt::Decimal("16")),
    ("decimal, signed", "-16", LitInt::Decimal("-16")),
    ("decimal, separated", "1_000", LitInt::Decimal("1_000")),
    ("decimal, leading zeros", "007", LitInt::Decimal("007")),
    ("hexadecimal", "0x10", LitInt::Hex("0x10")),
    ("hexadecimal, signed", "-0x10", LitInt::Hex("-0x10")),
    (
      "hexadecimal, separated",
      "0x_dead_beef",
      LitInt::Hex("0x_dead_beef"),
    ),
    ("octal", "0o20", LitInt::Octal("0o20")),
    ("octal, signed", "-0o20", LitInt::Octal("-0o20")),
    ("binary", "0b10000", LitInt::Binary("0b10000")),
    ("binary, signed", "-0b10000", LitInt::Binary("-0b10000")),
  ] {
    assert_eq!(
      LitInt::try_from(text),
      Ok(expected),
      "{form}: the door did not read back what the scanner reads"
    );
    assert!(
      LitFloat::try_from(text).is_err(),
      "{form}: an integer is not a float, and the door must not widen it"
    );
  }

  for (form, text, expected) in [
    ("float", "1.5", LitFloat::Decimal("1.5")),
    ("float, exponent", "1.5e3", LitFloat::Decimal("1.5e3")),
    (
      "float, signed exponent",
      "1.5e-3",
      LitFloat::Decimal("1.5e-3"),
    ),
    ("float, exponent only", "1e3", LitFloat::Decimal("1e3")),
    ("float, signed", "-1.5", LitFloat::Decimal("-1.5")),
    ("hex float", "0x1.8p3", LitFloat::Hex("0x1.8p3")),
    ("hex float, signed", "-0x1.8p3", LitFloat::Hex("-0x1.8p3")),
    ("hex float, no fraction", "0x1p3", LitFloat::Hex("0x1p3")),
  ] {
    assert_eq!(
      LitFloat::try_from(text),
      Ok(expected),
      "{form}: the door did not read back what the scanner reads"
    );
    assert!(
      LitInt::try_from(text).is_err(),
      "{form}: a float is not an integer, and the door must not narrow it"
    );
  }
}

/// The spellings a prefix test admits and the grammar does not.
///
/// Each of these produced a literal before al8n/smear#58, because the classifier the projection
/// carried inspected a prefix and never asked the scanner. `007` is on the list Codex reported and
/// is **not** here: the decimal production is `-?(?&digit)[0-9_]*`, so a leading zero is a
/// perfectly good GraphQLx decimal and the pin for it is in the round-trip test above.
#[test]
fn the_spellings_a_prefix_test_would_have_admitted_are_refused() {
  for (what, text) in [
    ("a binary digit that is not one", "0b2"),
    ("an octal digit that is not one", "0o8"),
    ("a radix prefix with no digits", "0x"),
    ("a binary prefix with no digits", "0b"),
    ("an octal prefix with no digits", "0o"),
    ("an exponent with no mantissa radix", "1p2"),
    ("a decimal suffix", "123abc"),
  ] {
    assert!(
      LitInt::try_from(text).is_err(),
      "{what} ({text}) read back as an integer"
    );
    assert!(
      LitFloat::try_from(text).is_err(),
      "{what} ({text}) read back as a float"
    );
  }

  // The wrong-kind pair, stated on its own: each of these IS a literal, and it is not the one the
  // caller asked for.
  assert!(
    LitFloat::try_from("0x1").is_err(),
    "`0x1` is a hexadecimal integer; asking for a float must refuse rather than relabel it"
  );
  assert!(
    LitInt::try_from("1.5").is_err(),
    "`1.5` is a decimal float; asking for an integer must refuse rather than truncate it"
  );
}

/// **Whole** means the whole slice, which is what makes this a re-cooking door rather than a
/// prefix classifier with more steps.
#[test]
fn a_slice_that_is_not_exactly_one_literal_is_refused() {
  for (what, text) in [
    ("nothing at all", ""),
    ("leading trivia", " 1"),
    ("trailing trivia", "1 "),
    ("two literals", "1 2"),
    ("a literal and a name", "1 x"),
    ("a name", "abc"),
    ("a comment after it", "1 # c"),
  ] {
    assert!(
      LitInt::try_from(text).is_err(),
      "{what} ({text:?}) read back as an integer"
    );
    assert!(
      LitFloat::try_from(text).is_err(),
      "{what} ({text:?}) read back as a float"
    );
  }
}

/// Every identifier shape the scanner accepts comes back as its own text.
///
/// Derived from the lexer's identifier rule rather than from a corpus: the two first-character
/// classes, digits from the second character on, an interior `_`, and a contextual keyword — which
/// is an identifier here and only becomes a keyword when [`super::ContextualKeyword`] classifies
/// it, so the door must not refuse one.
#[test]
fn every_identifier_shape_round_trips() {
  for (form, text) in [
    ("bare", "field"),
    ("leading underscore", "_field"),
    ("all underscores", "___"),
    ("digits after the first character", "f1"),
    ("interior underscore", "a_b"),
    ("upper case", "FIELD"),
    ("mixed", "Query_1"),
    ("a contextual keyword", "query"),
    ("the excluded fragment name", "on"),
    ("one character", "a"),
  ] {
    assert_eq!(
      super::identifier(text),
      Ok(text),
      "{form}: `{text}` is one whole identifier to the shipped scanner"
    );
  }
}

/// What the door refuses, and why each one matters to its caller.
///
/// The projection reads a name by the range of a token the *tree* labelled `Name`, so every
/// spelling below is one a caller-minted tree could put there and none is a name the parser
/// produces. `not one whole token` is the class that a prefix scan would have accepted.
#[test]
fn a_spelling_that_is_not_one_whole_identifier_refuses() {
  for (form, text) in [
    ("empty", ""),
    ("a number", "1"),
    ("a number with a suffix", "1a"),
    ("two identifiers", "a b"),
    ("an identifier and a token", "a::b"),
    ("an identifier and punctuation", "a!"),
    ("punctuation", "@"),
    ("a string literal", "\"a\""),
    ("leading space", " a"),
    ("trailing space", "a "),
    ("a comment after it", "a # c"),
  ] {
    assert!(
      super::identifier(text).is_err(),
      "{form}: `{text}` is not one whole identifier and the door must say so"
    );
  }
}
