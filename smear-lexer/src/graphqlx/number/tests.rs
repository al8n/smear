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

/// A typed literal answers its suffix, read off its own bytes, and how many bytes the suffix
/// takes: the one `_` directly before its letter included. A literal with none answers `None` and
/// 0, and so does one built over bytes that end in no suffix.
#[test]
fn a_typed_literal_answers_its_suffix() {
  use super::FloatSuffix;

  for (text, signed, width, len) in [
    ("1u8", false, 8u32, 2usize),
    ("-3i256", true, 256, 4),
    ("0xFF_u32", false, 32, 4),
    ("0b1010u8", false, 8, 2),
    ("0o17i16", true, 16, 3),
    ("0u0", false, 0, 2),
  ] {
    let lit = LitInt::try_from(text).unwrap_or_else(|e| panic!("{text:?} does not lex: {e:?}"));
    let suffix = lit
      .suffix()
      .unwrap_or_else(|| panic!("{text:?} answers no suffix"));
    assert_eq!(
      (suffix.is_signed(), suffix.width(), lit.suffix_len()),
      (signed, width, len),
      "{text:?}"
    );
  }

  for (text, suffix, len) in [
    ("1.5f32", FloatSuffix::F32, 3usize),
    ("1f64", FloatSuffix::F64, 3),
    ("1e3_f32", FloatSuffix::F32, 4),
    ("0x1p3f32", FloatSuffix::F32, 3),
  ] {
    let lit = LitFloat::try_from(text).unwrap_or_else(|e| panic!("{text:?} does not lex: {e:?}"));
    assert_eq!(
      (lit.suffix(), lit.suffix_len()),
      (Some(suffix), len),
      "{text:?}"
    );
  }

  let hex_digits = LitInt::Hex("0x1f32");
  assert!(hex_digits.suffix().is_none() && hex_digits.suffix_len() == 0);
  let plain = LitFloat::Decimal("1.5");
  assert!(plain.suffix().is_none() && plain.suffix_len() == 0);
  let built = LitInt::Decimal(&b"zz"[..]);
  assert!(built.suffix().is_none() && built.suffix_len() == 0);
}

/// The `char` literal's door reads back what the scanner reads, and refuses a slice that is not
/// one whole literal: a prefix of one, trivia around it, two of them, a refused literal, and a
/// token of another kind.
#[test]
fn the_char_door_reads_one_whole_literal() {
  use super::LitChar;

  for (text, value) in [
    ("'a'", 'a'),
    ("'\\n'", '\n'),
    ("'\\u{1F600}'", '\u{1F600}'),
    ("'\\''", '\''),
    ("'\"'", '"'),
  ] {
    let lit = LitChar::try_from(text).unwrap_or_else(|e| panic!("{text:?} does not lex: {e:?}"));
    assert_eq!((*lit.source_ref(), lit.value()), (text, value), "{text:?}");
  }

  for text in ["'a", "'", " 'a'", "'a' ", "'a''b'", "''", "'ab'", "a", "1"] {
    assert!(
      LitChar::try_from(text).is_err(),
      "{text:?} is not one whole `char` literal"
    );
  }
}

/// The refusal that `lexer`'s first item is, or `None` when that item is a token or there is none.
fn first_refusal<'a, L>(mut lexer: L) -> Option<<L::Token as tokora::Token<'a>>::Error>
where
  L: tokora::Lexer<'a>,
{
  lexer.lex()?.err()
}

/// A `char` literal's body is read by an inline string's rules. Each malformed escape here is
/// refused in a `char` literal with the errors the same body gets in a string, at the same offsets,
/// since both literals open with one quote; it is one unit, so the literal has no fault of its own.
/// Both lexers, over text and over bytes.
#[test]
fn a_char_body_is_refused_as_a_string_body_is() {
  use std::format;
  use tokora::Lexer as _;

  use super::{lossless::LosslessLexer, syntactic::SyntacticLexer};

  for body in [
    r"\u",
    r"\u12",
    r"\u12zz",
    r"\uD800",
    r"\uDC00",
    r"\u{}",
    r"\u{zz}",
    r"\u{12",
    r"\u{1234567}",
    r"\u{110000}",
    r"\u{D800}",
    r"\q",
  ] {
    let (string, character) = (format!("\"{body}\""), format!("'{body}'"));
    let refused = first_refusal(SyntacticLexer::<str>::new(&string));
    assert!(refused.is_some(), "{string:?} is refused");
    assert_eq!(
      first_refusal(SyntacticLexer::<str>::new(&character)),
      refused,
      "{character:?}"
    );
    assert_eq!(
      first_refusal(SyntacticLexer::<[u8]>::new(character.as_bytes())),
      first_refusal(SyntacticLexer::<[u8]>::new(string.as_bytes())),
      "{character:?} as bytes"
    );
    assert_eq!(
      first_refusal(LosslessLexer::<&str>::new(&character)),
      first_refusal(LosslessLexer::<&str>::new(&string)),
      "{character:?}, lossless"
    );
    assert_eq!(
      first_refusal(LosslessLexer::<&[u8]>::new(character.as_bytes())),
      first_refusal(LosslessLexer::<&[u8]>::new(string.as_bytes())),
      "{character:?} as bytes, lossless"
    );
  }
}

/// A raw control character gets a string's verdict in a `char` literal. Every C0 character, DEL and
/// every C1 character stands in a string but a line terminator, which ends the string before it,
/// and exactly the same ones stand in a `char` literal as its character, over text and over bytes.
#[test]
fn a_raw_control_character_gets_a_strings_verdict() {
  use std::format;
  use tokora::Lexer as _;

  use super::syntactic::{SyntacticLexer, SyntacticToken};

  for ch in ('\0'..='\u{1f}').chain('\u{7f}'..='\u{9f}') {
    let string = format!("\"{ch}\"");
    let accepted = SyntacticLexer::<str>::new(&string)
      .lex()
      .is_some_and(|item| item.is_ok());
    assert_eq!(accepted, !matches!(ch, '\n' | '\r'), "{ch:?} in a string");

    let character = format!("'{ch}'");
    let text = SyntacticLexer::<str>::new(&character).lex();
    let bytes = SyntacticLexer::<[u8]>::new(character.as_bytes()).lex();
    match (text, bytes) {
      (Some(Ok(SyntacticToken::LitChar(text))), Some(Ok(SyntacticToken::LitChar(bytes)))) => {
        assert!(accepted, "{ch:?} stands in a `char` literal alone");
        assert_eq!((text.value(), bytes.value()), (ch, ch), "{ch:?}");
      }
      (Some(Err(_)), Some(Err(_))) => assert!(!accepted, "{ch:?} is refused in a `char` alone"),
      other => panic!("{ch:?}: {other:?}"),
    }
  }
}

/// A `char` literal refused for its body and for itself reports both, each over the whole
/// literal: the body's faults first, as a string reports them, and then its own, as a string
/// reports last that its line ended it.
#[test]
fn a_char_refused_for_its_body_and_itself_reports_both() {
  use tokora::Lexer as _;

  use super::{error::LexerErrorData, syntactic::SyntacticLexer};

  for (text, faults, fault) in [
    ("'\\q", 1, LexerErrorData::UnterminatedChar),
    ("'\\q\\z'", 2, LexerErrorData::LongChar),
  ] {
    let errors = first_refusal(SyntacticLexer::<str>::new(text))
      .unwrap_or_else(|| panic!("{text:?} is refused"));
    let [body, own] = errors.as_slice() else {
      panic!("{text:?}: {errors:?}");
    };
    assert!(
      matches!(body.data(), LexerErrorData::String(found) if found.len() == faults
        && found.iter().all(|error| error.is_unexpected_escaped_character())),
      "{text:?}: {body:?}"
    );
    assert_eq!(own.data(), &fault, "{text:?}");
    let whole = tokora::SimpleSpan::new(0, text.len());
    assert_eq!((body.span(), own.span()), (whole, whole), "{text:?}");
  }
}
