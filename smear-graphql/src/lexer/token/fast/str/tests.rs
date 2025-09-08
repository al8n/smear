use logosky::utils::Span;

use super::*;

fn assert_token(source: &str, kind: Token, length: usize) {
  let mut lexer = Token::lexer(source);
  assert_eq!(
    lexer.next(),
    Some(Ok(kind)),
    "Testing the lexing of string '{source}'"
  );
  assert_eq!(
    lexer.span(),
    0..length,
    "Testing the lexing of string '{source}'"
  );
}

#[test]
fn test_unexpected_character() {
  let mut lexer = Token::lexer("+1");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_unexpected_lexeme()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'+');
  assert_eq!(err.position(), 0);

  let mut lexer = Token::lexer("-A");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_unexpected_lexeme()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'-');
  assert_eq!(err.position(), 0);
}

#[test]
fn test_unknown_character() {
  let mut lexer = Token::lexer("<");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_unknown_lexeme()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'<');
  assert_eq!(err.position(), 0);

  const INPUT: &str = r#"
# U+FEFF
# U+000B
# U+000C
# U+0085
 # U+00A0
‎# U+200E
‏# U+200F
 # U+2028
 # U+2029
  "#;

  let mut lexer = Token::lexer(INPUT);
  for expected_char in [
    '\u{000B}', '\u{000C}', '\u{0085}', '\u{00A0}', '\u{200E}', '\u{200F}', '\u{2028}', '\u{2029}',
  ] {
    let err = lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data()
      .unwrap_unknown_lexeme()
      .unwrap_char();
    assert_eq!(err.char_ref(), &expected_char);
  }
}

#[test]
fn test_number_leading_zero() {
  let mut lexer = Token::lexer("00");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Int(IntError::LeadingZeros(_))
  ));

  let mut lexer = Token::lexer("-01");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Int(IntError::LeadingZeros(_))
  ));

  let mut lexer = Token::lexer("01.23");
  // let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_leading_zero();
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::lexer("-01.23");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::lexer("01e3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::lexer("-01E3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::lexer("01e+3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::lexer("-01E+3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::lexer("01e-3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::lexer("-01E-3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::LeadingZeros(_))
  ));
}

#[test]
fn test_int_leading_zeros_and_suffix() {
  let mut lexer = Token::lexer("00abc");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  let err1 = errs[0]
    .data()
    .unwrap_int_ref()
    .unwrap_leading_zeros_ref()
    .unwrap_span_ref();
  assert_eq!(err1, &Span::from(0..2));
  let err2 = errs[1]
    .data()
    .unwrap_int_ref()
    .unwrap_unexpected_suffix_ref()
    .unwrap_span_ref();
  assert_eq!(err2, &Span::from(2..5));

  let mut lexer = Token::lexer("-00abc");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  let err1 = errs[0]
    .data()
    .unwrap_int_ref()
    .unwrap_leading_zeros_ref()
    .unwrap_span_ref();
  assert_eq!(err1, &Span::from(1..3));
  let err2 = errs[1]
    .data()
    .unwrap_int_ref()
    .unwrap_unexpected_suffix_ref()
    .unwrap_span_ref();
  assert_eq!(err2, &Span::from(3..6));
}

#[test]
fn test_float_leading_zeros_and_other() {
  let mut lexer = Token::lexer("01.");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  let err1 = errs[0]
    .data()
    .unwrap_float_ref()
    .unwrap_leading_zeros_ref()
    .unwrap_char_ref();
  assert_eq!(err1.char_ref(), &'0');
  assert_eq!(err1.position(), 0);
  let err2 = errs[1]
    .data()
    .unwrap_float_ref()
    .unwrap_unexpected_end_ref();
  assert_eq!(err2.hint(), &FloatHint::Fractional);
  assert_eq!(errs[1].span(), Span::from(0..3));

  let mut lexer = Token::lexer("-01.");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  let err1 = errs[0]
    .data()
    .unwrap_float_ref()
    .unwrap_leading_zeros_ref()
    .unwrap_char_ref();
  assert_eq!(err1.char_ref(), &'0');
  assert_eq!(err1.position(), 1);
  let err2 = errs[1]
    .data()
    .unwrap_float_ref()
    .unwrap_unexpected_end_ref();
  assert_eq!(err2.hint(), &FloatHint::Fractional);
  assert_eq!(errs[1].span(), Span::from(0..4));

  let mut lexer = Token::lexer("00001.23abcd");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  let err1 = errs[0]
    .data()
    .unwrap_float_ref()
    .unwrap_leading_zeros_ref()
    .unwrap_span_ref();
  assert_eq!(err1, &Span::from(0..4));
  let err2 = errs[1]
    .data()
    .unwrap_float_ref()
    .unwrap_unexpected_suffix_ref()
    .unwrap_span_ref();
  assert_eq!(err2, &Span::from(8..12));
}

#[test]
fn test_invalid_number_suffix() {
  let mut lexer = Token::lexer("0abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, (1..4).into());

  let mut lexer = Token::lexer("0a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 1);

  let mut lexer = Token::lexer("-0abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, Span::from(2..5));

  let mut lexer = Token::lexer("-0a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 2);

  let mut lexer = Token::lexer("123abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, Span::from(3..6));

  let mut lexer = Token::lexer("123a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 3);

  let mut lexer = Token::lexer("-123abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, Span::from(4..7));

  let mut lexer = Token::lexer("-123a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 4);

  let mut lexer = Token::lexer("123.45a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 6);

  let mut lexer = Token::lexer("-123.45a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 7);

  let mut lexer = Token::lexer("123e3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 5);

  let mut lexer = Token::lexer("-123E3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 6);

  let mut lexer = Token::lexer("123e+3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 6);

  let mut lexer = Token::lexer("-123E+3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 7);

  let mut lexer = Token::lexer("123e-3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 6);

  let mut lexer = Token::lexer("-123E-3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 7);

  let mut lexer = Token::lexer("1.23.4");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, Span::from(4..6));

  let mut lexer = Token::lexer("-1.23.4 ");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, Span::from(5..7));
  assert_eq!(lexer.span(), 0..7);

  // check that we don't consume trailing valid items
  let mut lexer = Token::lexer("1.23.{}");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'.');
  assert_eq!(err.position(), 4);
  assert_eq!(lexer.span(), 0..5);

  let mut lexer = Token::lexer("1.23. {}");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'.');
  assert_eq!(err.position(), 4);
  assert_eq!(lexer.span(), 0..5);

  let mut lexer = Token::lexer("1.23. []");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'.');
  assert_eq!(err.position(), 4);
  assert_eq!(lexer.span(), 0..5);

  let mut lexer = Token::lexer("1.23. foo");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'.');
  assert_eq!(err.position(), 4);
  assert_eq!(lexer.span(), 0..5);

  let mut lexer = Token::lexer("1.23. $foo");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'.');
  assert_eq!(err.position(), 4);
  assert_eq!(lexer.span(), 0..5);

  // // assert_token(".123", Token::ErrorFloatLiteralMissingZero, 4);
}

#[test]
fn test_missing_integer_part() {
  let mut lexer = Token::lexer(".123");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::MissingIntegerPart)
  ));

  let mut lexer = Token::lexer("-.123");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::MissingIntegerPart)
  ));

  let mut lexer = Token::lexer(".123e3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::MissingIntegerPart)
  ));
  let mut lexer = Token::lexer("-.123E3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::MissingIntegerPart)
  ));

  let mut lexer = Token::lexer(".123e+3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::MissingIntegerPart)
  ));
  let mut lexer = Token::lexer("-.123E+3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    ErrorData::Float(FloatError::MissingIntegerPart)
  ));
}

#[test]
fn test_missing_integer_part_and_invalid_suffix() {
  let mut lexer = Token::lexer(".123abcd");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);

  errs[0]
    .data()
    .unwrap_float_ref()
    .unwrap_missing_integer_part_ref();
  let err2 = errs[1]
    .data()
    .unwrap_float_ref()
    .unwrap_unexpected_suffix_ref()
    .unwrap_span();
  assert_eq!(err2, Span::from(4..8));

  let mut lexer = Token::lexer("-.123abcd");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  errs[0]
    .data()
    .unwrap_float_ref()
    .unwrap_missing_integer_part_ref();
  let err2 = errs[1]
    .data()
    .unwrap_float_ref()
    .unwrap_unexpected_suffix_ref()
    .unwrap_span();
  assert_eq!(err2, Span::from(5..9));
}

#[test]
fn test_unexpected_float_eof() {
  let mut lexer = Token::lexer("1.");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Fractional);

  let mut lexer = Token::lexer("-1.");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Fractional);

  let mut lexer = Token::lexer("1e");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

  let mut lexer = Token::lexer("-1e");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

  let mut lexer = Token::lexer("1e+");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::lexer("-1e+");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::lexer("1e-");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::lexer("-1e-");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::lexer("1.0e");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

  let mut lexer = Token::lexer("-1.0e");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

  let mut lexer = Token::lexer("1.0e-");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::lexer("-1.0e-");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::lexer("1.0e+");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::lexer("-1.0e+");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));
}

#[test]
fn test_unexpected_number_lexme() {
  let mut lexer = Token::lexer("1.a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'a');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 2);
  assert_eq!(lexer.span(), 0..3);

  let mut lexer = Token::lexer("-1.a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'a');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 3);
  assert_eq!(lexer.span(), 0..4);

  let mut lexer = Token::lexer("1.A");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 2);
  assert_eq!(lexer.span(), 0..3);

  let mut lexer = Token::lexer("-1.A");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 3);
  assert_eq!(lexer.span(), 0..4);

  let mut lexer = Token::lexer("1.abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_span(), (2..5).into());
  assert_eq!(lexer.span(), (0..5));

  let mut lexer = Token::lexer("-1.abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_span(), (3..6).into());
  assert_eq!(lexer.span(), (0..6));

  let mut lexer = Token::lexer("1.e1");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_span(), (2..4).into());
  assert_eq!(lexer.span(), (0..4));

  let mut lexer = Token::lexer("-1.e1");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_span(), (3..5).into());
  assert_eq!(lexer.span(), (0..5));

  let mut lexer = Token::lexer("1.0eA");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 4);
  assert_eq!(lexer.span(), (0..5));

  let mut lexer = Token::lexer("-1.0eA");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 5);
  assert_eq!(lexer.span(), 0..6);

  let mut lexer = Token::lexer("1.0eA123.456 some_name");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (4..12).into());
  assert_eq!(lexer.span(), 0..12);

  let mut lexer = Token::lexer("-1.0eA123.456 some_name");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (5..13).into());
  assert_eq!(lexer.span(), 0..13);

  let mut lexer = Token::lexer("1eA");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 2);
  assert_eq!(lexer.span(), 0..3);

  let mut lexer = Token::lexer("-1eA");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 3);
  assert_eq!(lexer.span(), 0..4);

  let mut lexer = Token::lexer("1eA123.456");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (2..10).into());
  assert_eq!(lexer.span(), 0..10);

  let mut lexer = Token::lexer("-1eA123.456");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (3..11).into());
  assert_eq!(lexer.span(), (0..11));

  let mut lexer = Token::lexer("1eA123.456 some_name");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (2..10).into());
  assert_eq!(lexer.span(), (0..10));

  let mut lexer = Token::lexer("-1eA123.456 some_name");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (3..11).into());
  assert_eq!(lexer.span(), (0..11));
}

#[test]
fn test_integer_ok() {
  const INPUT: &[(&str, Token, usize)] = &[
    ("4", Token::IntegerLiteral("4"), 1),
    ("-4", Token::IntegerLiteral("-4"), 2),
    ("9", Token::IntegerLiteral("9"), 1),
    ("0", Token::IntegerLiteral("0"), 1),
    ("-0", Token::IntegerLiteral("-0"), 2),
  ];

  for (source, kind, length) in INPUT {
    assert_token(source, *kind, *length);
  }
}

#[test]
fn test_float_ok() {
  const INPUT: &[(&str, Token, usize)] = &[
    ("4.123", Token::FloatLiteral("4.123"), 5),
    ("-4.123", Token::FloatLiteral("-4.123"), 6),
    ("0.123", Token::FloatLiteral("0.123"), 5),
    ("123e4", Token::FloatLiteral("123e4"), 5),
    ("123E4", Token::FloatLiteral("123E4"), 5),
    ("123e-4", Token::FloatLiteral("123e-4"), 6),
    ("123e+4", Token::FloatLiteral("123e+4"), 6),
    ("-1.123e4", Token::FloatLiteral("-1.123e4"), 8),
    ("-1.123E4", Token::FloatLiteral("-1.123E4"), 8),
    ("-1.123e-4", Token::FloatLiteral("-1.123e-4"), 9),
    ("-1.123e+4", Token::FloatLiteral("-1.123e+4"), 9),
    ("-1.123e4567", Token::FloatLiteral("-1.123e4567"), 11),
  ];

  for (source, kind, length) in INPUT {
    assert_token(source, *kind, *length);
  }
}

#[test]
fn test_unterminated_inline_string() {
  let mut lexer = Token::lexer(r#"""#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(err.span(), Span::from(0..1));
  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::Quote);
  assert_eq!(lexer.span(), 0..1);

  let mut lexer = Token::lexer(r#""unterminated"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);

  let err1 = errs.pop().unwrap();
  assert_eq!(err1.span(), Span::from(0..13));

  let err1 = err1
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::Quote);
  assert_eq!(lexer.span(), 0..13);

  let mut lexer = Token::lexer(r#""escaped \" quote"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(err.span(), Span::from(0..17));
  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::Quote);

  let mut lexer = Token::lexer(
    r#""escaped 
  new line""#,
  );
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(err.span(), Span::from(0..21));
  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unexpected_line_terminator();
  match err1.hint() {
    LineTerminatorHint::NewLine => {
      assert_eq!(err1.lexeme().unwrap_char_ref().char_ref(), &'\n');
      assert_eq!(err1.lexeme().unwrap_char_ref().position(), 9);
    }
    LineTerminatorHint::CarriageReturn => {
      assert_eq!(err1.lexeme().unwrap_char_ref().char_ref(), &'\r');
      assert_eq!(err1.lexeme().unwrap_char_ref().position(), 9);
    }
    LineTerminatorHint::CarriageReturnNewLine => {
      assert_eq!(err1.lexeme().unwrap_span(), Span::from(9..11));
    }
  }

  let mut lexer = Token::lexer(r#""hello✨"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(err.span(), Span::from(0..r#""hello✨"#.len()));
  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::Quote);

  let mut lexer = Token::lexer(r#""\n\n\\u{c}\nPSK\\u{1}\\0\\0\\0י"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(
    err.span(),
    Span::from(0..r#""\n\n\\u{c}\nPSK\\u{1}\\0\\0\\0י"#.len())
  );

  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::Quote);
}

#[test]
fn test_incomplete_unicode_and_eof() {
  let mut lexer = Token::lexer(r#""\u222"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();

  assert_eq!(err.span(), Span::from(0..6));

  let err = err.into_data().unwrap_string();
  assert_eq!(err.len(), 2, "Expected 2 errors, got {err:?}");
  let err1 = err[0].unwrap_unicode_ref().unwrap_incomplete_ref();
  assert_eq!(err1.span(), Span::from(1..6));

  let err2 = err[1].unwrap_unterminated_ref();
  assert_eq!(err2.hint(), &UnterminatedHint::Quote);
  assert_eq!(lexer.span(), 0..6);

  let mut lexer = Token::lexer(r#""\u"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();

  assert_eq!(err.span(), Span::from(0..3));

  let err = err.into_data().unwrap_string();
  assert_eq!(err.len(), 2, "Expected 2 errors, got {err:?}");
  let err1 = err[0].unwrap_unicode_ref().unwrap_incomplete_ref();
  assert_eq!(err1.span(), Span::from(1..3));

  let err2 = err[1].unwrap_unterminated_ref();
  assert_eq!(err2.hint(), &UnterminatedHint::Quote);
  assert_eq!(lexer.span(), 0..3);
}

#[test]
fn test_unexpected_line_terminator() {
  let mut lexer = Token::lexer(
    r#""
hello
""#,
  );

  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let errs = errs.pop().unwrap().into_data().unwrap_string();
  assert_eq!(errs.len(), 2, "Expected 2 errors, got {errs:?}");

  let err1 = errs[0].unwrap_unexpected_line_terminator_ref();
  match err1.hint() {
    LineTerminatorHint::NewLine => {
      assert_eq!(err1.lexeme().unwrap_char_ref().char_ref(), &'\n');
      assert_eq!(err1.lexeme().unwrap_char_ref().position(), 1);
    }
    LineTerminatorHint::CarriageReturn => {
      assert_eq!(err1.lexeme().unwrap_char_ref().char_ref(), &'\r');
      assert_eq!(err1.lexeme().unwrap_char_ref().position(), 1);
    }
    LineTerminatorHint::CarriageReturnNewLine => {
      assert_eq!(err1.lexeme().unwrap_span(), Span::from(1..3));
    }
  }
  assert_eq!(lexer.span(), 0..9);

  let err2 = errs[1].unwrap_unexpected_line_terminator_ref();
  match err2.hint() {
    LineTerminatorHint::NewLine => {
      assert_eq!(err2.lexeme().unwrap_char_ref().char_ref(), &'\n');
      assert_eq!(err2.lexeme().unwrap_char_ref().position(), 7);
    }
    LineTerminatorHint::CarriageReturn => {
      assert_eq!(err2.lexeme().unwrap_char_ref().char_ref(), &'\r');
      assert_eq!(err2.lexeme().unwrap_char_ref().position(), 7);
    }
    LineTerminatorHint::CarriageReturnNewLine => {
      assert_eq!(err2.lexeme().unwrap_span(), Span::from(7..9));
    }
  }
  assert_eq!(lexer.span(), 0..9);
}

#[test]
fn test_unexpected_escaped() {
  // "This is \"\"a test \a\d\q description"
  let mut lexer = Token::lexer(r#""This is \"\"a test \a\d\q description""#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err1 = errs.pop().unwrap().into_data().unwrap_string();
  assert_eq!(err1.len(), 3, "Expected 3 errors, got {err1:?}");
  assert!(
    errs.is_empty(),
    "Expected only one error, remaining {errs:?}"
  );

  let unexpected = err1[0].unwrap_unexpected_escaped_character_ref();
  assert_eq!(unexpected.char(), 'a');
  assert_eq!(unexpected.position(), 21);

  let unexpected = err1[1].unwrap_unexpected_escaped_character_ref();
  assert_eq!(unexpected.char(), 'd');
  assert_eq!(unexpected.position(), 23);

  let unexpected = err1[2].unwrap_unexpected_escaped_character_ref();
  assert_eq!(unexpected.char(), 'q');
  assert_eq!(unexpected.position(), 25);
}

#[test]
fn test_surrogate_pair() {
  const CASES: &[&str] = &[
    r#""string with unicode surrogate pair escape \uD83D\uDE00""#,
    r#""string with minimal surrogate pair escape \uD800\uDC00""#,
    r#""string with maximal surrogate pair escape \uDBFF\uDFFF""#,
  ];

  for case in CASES {
    let mut lexer = Token::lexer(case);
    let token = lexer.next().unwrap().expect(case);
    assert_eq!(token, Token::StringLiteral(case), "case: {case}");

    assert_eq!(lexer.slice(), *case, "case: {case}");
    assert_eq!(lexer.next(), None, "case: {case}");
  }
}

// #[test]
// fn test_invalid_surrogate() {

//   let mut lexer = Token::lexer(r#""Backwards pair \uDE00\uD83D""#);
//   let mut errs = lexer.next().unwrap().unwrap_err();
//   assert_eq!(errs.len(), 1);
//   let errs = errs.pop().unwrap().into_data().unwrap_string();
//   assert_eq!(errs.len(), 2, "Expected 2 errors, got {errs:?}");
//   let err1 = &errs[0];
//   let err = err1.unwrap_unicode_ref().unwrap_invalid_surrogate_ref();
//   assert_eq!(err.span(), Span::from(16..22));

//   let err2 = &errs[1];
//   let err = err2.unwrap_unicode_ref().unwrap_invalid_surrogate_ref();
//   assert_eq!(err.span(), Span::from(22..28));

//   let mut lexer = Token::lexer(r#""split pair \uD83D \uDE00""#);
//   let mut errs = lexer.next().unwrap().unwrap_err();
//   assert_eq!(errs.len(), 1);
//   let errs = errs.pop().unwrap().into_data().unwrap_string();
//   assert_eq!(errs.len(), 2, "Expected 2 errors, got {errs:?}");
//   let err1 = &errs[0];
//   let err = err1.unwrap_unicode_ref().unwrap_invalid_surrogate_ref();
//   assert_eq!(err.span(), Span::from(12..18));

//   let err2 = &errs[1];
//   let err = err2.unwrap_unicode_ref().unwrap_invalid_surrogate_ref();
//   assert_eq!(err.span(), Span::from(19..25));

//   let mut lexer = Token::lexer(r#""Lone lead surrogate \uD83E""#);
//   let mut errs = lexer.next().unwrap().unwrap_err();
//   assert_eq!(errs.len(), 1);
//   let errs = errs.pop().unwrap().into_data().unwrap_string();
//   assert_eq!(errs.len(), 1, "Expected 1 error, got {errs:?}");
//   let err = errs[0].unwrap_unicode_ref().unwrap_invalid_surrogate_ref();
//   assert_eq!(err.span(), Span::from(21..27));

//   let mut lexer = Token::lexer(r#""Lone trail surrogate \uDD80""#);
//   let mut errs = lexer.next().unwrap().unwrap_err();
//   assert_eq!(errs.len(), 1);
//   let errs = errs.pop().unwrap().into_data().unwrap_string();
//   assert_eq!(errs.len(), 1, "Expected 1 error, got {errs:?}");
//   let err = errs[0].unwrap_unicode_ref().unwrap_invalid_surrogate_ref();
//   assert_eq!(err.span(), Span::from(22..28));
// }

// #[test]
// fn test_string_lexing() {
//   let input = r#"
//            "test"
//            "escaped \" quote"
//            "unterminated
//            "
//        "#;
//   let mut lexer = Token::lexer(input);

//   assert_eq!(lexer.next(), Some(Ok(Token::StringLiteral("\"test\""))));
//   assert_eq!(lexer.slice(), "\"test\"");

//   assert_eq!(
//     lexer.next(),
//     Some(Ok(Token::StringLiteral(r#""escaped \" quote""#)))
//   );
//   assert_eq!(lexer.slice(), r#""escaped \" quote""#);

//   // assert_eq!(lexer.next(), Some(Err(())));
//   // assert_eq!(
//   //   lexer.extras.error_token,
//   //   Some(Token::ErrorUnterminatedString)
//   // );
//   assert_eq!(lexer.slice(), "\"unterminated");
// }

// #[test]
// fn test_invalid_character_lexing() {
//   let input = r#"
//            {
//                %%%
//                __typename
//                *
//            }
//        "#;
//   let mut lexer = Token::lexer(input);

//   assert_eq!(lexer.next(), Some(Ok(Token::BraceOpen)));
//   assert_eq!(lexer.slice(), "{");

//   assert_eq!(lexer.next(), Some(Err(())));
//   assert_eq!(lexer.slice(), "%");

//   assert_eq!(lexer.next(), Some(Err(())));
//   assert_eq!(lexer.slice(), "%");

//   assert_eq!(lexer.next(), Some(Err(())));
//   assert_eq!(lexer.slice(), "%");

//   assert_eq!(lexer.next(), Some(Ok(Token::Identifier("__typename"))));
//   assert_eq!(lexer.slice(), "__typename");

//   assert_eq!(lexer.next(), Some(Err(())));
//   assert_eq!(lexer.slice(), "*");

//   assert_eq!(lexer.next(), Some(Ok(Token::BraceClose)));
//   assert_eq!(lexer.slice(), "}");

//   assert_eq!(lexer.next(), None);
// }

// #[test]
// fn test_block_string_lexing() {
//   let input = r#"
//            # escaped
//            """tes\"""t"""
//            # empty
//            """"""
//            # 2 quotes in a string
//            """"" """
//            """
//                multi-
//                line
//            """
//            """unterminated
//        "#;
//   let mut lexer = Token::lexer(input);

//   assert_eq!(
//     lexer.next(),
//     Some(Ok(Token::BlockStringLiteral(r#""""tes\"""t""""#)))
//   );
//   assert_eq!(lexer.slice(), r#""""tes\"""t""""#);

//   assert_eq!(
//     lexer.next(),
//     Some(Ok(Token::BlockStringLiteral(r#""""""""#)))
//   );
//   assert_eq!(lexer.slice(), r#""""""""#);

//   assert_eq!(
//     lexer.next(),
//     Some(Ok(Token::BlockStringLiteral(r#"""""" """"#)))
//   );
//   assert_eq!(lexer.slice(), r#"""""" """"#);

//   assert_eq!(
//     lexer.next(),
//     Some(Ok(Token::BlockStringLiteral(
//       r#""""
//                multi-
//                line
//            """"#
//     )))
//   );
//   assert_eq!(
//     lexer.slice(),
//     r#""""
//                multi-
//                line
//            """"#
//   );

//   // assert_eq!(lexer.next(), Some(Err(())));
//   // assert_eq!(
//   //   lexer.extras.error_token,
//   //   Some(Token::ErrorUnterminatedBlockString)
//   // );
//   // Unterminated string just consumes the starting quotes
//   assert_eq!(lexer.slice(), r#"""""#);
// }

#[test]
fn test_bom_lexing() {
  let input = "\u{feff}";

  let mut lexer = Token::lexer(input);

  assert_eq!(lexer.next(), None);
}
