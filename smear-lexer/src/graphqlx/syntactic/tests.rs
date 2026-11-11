use std::{string::ToString, vec, vec::Vec};

use tokora::{Lexer as _, SimpleSpan, state::RecursionLimitExceeded, utils::Lexeme};

use crate::{
  SyntaxErrorKind,
  error::{StringError, StringErrors, UnicodeError},
  graphqlx::{
    LitFloat, LitInt,
    error::{LexerErrorData, LexerErrors},
    syntactic::{SyntacticLexer, SyntacticToken},
  },
};

/// Drive a `<str>`-sourced lexer to completion, unwrapping every result.
fn lex_all(src: &str) -> Vec<SyntacticToken<&str>> {
  let mut lexer = SyntacticLexer::<str>::new(src);
  let mut out = Vec::new();
  while let Some(tok) = lexer.lex() {
    out.push(tok.unwrap());
  }
  out
}

/// Lexes the refusal a document opens with, and answers its batch, the span of its error token and
/// the kind of the token after it.
macro_rules! refusal {
  ($lexer:expr, $src:expr) => {{
    let mut lexer = $lexer;
    let src = $src;
    let errors = match lexer.lex() {
      Some(Err(errors)) => errors,
      other => panic!("{src:?}: expected a refusal, got {other:?}"),
    };
    let span = lexer.error_span();
    let next = lexer
      .lex()
      .map(|token| token.expect("the input after the refusal lexes").kind());
    (errors, span, next)
  }};
}

/// The string errors a refused literal's batch holds, which come as its one `String` lexer error.
fn string_errors<Char: Clone + core::fmt::Debug>(
  errors: &LexerErrors<Char, RecursionLimitExceeded>,
) -> StringErrors<Char> {
  let [error] = errors.as_slice() else {
    panic!("expected one lexer error, got {errors:?}");
  };
  let LexerErrorData::String(errs) = error.data() else {
    panic!("expected a string error, got {error:?}");
  };
  errs.clone()
}

#[test]
fn path_separator_vs_colon() {
  assert_eq!(
    lex_all("a::b"),
    vec![
      SyntacticToken::Identifier("a"),
      SyntacticToken::PathSeparator,
      SyntacticToken::Identifier("b"),
    ]
  );
  assert_eq!(
    lex_all("a: b"),
    vec![
      SyntacticToken::Identifier("a"),
      SyntacticToken::Colon,
      SyntacticToken::Identifier("b"),
    ]
  );
  // `:::` splits as `::` (PathSeparator) then a lone `:` (Colon).
  assert_eq!(
    lex_all(":::"),
    vec![SyntacticToken::PathSeparator, SyntacticToken::Colon]
  );
}

#[test]
fn fat_arrow_vs_equal() {
  assert_eq!(
    lex_all("<K => V>"),
    vec![
      SyntacticToken::LAngle,
      SyntacticToken::Identifier("K"),
      SyntacticToken::FatArrow,
      SyntacticToken::Identifier("V"),
      SyntacticToken::RAngle,
    ]
  );
  assert_eq!(lex_all("x = 1")[1], SyntacticToken::Equal);
  // `==` is two separate `Equal` tokens, never a fat arrow.
  assert_eq!(
    lex_all("=="),
    vec![SyntacticToken::Equal, SyntacticToken::Equal]
  );
}

#[test]
fn angle_brackets_carry_generics() {
  assert_eq!(
    lex_all("Box<T>"),
    vec![
      SyntacticToken::Identifier("Box"),
      SyntacticToken::LAngle,
      SyntacticToken::Identifier("T"),
      SyntacticToken::RAngle,
    ]
  );
}

#[test]
fn plus_is_always_an_operator() {
  // `+` is never a number sign: `+5` is Plus then Int(5).
  assert_eq!(
    lex_all("+5"),
    vec![
      SyntacticToken::Plus,
      SyntacticToken::LitInt(LitInt::Decimal("5")),
    ]
  );
}

#[test]
fn minus_before_digit_is_a_negative_literal() {
  assert_eq!(
    lex_all("-5"),
    vec![SyntacticToken::LitInt(LitInt::Decimal("-5"))]
  );
  assert_eq!(
    lex_all("-0xFF"),
    vec![SyntacticToken::LitInt(LitInt::Hex("-0xFF"))]
  );
  assert_eq!(
    lex_all("-2.5"),
    vec![SyntacticToken::LitFloat(LitFloat::Decimal("-2.5"))]
  );
}

#[test]
fn minus_before_non_digit_is_the_operator() {
  assert_eq!(
    lex_all("- 4"),
    vec![
      SyntacticToken::Minus,
      SyntacticToken::LitInt(LitInt::Decimal("4")),
    ]
  );
  assert_eq!(
    lex_all("-x"),
    vec![SyntacticToken::Minus, SyntacticToken::Identifier("x")]
  );
  assert_eq!(lex_all("-"), vec![SyntacticToken::Minus]);
}

#[test]
fn radix_numbers_delegate_to_logos() {
  assert_eq!(
    lex_all("0xFF"),
    vec![SyntacticToken::LitInt(LitInt::Hex("0xFF"))]
  );
  assert_eq!(
    lex_all("0b1010"),
    vec![SyntacticToken::LitInt(LitInt::Binary("0b1010"))]
  );
  assert_eq!(
    lex_all("0o755"),
    vec![SyntacticToken::LitInt(LitInt::Octal("0o755"))]
  );
  assert_eq!(
    lex_all("0x1.8p3"),
    vec![SyntacticToken::LitFloat(LitFloat::Hex("0x1.8p3"))]
  );
}

/// A radix prefix with no digit after it refuses as an incomplete number wherever it stands: one
/// error at the prefix's own span, with the delimiter after it lexed as a token of its own. Before
/// a delimiter, the prefix used to yield an error token whose batch was empty.
#[test]
fn a_bare_radix_prefix_before_a_delimiter_is_an_incomplete_number() {
  use crate::graphqlx::syntactic::SyntacticTokenKind;

  macro_rules! assert_no_digit {
    ($lexer:expr, $src:expr, $message:expr, $after:expr) => {{
      let (errors, span, next) = refusal!($lexer, $src);
      let src = $src;
      assert_eq!(errors.len(), 1, "{src:?}: {errors:?}");
      assert_eq!(
        errors[0].kind(),
        SyntaxErrorKind::IncompleteNumber,
        "{src:?}"
      );
      assert_eq!(errors[0].to_string(), $message, "{src:?}");
      assert_eq!(errors[0].span(), SimpleSpan::new(0, 2), "{src:?}");
      assert_eq!(span, Some(SimpleSpan::new(0, 2)), "{src:?}");
      assert_eq!(next, $after, "{src:?}");
    }};
  }

  for (src, message, after) in [
    (
      "0x)",
      "incomplete number, expected a hex digit",
      Some(SyntacticTokenKind::RParen),
    ),
    (
      "0o )",
      "incomplete number, expected an octal digit",
      Some(SyntacticTokenKind::RParen),
    ),
    ("0b,", "incomplete number, expected a binary digit", None),
  ] {
    assert_no_digit!(SyntacticLexer::<str>::new(src), src, message, after);
    assert_no_digit!(
      SyntacticLexer::<[u8]>::new(src.as_bytes()),
      src,
      message,
      after
    );
  }
}

#[test]
fn asterisk_and_spread() {
  assert_eq!(lex_all("*"), vec![SyntacticToken::Asterisk]);
  assert_eq!(
    lex_all("...x"),
    vec![SyntacticToken::Spread, SyntacticToken::Identifier("x")]
  );
}

#[test]
fn dot_forms_take_the_longest_token() {
  // `..` is the parent-module sigil, taken by longest match after `...`: `..5` is `..` then `5`,
  // `.....` is `...` then `..`, and `....` is `...` then a stray `.`.
  assert_eq!(
    lex_all("..5"),
    vec![
      SyntacticToken::DotDot,
      SyntacticToken::LitInt(LitInt::Decimal("5")),
    ]
  );
  assert_eq!(
    lex_all("....."),
    vec![SyntacticToken::Spread, SyntacticToken::DotDot]
  );
  let mut lexer = SyntacticLexer::<str>::new("....");
  assert_eq!(lexer.lex().unwrap(), Ok(SyntacticToken::Spread));
  assert!(lexer.lex().unwrap().is_err());
  assert_eq!(lexer.error_span(), Some(tokora::SimpleSpan::new(3, 4)));

  // A lone `.` is still an unterminated-spread error and `.5` a missing-integer-part float. Both
  // are delegated, so the fast path never mis-reports one as another.
  let mut lexer = SyntacticLexer::<str>::new(".x");
  assert!(lexer.lex().unwrap().is_err());
  assert_eq!(lexer.error_span(), Some(tokora::SimpleSpan::new(0, 1)));
  assert_eq!(lexer.lex().unwrap(), Ok(SyntacticToken::Identifier("x")));

  let mut lexer = SyntacticLexer::<str>::new(".5");
  assert!(lexer.lex().unwrap().is_err());
  assert_eq!(lexer.error_span(), Some(tokora::SimpleSpan::new(0, 2)));

  // A number is lexed first: `1..` is the invalid float `1.`, whose error takes the `.` after it
  // as an invalid suffix, so no `..` is left over.
  let mut lexer = SyntacticLexer::<str>::new("1..");
  assert!(lexer.lex().unwrap().is_err());
  assert_eq!(lexer.error_span(), Some(tokora::SimpleSpan::new(0, 3)));
  assert!(lexer.lex().is_none());
}

#[test]
fn the_root_sigils_are_single_tokens_that_end_a_number() {
  assert_eq!(
    lex_all("~::a ^::b ..::c"),
    vec![
      SyntacticToken::Tilde,
      SyntacticToken::PathSeparator,
      SyntacticToken::Identifier("a"),
      SyntacticToken::Caret,
      SyntacticToken::PathSeparator,
      SyntacticToken::Identifier("b"),
      SyntacticToken::DotDot,
      SyntacticToken::PathSeparator,
      SyntacticToken::Identifier("c"),
    ]
  );
  assert_eq!(
    lex_all("1~2^"),
    vec![
      SyntacticToken::LitInt(LitInt::Decimal("1")),
      SyntacticToken::Tilde,
      SyntacticToken::LitInt(LitInt::Decimal("2")),
      SyntacticToken::Caret,
    ]
  );
}

#[test]
fn inline_string_fast_path() {
  let toks = lex_all("\"hello\"");
  assert_eq!(toks.len(), 1);
  assert!(matches!(&toks[0], SyntacticToken::LitInlineStr(_)));

  // An escaped inline string still fast-paths (its normalization metadata is
  // derived by the shared `skip_inline_str_simd`).
  let toks = lex_all("\"a\\nb\"");
  assert_eq!(toks.len(), 1);
  assert!(matches!(&toks[0], SyntacticToken::LitInlineStr(_)));
}

/// A braced escape ends at the first `}`, `"`, `\` or line break after `\u{`.
///
/// A `}` closes it, so content that is not one to six hex digits is an invalid sequence through the
/// brace, from a text source as from a byte source. Any other stop leaves the escape unclosed over
/// what its rule matched, and is lexed by its own rule: the literal still ends at its own quote, a
/// line break in it is still refused, and a `}` past the quote is a token of its own.
#[test]
fn a_braced_escape_ends_at_its_brace_or_at_the_first_delimiter() {
  use crate::graphqlx::syntactic::SyntacticTokenKind;

  fn unclosed<Char>(start: usize, end: usize) -> StringError<Char> {
    StringError::Unicode(UnicodeError::unclosed_braced_unicode_escape(
      SimpleSpan::new(start, end),
    ))
  }

  fn invalid<Char>(start: usize, end: usize) -> StringError<Char> {
    StringError::Unicode(UnicodeError::invalid_braced_unicode_escape_sequence(
      SimpleSpan::new(start, end),
    ))
  }

  // Each text literal is the whole input, so it ends at its own quote where the input does.
  let text: [(&str, Vec<StringError<char>>); 6] = [
    (r#""\u{G}""#, vec![invalid(1, 6)]),
    (r#""\u{12G4}""#, vec![invalid(1, 9)]),
    (r#""\u{G""#, vec![unclosed(1, 5)]),
    (
      "\"\\u{G\n}\"",
      vec![unclosed(1, 5), StringError::unexpected_new_line('\n', 5)],
    ),
    (r#""\u{""#, vec![unclosed(1, 4)]),
    (
      "\"\\u{\n}\"",
      vec![unclosed(1, 4), StringError::unexpected_new_line('\n', 4)],
    ),
  ];
  for (src, want) in text {
    let (errors, span, next) = refusal!(SyntacticLexer::<str>::new(src), src);
    assert_eq!(
      string_errors(&errors).as_slice(),
      want.as_slice(),
      "{src:?}"
    );
    assert_eq!(span, Some(SimpleSpan::new(0, src.len())), "{src:?}");
    assert_eq!(next, None, "{src:?}");
  }

  let bytes: [(&[u8], Vec<StringError<u8>>); 2] = [
    (b"\"\\u{\"", vec![unclosed(1, 4)]),
    (b"\"\\u{\xff}\"", vec![invalid(1, 6)]),
  ];
  for (doc, want) in bytes {
    let label = doc.escape_ascii().to_string();
    let (errors, span, next) = refusal!(SyntacticLexer::<[u8]>::new(doc), &label);
    assert_eq!(
      string_errors(&errors).as_slice(),
      want.as_slice(),
      "{label}"
    );
    assert_eq!(span, Some(SimpleSpan::new(0, doc.len())), "{label}");
    assert_eq!(next, None, "{label}");
  }

  // A byte that begins no UTF-8 scalar is refused on its own, after the escape it ends, and the `}`
  // past the closing quote is a token of its own.
  let doc: &[u8] = b"\"\\u{\xff\" }";
  let (errors, span, next) = refusal!(
    SyntacticLexer::<[u8]>::new(doc),
    doc.escape_ascii().to_string()
  );
  let want: [StringError<u8>; 2] = [
    unclosed(1, 4),
    StringError::UnsupportedCharacter(Lexeme::Range(SimpleSpan::new(4, 5))),
  ];
  assert_eq!(string_errors(&errors).as_slice(), want.as_slice());
  assert_eq!(span, Some(SimpleSpan::new(0, 6)));
  assert_eq!(next, Some(SyntacticTokenKind::RBrace));
}

#[test]
fn block_string_delegates() {
  let toks = lex_all("\"\"\"a block\"\"\"");
  assert_eq!(toks.len(), 1);
  assert!(matches!(&toks[0], SyntacticToken::LitBlockStr(_)));
}

#[test]
fn byte_source_matches_str_shapes() {
  // The `<[u8]>` source drives the same dispatch; spot-check a generic path
  // token and a path separator survive the byte flavor.
  let mut lexer = SyntacticLexer::<[u8]>::new(b"a::B<C>");
  let mut kinds = Vec::new();
  while let Some(tok) = lexer.lex() {
    kinds.push(tok.unwrap().kind());
  }
  use crate::graphqlx::syntactic::SyntacticTokenKind as K;
  assert_eq!(
    kinds,
    vec![
      K::Identifier,
      K::PathSeparator,
      K::Identifier,
      K::LAngle,
      K::Identifier,
      K::RAngle,
    ]
  );
}

#[test]
fn identifier_capability_classifies_tokens() {
  use tokora::token::IdentifierToken;
  // UFCS on `IdentifierToken`: the `IsVariant` derive also generates an inherent
  // `is_identifier`, so method-call syntax would resolve to that instead of the trait.
  assert!(IdentifierToken::is_identifier(
    &SyntacticToken::<&str>::Identifier("x")
  ));
  assert!(!IdentifierToken::is_identifier(
    &SyntacticToken::<&str>::LBrace
  ));
}

#[test]
fn keyword_literal_and_punctuator_capabilities_are_mapped() {
  use tokora::token::{KeywordToken, LitToken as _, PunctuatorToken};

  let true_token = SyntacticToken::<&str>::Identifier("true");
  assert_eq!(true_token.keyword(), Some("true"));
  assert!(true_token.is_true_literal());
  assert!(true_token.is_boolean_literal());
  assert!(SyntacticToken::<&str>::Identifier("false").is_false_literal());
  assert!(SyntacticToken::<&str>::Identifier("null").is_null_literal());

  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::open_angle(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::LAngle)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::close_angle(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::RAngle)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::open_brace(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::LBrace)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::close_brace(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::RBrace)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::open_paren(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::LParen)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::close_paren(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::RParen)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::open_bracket(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::LBracket)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::close_bracket(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::RBracket)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::at(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::At)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::fat_arrow(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::FatArrow)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::double_colon(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::PathSeparator)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::asterisk(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Asterisk)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::ampersand(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Ampersand)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::colon(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Colon)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::dollar(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Dollar)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::equal(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Equal)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::exclamation(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Bang)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::plus(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Plus)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::minus(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Minus)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::pipe(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Pipe)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::spread(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Spread)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::tilde(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Tilde)
  );
  assert_eq!(
    <SyntacticToken<&str> as PunctuatorToken<'_>>::caret(),
    Some(crate::graphqlx::syntactic::SyntacticTokenKind::Caret)
  );
}

/// Census: every declared `SyntacticTokenKind` variant must be producible by some
/// `SyntacticToken`, i.e. by some arm of `SyntacticToken::kind`.
///
/// Mirrors `lossless_token_kind_census` (`graphqlx/lossless/mod.rs`), added after the graphql
/// (non-x) dialect shipped a `LosslessTokenKind::Boolean` with no `LosslessToken` able to
/// produce it. `SyntacticTokenKind` has no such gap today — trivia kinds don't exist here at
/// all, since `SyntacticToken` skips trivia — so this is a regression guard, not a fix. The
/// `census!` macro shape (one variant list driving both an exhaustive `match` and the
/// round-trip assertions) means a variant added without a case here fails to compile, naming
/// the variant, instead of silently staying untested.
#[test]
fn syntactic_token_kind_census() {
  use crate::{
    LitBlockStr, LitInlineStr, LitPlainBlockStr, LitPlainInlineStr,
    graphqlx::syntactic::SyntacticTokenKind,
  };

  macro_rules! census {
    ($($variant:ident => $token:expr),+ $(,)?) => {
      fn sample(kind: SyntacticTokenKind) -> SyntacticToken<&'static str> {
        match kind {
          $(SyntacticTokenKind::$variant => $token,)+
        }
      }

      $(
        assert_eq!(
          sample(SyntacticTokenKind::$variant).kind(),
          SyntacticTokenKind::$variant,
          "SyntacticTokenKind::{} is declared but its sample token maps to a different kind",
          stringify!($variant),
        );
      )+
    };
  }

  census! {
    Identifier => SyntacticToken::Identifier("x"),
    Int => SyntacticToken::LitInt(LitInt::Decimal("1")),
    Float => SyntacticToken::LitFloat(LitFloat::Decimal("1.0")),
    InlineString => SyntacticToken::LitInlineStr(LitInlineStr::Plain(LitPlainInlineStr::new("\"s\""))),
    BlockString => SyntacticToken::LitBlockStr(LitBlockStr::Plain(LitPlainBlockStr::new("\"\"\"b\"\"\""))),
    Dollar => SyntacticToken::Dollar,
    FatArrow => SyntacticToken::FatArrow,
    LAngle => SyntacticToken::LAngle,
    RAngle => SyntacticToken::RAngle,
    LParen => SyntacticToken::LParen,
    RParen => SyntacticToken::RParen,
    Spread => SyntacticToken::Spread,
    Colon => SyntacticToken::Colon,
    Equal => SyntacticToken::Equal,
    Asterisk => SyntacticToken::Asterisk,
    At => SyntacticToken::At,
    LBracket => SyntacticToken::LBracket,
    RBracket => SyntacticToken::RBracket,
    LBrace => SyntacticToken::LBrace,
    RBrace => SyntacticToken::RBrace,
    Pipe => SyntacticToken::Pipe,
    Bang => SyntacticToken::Bang,
    Ampersand => SyntacticToken::Ampersand,
    Plus => SyntacticToken::Plus,
    Minus => SyntacticToken::Minus,
    PathSeparator => SyntacticToken::PathSeparator,
    Tilde => SyntacticToken::Tilde,
    DotDot => SyntacticToken::DotDot,
    Caret => SyntacticToken::Caret,
  }
}
