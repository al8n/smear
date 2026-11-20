//! The `const { … }` block's lexing, in both lexers: the states, the word step by step, the
//! ceiling a shift cannot lower, a lexer resumed inside a block, the prefixed string's literal and
//! its door.

use std::{borrow::Cow, string::String, vec::Vec};

use tokora::{Lexer as _, SimpleSpan};

use super::{
  LitPrefixedStr, StrPrefix,
  lossless::{LosslessLexer, LosslessState, LosslessToken},
  syntactic::{SyntacticLexer, SyntacticState, SyntacticToken},
};
use crate::{
  LitInlineStr, LitStr, decode_string_value,
  error::StringValueError,
  limits::{LosslessLimits, SyntacticLimits},
};

/// A syntactic state inside a block, with no `<` open.
fn entered() -> SyntacticState {
  let mut state = SyntacticState::default();
  state.enter_const_block();
  state
}

/// A lossless state inside a block, with no `<` open.
fn lossless_entered() -> LosslessState {
  let mut state = LosslessState::default();
  state.enter_const_block();
  state
}

/// `new` and `from` agree and start outside a block, `default()` is `new` of the limits' default,
/// and entering and leaving move the word alone: the limits stay as they were.
#[test]
fn the_states_enter_and_leave_a_block() {
  let limits = SyntacticLimits::with_max_nesting_depth(7);
  let state = SyntacticState::new(limits);
  assert_eq!(state, SyntacticState::from(limits));
  assert_eq!(
    SyntacticState::default(),
    SyntacticState::new(SyntacticLimits::default())
  );
  assert!(!state.in_const_block() && state.open_angles() == 0);
  let mut moved = state;
  moved.enter_const_block();
  assert!(moved.in_const_block() && moved.open_angles() == 0);
  assert_eq!(moved.limits(), &limits);
  moved.leave_const_block();
  assert_eq!(moved, state);
  moved.enter_const_block();
  assert!(moved.in_const_block());

  let limits = LosslessLimits::default().with_max_tokens(9);
  let state = LosslessState::new(limits);
  assert_eq!(state, LosslessState::from(limits));
  assert_eq!(
    LosslessState::default(),
    LosslessState::new(LosslessLimits::default())
  );
  assert!(!state.in_const_block() && state.open_angles() == 0);
  let mut moved = state;
  moved.enter_const_block();
  assert!(moved.in_const_block() && moved.open_angles() == 0);
  assert_eq!(moved.limits(), &limits);
  moved.leave_const_block();
  assert_eq!(moved, state);
  moved.enter_const_block();
  assert!(moved.in_const_block());
}

/// A brace clears the count of open `<` inside a block and leaves the block on; outside one it
/// leaves the word zero, so no brace enters a block.
#[test]
fn a_brace_clears_the_count_and_enters_nothing() {
  for src in ["< {", "< }"] {
    let mut lexer = SyntacticLexer::<str>::with_state(src, entered());
    assert!(lexer.lex().is_some_and(|item| item.is_ok()), "{src:?}");
    assert_eq!(lexer.state().open_angles(), 1, "{src:?}: `<` is open");
    assert!(lexer.lex().is_some_and(|item| item.is_ok()), "{src:?}");
    assert!(
      lexer.state().in_const_block(),
      "{src:?}: still in the block"
    );
    assert_eq!(
      lexer.state().open_angles(),
      0,
      "{src:?}: the brace cleared the count"
    );

    let mut lexer = LosslessLexer::<&str>::with_state(src, lossless_entered());
    let mut counts = Vec::new();
    while lexer.lex().is_some() {
      counts.push((lexer.state().in_const_block(), lexer.state().open_angles()));
    }
    assert_eq!(
      counts,
      [(true, 1), (true, 1), (true, 0)],
      "{src:?}, lossless"
    );
  }
  for src in ["{", "}", "< {"] {
    let mut lexer = SyntacticLexer::<str>::new(src);
    while lexer.lex().is_some() {
      assert!(!lexer.state().in_const_block(), "{src:?}");
    }
    let mut lexer = LosslessLexer::<&str>::new(src);
    while lexer.lex().is_some() {
      assert!(!lexer.state().in_const_block(), "{src:?}, lossless");
    }
  }
}

/// The count of open `<` and the bracket tally after each token, from a state inside a block: a
/// `<` raises both, a `>` while one is open lowers both, `>>` with none open is `Shr` and moves
/// neither, a lone `>` with none open is the stray closer that lowers the tally, and a brace clears
/// the count. In `[Box<u8, 8 >> 1]` the first `>` closes the stray `<` and the second is a stray
/// closer.
#[test]
fn the_word_step_by_step() {
  let cases: [(&str, &[(usize, usize)]); 4] = [
    ("1 << 4 >> 5", &[(0, 0); 5]),
    (
      "Wrap<Box<u8>>(1) >> 1",
      &[
        (0, 0),
        (1, 1),
        (1, 1),
        (2, 2),
        (2, 2),
        (1, 1),
        (0, 0),
        (0, 1),
        (0, 1),
        (0, 0),
        (0, 0),
        (0, 0),
      ],
    ),
    (
      "{ a: Box<u8 } >> 1",
      &[
        (0, 1),
        (0, 1),
        (0, 1),
        (0, 1),
        (1, 2),
        (1, 2),
        (0, 1),
        (0, 1),
        (0, 1),
      ],
    ),
    (
      "[Box<u8, 8 >> 1]",
      &[
        (0, 1),
        (0, 1),
        (1, 2),
        (1, 2),
        (1, 2),
        (0, 1),
        (0, 0),
        (0, 0),
        (0, 0),
      ],
    ),
  ];
  for (src, expected) in cases {
    let mut lexer = SyntacticLexer::<str>::with_state(src, entered());
    let mut steps: Vec<(usize, usize)> = Vec::new();
    while let Some(item) = lexer.lex() {
      assert!(item.is_ok(), "{src:?}: {item:?}");
      steps.push((lexer.state().open_angles(), lexer.state().limits().depth()));
    }
    assert_eq!(steps, expected, "{src:?}");

    let mut lexer = LosslessLexer::<&str>::with_state(src, lossless_entered());
    let mut steps: Vec<(usize, usize)> = Vec::new();
    while let Some(item) = lexer.lex() {
      let token = item.unwrap_or_else(|e| panic!("{src:?}: {e:?}"));
      if !token.is_trivia() {
        steps.push((lexer.state().open_angles(), lexer.state().depth()));
      }
    }
    assert_eq!(steps, expected, "{src:?}, lossless");
  }
}

/// `(1 >> (1 >> (1 >> (…` is refused at the ceiling inside a block, in both lexers: no shift lowers
/// the tally, so every `(` stays counted.
#[test]
fn a_shift_lowers_no_tally_and_the_ceiling_holds() {
  let ceiling = SyntacticLimits::default().max_nesting_depth();
  let src = "(1 >> ".repeat(ceiling + 1);

  let mut lexer = SyntacticLexer::<str>::with_state(src.as_str(), entered());
  let mut accepted = 0usize;
  while let Some(Ok(_)) = lexer.lex() {
    accepted += 1;
  }
  assert_eq!(accepted, 3 * ceiling, "the `(` past the ceiling is refused");

  let ceiling = LosslessState::default().max_nesting_depth();
  let mut lexer = LosslessLexer::<&str>::with_state(src.as_str(), lossless_entered());
  let mut accepted = 0usize;
  while let Some(Ok(token)) = lexer.lex() {
    if !token.is_trivia() {
      accepted += 1;
    }
  }
  assert_eq!(
    accepted,
    3 * ceiling,
    "the `(` past the ceiling is refused, lossless"
  );
}

/// Runs `$lexer` over `$src` from `$state`, keeping the state and the offset after each item, and
/// then resumes a lexer from each: the rest of the stream, with its states, is the first run's.
macro_rules! assert_resumes_alike {
  ($lexer:ty, $src:expr, $state:expr, $what:expr) => {{
    let src = $src;
    let mut lexer = <$lexer>::with_state(src, $state);
    let mut run = Vec::new();
    let mut stops = std::vec![(0usize, *lexer.state())];
    while let Some(item) = lexer.lex() {
      run.push((item, lexer.span(), *lexer.state()));
      stops.push((lexer.span().end(), *lexer.state()));
    }
    assert!(
      run.len() > 10,
      "{}: the stream is too short to show anything",
      $what
    );
    for (k, (offset, state)) in stops.into_iter().enumerate() {
      let mut resumed = <$lexer>::with_state(src, state);
      resumed.bump(&offset);
      let mut rest = Vec::new();
      while let Some(item) = resumed.lex() {
        rest.push((item, resumed.span(), *resumed.state()));
      }
      assert_eq!(
        rest.as_slice(),
        run.get(k..).unwrap_or_default(),
        "{}: resumed after item {k}",
        $what
      );
    }
  }};
}

/// A lexer resumed inside a block from a state and an offset answers what the lexer that ran
/// through did, in both lexers: the mode and the count live in the state, and nowhere else.
#[test]
fn a_lexer_resumed_inside_a_block_answers_as_the_one_that_ran_through() {
  const SRC: &str = "Pair<A, Box<u8>>(1) >> 2, 3 << 4, b\"x\"";
  assert_resumes_alike!(SyntacticLexer<'_, str>, SRC, entered(), "syntactic");
  assert_resumes_alike!(LosslessLexer<'_, &str>, SRC, lossless_entered(), "lossless");
}

/// The literal a prefixed string read with each lexer's own reader, the first time and again.
fn syntactic_literal(src: &str) -> LitStr<&str> {
  match SyntacticLexer::<str>::with_state(src, entered()).lex() {
    Some(Ok(SyntacticToken::LitPrefixedStr(lit))) => lit.literal(),
    other => panic!("{src:?} is no prefixed string: {other:?}"),
  }
}

/// `literal()` answers what the string rules answer: for each prefixed string, from both lexers
/// and over text and bytes, the literal the same lexer makes outside a block of the same string
/// without its letter, fact for fact.
#[test]
fn a_prefixed_strings_literal_is_the_string_its_letter_touches() {
  for (prefixed, plain) in [
    ("b\"ab\"", "\"ab\""),
    ("i\"7\"", "\"7\""),
    ("b\"\"\"x\"\"\"", "\"\"\"x\"\"\""),
    ("i\"\"\"x\"\"\"", "\"\"\"x\"\"\""),
    ("b\"é\"", "\"é\""),
  ] {
    let expected: LitStr<&str> = match SyntacticLexer::<str>::new(plain).lex() {
      Some(Ok(SyntacticToken::LitInlineStr(lit))) => LitStr::Inline(lit),
      Some(Ok(SyntacticToken::LitBlockStr(lit))) => LitStr::Block(lit),
      other => panic!("{plain:?}: {other:?}"),
    };
    assert_eq!(syntactic_literal(prefixed), expected, "{prefixed:?}");

    let expected_bytes: LitStr<&[u8]> = match SyntacticLexer::<[u8]>::new(plain.as_bytes()).lex() {
      Some(Ok(SyntacticToken::LitInlineStr(lit))) => LitStr::Inline(lit),
      Some(Ok(SyntacticToken::LitBlockStr(lit))) => LitStr::Block(lit),
      other => panic!("{plain:?}: {other:?}"),
    };
    match SyntacticLexer::<[u8]>::with_state(prefixed.as_bytes(), entered()).lex() {
      Some(Ok(SyntacticToken::LitPrefixedStr(lit))) => {
        assert_eq!(lit.literal(), expected_bytes, "{prefixed:?} as bytes");
      }
      other => panic!("{prefixed:?} as bytes: {other:?}"),
    }

    match LosslessLexer::<&str>::with_state(prefixed, lossless_entered()).lex() {
      Some(Ok(LosslessToken::LitPrefixedStr(lit))) => {
        assert_eq!(lit.literal(), expected, "{prefixed:?}, lossless");
      }
      other => panic!("{prefixed:?}, lossless: {other:?}"),
    }
  }

  // No twin outside a block: one byte for each of `\x89`, `\r` and `\n`, and three for `PNG`.
  match syntactic_literal("b\"\\x89PNG\\r\\n\"") {
    LitStr::Inline(LitInlineStr::Complex(lit)) => assert_eq!(lit.required_capacity(), 6),
    other => panic!("{other:?}"),
  }
}

/// What the text doors answer for a literal whose body holds `\x41`: the decoder refuses the
/// backslash at its offset in the spelling, and the `Cow` conversion copies it through. The door
/// that answers bytes is not this part's.
#[test]
fn the_text_doors_read_a_byte_escape_as_text() {
  let LitStr::Inline(lit) = syntactic_literal("b\"\\x41\"") else {
    panic!("an inline string");
  };
  assert_eq!(
    decode_string_value(lit.as_str().as_bytes(), &mut String::new()),
    Err(StringValueError::InvalidEscape(1))
  );
  assert_eq!(Cow::<str>::from(lit), Cow::<str>::Borrowed("\\x41"));
  assert!(matches!(lit, LitInlineStr::Complex(_)));
}

/// The prefixed string's door reads one whole literal from a state inside a block, and refuses a
/// prefix of one, trivia around it, a reserved prefix and a token of another kind.
#[test]
fn the_prefixed_string_door_reads_one_whole_literal() {
  for (text, prefix) in [
    ("b\"ab\"", StrPrefix::Bytes),
    ("i\"7\"", StrPrefix::Id),
    ("b\"\"\"x\"\"\"", StrPrefix::Bytes),
  ] {
    let lit = LitPrefixedStr::try_from(text).unwrap_or_else(|e| panic!("{text:?}: {e:?}"));
    assert_eq!(lit.prefix(), prefix, "{text:?}");
  }
  for text in [
    "b\"ab", " b\"ab\"", "b\"ab\" ", "\"ab\"", "b", "r\"x\"", "b'a'", "1",
  ] {
    assert!(
      LitPrefixedStr::try_from(text).is_err(),
      "{text:?} is not one whole prefixed string"
    );
  }
  assert_eq!(
    SimpleSpan::new(0, 5),
    {
      let mut lexer = SyntacticLexer::<str>::with_state("b\"ab\"", entered());
      let _ = lexer.lex();
      lexer.span()
    },
    "the token begins at the letter"
  );
}

/// A number the number grammar refuses leaves the block and its count as they were, in both
/// lexers: that grammar is lent the limits alone, so nothing it answers can write the word.
#[test]
fn a_refused_number_leaves_the_word_as_it_was() {
  const SRC: &str = "< 0x 1u08 .5 1";
  let expected = [
    (true, true, 1),
    (false, true, 1),
    (false, true, 1),
    (false, true, 1),
    (true, true, 1),
  ];

  let mut lexer = SyntacticLexer::<str>::with_state(SRC, entered());
  let mut seen: Vec<(bool, bool, usize)> = Vec::new();
  while let Some(item) = lexer.lex() {
    seen.push((
      item.is_ok(),
      lexer.state().in_const_block(),
      lexer.state().open_angles(),
    ));
  }
  assert_eq!(seen, expected);

  let mut lexer = LosslessLexer::<&str>::with_state(SRC, lossless_entered());
  let mut seen: Vec<(bool, bool, usize)> = Vec::new();
  while let Some(item) = lexer.lex() {
    if item
      .as_ref()
      .is_ok_and(|token: &LosslessToken<&str>| token.is_trivia())
    {
      continue;
    }
    seen.push((
      item.is_ok(),
      lexer.state().in_const_block(),
      lexer.state().open_angles(),
    ));
  }
  assert_eq!(seen, expected, "lossless");
}

/// Each item `lexer` answers, as its kind's name or its errors' `Debug`, with its span.
macro_rules! items {
  ($lexer:expr) => {{
    let mut lexer = $lexer;
    let mut items: Vec<(String, SimpleSpan)> = Vec::new();
    while let Some(item) = lexer.lex() {
      let what = match item {
        Ok(token) => std::format!("{:?}", token.kind()),
        Err(errors) => std::format!("{errors:?}"),
      };
      items.push((what, lexer.span()));
    }
    items
  }};
}

/// Inside a block a `-` before a fraction is the operator, and the fraction after it is lexed
/// alone, by the number grammars' longest match in both lexers: `-._5,` is `Minus`, one refusal
/// over `._5`, and `Separator`, and a hexadecimal fraction with its exponent is one number too.
#[test]
fn a_fraction_after_a_blocks_minus_is_one_number_in_both_lexers() {
  for src in [
    "-._5,",
    "-._5e3,",
    "-.5,",
    "-.ap1,",
    "-._a_p-1_,",
    "-._,",
    "-.a,",
    "-.ap,",
  ] {
    // No source here holds trivia, so the two streams compare item for item.
    let syntactic = items!(SyntacticLexer::<str>::with_state(src, entered()));
    let lossless = items!(LosslessLexer::<&str>::with_state(src, lossless_entered()));
    assert_eq!(syntactic, lossless, "{src:?}");
  }

  let missing = "LexerErrors([LexerError { span: SimpleSpan { start: 1, end: 4 }, data: \
                 Float(MissingIntegerPart(SimpleSpan { start: 1, end: 4 })) }])";
  assert_eq!(
    items!(SyntacticLexer::<str>::with_state("-._5,", entered())),
    [
      (String::from("Minus"), SimpleSpan::new(0, 1)),
      (String::from(missing), SimpleSpan::new(1, 4)),
      (String::from("Separator"), SimpleSpan::new(4, 5)),
    ]
  );
}
