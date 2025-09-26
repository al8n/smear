use logosky::{
  Lexable,
  logos::{Lexer, Logos},
};

use crate::error::{LexerError, StringError, StringErrors};

use super::{super::SealedLexer, BlockString};

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(crate = logosky::logos, error(StringError<char>))]
enum BlockStringToken {
  /// \\"\\"\\" inside block string
  #[token("\\\"\"\"")]
  EscapedTripleQuote,
  /// terminator
  #[token("\"\"\"")]
  TripleQuote,
  /// Runs of any characters except the double quote **and backslash**;
  /// includes newlines and C0 controls.
  #[regex(r#"[^"\\]+"#)]
  Continue,
  /// A lone backslash (not followed by `"""`) is just content.
  #[token("\\")]
  Backslash,
  /// A single quote that is **not** part of `"""` (content)
  #[token("\"")]
  Quote,
}

impl<'a, T, StateError> Lexable<SealedLexer<'_, 'a, T>, LexerError<char, StateError>>
  for BlockString<&'a str>
where
  T: Logos<'a, Source = str>,
{
  #[inline]
  fn lex(mut lexer: SealedLexer<'_, 'a, T>) -> Result<Self, LexerError<char, StateError>>
  where
    Self: Sized,
  {
    let remainder = lexer.remainder();
    let lexer_span = lexer.span();
    let mut string_lexer = BlockStringToken::lexer(remainder);
    let mut errs = StringErrors::default();

    let mut has_escaped_triple_quote = false;
    while let Some(string_token) = string_lexer.next() {
      match string_token {
        Err(mut e) => {
          e.bump(lexer_span.end);
          errs.push(e);
        }
        Ok(BlockStringToken::EscapedTripleQuote) => {
          has_escaped_triple_quote = true;
        }
        Ok(BlockStringToken::Continue | BlockStringToken::Backslash | BlockStringToken::Quote) => {}
        Ok(BlockStringToken::TripleQuote) => {
          // Outer lexer consumes up to (and including) the closing """
          lexer.bump(string_lexer.span().end);

          // inner content (between opening and closing)
          let source = &remainder[..string_lexer.span().start];

          // sub-lex the inner content to compute normalization knobs (no allocations)
          let mut lines = BlockLineTok::lexer_with_extras(source, BlockLineExtras::default());
          while lines.next().is_some() {
            // callbacks already updated `lines.extras`
          }

          let total_lines = lines.extras.terminators + 1;

          // special case: all-blank block → trailing = leading (keeps invariants)
          if !lines.extras.saw_nonblank_any && !source.is_empty() {
            lines.extras.trailing_blank_lines = lines.extras.leading_blank_lines;
          }

          let has_cr_terminators = lines.extras.has_cr_terminators;
          let leading_blank_lines = lines.extras.leading_blank_lines;
          let trailing_blank_lines = lines.extras.trailing_blank_lines;
          let common_indent = lines.extras.common_indent.unwrap_or(0);

          let is_clean = !has_escaped_triple_quote
            && !has_cr_terminators
            && leading_blank_lines == 0
            && trailing_blank_lines == 0
            && common_indent == 0;

          if is_clean {
            return Ok(BlockString::Clean(source));
          }

          return Ok(BlockString::Mixed {
            source,
            has_escaped_triple_quote,
            has_cr_terminators: lines.extras.has_cr_terminators,
            leading_blank_lines: lines.extras.leading_blank_lines,
            trailing_blank_lines: lines.extras.trailing_blank_lines,
            common_indent: lines.extras.common_indent.unwrap_or(0),
            total_lines,
          });
        }
      }
    }

    lexer.bump(string_lexer.span().end);
    errs.push(StringError::unterminated_block_string());
    Err(LexerError::new(lexer.span(), errs.into()))
  }
}

// ---- extras that accumulate line-level facts during the sub-lex ----
#[derive(Default, Debug, Clone, Copy)]
struct BlockLineExtras {
  has_cr_terminators: bool,
  leading_blank_lines: usize,
  trailing_blank_lines: usize,
  common_indent: Option<usize>, // min indent across non-blank lines after the first
  saw_nonblank_any: bool,
  saw_body_this_line: bool, // whether we saw a LineBody since last Terminator
  terminators: usize,       // count of seen line terminators
}

#[inline(always)]
fn is_blank_line(s: &str) -> bool {
  s.bytes().all(|b| b == b' ' || b == b'\t')
}

#[inline(always)]
fn leading_ws_indent(s: &str) -> usize {
  s.bytes()
    .take_while(|&c| c == b' ' || c == b'\t')
    .count()
}

// ---- sub-lexer over inner block-string content ----
#[derive(Logos, Debug)]
#[logos(crate = logosky::logos, extras = BlockLineExtras)]
enum BlockLineTok {
  /// Body of a line (one or more chars, never includes a terminator).
  /// We process the whole line in the callback.
  #[regex(r#"[^\r\n]+"#, on_line_body)]
  LineBody,

  /// One line terminator: \r\n | \r | \n
  #[regex(r#"\r\n|\r|\n"#, on_terminator)]
  Terminator,
}

// callbacks mutate `extras` to record state
#[inline]
fn on_line_body(lex: &mut Lexer<'_, BlockLineTok>) {
  let line = lex.slice();
  let blank = is_blank_line(line);
  let line_idx = lex.extras.terminators; // 0 for first logical line

  if !lex.extras.saw_nonblank_any {
    if blank {
      lex.extras.leading_blank_lines += 1;
    } else {
      lex.extras.saw_nonblank_any = true;
      if line_idx > 0 {
        let ind = leading_ws_indent(line);
        lex.extras.common_indent = Some(lex.extras.common_indent.map_or(ind, |m| m.min(ind)));
      }
      lex.extras.trailing_blank_lines = 0;
    }
  } else if blank {
    lex.extras.trailing_blank_lines += 1;
  } else {
    lex.extras.trailing_blank_lines = 0;
    if line_idx > 0 {
      let ind = leading_ws_indent(line);
      lex.extras.common_indent = Some(lex.extras.common_indent.map_or(ind, |m| m.min(ind)));
    }
  }

  lex.extras.saw_body_this_line = true;
}

#[inline]
fn on_terminator(lex: &mut Lexer<'_, BlockLineTok>) {
  let t = lex.slice().as_bytes();
  if !t.is_empty() && t[0] == b'\r' {
    lex.extras.has_cr_terminators = true;
  }

  // empty line (no LineBody since last terminator) is blank
  if !lex.extras.saw_body_this_line {
    if !lex.extras.saw_nonblank_any {
      lex.extras.leading_blank_lines += 1;
    } else {
      lex.extras.trailing_blank_lines += 1;
    }
  }

  lex.extras.terminators += 1;
  lex.extras.saw_body_this_line = false;
}
