use logosky::{
  Lexable, Source,
  logos::{Lexer, Logos},
};

use crate::error::{StringError, StringErrors};

use super::{super::SealedWrapper, BlockLineExtras, LitBlockStr, LitComplexBlockStr, LitPlainStr};

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(crate = logosky::logos, source = [u8], error(StringError<u8>))]
pub(crate) enum BlockStringToken {
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

impl<'a, S, T> Lexable<&mut SealedWrapper<Lexer<'a, T>>, StringErrors<u8>>
  for LitBlockStr<S::Slice<'a>>
where
  T: Logos<'a, Source = S>,
  S: Source + ?Sized + 'a,
  S::Slice<'a>: AsRef<[u8]>,
{
  #[inline]
  fn lex(lexer: &mut SealedWrapper<Lexer<'a, T>>) -> Result<Self, StringErrors<u8>>
  where
    Self: Sized,
  {
    lex_block_str_from_bytes(lexer)
  }
}

#[inline]
pub(crate) fn lex_block_str_from_bytes<'a, S, T>(
  lexer: &mut SealedWrapper<Lexer<'a, T>>,
) -> Result<LitBlockStr<S::Slice<'a>>, StringErrors<u8>>
where
  T: Logos<'a, Source = S>,
  S: Source + ?Sized + 'a,
  S::Slice<'a>: AsRef<[u8]>,
{
  let remainder = lexer.remainder();
  let remainder_bytes = remainder.as_ref();
  let lexer_span = lexer.span();
  let mut string_lexer = BlockStringToken::lexer(remainder_bytes);
  let mut errs = StringErrors::default();

  let mut num_escaped_triple_quotes = 0;
  while let Some(string_token) = string_lexer.next() {
    match string_token {
      Err(mut e) => {
        e.bump(lexer_span.end);
        errs.push(e);
      }
      Ok(BlockStringToken::EscapedTripleQuote) => {
        num_escaped_triple_quotes += 1;
      }
      Ok(BlockStringToken::Continue | BlockStringToken::Backslash | BlockStringToken::Quote) => {}
      Ok(BlockStringToken::TripleQuote) => {
        // Outer lexer consumes up to (and including) the closing """
        lexer.bump(string_lexer.span().end);

        // inner content (between opening and closing)
        let content = &remainder_bytes[..string_lexer.span().start];

        // sub-lex the inner content to compute normalization knobs (no allocations)
        let mut lines = BlockLineTok::lexer_with_extras(content, BlockLineExtras::default());
        while lines.next().is_some() {
          // callbacks already updated `lines.extras`
        }

        // Build the normalization plan + exact capacity
        let plan = super::compute_block_normalization_plan(
          &lines.extras,
          !content.is_empty(),
          num_escaped_triple_quotes,
        );

        if plan.is_clean {
          return Ok(LitPlainStr::new(lexer.slice()).into());
        }

        return Ok(
          LitComplexBlockStr::new(
            lexer.slice(),
            num_escaped_triple_quotes,
            lines.extras.has_cr_terminators,
            lines.extras.leading_blank_lines,
            plan.effective_trailing,
            plan.common_indent,
            plan.total_lines,
            plan.required_capacity,
          )
          .into(),
        );
      }
    }
  }

  lexer.bump(string_lexer.span().end);
  errs.push(StringError::unterminated_block_string());
  Err(errs)
}

// ---- sub-lexer over inner block-string content ----
#[derive(Logos, Debug)]
#[logos(crate = logosky::logos, source = [u8], extras = BlockLineExtras)]
enum BlockLineTok {
  /// Body of a line (one or more bytes, never includes a terminator).
  /// We process the whole line in the callback.
  #[regex(r#"[^\r\n]+"#, on_line_body)]
  LineBody,

  /// One line terminator: \r\n | \r | \n
  #[regex(r#"\r\n|\r|\n"#, on_terminator)]
  Terminator,
}

// callbacks mutate `extras` to record state + capacity
#[inline]
fn on_line_body(lex: &mut Lexer<'_, BlockLineTok>) {
  let line = lex.slice();
  let len = line.len();
  let blank = super::is_blank_line(line);
  let line_idx = lex.extras.terminators; // 0 for first logical line

  if !lex.extras.saw_nonblank_any {
    if blank {
      // leading blank run
      lex.extras.leading_blank_lines += 1;
      lex.extras.pending_blank_body_bytes += len;
    } else {
      // first nonblank line
      lex.extras.saw_nonblank_any = true;

      if line_idx > 0 {
        let ind = super::leading_ws_indent(line);
        lex.extras.common_indent = Some(lex.extras.common_indent.map_or(ind, |m| m.min(ind)));
        lex.extras.nonblank_after_first_count += 1;
      }
      lex.extras.nonblank_body_bytes += len;

      // drop the pending leading blanks (not kept)
      lex.extras.pending_blank_body_bytes = 0;
      lex.extras.trailing_blank_lines = 0;
    }
  } else if blank {
    // possible middle-or-trailing blank run
    lex.extras.pending_blank_body_bytes += len;
    lex.extras.trailing_blank_lines += 1;
  } else {
    // nonblank after some content
    if line_idx > 0 {
      let ind = super::leading_ws_indent(line);
      lex.extras.common_indent = Some(lex.extras.common_indent.map_or(ind, |m| m.min(ind)));
      lex.extras.nonblank_after_first_count += 1;
    }
    lex.extras.nonblank_body_bytes += len;

    // pending blanks are in the middle (kept); move to middle bucket
    lex.extras.middle_blank_body_bytes += lex.extras.pending_blank_body_bytes;
    lex.extras.pending_blank_body_bytes = 0;

    lex.extras.trailing_blank_lines = 0;
  }

  lex.extras.saw_body_this_line = true;
}

#[inline]
fn on_terminator(lex: &mut Lexer<'_, BlockLineTok>) {
  let t = lex.slice();
  if !t.is_empty() && t[0] == b'\r' {
    lex.extras.has_cr_terminators = true;
  }

  // empty line (no LineBody since last terminator) is blank
  if !lex.extras.saw_body_this_line {
    if !lex.extras.saw_nonblank_any {
      lex.extras.leading_blank_lines += 1;
      // body bytes for an empty line are 0
    } else {
      lex.extras.trailing_blank_lines += 1;
      // body bytes for this empty line are 0; pending stays as-is
    }
  }

  lex.extras.terminators += 1;
  lex.extras.saw_body_this_line = false;
}
