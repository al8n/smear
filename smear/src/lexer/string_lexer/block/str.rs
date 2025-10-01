use logosky::{
  Lexable, Source,
  logos::{Lexer, Logos},
};

use crate::error::{StringError, StringErrors};

use super::{super::SealedWrapper, BlockLineExtras, LitBlockStr, LitComplexBlockStr, LitPlainStr};

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(crate = logosky::logos, error(StringError<char>))]
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

impl<'l, S, T> Lexable<&mut SealedWrapper<Lexer<'l, T>>, StringErrors<char>>
  for LitBlockStr<S::Slice<'l>>
where
  T: Logos<'l, Source = S>,
  S: Source + ?Sized + 'l,
  S::Slice<'l>: AsRef<str>,
{
  #[inline]
  fn lex(lexer: &mut SealedWrapper<Lexer<'l, T>>) -> Result<Self, StringErrors<char>>
  where
    Self: Sized,
  {
    lex_block_str_from_str(lexer)
  }
}

#[inline]
pub(crate) fn lex_block_str_from_str<'l, S, T>(
  lexer: &mut SealedWrapper<Lexer<'l, T>>,
) -> Result<LitBlockStr<S::Slice<'l>>, StringErrors<char>>
where
  T: Logos<'l, Source = S>,
  S: Source + ?Sized + 'l,
  S::Slice<'l>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  let outer_span = lexer.span();

  let mut string_lexer = BlockStringToken::lexer(remainder_str);
  let mut errs = StringErrors::default();

  let mut escaped_triple_count = 0usize;

  while let Some(tok) = string_lexer.next() {
    match tok {
      Err(mut e) => {
        e.bump(outer_span.end);
        errs.push(e);
      }
      Ok(BlockStringToken::EscapedTripleQuote) => {
        escaped_triple_count += 1;
      }
      Ok(BlockStringToken::Continue | BlockStringToken::Backslash | BlockStringToken::Quote) => {}
      Ok(BlockStringToken::TripleQuote) => {
        // Consume up to (and including) the closing """
        let end_off = string_lexer.span().end;
        lexer.bump(end_off);

        // Inner content (between opening and closing)
        let content = &remainder_str[..string_lexer.span().start];

        // Sub-lex inner content to gather normalization facts + capacity
        let mut lines = BlockLineTok::lexer_with_extras(content, BlockLineExtras::default());
        while lines.next().is_some() {
          // callbacks update lines.extras
        }

        // Build the normalization plan + exact capacity
        let plan = super::compute_block_normalization_plan(
          &lines.extras,
          !content.is_empty(),
          escaped_triple_count,
        );

        if plan.is_clean {
          return Ok(LitPlainStr::new(lexer.slice()).into());
        }

        return Ok(
          LitComplexBlockStr::new(
            lexer.slice(),
            escaped_triple_count,
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

  // EOF without closing """
  lexer.bump(string_lexer.span().end);
  errs.push(StringError::unterminated_block_string());
  Err(errs)
}

#[derive(Logos, Debug)]
#[logos(crate = logosky::logos, extras = BlockLineExtras)]
enum BlockLineTok {
  /// Body of a line (never includes a terminator).
  #[regex(r#"[^\r\n]+"#, on_line_body)]
  LineBody,
  /// One line terminator: \r\n | \r | \n
  #[regex(r#"\r\n|\r|\n"#, on_terminator)]
  Terminator,
}

#[inline]
fn on_line_body(lex: &mut Lexer<'_, BlockLineTok>) {
  let line = lex.slice();
  let len = line.len(); // UTF-8 bytes
  let blank = super::is_blank_line(line.as_bytes());
  let line_idx = lex.extras.terminators; // 0 for first logical line

  if !lex.extras.saw_nonblank_any {
    if blank {
      lex.extras.leading_blank_lines += 1;
      lex.extras.pending_blank_body_bytes += len;
    } else {
      lex.extras.saw_nonblank_any = true;

      if line_idx > 0 {
        let ind = super::leading_ws_indent(line.as_bytes());
        lex.extras.common_indent = Some(lex.extras.common_indent.map_or(ind, |m| m.min(ind)));
        lex.extras.nonblank_after_first_count += 1;
      }
      lex.extras.nonblank_body_bytes += len;

      // discard pending leading blanks
      lex.extras.pending_blank_body_bytes = 0;
      lex.extras.trailing_blank_lines = 0;
    }
  } else if blank {
    // may end up middle or trailing
    lex.extras.pending_blank_body_bytes += len;
    lex.extras.trailing_blank_lines += 1;
  } else {
    // nonblank after some content
    if line_idx > 0 {
      let ind = super::leading_ws_indent(line.as_bytes());
      lex.extras.common_indent = Some(lex.extras.common_indent.map_or(ind, |m| m.min(ind)));
      lex.extras.nonblank_after_first_count += 1;
    }
    lex.extras.nonblank_body_bytes += len;

    // pending blanks are now 'middle' (kept)
    lex.extras.middle_blank_body_bytes += lex.extras.pending_blank_body_bytes;
    lex.extras.pending_blank_body_bytes = 0;

    lex.extras.trailing_blank_lines = 0;
  }

  lex.extras.saw_body_this_line = true;
}

#[inline]
fn on_terminator(lex: &mut Lexer<'_, BlockLineTok>) {
  let t = lex.slice().as_bytes();
  if !t.is_empty() && t[0] == b'\r' {
    lex.extras.has_cr_terminators = true;
  }

  // Empty physical line (no LineBody since last terminator) is a blank line
  if !lex.extras.saw_body_this_line {
    if !lex.extras.saw_nonblank_any {
      lex.extras.leading_blank_lines += 1;
      // (body bytes = 0)
    } else {
      lex.extras.trailing_blank_lines += 1;
      // (body bytes = 0; pending already accounts for it if any)
    }
  }

  lex.extras.terminators += 1;
  lex.extras.saw_body_this_line = false;
}
