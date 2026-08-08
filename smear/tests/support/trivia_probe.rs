//! The behavioural half of `SURFACES_TRIVIA`, owned in one place and read by both dialects' wiring
//! gates.
//!
//! # Why this is a module and not two copies
//!
//! `lossless_wiring.rs` and `lossless_x_wiring.rs` ask the same three questions of two different
//! lexers, and the questions are the content: the probe source has to carry **every** ignorable
//! form in a position the others do not cover, and the tiling and `is_trivia` assertions have to
//! be the same assertions or the two dialects are not being held to the same standard. A Rust
//! integration test is its own crate, so nothing in one file can name an item in the other and two
//! copies would be two copies — agreeing on the day they were written and free to drift after.
//! This is the mechanism `support/span_extent.rs` already uses for the pair of syntactic
//! span-extent gates, and `support/graphqlx_padding.rs` for the pair of GraphQLx padding gates.
//!
//! Directory placement is load-bearing. Cargo's test autodiscovery takes `tests/*.rs` and
//! `tests/*/main.rs`; a file one level down under any other name is not a target, which is what
//! lets this be a shared module rather than a third test binary that runs nothing.
//!
//! Nothing here names a dialect. Both gates instantiate it at their own `LosslessLexer` and pass
//! their own alphabet, so the module compiles under either dialect's feature and under both.

use tokora::{Lexer, Span, Token};

/// One source carrying every ignorable form, for the behavioural half of the `SURFACES_TRIVIA`
/// guards.
///
/// `\u{FEFF}` `# lead` `\r\n` `{` `\t` `f` ` ` `,` `\r` `g` `\n` `}` — the byte-order mark, a
/// comment, a CRLF, a tab, a space, a comma, a bare carriage return and a newline, which is all
/// eight forms `is_trivia` admits, each in a position the others do not cover. The bare `\r` is
/// deliberately not followed by a `\n`, because `\r\n` is one token and a probe that only ever
/// wrote the pair could not tell the two apart.
///
/// It is also a valid document (`{ f, g }` with padding) in **both** dialects, so the same bytes
/// are a live parse rather than a lexer-only curiosity.
const TRIVIA_PROBE: &str = "\u{FEFF}# lead\r\n{\tf ,\rg\n}";

/// Every token `src` lexes to, as `(kind, is_trivia, start, end)`.
///
/// `Token::kind` and `Token::is_trivia` are reached through the trait rather than through the
/// inherent methods of the same name, because the trait is the surface `SURFACES_TRIVIA` sits on
/// and the surface tokora's `Sink` reads.
fn scan<'inp, L>(src: &'inp str) -> Vec<(<L::Token as Token<'inp>>::Kind, bool, usize, usize)>
where
  L: Lexer<'inp, Source = str, Offset = usize>,
{
  let mut lexer = L::new(src);
  let mut tokens = Vec::new();
  while let Some(result) = lexer.lex() {
    let token = result.unwrap_or_else(|e| panic!("{src:?} must lex: {e:?}"));
    let span = lexer.span();
    tokens.push((
      Token::kind(&token),
      Token::is_trivia(&token),
      span.start(),
      span.end(),
    ));
  }
  tokens
}

/// The behavioural half of `SURFACES_TRIVIA`, over one dialect.
///
/// Three claims, and each is one way the declaration can become a lie:
///
/// - **Every form in `alphabet` reaches the caller as a token.** A lexer that gains a
///   `#[logos(skip …)]` for whitespace, or drops the `Comment` arm, keeps compiling and keeps
///   declaring `true`.
/// - **The tokens tile the source.** This is the form-agnostic half, and it is the one that still
///   answers when a *new* ignorable form is added to the dialect and not to `alphabet`: a skipped
///   run is a hole between one token's end and the next one's start, whatever it was made of.
/// - **`is_trivia` agrees with the alphabet in both directions.** A form that is surfaced but not
///   classified as trivia is not surfaced *as trivia*, and a lexer that answered `true` for
///   everything would satisfy the first two claims while making the atoms skip the whole file.
pub fn assert_trivia_survives_lexing<'inp, L>(
  dialect: &str,
  alphabet: &[(&str, <L::Token as Token<'inp>>::Kind)],
) where
  L: Lexer<'inp, Source = str, Offset = usize>,
{
  let tokens = scan::<L>(TRIVIA_PROBE);
  assert!(
    !tokens.is_empty(),
    "{dialect}: the trivia probe produced no tokens at all"
  );

  let missing: Vec<&str> = alphabet
    .iter()
    .filter(|(_, kind)| !tokens.iter().any(|(seen, ..)| seen == kind))
    .map(|(form, _)| *form)
    .collect();
  assert!(
    missing.is_empty(),
    "{dialect}: the lexer no longer surfaces {missing:?}, so `SURFACES_TRIVIA = true` is a promise \
     it does not keep. The declaration is a compile-time constant and dropping a form does not \
     move it — tokora's `Sink` still admits the lexer and the build stays clean. Other gates will \
     be red beside this one; this is the one that says which form went."
  );

  let mut cursor = 0usize;
  for (kind, _, start, end) in &tokens {
    assert_eq!(
      *start, cursor,
      "{dialect}: nothing was surfaced for the bytes at {cursor}..{start} — the lexer skipped a \
       run rather than emitting a token for it, and the next token is a {kind}"
    );
    cursor = *end;
  }
  assert_eq!(
    cursor,
    TRIVIA_PROBE.len(),
    "{dialect}: the token stream stops before the end of the probe"
  );

  for (kind, is_trivia, start, end) in &tokens {
    let in_alphabet = alphabet.iter().any(|(_, form)| form == kind);
    assert_eq!(
      *is_trivia, in_alphabet,
      "{dialect}: the {kind} at {start}..{end} answers `is_trivia() == {is_trivia}`, and the \
       alphabet says it should answer {in_alphabet}"
    );
  }
}
