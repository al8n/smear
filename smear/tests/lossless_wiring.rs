#![cfg(all(feature = "rowan", feature = "graphql"))]

use smear::lexer::graphql::lossless::LosslessToken;
use tokora::{Lexer, Span, Token};

/// The wiring guard, stated where it is actually decided.
///
/// tokora's `cst::Sink` is compile-time restricted to trivia-surfacing lexers. This declaration
/// is what admits the lossless door; without it `cst::parse_lossless` will not compile.
///
/// **Why a `const` item and not a runtime `assert!`.** `SURFACES_TRIVIA` is an associated
/// *constant*, so a runtime assertion over it re-checks a question the compiler already settled
/// while building the binary that runs the check — which is what `clippy::assertions_on_constants`
/// says, and it is right. A `const` item is the same assertion moved to the moment the answer
/// exists: a false declaration is then a build failure with this message in it, in **every**
/// `--features rowan` build, rather than one red test in the one profile that runs the suite.
/// Nothing is weakened by the move; the check is strictly earlier and strictly wider.
///
/// **What it does not check is that the declaration is true.** It is a promise about behaviour
/// made as a constant, so a lexer that stops surfacing a form while still declaring `true`
/// satisfies it and satisfies tokora's compile-time totality check with it — measured against both
/// of smear#131's drop defects. [`the_lossless_lexer_surfaces_every_trivia_form`] is the
/// behavioural companion, and the two are a pair: this one is what a build cannot get past, that
/// one is what a build cannot lie to.
const _: () = assert!(
  <LosslessToken<&str> as Token<'_>>::SURFACES_TRIVIA,
  "the lossless token must declare SURFACES_TRIVIA = true"
);

/// The guard above, named so `cargo test` reports it.
///
/// The body is deliberately empty: reaching this function at all means the crate compiled, and
/// the crate compiling is the assertion. Keeping the test is not ceremony — it is what puts
/// `the_lossless_token_surfaces_trivia` in the suite's roster, so the property has a name a
/// reader can look for rather than living only in a comment.
#[test]
fn the_lossless_token_surfaces_trivia() {}

/// One source carrying every ignorable form, for the behavioural half of the guard above.
///
/// `\u{FEFF}` `# lead` `\r\n` `{` `\t` `f` ` ` `,` `\r` `g` `\n` `}` — the byte-order mark, a
/// comment, a CRLF, a tab, a space, a comma, a bare carriage return and a newline, which is all
/// eight forms `is_trivia` admits, each in a position the others do not cover. The bare `\r` is
/// deliberately not followed by a `\n`, because `\r\n` is one token and a probe that only ever
/// wrote the pair could not tell the two apart.
///
/// It is also a valid document (`{ f, g }` with padding), so the same bytes are a live parse rather
/// than a lexer-only curiosity.
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
fn assert_trivia_survives_lexing<'inp, L>(
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

/// The behavioural companion to the declaration above, for GraphQL.
///
/// The `const _` two items up checks that the lexer *says* it surfaces trivia. This checks that it
/// does.
///
/// # What it adds over what was already here, measured rather than assumed
///
/// **It is not the only witness, and the issue that asked for it expected it would be.** smear#131
/// read the `pql` result — where byte-exact round-trip and gap-free tiling both stay green when the
/// lexer stops surfacing trivia — and inferred the same hole here. It is not the same hole. Planting
/// the two drop defects against this workspace reds seventeen test binaries (whitespace skipped) and
/// eleven (comments skipped), the gap census
/// `lossless_roundtrip::every_byte_is_carried_by_a_token` among them both times, because smear's
/// sink tiles the skipped run as a `Gap` and that census forbids a gap in a cleanly accepted parse.
/// And `lossless_trivia_atoms.rs::is_trivia_covers_every_graphql_ignored_token_and_commits_it`
/// already enumerates these same eight forms and asserts each is committed byte for byte. What
/// stayed green through both defects was the *declaration*, which is the hole the issue named
/// correctly.
///
/// So three things are on offer here rather than one:
///
/// - it sits **beside the constant it is about**, so a reader of the declaration meets the
///   measurement instead of the promise;
/// - it reads the **lexer**, where the atom-level test drives a parser `InputRef` behind
///   `feature = "test-support"` — this holds for a consumer who never builds a tree;
/// - it asserts the tokens **tile the source**, which nothing else asserts at this level and which
///   is the only claim here that still answers for a form no list names.
#[test]
fn the_lossless_lexer_surfaces_every_trivia_form() {
  use smear::lexer::graphql::lossless::{LosslessLexer, LosslessTokenKind as LK};

  assert_trivia_survives_lexing::<LosslessLexer<'_, &str>>(
    "graphql",
    &[
      ("space", LK::Space),
      ("tab", LK::Tab),
      ("newline", LK::Newline),
      ("carriage return", LK::CarriageReturn),
      ("CRLF", LK::CarriageReturnAndNewline),
      ("comment", LK::Comment),
      ("comma", LK::Comma),
      ("BOM", LK::Bom),
    ],
  );
}

/// The GraphQLx twin of the guard above.
///
/// Phase B found this missing: `smear/src/lexer/graphqlx/lossless/token.rs`'s two `tokora::Token`
/// impls define `Kind`, `Error`, `kind` and `is_trivia` and stop there, so the associated constant
/// took tokora's default of `false` and a GraphQLx `Sink` would have failed to build — as an
/// `E0080` from inside tokora, a long way from the declaration that caused it. This states the
/// requirement where a reader of the parser can find it.
#[cfg(feature = "graphqlx")]
const _: () = assert!(
  <smear::lexer::graphqlx::lossless::LosslessToken<&str> as Token<'_>>::SURFACES_TRIVIA,
  "the GraphQLx lossless token must declare SURFACES_TRIVIA = true"
);

/// The guard above, named so `cargo test` reports it.
#[cfg(feature = "graphqlx")]
#[test]
fn the_graphqlx_lossless_token_surfaces_trivia() {}

/// The behavioural companion to the declaration above, for GraphQLx.
///
/// The same eight forms, and the same reason: the GraphQLx lexer is a separate `Logos` derive over
/// a wider token set, so it can lose a form on its own — the GraphQL probes leave this test green,
/// which is the measurement that says the two are independent. See
/// [`the_lossless_lexer_surfaces_every_trivia_form`] for what this adds over
/// `lossless_x_trivia_atoms.rs`'s enumeration of the same eight.
#[cfg(feature = "graphqlx")]
#[test]
fn the_graphqlx_lossless_lexer_surfaces_every_trivia_form() {
  use smear::lexer::graphqlx::lossless::{LosslessLexer, LosslessTokenKind as LK};

  assert_trivia_survives_lexing::<LosslessLexer<'_, &str>>(
    "graphqlx",
    &[
      ("space", LK::Space),
      ("tab", LK::Tab),
      ("newline", LK::Newline),
      ("carriage return", LK::CarriageReturn),
      ("CRLF", LK::CarriageReturnAndNewline),
      ("comment", LK::Comment),
      ("comma", LK::Comma),
      ("BOM", LK::Bom),
    ],
  );
}

#[test]
fn tokoras_cst_layer_is_in_scope() {
  // Proves the `rowan` feature actually reaches tokora, not just this crate.
  fn _assert_sink_type_exists<'inp, L: tokora::Lexer<'inp>, E>(
    _: Option<tokora::cst::Sink<'inp, L, E>>,
  ) {
  }
}

/// Each dialect exposes **three** document roots as public `fn(&str) -> Parse`, at the dialect's
/// `lossless` module and not inside `test_support`.
///
/// This is the guard smear issue #67 was filed for the absence of. Two of the three roots existed,
/// worked, and were documented as things "a schema-only consumer would call directly" while being
/// `pub(crate)` — reachable only from a driver behind `feature = "test-support"`, which is
/// compiled out of every shipped build.
///
/// # What the coercion pins that a call could not
///
/// Binding each entry as a `fn(&str) -> Parse` **pointer** fails to compile if the item is moved
/// back behind the feature gate, made `pub(crate)`, renamed, or given a different signature —
/// including the plausible regression of taking the source by something other than `&str`, which
/// a call site passing a `&'static str` would silently coerce into. The array then states the
/// other half: all three have the *same* shape, so a consumer can pick a root at run time, which
/// is exactly what `lossless_x_roundtrip.rs`'s `ROOTS` table does.
///
/// The behavioural half — that the three are three *different* parsers rather than one function
/// under three names — is `lossless_parity.rs`'s
/// `both_alternate_roots_agree_with_their_syntactic_counterparts`, which holds each against its
/// own counterpart in the syntactic suite and counts the corpus entries that separate them.
#[cfg(feature = "graphql")]
#[test]
fn the_graphql_lossless_suite_exposes_its_three_roots() {
  use smear::parser::graphql::lossless::{
    Parse, parse_document, parse_executable_document, parse_type_system_document,
  };

  let roots: [fn(&str) -> Parse; 3] = [
    parse_document,
    parse_type_system_document,
    parse_executable_document,
  ];
  assert_eq!(roots.len(), 3);
}

/// The GraphQLx twin of the guard above.
#[cfg(feature = "graphqlx")]
#[test]
fn the_graphqlx_lossless_suite_exposes_its_three_roots() {
  use smear::parser::graphqlx::lossless::{
    Parse, parse_document, parse_executable_document, parse_type_system_document,
  };

  let roots: [fn(&str) -> Parse; 3] = [
    parse_document,
    parse_type_system_document,
    parse_executable_document,
  ];
  assert_eq!(roots.len(), 3);
}
