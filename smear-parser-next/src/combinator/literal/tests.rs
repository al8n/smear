// Literal atoms need `LiteralValueToken`, which both GraphQL and GraphQLx
// `SyntacticToken`s now supply, so the literal tests drive both dialects. GraphQL
// yields the raw source slice for ints/floats; GraphQLx preserves the radix in the
// `LitInt`/`LitFloat` payload, so its matrix also covers hex/octal/binary ints
// (including negatives folded into the slice), `_` separators, and hex floats.
// Every atom runs over the full source matrix (`str`, `[u8]`, and `Bytes`). The
// atoms are declining-only — they consume nothing on a miss and never emit a
// diagnostic — so the accept and decline paths need a single emitter mode; the
// harness stays emitter-parametrized to match the token atoms' harness. Each
// dialect lives in its own module so its drive helpers can pin one concrete lexer.
#![cfg(any(feature = "graphql", feature = "graphqlx"))]

use smear_lexer::tokora::error::{
  UnexpectedEot,
  syntax::{FullContainer, MissingSyntax, TooFew},
  token::{MissingToken, SeparatedError, UnexpectedToken},
};

/// A test error sink that absorbs every tokora error family (and each dialect's
/// lexer errors) into a unit. Implementing the full `From` set makes it a
/// [`FromEmitterError`](smear_lexer::tokora::emitter::FromEmitterError), so
/// [`Fatal`](smear_lexer::tokora::emitter::Fatal) instantiates as a complete
/// emitter over it.
#[derive(Debug)]
struct TestError;

impl<'a, T, Kind: Clone, S, Lang: ?Sized> From<UnexpectedToken<'a, T, Kind, S, Lang>>
  for TestError
{
  fn from(_: UnexpectedToken<'a, T, Kind, S, Lang>) -> Self {
    Self
  }
}

impl<'a, T, Kind: Clone, S, Lang: ?Sized> From<SeparatedError<'a, T, Kind, S, Lang>> for TestError {
  fn from(_: SeparatedError<'a, T, Kind, S, Lang>) -> Self {
    Self
  }
}

impl<'a, Kind: Clone, O, Lang: ?Sized> From<MissingToken<'a, Kind, O, Lang>> for TestError {
  fn from(_: MissingToken<'a, Kind, O, Lang>) -> Self {
    Self
  }
}

impl<O, Lang: ?Sized> From<UnexpectedEot<O, Lang>> for TestError {
  fn from(_: UnexpectedEot<O, Lang>) -> Self {
    Self
  }
}

impl<O, Lang: ?Sized> From<MissingSyntax<O, Lang>> for TestError {
  fn from(_: MissingSyntax<O, Lang>) -> Self {
    Self
  }
}

impl<S, Lang: ?Sized> From<FullContainer<S, Lang>> for TestError {
  fn from(_: FullContainer<S, Lang>) -> Self {
    Self
  }
}

impl<S, Lang: ?Sized> From<TooFew<S, Lang>> for TestError {
  fn from(_: TooFew<S, Lang>) -> Self {
    Self
  }
}

#[cfg(feature = "graphql")]
impl<Char, StateError> From<smear_lexer::graphql::error::LexerErrors<Char, StateError>>
  for TestError
{
  fn from(_: smear_lexer::graphql::error::LexerErrors<Char, StateError>) -> Self {
    Self
  }
}

#[cfg(feature = "graphqlx")]
impl<Char, StateError> From<smear_lexer::graphqlx::error::LexerErrors<Char, StateError>>
  for TestError
{
  fn from(_: smear_lexer::graphqlx::error::LexerErrors<Char, StateError>) -> Self {
    Self
  }
}

/// Views a lexer slice (`&str` or `&[u8]`) as bytes via `AsRef<[u8]>`, so one
/// assertion body reads a payload's text across every source representation.
fn as_bytes<S: AsRef<[u8]>>(slice: &S) -> &[u8] {
  slice.as_ref()
}

// Accept paths assert the returned span and the payload slice (for strings, the
// carrier's raw source slice via `source_ref`, not the unescaped text; for
// GraphQLx numbers, the radix-tagged `LitInt`/`LitFloat` variant plus its slice).
// Decline paths assert the leftover's TYPE by parsing it with the atom that
// matches it, which also proves the decline consumed nothing.

#[cfg(feature = "graphql")]
mod graphql {
  use super::{TestError, as_bytes};

  use smear_lexer::{
    graphql::syntactic::SyntacticLexer,
    tokora::{Emitter, InputRef, Parse, Parser, ParserContext, SimpleSpan, emitter::Fatal},
  };

  use crate::combinator::{
    ident,
    literal::{StringLiteral, try_block_str, try_float, try_inline_str, try_int, try_string},
  };

  // The drive helpers pin one concrete lexer per source yet stay generic over the
  // emitter, mirroring the token atoms' harness.

  fn drive_str<'inp, O, Em>(
    emitter: Em,
    f: impl for<'c> FnMut(
      &mut InputRef<
        'inp,
        'c,
        SyntacticLexer<'inp, str>,
        ParserContext<'inp, SyntacticLexer<'inp, str>, Em>,
      >,
    ) -> Result<O, TestError>,
    input: &'inp str,
  ) -> Result<O, TestError>
  where
    Em: Emitter<'inp, SyntacticLexer<'inp, str>, Error = TestError>,
  {
    let ctx: ParserContext<'inp, SyntacticLexer<'inp, str>, Em> = ParserContext::new(emitter);
    Parser::with_parser_and_context(f, ctx).parse_str(input)
  }

  fn drive_slice<'inp, O, Em>(
    emitter: Em,
    f: impl for<'c> FnMut(
      &mut InputRef<
        'inp,
        'c,
        SyntacticLexer<'inp, [u8]>,
        ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em>,
      >,
    ) -> Result<O, TestError>,
    input: &'inp [u8],
  ) -> Result<O, TestError>
  where
    Em: Emitter<'inp, SyntacticLexer<'inp, [u8]>, Error = TestError>,
  {
    let ctx: ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em> = ParserContext::new(emitter);
    Parser::with_parser_and_context(f, ctx).parse_slice(input)
  }

  #[cfg(feature = "bytes")]
  fn drive_bytes<'inp, O, Em>(
    emitter: Em,
    f: impl for<'c> FnMut(
      &mut InputRef<
        'inp,
        'c,
        SyntacticLexer<'inp, [u8]>,
        ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em>,
      >,
    ) -> Result<O, TestError>,
    input: &'inp ::bytes::Bytes,
  ) -> Result<O, TestError>
  where
    Em: Emitter<'inp, SyntacticLexer<'inp, [u8]>, Error = TestError>,
  {
    let ctx: ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em> = ParserContext::new(emitter);
    Parser::with_parser_and_context(f, ctx).parse_bytes(input)
  }

  /// Runs `body` against `src` under `emitter` across every source representation
  /// (`str`, `[u8]`, and `Bytes`), applying `assert` to each `Result`.
  macro_rules! drive_all {
    ($emitter:expr, |$inp:ident| $body:expr, $src:expr, $assert:expr) => {{
      $assert(drive_str($emitter, |$inp: &mut _| $body, $src));
      $assert(drive_slice($emitter, |$inp: &mut _| $body, $src.as_bytes()));
      #[cfg(feature = "bytes")]
      {
        let owned = ::bytes::Bytes::from_static($src.as_bytes());
        $assert(drive_bytes($emitter, |$inp: &mut _| $body, &owned));
      }
    }};
  }

  #[test]
  fn try_int_accepts_int() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_int(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert_eq!(as_bytes(&value), b"42");
        assert_eq!(span, SimpleSpan::new(0, 2));
        Ok::<_, TestError>(())
      },
      "42",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_float_accepts_float() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_float(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert_eq!(as_bytes(&value), b"-1.5e3");
        assert_eq!(span, SimpleSpan::new(0, 6));
        Ok::<_, TestError>(())
      },
      "-1.5e3",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_inline_str_accepts_inline() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_inline_str(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert_eq!(as_bytes(value.source_ref()), b"\"hi\"");
        assert_eq!(span, SimpleSpan::new(0, 4));
        Ok::<_, TestError>(())
      },
      "\"hi\"",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_block_str_accepts_block() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_block_str(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert_eq!(as_bytes(value.source_ref()), b"\"\"\"block\"\"\"");
        assert_eq!(span, SimpleSpan::new(0, 11));
        Ok::<_, TestError>(())
      },
      "\"\"\"block\"\"\"",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_string_accepts_inline() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_string(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_inline());
        match value {
          StringLiteral::Inline(inline) => assert_eq!(as_bytes(inline.source_ref()), b"\"hi\""),
          StringLiteral::Block(_) => panic!("expected an inline string payload"),
        }
        assert_eq!(span, SimpleSpan::new(0, 4));
        Ok::<_, TestError>(())
      },
      "\"hi\"",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_string_accepts_block() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_string(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_block());
        match value {
          StringLiteral::Block(block) => {
            assert_eq!(as_bytes(block.source_ref()), b"\"\"\"block\"\"\"")
          }
          StringLiteral::Inline(_) => panic!("expected a block string payload"),
        }
        assert_eq!(span, SimpleSpan::new(0, 11));
        Ok::<_, TestError>(())
      },
      "\"\"\"block\"\"\"",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_int_declines_on_ident_and_leaves_it() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let declined = try_int(inp)?.is_decline();
        // The declined identifier is untouched, so the committed `ident` atom pulls
        // it straight off the input; its slice and span prove the leftover is the
        // `x` identifier and that the decline consumed nothing.
        let id = ident(inp)?;
        assert_eq!(as_bytes(id.source_ref()), b"x");
        assert_eq!(id.span(), SimpleSpan::new(0, 1));
        Ok::<_, TestError>(declined)
      },
      "x",
      |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
    );
  }

  #[test]
  fn try_int_declines_on_float_and_leaves_it() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let declined = try_int(inp)?.is_decline();
        let attempt = try_float(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert_eq!(as_bytes(&value), b"-1.5e3");
        assert_eq!(span, SimpleSpan::new(0, 6));
        Ok::<_, TestError>(declined)
      },
      "-1.5e3",
      |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
    );
  }

  #[test]
  fn try_inline_str_declines_on_block_and_leaves_it() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let declined = try_inline_str(inp)?.is_decline();
        let attempt = try_block_str(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert_eq!(as_bytes(value.source_ref()), b"\"\"\"block\"\"\"");
        assert_eq!(span, SimpleSpan::new(0, 11));
        Ok::<_, TestError>(declined)
      },
      "\"\"\"block\"\"\"",
      |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
    );
  }

  #[test]
  fn try_block_str_declines_on_inline_and_leaves_it() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let declined = try_block_str(inp)?.is_decline();
        let attempt = try_inline_str(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert_eq!(as_bytes(value.source_ref()), b"\"hi\"");
        assert_eq!(span, SimpleSpan::new(0, 4));
        Ok::<_, TestError>(declined)
      },
      "\"hi\"",
      |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
    );
  }
}

#[cfg(feature = "graphqlx")]
mod graphqlx {
  use super::{TestError, as_bytes};

  use smear_lexer::{
    graphqlx::syntactic::SyntacticLexer,
    tokora::{Emitter, InputRef, Parse, Parser, ParserContext, SimpleSpan, emitter::Fatal},
  };

  use crate::combinator::{
    ident,
    literal::{StringLiteral, try_block_str, try_float, try_inline_str, try_int, try_string},
  };

  // The drive helpers pin one concrete lexer per source yet stay generic over the
  // emitter, mirroring the token atoms' harness.

  fn drive_str<'inp, O, Em>(
    emitter: Em,
    f: impl for<'c> FnMut(
      &mut InputRef<
        'inp,
        'c,
        SyntacticLexer<'inp, str>,
        ParserContext<'inp, SyntacticLexer<'inp, str>, Em>,
      >,
    ) -> Result<O, TestError>,
    input: &'inp str,
  ) -> Result<O, TestError>
  where
    Em: Emitter<'inp, SyntacticLexer<'inp, str>, Error = TestError>,
  {
    let ctx: ParserContext<'inp, SyntacticLexer<'inp, str>, Em> = ParserContext::new(emitter);
    Parser::with_parser_and_context(f, ctx).parse_str(input)
  }

  fn drive_slice<'inp, O, Em>(
    emitter: Em,
    f: impl for<'c> FnMut(
      &mut InputRef<
        'inp,
        'c,
        SyntacticLexer<'inp, [u8]>,
        ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em>,
      >,
    ) -> Result<O, TestError>,
    input: &'inp [u8],
  ) -> Result<O, TestError>
  where
    Em: Emitter<'inp, SyntacticLexer<'inp, [u8]>, Error = TestError>,
  {
    let ctx: ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em> = ParserContext::new(emitter);
    Parser::with_parser_and_context(f, ctx).parse_slice(input)
  }

  #[cfg(feature = "bytes")]
  fn drive_bytes<'inp, O, Em>(
    emitter: Em,
    f: impl for<'c> FnMut(
      &mut InputRef<
        'inp,
        'c,
        SyntacticLexer<'inp, [u8]>,
        ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em>,
      >,
    ) -> Result<O, TestError>,
    input: &'inp ::bytes::Bytes,
  ) -> Result<O, TestError>
  where
    Em: Emitter<'inp, SyntacticLexer<'inp, [u8]>, Error = TestError>,
  {
    let ctx: ParserContext<'inp, SyntacticLexer<'inp, [u8]>, Em> = ParserContext::new(emitter);
    Parser::with_parser_and_context(f, ctx).parse_bytes(input)
  }

  /// Runs `body` against `src` under `emitter` across every source representation
  /// (`str`, `[u8]`, and `Bytes`), applying `assert` to each `Result`.
  macro_rules! drive_all {
    ($emitter:expr, |$inp:ident| $body:expr, $src:expr, $assert:expr) => {{
      $assert(drive_str($emitter, |$inp: &mut _| $body, $src));
      $assert(drive_slice($emitter, |$inp: &mut _| $body, $src.as_bytes()));
      #[cfg(feature = "bytes")]
      {
        let owned = ::bytes::Bytes::from_static($src.as_bytes());
        $assert(drive_bytes($emitter, |$inp: &mut _| $body, &owned));
      }
    }};
  }

  // Decimal int/float parity with GraphQL — the same slice payload, now wrapped in
  // the radix-tagged `LitInt`/`LitFloat` carrier.

  #[test]
  fn try_int_accepts_decimal() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_int(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_decimal());
        assert_eq!(as_bytes(value.source_ref()), b"42");
        assert_eq!(span, SimpleSpan::new(0, 2));
        Ok::<_, TestError>(())
      },
      "42",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_float_accepts_decimal() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_float(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_decimal());
        assert_eq!(as_bytes(value.source_ref()), b"-2.5");
        assert_eq!(span, SimpleSpan::new(0, 4));
        Ok::<_, TestError>(())
      },
      "-2.5",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  // Non-decimal integer radixes reach `try_int` through the broadened
  // `is_integer_literal` gate, and the `LitInt` payload records which radix.

  #[test]
  fn try_int_accepts_hex() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_int(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_hex());
        assert_eq!(as_bytes(value.source_ref()), b"0xFF");
        assert_eq!(span, SimpleSpan::new(0, 4));
        Ok::<_, TestError>(())
      },
      "0xFF",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_int_accepts_negative_hex() {
    // GraphQLx folds a leading `-` into the number token, so `-0xFF` is one hex int
    // literal rather than a `Minus` operator followed by `0xFF`.
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_int(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_hex());
        assert_eq!(as_bytes(value.source_ref()), b"-0xFF");
        assert_eq!(span, SimpleSpan::new(0, 5));
        Ok::<_, TestError>(())
      },
      "-0xFF",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_int_accepts_octal() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_int(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_octal());
        assert_eq!(as_bytes(value.source_ref()), b"0o755");
        assert_eq!(span, SimpleSpan::new(0, 5));
        Ok::<_, TestError>(())
      },
      "0o755",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_int_accepts_binary() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_int(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_binary());
        assert_eq!(as_bytes(value.source_ref()), b"0b1010");
        assert_eq!(span, SimpleSpan::new(0, 6));
        Ok::<_, TestError>(())
      },
      "0b1010",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_int_accepts_underscore_separators() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_int(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_decimal());
        assert_eq!(as_bytes(value.source_ref()), b"1_000");
        assert_eq!(span, SimpleSpan::new(0, 5));
        Ok::<_, TestError>(())
      },
      "1_000",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  // The regression the `try_float` fix closes: a hex float reports through
  // tokora's `is_hex_float_literal`, which the old `is_float_literal`-only gate
  // excluded, so `try_float` used to decline it. It must now accept.
  #[test]
  fn try_float_accepts_hex_float() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_float(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_hex());
        assert_eq!(as_bytes(value.source_ref()), b"0x1.8p3");
        assert_eq!(span, SimpleSpan::new(0, 7));
        Ok::<_, TestError>(())
      },
      "0x1.8p3",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  // Strings are identical to GraphQL (shared string lexer).

  #[test]
  fn try_inline_str_accepts_inline() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_inline_str(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert_eq!(as_bytes(value.source_ref()), b"\"hi\"");
        assert_eq!(span, SimpleSpan::new(0, 4));
        Ok::<_, TestError>(())
      },
      "\"hi\"",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_block_str_accepts_block() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_block_str(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert_eq!(as_bytes(value.source_ref()), b"\"\"\"block\"\"\"");
        assert_eq!(span, SimpleSpan::new(0, 11));
        Ok::<_, TestError>(())
      },
      "\"\"\"block\"\"\"",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_string_accepts_inline() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_string(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_inline());
        match value {
          StringLiteral::Inline(inline) => assert_eq!(as_bytes(inline.source_ref()), b"\"hi\""),
          StringLiteral::Block(_) => panic!("expected an inline string payload"),
        }
        assert_eq!(span, SimpleSpan::new(0, 4));
        Ok::<_, TestError>(())
      },
      "\"hi\"",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  #[test]
  fn try_string_accepts_block() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let attempt = try_string(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_block());
        match value {
          StringLiteral::Block(block) => {
            assert_eq!(as_bytes(block.source_ref()), b"\"\"\"block\"\"\"")
          }
          StringLiteral::Inline(_) => panic!("expected a block string payload"),
        }
        assert_eq!(span, SimpleSpan::new(0, 11));
        Ok::<_, TestError>(())
      },
      "\"\"\"block\"\"\"",
      |out: Result<(), TestError>| assert!(out.is_ok())
    );
  }

  // Try-forms decline (consuming nothing) on a token that is not their literal.

  #[test]
  fn try_int_declines_on_ident_and_leaves_it() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let declined = try_int(inp)?.is_decline();
        // The declined identifier is untouched, so the committed `ident` atom pulls
        // it straight off the input; its slice and span prove the leftover is the
        // `x` identifier and that the decline consumed nothing.
        let id = ident(inp)?;
        assert_eq!(as_bytes(id.source_ref()), b"x");
        assert_eq!(id.span(), SimpleSpan::new(0, 1));
        Ok::<_, TestError>(declined)
      },
      "x",
      |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
    );
  }

  #[test]
  fn try_float_declines_on_int_and_leaves_it() {
    drive_all!(
      Fatal::<TestError>::new(),
      |inp| {
        let declined = try_float(inp)?.is_decline();
        let attempt = try_int(inp)?;
        assert!(attempt.is_accept());
        let (value, span) = attempt.unwrap_accept();
        assert!(value.is_decimal());
        assert_eq!(as_bytes(value.source_ref()), b"42");
        assert_eq!(span, SimpleSpan::new(0, 2));
        Ok::<_, TestError>(declined)
      },
      "42",
      |out: Result<bool, TestError>| assert!(matches!(out, Ok(true)))
    );
  }
}
