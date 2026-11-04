# Changelog

All notable changes to Smear are documented in this file.

## [Unreleased]

- `smear-lexer`: string literals in byte sources (`[u8]`, `Bytes`, `BStr`,
  `HipByt`) must be well-formed UTF-8. Ill-formed content is refused with
  `StringError::UnsupportedCharacter`, carrying each sequence's span.
  `LitStr::try_from(&[u8])` refuses likewise. `str` and `HipStr` sources are
  unchanged (#246).
- `smear-lexer`: comments in byte sources (`[u8]`, `Bytes`, `BStr`, `HipByt`)
  must be well-formed UTF-8. Both the syntactic and the lossless lexer refuse an
  ill-formed comment with one `LexerErrorData::UnknownLexeme` per ill-formed
  sequence, at that sequence's span, and consume the whole comment; the lossless
  error token covers the whole comment. `str` and `HipStr` sources are unchanged
  (#251).
- `smear-schema`: `SchemaErrorKind::UnreadableText` is no longer reachable from
  a parsed document. The kind is kept.
- Builds on tokora 0.12 and smol-bytes 0.2. Paths through the
  `smear::lexer::tokora` re-export follow tokora 0.12's layout. The `smol-bytes`
  feature of `smear`, `smear-lexer` and `smear-parser` now enables tokora's
  `smol-bytes_0_2` integration instead of `smol_bytes_0_1`, so the four
  smol-bytes source types are smol-bytes 0.2's. Supersedes #235.
- `smear-compiler`: `Rule::NoSchemaIntrospection`, an opt-in policy rule that
  refuses every `__schema` and `__type` field selection an operation reaches,
  at the field's name. It is the policy graphql-js ships as
  `NoSchemaIntrospectionCustomRule`, matched by name rather than by resolved
  type. It is outside `RuleSet::ALL`, so `validate_executable` still accepts
  introspection; opt in with `RuleSet::ALL.with(Rule::NoSchemaIntrospection)`.
  `__typename` is never refused. Code
  `smear::validation::no-schema-introspection`.
- `smear-lexer`: a backslash that no character follows in an inline string
  literal is refused as `StringError::IncompleteEscape`, at the backslash's
  byte, instead of the positionless `StringError::Other("unknown")`: the input
  ends right after it, or, in a byte source, the bytes after it are not
  well-formed UTF-8. `StringError` is not `#[non_exhaustive]`, so the new
  variant breaks an exhaustive `match` on it.
- `smear-schema`!: the diagnostic contract is tokora's. `smear_schema::diagnostic`,
  and through it `smear::diagnostic`, re-exports `tokora::diagnostic`'s `Diagnose`,
  `DiagnoseExt`, `Code`, `Severity`, `Location`, `Label`, `PathSegment`, `Labels`
  and `PathSegments` instead of declaring copies of them, so `SchemaError`, the
  introspection door's `ResponseError` and `DiagnosticDisplay` implement
  `tokora::diagnostic::Diagnose` and painty's `tokora` adapter renders them.
  Paths, names and method signatures are unchanged. What changed for a caller:
  each item is now tokora's own rather than a structurally identical twin, so a
  type that implemented both `Diagnose` traits now implements one trait twice;
  `Labels` and `PathSegments` no longer implement `ExactSizeIterator` (count them
  with `count()`, not `len()`), report a `size_hint` lower bound of `0`, and stop
  for good at the first index whose accessor answers `None` before the count is
  reached; and `Severity` implements `From<tokora::emitter::Severity>`.
- `smear-compiler`: a draft §5 diagnostic's message no longer ends in its byte
  range. `DiagnosticDisplay`'s `Display` wrote the sentence followed by
  ` (start..end)`; it now writes the sentence alone, so a renderer that draws the
  position from the contract does not print it twice. The position is where it
  already was: `Diagnose::primary()` and the labels, and `Diagnostic::span()` and
  `related_span()`.
- `graphql-proto`: a request error result carries the driver's own request
  errors — the syntax errors, validation failures and variable coercion
  failures raised where no executor exists. `RequestError` is one entry, with
  its message, its §7.1.6 `locations` as spans and its own `extensions`, built
  with `RequestError::new` or from any `Diagnose` with
  `RequestError::from_diagnose`, which takes the diagnostic's rendering as the
  message and its primary and label spans as the `locations`.
  `RequestErrorResult::from_driver` and `RequestErrorResult::push` list them; a
  refused entry comes back in a `RefusedEntry`, naming its
  `RequestErrorCeiling`. Three new `Limits` ceilings bound them:
  `max_request_errors` (128), `max_request_error_message_bytes` (4096) and
  `max_request_error_locations` (64).
- `graphql-proto`: `RequestErrorResult::errors` yields a `RequestErrorEntry`,
  which is the executor's refusal or the driver's entry, rather than a
  `StartError`, and `RequestErrorResult::error` returns `Option<StartError>`,
  `None` for a result built from the driver's entries. Both break code that
  reads them. `Limits` is not `#[non_exhaustive]`, so its three new fields break
  a struct literal that names every field; one ending in `..Limits::default()`
  is unaffected.
- `smear`: `json::write_request_error_result` and
  `json::write_request_error_result_with` take the request's `document: &str`,
  as `write_response` does, and write each driver entry's `locations`, resolved
  against it, and its `extensions`. The new parameter breaks every existing
  call.

## [0.0.1] - 2026-10-30

- Coordinated the first `0.0.1` release of the six publishable crates:
  `smear-lexer`, `smear-parser`, `smear-schema`, `smear-compiler`,
  `graphql-proto`, and `smear`.
