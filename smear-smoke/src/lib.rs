//! Consumes `smear` the way a dependent does.
//!
//! One `pub fn` per feature `smear` declares, each naming that feature's flagship surface through
//! the front door, plus the compatibility aliases the crate merge promised to keep. The point is
//! not that these functions are *called* — most of them are never run — it is that they
//! **compile**, across a real `[dependencies]` edge with `default-features = false` and a named
//! feature set. If a path stops resolving, the workspace stops building.
//!
//! # There is no `#[cfg]` in this crate, on purpose
//!
//! A `#[cfg(feature = "…")]` here would let a probe silently compile to nothing, which is the
//! shape of green [#81] is about: the lossless tower was thoroughly tested and untested in the
//! configuration that shipped. Every feature is unconditionally on in `Cargo.toml`, so every
//! probe is unconditionally compiled, and a feature that stops existing fails **resolution** —
//! loudly, before rustc is reached.
//!
//! # The two directions this gate discriminates
//!
//! Both were exercised against a deliberately broken tree before this crate landed, and the
//! observed failures are recorded because a gate nobody has seen fail is a gate nobody has
//! tested.
//!
//! - **A feature added to `smear` and not smoked** fails
//!   `tests::the_smoke_consumes_every_feature` (a `#[cfg(test)]` unit test, so a code span and
//!   not a link), which compares `smear`'s `[features]` keys against the list this crate's own
//!   manifest enables. Adding `zzz-throwaway-probe = []` to
//!   `smear` produced: *smear declares ["zzz-throwaway-probe"] and this crate does not enable
//!   them*.
//! - **A feature removed or renamed in `smear`** fails cargo's own resolution of the dependency
//!   entry, before this file is compiled at all. Deleting `lossless-coverage` produced:
//!   *package `smear-smoke` depends on `smear` with feature `lossless-coverage` but `smear` does
//!   not have that feature*.
//!
//! One caveat on the second direction, found while proving it. Six of the thirteen features —
//! `bytes`, `bstr`, `hipstr`, `smol-bytes`, `smallvec`, `rowan` — share a name with an OPTIONAL
//! DEPENDENCY, so deleting the explicit `[features]` line does not remove the name: cargo falls
//! back to the implicit feature an optional dependency creates, resolution succeeds, and only the
//! `tokora/<backing>` half is silently lost. Those six still red here — the probe below needs the
//! surface that half enables, so deleting `smol-bytes = [...]` broke compilation of
//! [`smol_bytes_source`] — but they red as a compile error rather than as a resolution error, and
//! they red BECAUSE a probe names the surface. That is the case for writing probes rather than
//! trusting the dependency edge alone.
//!
//! [#81]: https://github.com/al8n/smear/issues/81

use smear::lexer::tokora::{Lexer as _, Parse as _, Parser};

/// `std` — the OS tier. Off, the crate is `no_std` and `thiserror` builds without its `std`
/// feature, so the crate's `#[derive(thiserror::Error)]` types lose their
/// `std::error::Error` implementation.
///
/// Written as a bound rather than a call so the probe IS the trait implementation.
pub fn std_tier() -> &'static str {
  fn assert_error<E: std::error::Error>() -> &'static str {
    core::any::type_name::<E>()
  }
  assert_error::<smear::lexer::error::LengthError>()
}

/// `graphql` — the GraphQL dialect in the lexer: a syntactic lex driven to exhaustion.
pub fn graphql_lexer(src: &str) -> usize {
  let mut lexer = smear::lexer::graphql::syntactic::SyntacticLexer::<str>::new(src);
  let mut tokens = 0usize;
  while lexer.lex().is_some() {
    tokens += 1;
  }
  tokens
}

/// `graphqlx` — the GraphQLx dialect in the lexer: the extended token set, over the same driver.
pub fn graphqlx_lexer(src: &str) -> usize {
  let mut lexer = smear::lexer::graphqlx::syntactic::SyntacticLexer::<str>::new(src);
  let mut tokens = 0usize;
  while lexer.lex().is_some() {
    tokens += 1;
  }
  tokens
}

/// `parser` — the parser tower: a GraphQL executable document, parsed to an AST.
///
/// This is the probe the `parser` gate exists for. With the feature off, `smear::parser` is not a
/// module and this function does not resolve.
#[allow(clippy::type_complexity)]
pub fn graphql_parser(
  src: &str,
) -> Result<
  smear::parser::graphql::ast::ExecutableDocument<&str>,
  smear::parser::graphql::error::GraphqlErrors<&str>,
> {
  use smear::parser::graphql::{
    GraphQL, ast::ExecutableDocument, error::GraphqlErrors, syntactic::GraphqlLexer,
  };

  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(ExecutableDocument::<&str>::graphql)
  .parse_str(src)
}

/// `ErrorData`'s 22 variants, named from outside the crate, plus the wildcard arm
/// `#[non_exhaustive]` requires — which fails **closed**.
///
/// # What this replaced, and why it is not the same probe
///
/// `error_data_is_exhaustively_matchable` stood here and its match had no wildcard, so its
/// property was that `smear::parser::graphql::error::ErrorData` carries no `#[non_exhaustive]`.
/// Only this crate could hold that property: the attribute binds every crate except the one
/// declaring the enum, so an in-crate wildcard-free match — `smear-parser`'s own
/// `error_data_variant_census` — stays green whether or not the attribute is there, and this crate
/// is the one it binds.
///
/// That property is now deliberately false. `ErrorData`'s own doc argues it: the variant list is
/// smear's rather than the specification's, so it grows, and every future variant is otherwise a
/// major break. The probe therefore cannot be kept — and it is not simply deleted either, because
/// the thing it was *defending* is real. A wildcard becomes required; being complete and reachable
/// does not stop being worth pinning.
///
/// # What is left, and the two halves it is in
///
/// - **Complete.** 23 of the enum's 25 variants are named below, so a dependent can reach each of
///   those by name and a path that stops resolving still fails here. The arm the attribute forces
///   returns `None` instead of a tag, so a variant nobody listed is a *reported* failure rather
///   than a quiet fall-through into some neighbour's answer. The two that are not named are
///   `NestingLimitExceeded` and `TokenBudgetExhausted`: both are minted by machinery inside
///   `smear-parser` rather than by anything a dependent calls, and this list has never covered
///   them. **The completeness claim is `smear-parser`'s `error_data_variant_census`**, which is
///   wildcard-free over all 25; what lives out here is that the ones a dependent names still
///   resolve and still have a public producer.
/// - **Reachable.** [`error_data_variant_samples`] builds one value per name below through a
///   public constructor, from outside the crate, and the test pairs each against the tag it must
///   produce. The old probe never had this half: it proved the names *resolve*, not that a
///   dependent can *produce* what they name.
///
/// # The compile-time notice, and where it went
///
/// It is gone from here and cannot be brought back from outside `smear-parser`. A further
/// variant now compiles against this function, lands on the wildcard, and is caught at run time by
/// `tests::the_error_data_variant_set_is_complete_and_reachable_from_outside_the_crate` — a
/// `#[cfg(test)]` unit test, so a code span and not a link — only once somebody adds a sample for
/// it. The build-time half lives in `smear-parser` now, at `error_data_variant_census`, whose match
/// is wildcard-free in every configuration for exactly this reason — the same relocation pql made
/// for `PqlError`, whose notice moved to `PqlError::span`.
///
/// # Planted, because a gate nobody has seen fail is a gate nobody has tested
///
/// Deleting the `UnknownOperationType` arm below — which the attribute makes compile — reddens
/// that test with *the sample built for `UnknownOperationType` did not tag as
/// `UnknownOperationType`; a `None` here is the wildcard arm*. That is the decay the fail-closed
/// arm exists to refuse, and it is the whole of what this probe can still see.
pub fn error_data_variant_tag(
  error: &smear::parser::graphql::error::GraphqlError<&str>,
) -> Option<&'static str> {
  use smear::parser::graphql::error::ErrorData;

  Some(match error.data() {
    ErrorData::Lexer(_) => "Lexer",
    ErrorData::IntOverflow(_) => "IntOverflow",
    ErrorData::FloatOverflow(_) => "FloatOverflow",
    ErrorData::InvalidEnumValue(_) => "InvalidEnumValue",
    ErrorData::InvalidBooleanValue(_) => "InvalidBooleanValue",
    ErrorData::InvalidNullValue(_) => "InvalidNullValue",
    ErrorData::InvalidFragmentName(_) => "InvalidFragmentName",
    ErrorData::Unclosed(_) => "Unclosed",
    ErrorData::UnexpectedToken(_) => "UnexpectedToken",
    ErrorData::UnexpectedKeyword(_) => "UnexpectedKeyword",
    ErrorData::UnexpectedEndOfVariableValue(_) => "UnexpectedEndOfVariableValue",
    ErrorData::UnexpectedEndOfObjectFieldValue(_) => "UnexpectedEndOfObjectFieldValue",
    ErrorData::UnknownDirectiveLocation(_) => "UnknownDirectiveLocation",
    ErrorData::UnknownOperationType(_) => "UnknownOperationType",
    ErrorData::UnexpectedEndOfObjectExtension(_) => "UnexpectedEndOfObjectExtension",
    ErrorData::UnexpectedEndOfInterfaceExtension(_) => "UnexpectedEndOfInterfaceExtension",
    ErrorData::UnexpectedEndOfEnumExtension(_) => "UnexpectedEndOfEnumExtension",
    ErrorData::UnexpectedEndOfInputObjectExtension(_) => "UnexpectedEndOfInputObjectExtension",
    ErrorData::UnexpectedEndOfUnionExtension(_) => "UnexpectedEndOfUnionExtension",
    ErrorData::UnexpectedEndOfSchemaExtension(_) => "UnexpectedEndOfSchemaExtension",
    ErrorData::EndOfInput => "EndOfInput",
    ErrorData::TerminalEndOfInput => "TerminalEndOfInput",
    ErrorData::Other(_) => "Other",
    // The arm `#[non_exhaustive]` requires, and it must stay a `return None`. A tag here would
    // answer for a variant nobody read, on this arm's authority, and leave the run green while
    // saying something no one checked.
    _ => return None,
  })
}

/// One sample per `ErrorData` variant, built from outside the crate through the public door, each
/// paired with the name [`error_data_variant_tag`] must answer for it.
///
/// **Built the way a dependent has to build them**, never by writing `ErrorData::Variant(…)`. That
/// is the rule with teeth, and it is `smear-parser`'s own census rule reapplied across the
/// dependency edge: `IntOverflow` and `FloatOverflow` once sat in that enum with no constructor and
/// no construction site anywhere, and a sample naming the variant directly would have reported them
/// green. Here it also proves the constructor is *public* and its path resolves from a dependent,
/// which is the half the probe this replaced did not have.
///
/// The count is in the type, so dropping a sample is `E0308` in this function rather than a quieter
/// number somewhere else. What no length can pin from out here is the *enum's* count — that is the
/// guarantee `#[non_exhaustive]` took, and `smear-parser`'s `error_data_variant_census` is where it
/// now lives.
pub fn error_data_variant_samples() -> [(
  &'static str,
  smear::parser::graphql::error::GraphqlError<&'static str>,
); 23] {
  use smear::{
    lexer::{
      graphql::{error::LexerErrors, syntactic::SyntacticTokenKind},
      tokora::SimpleSpan,
    },
    parser::graphql::error::{
      EnumTypeExtensionHint, Expectation, GraphqlError, GraphqlErrors,
      InputObjectTypeExtensionHint, IntOverflow, IntWidth, InterfaceTypeExtensionHint,
      ObjectFieldValueHint, ObjectTypeExtensionHint, SchemaExtensionHint, UnionTypeExtensionHint,
      VariableValueHint,
    },
  };

  let span = SimpleSpan::new(0, 1);

  [
    (
      "Lexer",
      GraphqlError::from_lexer_errors(LexerErrors::default(), span),
    ),
    (
      "IntOverflow",
      GraphqlError::int_overflow(
        IntOverflow::checked("99999999999999999999999999", IntWidth::I64)
          .expect("a 26-digit literal is outside `i64` at any reading"),
        span,
      ),
    ),
    ("FloatOverflow", GraphqlError::float_overflow("1e400", span)),
    (
      "InvalidEnumValue",
      GraphqlError::invalid_enum_value("x", span),
    ),
    (
      "InvalidBooleanValue",
      GraphqlError::invalid_boolean_value("x", span),
    ),
    (
      "InvalidNullValue",
      GraphqlError::invalid_null_value("x", span),
    ),
    (
      "InvalidFragmentName",
      GraphqlError::invalid_fragment_name("on", span),
    ),
    ("Unclosed", GraphqlError::unclosed_list(span)),
    (
      "UnexpectedToken",
      GraphqlError::unexpected_token(SyntacticTokenKind::Colon, Expectation::Name, span),
    ),
    (
      "UnexpectedKeyword",
      GraphqlError::unexpected_keyword("quary", "query", span),
    ),
    (
      "UnexpectedEndOfVariableValue",
      GraphqlError::unexpected_end_of_variable_value(VariableValueHint::Name, span),
    ),
    (
      "UnexpectedEndOfObjectFieldValue",
      GraphqlError::unexpected_end_of_object_field_value(ObjectFieldValueHint::Name, span),
    ),
    (
      "UnknownDirectiveLocation",
      GraphqlError::unknown_directive_location("NOWHERE", span),
    ),
    (
      "UnknownOperationType",
      GraphqlError::unknown_operation_type("quary", span),
    ),
    (
      "UnexpectedEndOfObjectExtension",
      GraphqlError::unexpected_end_of_object_extension(span, ObjectTypeExtensionHint::Name),
    ),
    (
      "UnexpectedEndOfInterfaceExtension",
      GraphqlError::unexpected_end_of_interface_extension(span, InterfaceTypeExtensionHint::Name),
    ),
    (
      "UnexpectedEndOfEnumExtension",
      GraphqlError::unexpected_end_of_enum_extension(span, EnumTypeExtensionHint::Name),
    ),
    (
      "UnexpectedEndOfInputObjectExtension",
      GraphqlError::unexpected_end_of_input_object_extension(
        span,
        InputObjectTypeExtensionHint::Name,
      ),
    ),
    (
      "UnexpectedEndOfUnionExtension",
      GraphqlError::unexpected_end_of_union_extension(span, UnionTypeExtensionHint::Name),
    ),
    (
      "UnexpectedEndOfSchemaExtension",
      GraphqlError::unexpected_end_of_schema_extension(span, SchemaExtensionHint::Schema),
    ),
    ("EndOfInput", GraphqlError::unexpected_end_of_input(span)),
    // The end of input that stands in for a terminal scanner stop — smear issue #177. A second
    // public constructor beside the first, so a dependent can name the stop apart from the end of
    // input, which is the whole of what the split bought outside the crate.
    (
      "TerminalEndOfInput",
      GraphqlError::terminal_end_of_input(span),
    ),
    // Not a constructor: `Other`'s only producers are the `From` conversions in `smear-parser`'s
    // error glue, and this is the cheapest of them to mint from out here.
    (
      "Other",
      GraphqlErrors::from(LexerErrors::<char, ()>::default())
        .into_iter()
        .next()
        .expect("the lexer-error conversion emits one error"),
    ),
  ]
}

/// `graphqlx` + `parser` — the GraphQLx dialect in the parser, which is where the dialect's
/// imports, generics and namespaced paths actually live.
#[allow(clippy::type_complexity)]
pub fn graphqlx_parser(
  src: &str,
) -> Result<
  smear::parser::graphqlx::ast::Document<&str>,
  smear::parser::graphqlx::error::GraphqlxErrors<&str>,
> {
  use smear::parser::graphqlx::{
    GraphQLx, ast::Document, error::GraphqlxErrors, syntactic::GraphqlxLexer,
  };

  Parser::with_parser::<GraphqlxLexer<'_, str>, Document<&str>, GraphqlxErrors<&str>, _, GraphQLx>(
    Document::<&str>::graphqlx,
  )
  .parse_str(src)
}

/// `validator` — the built-once schema, reached through the dependency edge and built from a
/// parsed SDL document.
///
/// With the feature off, `smear::validator` is not a module and this function does not resolve.
pub fn graphql_schema(
  sdl: &str,
) -> Result<smear::validator::Schema, smear::validator::SchemaErrors> {
  use smear::parser::graphql::{
    GraphQL, ast::TypeSystemDocument, error::GraphqlErrors, syntactic::GraphqlLexer,
  };

  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(smear::parser::graphql::syntactic::type_system_document)
  .parse_str(sdl)
  .expect("the probe's SDL parses");

  smear::validator::Schema::build(&document)
}

/// `validator`, the other half — the source-slice bound, asserted over `tokora`'s whole
/// `Source<usize>` implementor lattice.
///
/// # Why this lives here and not in `smear`'s own tests
///
/// The claim is that `Schema::build`'s `S: AsRef<[u8]>` bound admits **every** slice type a
/// `smear` parse can produce, so no caller has to newtype, narrow, or copy to build a schema.
/// Proving it needs all four source-backing features on at once and all four backing crates
/// nameable — which is this crate's configuration and nothing else's, since it enables every
/// feature unconditionally and forbids `#[cfg]` on principle.
///
/// The assertion runs on `<Src as Source<usize>>::Slice<'_>` itself, the associated type the AST's
/// `S` is instantiated with, and it instantiates the real `Schema::build` at that type rather than
/// restating its bound. So it fails in both directions: a `tokora` change that gives a source a
/// slice type outside `AsRef<[u8]>`, and a `smear` change that narrows the builder's bound.
pub fn validator_source_lattice() {
  use smear::{
    lexer::tokora::Source,
    parser::graphql::ast::TypeSystemDocument,
    validator::{Schema, SchemaErrors},
  };

  fn assert_lattice_member<Src>()
  where
    Src: Source<usize> + ?Sized + 'static,
    for<'a> <Src as Source<usize>>::Slice<'a>: AsRef<[u8]>,
  {
    fn builds<S: AsRef<[u8]>>(document: &TypeSystemDocument<S>) -> Result<Schema, SchemaErrors> {
      Schema::build(document)
    }
    let _ = builds::<<Src as Source<usize>>::Slice<'static>>;
  }

  // Borrowed tier: slices are reborrows, so a diagnostic built from one cannot outlive the
  // request buffer.
  assert_lattice_member::<str>();
  assert_lattice_member::<[u8]>();
  assert_lattice_member::<&'static str>();
  assert_lattice_member::<&'static [u8]>();
  assert_lattice_member::<bstr::BStr>();
  assert_lattice_member::<&'static bstr::BStr>();

  // Refcounted tier: slices own, so a diagnostic may escape the call that produced it.
  assert_lattice_member::<bytes::Bytes>();
  assert_lattice_member::<smol_bytes::shared::Bytes>();
  assert_lattice_member::<smol_bytes::compact::Bytes>();
  assert_lattice_member::<smol_bytes::Utf8Bytes>();
  assert_lattice_member::<smol_bytes::compact::Utf8Bytes>();

  // Three-way tier: inline, borrowed, or shared, decided at runtime but pinned by `'h`.
  assert_lattice_member::<hipstr::HipStr<'static>>();
  assert_lattice_member::<hipstr::HipByt<'static>>();
}

/// `introspection` — the second construction door, reached across the dependency edge.
///
/// With the feature off, `smear::validator::schema::introspection` is not a module,
/// `Schema::from_introspection` is not a method, and this function does not resolve.
///
/// It probes both halves of the door's surface: the schema and the SDL it was built from. The
/// caller gets the two back so the test below can assert they describe the same thing, which is
/// the door's whole claim in one line of a consumer's code.
pub fn graphql_introspection(
  response: &str,
) -> Result<(smear::validator::Schema, String), smear::validator::schema::IntrospectionError> {
  use smear::validator::{Schema, schema::introspection};

  let sdl = introspection::to_sdl(response)?;
  let schema = Schema::from_introspection(response)?;
  Ok((schema, sdl))
}

/// `validator`, the executable half — draft §5 validation reached across the dependency edge.
///
/// Returns how many rules fired. The sink is the caller's, which is the whole point of the seam:
/// this function owns no diagnostic storage and neither does `smear`.
pub fn graphql_validate(sdl: &str, query: &str) -> u32 {
  use smear::{
    parser::graphql::{
      GraphQL,
      ast::ExecutableDocument,
      error::GraphqlErrors,
      syntactic::{GraphqlLexer, executable_document},
    },
    validator::{Budget, Count, Scratch, validate_executable},
  };

  let schema = graphql_schema(sdl).expect("the probe's SDL is a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(query)
  .expect("the probe's query parses");

  let mut scratch = Scratch::new();
  let mut sink = Count::new();
  let _ = validate_executable(
    &schema,
    &document,
    &mut scratch,
    &Budget::default(),
    &mut sink,
  );
  sink.get()
}

/// `validator`, the executable bound — asserted over `tokora`'s whole `Source<usize>` lattice.
///
/// [`validate_executable`] asks for one bound more than `Schema::build` does: `Clone` as well as
/// `AsRef<[u8]>`. That is not a convenience. A `Diagnostic` **owns** the document's own spelling
/// of the name it refused, because the design's escape contract says a diagnostic built from a
/// refcounted source may outlive the call that produced it — and owning the slice means cloning
/// it, which is a reborrow on the borrowed tier and a refcount bump on the rest.
///
/// So the claim to check is that the wider bound still admits every member. The assertion
/// instantiates the real entry point at each `Slice` associated type rather than restating its
/// bound, so it fails in both directions, exactly as its `Schema::build` twin does.
///
/// [`validate_executable`]: smear::validator::validate_executable
pub fn validator_executable_source_lattice() {
  use smear::{
    lexer::tokora::Source,
    parser::graphql::ast::ExecutableDocument,
    validator::{Budget, Ignore, Invalid, Schema, Scratch, validate_executable},
  };

  fn assert_lattice_member<Src>()
  where
    Src: Source<usize> + ?Sized + 'static,
    for<'a> <Src as Source<usize>>::Slice<'a>: AsRef<[u8]> + Clone,
  {
    fn validates<S: AsRef<[u8]> + Clone>(
      schema: &Schema,
      document: &ExecutableDocument<S>,
      scratch: &mut Scratch,
    ) -> Result<(), Invalid> {
      validate_executable(schema, document, scratch, &Budget::default(), &mut Ignore)
    }
    let _ = validates::<<Src as Source<usize>>::Slice<'static>>;
  }

  // Borrowed tier: a diagnostic is pinned to the request buffer and must not escape it.
  assert_lattice_member::<str>();
  assert_lattice_member::<[u8]>();
  assert_lattice_member::<&'static str>();
  assert_lattice_member::<&'static [u8]>();
  assert_lattice_member::<bstr::BStr>();
  assert_lattice_member::<&'static bstr::BStr>();

  // Refcounted tier: a diagnostic may escape, and cloning its subject is a refcount bump.
  assert_lattice_member::<bytes::Bytes>();
  assert_lattice_member::<smol_bytes::shared::Bytes>();
  assert_lattice_member::<smol_bytes::compact::Bytes>();
  assert_lattice_member::<smol_bytes::Utf8Bytes>();
  assert_lattice_member::<smol_bytes::compact::Utf8Bytes>();

  // Three-way tier: inline, borrowed, or shared, decided at runtime but pinned by `'h`.
  assert_lattice_member::<hipstr::HipStr<'static>>();
  assert_lattice_member::<hipstr::HipByt<'static>>();
}

/// `proto` — draft §6 execution, driven end to end across the dependency edge.
///
/// The probe is the `impl Values for Driver` below as much as the execution. `smear::proto`
/// defines no value type, so a dependent has to be able to implement that trait over *its own*
/// representation using nothing but the crate's public surface — no crate-private helper, no
/// sealed supertrait, no type it cannot name. A probe that only called `Executor::new` would
/// compile even if the trait were unimplementable from outside.
///
/// Returns the resolved leaf and how many field errors the response carried.
pub fn graphql_execute(sdl: &str, query: &str, value: Option<&str>) -> (Option<String>, usize) {
  use smear::{
    parser::graphql::{
      GraphQL,
      ast::ExecutableDocument,
      error::GraphqlErrors,
      syntactic::{GraphqlLexer, executable_document},
    },
    proto::{Executor, Leaf, Node, Values},
  };

  /// A dependent's value representation: an owned string, or nothing.
  #[derive(Clone)]
  struct Text(Option<String>);

  struct Driver;

  impl Values for Driver {
    type Value = Text;

    fn is_null(&self, value: &Text) -> bool {
      value.0.is_none()
    }
    fn as_bool(&self, value: &Text) -> Option<bool> {
      value.0.as_deref().map(|text| text == "true")
    }
    fn list_len(&self, _: &Text) -> Option<usize> {
      None
    }
    fn list_item(&mut self, _: &Text, _: usize) -> Text {
      Text(None)
    }
    fn type_name<'a>(&'a self, _: &'a Text) -> Option<&'a str> {
      None
    }
    fn coerce_leaf(&mut self, value: Text, _: Leaf<'_>) -> Option<Text> {
      Some(value)
    }
    fn variable(&mut self, _: &str) -> Option<Text> {
      None
    }
  }

  let schema = graphql_schema(sdl).expect("the probe's SDL is a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(query)
  .expect("the probe's query parses");

  let mut driver = Driver;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut driver, None, Text(Some(String::new())))
    .expect("the probe's operation resolves");
  while let Some(request) = executor.poll_resolve(&mut driver) {
    let id = request.id();
    executor.handle_resolved(&mut driver, id, Text(value.map(str::to_owned)));
  }

  let response = executor.poll_response().expect("nothing is outstanding");
  let errors = response.error_count();
  let leaf = match response.data() {
    Node::Object(mut fields) => match fields.next() {
      Some((_, Node::Leaf(Text(text)))) => text.clone(),
      _ => None,
    },
    _ => None,
  };
  (leaf, errors)
}

/// `smallvec` — the error container is SmallVec-backed rather than `Vec`-backed.
///
/// The coercion is the probe: with the feature off the deref target is a `Vec` and the binding
/// does not type-check.
pub fn smallvec_backed_errors(errors: &smear::lexer::graphql::error::LexerErrors) -> usize {
  let inline: &smallvec::SmallVec<[smear::lexer::graphql::error::LexerError; 1]> = errors;
  inline.len()
}

/// `rowan` — the lossless CST tower, reachable through the crate named `smear` for the first time
/// (#81). A `parse_document` -> [`Parse`] -> `SyntaxNode` round trip.
///
/// [`Parse`]: smear::parser::graphql::lossless::Parse
pub fn lossless_round_trip(src: &str) -> String {
  let parse: smear::parser::graphql::lossless::Parse =
    smear::parser::graphql::lossless::parse_document(src);
  let node: smear::parser::graphql::lossless::SyntaxNode = parse.syntax();
  node.text().to_string()
}

/// `lossless-coverage` — the per-node-kind hit counters the trivia gates measure with.
pub fn lossless_coverage(src: &str) -> u32 {
  use smear::parser::{graphql::kinds::SyntaxKind, lossless::coverage};

  coverage::reset::<SyntaxKind>();
  let _ = smear::parser::graphql::lossless::parse_document(src);
  coverage::hits_of::<SyntaxKind>(SyntaxKind::Document)
}

/// `test-support` — the lossless suites' scaffolding, which is public API in a build that enables
/// the feature and compiled to nothing in one that does not.
pub fn test_support_scaffolding() {
  smear::parser::lossless::test_support::assert_kind_space_is_well_formed::<
    smear::parser::graphql::kinds::SyntaxKind,
  >();
}

/// `materialized-numbers` — the value productions whose `Int` and `Float` leaves carry a number
/// instead of a source slice, reached across the dependency edge **at both widths**.
///
/// Both halves of the feature are named on purpose. `ast::materialized` is the type set, so
/// naming the return type proves the type path resolves; `syntactic::value::materialized::value`
/// is the production, so calling it proves the parser half compiles. With the feature off,
/// neither module exists and this function does not resolve.
///
/// **Generic over `I` rather than fixed at `i64`, which is what the feature actually ships.** One
/// feature gates one tree at two readings of draft §3.5.1, and a door that only ever named the
/// permissive one described half of it: `materialized32` could have been deleted from the crate
/// and this probe would not have noticed. It notices now, and the caller below runs it at `i32`
/// and at `i64`.
///
/// The bound is [`SmokeInt`] — **this crate's own** — because the parser's `MaterializedInt` is
/// not exported and no dependent can write it. Each impl of `SmokeInt` names one width at the
/// production, so this still stops compiling if either width's impl of the parser's trait goes;
/// what it no longer does is name the trait.
pub fn materialized_numbers<I>(src: &str) -> Option<(I, f64)>
where
  I: SmokeInt,
{
  I::materialized_numbers(src)
}

/// **The two widths, named by this crate because `smear` no longer names them.**
///
/// The parser's `MaterializedInt` is imported privately by `syntactic::value::materialized` and
/// not re-exported, so a dependent cannot write `I: MaterializedInt` and a probe cannot be generic
/// over it. It *could* be exported, and it was — for two commits — and that is precisely how a
/// reader escaped: a supertrait's items resolve through a generic subtrait bound, so
/// `fn read<I: MaterializedInt>(b: &[u8]) -> Option<I> { I::parse(b) }` compiled here and answered
/// `Some(7)` to `007`. The bound being unwriteable is the repair, and this trait is what a probe
/// uses instead.
///
/// Every method is the call a dependent writes **at one width**, and the two impls come from one
/// macro body below, so the widths cannot drift apart inside the probe itself.
///
/// # `WIDTH` is restated here, and the restatement is checked rather than trusted
///
/// `MaterializedInt::WIDTH` was the one public item on the parser's trait, and it was what made
/// the width a fact about the payload type rather than an argument somebody supplies. A dependent
/// cannot read it now, so this trait pairs `i32` with `IntWidth::I32` and `i64` with
/// `IntWidth::I64` — the exact restatement the parser's trait exists to avoid.
///
/// It is safe *here* because two properties read the pairing back out of the crate rather than
/// assuming it. `the_width_a_refusal_names_is_the_width_the_call_asked_for` compares the width on
/// a real refusal against the constant below at both widths, and `IntOverflow::checked` is asked
/// at that same constant in [`public_int_readings`], so a pairing that lied would put the door and
/// the production on different widths and empty the intersection. The parser's own
/// `the_widths_the_door_dispatches_on_are_the_widths_the_readers_name` is the in-crate half of the
/// same statement.
pub trait SmokeInt: Copy + core::fmt::Debug + Sized + 'static {
  /// The width a refusal at this payload type has to name.
  const WIDTH: smear::parser::graphql::error::IntWidth;

  /// The `materialized-numbers` flagship probe at this width: a list of an `Int` and a `Float`.
  fn materialized_numbers(src: &str) -> Option<(Self, f64)>;

  /// `materialized::int_value` — a **committed** integer head.
  ///
  /// `Ok(true)` when a payload was produced, `Err(Some(width))` when the literal was refused as an
  /// overflow and the error named that width, `Err(None)` when it was refused for its shape. The
  /// width travels out because it is the only place a dependent can observe which reading refused.
  fn committed_int(literal: &str) -> Result<bool, Option<smear::parser::graphql::error::IntWidth>>;

  /// `materialized::try_int_value` — the same conversion behind a head test that declines without
  /// consuming. `Ok(false)` is the decline.
  fn attempted_int(literal: &str) -> Result<bool, Option<smear::parser::graphql::error::IntWidth>>;

  /// `materialized::value` — the conversion reached as a **leaf inside a composite**, which is how
  /// the other nine width-carrying productions reach it. `Ok(false)` is "it parsed as some other
  /// kind of value".
  fn fused_value(literal: &str) -> Result<bool, Option<smear::parser::graphql::error::IntWidth>>;
}

/// The two impls, from one body.
///
/// A macro and not two hand-written impls: the point of the probe is that the widths are asked the
/// *same* question, and two bodies are two questions that agree today.
macro_rules! smoke_int {
  ($payload:ty, $width:ident) => {
    impl SmokeInt for $payload {
      const WIDTH: smear::parser::graphql::error::IntWidth =
        smear::parser::graphql::error::IntWidth::$width;

      fn materialized_numbers(src: &str) -> Option<($payload, f64)> {
        use smear::parser::graphql::{
          GraphQL,
          ast::materialized::InputValue,
          error::GraphqlErrors,
          syntactic::{GraphqlLexer, value::materialized},
        };

        let parsed: InputValue<&str, $payload> = Parser::with_parser::<
          GraphqlLexer<'_, str>,
          InputValue<&str, $payload>,
          GraphqlErrors<&str>,
          _,
          GraphQL,
        >(materialized::value::<_, _, $payload>)
        .parse_str(src)
        .ok()?;

        match parsed {
          InputValue::List(items) => match items.values() {
            [InputValue::Int(int), InputValue::Float(float)] => {
              Some((*int.source(), *float.source()))
            }
            _ => None,
          },
          _ => None,
        }
      }

      fn committed_int(
        literal: &str,
      ) -> Result<bool, Option<smear::parser::graphql::error::IntWidth>> {
        use smear::parser::graphql::{
          GraphQL,
          ast::IntValue,
          error::GraphqlErrors,
          syntactic::{GraphqlLexer, value::materialized},
        };

        let parsed: Result<IntValue<$payload>, GraphqlErrors<&str>> =
          Parser::with_parser::<
            GraphqlLexer<'_, str>,
            IntValue<$payload>,
            GraphqlErrors<&str>,
            _,
            GraphQL,
          >(materialized::int_value::<_, _, $payload>)
          .parse_str(literal);

        match parsed {
          Ok(_) => Ok(true),
          Err(errors) => Err(refusal_width(errors)),
        }
      }

      fn attempted_int(
        literal: &str,
      ) -> Result<bool, Option<smear::parser::graphql::error::IntWidth>> {
        use smear::{
          lexer::tokora::try_parse_input::ParseAttempt,
          parser::graphql::{
            GraphQL,
            ast::IntValue,
            error::GraphqlErrors,
            syntactic::{GraphqlLexer, value::materialized},
          },
        };

        let parsed: Result<ParseAttempt<IntValue<$payload>>, GraphqlErrors<&str>> =
          Parser::with_parser::<
            GraphqlLexer<'_, str>,
            ParseAttempt<IntValue<$payload>>,
            GraphqlErrors<&str>,
            _,
            GraphQL,
          >(materialized::try_int_value::<_, _, $payload>)
          .parse_str(literal);

        match parsed {
          Ok(ParseAttempt::Accept(_)) => Ok(true),
          Ok(ParseAttempt::Decline) => Ok(false),
          Err(errors) => Err(refusal_width(errors)),
        }
      }

      fn fused_value(
        literal: &str,
      ) -> Result<bool, Option<smear::parser::graphql::error::IntWidth>> {
        use smear::parser::graphql::{
          GraphQL,
          ast::materialized::InputValue,
          error::GraphqlErrors,
          syntactic::{GraphqlLexer, value::materialized},
        };

        let parsed: Result<InputValue<&str, $payload>, GraphqlErrors<&str>> =
          Parser::with_parser::<
            GraphqlLexer<'_, str>,
            InputValue<&str, $payload>,
            GraphqlErrors<&str>,
            _,
            GraphQL,
          >(materialized::value::<_, _, $payload>)
          .parse_str(literal);

        match parsed {
          Ok(InputValue::Int(_)) => Ok(true),
          Ok(_) => Ok(false),
          Err(errors) => Err(refusal_width(errors)),
        }
      }
    }
  };
}

smoke_int!(i32, I32);
smoke_int!(i64, I64);

/// The width an `IntOverflow` in this error set named, or `None` when the refusal was not one.
///
/// Out of crate this is the *only* place the width a production dispatched on is observable, now
/// that `MaterializedInt::WIDTH` is not reachable — which is what makes it the join between the
/// door's runtime width and the production's type-level one.
pub fn refusal_width(
  errors: smear::parser::graphql::error::GraphqlErrors<&str>,
) -> Option<smear::parser::graphql::error::IntWidth> {
  use smear::parser::graphql::error::ErrorData;

  errors.into_iter().find_map(|error| match error.data() {
    ErrorData::IntOverflow(overflow) => Some(overflow.width()),
    _ => None,
  })
}

/// What a public entry point rules **in** about one integer spelling at one width.
///
/// Three meanings and not two, because "refused" is two different facts about a document and the
/// crate reports them as two different errors: a literal past the width is an `IntOverflow` naming
/// the width that refused it, and a spelling draft §2.9.1 has no production for is the lexer's
/// refusal, which names no width at all. A property that collapsed the two could not see the thing
/// it is here for — `007` is converted by one path and refused by another, and "not an overflow"
/// is true of it either way.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntMeaning {
  /// A GraphQL `IntValue` (draft §2.9.1) this width converts.
  Converts,
  /// A GraphQL `IntValue` this width cannot hold — draft §3.5.1's reading refusing a spelling
  /// §2.9.1's grammar admits.
  Overflows,
  /// Not a GraphQL `IntValue` at all. **The lexer's answer**, and the one every path owes a
  /// leading zero.
  NotALiteral,
}

/// Every meaning there is, which is what an intersection over the readers starts from.
pub const INT_MEANINGS: [IntMeaning; 3] = [
  IntMeaning::Converts,
  IntMeaning::Overflows,
  IntMeaning::NotALiteral,
];

/// **Which decision procedure a public entry reaches** when it is asked about an integer literal.
///
/// The list of *entries* has been under-counted three times running — three readers, then a
/// fourth, then a fifth — because it was a hand-kept array of names, and a name is exactly the
/// thing an edit adds without telling anybody. So the census is over the procedures instead:
/// `syntactic::value::materialized`'s twenty-four public productions collapse onto **three** ways
/// of reaching the one conversion, and the two entries outside that module are one each. A
/// production maps to a variant here, [`PRODUCTION_DECIDERS`] is checked against the module's own
/// source, and [`public_int_readings`] exercises one representative per variant that has a
/// reading.
///
/// The three conversion variants are one implementation reached three ways, and that is the point:
/// the plumbing differs — a committed head, a head test that may decline, a leaf inside a
/// composite — so a defect can live in one shape's plumbing while the shared conversion is right.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntDecider {
  /// The lexer's grammar alone. A slice production converts nothing and carries the spelling out
  /// unchanged, so all it decides is whether the bytes are an `IntValue`.
  Grammar,
  /// The conversion behind a **committed** integer head — `materialized::int_value`.
  CommittedConversion,
  /// The conversion behind a head test that declines without consuming —
  /// `materialized::try_int_value`.
  AttemptedConversion,
  /// The conversion reached as a **leaf inside a composite** — `materialized::value` and the nine
  /// other composites that carry the width.
  FusedConversion,
  /// The runtime-width door, `IntOverflow::checked`, which takes an `IntWidth` as a value rather
  /// than reading one off a type.
  CheckedWidth,
  /// The production carries no integer payload at any width, so it has no reading to contribute
  /// and no way to disagree. The float leaf and the ten delegating leaves.
  NoIntegerPayload,
}

/// **Every public production of `syntactic::value::materialized`, and the decider it reaches.**
///
/// Checked against that module's source rather than believed:
/// `every_public_materialised_production_is_classified` parses the file, collects every
/// `materialized_parser!` and `width_free_parser!` invocation, and requires the two sets to be
/// equal in both directions. A production added there and not classified here fails; an entry left
/// here after a production is removed fails too.
///
/// The macro each production is generated by is a second oracle on the classification, and the
/// test uses it: `materialized_parser!` is the arm for the twelve productions whose output carries
/// the width, so one of those cannot be classified [`NoIntegerPayload`](IntDecider::NoIntegerPayload),
/// and a `width_free_parser!` production cannot be classified as a conversion.
pub const PRODUCTION_DECIDERS: &[(&str, IntDecider)] = &[
  // The integer leaf, in both shapes.
  ("int_value", IntDecider::CommittedConversion),
  ("try_int_value", IntDecider::AttemptedConversion),
  // The composites: each reaches the conversion through `Numbers::int` on a leaf.
  ("value", IntDecider::FusedConversion),
  ("const_value", IntDecider::FusedConversion),
  ("list_value", IntDecider::FusedConversion),
  ("const_list_value", IntDecider::FusedConversion),
  ("object_value", IntDecider::FusedConversion),
  ("const_object_value", IntDecider::FusedConversion),
  ("object_field", IntDecider::FusedConversion),
  ("const_object_field", IntDecider::FusedConversion),
  ("try_default_value", IntDecider::FusedConversion),
  ("default_value", IntDecider::FusedConversion),
  // The float leaf is converted and width-free: GraphQL's `Float` is IEEE 754 double precision at
  // every reading of `Int`, so it takes no width and decides nothing about an integer.
  ("float_value", IntDecider::NoIntegerPayload),
  ("try_float_value", IntDecider::NoIntegerPayload),
  // The five leaves materialisation does not touch, in both shapes.
  ("string_value", IntDecider::NoIntegerPayload),
  ("boolean_value", IntDecider::NoIntegerPayload),
  ("null_value", IntDecider::NoIntegerPayload),
  ("enum_value", IntDecider::NoIntegerPayload),
  ("variable_value", IntDecider::NoIntegerPayload),
  ("try_string_value", IntDecider::NoIntegerPayload),
  ("try_boolean_value", IntDecider::NoIntegerPayload),
  ("try_null_value", IntDecider::NoIntegerPayload),
  ("try_enum_value", IntDecider::NoIntegerPayload),
  ("try_variable_value", IntDecider::NoIntegerPayload),
];

/// **One representative of every decider that has a reading**, each answering with the set of
/// meanings it rules in at `I`'s width.
///
/// A *set* rather than a value, because the paths do not all resolve the question to the same
/// grain and a property that made one of them claim more than it knows would be testing the test.
/// `IntOverflow::checked` is told only whether the pair is an overflow: its `Err` is "not an
/// overflow at this width", which a literal that fits and a spelling that is not one both satisfy.
/// The slice production is width-free — it decides whether the bytes are an `IntValue` and carries
/// the spelling unconverted — so it rules out one meaning and never picks between the other two.
/// Only a materialising path resolves all three. Agreement is therefore the **intersection** being
/// non-empty, and what the paths agree *on* is the one meaning left in it.
///
/// The width is the same one throughout: [`SmokeInt::WIDTH`] is what the door is asked at and what
/// each production is instantiated at. That is the join — the door dispatches on a value and the
/// production dispatches on a type — and
/// `the_width_a_refusal_names_is_the_width_the_call_asked_for` is what keeps the two spellings of
/// it from drifting, now that a dependent cannot read `MaterializedInt::WIDTH`.
///
/// # Keyed by decider, because the defect was a path being added
///
/// `MaterializedInt::parse` was public for two commits and answered `Some(7)` to `007`, which every
/// path here refuses. Nothing was wrong with any of them; what was wrong was that another reader
/// shipped and every property in the tree ranged over one path. Naming the paths did not fix that
/// — the next round found a *fifth*, `try_int_value`, missing from the list of names. So the array
/// is one entry per [`IntDecider`], `PRODUCTION_DECIDERS` is checked against the module's source,
/// and `every_decider_with_a_reading_is_exercised` closes the loop between them: a production
/// added upstream must be classified, and a classification nothing exercises fails.
///
/// # What is still hand-kept, plainly
///
/// Two of the five entries — the slice production and `IntOverflow::checked` — live outside
/// `syntactic::value::materialized`, so no source scan reaches them, and neither would a reader
/// published somewhere new: a trait item, as `parse` was, or a door on an error type, as `checked`
/// is. The mechanical half covers the surface that actually grows with the grammar. The rest is
/// judgement, and this paragraph is the honest bound on it rather than a claim that the class is
/// closed.
///
/// Membership is "a public entry that decides something about an integer literal's spelling or its
/// magnitude". The lossless tower is not one of those: it records the lexer's diagnostics on a
/// tree and converts nothing, so it has no reading to contribute and no way to disagree.
pub fn public_int_readings<I>(
  literal: &str,
) -> [(IntDecider, &'static str, &'static [IntMeaning]); 5]
where
  I: SmokeInt,
{
  use smear::parser::graphql::{
    GraphQL,
    ast::IntValue,
    error::{GraphqlErrors, IntOverflow},
    syntactic::GraphqlLexer,
  };

  let slice = {
    let parsed: Result<IntValue<&str>, GraphqlErrors<&str>> = Parser::with_parser::<
      GraphqlLexer<'_, str>,
      IntValue<&str>,
      GraphqlErrors<&str>,
      _,
      GraphQL,
    >(IntValue::<&str>::graphql)
    .parse_str(literal);

    match parsed {
      // It converted nothing, so all it has said is that these bytes are an `IntValue` — which
      // rules out the lexer's refusal and picks between the other two not at all.
      Ok(_) => &[IntMeaning::Converts, IntMeaning::Overflows][..],
      Err(_) => &[IntMeaning::NotALiteral][..],
    }
  };

  let checked = match IntOverflow::checked(literal, I::WIDTH) {
    Ok(_) => &[IntMeaning::Overflows][..],
    Err(_) => &[IntMeaning::Converts, IntMeaning::NotALiteral][..],
  };

  [
    (IntDecider::Grammar, "the slice production", slice),
    (
      IntDecider::CommittedConversion,
      "materialized::int_value",
      conversion_reading(I::committed_int(literal)),
    ),
    (
      IntDecider::AttemptedConversion,
      "materialized::try_int_value",
      conversion_reading(I::attempted_int(literal)),
    ),
    (
      IntDecider::FusedConversion,
      "materialized::value",
      conversion_reading(I::fused_value(literal)),
    ),
    (IntDecider::CheckedWidth, "IntOverflow::checked", checked),
  ]
}

/// The one reading of a materialising outcome, shared by all three shapes.
///
/// One function rather than one per shape, for the same reason the impls come from one macro body:
/// the three plumbings are being compared, so the interpretation of what they returned must not be
/// three interpretations.
fn conversion_reading(
  outcome: Result<bool, Option<smear::parser::graphql::error::IntWidth>>,
) -> &'static [IntMeaning] {
  match outcome {
    Ok(true) => &[IntMeaning::Converts],
    // A decline, or a composite that parsed as some other kind of value. Either way these bytes
    // are not an integer literal this path converted.
    Ok(false) => &[IntMeaning::NotALiteral],
    Err(Some(_)) => &[IntMeaning::Overflows],
    Err(None) => &[IntMeaning::NotALiteral],
  }
}

/// The variant namespace, imported the way a dependent imports it.
///
/// `use ast::InputValue::{Int, String}` and `use ast::ConstInputValue::*` compile against an
/// `enum` and are `E0432` against a **type alias**, which is the reason the two value trees are
/// two enums rather than one carrier at two instantiations. This module *is* the probe: a `use`
/// cannot be written inside a function, so it has to live where imports live.
pub mod value_variant_namespace {
  pub use smear::parser::graphql::ast::{
    ConstInputValue::*,
    InputValue::{Int, List, Object, String},
  };

  /// The materialised tree, which makes the same promise and makes it separately.
  pub mod materialized {
    pub use smear::parser::graphql::ast::materialized::{
      ConstInputValue::*,
      InputValue::{Int, List, Object, String},
    };
  }
}

/// `materialized-numbers`, the other half — the value types' **source compatibility**, written
/// the way a dependent writes them.
///
/// Generalising the value tree's numeric leaves has failure modes that nothing inside `smear` can
/// see, because `smear` contains no caller that writes any of these shapes. Three of them were
/// live in successive drafts of this axis, and an empty diff over `graphql-proto` and
/// `smear-compiler` was taken as evidence they were not — which is a claim about the consumers
/// this workspace happens to have, not about the ones it does not.
///
/// # The list this is written against
///
/// Each draft repaired the surface the last finding named and not the one beside it. So the gate
/// is written against **what source-equivalence for an `enum` consists of**, rather than against
/// the findings:
///
/// | # | property | probed by |
/// |---|---|---|
/// | 1 | nominal identity — the name is an `enum` item, not an alias | [`value_variant_namespace`] |
/// | 2 | variant namespace — `use Enum::{V}` and `use Enum::*` | [`value_variant_namespace`] |
/// | 3 | arity | `arities` |
/// | 4 | parameter positions | the twelve element-type pins |
/// | 5 | inference at variant construction | the six `let`s, unannotated where a variant names the parameter |
/// | 6 | qualified patterns, exhaustive with no wildcard | `exhaustive` |
/// | 7 | not `#[non_exhaustive]` — an out-of-crate exhaustive match compiles | `exhaustive` |
/// | 8 | derive output — `Debug`, `Clone`, `PartialEq`, `IsVariant`, `Unwrap`, `TryUnwrap` | `derives` |
/// | 9 | hand-written impls — `AsSpan`, `IntoSpan` | `derives` |
/// | 10 | `From`, one per variant | `derives` |
/// | 11 | associated items and turbofish reached through the name | `turbofish` |
/// | 12 | a downstream `impl` naming the type | `impl Mine` |
///
/// Two of those twelve failed the draft before this one, and both were the variant namespace.
///
/// Row 7 is a property of the *value* trees rather than a house rule, and the enum next door shows
/// where the line is. `InputValue`'s nine variants are draft §2.9's nine alternatives, so the list
/// is the specification's and a wildcard forced on a consumer would be a wildcard over a closed
/// set. `ErrorData`'s list is smear's, which is why it carries `#[non_exhaustive]` deliberately and
/// is probed by [`error_data_variant_tag`] instead of by an exhaustive match.
///
/// # Why every axis is crossed with the tree, and now with the width
///
/// There are two value trees and each promises all twelve separately, so a probe that read only
/// the slice side would be half a gate. The containers are empty on purpose — the property is the
/// type each expression *has*, and a populated `Vec` would be testing the parser.
///
/// **The materialised tree took a second parameter, so the axes that can see one are crossed with
/// it too.** `I` is the integer width, and the properties it can break are not the ones `S` can:
/// row 3 is about arity, which changed; row 4 is about *positions*, and inserting `I` ahead of
/// `Container` is precisely the reinterpretation an earlier draft of this axis committed on
/// `ast::InputValue` and this probe caught. So the eight container pins below are written at both
/// widths, and each names `Container` in the argument slot it has always been in — if `I` were
/// ever given a default, `List<S, Own<…>>` would bind `Own<…>` to the payload and these lines
/// would stop compiling, which is the check standing in for a defaulted parameter's silence.
/// Rows 1, 2 and 6–12 do not depend on `I` beyond needing it named, and are crossed with it where
/// that is free.
pub fn value_parameters_are_source_compatible(src: &str) -> (usize, usize) {
  use smear::{
    lexer::tokora::SimpleSpan,
    parser::graphql::{
      GraphQL,
      ast::{
        ConstInputValue, ConstList, ConstObject, ConstObjectField, DefaultInputValue, InputValue,
        List, Object, ObjectField, StringValue, materialized,
      },
      error::GraphqlErrors,
      syntactic::GraphqlLexer,
    },
  };

  /// A dependent's own container — the whole reason `Container` is an argument at all.
  struct Own<T>(Vec<T>);

  impl<T> AsRef<[T]> for Own<T> {
    fn as_ref(&self) -> &[T] {
      &self.0
    }
  }

  /// Every value alias that takes no `Container`, named at exactly the parameters it publishes.
  ///
  /// **The slice tuple is the one that must not move.** A payload parameter added to any of those
  /// five *without* a default is `E0107` here; that is the shape an earlier draft of the
  /// materialisation axis had, and this line is where it failed.
  ///
  /// The materialised tuple names two parameters because the tree takes two — `S` and the integer
  /// width — and `IntValue<I>` beside a `FloatValue` that takes none is the arity reading off the
  /// declaration that the width reaches the integer leaf and stops there. A width parameter
  /// silently added to `FloatValue`, or silently removed from `IntValue`, is `E0107` on these
  /// lines.
  ///
  /// The two tuples are the census, so their length is the point rather than a smell.
  #[allow(clippy::type_complexity)]
  fn arities<S, I>(
    _slice: Option<(
      InputValue<S>,
      ConstInputValue<S>,
      ObjectField<S>,
      ConstObjectField<S>,
      DefaultInputValue<S>,
    )>,
    _materialized: Option<(
      materialized::InputValue<S, I>,
      materialized::ConstInputValue<S, I>,
      materialized::ObjectField<S, I>,
      materialized::ConstObjectField<S, I>,
      materialized::DefaultInputValue<S, I>,
      materialized::IntValue<I>,
      materialized::FloatValue,
    )>,
  ) {
  }

  arities::<&str, i32>(None, None);
  arities::<&str, i64>(None, None);

  // ── Inference: a variant built with no annotation, once per enum per alias set ────────────
  let parsed_string = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    StringValue<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(StringValue::<&str>::graphql)
  .parse_str(src)
  .expect("the probe's source is a string literal");

  let mut constructions = 0usize;

  let value = InputValue::String(parsed_string.clone());
  constructions += usize::from(value.is_string());

  let const_value = ConstInputValue::String(parsed_string.clone());
  constructions += usize::from(const_value.is_string());

  // At the materialised tree, `String` mentions neither parameter, so both come from inference:
  // `S` from the argument and `I` from the annotation these two carry. That is the honest shape —
  // a variant that names no payload cannot infer a payload — and it is what a defaulted `I` would
  // have hidden.
  let materialized_value: materialized::InputValue<&str, i64> =
    materialized::InputValue::String(parsed_string.clone());
  constructions += usize::from(materialized_value.is_string());

  let materialized_value_32: materialized::InputValue<&str, i32> =
    materialized::InputValue::String(parsed_string.clone());
  constructions += usize::from(materialized_value_32.is_string());

  let materialized_const_value: materialized::ConstInputValue<&str, i64> =
    materialized::ConstInputValue::String(parsed_string.clone());
  constructions += usize::from(materialized_const_value.is_string());

  let materialized_const_value_32: materialized::ConstInputValue<&str, i32> =
    materialized::ConstInputValue::String(parsed_string);
  constructions += usize::from(materialized_const_value_32.is_string());

  // ── Position: `Container` is the second argument, on both sides ───────────────────────────
  let span = SimpleSpan::new(0, 0);
  let mut positions = 0usize;

  let list = List::<&str, Own<InputValue<&str>>>::new(span, Own(Vec::new()));
  let _: &[InputValue<&str>] = list.values();
  positions += 1;

  let object = Object::<&str, Own<ObjectField<&str>>>::new(span, Own(Vec::new()));
  let _: &[ObjectField<&str>] = object.fields();
  positions += 1;

  let const_list = ConstList::<&str, Own<ConstInputValue<&str>>>::new(span, Own(Vec::new()));
  let _: &[ConstInputValue<&str>] = const_list.values();
  positions += 1;

  let const_object = ConstObject::<&str, Own<ConstObjectField<&str>>>::new(span, Own(Vec::new()));
  let _: &[ConstObjectField<&str>] = const_object.fields();
  positions += 1;

  // `Container` is the argument AFTER the width on the materialised aliases, and these four are
  // written at both widths because that is where a positional mistake shows. `Own<…>` in the
  // third slot is only `Own<…>` while `I` is named in the second; a defaulted `I` would bind it
  // to the payload instead, which is the silent reinterpretation this probe exists to refuse.
  fn materialized_positions<I>(span: SimpleSpan) -> usize {
    let list = materialized::List::<&str, I, Own<materialized::InputValue<&str, I>>>::new(
      span,
      Own(Vec::new()),
    );
    let _: &[materialized::InputValue<&str, I>] = list.values();

    let object = materialized::Object::<&str, I, Own<materialized::ObjectField<&str, I>>>::new(
      span,
      Own(Vec::new()),
    );
    let _: &[materialized::ObjectField<&str, I>] = object.fields();

    let const_list =
      materialized::ConstList::<&str, I, Own<materialized::ConstInputValue<&str, I>>>::new(
        span,
        Own(Vec::new()),
      );
    let _: &[materialized::ConstInputValue<&str, I>] = const_list.values();

    let const_object = materialized::ConstObject::<
      &str,
      I,
      Own<materialized::ConstObjectField<&str, I>>,
    >::new(span, Own(Vec::new()));
    let _: &[materialized::ConstObjectField<&str, I>] = const_object.fields();

    4
  }

  positions += materialized_positions::<i64>(span);
  positions += materialized_positions::<i32>(span);

  // ── Patterns, exhaustiveness, and the absence of `#[non_exhaustive]` ─────────────────────
  //
  // Out of crate, so `#[non_exhaustive]` on either tree would make this `E0004`. Both are
  // wildcard-free: a variant added to one and not the other fails here as well as in the
  // parser's own parity census.
  fn exhaustive(value: &InputValue<&str>) -> u8 {
    match value {
      InputValue::Variable(_) => 0,
      InputValue::Boolean(_) => 1,
      InputValue::String(_) => 2,
      InputValue::Float(_) => 3,
      InputValue::Int(_) => 4,
      InputValue::Enum(_) => 5,
      InputValue::Null(_) => 6,
      InputValue::List(_) => 7,
      InputValue::Object(_) => 8,
    }
  }

  // Generic over the width, so the exhaustiveness claim is about the declaration and not about
  // one instantiation of it.
  fn exhaustive_materialized<I>(value: &materialized::InputValue<&str, I>) -> u8 {
    match value {
      materialized::InputValue::Variable(_) => 0,
      materialized::InputValue::Boolean(_) => 1,
      materialized::InputValue::String(_) => 2,
      materialized::InputValue::Float(_) => 3,
      materialized::InputValue::Int(_) => 4,
      materialized::InputValue::Enum(_) => 5,
      materialized::InputValue::Null(_) => 6,
      materialized::InputValue::List(_) => 7,
      materialized::InputValue::Object(_) => 8,
    }
  }

  // ── Derive output and the two hand-written span impls ────────────────────────────────────
  fn derives<I: Clone + PartialEq + core::fmt::Debug>(
    value: &InputValue<&str>,
    materialized: &materialized::InputValue<&str, I>,
  ) -> bool {
    use smear::lexer::tokora::span::{AsSpan, IntoSpan};

    let _ = format!("{value:?}{materialized:?}");
    let _: &SimpleSpan = value.as_span();
    let _: &SimpleSpan = materialized.as_span();
    let _: SimpleSpan = value.clone().into_span();
    let _: SimpleSpan = materialized.clone().into_span();
    let _ = value.clone().try_unwrap_string().is_ok();
    let _ = materialized.clone().try_unwrap_string().is_ok();
    let _ = value.try_unwrap_string_ref().is_ok() && materialized.is_string();
    value.clone() == *value && materialized.clone() == *materialized
  }

  // ── Turbofish at a variant, on both trees ────────────────────────────────────────────────
  fn turbofish(
    text: StringValue<&'static str>,
  ) -> (
    InputValue<&'static str>,
    materialized::InputValue<&'static str, i64>,
    materialized::InputValue<&'static str, i32>,
  ) {
    (
      InputValue::<&'static str>::String(text.clone()),
      materialized::InputValue::<&'static str, i64>::String(text.clone()),
      materialized::InputValue::<&'static str, i32>::String(text),
    )
  }

  let _ = (
    exhaustive(&value),
    exhaustive_materialized(&materialized_value),
    exhaustive_materialized(&materialized_value_32),
  );
  assert!(derives(&value, &materialized_value));
  assert!(derives(&value, &materialized_value_32));
  let _ = turbofish;

  (constructions, positions)
}

/// A downstream `impl` naming both trees.
///
/// A dependent can only write this if the tree is a type it can name, which an alias to a
/// crate-private carrier would not be. `pub` because the probe is the pair of impls below, and a
/// private trait implemented for a foreign type is dead code rather than a claim.
pub trait ValueDepth {
  /// Nominal — the body is not the point, the two `impl` headers are.
  fn depth(&self) -> usize;
}

impl ValueDepth for smear::parser::graphql::ast::InputValue<&str> {
  fn depth(&self) -> usize {
    1
  }
}

/// Row 12 at the materialised tree, and **one `impl` covering every width**.
///
/// A blanket over `I` is what a downstream can only write if the width is a parameter on one
/// `enum`; against two enums it would have taken two impls, which is the duplication this axis
/// pushed out one crate at a time.
impl<I> ValueDepth for smear::parser::graphql::ast::materialized::InputValue<&str, I> {
  fn depth(&self) -> usize {
    1
  }
}

/// `bytes` — a `bytes::Bytes`-backed source.
pub fn bytes_source() -> Option<smear::lexer::graphql::ContextualKeyword> {
  keyword_of(bytes::Bytes::from_static(b"query"))
}

/// `bstr` — a `bstr::BStr`-backed source.
pub fn bstr_source() -> Option<smear::lexer::graphql::ContextualKeyword> {
  keyword_of(bstr::BStr::new(b"mutation"))
}

/// `hipstr` — a `hipstr::HipStr`-backed source.
pub fn hipstr_source() -> Option<smear::lexer::graphql::ContextualKeyword> {
  keyword_of(hipstr::HipStr::from("subscription"))
}

/// `smol-bytes` — a `smol_bytes::shared::Bytes`-backed source.
pub fn smol_bytes_source() -> Option<smear::lexer::graphql::ContextualKeyword> {
  keyword_of(smol_bytes::shared::Bytes::from_static(b"fragment"))
}

/// Classify an identifier held in whatever source type `S` is.
///
/// The four probes above differ only in that type, which is the whole of what their features
/// gate: each source backing is an `impl` block that exists only under its feature, so a missing
/// one is a trait-resolution failure here.
fn keyword_of<S>(source: S) -> Option<smear::lexer::graphql::ContextualKeyword>
where
  S: AsRef<[u8]>,
  smear::lexer::graphql::syntactic::SyntacticToken<S>:
    smear::lexer::tokora::utils::DowncastRef<smear::lexer::graphql::ContextualKeyword>,
{
  use smear::lexer::tokora::utils::DowncastRef as _;
  smear::lexer::graphql::syntactic::SyntacticToken::Identifier(source).downcast_ref()
}

// ---------------------------------------------------------------------------------------------
// the compatibility aliases the merge promised to keep
// ---------------------------------------------------------------------------------------------

/// `smear::parser::lexer::…` — the nested alias `smear-parser` published as
/// `smear_parser::lexer::…`, preserved by `pub use crate::lexer` in the parser module.
///
/// The `same_type` binding is the assertion: it only compiles if the alias resolves to the very
/// type `smear::lexer` names, rather than to some parallel re-export.
pub fn nested_lexer_alias(src: &str) -> usize {
  let via_alias = smear::parser::lexer::graphql::syntactic::SyntacticLexer::<str>::new(src);
  let same_type: smear::lexer::graphql::syntactic::SyntacticLexer<'_, str> = via_alias;
  same_type.span().len()
}

/// `smear::lexer::tokora` — the nested `tokora` re-export, an existing public path.
pub fn nested_tokora_alias() -> smear::lexer::tokora::SimpleSpan {
  smear::lexer::tokora::SimpleSpan::new(0, 0)
}

/// `smear::ast_node!` — the `#[macro_export]`ed typed-wrapper macro, at the merged crate root
/// rather than under `parser::`, exactly as `macro_export` has always placed it.
pub mod exported_macro {
  use smear::parser::graphql::kinds::SyntaxKind as K;

  smear::ast_node!(
    lang = smear::parser::graphql::kinds::GraphQLLang;
    /// A document, wrapped by the macro from outside the defining crate.
    SmokeDocument => K::Document {
      /// The document's definitions.
      definitions: many SmokeDefinition,
    }
  );

  smear::ast_node!(
    lang = smear::parser::graphql::kinds::GraphQLLang;
    /// A type-system definition.
    SmokeDefinition => K::ObjectTypeDefinition {
      /// The definition's name token.
      name: tok K::Name,
    }
  );
}

#[cfg(test)]
mod tests {
  use std::collections::BTreeSet;

  /// The source-lattice conformance assertion, run as a test so a failure names itself rather
  /// than appearing as "the workspace does not build".
  ///
  /// The body compiles the claim; calling it is what puts it in a test report.
  #[test]
  fn the_validator_admits_every_source_slice_type() {
    super::validator_source_lattice();
  }

  /// The validator, driven end to end across the dependency edge.
  #[test]
  fn the_validator_builds_a_schema_and_refuses_a_non_schema() {
    let schema = super::graphql_schema("type Query { ok: Int }").expect("a schema");
    assert!(schema.type_by_name(b"Query").is_some());
    // Introspection is unconditional, feature or no feature.
    assert!(schema.type_by_name(b"__Schema").is_some());

    let errors = super::graphql_schema("type NotARoot { ok: Int }").expect_err("not a schema");
    assert!(!errors.is_empty());
  }

  /// Draft §6 execution, driven end to end across the dependency edge, over a value type this
  /// crate defines and `smear` has never heard of.
  #[test]
  fn the_executor_completes_a_field_and_propagates_a_null() {
    let (leaf, errors) =
      super::graphql_execute("type Query { ok: String }", "{ ok }", Some("resolved"));
    assert_eq!(leaf.as_deref(), Some("resolved"));
    assert_eq!(errors, 0);

    // Draft §6.4.4 through the dependency edge: a null in a non-null position nulls `data`.
    let (leaf, errors) = super::graphql_execute("type Query { ok: String! }", "{ ok }", None);
    assert_eq!(leaf, None);
    assert_eq!(errors, 1);
  }

  /// The materialised-number productions, driven across the dependency edge.
  ///
  /// Run rather than merely compiled, because the feature's whole claim is about the *payload* a
  /// leaf carries: a probe that only type-checked would pass against a parser that returned
  /// `Default::default()` for both.
  #[test]
  fn the_materialized_numbers_door_carries_the_converted_payloads_at_both_widths() {
    assert_eq!(
      super::materialized_numbers::<i64>("[-7, 1.5e2]"),
      Some((-7_i64, 150.0))
    );
    assert_eq!(
      super::materialized_numbers::<i32>("[-7, 1.5e2]"),
      Some((-7_i32, 150.0))
    );

    // The documented bound: a literal that is valid GraphQL and outside the width is refused
    // here, where the slice parser above accepts it. The literal that separates the two widths is
    // the interesting one — refused at the width draft §3.5.1 specifies and carried at the other,
    // which is the whole reason one feature ships two readings.
    assert_eq!(
      super::materialized_numbers::<i64>("[2147483648, 1.0]"),
      Some((2_147_483_648_i64, 1.0))
    );
    assert_eq!(
      super::materialized_numbers::<i32>("[2147483648, 1.0]"),
      None
    );

    // And past both.
    assert_eq!(
      super::materialized_numbers::<i64>("[99999999999999999999999999, 1.0]"),
      None
    );
    assert_eq!(
      super::materialized_numbers::<i32>("[99999999999999999999999999, 1.0]"),
      None
    );
    assert!(super::graphql_parser("{ f(x: 99999999999999999999999999) }").is_ok());
  }

  /// One spelling, and the meaning **every** public reader has to reach for it at each width.
  struct Spelling {
    literal: &'static str,
    at_i32: super::IntMeaning,
    at_i64: super::IntMeaning,
  }

  /// The corpus the agreement is asked over.
  ///
  /// The leading zeroes are the rows the property exists for, and they are chosen so that no
  /// reader can reach `NotALiteral` by accident: each one is **in range at both widths**, or is
  /// in range at one and past the other, so a range reader has an answer for every one of them
  /// and giving that answer is the failure.
  ///
  /// The rest are the non-vacuity: a corpus of leading zeroes alone is satisfied by three readers
  /// that answer `NotALiteral` to everything, which is a parser that parses nothing.
  const CORPUS: &[Spelling] = {
    use super::IntMeaning::{Converts, NotALiteral, Overflows};

    &[
      // In range at both widths, and not an `IntValue`. `007` is the finding's own row.
      Spelling {
        literal: "007",
        at_i32: NotALiteral,
        at_i64: NotALiteral,
      },
      Spelling {
        literal: "00",
        at_i32: NotALiteral,
        at_i64: NotALiteral,
      },
      Spelling {
        literal: "-007",
        at_i32: NotALiteral,
        at_i64: NotALiteral,
      },
      Spelling {
        literal: "0000000000000000007",
        at_i32: NotALiteral,
        at_i64: NotALiteral,
      },
      Spelling {
        literal: "02147483647",
        at_i32: NotALiteral,
        at_i64: NotALiteral,
      },
      // Overflow-shaped, and still not an `IntValue`: the value is past a width, so a reader that
      // answered by range would answer `Overflows` here and `Converts` above. Both are wrong and
      // they are wrong differently, which is why the corpus carries both shapes.
      Spelling {
        literal: "02147483648",
        at_i32: NotALiteral,
        at_i64: NotALiteral,
      },
      Spelling {
        literal: "-02147483649",
        at_i32: NotALiteral,
        at_i64: NotALiteral,
      },
      Spelling {
        literal: "09223372036854775808",
        at_i32: NotALiteral,
        at_i64: NotALiteral,
      },
      Spelling {
        literal: "-09223372036854775809",
        at_i32: NotALiteral,
        at_i64: NotALiteral,
      },
      // Non-vacuity. A single zero is a leading zero to nobody: draft §2.9.1's `IntegerPart` is
      // `-? (0 | NonZeroDigit Digit*)`, so `0` and `-0` are the shapes the rows above differ from
      // by one byte.
      Spelling {
        literal: "0",
        at_i32: Converts,
        at_i64: Converts,
      },
      Spelling {
        literal: "-0",
        at_i32: Converts,
        at_i64: Converts,
      },
      Spelling {
        literal: "7",
        at_i32: Converts,
        at_i64: Converts,
      },
      Spelling {
        literal: "-2147483648",
        at_i32: Converts,
        at_i64: Converts,
      },
      // The literal the two readings disagree about, and the two past both of them.
      Spelling {
        literal: "2147483648",
        at_i32: Overflows,
        at_i64: Converts,
      },
      Spelling {
        literal: "9223372036854775808",
        at_i32: Overflows,
        at_i64: Overflows,
      },
      Spelling {
        literal: "99999999999999999999999999",
        at_i32: Overflows,
        at_i64: Overflows,
      },
    ]
  };

  /// The meaning the readers agree on for one spelling at `I`'s width, or a panic naming the
  /// reader that broke the agreement.
  fn agreed_meaning<I>(literal: &str) -> super::IntMeaning
  where
    I: super::SmokeInt,
  {
    let readings = super::public_int_readings::<I>(literal);

    let mut admitted: Vec<super::IntMeaning> = super::INT_MEANINGS.to_vec();
    for (_, _, ruled_in) in &readings {
      admitted.retain(|meaning| ruled_in.contains(meaning));
    }

    assert_eq!(
      admitted.len(),
      1,
      "{literal:?} at {}: the public readers admit {admitted:?} between them, and they have to \
       admit exactly one meaning — {readings:?}",
      I::WIDTH,
    );
    admitted[0]
  }

  /// **Every public reader of an integer literal reaches one meaning, at both widths.**
  ///
  /// Stated over the reader list rather than as one assertion per reader, because the defect this
  /// replaces was not a reader being wrong: it was a reader *shipping* — `MaterializedInt` went
  /// public and took its `parse` with it — while the properties in the tree each ranged over
  /// one path. `007` was `Some(7)` to the new door and refused by all three below, and every test
  /// in the workspace stayed green.
  ///
  /// So the quantifier is over [`public_int_readings`](super::public_int_readings), and that array
  /// is one entry per [`IntDecider`](super::IntDecider) rather than one per name — because naming
  /// the paths was the previous repair and the round after it found another name missing.
  ///
  /// It holds one thing beyond the leading zero, and holds it on every row: the slice production
  /// and the materialising ones never disagree about which spellings are `IntValue`s, because a
  /// path ruling the lexer's refusal in where another rules it out leaves the intersection empty.
  /// Materialisation converts a payload; it does not get to move the grammar.
  #[test]
  fn every_public_reader_of_an_int_literal_reaches_one_meaning() {
    use super::IntMeaning::{Converts, NotALiteral, Overflows};

    let (mut converts, mut overflows, mut not_a_literal) = (0usize, 0usize, 0usize);

    for row in CORPUS {
      for (agreed, expected) in [
        (agreed_meaning::<i32>(row.literal), row.at_i32),
        (agreed_meaning::<i64>(row.literal), row.at_i64),
      ] {
        assert_eq!(
          agreed, expected,
          "{:?}: the readers agree, and on the wrong meaning",
          row.literal,
        );
        match agreed {
          Converts => converts += 1,
          Overflows => overflows += 1,
          NotALiteral => not_a_literal += 1,
        }
      }
    }

    // Non-vacuity, per meaning: an intersection that only ever landed on one of the three would
    // satisfy every equality above while ranging over one arm.
    assert!(converts >= 9, "only {converts} rows convert");
    assert!(overflows >= 5, "only {overflows} rows overflow");
    assert!(
      not_a_literal >= 18,
      "only {not_a_literal} rows are refused for shape",
    );
  }

  /// **The width a refusal names is the width the call asked for**, out of crate, at both widths.
  ///
  /// The out-of-crate half of the parser's own
  /// `the_widths_the_door_dispatches_on_are_the_widths_the_readers_name`, and the thing that keeps
  /// [`SmokeInt::WIDTH`](super::SmokeInt::WIDTH) honest. This crate has to restate the
  /// payload-type-to-`IntWidth` pairing now that `MaterializedInt::WIDTH` is unreachable, and a
  /// restatement that nothing checks is the defect the parser's associated constant exists to
  /// prevent. So it is checked: the width below is read back off a real refusal that a real
  /// production raised, and compared with the constant this crate wrote down.
  ///
  /// Before the trait went private this property was implicit — the smoke read `I::WIDTH` off the
  /// parser and a wrong constant emptied the agreement intersection. It is explicit now because
  /// the smoke no longer reads it, and an implicit property whose mechanism has been removed is a
  /// property nothing asserts.
  #[test]
  fn the_width_a_refusal_names_is_the_width_the_call_asked_for() {
    use smear::parser::graphql::error::IntWidth;

    use super::SmokeInt;

    assert_eq!(<i32 as SmokeInt>::WIDTH, IntWidth::I32);
    assert_eq!(<i64 as SmokeInt>::WIDTH, IntWidth::I64);

    // `2147483648` is the literal the two readings disagree about: refused at the width draft
    // §3.5.1 specifies and carried at the permissive one.
    assert_eq!(
      <i32 as SmokeInt>::committed_int("2147483648"),
      Err(Some(<i32 as SmokeInt>::WIDTH))
    );
    assert_eq!(<i64 as SmokeInt>::committed_int("2147483648"), Ok(true));

    // And past both, where each width has to name its own rather than the other's.
    assert_eq!(
      <i32 as SmokeInt>::committed_int("9223372036854775808"),
      Err(Some(<i32 as SmokeInt>::WIDTH))
    );
    assert_eq!(
      <i64 as SmokeInt>::committed_int("9223372036854775808"),
      Err(Some(<i64 as SmokeInt>::WIDTH))
    );

    // The other two shapes reach the same conversion and therefore name the same width.
    assert_eq!(
      <i32 as SmokeInt>::attempted_int("2147483648"),
      Err(Some(IntWidth::I32))
    );
    assert_eq!(
      <i32 as SmokeInt>::fused_value("2147483648"),
      Err(Some(IntWidth::I32))
    );

    // Non-vacuity: the two constants above are different, so the assertions are not several
    // spellings of one width.
    assert_ne!(<i32 as SmokeInt>::WIDTH, <i64 as SmokeInt>::WIDTH);
  }

  /// `syntactic::value::materialized`'s own source, read as text.
  ///
  /// No dependency edge is created by this and none is wanted: `smear-smoke` depends on `smear`
  /// and on nothing else in the workspace, which is the arithmetic the root manifest's header
  /// turns on. `ci/source_census` reads this workspace's sources the same way and for the same
  /// reason.
  const MATERIALIZED_SOURCE: &str =
    include_str!("../../smear-parser/src/graphql/syntactic/value/materialized.rs");

  /// Every public production that module declares, paired with the macro that generated it.
  ///
  /// Parsed rather than grepped: the productions are `macro_rules!` invocations, so a text scan
  /// would be reading a shape `cargo fmt` is free to move, while `syn` reads the item list. The
  /// production's name is taken off the token stream's own rendering for the same reason.
  fn declared_productions() -> Vec<(String, String)> {
    let file =
      syn::parse_file(MATERIALIZED_SOURCE).expect("materialized.rs did not parse as Rust source");

    file
      .items
      .iter()
      .filter_map(|item| {
        let syn::Item::Macro(item) = item else {
          return None;
        };
        let generator = item.mac.path.segments.last()?.ident.to_string();
        if generator != "materialized_parser" && generator != "width_free_parser" {
          return None;
        }
        let tokens = item.mac.tokens.to_string();
        let production = tokens.split(',').next()?.trim().to_owned();
        Some((production, generator))
      })
      .collect()
  }

  /// **Every public production of the materialised module is classified, and classified
  /// consistently with the macro that generated it.**
  ///
  /// This is the repair for a list that was under-counted three times. The previous shape was an
  /// array of reader *names* in this file, and each round found a name missing from it — three,
  /// then four, then five — because adding a production upstream is not an edit anybody makes
  /// here. Now the upstream module's own source is the census: a production that is not in
  /// [`PRODUCTION_DECIDERS`](super::PRODUCTION_DECIDERS) fails, and so does an entry here for a
  /// production that no longer exists.
  ///
  /// The macro is a second oracle. `materialized_parser!` generates exactly the productions whose
  /// output type carries the width, so one of those classified `NoIntegerPayload` is a
  /// misclassification the source can catch, and a `width_free_parser!` production classified as a
  /// conversion is the mirror of it.
  #[test]
  fn every_public_materialised_production_is_classified() {
    use super::IntDecider;

    let declared = declared_productions();

    // Non-vacuity first. A scan that read nothing would satisfy every set comparison below by
    // being empty on one side, which is the failure mode this whole crate exists to refuse.
    assert!(
      declared.len() >= 24,
      "read only {} productions out of materialized.rs; the scan is wrong, not the module",
      declared.len()
    );

    let classified: BTreeSet<&str> = super::PRODUCTION_DECIDERS
      .iter()
      .map(|(name, _)| *name)
      .collect();
    assert_eq!(
      classified.len(),
      super::PRODUCTION_DECIDERS.len(),
      "two entries in PRODUCTION_DECIDERS claim the same production, so one production is \
       unclassified and the length hides it"
    );

    let scanned: BTreeSet<&str> = declared.iter().map(|(name, _)| name.as_str()).collect();

    let unclassified: Vec<&&str> = scanned.difference(&classified).collect();
    assert!(
      unclassified.is_empty(),
      "materialized.rs declares {unclassified:?} and PRODUCTION_DECIDERS does not classify them, \
       so nothing says which decision procedure they reach or whether it is exercised — add them \
       with the decider they delegate to"
    );

    let stale: Vec<&&str> = classified.difference(&scanned).collect();
    assert!(
      stale.is_empty(),
      "PRODUCTION_DECIDERS classifies {stale:?}, which materialized.rs no longer declares"
    );

    // The macro that generated each production is the second oracle on its classification.
    let mut width_carrying = 0usize;
    for (production, generator) in &declared {
      let decider = super::PRODUCTION_DECIDERS
        .iter()
        .find_map(|(name, decider)| (name == production).then_some(*decider))
        .expect("the set comparison above proves this entry exists");

      match generator.as_str() {
        "materialized_parser" => {
          width_carrying += 1;
          assert_ne!(
            decider,
            IntDecider::NoIntegerPayload,
            "`{production}` is generated by `materialized_parser!`, so its output carries the \
             width and it cannot decide nothing about an integer"
          );
        }
        "width_free_parser" => assert_eq!(
          decider,
          IntDecider::NoIntegerPayload,
          "`{production}` is generated by `width_free_parser!`, so its output is the same type at \
           every width and it has no integer payload to decide about"
        ),
        other => panic!("the scan admitted a macro it does not know: {other}"),
      }
    }

    assert_eq!(
      width_carrying, 12,
      "the module documents twelve width-carrying productions and the scan found \
       {width_carrying}; whichever moved, the split between the two macros is the statement about \
       where the width stops"
    );
  }

  /// **Every decider that has a reading is exercised**, so classifying a production is what puts
  /// it under the agreement property.
  ///
  /// The other half of the closure. `every_public_materialised_production_is_classified` makes a
  /// new production pick a decider; this makes that decider have a representative in
  /// [`public_int_readings`](super::public_int_readings). A production in a genuinely new class
  /// therefore fails here rather than being silently unread.
  #[test]
  fn every_decider_with_a_reading_is_exercised() {
    use super::IntDecider;

    let readings = super::public_int_readings::<i32>("7");
    let exercised: BTreeSet<IntDecider> = readings.iter().map(|(decider, _, _)| *decider).collect();

    // Against the array's own length rather than a written-down count. A literal here fired first
    // when the reading for a decider was deleted, and reported a duplicate instead of naming the
    // decider that had gone unread — found by planting exactly that deletion.
    assert_eq!(
      exercised.len(),
      readings.len(),
      "two entries of `public_int_readings` claim the same decider, so one reading is a duplicate \
       and some decider is unexercised"
    );

    for (production, decider) in super::PRODUCTION_DECIDERS {
      if *decider == IntDecider::NoIntegerPayload {
        continue;
      }
      assert!(
        exercised.contains(decider),
        "`{production}` is classified {decider:?} and no entry of `public_int_readings` reaches \
         that decider, so nothing asks it what an integer spelling means"
      );
    }

    // The two deciders that come from outside `materialized.rs`, which no source scan reaches and
    // which the loop above therefore cannot require.
    assert!(exercised.contains(&IntDecider::Grammar));
    assert!(exercised.contains(&IntDecider::CheckedWidth));
  }

  /// The twelve source-equivalence properties of the value trees, across the dependency edge.
  ///
  /// The bodies are the gate — none of them compiles if a name stops being an `enum`, loses a
  /// variant, gains `#[non_exhaustive]`, gains a payload parameter, or displaces a `Container`
  /// argument. Calling them is what puts a failure in a test report instead of in "the workspace
  /// does not build", and the two counts are what stop a probe from being deleted quietly.
  #[test]
  fn the_value_types_are_source_equivalent_on_every_axis() {
    // 6 constructions = 2 slice enums + the materialised pair at each of the two widths.
    // 12 container pins = 4 slice aliases + 4 materialised ones at each width. Both counts moved
    // when the width became a parameter, and they moved because the probe started crossing the
    // axes that can see one; update either only after checking which.
    assert_eq!(
      super::value_parameters_are_source_compatible("\"probe\""),
      (6, 12)
    );

    // The namespace probe is a `use`, so it is compiled rather than run; naming one item from
    // each tree is what keeps a broken import from being dead code the compiler tolerates.
    let _: fn(_) -> smear::parser::graphql::ast::InputValue<&'static str> =
      super::value_variant_namespace::Int;
    // The materialised `Int` payload mentions no `S`, so the tree parameters have to come from
    // the annotation. That is materialisation's own consequence and not the alias defect: the
    // slice `Int` above needs none. Both widths, because a variant import that resolved at one
    // and not the other would mean the import had stopped being an `enum` variant.
    let _: fn(_) -> smear::parser::graphql::ast::materialized::InputValue<&'static str, i64> =
      super::value_variant_namespace::materialized::Int;
    let _: fn(_) -> smear::parser::graphql::ast::materialized::InputValue<&'static str, i32> =
      super::value_variant_namespace::materialized::Int;
    let _ = <smear::parser::graphql::ast::InputValue<&str> as super::ValueDepth>::depth;
    let _ = <smear::parser::graphql::ast::materialized::InputValue<&str, i32> as super::ValueDepth>::depth;
  }

  /// `ErrorData`'s variant set, from outside the crate: every variant named, every variant
  /// reachable through a public constructor, and nothing falling through the wildcard arm
  /// `#[non_exhaustive]` forces.
  ///
  /// This replaces `the_error_data_variants_are_exhaustively_matchable_from_outside_the_crate`,
  /// which asserted the attribute was absent — see [`error_data_variant_tag`] for why that
  /// property is now deliberately false and what of it survives.
  ///
  /// The last stanza is the old test's whole body, kept: a *real parse failure* rather than a
  /// constructed value, so at least one arm is proven against "this variant, from this crate, right
  /// now" and not merely against "this type-checks".
  ///
  /// [`error_data_variant_tag`]: super::error_data_variant_tag
  #[test]
  fn the_error_data_variant_set_is_complete_and_reachable_from_outside_the_crate() {
    let samples = super::error_data_variant_samples();

    let names: BTreeSet<&str> = samples.iter().map(|(name, _)| *name).collect();
    assert_eq!(
      names.len(),
      samples.len(),
      "two samples claim the same variant name, so one variant is unsampled and the count hides it"
    );

    for (name, error) in &samples {
      assert_eq!(
        super::error_data_variant_tag(error),
        Some(*name),
        "the sample built for `{name}` did not tag as `{name}`; a `None` here is the wildcard arm, \
         which means the variant is no longer named in `error_data_variant_tag`"
      );
    }

    let errors = super::graphql_parser("{ f(").expect_err("a truncated document is a parse error");
    let error = errors.into_iter().next().expect("at least one error");
    assert!(
      super::error_data_variant_tag(&error).is_some(),
      "a parse error this crate produced fell through to the wildcard arm"
    );
  }

  /// The introspection door, driven end to end across the dependency edge.
  #[test]
  fn the_introspection_door_builds_a_schema_and_refuses_a_non_response() {
    const RESPONSE: &str = r#"{"data":{"__schema":{
      "queryType":{"name":"Query"},
      "directives":[],
      "types":[
        {"kind":"OBJECT","name":"Query","fields":[
          {"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}
        ]},
        {"kind":"SCALAR","name":"Int"}
      ]
    }}}"#;

    let (schema, sdl) = super::graphql_introspection(RESPONSE).expect("a schema");
    assert!(schema.type_by_name(b"Query").is_some());
    // Injected by the build, not by the response — the meta-schema is unconditional.
    assert!(schema.type_by_name(b"__Schema").is_some());
    // ... and therefore not re-declared by the door.
    assert!(sdl.contains("type Query"));
    assert!(!sdl.contains("scalar Int"));

    let error = super::graphql_introspection("not json").expect_err("not a response");
    assert!(error.response_kind().is_some());
  }

  /// The executable entry point's bound, over the same lattice, for the same reason.
  #[test]
  fn the_validator_admits_every_source_slice_type_for_documents_too() {
    super::validator_executable_source_lattice();
  }

  /// Draft §5 validation, driven end to end across the dependency edge.
  #[test]
  fn the_validator_accepts_and_refuses_executable_documents() {
    const SDL: &str = "type Query { hero: Character } interface Character { name: String! }";
    assert_eq!(super::graphql_validate(SDL, "{ hero { name } }"), 0);
    assert_eq!(super::graphql_validate(SDL, "{ hero { nickname } }"), 1);
  }

  /// Features named in `smear`'s manifest but deliberately not smoked, each with a written
  /// reason.
  ///
  /// **Empty, and meant to stay that way.** An allowlist is how a census stops being one; the
  /// entry that belongs here is a feature whose surface genuinely cannot be reached from a
  /// dependent, and no such feature exists today.
  const NOT_SMOKED: &[(&str, &str)] = &[];

  /// Every feature `smear` declares is enabled by this crate's dependency on it.
  ///
  /// Both manifests are read at **compile** time, so the test cannot pass by reading a file that
  /// is not the one the build used, and the smoke's own feature list is parsed back out of its
  /// manifest rather than restated here — one source of truth, not two that agree today.
  #[test]
  fn the_smoke_consumes_every_feature() {
    let smear: toml::Value = toml::from_str(include_str!("../../smear/Cargo.toml"))
      .expect("smear/Cargo.toml is not valid TOML");
    let smoke: toml::Value =
      toml::from_str(include_str!("../Cargo.toml")).expect("this crate's Cargo.toml is invalid");

    let declared: BTreeSet<String> = smear
      .get("features")
      .and_then(toml::Value::as_table)
      .expect("smear declares no [features] table, which cannot be right")
      .keys()
      // `default` is a resolution of the others, not a capability of its own.
      .filter(|name| name.as_str() != "default")
      .cloned()
      .collect();

    let enabled: BTreeSet<String> = smoke
      .get("dependencies")
      .and_then(|deps| deps.get("smear"))
      .and_then(|smear| smear.get("features"))
      .and_then(toml::Value::as_array)
      .expect("this crate does not name a feature list on its `smear` dependency")
      .iter()
      .map(|value| {
        value
          .as_str()
          .expect("a feature entry that is not a string")
          .to_owned()
      })
      .collect();

    let excused: BTreeSet<String> = NOT_SMOKED
      .iter()
      .map(|(name, _)| (*name).to_owned())
      .collect();

    // A census over an empty set passes vacuously, which is the failure mode this whole crate is
    // about. Assert it read something first.
    assert!(
      declared.len() >= 13,
      "read only {} features out of smear's manifest; the parse is wrong, not the manifest",
      declared.len()
    );

    let unsmoked: Vec<&String> = declared.difference(&enabled).collect();
    let unsmoked: Vec<&&String> = unsmoked
      .iter()
      .filter(|name| !excused.contains(**name))
      .collect();
    assert!(
      unsmoked.is_empty(),
      "smear declares {unsmoked:?} and this crate does not enable them, so nothing compiles \
       their surface through the dependency edge — add a probe in `lib.rs` and the feature here, \
       or record a written reason in NOT_SMOKED"
    );

    // The other direction. A feature enabled here that smear no longer declares would already
    // have failed cargo's resolution, but a name that moved into `default` or an entry left
    // behind by a rename would not, and either makes the list above stop describing the crate.
    let phantom: Vec<&String> = enabled.difference(&declared).collect();
    assert!(
      phantom.is_empty(),
      "this crate enables {phantom:?}, which smear does not declare as a non-default feature"
    );

    for (name, _) in NOT_SMOKED {
      assert!(
        declared.contains(*name),
        "NOT_SMOKED excuses `{name}`, which smear no longer declares; a stale excuse hides the \
         next real one"
      );
    }
  }
}
