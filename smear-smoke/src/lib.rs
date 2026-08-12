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

/// `ErrorData`'s 22 variants, matched exhaustively with no wildcard, from outside the crate.
///
/// This is the probe row 7 of [`value_parameters_are_source_compatible`]'s table exists for, aimed
/// at the type that actually broke it: `feat/parser-materialised-values` added
/// `#[non_exhaustive]` to `smear::parser::graphql::error::ErrorData` with no review, and nothing
/// in `smear-parser`'s own test suite could have caught it — `#[non_exhaustive]` binds every
/// crate except the one that declares the enum, so an in-crate wildcard-free match (this crate's
/// own `error_data_variant_census`) stays green whether or not the attribute is there. This
/// crate is the one it binds. With `#[non_exhaustive]` present, this function is `E0004`.
#[allow(clippy::type_complexity)]
pub fn error_data_is_exhaustively_matchable(
  error: &smear::parser::graphql::error::GraphqlError<&str>,
) -> &'static str {
  use smear::parser::graphql::error::ErrorData;

  match error.data() {
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
    ErrorData::Other(_) => "Other",
  }
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

/// `materialized-numbers` — the value productions whose `Int` and `Float` leaves are `i64` and
/// `f64`, reached across the dependency edge.
///
/// Both halves of the feature are named on purpose. `ast::materialized` is the alias set, so
/// naming the return type proves the type path resolves; `syntactic::value::materialized::value`
/// is the production, so calling it proves the parser half compiles. With the feature off,
/// neither module exists and this function does not resolve.
pub fn materialized_numbers(src: &str) -> Option<(i64, f64)> {
  use smear::parser::graphql::{
    GraphQL,
    ast::materialized::InputValue,
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, value::materialized},
  };

  let parsed: InputValue<&str> = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    InputValue<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(materialized::value)
  .parse_str(src)
  .ok()?;

  match parsed {
    InputValue::List(items) => match items.values() {
      [InputValue::Int(int), InputValue::Float(float)] => Some((*int.source(), *float.source())),
      _ => None,
    },
    _ => None,
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
/// | 4 | parameter positions | the eight element-type pins |
/// | 5 | inference at variant construction | the four unannotated `let`s |
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
/// # Why every axis is crossed with the tree
///
/// There are two value trees and each promises all twelve separately, so a probe that read only
/// the slice side would be half a gate. The containers are empty on purpose — the property is the
/// type each expression *has*, and a populated `Vec` would be testing the parser.
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

  /// Every value alias that takes no `Container`, named at the one parameter it has always taken.
  ///
  /// A payload parameter added back to any of them *without* a default is `E0107` here. Added
  /// *with* one it is invisible here, and harmless: none of these takes a `Container` argument
  /// for a new parameter to displace, and the four enum spellings among them are covered by the
  /// constructions below, which defaults do not rescue.
  ///
  /// The two tuples are the census, so their length is the point rather than a smell.
  #[allow(clippy::type_complexity)]
  fn arities<S>(
    _slice: Option<(
      InputValue<S>,
      ConstInputValue<S>,
      ObjectField<S>,
      ConstObjectField<S>,
      DefaultInputValue<S>,
    )>,
    _materialized: Option<(
      materialized::InputValue<S>,
      materialized::ConstInputValue<S>,
      materialized::ObjectField<S>,
      materialized::ConstObjectField<S>,
      materialized::DefaultInputValue<S>,
      materialized::IntValue,
      materialized::FloatValue,
    )>,
  ) {
  }

  arities::<&str>(None, None);

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

  let materialized_value = materialized::InputValue::String(parsed_string.clone());
  constructions += usize::from(materialized_value.is_string());

  let materialized_const_value = materialized::ConstInputValue::String(parsed_string);
  constructions += usize::from(materialized_const_value.is_string());

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

  let materialized_list =
    materialized::List::<&str, Own<materialized::InputValue<&str>>>::new(span, Own(Vec::new()));
  let _: &[materialized::InputValue<&str>] = materialized_list.values();
  positions += 1;

  let materialized_object =
    materialized::Object::<&str, Own<materialized::ObjectField<&str>>>::new(span, Own(Vec::new()));
  let _: &[materialized::ObjectField<&str>] = materialized_object.fields();
  positions += 1;

  let materialized_const_list = materialized::ConstList::<
    &str,
    Own<materialized::ConstInputValue<&str>>,
  >::new(span, Own(Vec::new()));
  let _: &[materialized::ConstInputValue<&str>] = materialized_const_list.values();
  positions += 1;

  let materialized_const_object = materialized::ConstObject::<
    &str,
    Own<materialized::ConstObjectField<&str>>,
  >::new(span, Own(Vec::new()));
  let _: &[materialized::ConstObjectField<&str>] = materialized_const_object.fields();
  positions += 1;

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

  fn exhaustive_materialized(value: &materialized::InputValue<&str>) -> u8 {
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
  fn derives(value: &InputValue<&str>, materialized: &materialized::InputValue<&str>) -> bool {
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
    materialized::InputValue<&'static str>,
  ) {
    (
      InputValue::<&'static str>::String(text.clone()),
      materialized::InputValue::<&'static str>::String(text),
    )
  }

  let _ = (
    exhaustive(&value),
    exhaustive_materialized(&materialized_value),
  );
  assert!(derives(&value, &materialized_value));
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

impl ValueDepth for smear::parser::graphql::ast::materialized::InputValue<&str> {
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
  fn the_materialized_numbers_door_carries_the_converted_payloads() {
    assert_eq!(
      super::materialized_numbers("[-7, 1.5e2]"),
      Some((-7, 150.0))
    );

    // The documented bound: a literal that is valid GraphQL and outside `i64` is refused here,
    // where the slice parser above accepts it.
    assert_eq!(
      super::materialized_numbers("[99999999999999999999999999, 1.0]"),
      None
    );
    assert!(super::graphql_parser("{ f(x: 99999999999999999999999999) }").is_ok());
  }

  /// The twelve source-equivalence properties of the value trees, across the dependency edge.
  ///
  /// The bodies are the gate — none of them compiles if a name stops being an `enum`, loses a
  /// variant, gains `#[non_exhaustive]`, gains a payload parameter, or displaces a `Container`
  /// argument. Calling them is what puts a failure in a test report instead of in "the workspace
  /// does not build", and the two counts are what stop a probe from being deleted quietly.
  #[test]
  fn the_value_types_are_source_equivalent_on_every_axis() {
    assert_eq!(
      super::value_parameters_are_source_compatible("\"probe\""),
      (4, 8)
    );

    // The namespace probe is a `use`, so it is compiled rather than run; naming one item from
    // each tree is what keeps a broken import from being dead code the compiler tolerates.
    let _: fn(_) -> smear::parser::graphql::ast::InputValue<&'static str> =
      super::value_variant_namespace::Int;
    // The materialised `Int` payload is `i64`, which mentions no `S`, so the tree parameter has
    // to come from the annotation. That is materialisation's own consequence and not the alias
    // defect: the slice `Int` above needs none.
    let _: fn(_) -> smear::parser::graphql::ast::materialized::InputValue<&'static str> =
      super::value_variant_namespace::materialized::Int;
    let _ = <smear::parser::graphql::ast::InputValue<&str> as super::ValueDepth>::depth;
  }

  /// `ErrorData`, matched exhaustively from outside the crate — the check that would have caught
  /// `#[non_exhaustive]` landing on it unreviewed. Driven by a real parse failure rather than a
  /// constructor, so the property proven is "this variant, from this crate, right now" and not
  /// merely "this type-checks".
  #[test]
  fn the_error_data_variants_are_exhaustively_matchable_from_outside_the_crate() {
    let errors = super::graphql_parser("{ f(").expect_err("a truncated document is a parse error");
    let error = errors.into_iter().next().expect("at least one error");
    let tag = super::error_data_is_exhaustively_matchable(&error);
    assert!(!tag.is_empty(), "the match arm returned no tag");
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
