//! Focused GraphQLx executable-production tests.

use smear_lexer::graphqlx::syntactic::SyntacticTokenKind;
use tokora::{FatalContext, Parse, Parser, SimpleSpan, utils::cmp::Equivalent};

use super::{
  executable_document, fragment_definition, operation_definition, operation_type,
  variable_definition, variables_definition,
};
use crate::graphqlx::{
  GraphQLx, ast,
  error::{ErrorData, Expectation, GraphqlxErrors, Unclosed},
  syntactic::{GraphqlxInput, GraphqlxLexer},
};

type StrCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, str>, GraphqlxErrors<&'inp str>, GraphQLx>;
type SliceCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, [u8]>, GraphqlxErrors<&'inp [u8]>, GraphQLx>;

#[cfg(feature = "bytes")]
type BytesCtx<'inp> =
  FatalContext<'inp, GraphqlxLexer<'inp, ::bytes::Bytes>, GraphqlxErrors<::bytes::Bytes>, GraphQLx>;

fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlxErrors<&'inp str>> {
  Parser::with_parser_of::<'inp, GraphqlxLexer<'inp, str>, O, GraphqlxErrors<&'inp str>, _, GraphQLx>(
    f,
  )
  .parse_str(input)
}

fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlxErrors<&'inp [u8]>> {
  Parser::with_parser_of::<
    'inp,
    GraphqlxLexer<'inp, [u8]>,
    O,
    GraphqlxErrors<&'inp [u8]>,
    _,
    GraphQLx,
  >(f)
  .parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes_as_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlxErrors<&'inp [u8]>> {
  Parser::with_parser_of::<
    'inp,
    GraphqlxLexer<'inp, [u8]>,
    O,
    GraphqlxErrors<&'inp [u8]>,
    _,
    GraphQLx,
  >(f)
  .parse_bytes(input)
}

#[cfg(feature = "bytes")]
fn drive_owned_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, ::bytes::Bytes, BytesCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<::bytes::Bytes>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlxErrors<::bytes::Bytes>> {
  Parser::with_parser_of::<
    'inp,
    GraphqlxLexer<'inp, ::bytes::Bytes>,
    O,
    GraphqlxErrors<::bytes::Bytes>,
    _,
    GraphQLx,
  >(f)
  .parse(input)
}

macro_rules! accept_all {
  ($parser:expr, $src:expr, $check:path) => {{
    $check(drive_str($parser, $src).expect(concat!("str accepts: ", $src)));
    $check(drive_slice($parser, $src.as_bytes()).expect(concat!("slice accepts: ", $src)));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $check(drive_bytes_as_slice($parser, &owned).expect(concat!("bytes accepts: ", $src)));
    }
  }};
}

fn assert_unexpected(
  result: Result<(), GraphqlxErrors<&str>>,
  expected: Expectation,
  found: Option<SyntacticTokenKind>,
) {
  let error = result
    .expect_err("fixture should report a typed error")
    .into_iter()
    .next()
    .expect("fatal context reports one error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &expected && unexpected.found().copied() == found
  ));
}

#[test]
fn variables_support_descriptions_defaults_and_constant_directives() {
  fn check_variable<S: AsRef<[u8]>>(variable: ast::DescribedVariableDefinition<S>) {
    assert!(variable.description().is_some());
    assert!("limit".equivalent(variable.variable().name().source()));
    assert!(variable.default_value().is_some());
    assert_eq!(
      variable
        .directives()
        .expect("directives")
        .directives()
        .len(),
      1
    );
  }
  accept_all!(
    ast::DescribedVariableDefinition::graphqlx,
    "\"page size\" $limit: ::paging::Int = 10 @range(min: 1)",
    check_variable
  );

  fn check_variables<S: AsRef<[u8]>>(variables: ast::VariablesDefinition<S>) {
    assert_eq!(variables.variable_definitions().len(), 2);
    assert!(variables.variable_definitions()[1].description().is_some());
  }
  accept_all!(
    ast::VariablesDefinition::graphqlx,
    "($first: Int, \"cursor\" $after: String)",
    check_variables
  );

  let absent = drive_str(ast::VariablesDefinition::graphqlx, "{ id }")
    .expect("an absent variable list is a zero-width optional collection");
  assert!(absent.variable_definitions().is_empty());
  assert_eq!(absent.span(), &SimpleSpan::new(0, 0));
}

#[test]
fn operation_dispatch_supports_shorthand_named_generics_and_constraints() {
  fn check_named<S: AsRef<[u8]>>(operation: ast::OperationDefinition<S>) {
    let named = operation.unwrap_named_ref();
    assert!(named.operation_type().is_query());
    let name = named.name().expect("name");
    assert!("GetItems".equivalent(name.name().source()));
    assert_eq!(
      name.generics().expect("generic declaration").params().len(),
      1
    );
    assert_eq!(
      named
        .variable_definitions()
        .expect("variables")
        .variable_definitions()
        .len(),
      1
    );
    assert_eq!(
      named.directives().expect("directives").directives().len(),
      1
    );
    assert_eq!(
      named
        .selection_set()
        .where_clause()
        .expect("where clause")
        .predicates()
        .len(),
      1
    );
    assert_eq!(named.selection_set().target().selections().len(), 1);
  }
  accept_all!(
    ast::OperationDefinition::graphqlx,
    "query GetItems<T>($limit: Int = 10) @cache where T: Node { items }",
    check_named
  );

  fn check_shorthand<S: AsRef<[u8]>>(operation: ast::OperationDefinition<S>) {
    assert!(operation.is_shorthand());
    assert_eq!(operation.unwrap_shorthand_ref().selections().len(), 1);
  }
  accept_all!(
    ast::OperationDefinition::graphqlx,
    "{ viewer }",
    check_shorthand
  );

  fn check_operation_type(operation: ast::OperationType) {
    assert!(operation.is_subscription());
  }
  accept_all!(
    ast::OperationType::graphqlx,
    "subscription",
    check_operation_type
  );
}

#[test]
fn fragments_preserve_generic_headers_paths_and_where_constraints() {
  fn check<S: AsRef<[u8]>>(fragment: ast::FragmentDefinition<S>) {
    let header = fragment.name();
    assert_eq!(
      header
        .implementation_generics()
        .expect("implementation generics")
        .params()
        .len(),
      1
    );
    assert!("Item".equivalent(header.name().name().source()));
    assert_eq!(
      header
        .name()
        .generics()
        .expect("name generics")
        .params()
        .len(),
      1
    );
    assert!(fragment.type_condition().name().path().is_fully_qualified());
    assert_eq!(
      fragment
        .directives()
        .expect("directives")
        .directives()
        .len(),
      1
    );
    assert_eq!(
      fragment
        .selection_set()
        .where_clause()
        .expect("where clause")
        .predicates()
        .len(),
      1
    );
    assert_eq!(fragment.selection_set().target().selections().len(), 1);
  }
  accept_all!(
    ast::FragmentDefinition::graphqlx,
    "fragment<T> Item<T> on ::model::Item<T> @cache where T: Node { id }",
    check
  );
}

#[test]
fn described_executables_and_documents_dispatch_imports_without_reclassifying_heads() {
  fn check_definition<S: AsRef<[u8]>>(definition: ast::DescribedExecutableDefinition<S>) {
    assert!(definition.description().is_some());
    assert!(definition.node().is_operation());
    assert!(definition.node().unwrap_operation_ref().is_named());
  }
  accept_all!(
    ast::DescribedExecutableDefinition::graphqlx,
    "\"operation docs\" query Named { id }",
    check_definition
  );

  fn check_document<S: AsRef<[u8]>>(document: ast::ExecutableDocument<S>) {
    assert_eq!(document.definitions().len(), 3);
    assert!(document.definitions()[0].is_import());
    assert!(document.definitions()[1].is_definition());
    assert!(document.definitions()[2].is_definition());
    assert!(
      document.definitions()[2]
        .unwrap_definition_ref()
        .node()
        .is_fragment()
    );
  }
  accept_all!(
    ast::ExecutableDocument::graphqlx,
    "import { Node } from \"types\" \"query docs\" query Get<T> { items { ... ::Item<T> } } fragment<T> Item<T> on ::model::Item<T> { id }",
    check_document
  );

  assert!(
    drive_str(
      ast::ExecutableDocument::graphqlx,
      "\"docs\" import { Node } from \"types\""
    )
    .is_err(),
    "a description commits to an executable definition and cannot decorate an import"
  );
}

#[test]
fn executable_document_accepts_graphqlx_executable_fixtures() {
  const FIXTURES: &[&str] = &[
    include_str!(
      "../../../../../smear/tests/fixtures/parser/graphqlx/ok/0016_operation_with_generics.graphqlx"
    ),
    include_str!(
      "../../../../../smear/tests/fixtures/parser/graphqlx/ok/0022_complex_fragments.graphqlx"
    ),
  ];

  for &source in FIXTURES {
    drive_str(ast::ExecutableDocument::graphqlx, source)
      .expect("str accepts executable GraphQLx fixture");
    drive_slice(ast::ExecutableDocument::graphqlx, source.as_bytes())
      .expect("slice accepts executable GraphQLx fixture");
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::copy_from_slice(source.as_bytes());
      drive_owned_bytes(ast::ExecutableDocument::graphqlx, &owned)
        .expect("owned Bytes accepts executable GraphQLx fixture");
    }
  }
}

#[test]
fn executable_productions_commit_required_tails_and_keep_delimiter_diagnostics() {
  assert_unexpected(
    drive_str(|inp| variable_definition(inp).map(|_| ()), "$value Int"),
    Expectation::Colon,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    drive_str(|inp| operation_type(inp).map(|_| ()), "fragment"),
    Expectation::Keyword("query, mutation, or subscription"),
    Some(SyntacticTokenKind::Identifier),
  );
  assert!(
    drive_str(|inp| operation_definition(inp).map(|_| ()), "query Get").is_err(),
    "a named operation requires its selection set"
  );
  assert!(
    drive_str(
      |inp| fragment_definition(inp).map(|_| ()),
      "fragment<T> Item<T> on"
    )
    .is_err(),
    "a fragment type condition requires its type path"
  );

  let unclosed = drive_str(|inp| executable_document(inp).map(|_| ()), "query { id")
    .expect_err("unterminated selection set fails")
    .into_iter()
    .next()
    .expect("fatal context reports one error");
  assert!(matches!(
    unclosed.into_data(),
    ErrorData::Unclosed(Unclosed::Object)
  ));

  assert!(
    drive_str(|inp| variables_definition(inp).map(|_| ()), "()").is_err(),
    "the optional variable collection is nonempty once its opener is present"
  );
}

#[cfg(feature = "bytes")]
#[test]
fn direct_executable_apis_preserve_owned_bytes_payloads() {
  let source = ::bytes::Bytes::from_static(b"query Owned { id }");
  let operation: ast::OperationDefinition<::bytes::Bytes> =
    drive_owned_bytes(ast::OperationDefinition::graphqlx, &source)
      .expect("owned Bytes operation parses without a borrowed fallback");
  assert_eq!(
    operation
      .unwrap_named_ref()
      .name()
      .expect("name")
      .name()
      .source(),
    &::bytes::Bytes::from_static(b"Owned")
  );
}
