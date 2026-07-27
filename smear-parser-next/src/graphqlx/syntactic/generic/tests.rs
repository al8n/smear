//! Focused GraphQLx generic-production tests.

use smear_lexer::graphqlx::syntactic::SyntacticTokenKind;
use tokora::{FatalContext, Parse, Parser, SimpleSpan, utils::cmp::Equivalent};

use super::super::{GraphqlxInput, GraphqlxLexer};
use crate::graphqlx::{
  GraphQLx, ast,
  error::{ErrorData, Expectation, GraphqlxErrors, Unclosed},
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
  Parser::with_parser::<'inp, GraphqlxLexer<'inp, str>, O, GraphqlxErrors<&'inp str>, _, GraphQLx>(
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
  Parser::with_parser::<
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
  Parser::with_parser::<
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
  Parser::with_parser::<
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
    $check(drive_str($parser, $src).expect(concat!("str accept: ", $src)));
    $check(drive_slice($parser, $src.as_bytes()).expect(concat!("slice accept: ", $src)));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $check(drive_bytes_as_slice($parser, &owned).expect(concat!("bytes accept: ", $src)));
    }
  }};
}

macro_rules! reject_all {
  ($parser:expr, $src:expr) => {{
    assert!(
      drive_str(|inp| $parser(inp).map(|_| ()), $src).is_err(),
      "str should reject: {:?}",
      $src
    );
    assert!(
      drive_slice(|inp| $parser(inp).map(|_| ()), $src.as_bytes()).is_err(),
      "slice should reject: {:?}",
      $src
    );
  }};
}

fn assert_unexpected<'inp>(
  parser: impl for<'c> FnMut(
    &mut GraphqlxInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<(), GraphqlxErrors<&'inp str>>,
  input: &'inp str,
  expected: Expectation,
  found: Option<SyntacticTokenKind>,
) {
  let error = drive_str(parser, input)
    .expect_err("input should report a typed parser error")
    .into_iter()
    .next()
    .expect("parser error collection should contain one error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &expected && unexpected.found().copied() == found
  ));
}

#[test]
fn generic_parameters_and_headers_are_source_generic() {
  fn check_definition_param<S: AsRef<[u8]>>(param: ast::DefinitionTypeParam<S>) {
    assert!("T".equivalent(param.name().source()));
    assert!(param.default().is_some());
  }
  accept_all!(
    ast::DefinitionTypeParam::graphqlx,
    "T = String",
    check_definition_param
  );

  fn check_definition_generics<S: AsRef<[u8]>>(generics: ast::DefinitionTypeGenerics<S>) {
    assert_eq!(generics.params().len(), 2);
    assert!("T".equivalent(generics.params()[0].name().source()));
    assert!(generics.params()[0].default().is_some());
    assert!("U".equivalent(generics.params()[1].name().source()));
  }
  accept_all!(
    ast::DefinitionTypeGenerics::graphqlx,
    "<T = String U>",
    check_definition_generics
  );

  fn check_extension_param<S: AsRef<[u8]>>(param: ast::ExtensionTypeParam<S>) {
    assert!("T".equivalent(param.name().source()));
  }
  accept_all!(
    ast::ExtensionTypeParam::graphqlx,
    "T",
    check_extension_param
  );

  fn check_extension_generics<S: AsRef<[u8]>>(generics: ast::ExtensionTypeGenerics<S>) {
    assert_eq!(generics.params().len(), 2);
    assert!("T".equivalent(generics.params()[0].name().source()));
    assert!("U".equivalent(generics.params()[1].name().source()));
  }
  accept_all!(
    ast::ExtensionTypeGenerics::graphqlx,
    "<T U>",
    check_extension_generics
  );

  fn check_executable_generics<S: AsRef<[u8]>>(generics: ast::ExecutableDefinitionTypeGenerics<S>) {
    assert_eq!(generics.params().len(), 2);
    assert!("T".equivalent(generics.params()[0].source()));
    assert!("U".equivalent(generics.params()[1].source()));
  }
  accept_all!(
    ast::ExecutableDefinitionTypeGenerics::graphqlx,
    "<T U>",
    check_executable_generics
  );

  fn check_definition_name<S: AsRef<[u8]>>(name: ast::DefinitionName<S>) {
    assert!("Result".equivalent(name.name().source()));
    assert_eq!(
      name.generics().expect("declared generics").params().len(),
      2
    );
  }
  accept_all!(
    ast::DefinitionName::graphqlx,
    "Result<T = String U>",
    check_definition_name
  );

  fn check_extension_name<S: AsRef<[u8]>>(name: ast::ExtensionName<S>) {
    assert!(name.path().is_fully_qualified());
    assert_eq!(name.path().segments().len(), 2);
    assert_eq!(
      name.generics().expect("extension generics").params().len(),
      2
    );
  }
  accept_all!(
    ast::ExtensionName::graphqlx,
    "::pkg::Result<T U>",
    check_extension_name
  );

  fn check_executable_name<S: AsRef<[u8]>>(name: ast::ExecutableDefinitionName<S>) {
    assert!("resolve".equivalent(name.name().source()));
    assert_eq!(
      name.generics().expect("executable generics").params().len(),
      2
    );
  }
  accept_all!(
    ast::ExecutableDefinitionName::graphqlx,
    "resolve<T U>",
    check_executable_name
  );
}

#[test]
fn type_paths_and_where_constraints_are_source_generic() {
  fn check_type_path<S: AsRef<[u8]>>(path: ast::TypePath<S>) {
    assert!(path.path().is_fully_qualified());
    assert_eq!(path.path().segments().len(), 2);
    assert_eq!(
      path.type_generics().expect("type generics").params().len(),
      2
    );
  }
  accept_all!(
    ast::TypePath::graphqlx,
    "::pkg::Result<String Item>",
    check_type_path
  );

  fn check_predicate<S: AsRef<[u8]>>(predicate: ast::WherePredicate<S>) {
    assert_eq!(predicate.bounds().len(), 2);
    assert_eq!(
      predicate
        .bounded_type()
        .type_generics()
        .expect("bounded type generics")
        .params()
        .len(),
      1
    );
  }
  accept_all!(
    ast::WherePredicate::graphqlx,
    "Result<String>: Node & Serializable",
    check_predicate
  );

  fn check_clause<S: AsRef<[u8]>>(clause: ast::WhereClause<S>) {
    assert_eq!(clause.predicates().len(), 2);
    assert_eq!(
      clause.predicates()[1]
        .bounded_type()
        .type_generics()
        .expect("second predicate generics")
        .params()
        .len(),
      1
    );
  }
  accept_all!(
    ast::WhereClause::graphqlx,
    "where T: Node U<String>: Serializable",
    check_clause
  );
}

#[test]
fn where_tails_preserve_multi_bound_and_predicate_spans() {
  let predicate_source = "T: Node & Serializable";
  let predicate =
    drive_str(ast::WherePredicate::graphqlx, predicate_source).expect("multiple where bounds");
  assert_eq!(predicate.bounds().len(), 2);
  assert_eq!(
    predicate.span(),
    &SimpleSpan::new(0, predicate_source.len())
  );

  let clause_source = "where T: Node & Serializable U: Resource & Named";
  let clause =
    drive_str(ast::WhereClause::graphqlx, clause_source).expect("multiple where predicates");
  assert_eq!(clause.predicates().len(), 2);
  assert_eq!(clause.predicates()[0].bounds().len(), 2);
  assert_eq!(clause.predicates()[1].bounds().len(), 2);
  assert_eq!(clause.span(), &SimpleSpan::new(0, clause_source.len()));

  let trailing_source = "where T: Node &";
  let error = drive_str(ast::WhereClause::graphqlx, trailing_source)
    .expect_err("an ampersand commits to its following bound")
    .into_iter()
    .next()
    .expect("typed error");
  assert_eq!(
    error.span(),
    SimpleSpan::new(trailing_source.len(), trailing_source.len())
  );
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected) if unexpected.found().is_none()
  ));
}

#[test]
fn where_clause_lookahead_leaves_an_ordinary_definition_head_for_the_caller() {
  let (predicate_count, following) = drive_str(
    |inp| {
      let clause = ast::WhereClause::graphqlx(inp)?;
      let following = super::super::name(inp)?;
      Ok((clause.predicates().len(), following))
    },
    "where T: Node U<String>: Serializable type Query",
  )
  .expect("where clause should stop before the next definition");

  assert_eq!(predicate_count, 2);
  assert!("type".equivalent(following.source()));

  let soft_keyword = drive_str(ast::WhereClause::graphqlx, "where type: Node")
    .expect("a soft keyword remains a valid bounded type name");
  assert_eq!(soft_keyword.predicates().len(), 1);
  assert!(
    "type".equivalent(
      soft_keyword.predicates()[0]
        .bounded_type()
        .path()
        .segments()[0]
        .source()
    )
  );

  let nested = (0..33).fold(String::from("T"), |path, _| format!("Wrapper<{path}>"));
  let source = format!("where First: Node {nested}: Serializable type Query");
  let (predicate_count, following) = drive_str(
    |inp| {
      let clause = ast::WhereClause::graphqlx(inp)?;
      let following = super::super::name(inp)?;
      Ok((clause.predicates().len(), following))
    },
    &source,
  )
  .expect("structural lookahead should not cap nested type-path predicates");

  assert_eq!(predicate_count, 2);
  assert!("type".equivalent(following.source()));
}

#[cfg(feature = "bytes")]
#[test]
fn direct_generic_apis_preserve_owned_bytes_payloads() {
  let source = ::bytes::Bytes::from_static(b"Result<T>");
  let name: ast::DefinitionName<::bytes::Bytes> =
    drive_owned_bytes(ast::DefinitionName::graphqlx, &source)
      .expect("owned Bytes generic definition name");
  assert_eq!(
    name.name().source(),
    &::bytes::Bytes::from_static(b"Result")
  );
}

#[test]
fn generic_productions_report_local_errors_and_unclosed_angles() {
  assert_unexpected(
    |inp| ast::DefinitionTypeParam::graphqlx(inp).map(|_| ()),
    ": String",
    Expectation::Name,
    Some(SyntacticTokenKind::Colon),
  );
  assert_unexpected(
    |inp| ast::WherePredicate::graphqlx(inp).map(|_| ()),
    "T Node",
    Expectation::Colon,
    Some(SyntacticTokenKind::Identifier),
  );
  assert_unexpected(
    |inp| ast::WhereClause::graphqlx(inp).map(|_| ()),
    "T: Node",
    Expectation::Keyword("where"),
    Some(SyntacticTokenKind::Identifier),
  );

  let error = drive_str(ast::DefinitionTypeGenerics::graphqlx, "<T")
    .expect_err("unterminated generic list should fail")
    .into_iter()
    .next()
    .expect("unterminated generic list should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::Unclosed(Unclosed::Angle)
  ));

  reject_all!(ast::DefinitionTypeGenerics::graphqlx, "<>");
  reject_all!(ast::ExtensionTypeGenerics::graphqlx, "<>");
  reject_all!(ast::ExecutableDefinitionTypeGenerics::graphqlx, "<>");
}
