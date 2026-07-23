use tokora::{FatalContext, Parse, Parser, try_parse_input::ParseAttempt};

use super::*;
use crate::graphql::{GraphQL, ast, error::GraphqlErrors};

type StrCtx<'inp> = FatalContext<'inp, GraphqlLexer<'inp, str>, GraphqlErrors<&'inp str>, GraphQL>;
type SliceCtx<'inp> =
  FatalContext<'inp, GraphqlLexer<'inp, [u8]>, GraphqlErrors<&'inp [u8]>, GraphQL>;

fn drive_str<'inp, O>(
  parser: impl for<'input> FnMut(
    &mut GraphqlInput<'inp, 'input, str, StrCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  source: &'inp str,
) -> O {
  Parser::with_parser_of::<'inp, GraphqlLexer<'inp, str>, O, GraphqlErrors<&'inp str>, _, GraphQL>(
    parser,
  )
  .parse_str(source)
  .expect("the str parser should accept the fixture")
}

fn drive_slice<'inp, O>(
  parser: impl for<'input> FnMut(
    &mut GraphqlInput<'inp, 'input, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  source: &'inp [u8],
) -> O {
  Parser::with_parser_of::<
    'inp,
    GraphqlLexer<'inp, [u8]>,
    O,
    GraphqlErrors<&'inp [u8]>,
    _,
    GraphQL,
  >(parser)
  .parse_slice(source)
  .expect("the byte-slice parser should accept the fixture")
}

#[test]
fn direct_ast_parsers_infer_sources_and_return_source_slices() {
  let _: ast::Name<&str> = drive_str(ast::Name::<&str>::graphql, "name");
  let _: ast::Name<&[u8]> = drive_slice(ast::Name::<&[u8]>::graphql, b"name");
  let _: ParseAttempt<ast::Name<&str>> = drive_str(ast::Name::<&str>::try_graphql, "name");
  let _: ParseAttempt<ast::Name<&[u8]>> = drive_slice(ast::Name::<&[u8]>::try_graphql, b"name");

  let _: ast::BooleanValue<&str> = drive_str(ast::BooleanValue::<&str>::graphql, "true");
  let _: ast::BooleanValue<&[u8]> = drive_slice(ast::BooleanValue::<&[u8]>::graphql, b"false");

  let _: ast::InputValue<&str> = drive_str(ast::InputValue::<&str>::graphql, "1");
  let _: ast::InputValue<&[u8]> = drive_slice(ast::InputValue::<&[u8]>::graphql, b"1");

  let _: ast::Type<ast::Name<&str>> = drive_str(ast::Type::<ast::Name<&str>>::graphql, "T");
  let _: ast::Type<ast::Name<&[u8]>> = drive_slice(ast::Type::<ast::Name<&[u8]>>::graphql, b"T");

  let _: ast::Argument<&str> = drive_str(ast::Argument::<&str>::graphql, "x: 1");
  let _: ast::Argument<&[u8]> = drive_slice(ast::Argument::<&[u8]>::graphql, b"x: 1");

  let _: ast::Arguments<&str> = drive_str(ast::Arguments::<&str>::graphql, "(x: 1)");
  let _: ast::Arguments<&[u8]> = drive_slice(ast::Arguments::<&[u8]>::graphql, b"(x: 1)");

  let _: ast::Directive<&str> = drive_str(ast::Directive::<&str>::graphql, "@d");
  let _: ast::Directive<&[u8]> = drive_slice(ast::Directive::<&[u8]>::graphql, b"@d");

  let _: ast::Directives<&str> = drive_str(ast::Directives::<&str>::graphql, "@d");
  let _: ast::Directives<&[u8]> = drive_slice(ast::Directives::<&[u8]>::graphql, b"@d");

  let _: ast::ObjectField<&str> = drive_str(ast::ObjectField::<&str>::graphql, "x: 1");
  let _: ast::ObjectField<&[u8]> = drive_slice(ast::ObjectField::<&[u8]>::graphql, b"x: 1");

  let _: Option<ast::DefaultInputValue<&str>> =
    drive_str(ast::DefaultInputValue::<&str>::graphql, "= 1");
  let _: Option<ast::DefaultInputValue<&[u8]>> =
    drive_slice(ast::DefaultInputValue::<&[u8]>::graphql, b"= 1");
  let _: ParseAttempt<ast::DefaultInputValue<&str>> =
    drive_str(ast::DefaultInputValue::<&str>::try_graphql, "= 1");
  let _: ParseAttempt<ast::DefaultInputValue<&[u8]>> =
    drive_slice(ast::DefaultInputValue::<&[u8]>::try_graphql, b"= 1");
}
