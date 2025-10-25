pub fn apollo_parser_parse_schema(schema: &str) {
  let parser = apollo_parser::Parser::new(schema);
  let _tree = parser.parse();
}

pub fn smear_parser_parse_schema(schema: &str) {
  use smear::parser::graphql::ast::{ParseStr, TypeSystemDocument};

  let _document = TypeSystemDocument::<&str>::parse_str(schema).unwrap();
}

pub fn graphql_parser_parse_schema(schema: &str) {
  let _document = graphql_parser::parse_schema::<&str>(schema).unwrap();
}

pub fn async_graphql_parser_parse_schema(schema: &str) {
  let _document = async_graphql_parser::parse_schema(schema).unwrap();
}

pub fn cynic_parser_parse_schema(schema: &str) {
  let _document = cynic_parser::parse_type_system_document(schema).unwrap();
}

pub fn apollo_parser_parse_query(schema: &str) {
  let parser = apollo_parser::Parser::new(schema);
  let _tree = parser.parse();
}

pub fn smear_parser_parse_query(schema: &str) {
  use smear::parser::graphql::ast::*;

  let _document = ExecutableDefinition::<&str>::parse_str(schema).unwrap();
}

pub fn graphql_parser_parse_query(schema: &str) {
  let _document = graphql_parser::parse_query::<&str>(schema).unwrap();
}

pub fn async_graphql_parser_parse_query(schema: &str) {
  let _document = async_graphql_parser::parse_query(schema).unwrap();
}

pub fn cynic_parser_parse_query(schema: &str) {
  let _document = cynic_parser::parse_executable_document(schema).unwrap();
}
