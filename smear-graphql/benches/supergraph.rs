use chumsky::{error::Simple, extra, span::SimpleSpan};
use criterion::*;
use smear_parser::parse::ParseStr;

fn apollo_parser_parse_schema(schema: &str) {
  use apollo_parser::cst;

  let parser = apollo_parser::Parser::new(schema);
  let tree = parser.parse();
  let errors = tree.errors().collect::<Vec<_>>();

  if !errors.is_empty() {
    panic!("error parsing query: {errors:?}");
  }

  let document = tree.document();

  for definition in document.definitions() {
    if let cst::Definition::ObjectTypeDefinition(operation) = definition {
      let fields = operation
        .fields_definition()
        .expect("the node FieldsDefinition is not optional in the spec; qed");
      for field in fields.field_definitions() {
        std::hint::black_box(field.ty());
      }
    }
  }
}

fn smear_parser_parse_schema(schema: &str) {
  use smear_graphql::cst;

  let document =
    cst::Document::<SimpleSpan>::parse_str_padded::<extra::Err<Simple<char>>>(schema).unwrap();

  for definition in document.content() {
    if let cst::TypeSystem::Definition(cst::Definition::Type(cst::TypeDefinition::Object(
      operation,
    ))) = definition
    {
      let fields = operation
        .fields_definition()
        .expect("the node FieldsDefinition is not optional in the spec; qed");
      for field in fields.field_definitions() {
        std::hint::black_box(field.ty());
      }
    }
  }
}

fn bench_apollo_parser_supergraph_parser(c: &mut Criterion) {
  let schema = include_str!("testdata/supergraph.graphql");

  c.bench_function("apollo-parser/supergraph_parser", move |b| {
    b.iter(|| apollo_parser_parse_schema(schema))
  });
}

fn bench_smear_parser_supergraph_parser(c: &mut Criterion) {
  let schema = include_str!("testdata/supergraph.graphql");

  c.bench_function("smear-graphql/supergraph_parser", move |b| {
    b.iter(|| smear_parser_parse_schema(schema))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_supergraph_parser,
  bench_smear_parser_supergraph_parser
);
criterion_main!(benches);
