use criterion::*;

const SCHEMA: &str = include_str!("../tests/fixtures/schemas/github_schema.graphql");

fn apollo_parser_parse_schema(schema: &str) {
  let parser = apollo_parser::Parser::new(schema);
  let _tree = parser.parse();
}

fn smear_parser_parse_schema(schema: &str) {
  use smear::parser::graphql::ast::{ParseStr, TypeSystemDocument};

  let _document = TypeSystemDocument::<&str>::parse_str(schema).unwrap();
}

fn graphql_parser_parse_schema(schema: &str) {
  let _document = graphql_parser::parse_schema::<&str>(schema).unwrap();
}

fn async_graphql_parser_parse_schema(schema: &str) {
  let _document = async_graphql_parser::parse_schema(schema).unwrap();
}

fn cynic_parser_parse_schema(schema: &str) {
  let _document = cynic_parser::parse_type_system_document(schema).unwrap();
}

fn bench_apollo_parser_parse_github_schema(c: &mut Criterion) {
  c.bench_function("apollo-parser/parse_github_schema", move |b| {
    b.iter(|| apollo_parser_parse_schema(SCHEMA))
  });
}

fn bench_smear_parser_parse_github_schema(c: &mut Criterion) {
  c.bench_function("smear-graphql/parse_github_schema", move |b| {
    b.iter(|| smear_parser_parse_schema(SCHEMA))
  });
}

fn bench_graphql_parser_parse_github_schema(c: &mut Criterion) {
  c.bench_function("graphql-parser/parse_github_schema", move |b| {
    b.iter(|| graphql_parser_parse_schema(SCHEMA))
  });
}

fn bench_async_graphql_parser_parse_github_schema(c: &mut Criterion) {
  c.bench_function("async-graphql-parser/parse_github_schema", move |b| {
    b.iter(|| async_graphql_parser_parse_schema(SCHEMA))
  });
}

fn bench_cynic_parser_parse_github_schema(c: &mut Criterion) {
  c.bench_function("cynic-parser/parse_github_schema", move |b| {
    b.iter(|| cynic_parser_parse_schema(SCHEMA))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_parse_github_schema,
  bench_smear_parser_parse_github_schema,
  bench_graphql_parser_parse_github_schema,
  bench_async_graphql_parser_parse_github_schema,
  // bench_cynic_parser_parse_github_schema,
);
criterion_main!(benches);
