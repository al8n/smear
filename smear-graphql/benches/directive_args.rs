use criterion::*;

const QUERY: &str = include_str!("../tests/fixtures/queries/directive_args.graphql");

fn apollo_parser_parse_query(query: &str) {
  let parser = apollo_parser::Parser::new(query);
  let _tree = parser.parse();
}

fn smear_parser_parse_query(query: &str) {
  use smear_graphql::parser::fast::{ExecutableDocument, ParseStr};

  let _operation = ExecutableDocument::<&str>::parse_str(query).unwrap();
}

fn graphql_parser_parse_query(query: &str) {
  let _document = graphql_parser::parse_query::<&str>(query).unwrap();
}

fn async_graphql_parser_parse_query(query: &str) {
  let _document = async_graphql_parser::parse_query(query).unwrap();
}

fn cynic_parser_parse_query(query: &str) {
  let _document = cynic_parser::parse_executable_document(query).unwrap();
}

fn bench_apollo_parser_directive_args(c: &mut Criterion) {
  c.bench_function("apollo-parser/directive_args", move |b| {
    b.iter(|| apollo_parser_parse_query(QUERY))
  });
}

fn bench_smear_parser_directive_args(c: &mut Criterion) {
  c.bench_function("smear-graphql/directive_args", move |b| {
    b.iter(|| smear_parser_parse_query(QUERY))
  });
}

fn bench_graphql_parser_directive_args(c: &mut Criterion) {
  c.bench_function("graphql-parser/directive_args", move |b| {
    b.iter(|| graphql_parser_parse_query(QUERY))
  });
}

fn bench_async_graphql_parser_directive_args(c: &mut Criterion) {
  c.bench_function("async-graphql-parser/directive_args", move |b| {
    b.iter(|| async_graphql_parser_parse_query(QUERY))
  });
}

fn bench_cynic_parser_directive_args(c: &mut Criterion) {
  c.bench_function("cynic-parser/directive_args", move |b| {
    b.iter(|| cynic_parser_parse_query(QUERY))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_directive_args,
  bench_smear_parser_directive_args,
  bench_graphql_parser_directive_args,
  bench_async_graphql_parser_directive_args,
  bench_cynic_parser_directive_args,
);
criterion_main!(benches);
