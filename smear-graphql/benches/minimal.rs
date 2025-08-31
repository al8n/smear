use chumsky::{error::Simple, extra, span::SimpleSpan};
use criterion::*;
use smear_parser::parse::ParseStr;

const QUERY: &str = include_str!("../tests/fixtures/queries/minimal.graphql");

fn apollo_parser_parse_query(query: &str) {
  let parser = apollo_parser::Parser::new(query);
  let _tree = parser.parse();
}

fn smear_parser_parse_query(query: &str) {
  use smear_graphql::cst;

  let _operation =
    cst::ExecutableDocument::<SimpleSpan>::parse_str_padded::<extra::Err<Simple<char>>>(query)
      .unwrap();
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

fn bench_apollo_parser_minimal(c: &mut Criterion) {
  c.bench_function("apollo-parser/minimal", move |b| {
    b.iter(|| apollo_parser_parse_query(QUERY))
  });
}

fn bench_smear_parser_minimal(c: &mut Criterion) {
  c.bench_function("smear-graphql/minimal", move |b| {
    b.iter(|| smear_parser_parse_query(QUERY))
  });
}

fn bench_graphql_parser_minimal(c: &mut Criterion) {
  c.bench_function("graphql-parser/minimal", move |b| {
    b.iter(|| graphql_parser_parse_query(QUERY))
  });
}

fn bench_async_graphql_parser_minimal(c: &mut Criterion) {
  c.bench_function("async-graphql-parser/minimal", move |b| {
    b.iter(|| async_graphql_parser_parse_query(QUERY))
  });
}

fn bench_cynic_parser_minimal(c: &mut Criterion) {
  c.bench_function("cynic-parser/minimal", move |b| {
    b.iter(|| cynic_parser_parse_query(QUERY))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_minimal,
  bench_smear_parser_minimal,
  bench_graphql_parser_minimal,
  bench_async_graphql_parser_minimal,
  bench_cynic_parser_minimal,
);
criterion_main!(benches);
