use chumsky::{error::Simple, extra, span::SimpleSpan};
use criterion::*;
use smear_parser::parse::ParseStr;

const QUERY: &str = include_str!("../tests/fixtures/queries/query_vars.graphql");

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

fn bench_apollo_parser_query_vars(c: &mut Criterion) {
  c.bench_function("apollo-parser/query_vars", move |b| {
    b.iter(|| apollo_parser_parse_query(QUERY))
  });
}

fn bench_smear_parser_query_vars(c: &mut Criterion) {
  c.bench_function("smear-graphql/query_vars", move |b| {
    b.iter(|| smear_parser_parse_query(QUERY))
  });
}

fn bench_graphql_parser_query_vars(c: &mut Criterion) {
  c.bench_function("graphql-parser/query_vars", move |b| {
    b.iter(|| graphql_parser_parse_query(QUERY))
  });
}

fn bench_async_graphql_parser_query_vars(c: &mut Criterion) {
  c.bench_function("async-graphql-parser/query_vars", move |b| {
    b.iter(|| async_graphql_parser_parse_query(QUERY))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_query_vars,
  bench_smear_parser_query_vars,
  bench_graphql_parser_query_vars,
  bench_async_graphql_parser_query_vars,
);
criterion_main!(benches);
