use chumsky::{error::Simple, extra, span::SimpleSpan};
use criterion::*;
use smear_parser::parse::ParseStr;

const QUERY: &str = "query ExampleQuery($topProductsFirst: Int) {\n  me { \n    id\n  }\n  topProducts(first:  $topProductsFirst) {\n    name\n    price\n    inStock\n weight\n test test test test test test test test test test test test }\n}";

const ALIAS: &str = include_str!("testdata/alias.graphql");

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

fn bench_apollo_query_parser(c: &mut Criterion) {
  c.bench_function("apollo-parser/query_parser", move |b| {
    b.iter(|| apollo_parser_parse_query(QUERY))
  });
}

fn bench_apollo_parser_many_aliases(c: &mut Criterion) {
  c.bench_function("apollo-parser/many_aliases", move |b| {
    b.iter(|| apollo_parser_parse_query(ALIAS))
  });
}

fn bench_smear_query_parser(c: &mut Criterion) {
  c.bench_function("smear-graphql/query_parser", move |b| {
    b.iter(|| smear_parser_parse_query(QUERY))
  });
}

fn bench_smear_parser_many_aliases(c: &mut Criterion) {
  c.bench_function("smear-graphql/many_aliases", move |b| {
    b.iter(|| smear_parser_parse_query(ALIAS))
  });
}

fn bench_graphql_parser_query_parser(c: &mut Criterion) {
  c.bench_function("graphql-parser/query_parser", move |b| {
    b.iter(|| graphql_parser_parse_query(QUERY))
  });
}

fn bench_graphql_parser_many_aliases(c: &mut Criterion) {
  c.bench_function("graphql-parser/many_aliases", move |b| {
    b.iter(|| graphql_parser_parse_query(ALIAS))
  });
}

fn bench_async_graphql_parser_query_parser(c: &mut Criterion) {
  c.bench_function("async-graphql-parser/query_parser", move |b| {
    b.iter(|| async_graphql_parser_parse_query(QUERY))
  });
}

fn bench_async_graphql_parser_many_aliases(c: &mut Criterion) {
  c.bench_function("async-graphql-parser/many_aliases", move |b| {
    b.iter(|| async_graphql_parser_parse_query(ALIAS))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_many_aliases,
  bench_apollo_query_parser,
  bench_smear_parser_many_aliases,
  bench_smear_query_parser,
  bench_graphql_parser_many_aliases,
  bench_graphql_parser_query_parser,
  bench_async_graphql_parser_many_aliases,
  bench_async_graphql_parser_query_parser,
);
criterion_main!(benches);
