use criterion::*;

include!("./utils.rs");

const QUERY: &str = "query ExampleQuery($topProductsFirst: Int) {\n  me { \n    id\n  }\n  topProducts(first:  $topProductsFirst) {\n    name\n    price\n    inStock\n weight\n test test test test test test test test test test test test }\n}";

const ALIAS: &str = include_str!("../tests/fixtures/executables/alias.graphql");

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

fn bench_cynic_parser_query_parser(c: &mut Criterion) {
  c.bench_function("cynic-parser/query_parser", move |b| {
    b.iter(|| cynic_parser_parse_query(QUERY))
  });
}

fn bench_cynic_parser_many_aliases(c: &mut Criterion) {
  c.bench_function("cynic-parser/many_aliases", move |b| {
    b.iter(|| cynic_parser_parse_query(ALIAS))
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
  bench_cynic_parser_many_aliases,
  bench_cynic_parser_query_parser,
);
criterion_main!(benches);
