use criterion::*;

include!("../utils.rs");

const QUERY: &str = include_str!("../../tests/fixtures/executables/query_vars.graphql");

fn bench_apollo_parser_query_vars(c: &mut Criterion) {
  c.bench_function(
    "apollo-parser: tests/fixtures/executables/query_vars.graphql",
    move |b| b.iter(|| apollo_parser_parse_query(QUERY)),
  );
}

fn bench_smear_parser_query_vars(c: &mut Criterion) {
  c.bench_function(
    "smear: tests/fixtures/executables/query_vars.graphql",
    move |b| b.iter(|| smear_parser_parse_query(QUERY)),
  );
}

fn bench_graphql_parser_query_vars(c: &mut Criterion) {
  c.bench_function(
    "graphql-parser: tests/fixtures/executables/query_vars.graphql",
    move |b| b.iter(|| graphql_parser_parse_query(QUERY)),
  );
}

fn bench_async_graphql_parser_query_vars(c: &mut Criterion) {
  c.bench_function(
    "async-graphql-parser: tests/fixtures/executables/query_vars.graphql",
    move |b| b.iter(|| async_graphql_parser_parse_query(QUERY)),
  );
}

fn bench_cynic_parser_query_vars(c: &mut Criterion) {
  c.bench_function(
    "cynic-parser: tests/fixtures/executables/query_vars.graphql",
    move |b| b.iter(|| cynic_parser_parse_query(QUERY)),
  );
}

criterion_group!(
  benches,
  bench_apollo_parser_query_vars,
  bench_smear_parser_query_vars,
  bench_graphql_parser_query_vars,
  bench_async_graphql_parser_query_vars,
  bench_cynic_parser_query_vars,
);
criterion_main!(benches);
