use criterion::*;

include!("../utils.rs");

const QUERY: &str = include_str!("../../tests/fixtures/executables/bench_01_tiny_simple.graphql");

fn bench_apollo_parser_bench_01_tiny_simple(c: &mut Criterion) {
  c.bench_function(
    "apollo-parser: tests/fixtures/executables/bench_01_tiny_simple.graphql",
    move |b| b.iter(|| apollo_parser_parse_query(QUERY)),
  );
}

fn bench_smear_parser_bench_01_tiny_simple(c: &mut Criterion) {
  c.bench_function(
    "smear: tests/fixtures/executables/bench_01_tiny_simple.graphql",
    move |b| b.iter(|| smear_parser_parse_query(QUERY)),
  );
}

fn bench_graphql_parser_bench_01_tiny_simple(c: &mut Criterion) {
  c.bench_function(
    "graphql-parser: tests/fixtures/executables/bench_01_tiny_simple.graphql",
    move |b| b.iter(|| graphql_parser_parse_query(QUERY)),
  );
}

fn bench_async_graphql_parser_bench_01_tiny_simple(c: &mut Criterion) {
  c.bench_function(
    "async-graphql-parser: tests/fixtures/executables/bench_01_tiny_simple.graphql",
    move |b| b.iter(|| async_graphql_parser_parse_query(QUERY)),
  );
}

fn bench_cynic_parser_bench_01_tiny_simple(c: &mut Criterion) {
  c.bench_function(
    "cynic-parser: tests/fixtures/executables/bench_01_tiny_simple.graphql",
    move |b| b.iter(|| cynic_parser_parse_query(QUERY)),
  );
}

criterion_group!(
  benches,
  bench_apollo_parser_bench_01_tiny_simple,
  bench_smear_parser_bench_01_tiny_simple,
  bench_graphql_parser_bench_01_tiny_simple,
  bench_async_graphql_parser_bench_01_tiny_simple,
  bench_cynic_parser_bench_01_tiny_simple,
);
criterion_main!(benches);
