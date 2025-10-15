use criterion::*;

include!("../utils.rs");

const QUERY: &str = include_str!("../../tests/fixtures/executables/fragment_spread.graphql");

fn bench_apollo_parser_fragment_spread(c: &mut Criterion) {
  c.bench_function(
    "apollo-parser: tests/fixtures/executables/fragment_spread.graphql",
    move |b| b.iter(|| apollo_parser_parse_query(QUERY)),
  );
}

fn bench_smear_parser_fragment_spread(c: &mut Criterion) {
  c.bench_function(
    "smear: tests/fixtures/executables/fragment_spread.graphql",
    move |b| b.iter(|| smear_parser_parse_query(QUERY)),
  );
}

fn bench_graphql_parser_fragment_spread(c: &mut Criterion) {
  c.bench_function(
    "graphql-parser: tests/fixtures/executables/fragment_spread.graphql",
    move |b| b.iter(|| graphql_parser_parse_query(QUERY)),
  );
}

fn bench_async_graphql_parser_fragment_spread(c: &mut Criterion) {
  c.bench_function(
    "async-graphql-parser: tests/fixtures/executables/fragment_spread.graphql",
    move |b| b.iter(|| async_graphql_parser_parse_query(QUERY)),
  );
}

fn bench_cynic_parser_fragment_spread(c: &mut Criterion) {
  c.bench_function(
    "cynic-parser: tests/fixtures/executables/fragment_spread.graphql",
    move |b| b.iter(|| cynic_parser_parse_query(QUERY)),
  );
}

criterion_group!(
  benches,
  bench_apollo_parser_fragment_spread,
  bench_smear_parser_fragment_spread,
  bench_graphql_parser_fragment_spread,
  bench_async_graphql_parser_fragment_spread,
  bench_cynic_parser_fragment_spread,
);
criterion_main!(benches);
