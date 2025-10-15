use criterion::*;

include!("../utils.rs");

const QUERY: &str = include_str!("../../tests/fixtures/executables/inline_fragment_dir.graphql");

fn bench_apollo_parser_inline_fragment_dir(c: &mut Criterion) {
  c.bench_function(
    "apollo-parser: tests/fixtures/executables/inline_fragment_dir.graphql",
    move |b| b.iter(|| apollo_parser_parse_query(QUERY)),
  );
}

fn bench_smear_parser_inline_fragment_dir(c: &mut Criterion) {
  c.bench_function(
    "smear: tests/fixtures/executables/inline_fragment_dir.graphql",
    move |b| b.iter(|| smear_parser_parse_query(QUERY)),
  );
}

fn bench_graphql_parser_inline_fragment_dir(c: &mut Criterion) {
  c.bench_function(
    "graphql-parser: tests/fixtures/executables/inline_fragment_dir.graphql",
    move |b| b.iter(|| graphql_parser_parse_query(QUERY)),
  );
}

fn bench_async_graphql_parser_inline_fragment_dir(c: &mut Criterion) {
  c.bench_function(
    "async-graphql-parser: tests/fixtures/executables/inline_fragment_dir.graphql",
    move |b| b.iter(|| async_graphql_parser_parse_query(QUERY)),
  );
}

fn bench_cynic_parser_inline_fragment_dir(c: &mut Criterion) {
  c.bench_function(
    "cynic-parser: tests/fixtures/executables/inline_fragment_dir.graphql",
    move |b| b.iter(|| cynic_parser_parse_query(QUERY)),
  );
}

criterion_group!(
  benches,
  bench_apollo_parser_inline_fragment_dir,
  bench_smear_parser_inline_fragment_dir,
  bench_graphql_parser_inline_fragment_dir,
  bench_async_graphql_parser_inline_fragment_dir,
  bench_cynic_parser_inline_fragment_dir,
);
criterion_main!(benches);
