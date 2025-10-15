use criterion::*;

include!("../utils.rs");

const QUERY: &str = include_str!("../../tests/fixtures/executables/string_literal.graphql");

fn bench_apollo_parser_string_literal(c: &mut Criterion) {
  c.bench_function(
    "apollo-parser: tests/fixtures/executables/string_literal.graphql",
    move |b| b.iter(|| apollo_parser_parse_query(QUERY)),
  );
}

fn bench_smear_parser_string_literal(c: &mut Criterion) {
  c.bench_function(
    "smear: tests/fixtures/executables/string_literal.graphql",
    move |b| b.iter(|| smear_parser_parse_query(QUERY)),
  );
}

fn bench_graphql_parser_string_literal(c: &mut Criterion) {
  c.bench_function(
    "graphql-parser: tests/fixtures/executables/string_literal.graphql",
    move |b| b.iter(|| graphql_parser_parse_query(QUERY)),
  );
}

fn bench_async_graphql_parser_string_literal(c: &mut Criterion) {
  c.bench_function(
    "async-graphql-parser: tests/fixtures/executables/string_literal.graphql",
    move |b| b.iter(|| async_graphql_parser_parse_query(QUERY)),
  );
}

fn bench_cynic_parser_string_literal(c: &mut Criterion) {
  c.bench_function(
    "cynic-parser: tests/fixtures/executables/string_literal.graphql",
    move |b| b.iter(|| cynic_parser_parse_query(QUERY)),
  );
}

criterion_group!(
  benches,
  bench_apollo_parser_string_literal,
  bench_smear_parser_string_literal,
  bench_graphql_parser_string_literal,
  bench_async_graphql_parser_string_literal,
  bench_cynic_parser_string_literal,
);
criterion_main!(benches);
