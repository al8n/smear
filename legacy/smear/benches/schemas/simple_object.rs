use criterion::*;

include!("../utils.rs");

const SCHEMA: &str = include_str!("../../tests/fixtures/schemas/simple_object.graphql");

fn bench_apollo_parser_parse_simple_object(c: &mut Criterion) {
  c.bench_function(
    "apollo-parser: tests/fixtures/schemas/simple_object.graphql",
    move |b| b.iter(|| apollo_parser_parse_schema(SCHEMA)),
  );
}

fn bench_smear_parser_parse_simple_object(c: &mut Criterion) {
  c.bench_function(
    "smear: tests/fixtures/schemas/simple_object.graphql",
    move |b| b.iter(|| smear_parser_parse_schema(SCHEMA)),
  );
}

fn bench_graphql_parser_parse_simple_object(c: &mut Criterion) {
  c.bench_function(
    "graphql-parser: tests/fixtures/schemas/simple_object.graphql",
    move |b| b.iter(|| graphql_parser_parse_schema(SCHEMA)),
  );
}

fn bench_async_graphql_parser_parse_simple_object(c: &mut Criterion) {
  c.bench_function(
    "async-graphql-parser: tests/fixtures/schemas/simple_object.graphql",
    move |b| b.iter(|| async_graphql_parser_parse_schema(SCHEMA)),
  );
}

fn bench_cynic_parser_parse_simple_object(c: &mut Criterion) {
  c.bench_function(
    "cynic-parser: tests/fixtures/schemas/simple_object.graphql",
    move |b| b.iter(|| cynic_parser_parse_schema(SCHEMA)),
  );
}

criterion_group!(
  benches,
  bench_apollo_parser_parse_simple_object,
  bench_smear_parser_parse_simple_object,
  bench_graphql_parser_parse_simple_object,
  bench_async_graphql_parser_parse_simple_object,
  bench_cynic_parser_parse_simple_object,
);
criterion_main!(benches);
