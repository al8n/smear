use criterion::*;

include!("../utils.rs");

const SCHEMA: &str = include_str!("../../tests/fixtures/schemas/schema_kitchen_sink.graphql");

fn bench_apollo_parser_parse_schema_kitchen_sink(c: &mut Criterion) {
  c.bench_function(
    "apollo-parser: tests/fixtures/schemas/schema_kitchen_sink.graphql",
    move |b| b.iter(|| apollo_parser_parse_schema(SCHEMA)),
  );
}

fn bench_smear_parser_parse_schema_kitchen_sink(c: &mut Criterion) {
  c.bench_function(
    "smear: tests/fixtures/schemas/schema_kitchen_sink.graphql",
    move |b| b.iter(|| smear_parser_parse_schema(SCHEMA)),
  );
}

fn bench_async_graphql_parser_parse_schema_kitchen_sink(c: &mut Criterion) {
  c.bench_function(
    "async-graphql-parser: tests/fixtures/schemas/schema_kitchen_sink.graphql",
    move |b| b.iter(|| async_graphql_parser_parse_schema(SCHEMA)),
  );
}

fn bench_cynic_parser_parse_schema_kitchen_sink(c: &mut Criterion) {
  c.bench_function(
    "cynic-parser: tests/fixtures/schemas/schema_kitchen_sink.graphql",
    move |b| b.iter(|| cynic_parser_parse_schema(SCHEMA)),
  );
}

criterion_group!(
  benches,
  bench_apollo_parser_parse_schema_kitchen_sink,
  bench_smear_parser_parse_schema_kitchen_sink,
  bench_async_graphql_parser_parse_schema_kitchen_sink,
  bench_cynic_parser_parse_schema_kitchen_sink,
);
criterion_main!(benches);
