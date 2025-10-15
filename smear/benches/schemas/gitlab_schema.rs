use criterion::*;

include!("../utils.rs");

const SCHEMA: &str = include_str!("../../tests/fixtures/schemas/gitlab_schema.graphql");

fn bench_apollo_parser_parse_gitlab_schema(c: &mut Criterion) {
  c.bench_function(
    "apollo-parser: tests/fixtures/schemas/gitlab_schema.graphql",
    move |b| b.iter(|| apollo_parser_parse_schema(SCHEMA)),
  );
}

fn bench_smear_parser_parse_gitlab_schema(c: &mut Criterion) {
  c.bench_function(
    "smear: tests/fixtures/schemas/gitlab_schema.graphql",
    move |b| b.iter(|| smear_parser_parse_schema(SCHEMA)),
  );
}

fn bench_graphql_parser_parse_gitlab_schema(c: &mut Criterion) {
  c.bench_function(
    "graphql-parser: tests/fixtures/schemas/gitlab_schema.graphql",
    move |b| b.iter(|| graphql_parser_parse_schema(SCHEMA)),
  );
}

fn bench_async_graphql_parser_parse_gitlab_schema(c: &mut Criterion) {
  c.bench_function(
    "async-graphql-parser: tests/fixtures/schemas/gitlab_schema.graphql",
    move |b| b.iter(|| async_graphql_parser_parse_schema(SCHEMA)),
  );
}

criterion_group!(
  benches,
  bench_apollo_parser_parse_gitlab_schema,
  bench_smear_parser_parse_gitlab_schema,
  bench_graphql_parser_parse_gitlab_schema,
  bench_async_graphql_parser_parse_gitlab_schema,
);
criterion_main!(benches);
