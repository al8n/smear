use criterion::*;

include!("./utils.rs");

const SCHEMA: &str = include_str!("../tests/fixtures/schemas/github_schema.graphql");

fn bench_apollo_parser_parse_github_schema(c: &mut Criterion) {
  c.bench_function("apollo-parser/parse_github_schema", move |b| {
    b.iter(|| apollo_parser_parse_schema(SCHEMA))
  });
}

fn bench_smear_parser_parse_github_schema(c: &mut Criterion) {
  c.bench_function("smear-graphql/parse_github_schema", move |b| {
    b.iter(|| smear_parser_parse_schema(SCHEMA))
  });
}

fn bench_graphql_parser_parse_github_schema(c: &mut Criterion) {
  c.bench_function("graphql-parser/parse_github_schema", move |b| {
    b.iter(|| graphql_parser_parse_schema(SCHEMA))
  });
}

fn bench_async_graphql_parser_parse_github_schema(c: &mut Criterion) {
  c.bench_function("async-graphql-parser/parse_github_schema", move |b| {
    b.iter(|| async_graphql_parser_parse_schema(SCHEMA))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_parse_github_schema,
  bench_smear_parser_parse_github_schema,
  bench_graphql_parser_parse_github_schema,
  bench_async_graphql_parser_parse_github_schema,
);
criterion_main!(benches);
