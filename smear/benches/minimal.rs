use criterion::*;

include!("./utils.rs");

const QUERY: &str = include_str!("../tests/fixtures/executables/minimal.graphql");

fn bench_apollo_parser_minimal(c: &mut Criterion) {
  c.bench_function("apollo-parser/minimal", move |b| {
    b.iter(|| apollo_parser_parse_query(QUERY))
  });
}

fn bench_smear_parser_minimal(c: &mut Criterion) {
  c.bench_function("smear-graphql/minimal", move |b| {
    b.iter(|| smear_parser_parse_query(QUERY))
  });
}

fn bench_graphql_parser_minimal(c: &mut Criterion) {
  c.bench_function("graphql-parser/minimal", move |b| {
    b.iter(|| graphql_parser_parse_query(QUERY))
  });
}

fn bench_async_graphql_parser_minimal(c: &mut Criterion) {
  c.bench_function("async-graphql-parser/minimal", move |b| {
    b.iter(|| async_graphql_parser_parse_query(QUERY))
  });
}

fn bench_cynic_parser_minimal(c: &mut Criterion) {
  c.bench_function("cynic-parser/minimal", move |b| {
    b.iter(|| cynic_parser_parse_query(QUERY))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_minimal,
  bench_smear_parser_minimal,
  bench_graphql_parser_minimal,
  bench_async_graphql_parser_minimal,
  bench_cynic_parser_minimal,
);
criterion_main!(benches);
