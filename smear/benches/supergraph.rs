use criterion::*;

include!("./utils.rs");

const SCHEMA: &str = include_str!("testdata/supergraph.graphql");

fn bench_apollo_parser_parse_supergraph(c: &mut Criterion) {
  c.bench_function("apollo-parser/supergraph_parser", move |b| {
    b.iter(|| apollo_parser_parse_schema(SCHEMA))
  });
}

fn bench_smear_parser_parse_supergraph(c: &mut Criterion) {
  c.bench_function("smear-graphql/supergraph_parser", move |b| {
    b.iter(|| smear_parser_parse_schema(SCHEMA))
  });
}

fn bench_graphql_parser_parse_supergraph(c: &mut Criterion) {
  c.bench_function("graphql-parser/supergraph_parser", move |b| {
    b.iter(|| graphql_parser_parse_schema(SCHEMA))
  });
}

fn bench_async_graphql_parser_parse_supergraph(c: &mut Criterion) {
  c.bench_function("async-graphql-parser/supergraph_parser", move |b| {
    b.iter(|| async_graphql_parser_parse_schema(SCHEMA))
  });
}

fn bench_cynic_parser_parse_supergraph(c: &mut Criterion) {
  c.bench_function("cynic-parser/supergraph_parser", move |b| {
    b.iter(|| cynic_parser_parse_schema(SCHEMA))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_parse_supergraph,
  bench_smear_parser_parse_supergraph,
  bench_graphql_parser_parse_supergraph,
  bench_async_graphql_parser_parse_supergraph,
  bench_cynic_parser_parse_supergraph,
);
criterion_main!(benches);
