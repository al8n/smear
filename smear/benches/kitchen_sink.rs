use criterion::*;

include!("./utils.rs");

const QUERY: &str = include_str!("../tests/fixtures/executables/kitchen-sink.graphql");

fn bench_apollo_parser_kitchen_sink(c: &mut Criterion) {
  c.bench_function("apollo-parser/kitchen_sink", move |b| {
    b.iter(|| apollo_parser_parse_query(QUERY))
  });
}

fn bench_smear_parser_kitchen_sink(c: &mut Criterion) {
  c.bench_function("smear-graphql/kitchen_sink", move |b| {
    b.iter(|| smear_parser_parse_query(QUERY))
  });
}

fn bench_graphql_parser_kitchen_sink(c: &mut Criterion) {
  c.bench_function("graphql-parser/kitchen_sink", move |b| {
    b.iter(|| graphql_parser_parse_query(QUERY))
  });
}

fn bench_cynic_parser_kitchen_sink(c: &mut Criterion) {
  c.bench_function("cynic-parser/kitchen_sink", move |b| {
    b.iter(|| cynic_parser_parse_query(QUERY))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_kitchen_sink,
  bench_smear_parser_kitchen_sink,
  bench_graphql_parser_kitchen_sink,
  bench_cynic_parser_kitchen_sink,
  // bench_async_graphql_parser_kitchen_sink,
);
criterion_main!(benches);
