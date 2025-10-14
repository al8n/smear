use criterion::*;

include!("./utils.rs");

const QUERY: &str = include_str!("../tests/fixtures/executables/inline_fragment.graphql");

fn bench_apollo_parser_inline_fragment(c: &mut Criterion) {
  c.bench_function("apollo-parser/inline_fragment", move |b| {
    b.iter(|| apollo_parser_parse_query(QUERY))
  });
}

fn bench_smear_parser_inline_fragment(c: &mut Criterion) {
  c.bench_function("smear-graphql/inline_fragment", move |b| {
    b.iter(|| smear_parser_parse_query(QUERY))
  });
}

fn bench_graphql_parser_inline_fragment(c: &mut Criterion) {
  c.bench_function("graphql-parser/inline_fragment", move |b| {
    b.iter(|| graphql_parser_parse_query(QUERY))
  });
}

fn bench_async_graphql_parser_inline_fragment(c: &mut Criterion) {
  c.bench_function("async-graphql-parser/inline_fragment", move |b| {
    b.iter(|| async_graphql_parser_parse_query(QUERY))
  });
}

fn bench_cynic_parser_inline_fragment(c: &mut Criterion) {
  c.bench_function("cynic-parser/inline_fragment", move |b| {
    b.iter(|| cynic_parser_parse_query(QUERY))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_inline_fragment,
  bench_smear_parser_inline_fragment,
  bench_graphql_parser_inline_fragment,
  bench_async_graphql_parser_inline_fragment,
  bench_cynic_parser_inline_fragment,
);
criterion_main!(benches);
