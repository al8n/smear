use criterion::*;

include!("utils.rs");

// Benchmark fixtures
const BENCH_01_TINY_SIMPLE: &str =
  include_str!("../tests/fixtures/executables/bench_01_tiny_simple.graphql");
const BENCH_02_SMALL_SIMPLE: &str =
  include_str!("../tests/fixtures/executables/bench_02_small_simple.graphql");
const BENCH_03_SMALL_VARIABLES: &str =
  include_str!("../tests/fixtures/executables/bench_03_small_variables.graphql");
const BENCH_04_MEDIUM_NESTED: &str =
  include_str!("../tests/fixtures/executables/bench_04_medium_nested.graphql");
const BENCH_05_MEDIUM_FRAGMENTS: &str =
  include_str!("../tests/fixtures/executables/bench_05_medium_fragments.graphql");
const BENCH_06_LARGE_COMPLEX: &str =
  include_str!("../tests/fixtures/executables/bench_06_large_complex.graphql");
const BENCH_07_LARGE_DEEP_NESTING: &str =
  include_str!("../tests/fixtures/executables/bench_07_large_deep_nesting.graphql");
const BENCH_08_MANY_FIELDS: &str =
  include_str!("../tests/fixtures/executables/bench_08_many_fields.graphql");
const BENCH_09_MANY_ALIASES: &str =
  include_str!("../tests/fixtures/executables/bench_09_many_aliases.graphql");
const BENCH_10_HUGE_COMPREHENSIVE: &str =
  include_str!("../tests/fixtures/executables/bench_10_huge_comprehensive.graphql");

fn bench_executable(c: &mut Criterion) {
  let fixtures = [
    ("bench_01_tiny_simple", BENCH_01_TINY_SIMPLE),
    ("bench_02_small_simple", BENCH_02_SMALL_SIMPLE),
    ("bench_03_small_variables", BENCH_03_SMALL_VARIABLES),
    ("bench_04_medium_nested", BENCH_04_MEDIUM_NESTED),
    ("bench_05_medium_fragments", BENCH_05_MEDIUM_FRAGMENTS),
    ("bench_06_large_complex", BENCH_06_LARGE_COMPLEX),
    ("bench_07_large_deep_nesting", BENCH_07_LARGE_DEEP_NESTING),
    ("bench_08_many_fields", BENCH_08_MANY_FIELDS),
    ("bench_09_many_aliases", BENCH_09_MANY_ALIASES),
    ("bench_10_huge_comprehensive", BENCH_10_HUGE_COMPREHENSIVE),
  ];

  for (name, query) in fixtures {
    let mut group = c.benchmark_group(name);

    group.bench_function("apollo-parser", |b| {
      b.iter(|| apollo_parser_parse_query(query))
    });

    group.bench_function("smear", |b| b.iter(|| smear_parser_parse_query(query)));

    group.bench_function("graphql-parser", |b| {
      b.iter(|| graphql_parser_parse_query(query))
    });

    group.bench_function("async-graphql-parser", |b| {
      b.iter(|| async_graphql_parser_parse_query(query))
    });

    group.bench_function("cynic-parser", |b| {
      b.iter(|| cynic_parser_parse_query(query))
    });

    group.finish();
  }
}

criterion_group!(benches, bench_executable);
criterion_main!(benches);
