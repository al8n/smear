use criterion::*;

include!("utils.rs");

// Schema benchmark fixtures
const APOLLO_STUDIO: &str = include_str!("../tests/fixtures/schemas/apollo-studio.graphql");
const CURVE_FINANCE: &str = include_str!("../tests/fixtures/schemas/curve-finance_schema.graphql");
const GITHUB: &str = include_str!("../tests/fixtures/schemas/github_schema.graphql");
const GITLAB: &str = include_str!("../tests/fixtures/schemas/gitlab_schema.graphql");
const GMX: &str = include_str!("../tests/fixtures/schemas/gmx_schema.graphql");
const KITCHEN_SINK: &str = include_str!("../tests/fixtures/schemas/kitchen-sink.graphql");
const KITCHEN_SINK_CANONICAL: &str =
  include_str!("../tests/fixtures/schemas/kitchen-sink_canonical.graphql");
const LIDO: &str = include_str!("../tests/fixtures/schemas/lido_schema.graphql");
const LIQUITY: &str = include_str!("../tests/fixtures/schemas/liquity_schema.graphql");
const MAKERDAO: &str = include_str!("../tests/fixtures/schemas/makerdao_schema.graphql");
const MINIMAL: &str = include_str!("../tests/fixtures/schemas/minimal.graphql");
const NFT: &str = include_str!("../tests/fixtures/schemas/nft.graphql");
const SCHEMA_KITCHEN_SINK: &str =
  include_str!("../tests/fixtures/schemas/schema_kitchen_sink.graphql");
const SCHEMA_LENDING: &str = include_str!("../tests/fixtures/schemas/schema-lending.graphql");
const SCHEMA_YIELD: &str = include_str!("../tests/fixtures/schemas/schema-yield.graphql");
const SIMPLE_OBJECT: &str = include_str!("../tests/fixtures/schemas/simple_object.graphql");
const SUPERGRAPH: &str = include_str!("../tests/fixtures/schemas/supergraph.graphql");
const UNISWAP_V2: &str = include_str!("../tests/fixtures/schemas/uniswap-v2_schema.graphql");

fn bench_schemas(c: &mut Criterion) {
  let fixtures = [
    ("apollo_studio", APOLLO_STUDIO),
    ("curve_finance_schema", CURVE_FINANCE),
    ("github_schema", GITHUB),
    ("gitlab_schema", GITLAB),
    ("gmx_schema", GMX),
    ("kitchen_sink", KITCHEN_SINK),
    ("kitchen_sink_canonical", KITCHEN_SINK_CANONICAL),
    ("lido_schema", LIDO),
    ("liquity_schema", LIQUITY),
    ("makerdao_schema", MAKERDAO),
    ("minimal", MINIMAL),
    ("nft", NFT),
    ("schema_kitchen_sink", SCHEMA_KITCHEN_SINK),
    ("schema_lending", SCHEMA_LENDING),
    ("schema_yield", SCHEMA_YIELD),
    ("simple_object", SIMPLE_OBJECT),
    ("supergraph", SUPERGRAPH),
    ("uniswap_v2_schema", UNISWAP_V2),
  ];

  for (name, schema) in fixtures {
    let mut group = c.benchmark_group(name);

    group.bench_function("apollo-parser", |b| {
      b.iter(|| apollo_parser_parse_schema(schema))
    });

    group.bench_function("smear", |b| b.iter(|| smear_parser_parse_schema(schema)));

    group.bench_function("graphql-parser", |b| {
      b.iter(|| graphql_parser_parse_schema(schema))
    });

    group.bench_function("async-graphql-parser", |b| {
      b.iter(|| async_graphql_parser_parse_schema(schema))
    });

    group.bench_function("cynic-parser", |b| {
      b.iter(|| cynic_parser_parse_schema(schema))
    });

    group.finish();
  }
}

criterion_group!(benches, bench_schemas);
criterion_main!(benches);
