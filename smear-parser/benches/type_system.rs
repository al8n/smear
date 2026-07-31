use std::hint::black_box;

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use smear_parser::graphql::{
  GraphQL, ast::TypeSystemDocument, error::GraphqlErrors, syntactic::GraphqlLexer,
};
use tokora::{Parse, Parser};

struct Fixture {
  name: &'static str,
  source: &'static str,
}

const FIXTURES: &[Fixture] = &[
  Fixture {
    name: "type_system_01_minimal",
    source: include_str!("../../smear/tests/fixtures/schemas/minimal_type.graphql"),
  },
  Fixture {
    name: "type_system_02_descriptions",
    source: include_str!(
      "../../smear/tests/fixtures/schemas/directive_descriptions_canonical.graphql"
    ),
  },
  Fixture {
    name: "type_system_03_descriptions_and_extensions",
    source: include_str!("../../smear/tests/fixtures/schemas/kitchen-sink_canonical.graphql"),
  },
  Fixture {
    name: "type_system_04_medium_lending_schema",
    source: include_str!("../../smear/tests/fixtures/schemas/schema-lending.graphql"),
  },
  Fixture {
    name: "type_system_05_large_apollo_studio_schema",
    source: include_str!("../../smear/tests/fixtures/schemas/apollo-studio.graphql"),
  },
];

fn parse_smear_parser<'inp>(
  source: &'inp str,
) -> Result<TypeSystemDocument<&'inp str>, GraphqlErrors<&'inp str>> {
  Parser::with_parser::<
    'inp,
    GraphqlLexer<'inp, str>,
    TypeSystemDocument<&'inp str>,
    GraphqlErrors<&'inp str>,
    _,
    GraphQL,
  >(TypeSystemDocument::<&str>::graphql)
  .parse_str(source)
}

fn preflight(fixture: &Fixture) {
  let smear_document = parse_smear_parser(fixture.source);
  assert!(
    smear_document.is_ok(),
    "smear-parser rejected {}: {smear_document:?}",
    fixture.name,
  );

  let apollo_document = apollo_parser::Parser::new(fixture.source).parse();
  assert_eq!(
    apollo_document.errors().len(),
    0,
    "apollo-parser rejected {}",
    fixture.name,
  );

  assert!(
    graphql_parser::parse_schema::<&str>(fixture.source).is_ok(),
    "graphql-parser rejected {}",
    fixture.name,
  );
  assert!(
    async_graphql_parser::parse_schema(fixture.source).is_ok(),
    "async-graphql-parser rejected {}",
    fixture.name,
  );
  assert!(
    cynic_parser::parse_type_system_document(fixture.source).is_ok(),
    "cynic-parser rejected {}",
    fixture.name,
  );
}

fn bench_type_system(c: &mut Criterion) {
  for fixture in FIXTURES {
    preflight(fixture);

    let mut group = c.benchmark_group(fixture.name);
    group.throughput(Throughput::Bytes(fixture.source.len() as u64));

    group.bench_function("smear-parser", |b| {
      b.iter(|| {
        let document =
          parse_smear_parser(black_box(fixture.source)).expect("fixture passed parser preflight");
        drop(black_box(document));
      });
    });
    group.bench_function("apollo-parser", |b| {
      b.iter(|| {
        let document = apollo_parser::Parser::new(black_box(fixture.source)).parse();
        drop(black_box(document));
      });
    });
    group.bench_function("graphql-parser", |b| {
      b.iter(|| {
        let document = graphql_parser::parse_schema::<&str>(black_box(fixture.source))
          .expect("fixture passed parser preflight");
        drop(black_box(document));
      });
    });
    group.bench_function("async-graphql-parser", |b| {
      b.iter(|| {
        let document = async_graphql_parser::parse_schema(black_box(fixture.source))
          .expect("fixture passed parser preflight");
        drop(black_box(document));
      });
    });
    group.bench_function("cynic-parser", |b| {
      b.iter(|| {
        let document = cynic_parser::parse_type_system_document(black_box(fixture.source))
          .expect("fixture passed parser preflight");
        drop(black_box(document));
      });
    });

    group.finish();
  }
}

criterion_group!(benches, bench_type_system);
criterion_main!(benches);
