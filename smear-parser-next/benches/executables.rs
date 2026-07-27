use std::hint::black_box;

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use smear_parser_next::graphql::{
  GraphQL, ast::ExecutableDocument, error::GraphqlErrors, syntactic::GraphqlLexer,
};
use tokora::{Parse, Parser};

struct Fixture {
  name: &'static str,
  source: &'static str,
}

const FIXTURES: &[Fixture] = &[
  Fixture {
    name: "bench_01_tiny_simple",
    source: include_str!("../../smear/tests/fixtures/executables/bench_01_tiny_simple.graphql"),
  },
  Fixture {
    name: "bench_02_small_simple",
    source: include_str!("../../smear/tests/fixtures/executables/bench_02_small_simple.graphql"),
  },
  Fixture {
    name: "bench_03_small_variables",
    source: include_str!("../../smear/tests/fixtures/executables/bench_03_small_variables.graphql"),
  },
  Fixture {
    name: "bench_04_medium_nested",
    source: include_str!("../../smear/tests/fixtures/executables/bench_04_medium_nested.graphql"),
  },
  Fixture {
    name: "bench_05_medium_fragments",
    source: include_str!(
      "../../smear/tests/fixtures/executables/bench_05_medium_fragments.graphql"
    ),
  },
  Fixture {
    name: "bench_06_large_complex",
    source: include_str!("../../smear/tests/fixtures/executables/bench_06_large_complex.graphql"),
  },
  Fixture {
    name: "bench_07_large_deep_nesting",
    source: include_str!(
      "../../smear/tests/fixtures/executables/bench_07_large_deep_nesting.graphql"
    ),
  },
  Fixture {
    name: "bench_08_many_fields",
    source: include_str!("../../smear/tests/fixtures/executables/bench_08_many_fields.graphql"),
  },
  Fixture {
    name: "bench_09_many_aliases",
    source: include_str!("../../smear/tests/fixtures/executables/bench_09_many_aliases.graphql"),
  },
  Fixture {
    name: "bench_10_huge_comprehensive",
    source: include_str!(
      "../../smear/tests/fixtures/executables/bench_10_huge_comprehensive.graphql"
    ),
  },
];

fn parse_smear_parser_next<'inp>(
  source: &'inp str,
) -> Result<ExecutableDocument<&'inp str>, GraphqlErrors<&'inp str>> {
  Parser::with_parser::<
    'inp,
    GraphqlLexer<'inp, str>,
    ExecutableDocument<&'inp str>,
    GraphqlErrors<&'inp str>,
    _,
    GraphQL,
  >(ExecutableDocument::<&str>::graphql)
  .parse_str(source)
}

fn preflight(fixture: &Fixture) {
  assert!(
    parse_smear_parser_next(fixture.source).is_ok(),
    "smear-parser-next rejected {}",
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
    graphql_parser::parse_query::<&str>(fixture.source).is_ok(),
    "graphql-parser rejected {}",
    fixture.name,
  );
  assert!(
    async_graphql_parser::parse_query(fixture.source).is_ok(),
    "async-graphql-parser rejected {}",
    fixture.name,
  );
  assert!(
    cynic_parser::parse_executable_document(fixture.source).is_ok(),
    "cynic-parser rejected {}",
    fixture.name,
  );
}

fn bench_executables(c: &mut Criterion) {
  for fixture in FIXTURES {
    preflight(fixture);

    let mut group = c.benchmark_group(fixture.name);
    group.throughput(Throughput::Bytes(fixture.source.len() as u64));

    group.bench_function("smear-parser-next", |b| {
      b.iter(|| {
        let document = parse_smear_parser_next(black_box(fixture.source))
          .expect("fixture passed parser preflight");
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
        let document = graphql_parser::parse_query::<&str>(black_box(fixture.source))
          .expect("fixture passed parser preflight");
        drop(black_box(document));
      });
    });
    group.bench_function("async-graphql-parser", |b| {
      b.iter(|| {
        let document = async_graphql_parser::parse_query(black_box(fixture.source))
          .expect("fixture passed parser preflight");
        drop(black_box(document));
      });
    });
    group.bench_function("cynic-parser", |b| {
      b.iter(|| {
        let document = cynic_parser::parse_executable_document(black_box(fixture.source))
          .expect("fixture passed parser preflight");
        drop(black_box(document));
      });
    });

    group.finish();
  }
}

criterion_group!(benches, bench_executables);
criterion_main!(benches);
