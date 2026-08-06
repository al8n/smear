//! GraphQLx-only document benchmarks.
//!
//! These fixtures intentionally exercise imports, generic headers, qualified
//! paths, `where` clauses, and extensions. They are benchmarked only against
//! smear-parser because standard GraphQL parsers do not accept the same
//! GraphQLx language surface.

use std::hint::black_box;

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use smear::parser::graphqlx::{
  GraphQLx, ast::Document, error::GraphqlxErrors, syntactic::GraphqlxLexer,
};
use tokora::{Parse, Parser};

struct Fixture {
  name: &'static str,
  source: &'static str,
}

const FIXTURES: &[Fixture] = &[
  Fixture {
    name: "graphqlx_01_imports_and_paths",
    source: include_str!("../tests/fixtures/parser/graphqlx/ok/0013_complex_import.graphqlx"),
  },
  Fixture {
    name: "graphqlx_02_definitions_and_extensions",
    source: include_str!("../tests/fixtures/parser/graphqlx/ok/0015_extend_with_generics.graphqlx"),
  },
  Fixture {
    name: "graphqlx_03_operations_and_generics",
    source: include_str!(
      "../tests/fixtures/parser/graphqlx/ok/0016_operation_with_generics.graphqlx"
    ),
  },
  Fixture {
    name: "graphqlx_04_complex_fragments",
    source: include_str!("../tests/fixtures/parser/graphqlx/ok/0022_complex_fragments.graphqlx"),
  },
];

fn parse_smear_parser<'inp>(
  source: &'inp str,
) -> Result<Document<&'inp str>, GraphqlxErrors<&'inp str>> {
  Parser::with_parser::<
    'inp,
    GraphqlxLexer<'inp, str>,
    Document<&'inp str>,
    GraphqlxErrors<&'inp str>,
    _,
    GraphQLx,
  >(Document::<&str>::graphqlx)
  .parse_str(source)
}

fn bench_graphqlx(c: &mut Criterion) {
  for fixture in FIXTURES {
    let preflight = parse_smear_parser(fixture.source);
    assert!(
      preflight.is_ok(),
      "smear-parser rejected {}: {preflight:?}",
      fixture.name,
    );

    let mut group = c.benchmark_group(fixture.name);
    group.throughput(Throughput::Bytes(fixture.source.len() as u64));
    group.bench_function("smear-parser", |b| {
      b.iter(|| {
        let document =
          parse_smear_parser(black_box(fixture.source)).expect("fixture passed parser preflight");
        drop(black_box(document));
      });
    });
    group.finish();
  }
}

criterion_group!(benches, bench_graphqlx);
criterion_main!(benches);
