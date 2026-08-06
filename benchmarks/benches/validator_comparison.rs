//! **G5 — the validator benchmark.** smear's draft §5 rules against `apollo-compiler`'s.
//!
//! # The number the design spec asks for
//!
//! Issue #85's G5 sets one target: **validate-only ≤ ¼ of apollo's on the same corpus**, with
//! parse+validate strictly ahead end to end. Its measured baseline is "apollo validates a
//! 529-byte realistic query in ≈13.4 µs, 41.8% of its 32 µs parse+validate", so the corpus here is
//! that probe's schema and query, reproduced verbatim in
//! [`corpus::REALISTIC_SDL`]/[`corpus::REALISTIC_QUERY`] — the comparison is against the same
//! bytes the target was set from, not against a document chosen afterwards.
//!
//! # apollo has no validate-only entry point, so this measures two decompositions
//!
//! [`ExecutableDocument::validate`][apollo_compiler::ExecutableDocument::validate] consumes its
//! receiver, so a timing loop around it would be timing a clone. Both apollo rows are therefore
//! differences, and they are not the same difference:
//!
//! * **`apollo/validate (design method)`** = `parse_and_validate` − `ast::Document::parse`. This is
//!   what the design spec measured and what the ¼ target was set against. It includes building
//!   apollo's `ExecutableDocument` IR — the `IndexMap`s, the `Node<_>` refcounts — because that
//!   build is where several of apollo's rules live (operation-name uniqueness, 5.3.1, the negative
//!   half of 5.3.3). It is validation as a server experiences it, not as a profiler would slice it.
//! * **`apollo/validate (build excluded)`** = `to_executable_validate` − `to_executable`. Both
//!   sides of that subtraction build the IR, so the difference is apollo's validate stage alone.
//!   Strictly smaller, and the harder number for smear to beat, which is why it is reported.
//!
//! smear's row needs no subtraction: `validate_executable` borrows the AST and the caller's
//! `Scratch`, so the timed region is validation and nothing else.
//!
//! # What is warm and what is not
//!
//! Both schemas are built once, outside every timed region, because both designs intend it: a
//! server builds a schema at startup and validates against `&Schema` per request. `schema_build`
//! is timed separately and informationally. smear's `Scratch` is created once and reused, which is
//! the steady state `smear/tests/validator_allocation.rs` proves allocates nothing — measuring a
//! fresh `Scratch` per iteration would time the warm-up the design exists to eliminate.

use core::hint::black_box;

use apollo_compiler::{
  ExecutableDocument as ApolloExecutable, Schema as ApolloSchema, ast as apollo_ast,
};
use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use smear::{
  lexer::tokora::{Parse as _, Parser as SmearParser},
  parser::graphql::{
    GraphQL,
    ast::ExecutableDocument as SmearExecutable,
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document},
  },
  validator::{Budget, First, Scratch, validate_executable},
};
use smear_apollo_bench::oracle::{Schemas, build_schemas, build_smear_schema, corpus};

/// One (schema, query) pair to time.
struct Workload {
  name: &'static str,
  sdl: &'static str,
  query: &'static str,
}

const WORKLOADS: &[Workload] = &[
  // The design spec's own baseline. Its apollo number is the one the ¼ target was set against.
  Workload {
    name: "realistic",
    sdl: corpus::REALISTIC_SDL,
    query: corpus::REALISTIC_QUERY,
  },
  // A second point with a real federated supergraph behind it: two orders of magnitude more schema
  // for the same size of query, which is where an interned, table-driven schema should show up
  // against string-keyed maps. The query is invalid (it selects `test`), so this row also measures
  // the path a server takes when it rejects.
  Workload {
    name: "supergraph",
    sdl: corpus::SUPERGRAPH,
    query: corpus::SUPERGRAPH_QUERY,
  },
];

fn smear_parse(query: &str) -> SmearExecutable<&str> {
  SmearParser::with_parser::<
    GraphqlLexer<'_, str>,
    SmearExecutable<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(query)
  .expect("the benchmark query parses")
}

fn validators(criterion: &mut Criterion) {
  for workload in WORKLOADS {
    let schemas: Schemas = build_schemas(workload.sdl).unwrap_or_else(|why| {
      panic!(
        "{}: both sides must build the SDL to be compared: {why:?}",
        workload.name
      )
    });

    // Parsed once so the validate-only rows time validation and not the parser. smear's AST
    // borrows the query text, which is `'static` here.
    let smear_document = smear_parse(workload.query);
    let apollo_ast = apollo_ast::Document::parse(workload.query, "query.graphql")
      .expect("the benchmark query parses for apollo too");

    let mut scratch = Scratch::new();
    let budget = Budget::default();

    let mut group = criterion.benchmark_group(format!("validator/{}", workload.name));
    group.throughput(Throughput::Bytes(workload.query.len() as u64));

    // ---- validate only -----------------------------------------------------------------
    group.bench_function("smear/validate", |bencher| {
      bencher.iter(|| {
        let mut sink = First::new();
        let verdict = validate_executable(
          schemas.smear(),
          black_box(&smear_document),
          &mut scratch,
          &budget,
          &mut sink,
        );
        black_box(verdict.is_ok())
      });
    });

    // ---- the two halves of apollo's design-method difference ----------------------------
    group.bench_function("apollo/parse", |bencher| {
      bencher.iter(|| {
        let document = apollo_ast::Document::parse(black_box(workload.query), "query.graphql");
        black_box(document.is_ok())
      });
    });
    group.bench_function("apollo/parse_and_validate", |bencher| {
      bencher.iter(|| {
        let result = ApolloExecutable::parse_and_validate(
          schemas.apollo(),
          black_box(workload.query),
          "query.graphql",
        );
        black_box(result.is_ok())
      });
    });

    // ---- the two halves of the build-excluded difference --------------------------------
    group.bench_function("apollo/to_executable", |bencher| {
      bencher.iter(|| {
        let document = apollo_ast.to_executable(schemas.apollo());
        black_box(document.is_ok())
      });
    });
    group.bench_function("apollo/to_executable_validate", |bencher| {
      bencher.iter(|| {
        let document = apollo_ast.to_executable_validate(schemas.apollo());
        black_box(document.is_ok())
      });
    });

    // ---- end to end --------------------------------------------------------------------
    group.bench_function("smear/parse", |bencher| {
      bencher.iter(|| black_box(smear_parse(black_box(workload.query))));
    });
    group.bench_function("smear/parse_and_validate", |bencher| {
      bencher.iter(|| {
        let document = smear_parse(black_box(workload.query));
        let mut sink = First::new();
        let verdict =
          validate_executable(schemas.smear(), &document, &mut scratch, &budget, &mut sink);
        black_box(verdict.is_ok())
      });
    });

    group.finish();
  }
}

/// Informational: both sides' one-off cost, which a server pays at startup and never again.
fn schema_build(criterion: &mut Criterion) {
  for workload in WORKLOADS {
    let mut group = criterion.benchmark_group(format!("schema_build/{}", workload.name));
    group.throughput(Throughput::Bytes(workload.sdl.len() as u64));
    group.bench_function("smear", |bencher| {
      bencher.iter(|| {
        let schema = build_smear_schema(black_box(workload.sdl));
        black_box(schema.is_ok())
      });
    });
    group.bench_function("apollo", |bencher| {
      bencher.iter(|| {
        let schema = ApolloSchema::parse_and_validate(black_box(workload.sdl), "schema.graphql");
        black_box(schema.is_ok())
      });
    });
    group.finish();
  }
}

criterion_group!(benches, validators, schema_build);
criterion_main!(benches);
