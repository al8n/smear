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
//! # Two smear doors, one apollo control
//!
//! smear has two validator entry points and both are timed here, against the same apollo rows, on
//! the same corpora, in the same process:
//!
//! * **`smear/validate`** takes the AST the syntactic parser builds. It is the fast path, and it
//!   has no counterpart in apollo, whose only parser is a lossless CST.
//! * **`smear/validate_lossless`** takes the rowan CST the lossless parser builds, projects it to
//!   that same AST and runs the same rules over it — [`validate_executable_lossless`] *is*
//!   [`validate_executable`] with [`project_executable_document_recovered`][project] in front. That
//!   is the architecturally matched comparison: `apollo-parser` is a rowan CST too, and
//!   [`ast::Document::parse`][apollo_ast::Document::parse] is a CST build followed by a conversion
//!   into apollo's AST, which is the shape of smear's lossless leg and not of its syntactic one.
//!
//! Neither smear row is a subtraction. The lossless row's timed region is projection plus rules,
//! and that is the whole of what the door costs a caller holding a CST; the projection is not
//! separable from it, because the projection is what makes the CST validatable at all. So
//! `smear/validate_lossless ÷ smear/validate` is the price of losslessness, and
//! `smear/validate_lossless` against apollo's rows is the comparison a consumer of a CST faces.
//!
//! The SDL half is the same pair: `schema_build/smear` parses with the syntactic parser and calls
//! `Schema::build`, `schema_build/smear_lossless` parses to a CST and calls
//! [`validate_schema_lossless`], which projects and reaches that same builder.
//!
//! # The lossless rows are gated on the corpus
//!
//! A lossless row means something only where the door answers what the syntactic door answers.
//! Before a workload gets one, three things are checked outside every timed region: that the
//! lossless parse reports no grammar error, that the projection is complete
//! ([`Recovery::is_complete`][complete]), and that the two doors return the same verdict. A
//! workload that fails any of them keeps its syntactic and apollo rows, loses its lossless ones,
//! and says why on stderr — a missing row is better than an invented one.
//!
//! # What is warm and what is not
//!
//! Both schemas are built once, outside every timed region, because both designs intend it: a
//! server builds a schema at startup and validates against `&Schema` per request. `schema_build`
//! is timed separately and informationally. smear's `Scratch` is created once and reused, which is
//! the steady state `smear/tests/validator_allocation.rs` proves allocates nothing — measuring a
//! fresh `Scratch` per iteration would time the warm-up the design exists to eliminate.
//!
//! The lossless leg reuses the same `Scratch` for the same reason, and its `Parse` is built once
//! outside the validate-only row so that row times the door and not the CST parser. The
//! `parse_lossless` and `parse_and_validate_lossless` rows are where the CST build is paid for.
//!
//! [project]: smear::parser::graphql::lossless::project_executable_document_recovered
//! [complete]: smear::validator::Recovery::is_complete

// The shared harness. It is a module compiled into this target rather than a library
// linked into it; `support/mod.rs` carries why, and why this file sits under `benches/`.
mod support;

use core::hint::black_box;

use crate::support::oracle::{Schemas, build_schemas, build_smear_schema, corpus};
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
    lossless::{Parse as LosslessParse, parse_executable_document, parse_type_system_document},
    syntactic::{GraphqlLexer, executable_document},
  },
  validator::{
    Budget, First, Schema, Scratch, validate_executable, validate_executable_lossless,
    validate_schema_lossless,
  },
};

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

/// Why the lossless executable door does not describe this workload the way the syntactic one
/// does, or `None` if it does.
///
/// `Some` names the reason, and the caller drops the workload's lossless rows rather than reporting
/// a number for a door that answered a different question. `syntactic_ok` is the verdict
/// [`validate_executable`] already returned for the same schema and the same bytes.
fn executable_lossless_blocker(
  schema: &Schema,
  parse: &LosslessParse,
  query: &str,
  scratch: &mut Scratch,
  budget: &Budget,
  syntactic_ok: bool,
) -> Option<String> {
  if parse.has_errors() {
    return Some("the lossless parser reports a grammar error".to_owned());
  }

  let mut sink = First::new();
  let verdict = validate_executable_lossless(schema, parse, query, scratch, budget, &mut sink);
  let recovery = match &verdict {
    Ok(recovery) => Some(*recovery),
    Err(invalid) => invalid.recovery(),
  };

  match recovery {
    // The budget refused before the projection ran, so there is no AST for the doors to disagree
    // about. Reported for the same reason a partial projection is: this comparison is only
    // meaningful over a document both doors actually looked at.
    None => return Some("the validation budget refused before the projection".to_owned()),
    Some(recovery) if !recovery.is_complete() => {
      return Some(format!("the projection is partial ({recovery})"));
    }
    Some(_) => {}
  }
  if verdict.is_ok() != syntactic_ok {
    return Some(format!(
      "the doors disagree: syntactic says {}, lossless says {}",
      if syntactic_ok { "valid" } else { "invalid" },
      if verdict.is_ok() { "valid" } else { "invalid" },
    ));
  }
  None
}

/// [`executable_lossless_blocker`]'s twin one section of the specification over.
///
/// Draft §3 lives in one builder both doors reach, so the verdict cannot differ once the same
/// document arrives; what can differ is the parse and the projection, which is what the two checks
/// here are. A refusal from the builder therefore says the projected document was not the one
/// [`build_smear_schema`] builds from, and is reported for the same reason.
fn schema_lossless_blocker(sdl: &str) -> Option<String> {
  let parse = parse_type_system_document(sdl);
  if parse.has_errors() {
    return Some("the lossless parser reports a grammar error".to_owned());
  }

  match validate_schema_lossless(&parse, sdl) {
    Ok((_, recovery)) if !recovery.is_complete() => {
      Some(format!("the projection is partial ({recovery})"))
    }
    Ok(_) => None,
    Err(errors) => Some(format!("the §3 pass refuses the projected SDL: {errors}")),
  }
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
    // borrows the query text, which is `'static` here, and so does the CST's `source` argument.
    let smear_document = smear_parse(workload.query);
    let lossless_parse = parse_executable_document(workload.query);
    let apollo_ast = apollo_ast::Document::parse(workload.query, "query.graphql")
      .expect("the benchmark query parses for apollo too");

    let mut scratch = Scratch::new();
    let budget = Budget::default();

    let syntactic_ok = {
      let mut sink = First::new();
      validate_executable(
        schemas.smear(),
        &smear_document,
        &mut scratch,
        &budget,
        &mut sink,
      )
      .is_ok()
    };
    let lossless_blocker = executable_lossless_blocker(
      schemas.smear(),
      &lossless_parse,
      workload.query,
      &mut scratch,
      &budget,
      syntactic_ok,
    );
    if let Some(why) = &lossless_blocker {
      eprintln!("validator/{}: no lossless rows — {why}", workload.name);
    }
    let lossless = lossless_blocker.is_none();

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
    if lossless {
      // Projection plus rules. The projection is inside the timed region because it is what the
      // door does — see the header.
      group.bench_function("smear/validate_lossless", |bencher| {
        bencher.iter(|| {
          let mut sink = First::new();
          let verdict = validate_executable_lossless(
            schemas.smear(),
            black_box(&lossless_parse),
            workload.query,
            &mut scratch,
            &budget,
            &mut sink,
          );
          black_box(verdict.is_ok())
        });
      });
    }

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
    if lossless {
      group.bench_function("smear/parse_lossless", |bencher| {
        bencher.iter(|| black_box(parse_executable_document(black_box(workload.query))));
      });
    }
    group.bench_function("smear/parse_and_validate", |bencher| {
      bencher.iter(|| {
        let document = smear_parse(black_box(workload.query));
        let mut sink = First::new();
        let verdict =
          validate_executable(schemas.smear(), &document, &mut scratch, &budget, &mut sink);
        black_box(verdict.is_ok())
      });
    });
    if lossless {
      group.bench_function("smear/parse_and_validate_lossless", |bencher| {
        bencher.iter(|| {
          let parse = parse_executable_document(black_box(workload.query));
          let mut sink = First::new();
          let verdict = validate_executable_lossless(
            schemas.smear(),
            &parse,
            workload.query,
            &mut scratch,
            &budget,
            &mut sink,
          );
          black_box(verdict.is_ok())
        });
      });
    }

    group.finish();
  }
}

/// Informational: both sides' one-off cost, which a server pays at startup and never again.
///
/// All three rows parse and then run draft §3, because that is what a server hands an SDL file to.
/// `smear_lossless` differs from `smear` only in which parser produced the document the one §3
/// builder reads, which is the same difference the executable group's lossless rows measure.
fn schema_build(criterion: &mut Criterion) {
  for workload in WORKLOADS {
    let blocker = schema_lossless_blocker(workload.sdl);
    if let Some(why) = &blocker {
      eprintln!("schema_build/{}: no lossless row — {why}", workload.name);
    }
    let lossless = blocker.is_none();

    let mut group = criterion.benchmark_group(format!("schema_build/{}", workload.name));
    group.throughput(Throughput::Bytes(workload.sdl.len() as u64));
    group.bench_function("smear", |bencher| {
      bencher.iter(|| {
        let schema = build_smear_schema(black_box(workload.sdl));
        black_box(schema.is_ok())
      });
    });
    if lossless {
      group.bench_function("smear_lossless", |bencher| {
        bencher.iter(|| {
          let parse = parse_type_system_document(black_box(workload.sdl));
          let schema = validate_schema_lossless(&parse, workload.sdl);
          black_box(schema.is_ok())
        });
      });
    }
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
