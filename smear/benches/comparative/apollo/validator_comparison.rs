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
//! # The syntactic road, two lossless roads, one apollo control
//!
//! smear's validator has one entry point, [`validate_executable`], and it reads the AST either
//! parser layer answers. The rows time it on each road, against the same apollo rows, on the same
//! corpora, in the same process:
//!
//! * **`smear/validate`** takes the AST the syntactic parser builds. It is the fast path, and it
//!   has no counterpart in apollo, whose only parser is a lossless CST.
//! * **`smear/validate_lossless`** takes a rowan `Parse` kept from outside the timed region, the
//!   road of a caller that kept one: [`project_executable_document_recovered`][project] checks that
//!   the parse is of the text and projects it to that same AST, and the same rules run over it.
//!   That is the architecturally matched comparison: `apollo-parser` is a rowan CST too, and
//!   [`ast::Document::parse`][apollo_ast::Document::parse] is a CST build followed by a conversion
//!   into apollo's AST, which is the shape of smear's lossless leg and not of its syntactic one.
//!
//! Neither smear row is a subtraction. The lossless row's timed region is projection plus rules,
//! and that is the whole of what a caller holding a `Parse` pays; the projection is not separable
//! from it, because the projection is what makes the tree validatable at all. So
//! `smear/validate_lossless ÷ smear/validate` is the price of losslessness, and
//! `smear/validate_lossless` against apollo's rows is the comparison a consumer of a CST faces.
//!
//! `smear/parse_and_validate_lossless` is the other lossless road, from the text: the lossless
//! layer, `Lossless::new().executable_document`, parses and projects its own tree, and the
//! validator reads the document it answers. The SDL half is the same pair: `schema_build/smear`
//! parses with the syntactic parser and calls `Schema::build`, and `schema_build/smear_lossless`
//! reads the SDL with the lossless layer and hands its document to that same builder.
//!
//! # The lossless rows are gated on the corpus
//!
//! A lossless row means something only where the lossless layer answers what the syntactic parser
//! answers. Before a workload gets one, these are checked outside every timed region: that the
//! lossless layer answers a document; that the text is one ([`Parsed::is_valid`][valid]); that the
//! projection is complete ([`Recovery::is_complete`][complete]); that the validator's verdict, or
//! the builder's, is the syntactic document's; and, for the executable rows, that the kept `Parse`
//! projects completely and validates to the same verdict too. A workload that fails any of them
//! keeps its syntactic and apollo rows, loses its lossless ones, and says why on stderr — a missing
//! row is better than an invented one.
//!
//! # What the lossless rows measure across the removal of the validator's lossless doors
//!
//! The end-to-end rows, `smear/parse_and_validate_lossless` and `schema_build/…/smear_lossless`,
//! time the same task on either side of that change, from a text to a validated document or a
//! built schema, and across it they are the measurement of the lossless road. Before it, the road
//! was the lossless parse, then the door's check that the parse and the text were one document,
//! then its projection, and on the executable row a prepayment for that projection out of
//! `validation_work`. After it, the road is the lossless layer's parse and its projection of its
//! own tree, with no pair to check and nothing prepaid. `smear/validate_lossless` still checks its
//! pair, in the recovering projection, and prepays nothing; `smear/parse_lossless` times the same
//! parse it did.
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
//! outside the validate-only row so that row times the projection and the rules and not the CST
//! parser. The `parse_lossless` and `parse_and_validate_lossless` rows are where the CST build is
//! paid for.
//!
//! [project]: smear::parser::graphql::lossless::project_executable_document_recovered
//! [valid]: smear::parser::layer::Parsed::is_valid
//! [complete]: smear::parser::graphql::Recovery::is_complete

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
    GraphQL, Lossless,
    ast::ExecutableDocument as SmearExecutable,
    error::GraphqlErrors,
    lossless::{
      Parse as LosslessParse, parse_executable_document, project_executable_document_recovered,
    },
    syntactic::{GraphqlLexer, executable_document},
  },
  validator::{Budget, First, Schema, Scratch, validate_executable},
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

/// Why the lossless roads to a validated document do not describe this workload the way the
/// syntactic one does, or `None` if they do.
///
/// `Some` names the reason, and the caller drops the workload's lossless rows rather than reporting
/// a number for a road that answered a different question. `syntactic_ok` is the verdict
/// [`validate_executable`] already returned for the same schema and the same bytes, and `parse` is
/// the `Parse` the validate-only row keeps.
fn executable_lossless_blocker(
  schema: &Schema,
  parse: &LosslessParse,
  query: &str,
  scratch: &mut Scratch,
  budget: &Budget,
  syntactic_ok: bool,
) -> Option<String> {
  let disagree = |lossless_ok: bool, road: &str| -> Option<String> {
    (lossless_ok != syntactic_ok).then(|| {
      format!(
        "the documents disagree: syntactic says {}, {road} says {}",
        if syntactic_ok { "valid" } else { "invalid" },
        if lossless_ok { "valid" } else { "invalid" },
      )
    })
  };

  // The road from the text: the lossless layer parses and projects its own tree.
  let Ok(parsed) = Lossless::new().executable_document(query, &mut ()) else {
    return Some("the lossless layer answers no document".to_owned());
  };
  if !parsed.is_valid() {
    return Some("the lossless layer reports a syntax error".to_owned());
  }
  let recovery = parsed.recovery();
  if !recovery.is_complete() {
    return Some(format!("the projection is partial ({recovery})"));
  }
  let mut sink = First::new();
  let verdict = validate_executable(schema, parsed.document(), scratch, budget, &mut sink);
  if let Some(why) = disagree(verdict.is_ok(), "the lossless layer") {
    return Some(why);
  }

  // The road of a caller that kept a `Parse`: the recovering projection, which checks the pair.
  let (document, recovery) = match project_executable_document_recovered(parse, query) {
    Ok(projected) => projected,
    Err(unverified) => return Some(format!("the kept parse is not of the query: {unverified}")),
  };
  if !recovery.is_complete() {
    return Some(format!("the kept parse projects partially ({recovery})"));
  }
  let mut sink = First::new();
  let verdict = validate_executable(schema, &document, scratch, budget, &mut sink);
  disagree(verdict.is_ok(), "the kept parse")
}

/// [`executable_lossless_blocker`]'s twin one section of the specification over.
///
/// Draft §3 lives in one builder both layers' documents reach, so the verdict cannot differ once
/// the same document arrives; what can differ is the parse and the projection, which is what the
/// checks here are. A refusal from the builder therefore says the projected document was not the
/// one [`build_smear_schema`] builds from, and is reported for the same reason.
fn schema_lossless_blocker(sdl: &str) -> Option<String> {
  let Ok(parsed) = Lossless::new().type_system_document(sdl, &mut ()) else {
    return Some("the lossless layer answers no document".to_owned());
  };
  if !parsed.is_valid() {
    return Some("the lossless layer reports a syntax error".to_owned());
  }
  let recovery = parsed.recovery();
  if !recovery.is_complete() {
    return Some(format!("the projection is partial ({recovery})"));
  }
  match Schema::build(parsed.document()) {
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
      // Projection plus rules: the road of a caller that kept a `Parse`. The projection is inside
      // the timed region because it is what that caller pays for — see the header.
      group.bench_function("smear/validate_lossless", |bencher| {
        bencher.iter(|| {
          let (document, _) =
            project_executable_document_recovered(black_box(&lossless_parse), workload.query)
              .expect("the kept parse is of the query");
          let mut sink = First::new();
          let verdict =
            validate_executable(schemas.smear(), &document, &mut scratch, &budget, &mut sink);
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
          let parsed = Lossless::new()
            .executable_document(black_box(workload.query), &mut ())
            .expect("the benchmark query is short enough for a tree");
          let mut sink = First::new();
          let verdict = validate_executable(
            schemas.smear(),
            parsed.document(),
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
/// `smear_lossless` differs from `smear` only in which parser layer produced the document the one
/// §3 builder reads, which is the same difference the executable group's end-to-end lossless row
/// measures.
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
          let parsed = Lossless::new()
            .type_system_document(black_box(workload.sdl), &mut ())
            .expect("the benchmark SDL is short enough for a tree");
          let schema = Schema::build(parsed.document());
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
