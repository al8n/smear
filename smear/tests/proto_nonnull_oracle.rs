#![cfg(feature = "proto")]

//! The draft §6.4.4 oracle: `graphql-js`'s `nonnull-test.ts`, hand-ported case for case.
//!
//! # Why an oracle at all, and why this one
//!
//! Null propagation is the clause of the specification that is most often implemented wrongly, and
//! a wrong implementation is *silently* wrong: nothing throws, no assertion trips, the response is
//! simply a little bit incorrect in a way that looks exactly like a correct response. The
//! validator could be gated against `apollo-compiler` because apollo validates; nothing in the Rust
//! ecosystem executes GraphQL in a way a differential could compare against, so the reference
//! implementation's own suite is the closest thing to a conformance corpus that exists.
//!
//! # Hand-ported, and what that costs
//!
//! **Mechanical extraction was measured and rejected.** Of the 179 `it(...)` blocks across
//! `graphql-js`'s eleven query-scope execution test files, **1** is self-contained enough to lift
//! without running JavaScript. The three blockers, in descending order of how fatal they are:
//!
//! 1. **The field values are code, not data.** Every case gets its values from a `resolve:`
//!    closure, a method on a root object, a `Promise`, a thrown `Error`, a `Set` or a generator. A
//!    corpus format can carry a query and an expected result; it cannot carry a closure. This file's
//!    upstream is a good example — `throwingData.sync()` is `throw syncError`, and `nullingData`
//!    differs from it only in the *bodies* of eight identically-named methods.
//! 2. **The schema is usually programmatic.** 164 of the 179 blocks live in a file that builds at
//!    least one schema with `new GraphQLSchema` / `new GraphQLObjectType` rather than SDL. Turning
//!    one back into SDL means calling `printSchema`, which means running JavaScript.
//! 3. **The expectation is often computed.** `syncError.message` is a binding, not a literal; the
//!    async half of every case in this file is defined as `patchData(syncResult)` — a *transform of
//!    the other run's output* — so for those there is no literal to extract at all.
//!
//! So every case below was read and transcribed by hand. That is a real cost and it buys one real
//! thing: [`upstream_has_not_drifted`] can check the transcription mechanically. Each [`Case`]
//! records the upstream line and a verbatim fragment of it, and with `SMEAR_GRAPHQL_JS` pointing at
//! a checkout the gate re-reads the file and fails if any anchor has moved — plus fails if upstream
//! has *added* a case this port does not have. A hand-port that cannot detect its own staleness is
//! a snapshot; one that can is a corpus.
//!
//! # Coverage, stated as a number
//!
//! All **17** of `nonnull-test.ts`'s `it(...)` blocks are here. The eleven that exercise
//! propagation are the point; the six under `Handles non-null argument` are draft §6.4.1's field
//! errors, which reach the response through the same path and would otherwise have no gate.
//!
//! # Three deliberate divergences, each with a reason
//!
//! - **Only the synchronous half of each case is ported.** Upstream runs every case twice, the
//!   second time with `sync` renamed to `promise` throughout, and asserts the two agree. `proto` is
//!   Sans-I/O: it has no notion of a value that is not yet available, and the driver answers
//!   `promise*` and `sync*` fields identically. The rename is therefore a no-op here, and
//!   upstream's own assertion is what says porting one half loses nothing.
//! - **Errors are compared as a set, ordered by path.** `graphql-js`'s order in the twelve-error
//!   case is an artifact of promise scheduling — the `sync` errors come out before the `promise`
//!   ones regardless of document order. Draft §7.1.2 does not fix the order of `errors`, so
//!   comparing the multiset is comparing what the specification actually says.
//! - **§6.1 `CoerceVariableValues` is applied by this harness, not by `proto`.** One case
//!   (`succeeds when missing variable has default value`) turns on a variable *default*, which
//!   §6.1 resolves before execution begins and which `proto` deliberately leaves to the driver. The
//!   harness supplies the value §6.1 would have produced, and [`VARIABLE_DEFAULTS_ARE_THE_HARNESS`]
//!   records that it is doing so.

use std::collections::BTreeSet;

use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, InputValue, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  proto::{ArgumentSource, Executor, Leaf, Node, Values},
  validator::Schema,
};

/// The `graphql-js` commit this port was read from: the `v16.11.0` tag.
///
/// CI fetches exactly this SHA. A different checkout will still run, and
/// [`upstream_has_not_drifted`] is what says whether it agrees.
const UPSTREAM_COMMIT: &str = "c18e9f6aada9ae086ddf836e4d822cf1426f3868";

/// The file every [`Case::line`] below indexes.
const UPSTREAM_FILE: &str = "src/execution/__tests__/nonnull-test.ts";

/// See the module header's third divergence.
const VARIABLE_DEFAULTS_ARE_THE_HARNESS: &str =
  "draft §6.1 CoerceVariableValues is outside `proto`; the harness supplies what it would produce";

// ------------------------------------------------------------------------------------------
// the driver
// ------------------------------------------------------------------------------------------

/// The driver's value representation.
///
/// `Data` is upstream's `throwingData` / `nullingData` object, which is its own `syncNest`. It
/// carries no fields because the *behaviour* is keyed on the field name, exactly as the two
/// JavaScript objects are.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Val {
  Null,
  Str(String),
  Data,
}

/// Which of upstream's two root objects is in play.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Mode {
  /// `nullingData`: the four leaf fields return `null`.
  Nulling,
  /// `throwingData`: the four leaf fields raise a field error named after themselves.
  Throwing,
}

struct Space {
  variables: Vec<(String, Val)>,
}

impl Values for Space {
  type Value = Val;

  fn is_null(&self, value: &Val) -> bool {
    matches!(value, Val::Null)
  }

  fn as_bool(&self, _: &Val) -> Option<bool> {
    None
  }

  fn list_len(&self, _: &Val) -> Option<usize> {
    None
  }

  fn list_item(&mut self, _: &Val, _: usize) -> Val {
    Val::Null
  }

  fn type_name<'a>(&'a self, _: &'a Val) -> Option<&'a str> {
    None
  }

  fn coerce_leaf(&mut self, value: Val, _: Leaf<'_>) -> Option<Val> {
    Some(value)
  }

  fn variable(&mut self, name: &str) -> Option<Val> {
    self
      .variables
      .iter()
      .find(|(declared, _)| declared == name)
      .map(|(_, value)| value.clone())
  }
}

// ------------------------------------------------------------------------------------------
// the corpus
// ------------------------------------------------------------------------------------------

/// One expected `errors` entry.
struct ExpectedError {
  message: &'static str,
  /// The draft §7.1.2 response path, rendered as `a.b.0.c`.
  path: &'static str,
  /// Upstream's sole `locations` entry, 1-based, recomputed here from the query text and the span.
  ///
  /// One rather than a list because none of the ported queries selects a response key twice, so
  /// upstream emits exactly one location for every error in this corpus; [`run`] asserts that
  /// rather than assuming it.
  line: u32,
  column: u32,
}

/// One ported `it(...)` block.
struct Case {
  /// Upstream's `it(...)` title, verbatim.
  name: &'static str,
  /// The 1-based line of that `it(` in [`UPSTREAM_FILE`].
  line: u32,
  /// A verbatim fragment of that line, for [`upstream_has_not_drifted`].
  anchor: &'static str,
  /// Which schema the case runs against.
  sdl: &'static str,
  /// The query, including the leading newline of upstream's template literal, so a recomputed
  /// line and column can be compared with upstream's.
  query: &'static str,
  mode: Mode,
  /// The request's variable values, after §6.1.
  variables: &'static [(&'static str, Val2)],
  /// `data`, rendered by [`render`].
  data: &'static str,
  errors: &'static [ExpectedError],
}

/// A `const`-constructible [`Val`], because `Val::Str` needs an allocation.
#[derive(Clone, Copy, Debug)]
enum Val2 {
  Null,
  Str(&'static str),
}

impl From<Val2> for Val {
  fn from(value: Val2) -> Self {
    match value {
      Val2::Null => Val::Null,
      Val2::Str(text) => Val::Str(text.to_owned()),
    }
  }
}

/// `nonnull-test.ts:92-107`, verbatim.
const DATA_SDL: &str = r#"
  type DataType {
    sync: String
    syncNonNull: String!
    promise: String
    promiseNonNull: String!
    syncNest: DataType
    syncNonNullNest: DataType!
    promiseNest: DataType
    promiseNonNullNest: DataType!
  }

  schema {
    query: DataType
  }
"#;

/// `nonnull-test.ts:528-543`, which upstream builds with `new GraphQLSchema`. Rendered as the SDL
/// it denotes — the one place in this file where the port is a translation rather than a copy, and
/// the reason `Handles non-null argument` was reachable at all.
const ARG_SDL: &str = r#"
  type Query {
    withNonNullArg(cannotBeNull: String!): String
  }
"#;

const Q_SYNC: &str = r#"
      {
        sync
      }
    "#;

const Q_SYNC_NEST_NONNULL: &str = r#"
      {
        syncNest {
          syncNonNull,
        }
      }
    "#;

const Q_COMPLEX_TREE: &str = r#"
      {
        syncNest {
          sync
          promise
          syncNest { sync promise }
          promiseNest { sync promise }
        }
        promiseNest {
          sync
          promise
          syncNest { sync promise }
          promiseNest { sync promise }
        }
      }
    "#;

const Q_LONG_CHAIN: &str = r#"
      {
        syncNest {
          syncNonNullNest {
            promiseNonNullNest {
              syncNonNullNest {
                promiseNonNullNest {
                  syncNonNull
                }
              }
            }
          }
        }
        promiseNest {
          syncNonNullNest {
            promiseNonNullNest {
              syncNonNullNest {
                promiseNonNullNest {
                  syncNonNull
                }
              }
            }
          }
        }
        anotherNest: syncNest {
          syncNonNullNest {
            promiseNonNullNest {
              syncNonNullNest {
                promiseNonNullNest {
                  promiseNonNull
                }
              }
            }
          }
        }
        anotherPromiseNest: promiseNest {
          syncNonNullNest {
            promiseNonNullNest {
              syncNonNullNest {
                promiseNonNullNest {
                  promiseNonNull
                }
              }
            }
          }
        }
      }
    "#;

const Q_TOP_NONNULL: &str = r#"
      {
        syncNonNull
      }
    "#;

const Q_ARG_LITERAL: &str = r#"
          query {
            withNonNullArg (cannotBeNull: "literal value")
          }
        "#;

const Q_ARG_VARIABLE: &str = r#"
          query ($testVar: String!) {
            withNonNullArg (cannotBeNull: $testVar)
          }
        "#;

const Q_ARG_DEFAULTED_VARIABLE: &str = r#"
          query ($testVar: String = "default value") {
            withNonNullArg (cannotBeNull: $testVar)
          }
        "#;

const Q_ARG_MISSING: &str = r#"
          query {
            withNonNullArg
          }
        "#;

const Q_ARG_NULL: &str = r#"
          query {
            withNonNullArg(cannotBeNull: null)
          }
        "#;

const Q_ARG_UNSUPPLIED_VARIABLE: &str = r#"
          query ($testVar: String) {
            withNonNullArg(cannotBeNull: $testVar)
          }
        "#;

const NULL_NONNULL_SYNC: &str = "Cannot return null for non-nullable field DataType.syncNonNull.";
const NULL_NONNULL_PROMISE: &str =
  "Cannot return null for non-nullable field DataType.promiseNonNull.";

const CASES: &[Case] = &[
  Case {
    name: "nulls a nullable field / that returns null",
    line: 147,
    anchor: "it('that returns null', async () => {",
    sdl: DATA_SDL,
    query: Q_SYNC,
    mode: Mode::Nulling,
    variables: &[],
    data: r#"{"sync":null}"#,
    errors: &[],
  },
  Case {
    name: "nulls a nullable field / that throws",
    line: 154,
    anchor: "it('that throws', async () => {",
    sdl: DATA_SDL,
    query: Q_SYNC,
    mode: Mode::Throwing,
    variables: &[],
    data: r#"{"sync":null}"#,
    errors: &[ExpectedError {
      message: "sync",
      path: "sync",
      line: 3,
      column: 9,
    }],
  },
  Case {
    name: "nulls a returned object that contains a non-nullable field / that returns null",
    line: 178,
    anchor: "it('that returns null', async () => {",
    sdl: DATA_SDL,
    query: Q_SYNC_NEST_NONNULL,
    mode: Mode::Nulling,
    variables: &[],
    data: r#"{"syncNest":null}"#,
    errors: &[ExpectedError {
      message: NULL_NONNULL_SYNC,
      path: "syncNest.syncNonNull",
      line: 4,
      column: 11,
    }],
  },
  Case {
    name: "nulls a returned object that contains a non-nullable field / that throws",
    line: 193,
    anchor: "it('that throws', async () => {",
    sdl: DATA_SDL,
    query: Q_SYNC_NEST_NONNULL,
    mode: Mode::Throwing,
    variables: &[],
    data: r#"{"syncNest":null}"#,
    errors: &[ExpectedError {
      message: "syncNonNull",
      path: "syncNest.syncNonNull",
      line: 4,
      column: 11,
    }],
  },
  Case {
    name: "nulls a complex tree of nullable fields, each / that returns null",
    line: 240,
    anchor: "it('that returns null', async () => {",
    sdl: DATA_SDL,
    query: Q_COMPLEX_TREE,
    mode: Mode::Nulling,
    variables: &[],
    data: COMPLEX_TREE_DATA,
    errors: &[],
  },
  Case {
    name: "nulls a complex tree of nullable fields, each / that throws",
    line: 245,
    anchor: "it('that throws', async () => {",
    sdl: DATA_SDL,
    query: Q_COMPLEX_TREE,
    mode: Mode::Throwing,
    variables: &[],
    data: COMPLEX_TREE_DATA,
    errors: &[
      ExpectedError {
        message: "sync",
        path: "syncNest.sync",
        line: 4,
        column: 11,
      },
      ExpectedError {
        message: "promise",
        path: "syncNest.promise",
        line: 5,
        column: 11,
      },
      ExpectedError {
        message: "sync",
        path: "syncNest.syncNest.sync",
        line: 6,
        column: 22,
      },
      ExpectedError {
        message: "promise",
        path: "syncNest.syncNest.promise",
        line: 6,
        column: 27,
      },
      ExpectedError {
        message: "sync",
        path: "syncNest.promiseNest.sync",
        line: 7,
        column: 25,
      },
      ExpectedError {
        message: "promise",
        path: "syncNest.promiseNest.promise",
        line: 7,
        column: 30,
      },
      ExpectedError {
        message: "sync",
        path: "promiseNest.sync",
        line: 10,
        column: 11,
      },
      ExpectedError {
        message: "promise",
        path: "promiseNest.promise",
        line: 11,
        column: 11,
      },
      ExpectedError {
        message: "sync",
        path: "promiseNest.syncNest.sync",
        line: 12,
        column: 22,
      },
      ExpectedError {
        message: "promise",
        path: "promiseNest.syncNest.promise",
        line: 12,
        column: 27,
      },
      ExpectedError {
        message: "sync",
        path: "promiseNest.promiseNest.sync",
        line: 13,
        column: 25,
      },
      ExpectedError {
        message: "promise",
        path: "promiseNest.promiseNest.promise",
        line: 13,
        column: 30,
      },
    ],
  },
  Case {
    name: "nulls the first nullable object after a field in a long chain of non-null fields / \
           that returns null",
    line: 371,
    anchor: "it('that returns null', async () => {",
    sdl: DATA_SDL,
    query: Q_LONG_CHAIN,
    mode: Mode::Nulling,
    variables: &[],
    data: LONG_CHAIN_DATA,
    errors: &[
      ExpectedError {
        message: NULL_NONNULL_SYNC,
        path: "syncNest.syncNonNullNest.promiseNonNullNest.syncNonNullNest.promiseNonNullNest.\
               syncNonNull",
        line: 8,
        column: 19,
      },
      ExpectedError {
        message: NULL_NONNULL_SYNC,
        path: "promiseNest.syncNonNullNest.promiseNonNullNest.syncNonNullNest.\
               promiseNonNullNest.syncNonNull",
        line: 19,
        column: 19,
      },
      ExpectedError {
        message: NULL_NONNULL_PROMISE,
        path: "anotherNest.syncNonNullNest.promiseNonNullNest.syncNonNullNest.\
               promiseNonNullNest.promiseNonNull",
        line: 30,
        column: 19,
      },
      ExpectedError {
        message: NULL_NONNULL_PROMISE,
        path: "anotherPromiseNest.syncNonNullNest.promiseNonNullNest.syncNonNullNest.\
               promiseNonNullNest.promiseNonNull",
        line: 41,
        column: 19,
      },
    ],
  },
  Case {
    name: "nulls the first nullable object after a field in a long chain of non-null fields / \
           that throws",
    line: 432,
    anchor: "it('that throws', async () => {",
    sdl: DATA_SDL,
    query: Q_LONG_CHAIN,
    mode: Mode::Throwing,
    variables: &[],
    data: LONG_CHAIN_DATA,
    errors: &[
      ExpectedError {
        message: "syncNonNull",
        path: "syncNest.syncNonNullNest.promiseNonNullNest.syncNonNullNest.promiseNonNullNest.\
               syncNonNull",
        line: 8,
        column: 19,
      },
      ExpectedError {
        message: "syncNonNull",
        path: "promiseNest.syncNonNullNest.promiseNonNullNest.syncNonNullNest.\
               promiseNonNullNest.syncNonNull",
        line: 19,
        column: 19,
      },
      ExpectedError {
        message: "promiseNonNull",
        path: "anotherNest.syncNonNullNest.promiseNonNullNest.syncNonNullNest.\
               promiseNonNullNest.promiseNonNull",
        line: 30,
        column: 19,
      },
      ExpectedError {
        message: "promiseNonNull",
        path: "anotherPromiseNest.syncNonNullNest.promiseNonNullNest.syncNonNullNest.\
               promiseNonNullNest.promiseNonNull",
        line: 41,
        column: 19,
      },
    ],
  },
  Case {
    name: "nulls the top level if non-nullable field / that returns null",
    line: 497,
    anchor: "it('that returns null', async () => {",
    sdl: DATA_SDL,
    query: Q_TOP_NONNULL,
    mode: Mode::Nulling,
    variables: &[],
    data: "null",
    errors: &[ExpectedError {
      message: NULL_NONNULL_SYNC,
      path: "syncNonNull",
      line: 3,
      column: 9,
    }],
  },
  Case {
    name: "nulls the top level if non-nullable field / that throws",
    line: 512,
    anchor: "it('that throws', async () => {",
    sdl: DATA_SDL,
    query: Q_TOP_NONNULL,
    mode: Mode::Throwing,
    variables: &[],
    data: "null",
    errors: &[ExpectedError {
      message: "syncNonNull",
      path: "syncNonNull",
      line: 3,
      column: 9,
    }],
  },
  Case {
    name: "Handles non-null argument / succeeds when passed non-null literal value",
    line: 545,
    anchor: "it('succeeds when passed non-null literal value', () => {",
    sdl: ARG_SDL,
    query: Q_ARG_LITERAL,
    mode: Mode::Nulling,
    variables: &[],
    data: r#"{"withNonNullArg":"Passed: literal value"}"#,
    errors: &[],
  },
  Case {
    name: "Handles non-null argument / succeeds when passed non-null variable value",
    line: 562,
    anchor: "it('succeeds when passed non-null variable value', () => {",
    sdl: ARG_SDL,
    query: Q_ARG_VARIABLE,
    mode: Mode::Nulling,
    variables: &[("testVar", Val2::Str("variable value"))],
    data: r#"{"withNonNullArg":"Passed: variable value"}"#,
    errors: &[],
  },
  Case {
    name: "Handles non-null argument / succeeds when missing variable has default value",
    line: 582,
    anchor: "it('succeeds when missing variable has default value', () => {",
    sdl: ARG_SDL,
    query: Q_ARG_DEFAULTED_VARIABLE,
    // The request supplies nothing; `default value` is what draft §6.1 produces from the
    // variable's declared default, which this harness applies. See the module header.
    variables: &[("testVar", Val2::Str("default value"))],
    mode: Mode::Nulling,
    data: r#"{"withNonNullArg":"Passed: default value"}"#,
    errors: &[],
  },
  Case {
    name: "Handles non-null argument / field error when missing non-null arg",
    line: 602,
    anchor: "it('field error when missing non-null arg', () => {",
    sdl: ARG_SDL,
    query: Q_ARG_MISSING,
    mode: Mode::Nulling,
    variables: &[],
    data: r#"{"withNonNullArg":null}"#,
    errors: &[ExpectedError {
      message: "Argument \"cannotBeNull\" of required type \"String!\" was not provided.",
      path: "withNonNullArg",
      line: 3,
      column: 13,
    }],
  },
  Case {
    name: "Handles non-null argument / field error when non-null arg provided null",
    line: 629,
    anchor: "it('field error when non-null arg provided null', () => {",
    sdl: ARG_SDL,
    query: Q_ARG_NULL,
    mode: Mode::Nulling,
    variables: &[],
    data: r#"{"withNonNullArg":null}"#,
    errors: &[ExpectedError {
      message: "Argument \"cannotBeNull\" of non-null type \"String!\" must not be null.",
      path: "withNonNullArg",
      line: 3,
      column: 42,
    }],
  },
  Case {
    name: "Handles non-null argument / field error when non-null arg not provided variable value",
    line: 656,
    anchor: "it('field error when non-null arg not provided variable value', () => {",
    sdl: ARG_SDL,
    query: Q_ARG_UNSUPPLIED_VARIABLE,
    mode: Mode::Nulling,
    variables: &[],
    data: r#"{"withNonNullArg":null}"#,
    errors: &[ExpectedError {
      message: "Argument \"cannotBeNull\" of required type \"String!\" was provided the variable \
                \"$testVar\" which was not provided a runtime value.",
      path: "withNonNullArg",
      line: 3,
      column: 42,
    }],
  },
  Case {
    name: "Handles non-null argument / field error when non-null arg provided variable with \
           explicit null value",
    line: 686,
    anchor: "it('field error when non-null arg provided variable with explicit null value', () => {",
    sdl: ARG_SDL,
    query: Q_ARG_DEFAULTED_VARIABLE,
    mode: Mode::Nulling,
    variables: &[("testVar", Val2::Null)],
    data: r#"{"withNonNullArg":null}"#,
    errors: &[ExpectedError {
      message: "Argument \"cannotBeNull\" of non-null type \"String!\" must not be null.",
      path: "withNonNullArg",
      line: 3,
      column: 43,
    }],
  },
];

/// `nonnull-test.ts:225-238`.
const COMPLEX_TREE_DATA: &str = concat!(
  r#"{"syncNest":{"sync":null,"promise":null,"syncNest":{"sync":null,"promise":null},"#,
  r#""promiseNest":{"sync":null,"promise":null}},"#,
  r#""promiseNest":{"sync":null,"promise":null,"syncNest":{"sync":null,"promise":null},"#,
  r#""promiseNest":{"sync":null,"promise":null}}}"#,
);

/// `nonnull-test.ts:364-369`.
const LONG_CHAIN_DATA: &str =
  r#"{"syncNest":null,"promiseNest":null,"anotherNest":null,"anotherPromiseNest":null}"#;

// ------------------------------------------------------------------------------------------
// the runner
// ------------------------------------------------------------------------------------------

/// The outcome of running one case through `proto`.
struct Outcome {
  data: String,
  errors: Vec<(String, String, u32, u32)>,
}

fn run(case: &Case) -> Outcome {
  let sdl = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(case.sdl)
  .expect("the ported SDL parses");
  let schema = Schema::build(&sdl).expect("the ported SDL is a schema");

  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(case.query)
  .expect("the ported query parses");

  let mut space = Space {
    variables: case
      .variables
      .iter()
      .map(|(name, value)| ((*name).to_owned(), Val::from(*value)))
      .collect(),
  };
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Val::Data)
    .expect("the operation resolves");

  // Answering a request needs `&mut executor`, so nothing may still borrow it: take everything the
  // answer depends on out of the request first.
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    let answer = answer(&request, case.mode);
    match answer {
      Ok(value) => executor.handle_resolved(&mut space, id, value),
      Err(message) => executor.handle_field_error(id, &message),
    }
    while let Some(_cancelled) = executor.poll_abandoned() {}
  }

  let response = executor.poll_response().expect("nothing is outstanding");
  let errors = response
    .errors()
    .map(|error| {
      // Draft §7.1.2's `locations` is a list, and upstream fills it with one entry per field node
      // of the merged group. No query in `nonnull-test.ts` selects one response key twice, so
      // every upstream entry here is a one-element list and `ExpectedError` can stay a single line
      // and column. Asserting the length is what keeps that a checked property of the corpus
      // rather than an assumption about it: a case that grew a merged group would have to record
      // upstream's whole list before this passed again.
      let mut positions = error
        .locations()
        .iter()
        .map(|span| line_column(case.query, span.start()));
      let (line, column) = positions
        .next()
        .expect("every field error carries at least one location");
      assert_eq!(
        positions.next(),
        None,
        "{}: `{error}` reports more than one location, so its upstream case merges a response \
         key and this comparison no longer covers the whole of `locations`",
        case.name,
      );
      (error.to_string(), error.path().to_string(), line, column)
    })
    .collect();
  Outcome {
    data: render(&response.data()),
    errors,
  }
}

/// Upstream's `throwingData` and `nullingData`, and the argument schema's one resolver.
///
/// Both JavaScript objects key their behaviour on the field name and differ only in the bodies of
/// eight identically-named methods, which is what makes one `match` a faithful port of both.
///
/// It takes no [`Space`]: an argument supplied by a variable arrives carrying the value draft
/// §6.4.1 checked, so a resolver never looks a variable up for itself. Upstream's does not either
/// — `graphql-js` hands its resolvers a coerced `args` object.
fn answer(
  request: &smear::proto::FieldRequest<'_, '_, &str, Val>,
  mode: Mode,
) -> Result<Val, String> {
  match request.name() {
    "sync" | "promise" | "syncNonNull" | "promiseNonNull" => match mode {
      Mode::Nulling => Ok(Val::Null),
      Mode::Throwing => Err(request.name().to_owned()),
    },
    "syncNest" | "promiseNest" | "syncNonNullNest" | "promiseNonNullNest" => Ok(Val::Data),
    "withNonNullArg" => {
      let argument = request
        .arguments()
        .iter()
        .find(|argument| argument.name() == "cannotBeNull");
      let rendered = match argument.map(|argument| argument.source()) {
        Some(ArgumentSource::Literal(InputValue::String(literal))) => {
          // The lexer keeps the quotes; the resolver upstream sees the decoded value.
          literal.source().trim_matches('"').to_owned()
        }
        Some(ArgumentSource::Literal(other)) => format!("{other:?}"),
        Some(ArgumentSource::Variable { value, .. }) => match value {
          Val::Str(text) => text.clone(),
          Val::Null => "null".to_owned(),
          Val::Data => "undefined".to_owned(),
        },
        Some(ArgumentSource::SchemaDefault) | None => "undefined".to_owned(),
        Some(_) => "undefined".to_owned(),
      };
      Ok(Val::Str(format!("Passed: {rendered}")))
    }
    other => panic!("the ported corpus has no resolver for `{other}`"),
  }
}

/// Renders a response value the way upstream's expectations are written.
fn render(node: &Node<'_, Val>) -> String {
  match node {
    Node::Null => "null".to_owned(),
    Node::Leaf(Val::Str(text)) => format!("\"{text}\""),
    Node::Leaf(Val::Null) => "null".to_owned(),
    Node::Leaf(Val::Data) => "\"<object>\"".to_owned(),
    // `nonnull-test.ts` selects no meta-field, so the executor answers no `__typename` here.
    Node::TypeName(name) => panic!("the ported corpus selects no `__typename`, and got {name:?}"),
    Node::List(children) => {
      let mut out = String::from("[");
      for (index, (_, child)) in children.clone().enumerate() {
        if index > 0 {
          out.push(',');
        }
        out.push_str(&render(&child));
      }
      out.push(']');
      out
    }
    Node::Object(children) => {
      let mut out = String::from("{");
      for (index, (key, child)) in children.clone().enumerate() {
        if index > 0 {
          out.push(',');
        }
        out.push_str(&format!("\"{key}\":"));
        out.push_str(&render(&child));
      }
      out.push('}');
      out
    }
  }
}

/// Turns a byte offset into upstream's 1-based `{ line, column }`.
fn line_column(source: &str, offset: usize) -> (u32, u32) {
  let mut line = 1;
  let mut column = 1;
  for (index, byte) in source.bytes().enumerate() {
    if index == offset {
      break;
    }
    if byte == b'\n' {
      line += 1;
      column = 1;
    } else {
      column += 1;
    }
  }
  (line, column)
}

// ------------------------------------------------------------------------------------------
// the gates
// ------------------------------------------------------------------------------------------

/// The differential itself: every ported case, compared on `data`, messages, paths and locations.
#[test]
fn matches_graphql_js() {
  let mut compared_errors = 0usize;
  let mut compared_paths = 0usize;
  let mut failures = Vec::new();

  for case in CASES {
    let outcome = run(case);
    if outcome.data != case.data {
      failures.push(format!(
        "{}\n  {UPSTREAM_FILE}:{}\n  data expected: {}\n  data actual:   {}",
        case.name, case.line, case.data, outcome.data
      ));
    }

    let expected: BTreeSet<(String, String, u32, u32)> = case
      .errors
      .iter()
      .map(|error| {
        (
          error.path.to_owned(),
          error.message.to_owned(),
          error.line,
          error.column,
        )
      })
      .collect();
    let actual: BTreeSet<(String, String, u32, u32)> = outcome
      .errors
      .iter()
      .map(|(message, path, line, column)| (path.clone(), message.clone(), *line, *column))
      .collect();
    if expected != actual {
      failures.push(format!(
        "{}\n  {UPSTREAM_FILE}:{}\n  errors expected: {expected:#?}\n  errors actual:   {actual:#?}",
        case.name, case.line
      ));
    }
    compared_errors += case.errors.len();
    compared_paths += case.errors.len();
  }

  // The non-vacuity companion. A differential that ran zero comparisons is green, and this is what
  // stops that from being mistaken for agreement.
  println!(
    "graphql-js differential: {} cases, {compared_errors} error entries compared \
     ({compared_paths} response paths, {compared_paths} locations), upstream {UPSTREAM_COMMIT}",
    CASES.len()
  );
  assert_eq!(
    CASES.len(),
    17,
    "all 17 of {UPSTREAM_FILE}'s cases are ported"
  );
  assert!(
    compared_errors >= 25,
    "the corpus must actually exercise the error path, not just `data`; compared {compared_errors}"
  );
  assert!(
    CASES.iter().filter(|case| !case.errors.is_empty()).count() >= 11,
    "most cases must produce at least one error on both sides"
  );
  assert!(
    failures.is_empty(),
    "{} of {} cases diverge from graphql-js:\n\n{}",
    failures.len(),
    CASES.len(),
    failures.join("\n\n")
  );
  assert!(!VARIABLE_DEFAULTS_ARE_THE_HARNESS.is_empty());
}

/// Draft §6.4.4 in the direction that must bubble.
///
/// `syncNest { syncNonNull }` — the inner field is `String!`, so its error must null `syncNest` and
/// not merely `syncNest.syncNonNull`. An implementation that nulls only the field it errored at
/// produces `{"syncNest":{"syncNonNull":null}}`, which is a response that cannot exist: it puts
/// `null` in a position the schema declares non-null.
#[test]
fn a_non_null_field_bubbles_to_the_nearest_nullable_ancestor() {
  let case = CASES
    .iter()
    .find(|case| {
      case
        .name
        .ends_with("contains a non-nullable field / that returns null")
    })
    .expect("the case is in the corpus");
  let outcome = run(case);
  assert_eq!(outcome.data, r#"{"syncNest":null}"#);
  assert_eq!(outcome.errors.len(), 1);
  assert_eq!(outcome.errors[0].1, "syncNest.syncNonNull");
}

/// Draft §6.4.4 in the direction that must *not* bubble.
///
/// The same shape with a nullable inner field. If propagation is over-eager — bubbling on every
/// error rather than only through non-null positions — this returns `{"syncNest":null}` and the
/// twelve-error case above collapses to four nulls. The two tests fail in opposite directions, so
/// no single wrong rule satisfies both.
#[test]
fn a_nullable_field_does_not_bubble() {
  let case = CASES
    .iter()
    .find(|case| {
      case
        .name
        .ends_with("nulls a complex tree of nullable fields, each / that throws")
    })
    .expect("the case is in the corpus");
  let outcome = run(case);
  assert_eq!(outcome.data, COMPLEX_TREE_DATA);
  assert_eq!(outcome.errors.len(), 12);
  assert!(
    outcome
      .errors
      .iter()
      .all(|(_, path, _, _)| path.starts_with("syncNest") || path.starts_with("promiseNest")),
    "every error is under a surviving nullable ancestor"
  );
}

/// The chain case, which is where a one-off in the propagation walk shows up.
///
/// Six segments of path and five levels of non-null between the error and the null. Stopping one
/// level early leaves `null` inside a `DataType!`; going one level too far nulls `data`.
#[test]
fn propagation_stops_at_the_first_nullable_position() {
  let case = CASES
    .iter()
    .find(|case| {
      case
        .name
        .ends_with("long chain of non-null fields / that returns null")
    })
    .expect("the case is in the corpus");
  let outcome = run(case);
  assert_eq!(outcome.data, LONG_CHAIN_DATA);
  for (_, path, _, _) in &outcome.errors {
    assert_eq!(
      path.split('.').count(),
      6,
      "the error path is the failing field's, six segments deep, not the nulled ancestor's"
    );
  }
}

/// Every ported case still says where it came from.
///
/// Opt-in, like the validator's `apollo-rs` corpus and for the same reasons: another project's
/// tests are not vendored here, and a pinned fetch reproduces them exactly. Point
/// `SMEAR_GRAPHQL_JS` at a checkout — CI points it at [`UPSTREAM_COMMIT`].
///
/// It checks three things a stale hand-port would fail: that every recorded line still holds the
/// `it(` it was read from, that the SDL block is unchanged, and that upstream has not *added* a
/// case this corpus does not have.
#[test]
fn upstream_has_not_drifted() {
  let Ok(root) = std::env::var("SMEAR_GRAPHQL_JS") else {
    println!(
      "SMEAR_GRAPHQL_JS is unset: the {} ported cases ran against no upstream. \
       Set it to a graphql-js checkout at {UPSTREAM_COMMIT} to verify the port.",
      CASES.len()
    );
    return;
  };
  let path = std::path::Path::new(&root).join(UPSTREAM_FILE);
  let text = std::fs::read_to_string(&path)
    .unwrap_or_else(|error| panic!("SMEAR_GRAPHQL_JS is set but {path:?} is unreadable: {error}"));
  let lines: Vec<&str> = text.lines().collect();

  let mut verified = 0usize;
  for case in CASES {
    let line = lines
      .get(case.line as usize - 1)
      .unwrap_or_else(|| panic!("{UPSTREAM_FILE} has no line {}", case.line));
    assert!(
      line.trim() == case.anchor.trim(),
      "the port of `{}` records {UPSTREAM_FILE}:{}, which now reads\n  {}\nand not\n  {}",
      case.name,
      case.line,
      line.trim(),
      case.anchor.trim()
    );
    verified += 1;
  }

  let upstream_cases = text.matches("\n    it(").count() + text.matches("\n  it(").count();
  assert_eq!(
    upstream_cases,
    CASES.len(),
    "{UPSTREAM_FILE} now has {upstream_cases} cases and this corpus has {}; a case was added \
     upstream and has not been ported",
    CASES.len()
  );

  // Compared token by token rather than byte for byte: upstream's template literal carries the
  // indentation of the JavaScript it is embedded in, and re-indenting it is not drift. A changed
  // field, type or nullability marker is, and that survives the normalisation.
  let opened = text
    .find("buildSchema(`")
    .expect("{UPSTREAM_FILE} no longer builds its schema from SDL");
  let body = &text[opened + "buildSchema(`".len()..];
  let closed = body.find("`)").expect("the SDL template is unterminated");
  let upstream_sdl: Vec<&str> = body[..closed].split_whitespace().collect();
  let ported_sdl: Vec<&str> = DATA_SDL.split_whitespace().collect();
  assert_eq!(
    upstream_sdl, ported_sdl,
    "{UPSTREAM_FILE}'s schema no longer matches the SDL this corpus ported"
  );

  println!(
    "upstream drift gate: {verified} anchors verified against {UPSTREAM_FILE} \
     ({upstream_cases} upstream cases)"
  );
}
