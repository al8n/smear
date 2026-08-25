//! The contract exemptions, as data with a reason on every one of them.
//!
//! Same shape and same guards as `crate::exempt`, and for the same argument: an omission is
//! invisible, so a type that does not answer the contract is *recorded* here — printed on every
//! run, argued for in writing, and failing the run if the argument is missing — rather than
//! quietly absent from a check.
//!
//! # What the four kinds mean
//!
//! [`Kind::Aggregate`] and [`Kind::Verdict`] are the two al8n/smear#126's design names: "never by
//! aggregates or verdicts". An aggregate holds diagnostics and is not one, so it must say which
//! element answers, and the census then requires that element to be a contract row — an aggregate
//! of an exempt type answers nothing, and the chain reads fine one record at a time. A verdict is
//! not a diagnostic either: it says *that* something failed, and the diagnostics that say what
//! went somewhere else, usually to a sink.
//!
//! [`Kind::Unresolved`] is the design's other half — "implemented by the resolved views". A
//! diagnostic whose spans and symbols mean nothing without the schema they index cannot answer on
//! its own; the value that pairs the two can, and this record names it.
//!
//! [`Kind::Tracked`] is a family that has not joined the contract and is expected to. Every one of
//! them today is al8n/smear#126's, and each must name its issue, so it is debt with a home rather
//! than debt with an excuse. Two of that issue's phases are represented: **phase E**, the parser
//! and lexer families, sequenced after the validator ones deliberately; and **phase D**, the §7
//! writer and proto's adoption of the contract, which the issue sequences *strictly after*
//! `feat/proto-execute-query` merges. Neither ordering is this table's — it records them.
//!
//! [`Kind::NotDiagnostic`] is the name pattern misfiring: a type whose name ends in `Error` and
//! which is not an error a consumer is ever handed. It is counted separately in the run's own
//! output, because if it stops being a handful the pattern is wrong and recording will not fix
//! that. There is none today, and the kind exists so that the first one is a record rather than a
//! change to the pattern.

use crate::exempt::reason_problems;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Kind {
  /// A collection whose elements are the diagnostics. Must name the element type.
  Aggregate,
  /// An outcome rather than a diagnostic: it reports *that* something failed, and the values that
  /// say what went to a sink or to an accessor.
  Verdict,
  /// A diagnostic that cannot answer alone. Must name the resolved view that can.
  Unresolved,
  /// A family that owes the contract and has not paid yet. Must name its issue.
  Tracked,
  /// The name pattern misfired: not an error a consumer is handed at all.
  NotDiagnostic,
}

/// One type recorded as not answering [`Diagnose`](super::TRAIT).
#[derive(Clone, Copy)]
pub struct Exemption {
  /// The path a consumer names the type by, exactly as the census prints it.
  ///
  /// The path and not the ident: this crate declares `LexerError`, `FloatError`, `Errors` and
  /// `Diagnostic` twice each, and a table keyed by name would record one of every pair and
  /// quietly exempt the other.
  pub path: &'static str,
  pub kind: Kind,
  /// What answers instead, by its own path. Required for [`Kind::Aggregate`] and
  /// [`Kind::Unresolved`], forbidden elsewhere, and required to be a contract row.
  pub element: Option<&'static str>,
  /// The issue that owns it. Required for [`Kind::Tracked`], forbidden elsewhere.
  pub issue: Option<u32>,
  pub reason: &'static str,
}

/// The lexer families: draft §2 lexical refusals, one per numeric and string shape.
const LEXER_FAMILY: &str = "A draft §2 lexical refusal. al8n/smear#126 sequences the parser and \
   lexer families into phase E, after the validator families and after the draft §3 layering memo \
   that phase writes: the `Code` namespace has to settle where a lexical refusal sits relative to \
   a syntactic one and to a validation rule before either family can be given codes meant to \
   outlive a rewording, and that ordering is the issue's, not this table's. Recorded, not \
   accepted.";

/// The parser families: draft §2 syntactic refusals, and the lossless tower's own reports.
const PARSER_FAMILY: &str = "A draft §2 syntactic refusal. Phase E of al8n/smear#126 owns it \
   together with the lexer families, and for the same reason: the §3 question of which \
   obligations a parser discharges and which a validation rule does is the one the contract was \
   built to make rigorous, so codes assigned before that memo would be the guess the issue exists \
   to stop making. Recorded, not accepted.";

/// Every type the D1 inventory finds that does not answer the contract, each with its argument.
///
/// It is long, and its length is the finding rather than a defect in the table: almost all of it is
/// the lexer and parser families phase E owns. The rows it does *not* cover are the ones phase A
/// implemented, and D2 proves those with the compiler.
///
/// The counts are deliberately not written out here. The run's own summary line is the first place
/// in the repository that states how many there are, and it recomputes them; a pair copied into a
/// comment goes stale on the next public type anybody adds, as the pair that used to be here had —
/// it read thirty-two where the run printed thirty. `crate::exempt`'s misfire group records the
/// same lesson about the same kind of sentence.
// SPELL A PATH AT THE CRATE THAT DECLARES THE TYPE, not at a re-export of it. `syn` cannot follow
// `pub use smear_schema as schema;` from the umbrella into another package, so since the split the
// canonical path a mounted root produces is the DECLARING member's — `smear::validator::schema::
// SchemaErrors`, not the `smear::validator::SchemaErrors` that `validator/mod.rs` also publishes.
// Both are real paths for a consumer; only one is the one this table matches on. The split moved
// two entries this way and the table said so on the same run, which is what it is for.
pub const EXEMPTIONS: &[Exemption] = &[
  // ── Aggregates: the elements answer ──────────────────────────────────────────────────────────
  Exemption {
    path: "smear::validator::schema::SchemaErrors",
    kind: Kind::Aggregate,
    element: Some("smear::validator::schema::SchemaError"),
    issue: None,
    reason: "The `Vec<SchemaError>` a refused build hands back, with the interning arena those \
             errors' symbols index. A collection is not a diagnostic — it has no code, no primary \
             span and no severity of its own — and the design says so: implemented by the \
             resolved views, never by aggregates. Its elements are `SchemaError`, which is a \
             contract row and which D2 asserts.",
  },
  // ── Unresolved: the resolved view answers ────────────────────────────────────────────────────
  Exemption {
    path: "smear::validator::Diagnostic",
    kind: Kind::Unresolved,
    element: Some("smear::validator::DiagnosticDisplay"),
    issue: None,
    reason: "A draft §5 diagnostic holds a `Rule`, spans and a `Context` of interned `Sym`s, and a \
             `Sym` is an index into one particular `Schema`. On its own it cannot render its own \
             message, so it cannot satisfy the `Display` supertrait meaningfully and cannot answer \
             `primary_label` at all. `Diagnostic::display(schema)` pairs the two, and that pair is \
             what implements the contract — which is the design's rule, not an exception to it.",
  },
  // ── Verdicts: an outcome, carrying or counting the diagnostics rather than being one ─────────
  Exemption {
    path: "smear::validator::Invalid",
    kind: Kind::Verdict,
    element: None,
    issue: None,
    reason: "The verdict of a failed executable validation: how many diagnostics were emitted, \
             whether the sink stopped early, whether a budget refused the document. The \
             diagnostics themselves went to the sink and never enter this value, so there is no \
             code to answer and no span to point at — asking it for a primary location would mean \
             inventing one for a counter.",
  },
  // ── §7.1.7 `extensions`: two refusals that hand the caller's value back ──────────────────────
  Exemption {
    path: "smear::proto::SetExtensionsError",
    kind: Kind::Verdict,
    element: None,
    issue: None,
    reason: "Why `Executor::set_extensions` refused an `extensions` map, and the map itself, \
             returned through `into_extensions` so a refusal cannot close a handle the driver still \
             owns. It is not a diagnostic about the *document*: all three refusals are properties of \
             the call — no operation is running, the response has already been delivered, or the map \
             is over this executor's ceilings — so there is no source position to point at and no \
             response path to carry. Unlike `proto::StartError` it is not even a draft §6.1 request \
             error; nothing about it ever reaches a §7 response, because the whole point of the \
             refusal is that nothing will.",
  },
  Exemption {
    path: "smear::proto::Full",
    kind: Kind::Verdict,
    element: None,
    issue: None,
    reason: "Which of the two §7.1.7 extension ceilings refused an insert, and the driver's value \
             handed back unconsumed. The same argument as `SetExtensionsError` and one step \
             smaller: a ceiling refusing is an outcome about a container the service is filling, \
             with no document behind it to have a span in. `Ceiling::field` names the `Limits` field \
             that refused, which is the machine-readable part a code would otherwise have carried.",
  },
  Exemption {
    path: "smear::proto::SourceEventError",
    kind: Kind::Verdict,
    element: None,
    issue: None,
    reason: "Why `Executor::handle_source_event` refused a draft §6.2.3.2 source event, and the \
             event itself, returned through `into_value` so a refusal cannot close a handle the \
             driver still owns. The same argument as `SetExtensionsError`, whose two lifecycle \
             variants these two are the subscription's: no response stream is open, or the \
             previous event's execution result has not been taken. Both are properties of the \
             *call* — of where the machine is, not of anything in the document — so there is no \
             source position to point at and no response path to carry, and neither ever reaches a \
             §7 response, because the point of the refusal is that the event does not. \
             `Executor::response_stream` names the state that refused, which is the \
             machine-readable part a code would otherwise have carried, and the remedy: drain the \
             result, or stop pushing.",
  },
  Exemption {
    path: "smear::proto::TooLarge",
    kind: Kind::Verdict,
    element: None,
    issue: None,
    reason: "Which of the two §7.1.7 extension ceilings refused a whole map on its way into a \
             draft §7.1.3 request error result, and the map handed back so a refusal cannot close \
             a handle the driver still owns. Same argument as `Full`, one level up: `Full` is an \
             insert refused and this is an attach refused, and neither has a document behind it to \
             have a span in — the map is the *service's* own, built after execution was declined. \
             The value it is refused by happens to be a §7 response, and that changes nothing: \
             this refusal never reaches one, because the whole point is that the map does not. \
             `Ceiling::field` names the `Limits` field that refused, which is the machine-readable \
             part a code would otherwise have carried.",
  },
  Exemption {
    path: "smear::validator::LosslessInvalid",
    kind: Kind::Verdict,
    element: None,
    issue: None,
    reason: "`Invalid` paired with the `Recovery` the successful arm carries, so a caller learns \
             what was wrong and how much of the document had an AST image from one value. It is a \
             verdict for its inner verdict's reason, and adding a recovery fraction to a counter \
             does not make either of them a diagnostic.",
  },
  Exemption {
    path: "smear::validator::LosslessSchemaErrors",
    kind: Kind::Verdict,
    element: None,
    issue: None,
    reason: "`LosslessInvalid`'s twin on the SDL side: the `SchemaErrors` draft §3 produced, \
             paired with the parse's `Recovery`. Recorded as a verdict rather than as an \
             aggregate because its element is `SchemaErrors`, which is itself an aggregate — the \
             contract rows are two levels down, reachable through `errors()`, and a record \
             claiming a direct element would be claiming something untrue.",
  },
  Exemption {
    path: "smear::validator::schema::IntrospectionError",
    kind: Kind::Verdict,
    element: None,
    issue: None,
    reason: "The two ways `Schema::from_introspection` can refuse, as a sum: `Response` carries a \
             `ResponseError` and `Schema` carries the whole `SchemaErrors` draft §3 produced. It \
             is the door's return type rather than a diagnostic, and the two arms answer through \
             their own payloads — one a contract row, the other an aggregate of one. A `Diagnose` \
             on the sum would have to invent a primary location for a variant holding a hundred of \
             them.",
  },
  // ── §6 execution — al8n/smear#126 phase D ────────────────────────────────────────────────────
  //
  // The two are recorded separately and for different reasons, because they are different animals:
  // one is a draft §7.1.2 *field* error, which is the shape the contract's response-path axis was
  // added for, and the other is a §6.1 refusal that never reaches a response at all.
  Exemption {
    path: "smear::proto::Error",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: "One entry of a draft §7.1.2 `errors` array, and the only type in this crate that has \
             a response path to answer with — `path_segments` exists on the contract because of \
             this shape, so the axis has exactly one candidate implementor and it is this. \
             al8n/smear#126 sequences it into phase D, the §7 writer together with proto's \
             adoption, *strictly after* `feat/proto-execute-query` merges, and the ordering is \
             load-bearing rather than administrative: phase D decides whether the path is \
             published through `Diagnose::path_segment` — which would make `proto::Segment` and \
             `diagnostic::PathSegment` one type instead of two spellings of the same two \
             variants — or stays behind `Error::path()`. Assigning codes and an accessor shape \
             before that decision would be the guess the phase exists to stop making. Everything \
             else it needs is already here: `Display` renders the specified message, `locations()` \
             is non-empty for every error a driver can observe, and `Path::get` is already an \
             indexed accessor. Recorded, not accepted.",
  },
  Exemption {
    path: "smear::proto::StartError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: "How `Executor::start` refuses: draft §6.1's `GetOperation` failures — no operation, \
             no operation of that name, more than one and none named — plus a schema missing one \
             of the three root types, and draft §6.2.3.1 `CreateSourceEventStream`'s three request \
             errors, which are request errors in the specification's own words because they are \
             raised before any execution exists. Phase D of al8n/smear#126 owns it \
             with `proto::Error`, and it carries a question of its own that phase has to answer \
             first: unlike every other row here it is a *request* error, refused before execution \
             begins, so it has no response path, and it is refused on a property of the whole \
             document or of the schema rather than at a position in the text, so it has no span \
             either. Its `primary()` would therefore be the second user of `Location::entire`, \
             and `only_the_introspection_door_answers_entire` in \
             `smear/tests/diagnostic_codes.rs` pins that there is exactly one — so joining the \
             contract here means changing a gate deliberately, which is the reviewed decision \
             al8n/smear#126's own first correction asks for. Recorded, not accepted.",
  },
  Exemption {
    path: "smear::json::Error",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: "How the draft §7.2.1 writer refuses: a sink that would not take the bytes, a `Float` \
             that is not finite, a `\\u` escape naming a surrogate, a malformed escape, and an \
             allocator that would not give a §7.1.2 response path room. Phase D \
             of al8n/smear#126 is *the §7 writer together with proto's adoption*, so this is that \
             phase's third row rather than a new debt beside it — and it has to be decided with \
             the other two instead of before them, because the phase's open question is where a \
             draft §7 position gets published, and giving this type codes and an accessor shape \
             first would settle half of that by accident. It also carries the sharper version of \
             `StartError`'s own problem: a serialisation refusal has no position in ANY input — \
             not a span, not a whole document, not a response path — because it is raised while \
             rendering a response that has already been decided, so its `primary()` would be a \
             *third* user of `Location::entire` against a gate that pins the count at one, and \
             `Location`'s own documentation calls that spelling an exception with a named holder \
             rather than a default. The allocation variant sharpens that again rather than \
             softening it: it is the one refusal with no position even in the RESPONSE, since it \
             is raised because the path could not be assembled. Recorded, not accepted.",
  },
  // ── The lexer families — al8n/smear#126 phase E ──────────────────────────────────────────────
  Exemption {
    path: "smear::lexer::error::LengthError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::error::StringError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::error::StringErrors",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::error::UnicodeError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::error::BracedUnicodeEscapeError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::error::FixedUnicodeEscapeError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphql::error::LexerError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphql::error::LexerErrors",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphql::error::FloatError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphql::error::DecimalError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphqlx::error::LexerError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphqlx::error::LexerErrors",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphqlx::error::FloatError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphqlx::error::HexFloatError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphqlx::error::DecimalError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphqlx::error::HexError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphqlx::error::OctalError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  Exemption {
    path: "smear::lexer::graphqlx::error::BinaryError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: LEXER_FAMILY,
  },
  // ── The parser families — al8n/smear#126 phase E ─────────────────────────────────────────────
  Exemption {
    path: "smear::parser::graphql::error::Error",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: PARSER_FAMILY,
  },
  Exemption {
    path: "smear::parser::graphql::error::Errors",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: PARSER_FAMILY,
  },
  Exemption {
    path: "smear::parser::graphql::syntactic::GraphqlError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: PARSER_FAMILY,
  },
  Exemption {
    path: "smear::parser::graphql::lossless::GraphqlLosslessError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: PARSER_FAMILY,
  },
  Exemption {
    path: "smear::parser::graphql::lossless::GraphqlLosslessErrors",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: PARSER_FAMILY,
  },
  Exemption {
    path: "smear::parser::graphqlx::error::Error",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: PARSER_FAMILY,
  },
  Exemption {
    path: "smear::parser::graphqlx::error::Errors",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: PARSER_FAMILY,
  },
  Exemption {
    path: "smear::parser::graphqlx::syntactic::GraphqlxError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: PARSER_FAMILY,
  },
  Exemption {
    path: "smear::parser::graphqlx::lossless::GraphqlxLosslessError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: PARSER_FAMILY,
  },
  Exemption {
    path: "smear::parser::graphqlx::lossless::GraphqlxLosslessErrors",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: PARSER_FAMILY,
  },
  Exemption {
    path: "smear::parser::lossless::project::ProjectError",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: "The projection's refusal: the caller's buffer does not match the green tree the \
             projection was asked to re-slice it by. Phase E of al8n/smear#126 owns it with the \
             rest of the parser families. It is also the one refusal in this group that is about \
             a caller's mistake rather than a document's, which is a distinction the phase's \
             `Code` namespace has to make and cannot make before it exists.",
  },
  Exemption {
    path: "smear::parser::graphql::lossless::Unverified",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: "What a whole-root verification answers, and it is `ProjectError` above with the two \
             outcomes separated — a stale pair from a tree too deep to descend — so it belongs to \
             the same family and the same phase E. It also carries strictly LESS than the entry \
             it is built from: `ProjectError` has a byte range, and this deliberately has none, \
             because 'these are not the same document' is not a fact about a position in either \
             of them. `Diagnose::primary` returns a `Location`, so answering the contract here \
             means inventing one, and phase E is where what a caller-mistake refusal points at \
             gets decided rather than guessed. Recorded, not accepted.",
  },
  Exemption {
    path: "smear::parser::lossless::runner::Diagnostic",
    kind: Kind::Tracked,
    element: None,
    issue: Some(126),
    reason: "What a lossless parse recorded: a byte range, a severity, and the token count of a \
             recovery hole. It is deliberately payload-free — carrying the typed error would give \
             `Parse` the parse's lifetime and stop a consumer caching one per file — so joining \
             the contract means deciding which `Code` a lifetime-free record can honestly answer \
             with, and that is phase E's question rather than a line missing here.",
  },
];

/// Refuses a table that would let a type through without an argument.
pub fn validate() -> Vec<String> {
  EXEMPTIONS
    .iter()
    .enumerate()
    .flat_map(|(index, exemption)| check_one(exemption, index))
    .collect()
}

/// The guards, on one record. Public so the selftest can hand it a deliberately broken one.
pub fn check_one(exemption: &Exemption, index: usize) -> Vec<String> {
  let at = format!("contract exemption {index} ({})", exemption.path);
  let mut problems = reason_problems(exemption.reason, &at);

  let delegates = matches!(exemption.kind, Kind::Aggregate | Kind::Unresolved);
  match (delegates, exemption.element) {
    (true, None) => problems.push(format!(
      "{at}: it is recorded as {:?}, which is a claim that something *else* answers the contract, \
       so it must name what. Without one the record claims nothing and cannot be checked",
      exemption.kind
    )),
    (false, Some(element)) => problems.push(format!(
      "{at}: it is recorded as {:?} and yet names `{element}` as what answers instead. Only an \
       aggregate or an unresolved view delegates to one",
      exemption.kind
    )),
    _ => {}
  }

  match (exemption.kind, exemption.issue) {
    (Kind::Tracked, None) => problems.push(format!(
      "{at}: a tracked family must name the issue that owns it, or it is debt with nowhere to be \
       paid"
    )),
    (Kind::Tracked, Some(_)) => {}
    (_, Some(issue)) => problems.push(format!(
      "{at}: it is recorded as {:?} — nothing anyone closes — and yet names issue #{issue}. One \
       of the two is wrong",
      exemption.kind
    )),
    (_, None) => {}
  }

  if exemption.path.is_empty() {
    problems.push(format!("{at}: the type must be named"));
  }
  problems
}

impl Exemption {
  /// Whether this record claims some other type answers on its behalf.
  pub fn delegates(&self) -> bool {
    matches!(self.kind, Kind::Aggregate | Kind::Unresolved)
  }

  pub fn label(&self) -> String {
    match (self.kind, self.element, self.issue) {
      (Kind::Aggregate, Some(element), _) => format!("AGGREGATE of {element}"),
      (Kind::Unresolved, Some(element), _) => format!("RESOLVED BY {element}"),
      (Kind::Aggregate | Kind::Unresolved, None, _) => {
        "DELEGATES (to nothing — invalid)".to_string()
      }
      (Kind::Verdict, ..) => "VERDICT".to_string(),
      (Kind::Tracked, _, Some(issue)) => format!("TRACKED al8n/smear#{issue}"),
      (Kind::Tracked, _, None) => "TRACKED (no issue — invalid)".to_string(),
      (Kind::NotDiagnostic, ..) => "NOT A DIAGNOSTIC — the pattern misfired".to_string(),
    }
  }
}
