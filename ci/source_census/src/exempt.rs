//! The exemptions, as data with a reason on every one of them.
//!
//! # Why the format is what it is
//!
//! An omission is invisible. A line missing from a check looks exactly like a property that holds,
//! which is the whole shape al8n/smear#122 is about, so an exemption here is not permission to
//! skip a site — it is a *record* of a narrowing, printed on every run, that has to be argued for
//! in writing.
//!
//! Four things make an unexplained exemption fail rather than pass:
//!
//! 1. **`reason` is a struct field, not an option.** A [`Exemption`] cannot be constructed without
//!    one; leaving it out is a compile error in this crate, before any census runs.
//! 2. **A reason that says nothing fails the run.** [`validate`] rejects one shorter than
//!    [`MIN_REASON`] bytes, and rejects the placeholder words people reach for when they have not
//!    got a reason (`todo`, `tbd`, `fixme`, `see above`, `n/a`).
//! 3. **`issue` is required for anything not structural.** An exemption whose `kind` is
//!    [`Kind::Tracked`] must name the issue that owns the narrowing, so it is debt with a home
//!    rather than debt with an excuse. [`Kind::Structural`] is for a constraint that no issue will
//!    ever close, and it must say what forces it.
//! 4. **A stale exemption fails.** An entry that matches nothing in the crate is an error, not a
//!    no-op. That is what forces the table to shrink when #121 and #103 widen their doors: the day
//!    `validate_schema_lossless` stops taking `&str`, this table stops matching and the census
//!    goes red until the line is deleted.
//!
//! # What is exempt today, and what is not
//!
//! The two `&str` parameters that are *not* narrowings at all — `Executor::start`'s `operation`
//! and `Executor::handle_field_error`'s `message` — are absent from this table on purpose. They
//! are classified by the rule, structurally, and needing an exemption for them would have meant
//! the rule was wrong. See `rule`'s header.

/// The shortest reason the census will accept. Long enough that "rowan" or "#121" alone does not
/// clear it, short enough that a genuine one-line reason does.
pub const MIN_REASON: usize = 40;

const PLACEHOLDERS: &[&str] = &["todo", "tbd", "fixme", "see above", "n/a", "xxx", "wip"];

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Kind {
  /// A narrowing that is tracked and is expected to go away. Must name its issue.
  Tracked,
  /// A constraint the crate cannot remove, whatever anyone decides. Must say what imposes it.
  Structural,
  /// Not a narrowing at all — the rule's default-convict fired on a parameter that is a datum,
  /// and the record says why. This is the one kind that is evidence AGAINST the rule, so it is
  /// counted separately in the run's own output: if it ever stops being a handful, the rule is
  /// wrong and no amount of recording will fix that.
  NotSource,
}

/// One recorded narrowing.
#[derive(Clone, Copy)]
pub struct Exemption {
  /// The module the entry is declared in, exactly as the census prints it.
  pub module: &'static str,
  /// `parse_document`, or `Schema::from_introspection` for an inherent method.
  pub entry: &'static str,
  /// The parameter. `*` records every narrowed parameter of the entry.
  pub param: &'static str,
  pub kind: Kind,
  /// The issue that owns it. Required for [`Kind::Tracked`].
  pub issue: Option<u32>,
  pub reason: &'static str,
}

/// The three lossless roots of each dialect, `fn(&str) -> Parse`.
const LOSSLESS_ROOT: &str = "The materialization door: it builds a rowan green tree, and rowan's \
   `GreenNodeBuilder::token` takes `&str`, so UTF-8 binds SOMEWHERE on this path. Whether it \
   binds at the public door or deeper is exactly al8n/smear#121's third question — the substrate \
   runner one layer down is already generic (`Lx::Source: tokora::cst::CstText` at \
   `parser/lossless/runner.rs`), so the abstraction exists and it is this entry that spells the \
   concrete type. Recorded, not accepted.";

/// The projections from a rowan tree back to a borrowing AST.
const LOSSLESS_PROJECTION: &str = "A projection re-slices the caller's own buffer by the tree's byte offsets and hands back an \
   AST borrowing it, so the AST's source type is whatever this parameter is — which is why the \
   `&'src str` here forces `Document<&'src str>` on every consumer of the lossless half while \
   the syntactic half is generic. Nothing in the signature needs UTF-8 except rowan's own \
   `SyntaxToken::text()` comparison, which is al8n/smear#121's question about where the \
   constraint really binds.";

/// Everything the census finds on `main` and does not fail on, each with its argument.
///
/// The three sites al8n/smear#122 names as the calibration answer are the first three groups. They
/// are recorded rather than fixed because #121 and #103 own them, and because a census whose known
/// answer is invisible in its own output cannot be checked against that answer.
pub const EXEMPTIONS: &[Exemption] = &[
  // ── §5 lossless, the validator doors — al8n/smear#121 ────────────────────────────────────────
  Exemption {
    module: "smear::validator::lossless",
    entry: "validate_executable_lossless",
    param: "*",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: "The lossless twin of `validate_executable<S, K> where S: AsRef<[u8]> + Clone, \
             K: Sink<S>`, narrowed to `&'src str` at both the source parameter and the sink's \
             source type. A consumer holding `bytes::Bytes` can use the syntactic door and not \
             this one. #121 is settling whether the rowan boundary genuinely binds at the door or \
             deeper; until it does, this is a recorded narrowing and not an accepted shape.",
  },
  Exemption {
    module: "smear::validator::lossless",
    entry: "validate_executable_lossless_with",
    param: "*",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: "`validate_executable_lossless`'s `rules` sibling, and narrowed identically — the \
             same `&'src str` source and the same `Sink<&'src str>`. Recorded separately because \
             widening one door and not the other would leave the asymmetry in place under a \
             shrinking table.",
  },
  Exemption {
    module: "smear::validator::lossless",
    entry: "validate_schema_lossless",
    param: "*",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: "The §3 door `Schema::build<S>(&TypeSystemDocument<S>) where S: AsRef<[u8]>` is \
             generic; this lossless door in front of it is not. Nothing in its signature carries \
             a source type — `&Parse` is a rowan tree — so the `&str` is the document itself, \
             tracked on #121.",
  },
  // ── §4 introspection — al8n/smear#103 ────────────────────────────────────────────────────────
  Exemption {
    module: "smear::validator::schema::introspection",
    entry: "Schema::from_introspection",
    param: "response",
    kind: Kind::Tracked,
    issue: Some(103),
    reason: "An introspection response arrives off a transport, which is where `bytes::Bytes` is \
             most likely to be what the caller already holds, and `&str` makes them validate \
             UTF-8 over the whole body first. #103 replaces the `serde_json` reader with a tokora \
             one and settles the string representation while it is in there.",
  },
  Exemption {
    module: "smear::validator::schema::introspection",
    entry: "to_sdl",
    param: "response",
    kind: Kind::Tracked,
    issue: Some(103),
    reason: "`Schema::from_introspection`'s intermediate form, public in its own right and \
             narrowed at the same parameter for the same reason. Widening one without the other \
             would leave the door a caller reaches for directly still narrow.",
  },
  // ── §5 lossless, the parser doors — al8n/smear#121 ───────────────────────────────────────────
  //
  // NOT in #121's table, which enumerates the two validator doors. They are the same family: the
  // census found them by the same rule and they narrow the same property, so they are recorded
  // under the issue that owns the question rather than left out of it.
  Exemption {
    module: "smear::parser::graphql::lossless::runner",
    entry: "parse_document",
    param: "src",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_ROOT,
  },
  Exemption {
    module: "smear::parser::graphql::lossless::runner",
    entry: "parse_executable_document",
    param: "src",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_ROOT,
  },
  Exemption {
    module: "smear::parser::graphql::lossless::runner",
    entry: "parse_type_system_document",
    param: "src",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_ROOT,
  },
  Exemption {
    module: "smear::parser::graphqlx::lossless::runner",
    entry: "parse_document",
    param: "src",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_ROOT,
  },
  Exemption {
    module: "smear::parser::graphqlx::lossless::runner",
    entry: "parse_executable_document",
    param: "src",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_ROOT,
  },
  Exemption {
    module: "smear::parser::graphqlx::lossless::runner",
    entry: "parse_type_system_document",
    param: "src",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_ROOT,
  },
  Exemption {
    module: "smear::parser::graphql::lossless::project",
    entry: "project",
    param: "source",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_PROJECTION,
  },
  Exemption {
    module: "smear::parser::graphql::lossless::project",
    entry: "project_executable_document",
    param: "source",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_PROJECTION,
  },
  Exemption {
    module: "smear::parser::graphql::lossless::project",
    entry: "project_executable_document_recovered",
    param: "source",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_PROJECTION,
  },
  Exemption {
    module: "smear::parser::graphql::lossless::project",
    entry: "project_type_system_document",
    param: "source",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_PROJECTION,
  },
  Exemption {
    module: "smear::parser::graphql::lossless::project",
    entry: "project_type_system_document_recovered",
    param: "source",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_PROJECTION,
  },
  Exemption {
    module: "smear::parser::graphql::lossless::project",
    entry: "Document::to_ast",
    param: "source",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_PROJECTION,
  },
  Exemption {
    module: "smear::parser::graphql::lossless::project",
    entry: "ExecutableDocument::to_ast",
    param: "source",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_PROJECTION,
  },
  Exemption {
    module: "smear::parser::graphql::lossless::project",
    entry: "TypeSystemDocument::to_ast",
    param: "source",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: LOSSLESS_PROJECTION,
  },
  Exemption {
    module: "smear::parser::lossless::project",
    entry: "verify_slice",
    param: "source",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: "The dialect-independent one-token form of the check the projections above make, and \
             the narrowest place the rowan constraint could bind: it compares the caller's slice \
             against `SyntaxToken::text()`, which rowan returns as `&str`. If al8n/smear#121 \
             concludes that the constraint is genuinely rowan's rather than the door's, this is \
             the entry the conclusion is about, and it becomes STRUCTURAL rather than deleted.",
  },
  Exemption {
    module: "smear::parser::lossless::project",
    entry: "verify_source",
    param: "source",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: "The whole-tree form of `verify_slice`, and the one every projection door above \
             actually calls since al8n/smear#127 hoisted the check out of the per-token path. It \
             compares `source` against the green tree's own token text, which rowan stores as \
             `&str`, so it narrows for `verify_slice`'s reason at the door rather than at a \
             token — and it reaches al8n/smear#121's conclusion by the same route.",
  },
  Exemption {
    module: "smear::parser::lossless::project",
    entry: "verify_source_at",
    param: "source",
    kind: Kind::Tracked,
    issue: Some(121),
    reason: "`verify_source` for a subtree, which the compositional `to_ast` doors and the \
             recovering door's per-definition check call. Recorded separately because widening \
             one and not the other would leave half the projection's doors narrow under a table \
             that had stopped naming them.",
  },
  // ── Not narrowings: the rule's default-convict misfiring ─────────────────────────────────────
  //
  // Two, out of the two dozen narrowed parameters this table records over a public surface of
  // several hundred entries. The proportion is the evidence for the rule rather than a note about
  // it: a rule needing an exemption for every second entry would be the wrong rule. The exact
  // counts are deliberately not written here — the run prints both on its own summary line, and a
  // pair copied into a comment goes stale on the next public item anyone adds, as this one had.
  Exemption {
    module: "smear::validator::schema::repr::location",
    entry: "DirectiveLocation::from_name",
    param: "name",
    kind: Kind::NotSource,
    issue: None,
    reason: "A draft §3 directive-location keyword — `QUERY`, `FIELD_DEFINITION` — matched \
             against a closed, ASCII set this enum defines. It is a key the caller composes, not \
             a document, and every source representation produces one for free. Test 3 does not \
             acquit it only because `DirectiveLocation` is a plain enum with no source type to \
             hold.",
  },
  Exemption {
    module: "smear::validator::schema::repr::ty",
    entry: "PackedType::write",
    param: "name",
    kind: Kind::NotSource,
    issue: None,
    reason: "The already-interned base type name, handed back in so the wrapper can be printed \
             around it — `[Foo!]!`. It comes out of the schema's own name arena, so it is this \
             crate's `&str` and not the caller's buffer. Test 3 does not acquit it because \
             `PackedType` is a `u32` bitfield with no source type to hold.",
  },
];

/// Refuses a table that would let a narrowing through without an argument.
pub fn validate() -> Vec<String> {
  EXEMPTIONS
    .iter()
    .enumerate()
    .flat_map(|(index, exemption)| check_one(exemption, index))
    .collect()
}

/// The guards, on one record. Public so the selftest can hand it a deliberately broken one.
pub fn check_one(exemption: &Exemption, index: usize) -> Vec<String> {
  let mut problems = Vec::new();
  let at = format!(
    "exemption {} ({}::{} / {})",
    index, exemption.module, exemption.entry, exemption.param
  );
  let reason = exemption.reason.trim();
  if reason.len() < MIN_REASON {
    problems.push(format!(
      "{at}: the reason is {} bytes and the census requires at least {MIN_REASON}. An exemption \
       is a claim that a narrowing is acceptable; a claim that short is not one",
      reason.len()
    ));
  }
  let lowered = reason.to_ascii_lowercase();
  if let Some(placeholder) = PLACEHOLDERS.iter().find(|p| lowered.contains(**p)) {
    problems.push(format!(
      "{at}: the reason contains the placeholder {placeholder:?}, which is what a missing reason \
       looks like when someone has to fill a required field"
    ));
  }
  match (exemption.kind, exemption.issue) {
    (Kind::Tracked, None) => problems.push(format!(
      "{at}: a tracked narrowing must name the issue that owns it, or it is debt with nowhere to \
       be paid"
    )),
    (Kind::Structural | Kind::NotSource, Some(issue)) => problems.push(format!(
      "{at}: it is recorded as {:?} — nothing anyone closes — and yet names issue #{issue}. One \
       of the two is wrong",
      exemption.kind
    )),
    _ => {}
  }
  if exemption.module.is_empty() || exemption.entry.is_empty() || exemption.param.is_empty() {
    problems.push(format!("{at}: module, entry and param must all be named"));
  }
  problems
}

impl Exemption {
  pub fn matches(&self, module: &str, entry: &str, param: &str) -> bool {
    self.module == module && self.entry == entry && (self.param == "*" || self.param == param)
  }

  pub fn label(&self) -> String {
    match (self.kind, self.issue) {
      (Kind::Tracked, Some(issue)) => format!("TRACKED al8n/smear#{issue}"),
      (Kind::Tracked, None) => "TRACKED (no issue — invalid)".to_string(),
      (Kind::Structural, _) => "STRUCTURAL".to_string(),
      (Kind::NotSource, _) => "NOT SOURCE — the rule misfired".to_string(),
    }
  }
}
