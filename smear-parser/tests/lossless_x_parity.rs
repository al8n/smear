#![cfg(all(feature = "rowan", feature = "graphqlx"))]

//! Gate 1: acceptance parity between the GraphQLx lossless suite and the shipped GraphQLx
//! `syntactic` one.
//!
//! The cross-suite invariant is a **verdict**: for every corpus entry, both suites must agree on
//! *whether* the input is valid GraphQLx. Diagnostic sets are deliberately not compared — the
//! lossless suite recovers and is therefore more verbose by construction.
//!
//! # The corpus was built by measurement, and the measurement is the finding
//!
//! GraphQLx is widely believed to be a superset of GraphQL. *Believed* was the operative word, so
//! all eighty-seven entries of `tests/corpus/` were run through the **GraphQLx syntactic** parser
//! before any of them was copied. The result:
//!
//! - **All fifty-six `valid_*` entries are accepted.** The superset holds over the whole positive
//!   half, so `GRAPHQL_ENTRIES_GRAPHQLX_REJECTS` — the list the plan expected to fill — is empty
//!   and does not exist.
//! - **Exactly one `invalid_*` entry is *accepted*,** which is the opposite direction and the one
//!   the plan did not anticipate. `fragment on on T { f }` is a GraphQL error and valid GraphQLx:
//!   GraphQL spends a whole production on `FragmentName: Name but not "on"` and GraphQLx's
//!   `executable_definition_name` is a plain `take_name`. It is therefore in this corpus under
//!   `valid_`, and the rename is pinned in [`GRAPHQL_ENTRIES_GRAPHQLX_ACCEPTS`] so a future
//!   grammar change reds instead of the corpus drifting.
//!
//! [`the_inherited_half_is_exactly_the_graphql_corpus`] holds the copy itself: every GraphQL entry
//! is here, under its own stem or under the one pinned rename, and nothing is here twice.
//!
//! A `.graphqlx` extension and a separate directory, because gate 5's golden set is keyed by
//! corpus file and mixing the dialects in one directory would make every GraphQLx golden look like
//! a missing GraphQL one.
//!
//! # The differential sweep found nothing, and that is why the proof of life is structural
//!
//! Phase A's sweep found **eleven** divergences over 240 sources, all in one direction, across two
//! spec rules — const positions and `FragmentName but not "on"`. Phase B's sweep found **zero**
//! over 257: twenty-three const sites × eight value forms (including `set { $v }`,
//! `map { $v => 1 }` and `map { 1 => $v }`, which have no GraphQL counterpart), four non-const
//! sites for the control, and forty-one hand-aimed GraphQLx candidates — a fragment named `on`, an
//! import member aliased to `null` and to `set`, a type named `set`, a field named `set` with and
//! without a selection set, `where` at each of its eight sites, `<>`, `<Int => >`, `Int!!`, and a
//! description in front of an import, an extension and a shorthand operation. The reason is that
//! GraphQLx's `Constness` parameter and its reserved-spelling checks were written into Tasks 10–14
//! from the start rather than retrofitted after a sweep found them missing.
//!
//! So this gate has no *language* divergence to prove its comparison can fail. The proof of life
//! is a **production** divergence instead, which is permanent by construction:
//! [`the_corpus_can_tell_the_two_suites_apart`] shows forty-three entries on which the lossless
//! SDL-only root and the mixed root disagree, and that every one of them is a *clean*
//! discriminator — the mixed root agreeing with the syntactic suite about it. Closing that
//! divergence would be a bug, so unlike Phase A's eleven it cannot quietly go away.
//!
//! # The corpus is run twice: compact, and padded
//!
//! [`both_suites_agree_on_every_corpus_entry`] reads the corpus as written. Every lexable entry is
//! also run with each of the eight ignorable forms injected at every token boundary, by
//! [`both_suites_agree_on_every_padded_corpus_entry`], because the compact half cannot see the
//! asymmetry that makes the pairing worth gating: **the lossless suite is the only one of the two
//! that sees trivia.** `syntactic/`'s lexer skips it before the grammar exists; the lossless lexer
//! surfaces every byte and each atom has to cross it deliberately. An atom that stops crossing
//! turns valid formatted GraphQLx into a rejection in one suite only.
//!
//! GraphQLx raises the stakes on that: three of its productions decide a **node kind** on what the
//! next significant token is — `set`/`map` followed by `{`, `<T>` versus `<K => V>`, and a `where`
//! clause's two-token continuation window — and each has to reach past arbitrary trivia to ask.
//!
//! **The padding is not defined here.** The plan's Step 6 says to assert the padded set is exactly
//! the one gate 2 derives, and two agreeing `const`s in two test binaries cannot be that — nothing
//! in one integration-test crate can name an item in another, so the "assertion" would be a copy
//! keeping its own counsel. Task 17 discharged it by identity instead: [`ALPHABET`],
//! [`token_boundaries`], [`inject`] and [`UNPADDABLE`] are gate 2's, imported from
//! `tests/support/graphqlx_padding.rs`, and this gate's padded sweep is therefore gate 2's set
//! widened to the invalid half rather than a second set that resembles it.

use std::{collections::BTreeSet, path::PathBuf};

use smear_parser::graphqlx::{
  GraphQLx,
  ast::{Document, ExecutableDocument, TypeSystemDocument},
  error::GraphqlxErrors,
  lossless::{parse_executable_document, parse_str, parse_type_system_document},
  syntactic::{GraphqlxLexer, document, executable_document, type_system_document},
};
use tokora::{Parse as _, Parser};

#[path = "support/graphqlx_padding.rs"]
mod padding;

use padding::{ALPHABET, UNPADDABLE, corpus_files, inject, name_of, token_boundaries};

/// The lossless verdict: did the parse report a grammar **error**?
///
/// Recovery holes and warnings do not count — see `Parse::has_errors`.
fn lossless_has_errors(src: &str) -> bool {
  parse_str(src).has_errors()
}

/// The syntactic verdict: did the shipped GraphQLx `document` production reject the source?
///
/// A whole-input verdict rather than a prefix one, for the reason GraphQL's twin records: tokora's
/// `parse_str` does not check for end-of-input, and `document` is nonetheless a whole-input
/// production because its repetition re-enters the entry dispatcher on trailing junk and fails
/// there rather than leaving it behind.
fn syntactic_has_errors<'inp>(src: &'inp str) -> bool {
  Parser::with_parser::<
    'inp,
    GraphqlxLexer<'inp, str>,
    Document<&'inp str>,
    GraphqlxErrors<&'inp str>,
    _,
    GraphQLx,
  >(document)
  .parse_str(src)
  .is_err()
}

/// The syntactic verdict for the **SDL-only** root — imports and type-system entries, no
/// executable definition and no shorthand operation.
///
/// Whole-input for [`syntactic_has_errors`]'s reason: the repetition re-enters the entry
/// dispatcher on anything it cannot stop at, so a trailing executable definition fails there
/// rather than being left behind.
fn syntactic_type_system_has_errors<'inp>(src: &'inp str) -> bool {
  Parser::with_parser::<
    'inp,
    GraphqlxLexer<'inp, str>,
    TypeSystemDocument<&'inp str>,
    GraphqlxErrors<&'inp str>,
    _,
    GraphQLx,
  >(type_system_document)
  .parse_str(src)
  .is_err()
}

/// The syntactic verdict for the **executable-only** root,
/// [`syntactic_type_system_has_errors`]'s mirror.
fn syntactic_executable_has_errors<'inp>(src: &'inp str) -> bool {
  Parser::with_parser::<
    'inp,
    GraphqlxLexer<'inp, str>,
    ExecutableDocument<&'inp str>,
    GraphqlxErrors<&'inp str>,
    _,
    GraphQLx,
  >(executable_document)
  .parse_str(src)
  .is_err()
}

/// The cross-suite invariant: both suites must agree on *whether* the input is valid GraphQLx.
/// Diagnostic sets are deliberately NOT compared — the lossless suite may be more verbose.
#[test]
fn both_suites_agree_on_every_corpus_entry() {
  let mut checked_valid = 0usize;
  let mut checked_invalid = 0usize;

  for entry in corpus_files() {
    let src = std::fs::read_to_string(&entry).unwrap();
    let lossless_errs = lossless_has_errors(&src);
    let syntactic_errs = syntactic_has_errors(&src);
    assert_eq!(
      lossless_errs,
      syntactic_errs,
      "{}: lossless says errors={lossless_errs}, syntactic says errors={syntactic_errs}",
      entry.display()
    );

    // Each class is asserted against **both** suites, not only against `lossless_errs`. The
    // equality above already implies the syntactic half — but it is the one assertion in this
    // loop that no corpus entry can ever make fail, precisely because the corpus is an agreeing
    // set. Neutering it would therefore be a silent mutation, and these lines are what keep that
    // mutation harmless: without them, dropping the comparison would drop the syntactic suite out
    // of the gate entirely and leave a lossless-only smoke test wearing a parity gate's name.
    let name = name_of(&entry);
    if name.starts_with("invalid_") {
      assert!(
        lossless_errs,
        "{name} is named invalid but the lossless suite accepted it"
      );
      assert!(
        syntactic_errs,
        "{name} is named invalid but the syntactic suite accepted it"
      );
      checked_invalid += 1;
    } else if name.starts_with("valid_") {
      assert!(
        !lossless_errs,
        "{name} is named valid but the lossless suite rejected it"
      );
      assert!(
        !syntactic_errs,
        "{name} is named valid but the syntactic suite rejected it"
      );
      checked_valid += 1;
    } else {
      // A `panic!`, not a fall-through to "valid". Phase A's original routed on the `invalid_`
      // prefix and treated everything else as valid, so `invald_foo.graphqlx` would have been
      // silently asserted to parse.
      panic!("{name}: a corpus entry must be named valid_* or invalid_*");
    }
  }

  assert!(
    checked_valid >= 20,
    "only {checked_valid} valid entries; the corpus is too thin"
  );
  assert!(
    checked_invalid >= 5,
    "only {checked_invalid} invalid entries; no negative control"
  );
}

/// A misspelled prefix cannot smuggle an entry into the wrong class.
///
/// The `panic!` inside the parity loop already catches this, but only once that loop reaches the
/// entry — and it reaches entries in sorted order behind a `assert_eq!` that could stop it first.
/// This states the naming rule on its own.
#[test]
fn every_corpus_entry_declares_its_expected_verdict() {
  for entry in corpus_files() {
    let name = name_of(&entry);
    assert!(
      name.starts_with("valid_") || name.starts_with("invalid_"),
      "{name}: a corpus entry must be named valid_* or invalid_*"
    );
  }
}

/// The GraphQL corpus entry GraphQLx **accepts**, and the name it is filed under here.
///
/// One entry, measured rather than assumed — see this file's module docs. The pin is a rename
/// rather than an exclusion, because excluding it would drop the coverage the fixture carries and
/// leave the divergence untested; filed as `valid_`, the divergence is a fact this gate holds
/// every run.
const GRAPHQL_ENTRIES_GRAPHQLX_ACCEPTS: &[(&str, &str, &str)] = &[(
  "invalid_fragment_named_on.graphql",
  "valid_fragment_named_on.graphqlx",
  "GraphQL's `FragmentName: Name but not \"on\"` has no GraphQLx counterpart: \
   `executable_definition_name` is a plain `take_name`",
)];

/// Every GraphQL corpus entry is here exactly once, under its own stem or under a pinned rename.
///
/// The inherited half is a **copy**, and a copy is the kind of thing that rots quietly: an entry
/// added to `tests/corpus/` and not to `tests/corpusx/` leaves this gate measuring a strictly
/// smaller language than the one the GraphQL gate measures, and nothing else would say so.
#[test]
fn the_inherited_half_is_exactly_the_graphql_corpus() {
  let graphql_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpus");
  let inherited: BTreeSet<String> = std::fs::read_dir(&graphql_dir)
    .unwrap_or_else(|e| panic!("the GraphQL corpus is unreadable: {e}"))
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphql"))
    .map(|path| name_of(&path))
    .collect();
  let here: BTreeSet<String> = corpus_files().iter().map(|p| name_of(p)).collect();

  for source in &inherited {
    let renamed = GRAPHQL_ENTRIES_GRAPHQLX_ACCEPTS
      .iter()
      .find(|(from, _, _)| from == source)
      .map(|(_, to, _)| (*to).to_string());
    let expected = renamed.unwrap_or_else(|| source.replace(".graphql", ".graphqlx"));
    assert!(
      here.contains(&expected),
      "{source} has no GraphQLx counterpart; expected {expected} in tests/corpusx/"
    );
  }

  // The other direction: a pinned rename whose source has been deleted, or whose target was never
  // written, is a stale pin rather than a passing one.
  for (from, to, _) in GRAPHQL_ENTRIES_GRAPHQLX_ACCEPTS {
    assert!(
      inherited.contains(*from),
      "{from} is pinned as a GraphQLx acceptance but is not in the GraphQL corpus"
    );
    assert!(
      here.contains(*to),
      "{from} is pinned as renamed to {to}, which does not exist"
    );
    assert!(
      !here.contains(&from.replace(".graphql", ".graphqlx")),
      "{from} is pinned as renamed to {to} but is also present under its own stem"
    );
  }

  // And the GraphQLx-only half is not allowed to be a token gesture. Every divergence in Tasks
  // 10–14's table needs an entry, which is twenty-two at minimum.
  let own: Vec<&String> = here.iter().filter(|n| n.contains("_x_")).collect();
  let own_valid = own.iter().filter(|n| n.starts_with("valid_")).count();
  let own_invalid = own.iter().filter(|n| n.starts_with("invalid_")).count();
  assert!(
    own_valid >= 22,
    "only {own_valid} GraphQLx-only valid entries; one per divergence is twenty-two"
  );
  assert!(
    own_invalid >= 8,
    "only {own_invalid} GraphQLx-only invalid entries; the GraphQLx archetypes are unterminated \
     `<`, a `where` with no block, and an import with no `from`"
  );
}

/// Neither verdict function is a constant.
///
/// The one mutation the parity loop cannot survive is a verdict function pinned to a literal, and
/// it is invisible there: a corpus of agreeing entries agrees with `false` as readily as with the
/// truth. Two sources each, one accepted and one rejected.
#[test]
fn both_verdicts_answer_in_both_directions() {
  const ACCEPTED: &str = "type T<A> where A: Node { f: A }";
  const REJECTED: &str = "type T { a: ns::Box<Int }";

  assert!(!lossless_has_errors(ACCEPTED));
  assert!(lossless_has_errors(REJECTED));
  assert!(!syntactic_has_errors(ACCEPTED));
  assert!(syntactic_has_errors(REJECTED));
}

/// The corpus is discriminating material rather than inert.
///
/// GraphQLx has three roots and `parse_str` drives the mixed one. The SDL-only root rejects every
/// executable definition and every shorthand operation, so an entry on which the two roots
/// **disagree** is an entry that can tell two GraphQLx productions apart — which is what makes the
/// parity comparison a claim rather than a tautology.
///
/// Each counted entry is additionally asserted to be a *clean* discriminator: the mixed root
/// agrees with the syntactic suite about it. Without that half a discriminator could be an entry
/// both roots get wrong in different ways.
#[test]
fn the_corpus_can_tell_the_two_suites_apart() {
  let mut discriminators = 0usize;
  for entry in corpus_files() {
    let src = std::fs::read_to_string(&entry).unwrap();
    let mixed = lossless_has_errors(&src);
    let sdl_only = parse_type_system_document(&src).has_errors();
    if mixed == sdl_only {
      continue;
    }
    discriminators += 1;
    assert_eq!(
      mixed,
      syntactic_has_errors(&src),
      "{}: separates the two roots, but the mixed root disagrees with the syntactic suite — \
       a dirty discriminator proves nothing",
      entry.display()
    );
  }
  assert!(
    discriminators >= 20,
    "only {discriminators} entries separate the SDL-only root from the mixed one; the corpus \
     cannot tell two GraphQLx productions apart and this gate would pass on an inert set"
  );
}

/// Gate 1, extended to the **two alternate roots** now that each is a shipped entry point.
///
/// GraphQL's twin carries the reasoning; the short version is that until smear issue #67 the two
/// alternate roots were reachable only from a `test_support` driver, so every assertion over them
/// compared the lossless suite with itself. Each is now held against **its own syntactic
/// counterpart** — not against the mixed one, which accepts strictly more than either and would
/// force the comparison to be weakened into an implication.
#[test]
fn both_alternate_roots_agree_with_their_syntactic_counterparts() {
  let mut sdl_accepted = 0usize;
  let mut sdl_rejected = 0usize;
  let mut roots_disagree = 0usize;

  for entry in corpus_files() {
    let src = std::fs::read_to_string(&entry).unwrap();
    let name = entry.display();

    let lossless_sdl = parse_type_system_document(&src).has_errors();
    let lossless_executable = parse_executable_document(&src).has_errors();

    assert_eq!(
      lossless_sdl,
      syntactic_type_system_has_errors(&src),
      "{name}: the SDL-only roots disagree — lossless says has_errors={lossless_sdl}"
    );
    assert_eq!(
      lossless_executable,
      syntactic_executable_has_errors(&src),
      "{name}: the executable-only roots disagree — lossless says \
       has_errors={lossless_executable}"
    );

    if lossless_sdl {
      sdl_rejected += 1;
    } else {
      sdl_accepted += 1;
    }
    if lossless_sdl != lossless_executable {
      roots_disagree += 1;
    }
  }

  assert!(
    sdl_accepted > 0 && sdl_rejected > 0,
    "the SDL-only verdict is a constant over this corpus ({sdl_accepted} accepted, \
     {sdl_rejected} rejected); a parity assertion against it proves nothing"
  );
  assert!(
    roots_disagree >= 20,
    "only {roots_disagree} corpus entries separate the SDL-only entry point from the \
     executable-only one; the two are being compared against oracles that cannot tell them apart"
  );
}

/// Gate 1 over the padded variants: the two suites agree about trivia-laden input too.
///
/// # Why the compact corpus is not enough, and why GraphQLx needs this more than GraphQL does
///
/// Hand-written source never puts trivia at the junctions where a lossless atom's answer decides
/// something. In GraphQL those junctions are `$ x`, `@ d`, `... F`, `alias : f`, `Int !`,
/// `f (a: 1)`. GraphQLx adds three where the *node kind* itself is at stake:
///
/// - `set { … }` versus the enum value `set` — decided on whether the next significant token is a
///   `{`, which may be any number of trivia away, which is why the production commits the keyword
///   and retro-wraps rather than peeking two tokens;
/// - `<T>` versus `<K => V>` — a set type or a map type, decided after the first inner type;
/// - a `where` clause's continuation — a two-token window whose second token is reached across
///   trivia.
///
/// An atom that stopped crossing trivia would answer the wrong one of each pair, and every one of
/// those wrong answers is a *rejection* in the lossless suite while `syntactic/` — whose lexer
/// skips trivia before the grammar exists — keeps accepting. That is a parity failure by
/// construction, and no compact fixture can produce it.
#[test]
fn both_suites_agree_on_every_padded_corpus_entry() {
  let mut skipped = Vec::new();
  let mut variants = 0usize;

  for entry in corpus_files() {
    let name = name_of(&entry);
    let src = std::fs::read_to_string(&entry).unwrap();
    let Some(boundaries) = token_boundaries(&src) else {
      skipped.push(name);
      continue;
    };
    for (label, pad) in ALPHABET {
      let padded = inject(&src, &boundaries, pad);
      let lossless_errs = lossless_has_errors(&padded);
      let syntactic_errs = syntactic_has_errors(&padded);
      assert_eq!(
        lossless_errs, syntactic_errs,
        "{name} padded with {label}: lossless says errors={lossless_errs}, syntactic says \
         errors={syntactic_errs}"
      );
      // The verdict must also survive the padding, and it is asserted for **each suite against
      // its own compact answer** rather than only for the pair against each other. Injecting an
      // ignorable form cannot change whether a document is valid, so a run in which both suites
      // had started rejecting together would pass the line above and fail here. This is also what
      // keeps the syntactic suite load-bearing in the padded half: the `assert_eq!` above is the
      // one assertion an agreeing corpus can never red, exactly as in the compact loop.
      assert_eq!(
        lossless_errs,
        lossless_has_errors(&src),
        "{name} padded with {label}: the lossless verdict changed under an ignorable form"
      );
      assert_eq!(
        syntactic_errs,
        syntactic_has_errors(&src),
        "{name} padded with {label}: the syntactic verdict changed under an ignorable form"
      );
      variants += 1;
    }
  }

  assert_eq!(
    skipped, UNPADDABLE,
    "the set of unlexable corpus entries moved; pin the change on purpose"
  );
  assert_eq!(
    variants,
    (corpus_files().len() - UNPADDABLE.len()) * ALPHABET.len(),
    "the padded sweep did not run one variant per lexable entry per ignorable form"
  );
}

/// [`token_boundaries`] answers `None` on exactly the class [`UNPADDABLE`] names.
///
/// Without this, an entry could drop out of the padded sweep because the boundary scan quietly
/// failed on it rather than because it is unlexable, and `skipped == UNPADDABLE` would still hold
/// the day somebody added it to the list.
#[test]
fn only_the_unlexable_entries_have_no_token_boundaries() {
  for entry in corpus_files() {
    let name = name_of(&entry);
    let src = std::fs::read_to_string(&entry).unwrap();
    let unlexable = token_boundaries(&src).is_none();
    assert_eq!(
      unlexable,
      UNPADDABLE.contains(&name.as_str()),
      "{name}: unlexable={unlexable}, but UNPADDABLE says otherwise"
    );
  }
}

/// The injection is an insertion and nothing else.
///
/// `inject` is the only thing standing between the padded sweep and a set of variants that are not
/// the corpus at all. Two properties pin it: the padded source contains the original as a
/// subsequence with the pad between every pair of boundaries, and removing every injected pad
/// gives the original back.
#[test]
fn the_injection_only_inserts() {
  let src = "type T<A> where A: Node { f: A }";
  let boundaries = token_boundaries(src).expect("the fixture lexes");
  for (label, pad) in ALPHABET {
    let padded = inject(src, &boundaries, pad);
    assert_eq!(
      padded.len(),
      src.len() + boundaries.len() * pad.len(),
      "{label}: the injection changed the source's own bytes"
    );
    assert_eq!(
      padded.replace(pad, ""),
      src.replace(pad, ""),
      "{label}: removing the pad does not give the source back"
    );
  }
}
