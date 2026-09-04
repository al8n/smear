#![cfg(all(feature = "rowan", any(feature = "graphql", feature = "graphqlx")))]

//! Every lossless door takes the source types the syntactic doors take, and the one boundary that
//! is UTF-8 answers `Err(Refused)` — no parse at all — rather than a diagnostic or a panic.
//!
//! # What al8n/smear#121 asked, and what these cells answer
//!
//! The issue's acceptance is *either* the doors accept the same source types the syntactic doors
//! do, with a cell instantiating each at a non-`str` source the crate ships an integration for,
//! *or* the `&str` stays and the header says which boundary forces it. This is the first half.
//!
//! **A backing is not the door's type parameter, and that is the shape of the answer.** The
//! lossless lexer is parameterised by the *slice* a source yields rather than by the source, so
//! its `Lexer::Source` is whichever of the two `logos` vocabularies generated it — `str` or
//! `[u8]`, and nothing else. `smear_parser::lossless::LosslessSource` is the bound the doors
//! carry instead: every shipped backing is viewed as one of those two without a copy, which is
//! what lets a `bytes::Bytes` holder call a lossless door at all, and call it **without
//! validating UTF-8 first**.
//!
//! # The three properties
//!
//! - **Parity.** For one document, every backing produces a `Parse` byte-identical to the `&str`
//!   parse: same green tree, same diagnostics, same verdict. The `Parse` is lifetime-free and
//!   carries no trace of which type produced it, so a door that read one source type differently
//!   shows up here and nowhere else.
//! - **The refusal.** A byte source that is not UTF-8 produces `Err(Refused::NonUtf8Source {
//!   valid_up_to, source_len })` and **no** `Parse` at all — not an empty tree, and not a panic.
//!   Deleting the door's up-front check is the plant: these cells then abort with `the graphql
//!   lossless event stream was refused: the bound source is not valid UTF-8 (first invalid
//!   sequence at byte 13). No production in this crate emits a stream this door refuses`, which is
//!   round 8 of al8n/smear#193's assertion being made false by a caller's input. `Refused`'s own
//!   note carries why the answer is an `Err` rather than a flagged `Parse`.
//! - **Deref coercion still reaches the door.** `&String` and `&&str` compiled against the `&str`
//!   doors and do not against a bare generic `&Src`; the cell below is what says the enumerated
//!   implementations cover them.
//! - **One alphabet.** Non-ASCII *outside* a string or a comment is where the two `logos`
//!   scanners disagree, and it is the population a clean document and a description string cannot
//!   speak for. Measured on this branch when a byte view was handed to the byte scanner, over
//!   `query Q {{ é }}`: `&str` gave two diagnostics at `10..12` and `13..14`, and `&[u8]`
//!   **aborted the process** — the byte scanner reported the two bytes of `é` separately, so a
//!   lexer-error span ended inside a code point and the sink refused to materialise the stream
//!   (`the lexer-error diagnostic at index 11 carries a span that does not slice the source`).
//!   Three-byte and four-byte scalars, a bare scalar between definitions and one at a token
//!   boundary all aborted the same way; the in-string, in-comment and clean-ASCII controls agreed.
//!   The doors validate and scan as text now, so the cells below assert identity — and they do it
//!   under ceilings tight enough to reach the point where the byte side first diverged, which was
//!   `max_produce_events = 7` and `max_tokens = 6` for GraphQL.

use smear::parser::lossless::{Refused, runner::Parse as ParseOf};

/// One document that exercises both halves of the mixed root.
const DOC: &str = "type T { f(a: Int = 1): [String!]! @d } query Q($v: Int) { f(a: $v) { g } }";

/// An SDL-only document, for the type-system root.
const SDL: &str = "\"\"\"docs\"\"\" type T implements I @d { f: Int } extend type T { g: String }";

/// An executable-only document, for the request root.
const REQ: &str =
  "query Q($v: Int = 3) { f(a: $v) { ...F } } fragment F on T { g @skip(if: true) }";

/// A document with a syntax error the parser recovers from, and one with a multi-byte character.
///
/// These are the two populations where the two scan alphabets could disagree and a clean document
/// cannot say so: `str` scans with `Char = char` and `[u8]` with `Char = u8`, so a *reported*
/// lexeme and a non-ASCII one are where a difference would live. A [`Parse`](ParseOf) keeps the
/// span and the severity and drops the typed payload, so what these rows compare is exactly what a
/// consumer sees.
const BROKEN: &str = "type T { f(: Int } query Q { f(a: ) } fragment on T { g }";
const WIDE: &str = "\"\"\"héllo — a description with a multi-byte dash\"\"\" type T { f: Int }";

/// Non-ASCII **outside** any string or comment, which is where the two scan alphabets disagreed.
///
/// Two-, three- and four-byte scalars in a selection, a bare scalar between two definitions, and
/// one directly against a token boundary. Each is valid UTF-8 and each is a lexeme this parser
/// reports, which is the combination that made the byte scanner emit a span inside a code point.
const NON_ASCII: &[(&str, &str)] = &[
  ("2-byte scalar in a selection", "query Q { é }"),
  ("3-byte scalar in a selection", "query Q { € }"),
  ("4-byte scalar in a selection", "query Q { \u{1F600} }"),
  (
    "bare scalar between definitions",
    "type A { f: Int } € type B { g: Int }",
  ),
  ("scalar at a token boundary", "type A{f:Int}€type B{g:Int}"),
];

/// The document the ceiling sweeps run over, and the highest ceiling they reach.
///
/// `11` is past the point where the second diagnostic appears at both ceilings — measured for
/// GraphQL at `max_produce_events = 7` and `max_tokens = 6`, which are exactly the first values at
/// which the byte scanner diverged. The sweep asserts identity at every step and that it actually
/// crossed that point, rather than pinning a number that is a fact about one dialect's vocabulary.
const SWEEP_SRC: &str = "query Q { é }";
const SWEEP_MAX: usize = 11;

/// A source whose byte 13 is not a UTF-8 lead, so `Utf8Error::valid_up_to` is 13.
const BAD: &[u8] = b"query Q { f }\xFF\xFEmore";

/// Where [`BAD`] stops being text.
const BAD_VALID_UP_TO: usize = 13;

/// Two parses agree on everything a `Parse` carries.
fn same<L: rowan::Language>(label: &str, got: &ParseOf<L>, want: &ParseOf<L>) {
  assert_eq!(
    got.green(),
    want.green(),
    "{label}: the green tree differs from the `&str` parse's"
  );
  assert_eq!(
    got.diagnostics(),
    want.diagnostics(),
    "{label}: the diagnostics differ from the `&str` parse's"
  );
  assert_eq!(
    got.has_errors(),
    want.has_errors(),
    "{label}: the verdict differs from the `&str` parse's"
  );
}

/// The `Parse` a fallible door produced, or a message naming what it refused instead.
///
/// Every `_from` sibling answers `Result<Parse, Refused>`, and in a parity cell an `Err` is a bug
/// in the door rather than a case to handle: every source these cells hand over is valid UTF-8.
fn ok<L: rowan::Language>(got: Result<ParseOf<L>, Refused>) -> ParseOf<L> {
  got.unwrap_or_else(|refused| panic!("a valid source was refused: {refused}"))
}

/// What a fallible door answers for a source `rowan` cannot store.
///
/// `Err`, with both numbers, and **no** `Parse`. That is the whole shape: there is no tree to
/// reprint, no `has_errors()` to consult and nothing to hand a pairing door, because the value the
/// earlier rounds of al8n/smear#121 built does not exist. See [`Refused`] for what it cost.
fn refused<L: rowan::Language>(label: &str, got: Result<ParseOf<L>, Refused>) {
  match got {
    Ok(_) => panic!("{label}: a source that is not UTF-8 produced a `Parse`"),
    Err(refusal) => assert_eq!(
      refusal,
      Refused::NonUtf8Source {
        valid_up_to: BAD_VALID_UP_TO,
        source_len: BAD.len(),
      },
      "{label}: the refusal does not carry both numbers"
    ),
  }
}

macro_rules! dialect {
  ($modname:ident, $feature:literal, $lossless:path) => {
    #[cfg(feature = $feature)]
    mod $modname {
      use super::{
        BAD, BROKEN, DOC, NON_ASCII, REQ, Refused, SDL, SWEEP_MAX, SWEEP_SRC, WIDE, ok, refused,
        same,
      };
      use smear::lexer::limits::LosslessLimits;
      use $lossless as door;

      /// Every backing this crate ships an integration for, at all three roots, against `&str`.
      ///
      /// The rows are derived from `smear`'s own optional-source feature list rather than from
      /// the two backings the issue happened to name, and each row hands the door the backing
      /// **itself** — not a view the test took first, which is the whole claim.
      #[test]
      fn every_backing_parses_to_the_same_tree_as_str() {
        let mut rows = 0usize;

        macro_rules! at_every_root {
          ($label:literal, $src:expr) => {{
            same(
              concat!($label, ": parse_document"),
              &ok(door::parse_document_from($src)),
              &door::parse_document(DOC),
            );
            same(
              concat!($label, ": parse_document_with_limits"),
              &ok(door::parse_document_from_with_limits($src, Default::default())),
              &door::parse_document(DOC),
            );
            rows += 1;
          }};
          ($label:literal, $src:expr, broken) => {{
            let want = door::parse_document(BROKEN);
            assert!(
              want.has_errors(),
              "the BROKEN document must be one the parser reports"
            );
            same(
              concat!($label, ": parse_document"),
              &ok(door::parse_document_from($src)),
              &want,
            );
            rows += 1;
          }};
          ($label:literal, $src:expr, wide) => {{
            let want = door::parse_document(WIDE);
            assert!(
              !want.has_errors(),
              "the WIDE document must be one the parser accepts"
            );
            same(
              concat!($label, ": parse_document"),
              &ok(door::parse_document_from($src)),
              &want,
            );
            rows += 1;
          }};
          ($label:literal, $src:expr, sdl) => {{
            same(
              concat!($label, ": parse_type_system_document"),
              &ok(door::parse_type_system_document_from($src)),
              &door::parse_type_system_document(SDL),
            );
            same(
              concat!($label, ": parse_type_system_document_with_limits"),
              &ok(door::parse_type_system_document_from_with_limits($src, Default::default())),
              &door::parse_type_system_document(SDL),
            );
          }};
          ($label:literal, $src:expr, req) => {{
            same(
              concat!($label, ": parse_executable_document"),
              &ok(door::parse_executable_document_from($src)),
              &door::parse_executable_document(REQ),
            );
            same(
              concat!($label, ": parse_executable_document_with_limits"),
              &ok(door::parse_executable_document_from_with_limits($src, Default::default())),
              &door::parse_executable_document(REQ),
            );
          }};
        }

        // The two shapes a view can be, and the byte half is the one the `&str` doors refused.
        at_every_root!("&[u8]", DOC.as_bytes());
        at_every_root!("&[u8]", SDL.as_bytes(), sdl);
        at_every_root!("&[u8]", REQ.as_bytes(), req);

        // The two populations a clean document cannot speak for: a recovered parse, and one
        // whose lexemes are not all one byte wide.
        at_every_root!("&[u8] over a recovered parse", BROKEN.as_bytes(), broken);
        at_every_root!("&[u8] over a multi-byte document", WIDE.as_bytes(), wide);

        // A DOOR TAKEN AS A VALUE. The concrete one is already a `fn(&str)`, and the wide one
        // coerces at whichever source type the expected signature names — so a consumer passing
        // either to a higher-order function is unaffected. Compile-time assertions: binding them
        // is the whole test.
        let _as_value: fn(&str) -> door::Parse = door::parse_document;
        let _wide_str: fn(&str) -> Result<door::Parse, Refused> = door::parse_document_from;
        let _wide_bytes: fn(&[u8]) -> Result<door::Parse, Refused> = door::parse_document_from;

        // What deref coercion used to reach the `&str` doors with.
        let (owned_doc, owned_sdl, owned_req) = (DOC.to_owned(), SDL.to_owned(), REQ.to_owned());
        at_every_root!("&String", &owned_doc);
        at_every_root!("&String", &owned_sdl, sdl);
        at_every_root!("&String", &owned_req, req);
        at_every_root!("&&str", &DOC);
        at_every_root!("Box<str>", &DOC.to_owned().into_boxed_str());
        at_every_root!("Vec<u8>", &DOC.as_bytes().to_vec());
        at_every_root!("Cow<str>", &std::borrow::Cow::<str>::Borrowed(DOC));
        at_every_root!("Arc<str>", &std::sync::Arc::<str>::from(DOC));

        #[cfg(feature = "bytes")]
        {
          at_every_root!(
            "bytes::Bytes",
            &bytes::Bytes::copy_from_slice(DOC.as_bytes())
          );
          at_every_root!(
            "bytes::Bytes",
            &bytes::Bytes::copy_from_slice(SDL.as_bytes()),
            sdl
          );
          at_every_root!(
            "bytes::Bytes",
            &bytes::Bytes::copy_from_slice(REQ.as_bytes()),
            req
          );
        }

        #[cfg(feature = "bstr")]
        {
          at_every_root!("bstr::BStr", bstr::BStr::new(DOC.as_bytes()));
          at_every_root!("bstr::BStr", bstr::BStr::new(SDL.as_bytes()), sdl);
          at_every_root!("bstr::BStr", bstr::BStr::new(REQ.as_bytes()), req);
        }

        #[cfg(feature = "hipstr")]
        {
          at_every_root!("hipstr::HipStr", &hipstr::HipStr::from(DOC));
          at_every_root!("hipstr::HipByt", &hipstr::HipByt::from(DOC.as_bytes()));
          at_every_root!("hipstr::HipStr", &hipstr::HipStr::from(SDL), sdl);
          at_every_root!("hipstr::HipByt", &hipstr::HipByt::from(REQ.as_bytes()), req);
        }

        // NO smol-bytes ROW, and the reason is the manifest rather than the door.
        // `smear/Cargo.toml` forwards that feature (`smear-lexer/smol-bytes`,
        // `smear-parser/smol-bytes`, `tokora/smol_bytes_0_1`) and takes no `dep:` on the crate, so
        // this test — which is compiled against `smear` — cannot name the four types. Their
        // `LosslessSource` implementations live beside the others in
        // `smear-parser/src/lossless/source.rs` and are compiled by that crate's own
        // `--all-features` build.

        // Asserted rather than merely printed: a backing whose feature is off is a smaller number
        // here, not a green run over one row.
        println!("  {rows} non-`&str` sources agreed with the `&str` parse at every root");
        assert!(rows >= 8, "only {rows} non-`&str` sources were exercised");
      }

      /// Non-ASCII outside strings and comments reads the same through every byte backing.
      ///
      /// The population the parity cell above cannot reach: a multi-byte scalar the parser
      /// *reports*. At HEAD, with the byte view handed to the byte scanner, every row here
      /// aborted the process over `&[u8]` while `&str` answered two diagnostics — see this file's
      /// header for the measurement. All six doors, because a root that dispatched differently
      /// would show up at one of them and not the others.
      #[test]
      fn non_ascii_outside_strings_reads_the_same_through_every_byte_backing() {
        let mut checked = 0usize;
        for (what, src) in NON_ASCII {
          let bytes = src.as_bytes();

          macro_rules! at_each_door {
            ($label:expr, $view:expr) => {{
              let v = $view;
              same(
                &format!("{what} / {}: parse_document", $label),
                &ok(door::parse_document_from(v)),
                &door::parse_document(*src),
              );
              same(
                &format!("{what} / {}: parse_document_with_limits", $label),
                &ok(door::parse_document_from_with_limits(v, Default::default())),
                &door::parse_document(*src),
              );
              same(
                &format!("{what} / {}: parse_type_system_document", $label),
                &ok(door::parse_type_system_document_from(v)),
                &door::parse_type_system_document(*src),
              );
              same(
                &format!("{what} / {}: parse_type_system_document_with_limits", $label),
                &ok(door::parse_type_system_document_from_with_limits(v, Default::default())),
                &door::parse_type_system_document(*src),
              );
              same(
                &format!("{what} / {}: parse_executable_document", $label),
                &ok(door::parse_executable_document_from(v)),
                &door::parse_executable_document(*src),
              );
              same(
                &format!("{what} / {}: parse_executable_document_with_limits", $label),
                &ok(door::parse_executable_document_from_with_limits(v, Default::default())),
                &door::parse_executable_document(*src),
              );
              checked += 1;
            }};
          }

          // The `&str` parse must actually report something, or every row below compares two
          // clean parses and the cell is vacuous on the thing it is for.
          assert!(
            door::parse_document(*src).has_errors(),
            "{what}: `{src}` parses clean, so this row cannot see a lexeme-boundary difference"
          );

          at_each_door!("&[u8]", bytes);
          #[cfg(feature = "bytes")]
          at_each_door!("bytes::Bytes", &bytes::Bytes::copy_from_slice(bytes));
          #[cfg(feature = "bstr")]
          at_each_door!("bstr::BStr", bstr::BStr::new(bytes));
          #[cfg(feature = "hipstr")]
          at_each_door!("hipstr::HipByt", &hipstr::HipByt::from(bytes));
        }
        println!("  {checked} (document, byte backing) pairs agreed with `&str` at all six doors");
        assert!(checked >= NON_ASCII.len(), "only {checked} pairs were exercised");
      }

      /// The same identity under ceilings tight enough to reach where the byte side diverged.
      ///
      /// A per-byte alphabet spends more lexemes over the same characters, so a token or
      /// produce-event ceiling refuses at a different point — which is a difference a parse at the
      /// default limits cannot show. The sweep asserts `&str` and `&[u8]` agree at every ceiling
      /// **and** that it crossed the point where a second diagnostic appears, so a sweep that only
      /// ever saw the first one could not pass by being too narrow.
      #[test]
      fn every_ceiling_answers_the_same_through_a_byte_view() {
        let bytes = SWEEP_SRC.as_bytes();
        let mut widths = std::collections::BTreeSet::new();

        for (name, mk) in [
          (
            "max_produce_events",
            (|n| LosslessLimits::default().with_max_produce_events(n))
              as fn(usize) -> LosslessLimits,
          ),
          ("max_tokens", |n| {
            LosslessLimits::default().with_max_tokens(n)
          }),
        ] {
          for n in 0..=SWEEP_MAX {
            let want = door::parse_document_with_limits(SWEEP_SRC, mk(n));
            widths.insert((name, want.diagnostics().len()));
            same(
              &format!("{name}={n}: &[u8]"),
              &ok(door::parse_document_from_with_limits(bytes, mk(n))),
              &want,
            );
            #[cfg(feature = "bytes")]
            same(
              &format!("{name}={n}: bytes::Bytes"),
              &ok(door::parse_document_from_with_limits(
                &bytes::Bytes::copy_from_slice(bytes),
                mk(n),
              )),
              &want,
            );
            #[cfg(feature = "bstr")]
            same(
              &format!("{name}={n}: bstr::BStr"),
              &ok(door::parse_document_from_with_limits(
                bstr::BStr::new(bytes),
                mk(n),
              )),
              &want,
            );
          }
        }

        // The control on the sweep's own reach. Both ceilings must show at least two different
        // diagnostic counts across `0..=SWEEP_MAX`, which is what says the range straddles the
        // point the byte alphabet first answered differently.
        for name in ["max_produce_events", "max_tokens"] {
          let seen = widths.iter().filter(|(n, _)| *n == name).count();
          assert!(
            seen >= 2,
            "{name}: the sweep saw {seen} distinct diagnostic counts over 0..={SWEEP_MAX}, so it \
             never reached the ceiling where a second lexeme appears and cannot be evidence about \
             it"
          );
        }
        println!("  both ceilings agreed at every step of 0..={SWEEP_MAX} through a byte view");
      }

      /// A byte source that is not UTF-8 is refused at every sibling, with both numbers.
      ///
      /// `Err(Refused::NonUtf8Source { valid_up_to, source_len })` and **no** `Parse` — see
      /// [`Refused`] for the two findings that shape replaced.
      #[test]
      fn every_sibling_refuses_a_non_utf8_source_with_both_numbers() {
        refused("parse_document", door::parse_document_from(BAD));
        refused(
          "parse_document_with_limits",
          door::parse_document_from_with_limits(BAD, Default::default()),
        );
        refused(
          "parse_type_system_document",
          door::parse_type_system_document_from(BAD),
        );
        refused(
          "parse_type_system_document_with_limits",
          door::parse_type_system_document_from_with_limits(BAD, Default::default()),
        );
        refused(
          "parse_executable_document",
          door::parse_executable_document_from(BAD),
        );
        refused(
          "parse_executable_document_with_limits",
          door::parse_executable_document_from_with_limits(BAD, Default::default()),
        );
      }

      /// The same refusal through every byte **backing**, which is the shape a consumer has.
      ///
      /// Every row hands the door the backing itself, and the `source_len` it comes back with is
      /// what says the door read the whole thing rather than a prefix. The concrete doors cannot
      /// reach this case at all: they take `&str`, and a `&str` has already met the only thing a
      /// green tree requires.
      #[test]
      fn every_byte_backing_is_refused_the_same_way() {
        refused("&[u8]", door::parse_document_from(BAD));

        #[cfg(feature = "bytes")]
        refused(
          "bytes::Bytes",
          door::parse_document_from(&bytes::Bytes::copy_from_slice(BAD)),
        );
        #[cfg(feature = "bstr")]
        refused("bstr::BStr", door::parse_document_from(bstr::BStr::new(BAD)));
        #[cfg(feature = "hipstr")]
        refused(
          "hipstr::HipByt",
          door::parse_document_from(&hipstr::HipByt::from(BAD)),
        );

        // A `Vec<u8>` and a `Box<[u8]>` too: the delegating implementations are the ones a
        // consumer reaches without naming a backing at all, and they must refuse identically.
        refused("Vec<u8>", door::parse_document_from(&BAD.to_vec()));
        refused(
          "Box<[u8]>",
          door::parse_document_from(&BAD.to_vec().into_boxed_slice()),
        );
      }

      /// A byte-string literal reaches a `_from` door as it is written, and parses to the `&str`
      /// tree.
      ///
      /// `b"…"` is a `&[u8; N]`, and inference does not unsize an array to `[u8]` to satisfy a
      /// generic bound, so without `LosslessSource for [u8; N]` every call below is `E0277`.
      /// Its red is therefore a compile failure of this file, not a named failing test.
      #[test]
      fn a_byte_string_literal_reaches_a_from_door() {
        const DOC_BYTES: &[u8; 75] =
          b"type T { f(a: Int = 1): [String!]! @d } query Q($v: Int) { f(a: $v) { g } }";
        assert_eq!(DOC_BYTES.as_slice(), DOC.as_bytes(), "the literal must be DOC's bytes");

        same(
          "b\"…\": parse_document",
          &ok(door::parse_document_from(
            b"type T { f(a: Int = 1): [String!]! @d } query Q($v: Int) { f(a: $v) { g } }",
          )),
          &door::parse_document(DOC),
        );
        same(
          "b\"…\": parse_document_with_limits",
          &ok(door::parse_document_from_with_limits(DOC_BYTES, Default::default())),
          &door::parse_document(DOC),
        );

        // An owned array, borrowed: the same implementation, reached through `&[u8; N]`.
        let owned: [u8; 75] = *DOC_BYTES;
        same(
          "[u8; N]: parse_document",
          &ok(door::parse_document_from(&owned)),
          &door::parse_document(DOC),
        );

        // And a literal that is not UTF-8 is refused like every other byte source.
        refused(
          "b\"…\" not UTF-8",
          door::parse_document_from(b"query Q { f }\xFF\xFEmore"),
        );
      }
    }
  };
}

/// The size half of the class, asked at the bound and asked of a number.
///
/// # The defect this is the gate for
///
/// al8n/smear#121 is "the bytes rowan cannot store", and for four rounds it had one member. The
/// other is length: `rowan` addresses text with `u32`, tokora's replay checks
/// `u32::try_from(source.len())` right after the root kind and answers
/// `FinishError::OffsetOverflow { index: 0 }` past it,
/// and `finish_parsed_root_with` unwraps that into a panic. So a `Vec<u8>` or a `bytes::Bytes`
/// longer than `u32::MAX` reached a `_from` door that advertises `Result<Parse, Refused>` and
/// terminated the thread instead — or the process, under `panic=abort`.
///
/// # Why the cell asks a number and not a door
///
/// Because the door-level case needs a **four gibibyte** allocation to reach, which is not a test
/// this suite can run on any machine it targets. So the classification is a function over `usize`
/// — [`Refused::for_source_len`] — the doors call it, and this walks it at the bound, one below
/// and one above. That is the whole of the decision; what the door adds is calling it first, which
/// the plant removes.
///
/// The `+1` row is guarded by `checked_add` rather than skipped: on a 32-bit target no `usize`
/// exceeds the bound, so there is no oversized source to classify and the row is vacuous **by
/// construction** rather than by omission. The assertion below says which of the two happened.
///
/// # What this cell does not reach, and what stands in for it
///
/// That a **door** consults the classifier. Measured: deleting the door's call and running this
/// whole file is green, because the door-level case needs the allocation this cell exists to
/// avoid. A cell cannot close that, so a type does — `LosslessView::as_text` answers `&str` or
/// `Refused` and decides both refusals itself, so there is no call a door can make that performs
/// the UTF-8 check and skips the size one. The fence is that the wrong door is unwritable, not
/// that a test would catch it.
#[test]
fn a_source_longer_than_a_green_tree_can_address_is_classified_as_refused() {
  const MAX: usize = Refused::MAX_SOURCE_LEN;

  // The bound is tokora's, read off the resolved dependency: `u32::try_from(source.len())`.
  assert_eq!(MAX, u32::MAX as usize, "the bound is not `u32::MAX`");

  // Everything at or under it is a source, not a refusal — including the two boundary rows and
  // the degenerate ones, so a classifier that refused everything could not pass.
  for len in [0, 1, 4096, MAX - 1, MAX] {
    assert_eq!(
      Refused::for_source_len(len),
      None,
      "{len} bytes is addressable and must not be refused"
    );
  }

  // And one byte more is not.
  match MAX.checked_add(1) {
    Some(over) => assert_eq!(
      Refused::for_source_len(over),
      Some(Refused::SourceTooLong {
        source_len: over,
        max: MAX,
      }),
      "{over} bytes is past the bound and must be refused, with both numbers"
    ),
    None => {
      // 32-bit: `usize::MAX == u32::MAX`, so the refusal is unreachable and that is the platform's
      // statement rather than this crate's. Asserted so the row is not silently absent.
      assert_eq!(
        core::mem::size_of::<usize>(),
        4,
        "the bound has no successor, which is only true where `usize` is 32 bits"
      );
    }
  }
}

/// The concrete doors still take everything they took before the widening.
///
/// al8n/smear#121's second round made all twelve of them generic, and Codex found what that costs
/// at a call site that never mentions a type: `String` implements `AsRef<str>` **and**
/// `AsRef<[u8]>`, both targets are a `LosslessSource`, so `parse_document(s.as_ref())` — which
/// compiled against the `&str` door — is `E0283` against a generic one. Measured with that exact
/// code, at the door, before the repair.
///
/// So the concrete doors are concrete again and the wide capability is a `_from` sibling. Every
/// line below is a compile-time assertion: `as_ref()`, a door taken as a value, and the deref
/// coercions a generic `&Src` would have refused.
///
/// **Its red is a compile failure of this file, not a named failing test**, which is what a
/// compile-time cell's red always is. Making `parse_document` generic again takes the whole binary
/// down; the first error rustc reports is the `MutexGuard` line rather than the `as_ref()` one,
/// because the moved `owned` taints what follows. Both halves were therefore also measured on
/// their own, and `as_ref()` alone is `E0283: cannot infer type of the type parameter Src`.
#[cfg(feature = "graphql")]
#[test]
fn the_concrete_doors_take_what_they_always_took() {
  use smear::parser::graphql::lossless as door;

  let owned = String::from(DOC);

  // THE ONE CODEX FOUND. No turbofish, no annotation, no `&*`.
  assert!(!door::parse_document(owned.as_ref()).has_errors());
  assert!(!door::parse_document_with_limits(owned.as_ref(), Default::default()).has_errors());

  // A door as a value, at each of the three roots.
  let _mixed: fn(&str) -> door::Parse = door::parse_document;
  let _sdl: fn(&str, smear::lexer::limits::LosslessLimits) -> door::Parse =
    door::parse_type_system_document_with_limits;
  let _req: fn(&str) -> door::Parse = door::parse_executable_document;

  // The deref coercions. Each is one the `&str` signature performs and a generic `&Src` does not.
  assert!(!door::parse_document(&owned).has_errors());
  assert!(!door::parse_document(&&*owned).has_errors());
  assert!(!door::parse_document(&owned.clone().into_boxed_str()).has_errors());
  assert!(!door::parse_document(&std::borrow::Cow::<str>::Borrowed(DOC)).has_errors());
  assert!(!door::parse_document(&std::sync::Arc::<str>::from(DOC)).has_errors());
  assert!(!door::parse_document(&std::sync::Mutex::new(owned).lock().unwrap()).has_errors());
}

/// A source the parser will not read produces no `Parse`, so nothing downstream can be handed one.
///
/// # The defect this is the gate for
///
/// The refusal used to be a `Parse` with an empty green root and one diagnostic, and an empty root
/// is a **success-shaped** value. Measured on `b"query Q {{ f }}\xFF\xFEmore"` — 19 bytes,
/// `valid_up_to` 13 — before the repair:
///
/// - `refused.syntax().text()` was `""`, and `syntax()`/`green()` are unconditional, so a
///   formatter that reprints an errorful tree — they commonly do — wrote an empty file;
/// - `has_errors()` could not separate it from a genuine parse of `""`, because an empty document
///   is itself a syntax error here and both reported;
/// - `verify_parse(&refused, "")` answered `Ok(())`, `Verified::new(&refused, "")` answered `Ok`
///   with a projection cost of 1, and `project_executable_document_recovered(&refused, "")`
///   answered `Ok(Recovery { projected: 0, skipped: 0 })` with `is_complete()` **true** — a clean,
///   complete projection of a document nobody parsed.
///
/// A round of flags answered each of those in turn and left the bad value constructible. This is
/// the repair instead: the fallible doors answer `Result<Parse, Refused>`, so there is no `Parse`
/// to reprint, no tree to certify and no state anyone has to remember to ask about.
///
/// # What can be asserted about a value that does not exist
///
/// Its absence, at every door, which is the first half below — and that the doors which *would*
/// have consumed it still work on parses that do exist, which is the second. The stronger
/// statement is the compiler's: `parse_document_from` hands back a `Result`, so the pairing doors
/// cannot be reached with a refusal without an `unwrap` that is not there to write.
#[cfg(feature = "graphql")]
#[test]
fn a_refused_source_produces_no_parse_for_anything_downstream() {
  use smear::parser::graphql::lossless::{
    self as door, Verified, project, project_executable_document,
    project_executable_document_recovered, project_type_system_document,
    project_type_system_document_recovered, verify_parse,
  };

  const WANT: Refused = Refused::NonUtf8Source {
    valid_up_to: BAD_VALID_UP_TO,
    source_len: BAD.len(),
  };

  // No `Parse` from any of the six, and both numbers on every one.
  for (label, got) in [
    ("parse_document_from", door::parse_document_from(BAD)),
    (
      "parse_document_from_with_limits",
      door::parse_document_from_with_limits(BAD, Default::default()),
    ),
    (
      "parse_type_system_document_from",
      door::parse_type_system_document_from(BAD),
    ),
    (
      "parse_type_system_document_from_with_limits",
      door::parse_type_system_document_from_with_limits(BAD, Default::default()),
    ),
    (
      "parse_executable_document_from",
      door::parse_executable_document_from(BAD),
    ),
    (
      "parse_executable_document_from_with_limits",
      door::parse_executable_document_from_with_limits(BAD, Default::default()),
    ),
  ] {
    assert_eq!(got.err(), Some(WANT), "{label}");
  }

  // And the seven doors that would have taken one still take a parse that exists — the control
  // that says the absence above is about the refusal rather than about a door that stopped
  // working. `project*` refuse this document for their own reasons (it is not their root), so
  // what is asserted is that they answer, not what they answer.
  let ran = door::parse_document(DOC);
  assert!(verify_parse(&ran, DOC).is_ok());
  assert!(Verified::new(&ran, DOC).is_ok());
  assert!(project(&ran, DOC).is_ok());
  let _ = project_executable_document(&ran, DOC);
  let _ = project_type_system_document(&ran, DOC);
  let _ = project_executable_document_recovered(&ran, DOC);
  let _ = project_type_system_document_recovered(&ran, DOC);
}

dialect!(graphql, "graphql", smear::parser::graphql::lossless);
dialect!(graphqlx, "graphqlx", smear::parser::graphqlx::lossless);
