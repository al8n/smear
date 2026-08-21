//! The `MaybeTerminal` arm census: which errors end the parse.
//!
//! GraphQL's sibling is `graphql/error/tests/terminal.rs`, and the two exist as a **pair on
//! purpose**. The `MaybeTerminal` impls are hand-written per dialect over disjoint variant sets,
//! so they are two implementations of one ruling rather than one implementation reached twice —
//! the shape where a repair lands in one file and dies there. This branch was caught by planting
//! three times on that axis; the third catch was this census existing only for GraphQL, with the
//! motivating plant (flip the `Lexer` arm to `false`) leaving every cell in the tree green over
//! here.
//!
//! **There are three impls per dialect, not one, and only two of them can be reached by a
//! parse.** [`ErrorData`]'s arms are the census below; [`Error`]'s delegation and [`Errors`]'s
//! fold sit above them. The delegation is on the path every document-root catch site takes, so
//! `nesting_depth.rs` pins it — planted `false`, and
//! `a_refusal_is_one_diagnostic_at_every_cycle` and `a_refusal_ends_every_document_root` both
//! went red at their GraphQLx cells. The fold cannot be reached that way at all, and
//! [`the_container_fold_keeps_a_stop_that_is_not_alone`] is why it has a cell here.

use core::marker::PhantomData;

use smear_lexer::{
  error::BadStateError,
  graphqlx::{error::LexerErrors, syntactic::SyntacticTokenKind},
};
use tokora::{SimpleSpan as Span, error::MaybeTerminal, utils::CowStr};

use crate::graphqlx::error::{Error, ErrorData, Errors, Expectation, UnexpectedEnd};

/// A stand-in for `smear_lexer::limits`'s `LimitExceeded`. The `Lexer` arm asks whether the
/// *variant* is `State`, not what the payload is, so any inhabited type exercises it — and using
/// one of this crate's own keeps the census out of the lexer's feature matrix.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct StateErr;

/// The error this census is written over, keyed the way the **lossless** door keys it: the state
/// error is a real budget rather than `()`, because that is what makes the `Lexer` arm's question
/// a real one.
type Data = ErrorData<&'static str, SyntacticTokenKind, char, Expectation, StateErr>;
type Err = Error<&'static str, SyntacticTokenKind, char, Expectation, StateErr>;
type Errs = Errors<&'static str, SyntacticTokenKind, char, Expectation, StateErr>;

/// Every variant of [`ErrorData`] answers [`MaybeTerminal::is_terminal`], and answers what this
/// list says.
///
/// # Two properties, one list, and the second is why this is a census rather than a few asserts
///
/// * **Every variant is covered.** The generated `tag` is a wildcard-free `match`, so a variant
///   added without an entry is an `E0004` *here* as well as in the impl. The impl's own
///   exhaustiveness already forces an *arm*; this is what forces the arm to be a **decision** — a
///   new variant cannot be waved through with `=> false` and no test saying so.
/// * **Each entry carries its expected answer beside its sample**, so there is no state in which a
///   variant is listed and unchecked.
///
/// Some variants appear **twice**, and those are the point: an arm that delegates has to be shown
/// reading the value rather than the variant, which takes one sample on each side. `Lexer` and
/// `UnexpectedEnd` are the two.
///
/// # Naming a variant directly is allowed here, and is not allowed in the producibility census
///
/// `graphql/error/tests/census.rs` forbids `ErrorData::Variant(…)` samples because the question
/// there is *can this be produced through a public door*, which a direct construction begs. The
/// question here is *what does the arm answer for this value*, and a value is exactly what it
/// needs; where a constructor exists it is used anyway, because a constructor `descend` or a
/// conversion actually reaches is the more useful sample.
///
/// # Why this is not an end-to-end cell
///
/// Two of the `true` answers cannot be reached through a shipped door. A `Lexer` error only
/// becomes an `Err` on the parser's channel when the **emitter rejects** its diagnostic, and both
/// lossless doors pin `Verbose`, which never rejects; with an accepting emitter a lexer state trip
/// latches tokora's poison boundary instead, the root loop's peek answers `None`, and the loop
/// exits `Ok` with no error to classify at all. So the arm is live only for a consumer with a
/// fail-fast emitter over this dialect's own container — precisely the caller tokora's rule tells
/// to write it, and precisely the caller no in-tree parse is.
#[test]
fn the_terminal_arms_answer_for_every_variant() {
  macro_rules! census {
    ($($variant:ident => [$(($sample:expr, $expected:expr)),+ $(,)?]),+ $(,)?) => {
      // Wildcard-free and generated from the same list as the samples below.
      fn tag(data: &Data) -> &'static str {
        match data {
          $(ErrorData::$variant { .. } => stringify!($variant),)+
        }
      }

      let mut cells = 0usize;
      $($({
        let sample: Data = $sample;
        assert_eq!(
          tag(&sample),
          stringify!($variant),
          "the sample recorded for {} produced a different variant",
          stringify!($variant),
        );
        assert_eq!(
          sample.is_terminal(),
          $expected,
          "{} answered {} for `is_terminal`, expected {}",
          stringify!($variant),
          sample.is_terminal(),
          $expected,
        );
        cells += 1;
      })+)+
      // A census that selected nothing exits `ok`. 9 = 7 variants + the two second samples.
      assert_eq!(cells, 9, "the cell set collapsed");
    };
  }

  let span = Span::new(0, 1);
  let end = UnexpectedEnd::with_name(
    0usize,
    CowStr::from_static("GraphQLx production"),
    Expectation::Name,
  );

  census! {
    // Terminal only when it holds a `State` refusal — a spent `smear_lexer::limits` budget, and
    // the carrier a scanner trip lands on unmarked when the emitter rejects its diagnostic. This
    // is the arm tokora's rule singles out as the one that catches people.
    Lexer => [
      (ErrorData::Lexer(LexerErrors::bad_state(span, StateErr)), true),
      (ErrorData::Lexer(LexerErrors::default()), false),
    ],

    // Always: a frame budget is never cleared by more input. Through the constructor
    // `lossless::depth::descend` reaches the variant by, so the two cannot disagree.
    NestingLimitExceeded => [(Err::nesting_limit_exceeded(span).into_data(), true)],

    // Delegated: the value decides, not the variant. A production that ran out of input is a
    // grammar rejection; the same variant is terminal when the scanner raised the flag on it.
    UnexpectedEnd => [
      (ErrorData::UnexpectedEnd(end.clone()), false),
      (ErrorData::UnexpectedEnd(end.into_terminal()), true),
    ],

    // Affirmatively recoverable: built from a construct the grammar rejected, not from a limit the
    // runtime refused.
    Unclosed => [(Err::unclosed_list(span).into_data(), false)],
    UnexpectedToken => [(
      Err::unexpected_token(SyntacticTokenKind::Colon, Expectation::Name, span).into_data(),
      false
    )],
    Other => [(ErrorData::Other(std::borrow::Cow::Borrowed("x")), false)],
    // Carries no diagnostic at all — it exists to keep `S` used.
    Source => [(ErrorData::Source(PhantomData), false)],
  }
}

/// The container fold is `any`, so a stop recorded beside an ordinary diagnostic is still a stop.
///
/// # Why the census above cannot ask this, and why nothing that parses can either
///
/// [`ErrorData`]'s arms are what the census covers. Two more [`MaybeTerminal`] impls sit above
/// them — [`Error`]'s delegation and [`Errors`]'s fold — and each is hand-written per dialect,
/// with its own way of being wrong. The fold's is `all` where it says `any`, and **no end-to-end
/// cell can see that one.** Every conversion `lossless_error_impls!` generates ends in
/// `…(span).into()`, and `From<Error> for Errors` is `core::iter::once(error).collect()`, so
/// every container the lossless machinery puts on the parser's error channel holds exactly one
/// error — the one length at which `any` and `all` agree. Planted: `any` → `all` left both
/// terminal censuses and all thirteen of `nesting_depth.rs`'s tests green.
///
/// [`Error`]'s delegation is **not** in that position and gets no cell of its own — it is on the
/// path every catch site takes, so planting `false` on it reddened
/// `a_refusal_is_one_diagnostic_at_every_cycle` and `a_refusal_ends_every_document_root` at their
/// GraphQLx cells. The two `Error`-level assertions below are this fold's premise rather than a
/// pin: a fold cannot be read without knowing what its elements answer.
///
/// # The multi-element container is reachable, so the fold is a contract and not dead code
///
/// [`Errors`] is public, with [`Extend`], `DerefMut<Target = Vec<_>>` and `From<Vec<_>>`, and
/// tokora's `ParseContext` is caller-implemented. An accumulating context is exactly the consumer
/// the fold's own note describes: the one whose real stop `all` would spend the moment one
/// ordinary diagnostic was recorded beside it.
///
/// # Three cells, three plants, because `all` reddens only the first of them
///
/// `all` fails this test at `[ordinary, stop]` and libtest stops there, which says nothing about
/// the two cells after it — so each was planted on its own, with a fold that is `any` everywhere
/// else:
///
/// * `self.0.is_empty() || …any(…)` → *an empty container holds no stop*. This is the shape
///   `all`'s vacuous truth takes, and it is the cell most easily written and never exercised.
/// * `self.0.last().is_some_and(…)` → *and so is one recorded before it*. A fold that reads one
///   position rather than the set passes both `[ordinary, stop]` and the empty case.
#[test]
fn the_container_fold_keeps_a_stop_that_is_not_alone() {
  fn container(errors: impl IntoIterator<Item = Err>) -> Errs {
    let mut collected = Errs::default();
    collected.extend(errors);
    collected
  }

  let span = Span::new(0, 1);
  let stop = Err::nesting_limit_exceeded(span);
  let ordinary = Err::unclosed_list(span);

  // The premise: what each element answers alone, through `Error`'s delegation to its data.
  assert!(
    stop.is_terminal(),
    "the refusal is the stop this fold exists to keep"
  );
  assert!(
    !ordinary.is_terminal(),
    "an unclosed list is a grammar rejection, not a stop"
  );

  // `n = 1` — the only length any in-tree parse reaches, and where `any` and `all` agree. Listed
  // so that the cells below are visibly the ones doing the work.
  assert!(container([stop.clone()]).is_terminal());
  assert!(!container([ordinary.clone()]).is_terminal());

  // `n = 2`, both orders. `all` answers `false` for each of these, and position must not decide.
  assert!(
    container([ordinary.clone(), stop.clone()]).is_terminal(),
    "a stop recorded after an ordinary diagnostic is still a stop"
  );
  assert!(
    container([stop, ordinary.clone()]).is_terminal(),
    "and so is one recorded before it"
  );
  assert!(
    !container([ordinary.clone(), ordinary]).is_terminal(),
    "two grammar rejections do not add up to a stop"
  );

  // `n = 0`. `all` answers `true` here, which would end a parse that had reported nothing.
  assert!(
    !Errs::default().is_terminal(),
    "an empty container holds no stop"
  );
}
