#![cfg(all(feature = "std", any(feature = "graphql", feature = "graphqlx")))]

//! `LosslessLimits::max_tokens` holds through **every** public lossless lexer entry point.
//!
//! # The defect this is the gate for
//!
//! The ceiling is enforced in `tokora`'s Logos adapter: one post-scan `check()` after each scan,
//! whose failure reports the refusal and then latches, so every later `lex()` answers `None`. Both
//! dialects used to publish their lexer as
//!
//! ```text
//! pub type LosslessLexer<'a, S = &'a str> = tokora::lexer::LogosLexer<'a, LosslessToken<S>>;
//! ```
//!
//! and a type alias is transparent, so that spelling also published `LogosLexer::into_inner`,
//! `::inner` and `::inner_mut` — three ways to reach the raw `logos::Lexer`, which is an `Iterator`
//! and runs neither the check nor the latch. Measured at `be0c3b0` on 4 000 `-` under
//! `with_max_tokens(0)`, in both dialects at both source types: the checked door produced **1**
//! item, the raw door produced **4 000**. `max_tokens` says the lex stops one lexeme past the
//! ceiling; through that door it did not stop at all.
//!
//! The repair removes the door — the aliases now name `smear_lexer::LosslessLexer`, a newtype that
//! publishes `tokora::Lexer` and nothing else. That the three accessors are gone is pinned where
//! it can be, at compile time, by the coded `compile_fail` fences on that type. **This file pins
//! the property instead of the door**: whatever surface exists, the item count obeys the ceiling.
//! A fence says one spelling stopped compiling; this says the guarantee holds.
//!
//! # What is enumerated, and why it is more than the two aliases
//!
//! The two dialect aliases are not the whole entry surface. Each is generic over the **slice**
//! type, and `token!` instantiates five of them per dialect — `&str` and `HipStr` over a `str`
//! source, `&[u8]`, `bytes::Bytes` and `HipByt` over a `[u8]` source. Ten cells, and every one is a
//! public path a consumer can name. `smear-parser`'s `GraphqlLosslessLexer` / `GraphqlxLosslessLexer`
//! and the `Lexer` alias beside each are further aliases *of these*, so they are the same types
//! rather than more of them.
//!
//! The **syntactic** lexers are deliberately not here. They are smear's own structs, they carry
//! `SyntacticLimits`, and that type has no token ceiling at all — only a nesting depth. There is no
//! `max_tokens` on that side for a door to escape.
//!
//! # The boundaries
//!
//! Read off the contract rather than the implementation: `with_max_tokens(n)` buys a lex that stops
//! one lexeme after its `n`th survivor, so a document of `n_lexemes` items yields
//! `min(ceiling + 1, n_lexemes)`. Both sides of the cliff are checked — `0`, `1`, one below the
//! document's own count, the **exact** count, and one above — because the interesting failure is a
//! ceiling that is reached and does not fire, and a ceiling that is not reached and does.
//!
//! `n_lexemes` is measured per cell with the ceiling lifted rather than asserted from a constant:
//! the count is a fact about the dialect's vocabulary, and pinning it here would make this file
//! fail for the reason a vocabulary changed instead of for the reason a ceiling stopped holding.

use smear::lexer::{limits::LosslessLimits, tokora::Lexer};

/// A malformed run: `-` is a rule that can only fail in both dialects, and it is the shape the
/// escape was measured on. Each `-` is one lexeme.
const RUN: usize = 400;

/// Drives one lexer to exhaustion under `ceiling` and counts the items it produced.
fn items<'a, L>(src: &'a L::Source, ceiling: Option<usize>) -> usize
where
  L: Lexer<'a, State = LosslessLimits>,
{
  let limits = match ceiling {
    Some(max) => LosslessLimits::default().with_max_tokens(max),
    None => LosslessLimits::default(),
  };
  let mut lexer = L::with_state(src, limits);
  let mut n = 0usize;
  while lexer.lex().is_some() {
    n += 1;
  }
  n
}

/// Checks one cell across the whole cliff.
fn cell<'a, L>(label: &str, src: &'a L::Source)
where
  L: Lexer<'a, State = LosslessLimits>,
{
  let total = items::<L>(src, None);
  assert!(
    total > 2,
    "{label}: the uncapped lex produced {total} items, which is too few for the boundaries below \
     to mean anything"
  );

  for ceiling in [0, 1, total - 1, total, total + 1] {
    let got = items::<L>(src, Some(ceiling));
    let want = core::cmp::min(ceiling + 1, total);
    assert_eq!(
      got, want,
      "{label}: max_tokens({ceiling}) over a {total}-lexeme document produced {got} items rather \
       than {want} — the ceiling stops the lex one lexeme past itself, and below its own count it \
       must stop at all"
    );
  }
  println!(
    "  {label}: {total} lexemes, ceiling honoured at 0, 1, {}, {total}, {}",
    total - 1,
    total + 1
  );
}

macro_rules! dialect {
  ($modname:ident, $feature:literal, $lex:path) => {
    #[cfg(feature = $feature)]
    mod $modname {
      use super::{RUN, cell};
      use $lex as LL;

      #[test]
      fn str_source() {
        let s = "-".repeat(RUN);
        cell::<LL<'_, &str>>("&str", s.as_str());
      }

      #[test]
      fn slice_source() {
        let s = "-".repeat(RUN);
        cell::<LL<'_, &[u8]>>("&[u8]", s.as_bytes());
      }

      #[cfg(feature = "bytes")]
      #[test]
      fn bytes_source() {
        let s = "-".repeat(RUN);
        cell::<LL<'_, bytes::Bytes>>("bytes::Bytes", s.as_bytes());
      }

      #[cfg(feature = "hipstr")]
      #[test]
      fn hipstr_source() {
        let s = "-".repeat(RUN);
        cell::<LL<'_, hipstr::HipStr<'_>>>("HipStr", s.as_str());
      }

      #[cfg(feature = "hipstr")]
      #[test]
      fn hipbyt_source() {
        let s = "-".repeat(RUN);
        cell::<LL<'_, hipstr::HipByt<'_>>>("HipByt", s.as_bytes());
      }
    }
  };
}

dialect!(
  graphql,
  "graphql",
  smear::lexer::graphql::lossless::LosslessLexer
);
dialect!(
  graphqlx,
  "graphqlx",
  smear::lexer::graphqlx::lossless::LosslessLexer
);
