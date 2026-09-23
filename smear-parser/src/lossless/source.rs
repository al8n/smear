//! What a lossless door accepts as a source, and the one conversion it makes.
//!
//! # Two requirements, and where they bind
//!
//! A green tree stores token text as `&str` and addresses it with `u32`, so `rowan` requires a
//! source to be **valid UTF-8** and **no longer than [`Refused::MAX_SOURCE_LEN`]**. The
//! requirements are independent: a 5 GiB `&str` is good text and cannot be materialised, and a
//! three-byte `[u8]` that is not text is short and cannot be materialised either. tokora states
//! the first as [`CstText`](tokora::cst::CstText) on the sink's source and the second as a
//! `u32::try_from` on its length; both are *refusals* at materialisation rather than type errors,
//! which is what lets a door take bytes at all.
//!
//! [`LosslessView::as_text`] decides both **for the `_from` siblings**, and answers a
//! [`Refused`]. The concrete `parse_*` doors do not call it: `&str` discharges the encoding
//! requirement by type and the length requirement not at all, so an over-length `&str` reaches
//! materialisation and panics there — the precondition documented on those doors.
//!
//! # A backing is viewed, never converted
//!
//! [`LosslessSource`] borrows a source as [`LosslessView::Text`] or [`LosslessView::Bytes`]
//! without a copy, and every backing this crate ships an integration for reaches a `_from` door
//! that way: `bytes::Bytes`, `bstr::BStr`, `HipStr`, `HipByt`, the smol-bytes forms, `String`,
//! `Vec<u8>`, a `[u8; N]` array (so a `b"…"` literal), a `Cow`, a `Box`, an `Rc`, an `Arc` and a
//! reference to any of them. What a byte backing pays is one length comparison and one UTF-8
//! validation, the second of which [`CstText`](tokora::cst::CstText) would have charged at
//! materialisation anyway.
//!
//! # There is one scanner below this module, and its alphabet is `char`
//!
//! Both dialects' lossless scanners are `logos` derives over `str` or over `[u8]`, and on
//! unmatched input `logos` resumes at the next *element* of its source — a character for one, a
//! byte for the other. They therefore disagree about lexeme boundaries on the same bytes, and only
//! one of the two can be materialised: over `query Q {{ é }}` the byte scanner reports the two
//! bytes of `é` separately, so a lexer-error span ends inside a code point and the sink refuses
//! the stream outright. So a byte view is validated and scanned **as text**, and
//! `parse_document(s)` and `parse_document_from(s.as_bytes())` describe the same parse:
//!
//! ```
//! # #[cfg(all(feature = "graphql", feature = "rowan"))] {
//! use smear_parser::graphql::lossless::{parse_document, parse_document_from};
//!
//! let src = "type T { f: Int } query Q { f }";
//!
//! let narrow = parse_document(src);
//! let wide = parse_document_from(src.as_bytes())
//!   .expect("valid UTF-8, and short enough for a green tree to address");
//!
//! // `Parse` is not `PartialEq` — it holds a green tree — so the comparison is over everything
//! // it publishes.
//! assert_eq!(wide.green(), narrow.green());
//! assert_eq!(wide.diagnostics(), narrow.diagnostics());
//! assert_eq!(wide.has_errors(), narrow.has_errors());
//! # }
//! ```
//!
//! `parse_document(s.as_bytes())` does not compile: the concrete doors are `fn(&str) -> Parse`,
//! and a byte source goes through `parse_document_from`.

use std::{borrow::Cow, boxed::Box, rc::Rc, string::String, sync::Arc, vec::Vec};

/// Why a source could not be read as a document at all.
///
/// The error half of every `parse_*_from` door. There is no [`Parse`](super::runner::Parse) on
/// this path and no tree: the door answers before it scans, so nothing was lexed and no event was
/// recorded.
///
/// # Two requirements, and only one of them is about being text
///
/// A green tree needs its source to be **valid UTF-8** *and* **no longer than
/// [`MAX_SOURCE_LEN`](Self::MAX_SOURCE_LEN)**, because `rowan` addresses text with `u32`. `&str`
/// proves the first and says nothing about the second.
///
/// # Which doors check which
///
/// - The `_from` siblings check **both**, size first, and answer this type.
/// - The concrete `parse_*` doors take `&str` and return a bare `Parse`. The length requirement is
///   discharged by nothing: an over-length `&str` reaches materialisation and panics there. Those
///   signatures return a bare `Parse` and have nowhere to report it; a caller who cannot bound its
///   input uses a sibling.
///
/// # Why an `Err` and not a state on the parse
///
/// Because an empty tree is a **success-shaped** value. `syntax()` and `green()` are
/// unconditional, so a refusal carried as a `Parse` with an empty root would let a formatter
/// reprint an empty file, and `has_errors()` could not separate it from a genuine parse of `""` —
/// an empty document is itself a syntax error in this grammar. Downstream it is worse: the
/// tree-only verification helpers would certify such a pair against `""`, and the recovering
/// projection would report a *complete* recovery of a document nobody parsed. A refusal that
/// cannot be a `Parse` removes all of it at once.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Refused {
  /// The source is not UTF-8, so no green tree could be built from it.
  ///
  /// Materialisation is where UTF-8 is required — the lexer, the productions and the substrate are
  /// all generic over bytes — and a `_from` sibling asks before scanning. It is one of the **two**
  /// requirements that boundary has; [`SourceTooLong`](Self::SourceTooLong) is the other, and
  /// [`LosslessView::as_text`] decides both **for the siblings**. A concrete `&str` door discharges
  /// this one by type and the other not at all.
  NonUtf8Source {
    /// Leading bytes that **are** valid UTF-8, from
    /// [`Utf8Error::valid_up_to`](core::str::Utf8Error::valid_up_to). Also the offset of the
    /// sequence that failed — the one position in the source a consumer can act on.
    valid_up_to: usize,
    /// How many bytes the source had.
    ///
    /// Carried because `valid_up_to` alone cannot say how much was refused: the same offset means
    /// something different in a 14-byte source and a 14-megabyte one.
    source_len: usize,
  },
  /// The source is longer than a green tree can address, so no tree could be built from it.
  ///
  /// tokora refuses the whole materialisation rather than truncating; see
  /// [`MAX_SOURCE_LEN`](Refused::MAX_SOURCE_LEN) for the bound and where it comes from.
  ///
  /// **Unreachable on a 32-bit target**, where no `usize` exceeds that bound.
  SourceTooLong {
    /// How many bytes the source had.
    source_len: usize,
    /// The largest a source may be — [`MAX_SOURCE_LEN`](Refused::MAX_SOURCE_LEN), repeated here
    /// so a caller rendering the error does not have to reach for the constant.
    max: usize,
  },
}

impl Refused {
  /// The largest source a green tree can address, in bytes: `u32::MAX`.
  ///
  /// tokora's `replay` runs `u32::try_from(source.len())` right after its two root-kind checks
  /// and refuses on `Err` with `FinishError::OffsetOverflow { index: 0 }`, before any token span
  /// is looked at — so this is the bound it enforces, and the **source length** is what it
  /// enforces it against. A source of exactly this many bytes is accepted and one byte more is
  /// refused. Every other offset in the materialisation — a token's start and end, a diagnostic's
  /// span — is an index *into* that source, so bounding it bounds all of them.
  pub const MAX_SOURCE_LEN: usize = u32::MAX as usize;

  /// The refusal a source of `source_len` bytes earns for its **size alone**, or `None`.
  ///
  /// Separate from the doors so it can be asked at the bound without allocating four gibibytes:
  /// the question is about a number, and a caller asking it passes a number.
  ///
  /// Written as a comparison rather than as a second `try_from` so a 32-bit target compiles it
  /// without a lint about a conversion that cannot fail.
  #[inline]
  pub const fn for_source_len(source_len: usize) -> Option<Self> {
    if source_len > Self::MAX_SOURCE_LEN {
      Some(Self::SourceTooLong {
        source_len,
        max: Self::MAX_SOURCE_LEN,
      })
    } else {
      None
    }
  }
}

impl core::fmt::Display for Refused {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::NonUtf8Source {
        valid_up_to,
        source_len,
      } => write!(
        f,
        "the {source_len}-byte source is not valid UTF-8 (first invalid sequence at byte \
         {valid_up_to}), so it is not a document"
      ),
      Self::SourceTooLong { source_len, max } => write!(
        f,
        "the source is {source_len} bytes, longer than the {max} a green tree can address"
      ),
    }
  }
}

impl core::error::Error for Refused {}

/// A lossless source, borrowed as a door will read it.
///
/// Two variants because a source is either already text or a run of bytes. Nothing is copied: both
/// arms borrow out of the value [`LosslessSource::lossless_view`] was called on, and
/// [`as_text`](Self::as_text) is where either becomes a `&str` a green tree can hold — or the
/// [`Refused`] naming which of the two requirements it failed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LosslessView<'a> {
  /// The source is already text.
  Text(&'a str),
  /// The source is bytes, and has not been shown to be UTF-8.
  Bytes(&'a [u8]),
}

impl<'a> LosslessView<'a> {
  /// How many bytes this view spans.
  ///
  /// Read on both refusal paths: it is what [`Refused::for_source_len`] classifies, and it is half
  /// of what [`Refused::NonUtf8Source`] records — `valid_up_to` says where the source stopped
  /// being text, and this says how much of it there was. Named `byte_len` rather than `len`
  /// because a text view's character count is a different number and this is never it.
  #[inline]
  pub const fn byte_len(self) -> usize {
    match self {
      Self::Text(text) => text.len(),
      Self::Bytes(bytes) => bytes.len(),
    }
  }

  /// This view as the text a green tree can hold — **the one place a `_from` sibling validates a
  /// source and borrows it as addressable text**, and the only place either reason one can be
  /// refused is decided. It builds no [`Parse`](super::runner::Parse): on `Ok` the `_from` sibling
  /// hands the `&str` to its concrete root, and that root runs the parse and constructs it.
  ///
  /// The concrete `parse_*` doors do not come through here at all: they take `&str` and reach that
  /// concrete root without it, so nothing on this page is a statement about them. What they get
  /// instead is the encoding requirement discharged by their parameter type and the length
  /// requirement discharged by nothing — see [`Refused`]'s `Which doors check which`.
  ///
  /// # Why both questions are one call
  ///
  /// The two ways a source is not something `rowan` can store are unrelated, and a door that asked
  /// one and forgot the other would compile and pass every cell — the size case needs a
  /// four-gibibyte source to reach, and no cell can allocate one. So neither question is a
  /// sibling's to ask: a sibling gets a `&str` or a [`Refused`], and there is no call that
  /// performs one check and skips the other.
  ///
  /// # Order, and why size comes first
  ///
  /// The size question is one comparison and the UTF-8 question is a pass over the whole buffer,
  /// and a source too long to address is refused either way — so asking size second would validate
  /// four gibibytes to reach a verdict already settled.
  ///
  /// [`Text`](Self::Text) then costs one comparison and nothing else: the compiler has already
  /// proved it is UTF-8. [`Bytes`](Self::Bytes) costs the comparison plus
  /// [`core::str::from_utf8`], one pass, and that is the whole price of reaching a `_from` door
  /// from a byte backing.
  ///
  /// # Errors
  ///
  /// [`Refused::SourceTooLong`] past [`Refused::MAX_SOURCE_LEN`], and
  /// [`Refused::NonUtf8Source`] for a byte view that is not text — the first invalid sequence's
  /// offset, and the length. A `_from` sibling returns these; it does not build a
  /// [`Parse`](super::runner::Parse) for either.
  #[inline]
  pub const fn as_text(self) -> Result<&'a str, Refused> {
    // SIZE FIRST. See this function's `Order` note; `byte_len` is `const` and so is the compare.
    if let Some(refused) = Refused::for_source_len(self.byte_len()) {
      return Err(refused);
    }
    match self {
      Self::Text(text) => Ok(text),
      Self::Bytes(bytes) => match core::str::from_utf8(bytes) {
        Ok(text) => Ok(text),
        Err(err) => Err(Refused::NonUtf8Source {
          valid_up_to: err.valid_up_to(),
          source_len: bytes.len(),
        }),
      },
    }
  }
}

/// A type a lossless door will parse.
///
/// # Why the doors take this rather than `&str`
///
/// The crate ships five source-representation integrations so a consumer can pick one, and this
/// is what makes the lossless half honour them: a caller holding `bytes::Bytes` reaches a `_from`
/// door with it. The bound asks for one borrow rather than for a conversion — a byte backing hands
/// over the bytes it already has.
///
/// # Why it is a trait and not a `&str` after deref coercion
///
/// Because `&str` reaches a `&str` door from `String`, `Box<str>`, `Cow<'_, str>` and every other
/// smart pointer *by* deref coercion, and deref coercion does not apply to a generic `&Src`. The
/// implementations below are what that coercion rule does, written out.
///
/// **The list is still not the rule, which is why this bound is on a sibling rather than on the
/// door.** `s.as_ref()` names no type at all, so no list can serve it: `String` is both
/// `AsRef<str>` and `AsRef<[u8]>` and both are implemented here, which makes such a call `E0283`.
/// That is unfixable from this side, and is what settled the shape — twelve concrete `&str` doors,
/// and a `_from` sibling each carrying this bound.
///
/// # What an enumeration does not cover, stated
///
/// Deref coercion is a rule and this is a list, so the list is not the rule, and **one** shape it
/// does not reach: a smart pointer over `str` or `[u8]` that is not implemented below — a
/// `MutexGuard<String>`, a downstream `Arc`-like. Through a `_from` sibling that needs a reborrow
/// (`&*guard`) or one implementation of this trait, which is why the trait is open.
///
/// It costs nothing, because it is a residual of the **sibling** and not of the door. The concrete
/// `parse_*` doors are `&str` and coerce, so a caller holding any of those shapes — or writing
/// `s.as_ref()`, which is the case an inferred `Src` cannot serve at all — uses the door and never
/// reaches this trait.
///
/// # It is open on purpose
///
/// Implementing it says only *view me as text or as bytes*, over a buffer the implementor already
/// owns. There is no emitter, no error container and no reporting authority anywhere in the
/// signature, which is what keeps smear issue #193's guarantee intact while the doors are generic:
/// the class it closed is *an in-crate caller chooses a type through which state can be shared*,
/// and a source buffer shares nothing. What a door does with the view is the door's and not the
/// implementor's: validate, then scan as text.
pub trait LosslessSource {
  /// This source, borrowed as text or as bytes.
  fn lossless_view(&self) -> LosslessView<'_>;
}

// ── THE TWO THINGS A VIEW CAN BE ──────────────────────────────────────────────────────────────
//
// Every other implementation below borrows into one of these.

impl LosslessSource for str {
  #[inline(always)]
  fn lossless_view(&self) -> LosslessView<'_> {
    LosslessView::Text(self)
  }
}

impl LosslessSource for [u8] {
  #[inline(always)]
  fn lossless_view(&self) -> LosslessView<'_> {
    LosslessView::Bytes(self)
  }
}

/// One delegating implementation: `$ty` is viewed as whatever `$view` borrows out of it.
///
/// A `macro_rules!` and not a blanket `impl<T: Deref<Target: LosslessSource>>`, which is what this
/// wants to be and **does not compile**: rustc refuses it as E0119 against the two implementations
/// above, on the ground that an upstream crate may add `impl Deref for str`. So the delegations
/// are enumerated, and the list is this file's answer to *which source types a lossless door
/// accepts*.
macro_rules! viewed_as {
  // The generic forms. The parameters are bracketed rather than written `where …` because a
  // `$(where $($t:tt)+)?` tail is a *local ambiguity* against the `$ty:ty` before it — rustc says
  // so outright — and brackets cannot be confused with the start of a type here, since neither
  // primitive is declared through this macro.
  ($(#[$meta:meta])* [$($generics:tt)*] $ty:ty => |$s:ident| $view:expr) => {
    $(#[$meta])*
    impl<$($generics)*> LosslessSource for $ty {
      #[inline(always)]
      fn lossless_view(&self) -> LosslessView<'_> {
        let $s = self;
        LosslessSource::lossless_view($view)
      }
    }
  };
  ($(#[$meta:meta])* $ty:ty => |$s:ident| $view:expr) => {
    $(#[$meta])*
    impl LosslessSource for $ty {
      #[inline(always)]
      fn lossless_view(&self) -> LosslessView<'_> {
        let $s = self;
        LosslessSource::lossless_view($view)
      }
    }
  };
}

// ── REFERENCES AND SMART POINTERS ─────────────────────────────────────────────────────────────
//
// What the `&str` doors accepted by deref coercion and a generic `&Src` does not. Each is one
// borrow and no copy.

viewed_as!([T: LosslessSource + ?Sized] &T => |s| *s);
viewed_as!([T: LosslessSource + ?Sized] Box<T> => |s| &**s);
viewed_as!([T: LosslessSource + ?Sized] Rc<T> => |s| &**s);
viewed_as!([T: LosslessSource + ?Sized] Arc<T> => |s| &**s);
viewed_as!([T: LosslessSource + ToOwned + ?Sized] Cow<'_, T> => |s| &**s);

// ── THE STANDARD OWNED FORMS ──────────────────────────────────────────────────────────────────

viewed_as!(String => |s| s.as_str());
viewed_as!(Vec<u8> => |s| s.as_slice());
viewed_as!(
  /// A byte array, so a byte-string literal reaches a `_from` door: inference does not unsize
  /// `[u8; N]` to `[u8]` through a generic bound, so without this `parse_document_from(b"…")` is
  /// `E0277`.
  [const N: usize] [u8; N] => |s| s.as_slice()
);

// ── THE SHIPPED BACKINGS ──────────────────────────────────────────────────────────────────────
//
// The five source integrations this crate ships, each gated with its own feature and each viewed
// as the half it already is. A byte one is **not** scanned as bytes — see the module header for
// the measurement that rules that out — it is validated once and scanned as text, exactly as a
// `&str` is.

viewed_as!(
  #[cfg(feature = "bytes")]
  #[cfg_attr(docsrs, doc(cfg(feature = "bytes")))]
  bytes::Bytes => |s| &s[..]
);

viewed_as!(
  #[cfg(feature = "bstr")]
  #[cfg_attr(docsrs, doc(cfg(feature = "bstr")))]
  bstr::BStr => |s| <bstr::BStr as AsRef<[u8]>>::as_ref(s)
);

viewed_as!(
  #[cfg(feature = "hipstr")]
  #[cfg_attr(docsrs, doc(cfg(feature = "hipstr")))]
  hipstr::HipStr<'_> => |s| s.as_str()
);

viewed_as!(
  #[cfg(feature = "hipstr")]
  #[cfg_attr(docsrs, doc(cfg(feature = "hipstr")))]
  hipstr::HipByt<'_> => |s| &s[..]
);

viewed_as!(
  #[cfg(feature = "smol-bytes")]
  #[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes")))]
  smol_bytes::shared::Bytes => |s| &s[..]
);

viewed_as!(
  #[cfg(feature = "smol-bytes")]
  #[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes")))]
  smol_bytes::compact::Bytes => |s| &s[..]
);

viewed_as!(
  #[cfg(feature = "smol-bytes")]
  #[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes")))]
  smol_bytes::Utf8Bytes => |s| s.as_str()
);

viewed_as!(
  #[cfg(feature = "smol-bytes")]
  #[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes")))]
  smol_bytes::compact::Utf8Bytes => |s| s.as_str()
);
