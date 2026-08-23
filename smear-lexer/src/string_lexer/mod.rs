use derive_more::{Deref, DerefMut, From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  logos::Lexer,
  utils::{
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
  },
};

use crate::error::{StringError, StringErrors};

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) use block::skip_block_str_from_bytes;
pub use block::{LitBlockStr, LitComplexBlockStr};
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) use inline::skip_inline_str_simd;
pub use inline::{LitComplexInlineStr, LitInlineStr};

/// Generates one literal carrier: a private `source`, the line facts the lexer measured on *that*
/// source, a `pub(crate)` constructor, and read-only doors over the rest.
///
/// # The optional `<Kind>` parameter, and why one of the three carriers has it
///
/// [`LitComplexInlineStr`] and [`LitComplexBlockStr`] say which literal they came out of in their
/// names, so the enum variant that holds one cannot be handed the other kind's carrier: the types
/// differ. The plain carrier had no such distinction — one `LitPlainStr<S>` served both
/// [`LitInlineStr::Plain`] and [`LitBlockStr::Plain`] — so a caller could take the carrier out of a
/// legitimately lexed `"x"` and spell `LitBlockStr::Plain(that_carrier)`. Nothing about the source
/// was forged; what was forged is *which grammar it was lexed under*, because the carrier is
/// kind-agnostic and the variant supplies the kind. Both value doors then read that kind and
/// answered with it: the block conversion returned the inline literal's quoted spelling `"x"`
/// where its value is `x`, and wrapping `"""block"""` as inline returned `""block""`.
///
/// Naming the parameter puts the kind *in the type* — `$name<S, Kind>`, with [`InlineKind`] and
/// [`BlockKind`] as the two markers — and every generated door carries `Kind` through unchanged, so
/// there is no conversion from one to the other to reach for. It is a `PhantomData`, so it costs no
/// bytes; `Debug` is written by hand beside the type rather than derived, for the same reason.
macro_rules! variant_type {
  (
    $(#[$meta:meta])*
    $vis:vis struct $name:ident $(<$kind:ident>)? {
      $(
        $(#[$field_meta:meta])*
        $field:ident: $ty:ty $(,)?
      )*
    }
  ) => {
    $(#[$meta])*
    $vis struct $name<S $(, $kind)?> {
      source: S,
      $($field: $ty,)*
      $(_kind: core::marker::PhantomData<$kind>,)?
    }

    impl<'a $(, $kind)?> TryFrom<$name<&'a [u8] $(, $kind)?>> for $name<&'a str $(, $kind)?> {
      type Error = core::str::Utf8Error;

      #[inline]
      fn try_from(value: $name<&'a [u8] $(, $kind)?>) -> Result<Self, Self::Error> {
        core::str::from_utf8(value.source())
          .map(|s| {
            Self::new(s, $(value.$field),*)
          })
      }
    }

    impl<S $(, $kind)?> $name<Option<S> $(, $kind)?> {
      /// Moves the `Option` out of the source position, leaving every claim the carrier makes —
      /// the lexer's variant, the line facts beside it, and the kind it was lexed under — attached
      /// to the same bytes it was attached to before.
      pub fn transpose(self) -> Option<$name<S $(, $kind)?>> {
        match self.source {
          Some(source) => Some($name::new(source, $(self.$field),*)),
          None => None,
        }
      }
    }

    impl<S $(, $kind)?> $name<S $(, $kind)?> {
      #[inline(always)]
      #[allow(clippy::too_many_arguments)]
      pub(crate) const fn new(source: S, $($field: $ty),*) -> Self {
        Self {
          source,
          $($field,)*
          $(_kind: core::marker::PhantomData::<$kind>,)?
        }
      }

      $(
        $( #[$field_meta] )*
        #[inline(always)]
        pub const fn $field(&self) -> $ty {
          self.$field
        }
      )*

      /// Returns the source of the simple escape string.
      #[inline(always)]
      pub const fn source_ref(&self) -> &S {
        &self.source
      }

      /// Returns the underlying source.
      #[inline(always)]
      pub const fn source(&self) -> S where S: Copy {
        self.source
      }

      /// Converts this to an equivalent type.
      ///
      /// The bound is what makes this the only representation change a literal has.
      /// [`ToEquivalent`](tokora::utils::ToEquivalent) is sealed, so `T` ranges over
      /// byte-equivalent spellings of the same source and over nothing else — and a literal's
      /// carrier is not free-form data. The lexer's variant, the line facts beside it, the kind it
      /// was lexed under and, for a plain one, the claim that its cooked value *is* its source are
      /// all statements about these exact bytes. A conversion that could hand the source to an
      /// arbitrary `FnOnce` would leave every one of them attached to a spelling that never
      /// justified it, which is why there is no `map` here.
      #[inline(always)]
      pub fn to_equivalent<T>(&self) -> $name<T $(, $kind)?>
      where
        S: tokora::utils::ToEquivalent<T>,
      {
        $name::new(self.source.to_equivalent(), $(self.$field),*)
      }

      /// Converts this to an equivalent type.
      #[inline(always)]
      pub fn into_equivalent<T>(self) -> $name<T $(, $kind)?>
      where
        S: tokora::utils::IntoEquivalent<T>,
      {
        $name::new(self.source.into_equivalent(), $(self.$field),*)
      }
    }

    impl<S: tokora::utils::human_display::DisplayHuman $(, $kind)?> core::fmt::Display
      for $name<S $(, $kind)?>
    {
      #[inline]
      fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        tokora::utils::human_display::DisplayHuman::fmt(&self.source, f)
      }
    }

    impl<'a $(, $kind)?> $name<&'a str $(, $kind)?> {
      /// Returns the str representation.
      #[inline(always)]
      pub const fn as_str(&self) -> &'a str {
        self.source
      }
    }

    impl<'a $(, $kind)?> $name<&'a [u8] $(, $kind)?> {
      /// Returns the byte slice representation.
      #[inline(always)]
      pub const fn as_bytes(&self) -> &'a [u8] {
        self.source
      }
    }
  }
}

/// The doors every literal carrier has over the bare `str`/`[u8]`: equality both ways round, and
/// the three one-way ways to reach the source spelling.
///
/// Cross-type **ordering** is deliberately not among them — it is [`impl_source_ordering`], and
/// only two of the four carriers may have it. See that macro for which and why.
macro_rules! impl_common_traits {
  ($name:ident::<&$lt:lifetime $ty:ty>::$fn:ident) => {
    impl PartialEq<$ty> for $name<&'_ $ty> {
      #[inline(always)]
      fn eq(&self, other: &$ty) -> bool {
        self.$fn().eq(other)
      }
    }

    impl PartialEq<$name<&'_ $ty>> for $ty {
      #[inline(always)]
      fn eq(&self, other: &$name<&'_ $ty>) -> bool {
        other.eq(self)
      }
    }

    // DELIBERATELY NO `Borrow<$ty>`. It is not an ergonomic synonym for `AsRef`: it promises
    // that the borrowed value and the owned one are interchangeable as map keys, which requires
    // `Hash`, `Eq` and `Ord` to agree between them. Every carrier here derives all three over
    // more than the bytes `$fn` hands out — the enums mix in a discriminant, and the complex
    // carriers mix in `required_capacity` — so `str`'s hash and this type's could not agree, and
    // a `HashMap<LitInlineStr<&str>, V>` looked up through `&str` never hit. The impls were
    // there and the promise was false.
    //
    // Making it true was the alternative and it costs more than it buys. `Hash` cannot narrow to
    // the source alone while `Eq` and `Ord` still read the whole value, so all three would have
    // to narrow together: two carriers making DIFFERENT claims about the same bytes would become
    // equal, which is the pairing the type was just rebuilt to forbid, and derived `Ord`'s
    // `Plain < Complex` would silently become source order in every `BTreeMap` and `sort` over
    // these. `AsRef` carries no such contract, `Deref` gives `&*lit`, and `From<…> for &$ty` gives
    // the owned reborrow, so nothing an in-tree caller does is lost.
    impl AsRef<$ty> for $name<&'_ $ty> {
      #[inline(always)]
      fn as_ref(&self) -> &$ty {
        self.$fn()
      }
    }

    impl core::ops::Deref for $name<&'_ $ty> {
      type Target = $ty;

      #[inline(always)]
      fn deref(&self) -> &Self::Target {
        self.$fn()
      }
    }

    impl<$lt> From<$name<&$lt $ty>> for &$lt $ty {
      #[inline(always)]
      fn from(s: $name<&$lt $ty>) -> Self {
        s.$fn()
      }
    }
  };
}

/// Ordering against the bare `str`/`[u8]`, both ways round, for the carriers whose **own**
/// ordering is that same source order.
///
/// # Two of the four, and the third value is what decides it
///
/// `PartialOrd`'s requirements hold *across* implementations: with `A: PartialOrd<B>`,
/// `B: PartialOrd<C>` and `A: PartialOrd<C>` all in scope, `a < b` and `b < c` must give `a < c`.
/// A cross-type impl therefore cannot read the source alone while the type's own `Ord` reads
/// something else — the two are then different relations, and no third value has to respect both.
///
/// A **struct** carrier is safe: `variant_type!` declares `source` first, so derived `Ord`
/// compares the source first and breaks ties on the line facts — and the line facts are computed
/// from the source, so no two carriers the lexer mints can reach that tie-break with different
/// answers. Source order and derived order are one relation.
///
/// The **enums** are not, and this is the same disagreement [`LitInlineStr`] and [`LitBlockStr`]
/// refuse `Borrow` over, one trait along. Derived `Ord` on an enum ranks the discriminant ahead of
/// the fields, so `Plain < Complex` whatever the bytes say, while a cross-type impl would answer
/// on bytes. Three ordinarily lexed values witnessed it, and none of them is forged:
///
/// | pair | route | answer |
/// |---|---|---|
/// | plain `"z"` vs complex `"a\n"` | derived, discriminant first | `Less` |
/// | complex `"a\n"` vs the `str` `"\"m\""` | cross-type, source bytes | `Less` |
/// | plain `"z"` vs the `str` `"\"m\""` | cross-type, source bytes | **`Greater`** |
///
/// Narrowing the enums' own `Ord` to source order was the other way to close it, and it is the
/// way `db0ea56` already refused for `Borrow`: `Ord` has to stay consistent with `Eq`, so `Plain`
/// and `Complex` carrying the same bytes — two different claims about them — would compare equal
/// in every `BTreeMap` and `sort` over these. Removing the cross-type impl is the direction that
/// costs a caller only `lit.as_str().cmp(other)`, which is what it meant anyway.
macro_rules! impl_source_ordering {
  ($name:ident::<&$lt:lifetime $ty:ty>::$fn:ident) => {
    impl PartialOrd<$ty> for $name<&'_ $ty> {
      #[inline(always)]
      fn partial_cmp(&self, other: &$ty) -> Option<core::cmp::Ordering> {
        self.$fn().partial_cmp(other)
      }
    }

    impl PartialOrd<$name<&'_ $ty>> for $ty {
      #[inline(always)]
      fn partial_cmp(&self, other: &$name<&'_ $ty>) -> Option<core::cmp::Ordering> {
        other.partial_cmp(self).map(core::cmp::Ordering::reverse)
      }
    }
  };
}

mod block;
mod inline;

#[cfg(test)]
mod tests;

/// The kind marker of a plain carrier the lexer read as an **inline** `"…"` literal.
///
/// A type-level tag and nothing else: see [`LitPlainStr`] for what it is tagging and why.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct InlineKind;

/// The kind marker of a plain carrier the lexer read as a **block** `"""…"""` literal.
///
/// A type-level tag and nothing else: see [`LitPlainStr`] for what it is tagging and why.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BlockKind;

/// The carrier of [`LitInlineStr::Plain`] — a `"…"` literal the lexer found no escape in.
pub type LitPlainInlineStr<S> = LitPlainStr<S, InlineKind>;

/// The carrier of [`LitBlockStr::Plain`] — a `"""…"""` literal §2.9.4's algorithm leaves alone.
pub type LitPlainBlockStr<S> = LitPlainStr<S, BlockKind>;

variant_type!(
  /// A plain string without any escapes, tagged with the literal it was lexed out of.
  ///
  /// The two spellings have names: [`LitPlainInlineStr<S>`] and [`LitPlainBlockStr<S>`].
  ///
  /// # The tag is the point
  ///
  /// A plain carrier holds a source and one claim about it — *the value of this literal is this
  /// source, delimiters off* — and that claim is only readable against a grammar. Which delimiters
  /// come off, and what "no normalization to do" even means, are different questions for `"…"` and
  /// for `"""…"""`. While the carrier was kind-agnostic the enum variant supplied the kind, so a
  /// caller could take the carrier out of a real inline literal and re-label it:
  ///
  /// ```compile_fail,E0308
  /// use smear_lexer::{LitBlockStr, LitInlineStr, LitStr};
  ///
  /// let LitStr::Inline(inline) = LitStr::try_from("\"x\"").unwrap() else { unreachable!() };
  /// let LitInlineStr::Plain(carrier) = inline else { unreachable!() };
  /// // The carrier is an inline literal's. Calling it a block literal's used to typecheck, and
  /// // `Cow::from` then answered the quoted spelling `"x"` where the block value is `x`.
  /// let forged = LitBlockStr::Plain(carrier);
  /// ```
  ///
  /// and the other way round just as easily:
  ///
  /// ```compile_fail,E0308
  /// use smear_lexer::{LitBlockStr, LitInlineStr, LitStr};
  ///
  /// let LitStr::Block(block) = LitStr::try_from("\"\"\"block\"\"\"").unwrap() else { unreachable!() };
  /// let LitBlockStr::Plain(carrier) = block else { unreachable!() };
  /// // Wrapping a block literal as inline used to answer `""block""` — the outer `"""` losing one
  /// // quote each side to the inline delimiter strip.
  /// let forged = LitInlineStr::Plain(carrier);
  /// ```
  ///
  /// # Nothing outside this crate mints one
  ///
  /// Re-labelling is the door a caller can reach *given* a carrier. The other half of the
  /// invariant is that a carrier can only come from a lex in the first place — `new` is
  /// `pub(crate)`:
  ///
  /// ```compile_fail,E0624
  /// use smear_lexer::LitPlainInlineStr;
  ///
  /// // `new` is `pub(crate)` to `smear-lexer`: the lexer is the only constructor.
  /// let forged = LitPlainInlineStr::new("\"x\"");
  /// ```
  ///
  /// and `source` is private, which is what closes the struct literal and any later write to the
  /// field a caller holds:
  ///
  /// ```compile_fail,E0616
  /// use smear_lexer::{LitInlineStr, LitStr};
  ///
  /// let LitStr::Inline(inline) = LitStr::try_from("\"x\"").unwrap() else { unreachable!() };
  /// let LitInlineStr::Plain(carrier) = inline else { unreachable!() };
  /// let raw = carrier.source;
  /// ```
  ///
  /// (`LitPlainBlockStr { source: … }` is refused too, by that same privacy; rustc gives that
  /// diagnostic no error code, so this snippet pins the fact rather than the phrasing.)
  ///
  /// Together those are the whole of the invariant: a plain literal's kind and its source are
  /// established in the same place, by the lexer, and no caller can pair one with the other.
  ///
  /// Per this repository's convention the error codes above are checked only under a nightly
  /// `cargo test --doc`; on stable the assertion is that the snippets do not compile at all.
  #[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
  #[repr(transparent)]
  pub struct LitPlainStr<Kind> {}
);

/// Byte-for-byte what `#[derive(Debug)]` printed before the kind parameter existed.
///
/// Written by hand only because the derive would render the `PhantomData` beside `source`, and a
/// type-level tag is not data — the enclosing `LitInlineStr(Plain(…))` / `LitBlockStr(Plain(…))`
/// already names the kind. Frozen lexer fixtures read this output, so it is deliberately stable.
impl<S, Kind> core::fmt::Debug for LitPlainStr<S, Kind>
where
  S: core::fmt::Debug,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("LitPlainStr")
      .field("source", self.source_ref())
      .finish()
  }
}

impl<S, Kind> DisplayCompact for LitPlainStr<S, Kind>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S, Kind> DisplayPretty for LitPlainStr<S, Kind>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

/// A GraphQL string literal, either inline or block.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitStr<S> {
  /// An inline string literal.
  Inline(LitInlineStr<S>),
  /// A block string literal.
  Block(LitBlockStr<S>),
}

impl<S: DisplayHuman> core::fmt::Display for LitStr<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Inline(s) => write!(f, "{s}"),
      Self::Block(s) => write!(f, "{s}"),
    }
  }
}

impl<S: DisplayHuman> DisplayHuman for LitStr<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

impl<S> LitStr<S> {
  /// Returns the underlying source
  #[inline(always)]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Inline(s) => s.source(),
      Self::Block(s) => s.source(),
    }
  }

  /// Returns the reference to the underlying source
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Inline(s) => s.source_ref(),
      Self::Block(s) => s.source_ref(),
    }
  }

  /// Converts this to an equivalent type.
  #[inline(always)]
  pub fn to_equivalent<T>(&self) -> LitStr<T>
  where
    S: tokora::utils::ToEquivalent<T>,
  {
    match self {
      Self::Inline(s) => LitStr::Inline(s.to_equivalent()),
      Self::Block(s) => LitStr::Block(s.to_equivalent()),
    }
  }

  /// Converts this to an equivalent type.
  #[inline(always)]
  pub fn into_equivalent<T>(self) -> LitStr<T>
  where
    S: tokora::utils::IntoEquivalent<T>,
  {
    match self {
      Self::Inline(s) => LitStr::Inline(s.into_equivalent()),
      Self::Block(s) => LitStr::Block(s.into_equivalent()),
    }
  }
}

#[derive(Deref, DerefMut)]
#[repr(transparent)]
pub(super) struct SealedWrapper<L: ?Sized>(L);

impl<T> SealedWrapper<T> {
  #[inline(always)]
  pub const fn from_mut(t: &mut T) -> &mut Self {
    // Safety: This is safe because SealedWrapper is repr(transparent) over T
    unsafe { &mut *(t as *mut T as *mut Self) }
  }
}

impl<'de: 'a, 'a> TryFrom<&'de str> for LitStr<&'a str> {
  type Error = StringErrors<char>;

  #[inline]
  fn try_from(value: &'de str) -> Result<Self, Self::Error> {
    if value.starts_with("\"\"\"") {
      let mut lexer = Lexer::<block::BlockStringToken>::new(value);
      lexer.bump(3);
      block::lex_block_str_from_str(SealedWrapper::from_mut(&mut lexer)).map(Self::Block)
    } else if value.starts_with('"') {
      let mut lexer = Lexer::<inline::StringToken>::new(value);
      lexer.bump(1);
      inline::lex_inline_str_from_str(SealedWrapper::from_mut(&mut lexer)).map(Self::Inline)
    } else {
      Err(StringError::unopened_string(value.chars().next(), 0).into())
    }
  }
}

impl<'de: 'a, 'a> TryFrom<&'de [u8]> for LitStr<&'a [u8]> {
  type Error = StringErrors<u8>;

  #[inline]
  fn try_from(value: &'de [u8]) -> Result<Self, Self::Error> {
    if value.starts_with(b"\"\"\"") {
      let mut lexer = Lexer::<block::BytesBlockStringToken>::new(value);
      lexer.bump(3);
      block::lex_block_str_from_bytes(SealedWrapper::from_mut(&mut lexer)).map(Self::Block)
    } else if value.starts_with(b"\"") {
      let mut lexer = Lexer::<inline::BytesStringToken>::new(value);
      lexer.bump(1);
      inline::lex_inline_str_from_bytes(SealedWrapper::from_mut(&mut lexer)).map(Self::Inline)
    } else {
      Err(StringError::unopened_string(value.first().copied(), 0).into())
    }
  }
}

/// Panic message for the (statically unreachable) case where a string the SIMD
/// fast path routed to error delegation turns out to lex cleanly.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
const VALID_STRING_UNREACHABLE: &str =
  "SIMD string fast path emits every valid string literal; only errors reach delegation";

/// Delegate a malformed string literal — whose opener the SIMD fast path could
/// not resolve — straight to the `string_lexer` sub-lexer, i.e. the very same
/// `lex_*` code the full grammar reaches through its `#[token("\"")]` /
/// `#[token("\"\"\"")]` arms, but *without* paying to build the whole grammar.
///
/// Implemented for the two Logos scan primitives (`str`, `[u8]`). Both dialects'
/// SIMD lexers call this on the string-error arms of their dispatch loop; it is
/// the string twin of `simd::delegate_to_logos`.
///
/// The seek combines the position-0 `TryFrom` idiom above (construct a carrier,
/// `bump` past the opener) with `delegate_to_logos`'s mid-stream positioning
/// (start from the whole source and advance to `token_start`): the carrier is
/// built over the entire source, then bumped to `token_start + opener_len` so
/// the sub-lexer's base offset lands exactly where the full grammar's did. The
/// returned end offset is `carrier.span().end` after lexing — equal to the end
/// of the error-token span the full grammar reports — so the caller can build a
/// byte-for-byte-identical error span of `token_start..end`.
///
/// Only ever invoked on the error path: the SIMD fast path emits every *valid*
/// string itself, so the delegated `lex_*` here always returns `Err` (hence the
/// [`VALID_STRING_UNREACHABLE`] `expect_err`).
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) trait DelegateStringError {
  /// The `StringErrors` character type this primitive lexes into: `char` for
  /// `str`, `u8` for `[u8]` — matching the SIMD lexer's own `Slice::Char`.
  type Char;

  /// Lex the malformed string literal opening at `token_start`, returning its
  /// collected errors and the absolute byte offset where lexing stopped.
  ///
  /// `block` selects the block (`"""`, 3-byte opener) carrier over the inline
  /// (`"`, 1-byte opener) one. See the trait docs for the seek/span contract.
  fn delegate_string_error(
    &self,
    token_start: usize,
    block: bool,
  ) -> (StringErrors<Self::Char>, usize);
}

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
impl DelegateStringError for str {
  type Char = char;

  #[inline]
  fn delegate_string_error(&self, token_start: usize, block: bool) -> (StringErrors<char>, usize) {
    if block {
      let mut lexer = Lexer::<block::BlockStringToken>::new(self);
      lexer.bump(token_start + 3);
      let errs = block::lex_block_str_from_str(SealedWrapper::from_mut(&mut lexer))
        .expect_err(VALID_STRING_UNREACHABLE);
      (errs, lexer.span().end)
    } else {
      let mut lexer = Lexer::<inline::StringToken>::new(self);
      lexer.bump(token_start + 1);
      let errs = inline::lex_inline_str_from_str(SealedWrapper::from_mut(&mut lexer))
        .expect_err(VALID_STRING_UNREACHABLE);
      (errs, lexer.span().end)
    }
  }
}

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
impl DelegateStringError for [u8] {
  type Char = u8;

  #[inline]
  fn delegate_string_error(&self, token_start: usize, block: bool) -> (StringErrors<u8>, usize) {
    if block {
      let mut lexer = Lexer::<block::BytesBlockStringToken>::new(self);
      lexer.bump(token_start + 3);
      let errs = block::lex_block_str_from_bytes(SealedWrapper::from_mut(&mut lexer))
        .expect_err(VALID_STRING_UNREACHABLE);
      (errs, lexer.span().end)
    } else {
      let mut lexer = Lexer::<inline::BytesStringToken>::new(self);
      lexer.bump(token_start + 1);
      let errs = inline::lex_inline_str_from_bytes(SealedWrapper::from_mut(&mut lexer))
        .expect_err(VALID_STRING_UNREACHABLE);
      (errs, lexer.span().end)
    }
  }
}

#[cfg(all(feature = "bytes", any(feature = "graphql", feature = "graphqlx")))]
const _: () = {
  use bytes::Bytes;

  impl DelegateStringError for Bytes {
    type Char = u8;

    #[inline]
    fn delegate_string_error(&self, token_start: usize, block: bool) -> (StringErrors<u8>, usize) {
      self.as_ref().delegate_string_error(token_start, block)
    }
  }
};

#[cfg(all(feature = "hipstr", any(feature = "graphql", feature = "graphqlx")))]
const _: () = {
  use hipstr::{HipByt, HipStr};

  impl DelegateStringError for HipByt<'_> {
    type Char = u8;

    #[inline]
    fn delegate_string_error(&self, token_start: usize, block: bool) -> (StringErrors<u8>, usize) {
      self.as_ref().delegate_string_error(token_start, block)
    }
  }

  impl DelegateStringError for HipStr<'_> {
    type Char = char;

    #[inline]
    fn delegate_string_error(
      &self,
      token_start: usize,
      block: bool,
    ) -> (StringErrors<char>, usize) {
      let s: &str = self.as_ref();
      s.delegate_string_error(token_start, block)
    }
  }
};

#[cfg(all(feature = "bstr", any(feature = "graphql", feature = "graphqlx")))]
const _: () = {
  use bstr::BStr;

  impl DelegateStringError for BStr {
    type Char = u8;

    #[inline]
    fn delegate_string_error(&self, token_start: usize, block: bool) -> (StringErrors<u8>, usize) {
      let s: &[u8] = self.as_ref();
      s.delegate_string_error(token_start, block)
    }
  }
};
