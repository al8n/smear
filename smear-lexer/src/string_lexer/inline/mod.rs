use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::utils::{DisplayCompact, DisplayHuman, DisplayPretty};

use super::{
  LitPlainInlineStr,
  escape::ends_escape,
  value::{Invalid, inline_value},
};
use std::{borrow::Cow, string::String};

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) use self::simd::skip_inline_str_simd;
pub(crate) use self::{
  str::{StringToken, lex_inline_str_from_str},
  u8_slice::{StringToken as BytesStringToken, lex_inline_str_from_bytes},
};

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod simd;
mod str;
mod u8_slice;

variant_type!(
  /// A complex inline string representation in GraphQL containing one or more escapes.
  /// This includes simple escapes, unicode escapes, or both.
  #[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
  pub struct LitComplexInlineStr {
    /// The capacity required to store the normalized string.
    required_capacity: usize,
  }
);

impl<S> DisplayCompact for LitComplexInlineStr<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S> DisplayPretty for LitComplexInlineStr<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

/// An inline string representation in GraphQL.
///
/// # Not a map key reachable through `&str`
///
/// This type, [`LitBlockStr`](super::LitBlockStr) and both complex carriers deliberately do **not**
/// implement [`Borrow`](core::borrow::Borrow). That trait is not a synonym for [`AsRef`]: it
/// promises the borrowed and the owned value are interchangeable as map keys, and every carrier
/// here derives `Hash` over more than the bytes `as_str` hands out. The impls existed and the
/// promise was false — a `&str` lookup hashed the bytes while the key hashed the discriminant, so
/// it could never hit. The absence is what the snippet pins — with `Borrow<str>` gone the only
/// candidate left is the blanket `Borrow<T> for T`, so the refusal arrives as a type mismatch on
/// the argument rather than as an unsatisfied bound:
///
/// ```compile_fail,E0308
/// use std::collections::HashMap;
/// use smear_lexer::LitInlineStr;
///
/// let map: HashMap<LitInlineStr<&str>, ()> = HashMap::new();
/// let _ = map.get("query");
/// ```
///
/// [`AsRef`], [`Deref`](core::ops::Deref) and `From<LitInlineStr<&str>> for &str` all still hand
/// back the source spelling, so a caller who wants that key writes it: `map.get(lit.as_str())` on
/// a `HashMap<&str, _>`.
///
/// # Not ordered against `str` or `[u8]` either
///
/// The same disagreement, one trait along, and this one needs no map to show itself.
/// [`PartialOrd`]'s requirements hold **across** implementations — `a < b` and `b < c` drawn from
/// two different impls still have to give `a < c` — so an impl reading the source alone cannot sit
/// beside a derived `Ord` that reads something else first. This one does: derived `Ord` on an enum
/// ranks the discriminant ahead of the fields, so `Plain < Complex` before a byte is compared.
/// Three ordinarily lexed values witnessed the break, none of them forged:
///
/// | pair | route | answer |
/// |---|---|---|
/// | plain `"z"` vs complex `"a\n"` | derived, discriminant first | `Less` |
/// | complex `"a\n"` vs the `str` `"\"m\""` | cross-type, source bytes | `Less` |
/// | plain `"z"` vs the `str` `"\"m\""` | cross-type, source bytes | **`Greater`** |
///
/// The third value is any spelling that sorts between the two sources, and `"\"m\""` is one — the
/// probe carries the literal's `"` delimiters because the source does. Narrowing the enum's own
/// `Ord` to source order was the other way to close it, and it is the way this type has already
/// refused once: `Ord` must stay consistent with `Eq`, so `Plain` and `Complex` carrying the same
/// bytes — two different claims about them — would compare equal in every `BTreeMap` and `sort`.
///
/// So this type and [`LitBlockStr`](super::LitBlockStr) have no `PartialOrd<str>` and no
/// `PartialOrd<[u8]>`. The refusal arrives the same way the `Borrow` one does, as a type mismatch
/// against the derived `PartialOrd<Self>`:
///
/// ```compile_fail,E0308
/// use smear_lexer::LitStr;
///
/// let LitStr::Inline(lit) = LitStr::try_from("\"z\"").unwrap() else { unreachable!() };
/// let _ = lit < *"\"m\"";
/// ```
///
/// ```compile_fail,E0308
/// use smear_lexer::LitStr;
///
/// let LitStr::Inline(lit) = LitStr::try_from(b"\"z\"".as_slice()).unwrap() else {
///   unreachable!()
/// };
/// let _ = lit < *b"\"m\"".as_slice();
/// ```
///
/// The complex carriers keep theirs: `variant_type!` declares `source` first, so their derived
/// `Ord` compares the source first and breaks ties on line facts computed from that same source —
/// one relation, with no gap for a third value to fall into. A caller who wants source order over
/// a whole literal writes it, and says so: `lit.as_str().cmp(other)`.
///
/// Per this repository's convention the error codes are checked only under a nightly
/// `cargo test --doc`; on stable the assertion is that the snippets do not compile at all.
#[derive(
  Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, From, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitInlineStr<S> {
  /// A clean string without any escaped characters or escaped unicode.
  Plain(LitPlainInlineStr<S>),
  /// A complex string containing escaped characters.
  ///
  /// This includes escapes like:
  /// 1. `\"`, `\\`, `\n`, etc,
  /// 2. fixed-width unicode escapes like `\u1234`
  /// 3. variable-width unicode escapes like `\u{1F600}`
  Complex(LitComplexInlineStr<S>),
}

impl<S: DisplayHuman> core::fmt::Display for LitInlineStr<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Plain(s) => DisplayHuman::fmt(s.source_ref(), f),
      Self::Complex(s) => DisplayHuman::fmt(s.source_ref(), f),
    }
  }
}

impl<'a> LitInlineStr<&'a str> {
  /// Returns the str representation of the inline string.
  #[inline(always)]
  pub const fn as_str(&self) -> &'a str {
    match self {
      Self::Plain(s) => s.as_str(),
      Self::Complex(s) => s.as_str(),
    }
  }
}

impl<'a> LitInlineStr<&'a [u8]> {
  /// Returns the byte slice representation of the inline string.
  #[inline(always)]
  pub const fn as_bytes(&self) -> &'a [u8] {
    match self {
      Self::Plain(s) => s.as_bytes(),
      Self::Complex(s) => s.as_bytes(),
    }
  }
}

impl<'a> TryFrom<LitInlineStr<&'a [u8]>> for LitInlineStr<&'a str> {
  type Error = core::str::Utf8Error;

  #[inline]
  fn try_from(value: LitInlineStr<&'a [u8]>) -> Result<Self, Self::Error> {
    match value {
      LitInlineStr::Plain(s) => s.try_into().map(Self::Plain),
      LitInlineStr::Complex(s) => s.try_into().map(Self::Complex),
    }
  }
}

/// Answers the literal's **value** — draft §2.9.1's `StringValue`, delimiters gone and every
/// escape applied — borrowing it whenever the spelling already *is* the value.
///
/// # Value, not spelling
///
/// This type has a second `&str` door that answers the other question, and the two are not
/// interchangeable: [`as_str`](LitInlineStr::as_str), [`Deref`](core::ops::Deref), [`AsRef`]
/// and `From<LitInlineStr<&str>> for &str` all hand back the **source spelling**, `"` delimiters and backslashes included, because keeping the
/// source is what makes lexing allocate nothing. This conversion is the cooked reading, and
/// [`Cow`] is its return type precisely so that the [`Plain`](LitInlineStr::Plain) half — a
/// literal with no escape in it — still costs nothing but a reslice.
///
/// The two variants therefore answer the same question, which is what makes this door meaningful:
/// they used to disagree. `Plain` returned the source *with* its quotes, so `Cow::from` of the
/// literal `""` was the two-character string `""`, while `Complex` returned a cooked value with
/// the delimiters already off.
///
/// # The reslice is a claim about *these* bytes read as *this* grammar
///
/// Answering `Plain` by borrowing asserts that the literal's cooked value is its source with the
/// `"` delimiters taken off. That is two things at once — a source, and the grammar it was read
/// under — and the assertion holds only while a caller can change neither.
///
/// **The source.** This type has no source-replacing conversion. It had one, a `map` taking
/// `FnOnce(S) -> O`, and the discriminant rode across it: a `Plain` `"x"` remapped to the spelling
/// `"\n"` stayed `Plain`, so this conversion handed back the two characters `\` and `n` where the
/// value is one line feed. The only representation change left is
/// [`into_equivalent`](LitInlineStr::into_equivalent), whose sealed bound keeps the bytes.
///
/// **The grammar.** Removing `map` did not close that half, because the carrier is not what names
/// the grammar — the variant is, and one kind-agnostic carrier fitted both. A block literal's
/// carrier wrapped as inline is an honest source under the wrong reading, and `inline_body` then
/// takes one quote off each end of a `"""` delimiter. [`LitPlainStr`](super::LitPlainStr) carries
/// its kind in its type now, so the pairing has no spelling:
///
/// ```compile_fail,E0308
/// use smear_lexer::{LitBlockStr, LitInlineStr, LitStr};
///
/// let LitStr::Block(block) = LitStr::try_from("\"\"\"block\"\"\"").unwrap() else {
///   unreachable!()
/// };
/// let LitBlockStr::Plain(carrier) = block else { unreachable!() };
/// // This used to typecheck, and `Cow::from` of it answered `""block""`.
/// let forged = LitInlineStr::Plain(carrier);
/// ```
///
/// Per this repository's convention the error code above is checked only under a nightly
/// `cargo test --doc`; on stable the assertion is that the snippet does not compile at all.
impl<'a> From<LitInlineStr<&'a str>> for Cow<'a, str> {
  #[inline]
  fn from(value: LitInlineStr<&'a str>) -> Self {
    match value {
      LitInlineStr::Plain(s) => Cow::Borrowed(inline_body(s.as_str())),
      LitInlineStr::Complex(s) => {
        let mut cooked = String::with_capacity(s.required_capacity());
        // The decoder's own inline walk. It copies a backslash that begins no escape through as
        // content rather than refusing, and a `String` takes every piece, so it answers `Ok`
        // whatever the carrier holds and there is no refusal to read.
        let _ = inline_value(
          inline_body(s.as_str()),
          Invalid::CopyThrough,
          &mut |piece: &str| -> core::fmt::Result {
            cooked.push_str(piece);
            Ok(())
          },
        );
        Cow::Owned(cooked)
      }
    }
  }
}

/// Strips an inline literal's `"` delimiters.
///
/// Total on a slice that does not carry them. Both carriers are `pub(crate)`-constructed and every
/// construction in this crate spans a whole `"…"` token, so the delimiters are the lexer's
/// guarantee — and a conversion on an already-lexed literal is not the place to re-check it with a
/// panicking index.
#[inline]
fn inline_body(raw: &str) -> &str {
  raw
    .strip_prefix('"')
    .and_then(|rest| rest.strip_suffix('"'))
    .unwrap_or(raw)
}

impl<S> LitInlineStr<Option<S>> {
  /// a
  #[inline(always)]
  pub fn transpose(self) -> Option<LitInlineStr<S>> {
    match self {
      LitInlineStr::Plain(s) => s.transpose().map(LitInlineStr::Plain),
      LitInlineStr::Complex(s) => s.transpose().map(LitInlineStr::Complex),
    }
  }
}

impl<S> LitInlineStr<S> {
  /// Returns the underlying source.
  #[inline(always)]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Plain(s) => s.source(),
      Self::Complex(s) => s.source(),
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Plain(s) => s.source_ref(),
      Self::Complex(s) => s.source_ref(),
    }
  }

  /// Converts this to an equivalent type.
  #[inline(always)]
  pub fn to_equivalent<T>(&self) -> LitInlineStr<T>
  where
    S: tokora::utils::ToEquivalent<T>,
  {
    match self {
      Self::Plain(s) => LitInlineStr::Plain(s.to_equivalent()),
      Self::Complex(s) => LitInlineStr::Complex(s.to_equivalent()),
    }
  }

  /// Converts this to an equivalent type.
  #[inline(always)]
  pub fn into_equivalent<T>(self) -> LitInlineStr<T>
  where
    S: tokora::utils::IntoEquivalent<T>,
  {
    match self {
      Self::Plain(s) => LitInlineStr::Plain(s.into_equivalent()),
      Self::Complex(s) => LitInlineStr::Complex(s.into_equivalent()),
    }
  }
}

impl_common_traits!(LitInlineStr::<&'a str>::as_str);
impl_common_traits!(LitInlineStr::<&'a [u8]>::as_bytes);
impl_common_traits!(LitComplexInlineStr::<&'a str>::as_str);
impl_common_traits!(LitComplexInlineStr::<&'a [u8]>::as_bytes);

// The complex carrier only. `LitInlineStr` is an enum, and its derived `Ord` ranks `Plain` ahead
// of `Complex` before it reads a byte, so a source-ordered impl beside it would not be part of the
// same order — see `impl_source_ordering`.
impl_source_ordering!(LitComplexInlineStr::<&'a str>::as_str);
impl_source_ordering!(LitComplexInlineStr::<&'a [u8]>::as_bytes);

#[inline(always)]
const fn utf8_len_for_scalar(cp: u32) -> usize {
  match cp {
    0x0000..=0x007F => 1,
    0x0080..=0x07FF => 2,
    0x0800..=0xFFFF => 3, // (surrogates are rejected elsewhere)
    _ => 4,               // 0x10000..=0x10_FFFF
  }
}

/// Where the `}` closing a braced Unicode escape is, in `rest`, the input after what a
/// partial-brace rule matched: `\u{` with one to six hex digits, with one other character, or
/// alone.
///
/// The escape ends at the first `}`, `"`, `\`, CR or LF in `rest`. A `}` closes it, and its index
/// is the answer. Each of the others belongs to a rule of its own, so the escape is unclosed and
/// the answer is `None`, as it is at the end of the input. The search therefore never runs past the
/// literal's closing quote, a line break or the next escape to a `}` beyond them.
///
/// All five stops are ASCII, so the one byte scan serves a text source and a byte source alike, and
/// in a text source the byte after the `}` is a character boundary. The four besides the `}` are
/// `ends_escape`'s, where a bare `\u` stops too.
#[inline(always)]
fn braced_escape_close(rest: &[u8]) -> Option<usize> {
  rest
    .iter()
    .position(|&byte| byte == b'}' || ends_escape(byte))
    .filter(|&stop| rest[stop] == b'}')
}
