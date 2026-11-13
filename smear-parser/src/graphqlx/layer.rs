//! GraphQLx's two parser layers, and the trait both implement.

use std::vec::Vec;

use smear_lexer::{SyntaxErrorKind, graphqlx::ContextualKeyword, limits::SyntacticLimits};
use tokora::{
  Lexer, Parse as _, Parser, SimpleSpan, Source, diagnostic::Diagnose, utils::DowncastRef,
};

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
use super::lossless;
use super::{
  GraphQLx,
  ast::{TypeSystemDocument, TypeSystemItem},
  error::{self, ErrorData, GraphqlxErrors},
  syntactic::{GraphqlxLexer, GraphqlxSlice, GraphqlxToken, type_system_document},
};
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
use crate::lossless::Refused;
use crate::{
  layer::{Parsed, Recovery, Rejected, Report, sealed},
  value::Leaf,
};

/// The syntactic layer of GraphQLx.
///
/// It stops at a document's first syntax error and keeps nothing beside the document. It reads
/// the sources the GraphQLx syntactic lexer reads, and answers the AST over that source's own
/// slice: `str` and `[u8]`, and behind their features `BStr`, `Bytes`, `HipStr`, `HipByt` and the
/// four smol-bytes forms. It has one [`Layer`] impl for each of them and no other, so a reference
/// to one, such as `&str` as the source itself, is not a source it reads.
pub type Syntactic = crate::layer::Syntactic<GraphQLx>;

/// The lossless layer of GraphQLx.
///
/// It recovers, lends every syntax error it kept to the caller's sink, and keeps the tree. It
/// reads `str`: a tree holds text only, so a caller that holds bytes decodes them first, and
/// reading byte sources is al8n/smear#121.
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
#[cfg_attr(docsrs, doc(cfg(all(feature = "rowan_0_17", feature = "std"))))]
pub type Lossless = crate::layer::Lossless<GraphQLx>;

/// One of smear's two parser layers, reading GraphQLx from a `Src`.
///
/// Which sources a layer reads is which impls exist. The syntactic layer has one for each source
/// its lexer reads: `str`, `[u8]`, `BStr`, `Bytes`, `HipStr`, `HipByt` and the four smol-bytes
/// forms, each of the last eight behind its feature. The lossless layer has one, for `str`. A
/// reference form, `&str` or `&[u8]` as the source itself, is not among them.
///
/// The trait is sealed for each layer and source: no other crate can implement it, for a layer of
/// its own or over a source of its own. Each layer has the trait's methods as inherent methods
/// too, so a call on a concrete layer imports nothing.
///
/// A consumer of text bounds `P: Layer<'a>`, and its document is `TypeSystemDocument<&'a str>`
/// whichever layer `P` is. A consumer generic over the source states `Src: Source<usize>` to call
/// a method.
pub trait Layer<'src, Src: ?Sized = str>: sealed::Sealed<'src, Src> {
  /// One syntax error, as this layer records it.
  type Error: Diagnose;
  /// Why this layer answers no document.
  type Failure;
  /// What this layer keeps for the caller beside the document.
  type Tree;

  /// Parses `source` as a type-system document.
  ///
  /// Lends `report` every syntax error it kept. Answers the items that have an AST image, how
  /// much of the text they cover, how many errors there were, and what the layer kept; or why it
  /// answers no document.
  fn type_system_document<K>(
    &self,
    source: &'src Src,
    report: &mut K,
  ) -> Result<
    Parsed<TypeSystemDocument<<Src as Source<usize>>::Slice<'src>>, Self::Tree>,
    Self::Failure,
  >
  where
    Src: Source<usize>,
    K: Report<Self::Error> + ?Sized;
}

/// Implements [`Layer`] for [`Syntactic`] over each source of a list, and seals each impl.
///
/// An entry is the attributes that gate its source, the impl's generics, and the source. Both
/// items of an entry carry its attributes and its generics, so a seal holds exactly where its
/// impl does.
///
/// The entry names the text's lifetime, and this macro only repeats the name it is given: the
/// source's type is the entry's, so a lifetime written here would not be the one that type
/// names.
macro_rules! syntactic_layer {
  ($(
    $(#[$gate:meta])*
    <$src:lifetime $(, $held:lifetime : $outlived:lifetime)*> $source:ty;
  )*) => {$(
    $(#[$gate])*
    #[allow(
      single_use_lifetimes,
      clippy::needless_lifetimes,
      reason = "the seal has its `Layer` impl's generics, so the two cannot differ: a lifetime \
                that impl names many times is named once here"
    )]
    impl<$src $(, $held: $outlived)*> sealed::Sealed<$src, $source> for Syntactic {}

    $(#[$gate])*
    impl<$src $(, $held: $outlived)*> Layer<$src, $source> for Syntactic {
      type Error = error::GraphqlxError<GraphqlxSlice<$src, $source>>;
      type Failure = Rejected;
      type Tree = ();

      #[inline]
      fn type_system_document<K>(
        &self,
        source: &$src $source,
        report: &mut K,
      ) -> Result<Parsed<TypeSystemDocument<GraphqlxSlice<$src, $source>>, ()>, Rejected>
      where
        K: Report<Self::Error> + ?Sized,
      {
        read_type_system_document(self.limits(), source, report)
      }
    }
  )*};
}

// The sources the GraphQLx syntactic lexer reads, which are the ones `smear-lexer` implements its
// crate-private `DelegateStringError` for, each under the feature that brings its type. A source
// listed here that the lexer does not read fails to compile at its entry. A reference form is not
// in the list: the lexer reads `str`, not `&str`.
//
// `HipStr` and `HipByt` borrow, and each is its own slice, so the error an impl names holds the
// source's lifetime: `'h: 'src` is what makes that type well-formed for `'src`.
syntactic_layer! {
  <'src> str;
  <'src> [u8];
  #[cfg(feature = "bstr_1")]
  #[cfg_attr(docsrs, doc(cfg(feature = "bstr_1")))]
  <'src> bstr_1::BStr;
  #[cfg(feature = "bytes_1")]
  #[cfg_attr(docsrs, doc(cfg(feature = "bytes_1")))]
  <'src> bytes_1::Bytes;
  #[cfg(feature = "hipstr_0_8")]
  #[cfg_attr(docsrs, doc(cfg(feature = "hipstr_0_8")))]
  <'src, 'h: 'src> hipstr_0_8::HipStr<'h>;
  #[cfg(feature = "hipstr_0_8")]
  #[cfg_attr(docsrs, doc(cfg(feature = "hipstr_0_8")))]
  <'src, 'h: 'src> hipstr_0_8::HipByt<'h>;
  #[cfg(feature = "smol-bytes_0_2")]
  #[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes_0_2")))]
  <'src> smol_bytes_0_2::shared::Bytes;
  #[cfg(feature = "smol-bytes_0_2")]
  #[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes_0_2")))]
  <'src> smol_bytes_0_2::compact::Bytes;
  #[cfg(feature = "smol-bytes_0_2")]
  #[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes_0_2")))]
  <'src> smol_bytes_0_2::Utf8Bytes;
  #[cfg(feature = "smol-bytes_0_2")]
  #[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes_0_2")))]
  <'src> smol_bytes_0_2::compact::Utf8Bytes;
}

/// The lossless layer is sealed over `str` alone, the one source it reads.
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
impl sealed::Sealed<'_, str> for Lossless {}

/// The lossless layer reads `str` alone, because a projection answers an AST that borrows the
/// text; reading byte sources is al8n/smear#121.
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
impl<'src> Layer<'src, str> for Lossless {
  type Error = lossless::GraphqlxLosslessErrorValue<&'src str>;
  type Failure = Refused;
  type Tree = lossless::Parse;

  // The three laws of a sink, from this body and the door's.
  //
  // 1. The count is of the door's calls, one for each error it lends, and of the error records
  //    the parse refused past its ceiling on kept diagnostics, which the tree answers as
  //    `refused_errors()`. `report` is handed each lent error until it breaks, so a sink that
  //    never breaks is called exactly `error_count()` times, less the refused ones.
  // 2. The count does not read what `report` answers, so a sink that breaks changes neither it
  //    nor the verdict `Parsed` draws from it. A break ends the hand-over and nothing else: the
  //    parse had ended before the first error was lent.
  // 3. The door lends at least one error for each error diagnostic of the tree it answers, and
  //    none beside them: a record it keeps lends each member that is not a budget member, or one
  //    error made for it when it holds none; a record it drops lends nothing; and its own budget
  //    refusal is one lent error beside the one diagnostic it appends. The tree's `has_errors()`
  //    reads the refused errors beside the kept diagnostics, so the count is zero exactly when it
  //    is false. The count is still never less than the number of the tree's error diagnostics.
  fn type_system_document<K>(
    &self,
    source: &'src str,
    report: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<&'src str>, lossless::Parse>, Refused>
  where
    K: Report<Self::Error> + ?Sized,
  {
    // Asked before any scan: a text longer than a tree can address has no parse, where the `&str`
    // door would panic on it. Nothing is lent for it.
    if let Some(refused) = Refused::for_source_len(source.len()) {
      return Err(refused);
    }
    let mut errors = 0usize;
    let mut taking = true;
    let parse = lossless::runner::type_system_document_root_lending(
      source,
      self.limits(),
      |error: &lossless::GraphqlxLosslessErrorValue<&'src str>| {
        errors = errors.saturating_add(1);
        if taking && report.error(error).is_break() {
          taking = false;
        }
      },
    );
    let errors = errors.saturating_add(parse.refused_errors());
    let (document, recovery) = lossless::project::project_own_type_system_document(&parse, source);
    Ok(Parsed::new(document, recovery, errors, parse))
  }
}

impl Syntactic {
  /// Parses `source` as a type-system document: [`Layer::type_system_document`], callable with no
  /// trait in scope.
  ///
  /// A text with a syntax error has no document here. Each error of the fail-fast parse is lent to
  /// `report`, and the answer is [`Rejected`].
  ///
  /// It is generic over the source, and it exists where the layer has an impl: for the sources
  /// the lexer reads. For any other the compiler refuses the call: the bound
  /// `Syntactic<GraphQLx>: Layer<'_, Src>` is not satisfied, and its help names sources the layer
  /// does read. So a holder of a `String` passes `s.as_str()`, a holder of a `Vec<u8>` passes
  /// `v.as_slice()`, and a holder of a `&str` passes it as it is, not a reference to it.
  pub fn type_system_document<'src, Src, K>(
    &self,
    source: &'src Src,
    report: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<<Src as Source<usize>>::Slice<'src>>, ()>, Rejected>
  where
    Src: Source<usize> + ?Sized,
    Self: Layer<'src, Src, Failure = Rejected, Tree = ()>,
    K: Report<<Self as Layer<'src, Src>>::Error> + ?Sized,
  {
    <Self as Layer<'src, Src>>::type_system_document(self, source, report)
  }
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
impl Lossless {
  /// Parses `source` as a type-system document: [`Layer::type_system_document`], callable with no
  /// trait in scope.
  ///
  /// It is generic over the source and `str` alone satisfies it: a tree holds text only. A holder
  /// of a `String` passes `s.as_str()`, and a holder of bytes decodes them with
  /// `core::str::from_utf8` first. Any other source is refused by the compiler as a mismatched
  /// type, `expected &str`: the layer's one impl decides the source.
  ///
  /// The answer is a document for every text the parse can build a tree of, broken or not:
  /// [`Parsed::is_valid`] says whether the text was a document. The failure is the tree's own
  /// refusal of a text longer than it can address, `Refused::SourceTooLong`, decided before any
  /// scan.
  pub fn type_system_document<'src, Src, K>(
    &self,
    source: &'src Src,
    report: &mut K,
  ) -> Result<
    Parsed<TypeSystemDocument<<Src as Source<usize>>::Slice<'src>>, lossless::Parse>,
    Refused,
  >
  where
    Src: Source<usize> + ?Sized,
    Self: Layer<'src, Src, Failure = Refused, Tree = lossless::Parse>,
    K: Report<<Self as Layer<'src, Src>>::Error> + ?Sized,
  {
    <Self as Layer<'src, Src>>::type_system_document(self, source, report)
  }
}

/// What the syntactic layer answers for a type-system document over a source its lexer reads: the
/// one body behind each of its [`Layer`] impls.
///
/// The where-clause is the production's own, with the lexer's state fixed for the limits a parse
/// starts from. A source the lexer does not read does not satisfy it.
fn read_type_system_document<'src, Src, K>(
  limits: SyntacticLimits,
  source: &'src Src,
  report: &mut K,
) -> Result<Parsed<TypeSystemDocument<GraphqlxSlice<'src, Src>>, ()>, Rejected>
where
  Src: Source<usize> + ?Sized + 'src,
  GraphqlxSlice<'src, Src>: Clone + Leaf,
  GraphqlxToken<'src, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'src, Src>: Lexer<
      'src,
      State = SyntacticLimits,
      Source = Src,
      Token = GraphqlxToken<'src, Src>,
      Span = SimpleSpan,
      Offset = usize,
    >,
  K: Report<error::GraphqlxError<GraphqlxSlice<'src, Src>>> + ?Sized,
{
  let parsed: Result<
    TypeSystemDocument<GraphqlxSlice<'src, Src>>,
    GraphqlxErrors<GraphqlxSlice<'src, Src>>,
  > = Parser::with_parser::<
    'src,
    GraphqlxLexer<'src, Src>,
    TypeSystemDocument<GraphqlxSlice<'src, Src>>,
    GraphqlxErrors<GraphqlxSlice<'src, Src>>,
    _,
    GraphQLx,
  >(type_system_document)
  .parse_with_state(source, limits);
  match parsed {
    Ok(document) => {
      let recovery = Recovery::new(items(&document), 0);
      Ok(Parsed::new(document, recovery, 0, ()))
    }
    Err(errors) => Err(lend(&errors, report)),
  }
}

/// Lends `report` each error of a fail-fast parse until it breaks, and answers how many the parse
/// returned.
///
/// No production of this dialect returns an empty container: every conversion into it builds one
/// error. Were one ever to, the text would still not be a document, so one `invalid-syntax` error
/// at the start of the text is lent in its place and the count is never zero.
fn lend<S, K>(errors: &GraphqlxErrors<S>, report: &mut K) -> Rejected
where
  K: Report<error::GraphqlxError<S>> + ?Sized,
{
  if errors.is_empty() {
    let invalid = error::GraphqlxError::<S>::new(
      SimpleSpan::new(0, 0),
      ErrorData::Lexical(SyntaxErrorKind::InvalidSyntax),
    );
    let _ = report.error(&invalid);
    return Rejected::new(1);
  }
  for each in errors.iter() {
    if report.error(each).is_break() {
      break;
    }
  }
  Rejected::new(errors.len())
}

/// How many items `document` holds at every module depth: each `use` item, inline module,
/// definition and extension, and the items inside each module, which an inline module counts one
/// beside. The count saturates.
///
/// A worklist and not a recursion, so a document nested however deep costs no native stack; it
/// allocates only when a module holds items.
fn items<S>(document: &TypeSystemDocument<S>) -> u32 {
  let mut count = 0u32;
  let mut pending: Vec<&[TypeSystemItem<S>]> = Vec::new();
  let mut current: &[TypeSystemItem<S>] = document.definitions();
  loop {
    for item in current {
      count = count.saturating_add(1);
      if let TypeSystemItem::Mod(module) = item {
        pending.push(module.items());
      }
    }
    match pending.pop() {
      Some(next) => current = next,
      None => return count,
    }
  }
}
