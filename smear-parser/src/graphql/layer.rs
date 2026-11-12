//! GraphQL's two parser layers, and the trait both implement.

use smear_lexer::{SyntaxErrorKind, graphql::ContextualKeyword, limits::SyntacticLimits};
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
use tokora::emitter::Severity;
use tokora::{
  Lexer, Parse as _, Parser, SimpleSpan, Source, diagnostic::Diagnose, utils::DowncastRef,
};

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
use super::lossless;
use super::{
  GraphQL,
  ast::{ExecutableDocument, TypeSystemDocument},
  error::{self, ErrorData, GraphqlErrors},
  syntactic::{
    GraphqlLexer, GraphqlSlice, GraphqlToken, executable_document, type_system_document,
  },
};
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
use crate::lossless::Refused;
use crate::{
  layer::{Parsed, Recovery, Rejected, Report, sealed},
  value::Leaf,
};

/// The syntactic layer of GraphQL.
///
/// It stops at a document's first syntax error and keeps nothing beside the document. It reads
/// every source the GraphQL syntactic lexer reads, and answers the AST over that source's own
/// slice.
pub type Syntactic = crate::layer::Syntactic<GraphQL>;

/// The lossless layer of GraphQL.
///
/// It recovers, records every syntax error in its tree, and keeps the tree. It reads `str`: a tree
/// holds text only, so a caller that holds bytes decodes them first.
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
#[cfg_attr(docsrs, doc(cfg(all(feature = "rowan_0_17", feature = "std"))))]
pub type Lossless = crate::layer::Lossless<GraphQL>;

/// One of smear's two parser layers, reading GraphQL from a `Src`.
///
/// Which sources a layer reads is which impls exist: the syntactic layer reads every source its
/// lexer reads, and the lossless layer reads `str`. The trait is sealed, and each layer has its
/// methods as inherent methods too, so a call on a concrete layer imports nothing.
///
/// A consumer of text bounds `P: Layer<'a>`, and its documents are over `&'a str` whichever layer
/// `P` is. A consumer generic over the source states `Src: Source<usize>` to call a method.
pub trait Layer<'src, Src: ?Sized = str>: sealed::Sealed {
  /// One syntax error, as this layer records it.
  type Error: Diagnose;
  /// Why this layer answers no document.
  type Failure;
  /// What this layer keeps for the caller beside the document.
  type Tree;

  /// Parses `source` as a type-system document.
  ///
  /// Lends `report` every syntax error it found. Answers the definitions that have an AST image,
  /// how much of the text they cover, how many errors there were, and what the layer kept; or why
  /// it answers no document.
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

  /// Parses `source` as an executable document.
  ///
  /// Lends `report` every syntax error it found. Answers the definitions that have an AST image,
  /// how much of the text they cover, how many errors there were, and what the layer kept; or why
  /// it answers no document.
  fn executable_document<K>(
    &self,
    source: &'src Src,
    report: &mut K,
  ) -> Result<
    Parsed<ExecutableDocument<<Src as Source<usize>>::Slice<'src>>, Self::Tree>,
    Self::Failure,
  >
  where
    Src: Source<usize>,
    K: Report<Self::Error> + ?Sized;
}

impl<'src, Src> Layer<'src, Src> for Syntactic
where
  Src: Source<usize> + ?Sized + 'src,
  GraphqlSlice<'src, Src>: Clone + Leaf,
  GraphqlToken<'src, Src>: DowncastRef<ContextualKeyword>,
  GraphqlLexer<'src, Src>: Lexer<
      'src,
      State = SyntacticLimits,
      Source = Src,
      Token = GraphqlToken<'src, Src>,
      Span = SimpleSpan,
      Offset = usize,
    >,
{
  type Error = error::GraphqlError<GraphqlSlice<'src, Src>>;
  type Failure = Rejected;
  type Tree = ();

  fn type_system_document<K>(
    &self,
    source: &'src Src,
    report: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<GraphqlSlice<'src, Src>>, ()>, Rejected>
  where
    K: Report<Self::Error> + ?Sized,
  {
    let parsed: Result<
      TypeSystemDocument<GraphqlSlice<'src, Src>>,
      GraphqlErrors<GraphqlSlice<'src, Src>>,
    > = Parser::with_parser::<
      'src,
      GraphqlLexer<'src, Src>,
      TypeSystemDocument<GraphqlSlice<'src, Src>>,
      GraphqlErrors<GraphqlSlice<'src, Src>>,
      _,
      GraphQL,
    >(type_system_document)
    .parse_with_state(source, self.limits());
    match parsed {
      Ok(document) => {
        let recovery = Recovery::new(count(document.definitions().len()), 0);
        Ok(Parsed::new(document, recovery, 0, ()))
      }
      Err(errors) => Err(lend(&errors, report)),
    }
  }

  fn executable_document<K>(
    &self,
    source: &'src Src,
    report: &mut K,
  ) -> Result<Parsed<ExecutableDocument<GraphqlSlice<'src, Src>>, ()>, Rejected>
  where
    K: Report<Self::Error> + ?Sized,
  {
    let parsed: Result<
      ExecutableDocument<GraphqlSlice<'src, Src>>,
      GraphqlErrors<GraphqlSlice<'src, Src>>,
    > = Parser::with_parser::<
      'src,
      GraphqlLexer<'src, Src>,
      ExecutableDocument<GraphqlSlice<'src, Src>>,
      GraphqlErrors<GraphqlSlice<'src, Src>>,
      _,
      GraphQL,
    >(executable_document)
    .parse_with_state(source, self.limits());
    match parsed {
      Ok(document) => {
        let recovery = Recovery::new(count(document.definitions().len()), 0);
        Ok(Parsed::new(document, recovery, 0, ()))
      }
      Err(errors) => Err(lend(&errors, report)),
    }
  }
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
impl<'src> Layer<'src, str> for Lossless {
  type Error = lossless::GraphqlLosslessErrorValue<&'src str>;
  type Failure = Refused;
  type Tree = lossless::Parse;

  // The hand-over is the door's to make while its emitter is alive, and the door does not make it
  // yet: these methods lend nothing, and count the errors the tree records.
  fn type_system_document<K>(
    &self,
    source: &'src str,
    _report: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<&'src str>, lossless::Parse>, Refused>
  where
    K: Report<Self::Error> + ?Sized,
  {
    // Asked before any scan: a text longer than a tree can address has no parse, where the `&str`
    // door would panic on it.
    if let Some(refused) = Refused::for_source_len(source.len()) {
      return Err(refused);
    }
    let parse = lossless::parse_type_system_document_with_limits(source, self.limits());
    let errors = errors_in(&parse);
    let (document, recovery) = lossless::project::project_own_type_system_document(&parse, source);
    Ok(Parsed::new(document, recovery, errors, parse))
  }

  fn executable_document<K>(
    &self,
    source: &'src str,
    _report: &mut K,
  ) -> Result<Parsed<ExecutableDocument<&'src str>, lossless::Parse>, Refused>
  where
    K: Report<Self::Error> + ?Sized,
  {
    if let Some(refused) = Refused::for_source_len(source.len()) {
      return Err(refused);
    }
    let parse = lossless::parse_executable_document_with_limits(source, self.limits());
    let errors = errors_in(&parse);
    let (document, recovery) = lossless::project::project_own_executable_document(&parse, source);
    Ok(Parsed::new(document, recovery, errors, parse))
  }
}

impl Syntactic {
  /// Parses `source` as a type-system document: [`Layer::type_system_document`], callable with no
  /// trait in scope.
  ///
  /// A text with a syntax error has no document here. Each error of the fail-fast parse is lent to
  /// `report`, and the answer is [`Rejected`].
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

  /// Parses `source` as an executable document: [`Layer::executable_document`], callable with no
  /// trait in scope.
  ///
  /// A text with a syntax error has no document here. Each error of the fail-fast parse is lent to
  /// `report`, and the answer is [`Rejected`].
  pub fn executable_document<'src, Src, K>(
    &self,
    source: &'src Src,
    report: &mut K,
  ) -> Result<Parsed<ExecutableDocument<<Src as Source<usize>>::Slice<'src>>, ()>, Rejected>
  where
    Src: Source<usize> + ?Sized,
    Self: Layer<'src, Src, Failure = Rejected, Tree = ()>,
    K: Report<<Self as Layer<'src, Src>>::Error> + ?Sized,
  {
    <Self as Layer<'src, Src>>::executable_document(self, source, report)
  }
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
impl Lossless {
  /// Parses `source` as a type-system document: [`Layer::type_system_document`], callable with no
  /// trait in scope.
  ///
  /// It is generic over the source and `str` alone satisfies it: a tree holds text only. A holder
  /// of a `String` passes `s.as_str()`, and a holder of bytes decodes them with
  /// `core::str::from_utf8` first.
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

  /// Parses `source` as an executable document: [`Layer::executable_document`], callable with no
  /// trait in scope.
  ///
  /// It reads the sources [`type_system_document`](Self::type_system_document) reads, and answers
  /// on the same terms.
  pub fn executable_document<'src, Src, K>(
    &self,
    source: &'src Src,
    report: &mut K,
  ) -> Result<
    Parsed<ExecutableDocument<<Src as Source<usize>>::Slice<'src>>, lossless::Parse>,
    Refused,
  >
  where
    Src: Source<usize> + ?Sized,
    Self: Layer<'src, Src, Failure = Refused, Tree = lossless::Parse>,
    K: Report<<Self as Layer<'src, Src>>::Error> + ?Sized,
  {
    <Self as Layer<'src, Src>>::executable_document(self, source, report)
  }
}

/// Lends `report` each error of a fail-fast parse until it breaks, and answers how many the parse
/// returned.
///
/// No production of this dialect returns an empty container: every conversion into it builds one
/// error. Were one ever to, the text would still not be a document, so one `invalid-syntax` error
/// at the start of the text is lent in its place and the count is never zero.
fn lend<S, K>(errors: &GraphqlErrors<S>, report: &mut K) -> Rejected
where
  K: Report<error::GraphqlError<S>> + ?Sized,
{
  if errors.is_empty() {
    let invalid = error::GraphqlError::<S>::new(
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

/// A document's definition count as a recovery counts it: standard GraphQL has no modules, so
/// every item is a top-level definition. The count saturates.
fn count(definitions: usize) -> u32 {
  u32::try_from(definitions).unwrap_or(u32::MAX)
}

/// How many errors a lossless parse recorded.
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
fn errors_in(parse: &lossless::Parse) -> usize {
  let mut errors = 0usize;
  for diagnostic in parse.diagnostics() {
    if diagnostic.severity() == Severity::Error {
      errors += 1;
    }
  }
  errors
}
