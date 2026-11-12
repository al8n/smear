//! smear's two parser layers as values, what a layer answers for one document, and where a layer
//! hands the syntax errors it found.
//!
//! The syntactic layer stops at a document's first syntax error and keeps nothing beside the
//! document. The lossless layer recovers, records every syntax error in its tree, and keeps the
//! tree. Both answer the same AST: a consumer that is handed a document never learns which layer
//! made it, and a consumer that is handed text takes the layer as a value.
//!
//! Each dialect module declares its own sealed `Layer` trait, implements it for the two values
//! typed by its dialect, and names them `Syntactic` and `Lossless`. This module holds what the
//! dialects share: the values, the answer ([`Parsed`], [`Recovery`], [`Rejected`]), the sink
//! ([`Report`], [`from_fn`]), and [`SyntaxTree`], what a kept tree answers for an extent.

use core::{fmt, marker::PhantomData, ops::ControlFlow};

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
use smear_lexer::limits::LosslessLimits;
use smear_lexer::limits::SyntacticLimits;
use tokora::{SimpleSpan, utils::IntoComponents};

/// Where a layer hands the syntax errors it found.
///
/// A layer calls [`error`](Self::error) once for each syntax error it kept, before its method
/// returns and never afterwards, in its own recording order. Each error is the layer's own typed
/// value, lent for the call.
///
/// Three sinks need no code: `()` keeps nothing, a `Vec<E>` keeps a clone of each error, and
/// `&mut K` forwards to `K`. A closure is a sink through [`from_fn`].
pub trait Report<E: ?Sized> {
  /// Takes one syntax error, lent for the call.
  ///
  /// [`Break`](ControlFlow::Break) asks for no more of them. It stops the hand-over and nothing
  /// else: the parse, the count of errors and the answer are what they would have been.
  fn error(&mut self, error: &E) -> ControlFlow<()>;
}

impl<E: ?Sized, K: Report<E> + ?Sized> Report<E> for &mut K {
  #[inline]
  fn error(&mut self, error: &E) -> ControlFlow<()> {
    (**self).error(error)
  }
}

/// The sink that keeps nothing.
impl<E: ?Sized> Report<E> for () {
  #[inline]
  fn error(&mut self, _: &E) -> ControlFlow<()> {
    ControlFlow::Continue(())
  }
}

/// The sink that keeps a clone of each error, in the order they were lent.
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
impl<E: Clone> Report<E> for std::vec::Vec<E> {
  #[inline]
  fn error(&mut self, error: &E) -> ControlFlow<()> {
    self.push(error.clone());
    ControlFlow::Continue(())
  }
}

/// A closure as a sink.
///
/// A closure is not a [`Report`] by itself. `&mut K` forwards to `K`, and a blanket impl for
/// closures would overlap that forward, because `&mut F` is itself a closure when `F` is.
///
/// A closure whose body reads the error writes the error's type, on its parameter or as
/// `from_fn::<E, _>`: the type is known only once the sink meets a layer, and the body is checked
/// before that. A body that does not read the error needs no type.
#[inline]
pub const fn from_fn<E: ?Sized, F>(f: F) -> FromFn<F>
where
  F: FnMut(&E) -> ControlFlow<()>,
{
  FromFn { f }
}

/// A closure as a sink: what [`from_fn`] answers.
#[derive(Clone, Copy)]
pub struct FromFn<F> {
  f: F,
}

impl<F> fmt::Debug for FromFn<F> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("FromFn").finish_non_exhaustive()
  }
}

impl<E: ?Sized, F> Report<E> for FromFn<F>
where
  F: FnMut(&E) -> ControlFlow<()>,
{
  #[inline]
  fn error(&mut self, error: &E) -> ControlFlow<()> {
    (self.f)(error)
  }
}

/// How much of a document an answer covers: how many items have an AST image, and how many
/// elements have none.
///
/// The syntactic layer answers a document only for a text it read whole, so its recovery is always
/// complete, and it counts every item, the items inside inline modules included.
///
/// The fail-fast projection answers `Result<Ast, ProjectError>`: one refusal, at the first
/// obstruction, for a caller that wants the AST or nothing. A recovering projection answers the
/// question an editor asks instead — *what does the part that is still well-formed say?* — and
/// this is the honesty half of that answer. A recovering projection counts the top-level elements
/// of the tree it walks.
///
/// # What `skipped` says
///
/// `skipped() > 0` means at least one top-level element had no AST image, and nothing in the AST
/// covers it.
///
/// [`is_complete`](Self::is_complete) is the one-call form of that question. It is a statement
/// about **loss**, not about validity: it is the only state in which the AST covers the whole
/// document, and it does not imply that a fail-fast projection of the same parse succeeds.
///
/// # Complete here, refused fail-fast — deliberately
///
/// The two door families answer different contracts. A recovering door steps through **every**
/// document container under the root and answers per entry; a fail-fast door asserts the root holds
/// **exactly one** container plus trivia, and that the document has a definition. So two shapes are
/// complete here and refused there:
///
/// - a caller-minted root holding two valid containers: every definition of both is projected,
///   `skipped` is zero, and the fail-fast door answers `UnexpectedChild { parent: Root, .. }` at
///   the second container. This is kept, not closed: **each container is a legitimate document
///   image, and the recovering door's contract is per entry** — it reports what had an AST image
///   and what did not, and every definition of the second container has one; and
/// - an empty or trivia-only parse: nothing is lost and nothing is projected, and the fail-fast
///   door refuses a document with no definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Recovery {
  projected: u32,
  skipped: u32,
}

impl Recovery {
  /// Builds a recovery tally.
  #[inline]
  pub const fn new(projected: u32, skipped: u32) -> Self {
    Self { projected, skipped }
  }

  /// Returns how many items had an AST image.
  ///
  /// A recovering projection counts the top-level definitions it projected; the syntactic layer
  /// counts every item of its document, at every module depth.
  #[inline]
  pub const fn projected(&self) -> u32 {
    self.projected
  }

  /// Returns how many top-level elements had no AST image and were dropped.
  ///
  /// An *element*, not a definition: the count includes a definition the projection refused, a
  /// recovery hole or gap tile the parser left in the definition's place, and each non-trivia token
  /// the parser could not attach to a definition. One mistyped keyword can leave several.
  #[inline]
  pub const fn skipped(&self) -> u32 {
    self.skipped
  }

  /// Returns whether every top-level element had an AST image.
  ///
  /// True exactly when [`skipped`](Self::skipped) is zero: every top-level element had an AST
  /// image.
  ///
  /// It does not say the fail-fast projection would have succeeded. That door additionally asserts
  /// the root holds exactly one document container and that the document has a definition, so a
  /// caller-minted root with two valid containers and an empty or trivia-only parse are both
  /// complete here and refused there — see the type's documentation for why.
  #[inline]
  pub const fn is_complete(&self) -> bool {
    self.skipped == 0
  }
}

impl fmt::Display for Recovery {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Self { projected, skipped } = self;
    write!(f, "{projected} projected, {skipped} skipped")
  }
}

/// What a layer answers for one document: the document, how much of the text it covers, how many
/// syntax errors the parse found, and what the layer kept beside it.
///
/// It has no public constructor: the layers are the only ones that make one.
#[derive(Debug, Clone, PartialEq)]
pub struct Parsed<D, T> {
  document: D,
  recovery: Recovery,
  errors: usize,
  tree: T,
}

impl<D, T> Parsed<D, T> {
  /// The answer a layer assembles.
  #[cfg(all(
    any(feature = "std", feature = "alloc"),
    any(feature = "graphql", feature = "graphqlx")
  ))]
  #[inline]
  pub(crate) const fn new(document: D, recovery: Recovery, errors: usize, tree: T) -> Self {
    Self {
      document,
      recovery,
      errors,
      tree,
    }
  }

  /// The items that have an AST image, as the syntactic AST.
  #[inline]
  pub const fn document(&self) -> &D {
    &self.document
  }

  /// How much of the text the document covers.
  #[inline]
  pub const fn recovery(&self) -> Recovery {
    self.recovery
  }

  /// How many syntax errors the parse found.
  ///
  /// It is not how many a sink took: a sink that breaks takes fewer, and the count stays.
  #[inline]
  pub const fn error_count(&self) -> usize {
    self.errors
  }

  /// Whether the text is a valid document: the parse found no syntax error.
  ///
  /// It speaks of the text, where [`recovery`](Self::recovery) speaks of the document. A document
  /// the lossless layer answers for a broken text holds the text's well-formed part, and this is
  /// `false` for it.
  #[inline]
  pub const fn is_valid(&self) -> bool {
    self.errors == 0
  }

  /// What the layer kept beside the document: `&()` for the syntactic layer, which keeps nothing,
  /// and the tree for the lossless layer.
  #[inline]
  pub const fn tree(&self) -> &T {
    &self.tree
  }
}

impl<D, T> IntoComponents for Parsed<D, T> {
  /// The document, the recovery, the error count and the tree.
  type Components = (D, Recovery, usize, T);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.document, self.recovery, self.errors, self.tree)
  }
}

/// The fail-fast parser found a syntax error, so the syntactic layer answers no document for the
/// text.
///
/// The errors themselves were lent to the sink before the answer, and this counts them. The
/// lossless layer's failure, `Refused`, is a different event: that layer did not read the source,
/// because a tree cannot address a text that long.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Rejected {
  errors: usize,
}

impl Rejected {
  /// The failure of a parse that returned `errors` syntax errors, at least one.
  #[cfg(all(
    any(feature = "std", feature = "alloc"),
    any(feature = "graphql", feature = "graphqlx")
  ))]
  #[inline]
  pub(crate) const fn new(errors: usize) -> Self {
    Self { errors }
  }

  /// How many syntax errors the parse returned: at least one.
  #[inline]
  pub const fn error_count(&self) -> usize {
    self.errors
  }
}

impl fmt::Display for Rejected {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.errors {
      1 => f.write_str("the text has a syntax error, so it is not a document"),
      errors => write!(
        f,
        "the text has {errors} syntax errors, so it is not a document"
      ),
    }
  }
}

impl core::error::Error for Rejected {}

/// The syntactic layer of the dialect `D`: fast, stops at a document's first syntax error, and
/// keeps nothing beside the document.
///
/// `D` is the dialect's brand, and each dialect names its own instance `Syntactic`. The value holds
/// the lexer's [`SyntacticLimits`], which bound how deep a parse descends.
pub struct Syntactic<D: ?Sized> {
  limits: SyntacticLimits,
  dialect: PhantomData<fn() -> D>,
}

impl<D: ?Sized> Syntactic<D> {
  /// The layer under [`SyntacticLimits::new`], the limits every syntactic parse starts from.
  #[inline]
  pub const fn new() -> Self {
    Self::with_limits(SyntacticLimits::new())
  }

  /// The layer under `limits`.
  #[inline]
  pub const fn with_limits(limits: SyntacticLimits) -> Self {
    Self {
      limits,
      dialect: PhantomData,
    }
  }

  /// The limits a parse runs under.
  #[inline]
  pub const fn limits(&self) -> SyntacticLimits {
    self.limits
  }
}

impl<D: ?Sized> Clone for Syntactic<D> {
  #[inline]
  fn clone(&self) -> Self {
    *self
  }
}

impl<D: ?Sized> Copy for Syntactic<D> {}

impl<D: ?Sized> Default for Syntactic<D> {
  #[inline]
  fn default() -> Self {
    Self::new()
  }
}

impl<D: ?Sized> fmt::Debug for Syntactic<D> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Syntactic")
      .field("limits", &self.limits)
      .finish()
  }
}

/// The lossless layer of the dialect `D`: recovers, records every syntax error in its tree, and
/// keeps the tree.
///
/// `D` is the dialect's brand, and each dialect names its own instance `Lossless`. The value holds
/// the lexer's [`LosslessLimits`], which bound how deep a parse descends and how much work a text
/// may buy. A text is read as `str`: a tree holds text only, so a caller that holds bytes decodes
/// them first.
#[cfg(all(feature = "rowan_0_17", feature = "std"))]
#[cfg_attr(docsrs, doc(cfg(all(feature = "rowan_0_17", feature = "std"))))]
pub struct Lossless<D: ?Sized> {
  limits: LosslessLimits,
  dialect: PhantomData<fn() -> D>,
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
impl<D: ?Sized> Lossless<D> {
  /// The layer under [`LosslessLimits::new`], the limits every lossless door starts from.
  #[inline]
  pub const fn new() -> Self {
    Self::with_limits(LosslessLimits::new())
  }

  /// The layer under `limits`.
  #[inline]
  pub const fn with_limits(limits: LosslessLimits) -> Self {
    Self {
      limits,
      dialect: PhantomData,
    }
  }

  /// The limits a parse runs under.
  #[inline]
  pub const fn limits(&self) -> LosslessLimits {
    self.limits
  }
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
impl<D: ?Sized> Clone for Lossless<D> {
  #[inline]
  fn clone(&self) -> Self {
    *self
  }
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
impl<D: ?Sized> Copy for Lossless<D> {}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
impl<D: ?Sized> Default for Lossless<D> {
  #[inline]
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
impl<D: ?Sized> fmt::Debug for Lossless<D> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Lossless")
      .field("limits", &self.limits)
      .finish()
  }
}

/// A syntax tree that can find the node an extent belongs to.
///
/// It lets a crate that names no tree type offer the lookup over whatever a layer kept.
pub trait SyntaxTree: sealed::Sealed {
  /// What the tree answers for an extent.
  type Node;

  /// The smallest node whose range covers `span`, or the parent of the token that covers it;
  /// `None` for a span that does not lie inside the tree's text.
  fn covering_node(&self, span: SimpleSpan) -> Option<Self::Node>;
}

/// What keeps the layer traits closed: only this crate implements `Sealed`.
pub(crate) mod sealed {
  /// Implemented by the two layers and the lossless tree, and by nothing outside this crate.
  pub trait Sealed {}

  impl<D: ?Sized> Sealed for super::Syntactic<D> {}

  #[cfg(all(feature = "rowan_0_17", feature = "std"))]
  impl<D: ?Sized> Sealed for super::Lossless<D> {}
}
