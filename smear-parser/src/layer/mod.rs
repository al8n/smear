//! smear's two parser layers as values, what a layer answers for one document, and where a layer
//! hands the syntax errors it found.
//!
//! The syntactic layer stops at a document's first syntax error and keeps nothing beside the
//! document. The lossless layer recovers, lends every syntax error to the caller's sink, and keeps
//! the tree. Both answer the same AST: a consumer that is handed a document never learns which
//! layer made it, and a consumer that is handed text takes the layer as a value.
//!
//! Each dialect module declares its own `Layer` trait, implements it for the two values typed by
//! its dialect, and names them `Syntactic` and `Lossless`. The trait is sealed for each layer and
//! source: an impl exists for the syntactic layer over each source its lexer reads and for the
//! lossless layer over `str`, and no other crate can add one, whatever source it brings. This
//! module holds what the dialects share: the values, the answer ([`Parsed`], [`Recovery`],
//! [`Rejected`]), the sink ([`Report`], [`from_fn`]), and [`SyntaxTree`], what a kept tree answers
//! for an extent.

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
/// Both layers count by one rule, at every module depth. Standard GraphQL has no modules, so there
/// the rule counts the document's top level:
///
/// - [`projected`](Self::projected) counts every item that has an AST image. An item is what the
///   AST calls one: in GraphQLx a `use` item, an inline module, a definition or an extension; in
///   standard GraphQL a definition or an extension. An inline module counts one, beside the items
///   inside it, and a `use` item counts one.
/// - [`skipped`](Self::skipped) counts every element that has none: an item the projection
///   refused, a recovery hole or gap tile the parser left in an item's place, any other node it
///   left between items, and each token between items that is neither trivia nor the `}` that
///   closes a module's body. A module whose header has no image is one skipped element, and
///   nothing inside it is counted.
///
/// A missing `}` costs nothing: a module whose node holds no `}` of its own is projected with what
/// it holds. Both counts saturate.
///
/// The syntactic layer answers a document only for a text it read whole, so its `skipped` is zero
/// and its recovery is always complete.
///
/// The fail-fast projection answers `Result<Ast, ProjectError>`: one refusal, at the first
/// obstruction, for a caller that wants the AST or nothing. A recovering projection answers the
/// question an editor asks instead — *what does the part that is still well-formed say?* — and
/// this is the honesty half of that answer.
///
/// # What `skipped` says
///
/// `skipped() > 0` means at least one element, at some module depth, had no AST image: no item
/// of the AST stands for it. Its text can still lie inside a span. A module that was kept spans
/// its whole node, so its span covers whatever was skipped inside it, and the document spans its
/// first kept item to its last.
///
/// [`is_complete`](Self::is_complete) is the one-call form of that question. It is a statement
/// about **loss**, not about validity: it is the only state in which every element of the document
/// has an image in the AST, and it does not imply that a fail-fast projection of the same parse
/// succeeds.
///
/// # Complete here, refused fail-fast — deliberately
///
/// The two door families answer different contracts. A recovering door steps through **every**
/// document container under the root and answers per entry; a fail-fast door asserts the root holds
/// **exactly one** container plus trivia, and that the document has an item. So two shapes are
/// complete here and refused there:
///
/// - a caller-minted root holding two valid containers: every item of both is projected,
///   `skipped` is zero, and the fail-fast door answers `UnexpectedChild { parent: Root, .. }` at
///   the second container. This is kept, not closed: **each container is a legitimate document
///   image, and the recovering door's contract is per entry** — it reports what had an AST image
///   and what did not, and every item of the second container has one; and
/// - an empty or trivia-only parse: nothing is lost and nothing is projected, and the fail-fast
///   door refuses a document with no item.
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

  /// Returns how many items had an AST image, at every module depth.
  ///
  /// An inline module counts one, beside the items inside it, and a `use` item counts one. The
  /// count saturates.
  #[inline]
  pub const fn projected(&self) -> u32 {
    self.projected
  }

  /// Returns how many elements had no AST image and were dropped, at every module depth.
  ///
  /// An *element*, not an item: the count includes an item the projection refused, a recovery
  /// hole or gap tile the parser left in an item's place, any other node it left between items,
  /// and each non-trivia token the parser could not attach to an item. One mistyped keyword can
  /// leave several. A module whose header has no image counts one, and nothing inside it is
  /// counted. The count saturates.
  #[inline]
  pub const fn skipped(&self) -> u32 {
    self.skipped
  }

  /// Returns whether every element had an AST image, at every module depth.
  ///
  /// True exactly when [`skipped`](Self::skipped) is zero.
  ///
  /// It does not say the fail-fast projection would have succeeded. That door additionally asserts
  /// the root holds exactly one document container and that the document has an item, so a
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

  /// How many syntax errors the parse found: how many a sink that never breaks is lent.
  ///
  /// It is not how many a sink took: a sink that breaks takes fewer, and the count stays.
  ///
  /// It counts errors, where a kept tree counts records: one record of the lossless tree can hold
  /// several errors. So for the lossless layer the count is never less than the number of the
  /// tree's error diagnostics, and it is zero exactly when the tree has none.
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

/// The lossless layer of the dialect `D`: recovers, lends every syntax error to the caller's sink,
/// and keeps the tree.
///
/// `D` is the dialect's brand, and each dialect names its own instance `Lossless`. The value holds
/// the lexer's [`LosslessLimits`], which bound how deep a parse descends and how much work a text
/// may buy. A text is read as `str`: a tree holds text only, so a caller that holds bytes decodes
/// them first.
///
/// # Memory, and untrusted text
///
/// An error is lent by reference and never copied, so an answer keeps what its tree keeps and
/// nothing more. While a parse runs it holds one record for each error it has found, and nothing
/// bounds their number but the text: a text dense with errors is recorded error for error. For
/// untrusted text the bound is the caller's own
/// [`with_max_produce_events`](LosslessLimits::with_max_produce_events): the records a parse
/// keeps are a bounded multiple of the lexical items it is allowed to produce.
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
pub trait SyntaxTree: sealed::SealedTree {
  /// What the tree answers for an extent.
  type Node;

  /// The smallest node whose range covers `span`, or the parent of the token that covers it;
  /// `None` for a span that does not lie inside the tree's text.
  fn covering_node(&self, span: SimpleSpan) -> Option<Self::Node>;
}

/// What keeps the layer traits and [`SyntaxTree`] closed: only this crate implements these.
pub(crate) mod sealed {
  /// The seal of a dialect's `Layer<'src, Src>`, with that trait's own parameters: implemented for
  /// a layer over each source it reads, beside the `Layer` impl it seals, and by nothing outside
  /// this crate.
  ///
  /// A seal with no source parameter would hold for a layer whatever the source, and another
  /// crate could then implement `Layer` over a source of its own: the orphan rule admits such an
  /// impl because the source is local to it, and no impl here would overlap it.
  #[cfg(all(
    any(feature = "std", feature = "alloc"),
    any(feature = "graphql", feature = "graphqlx")
  ))]
  pub trait Sealed<'src, Src: ?Sized> {}

  /// The seal of [`SyntaxTree`](super::SyntaxTree): implemented by the lossless tree, and by
  /// nothing outside this crate.
  pub trait SealedTree {}
}

// ── The layers' compile-time pins ──────────────────────────────────────────────────────────────
//
// Each `compile_fail` block sits beside a block that compiles and differs from it only in what the
// pin is about. A `compile_fail` passes for any error, a typo included, so the twin is what says a
// pin fails for its own reason; the error code says which, and rustdoc checks a code on nightly
// only. Each section is gated as the names its blocks use, so a build without a feature has neither
// the pin nor its twin.
//
// The seal's pins are written as another crate would write them, which a doctest is. Each
// implements a dialect's `Layer` for one of the layer values over `M`, a source of the doctest's
// own, and its twin is the same block with the impl written for `Mirror`, a local trait that has
// `Layer`'s items. `M` is one type in all of them: a real tokora source whose slice is `&str`. So
// nothing but the seal refuses a pin: the orphan rule admits the impl, because `M` is local to it,
// and no impl of this crate overlaps it, because each of those names one of the lexer's own
// sources.
#[cfg_attr(
  all(any(feature = "std", feature = "alloc"), feature = "graphqlx"),
  doc = r##"
# A closure that reads the error writes its type

The closure's body is checked before a layer fixes the error's type, so a body that calls a method
on the error does not compile without the type:

```compile_fail,E0282
use core::ops::ControlFlow;
use smear_parser::{graphqlx::Syntactic, layer::from_fn};

let mut messages = Vec::new();
let mut sink = from_fn(|error| {
  messages.push(error.to_string());
  ControlFlow::Continue(())
});
let _ = Syntactic::new().type_system_document("type T {", &mut sink);
```

and with the type on the parameter it does:

```
use core::ops::ControlFlow;
use smear_parser::{
  graphqlx::{Syntactic, error::GraphqlxError},
  layer::from_fn,
};

let mut messages = Vec::new();
let mut sink = from_fn(|error: &GraphqlxError<&str>| {
  messages.push(error.to_string());
  ControlFlow::Continue(())
});
let _ = Syntactic::new().type_system_document("type T {", &mut sink);
assert!(!messages.is_empty());
```

# The syntactic layer reads the sources its lexer reads

The layer has one impl for each of them, and none for anything else. A `String` is not a source,
and a `&String` does not coerce at a generic `&Src`:

```compile_fail,E0277
use smear_parser::graphqlx::Syntactic;

let text = String::from("type T { f: Int }");
let _ = Syntactic::new().type_system_document(&text, &mut ());
```

`String` is `AsRef<str>` and `AsRef<[u8]>`, so `as_ref` names no source. The compiler says so
twice: first as E0284, for the slice of a source it cannot name, then as E0283, for the impls it
cannot choose between, and rustdoc asks only that the code a fence names be among those it emits:

```compile_fail,E0284
use smear_parser::graphqlx::Syntactic;

let text = String::from("type T { f: Int }");
let _ = Syntactic::new().type_system_document(text.as_ref(), &mut ());
```

A `&&str` makes the source `&str`, which is a tokora source and not one the lexer reads, so the
layer has no impl over it:

```compile_fail,E0277
use smear_parser::graphqlx::Syntactic;

let text: &str = "type T { f: Int }";
let _ = Syntactic::new().type_system_document(&text, &mut ());
```

The three, with the `str` passed:

```
use smear_parser::graphqlx::Syntactic;

let text = String::from("type T { f: Int }");
let parsed = Syntactic::new()
  .type_system_document(text.as_str(), &mut ())
  .expect("a document");
assert!(parsed.is_valid());

let text: &str = "type T { f: Int }";
let parsed = Syntactic::new()
  .type_system_document(text, &mut ())
  .expect("a document");
assert!(parsed.is_valid());
```

# A consumer generic over the source states the source's bound

The trait's declaration asks nothing of `Src`, and its methods ask `Src: Source<usize>`:

```compile_fail,E0277
use smear_parser::graphqlx::Layer;

fn read<'s, Src: ?Sized, P: Layer<'s, Src>>(parser: &P, source: &'s Src) -> bool {
  parser.type_system_document(source, &mut ()).is_ok()
}
```

and with the bound stated it compiles:

```
use smear_parser::graphqlx::{Layer, Syntactic};
use tokora::Source;

fn read<'s, Src: Source<usize> + ?Sized, P: Layer<'s, Src>>(parser: &P, source: &'s Src) -> bool {
  parser.type_system_document(source, &mut ()).is_ok()
}

assert!(read(&Syntactic::new(), "type T { f: Int }"));
```

# The trait is sealed for each layer and source

Another crate cannot implement it for a layer value of a brand of its own, over a source of its
own:

```compile_fail,E0277
use smear_parser::{
  graphqlx::{self, ast::TypeSystemDocument, error::GraphqlxError},
  layer::{Parsed, Report},
};
use tokora::{Source, diagnostic::Diagnose};

#[derive(Debug)]
struct M;

impl Source<usize> for M {
  type Slice<'s>
    = &'s str
  where
    Self: 's;

  fn is_empty(&self) -> bool {
    unimplemented!()
  }

  fn len(&self) -> usize {
    unimplemented!()
  }

  fn as_slice(&self) -> Self::Slice<'_> {
    unimplemented!()
  }

  fn slice<R>(&self, _: R) -> Option<Self::Slice<'_>>
  where
    R: core::ops::RangeBounds<usize>,
  {
    unimplemented!()
  }

  fn is_boundary(&self, _: usize) -> bool {
    unimplemented!()
  }
}

trait Mirror<'src, Src: ?Sized = str> {
  type Error: Diagnose;
  type Failure;
  type Tree;

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

struct MyBrand;

impl<'a> graphqlx::Layer<'a, M> for smear_parser::layer::Syntactic<MyBrand> {
  type Error = GraphqlxError<&'a str>;
  type Failure = ();
  type Tree = ();

  fn type_system_document<K>(
    &self,
    _: &'a M,
    _: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<<M as Source<usize>>::Slice<'a>>, ()>, ()>
  where
    K: Report<Self::Error> + ?Sized,
  {
    unimplemented!()
  }
}
```

The same block, for the local trait:

```
use smear_parser::{
  graphqlx::{self, ast::TypeSystemDocument, error::GraphqlxError},
  layer::{Parsed, Report},
};
use tokora::{Source, diagnostic::Diagnose};

#[derive(Debug)]
struct M;

impl Source<usize> for M {
  type Slice<'s>
    = &'s str
  where
    Self: 's;

  fn is_empty(&self) -> bool {
    unimplemented!()
  }

  fn len(&self) -> usize {
    unimplemented!()
  }

  fn as_slice(&self) -> Self::Slice<'_> {
    unimplemented!()
  }

  fn slice<R>(&self, _: R) -> Option<Self::Slice<'_>>
  where
    R: core::ops::RangeBounds<usize>,
  {
    unimplemented!()
  }

  fn is_boundary(&self, _: usize) -> bool {
    unimplemented!()
  }
}

trait Mirror<'src, Src: ?Sized = str> {
  type Error: Diagnose;
  type Failure;
  type Tree;

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

struct MyBrand;

impl<'a> Mirror<'a, M> for smear_parser::layer::Syntactic<MyBrand> {
  type Error = GraphqlxError<&'a str>;
  type Failure = ();
  type Tree = ();

  fn type_system_document<K>(
    &self,
    _: &'a M,
    _: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<<M as Source<usize>>::Slice<'a>>, ()>, ()>
  where
    K: Report<Self::Error> + ?Sized,
  {
    unimplemented!()
  }
}
```

Nor for the dialect's own syntactic layer, over a source of its own. This is the case the seal
alone decides: the layer's impls each name one of the lexer's sources, so none of them overlaps an
impl over `M`, whatever `M`'s slice is:

```compile_fail,E0277
use smear_parser::{
  graphqlx::{self, ast::TypeSystemDocument, error::GraphqlxError},
  layer::{Parsed, Report},
};
use tokora::{Source, diagnostic::Diagnose};

#[derive(Debug)]
struct M;

impl Source<usize> for M {
  type Slice<'s>
    = &'s str
  where
    Self: 's;

  fn is_empty(&self) -> bool {
    unimplemented!()
  }

  fn len(&self) -> usize {
    unimplemented!()
  }

  fn as_slice(&self) -> Self::Slice<'_> {
    unimplemented!()
  }

  fn slice<R>(&self, _: R) -> Option<Self::Slice<'_>>
  where
    R: core::ops::RangeBounds<usize>,
  {
    unimplemented!()
  }

  fn is_boundary(&self, _: usize) -> bool {
    unimplemented!()
  }
}

trait Mirror<'src, Src: ?Sized = str> {
  type Error: Diagnose;
  type Failure;
  type Tree;

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

impl<'a> graphqlx::Layer<'a, M> for graphqlx::Syntactic {
  type Error = GraphqlxError<&'a str>;
  type Failure = ();
  type Tree = ();

  fn type_system_document<K>(
    &self,
    _: &'a M,
    _: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<<M as Source<usize>>::Slice<'a>>, ()>, ()>
  where
    K: Report<Self::Error> + ?Sized,
  {
    unimplemented!()
  }
}
```

The same block, for the local trait:

```
use smear_parser::{
  graphqlx::{self, ast::TypeSystemDocument, error::GraphqlxError},
  layer::{Parsed, Report},
};
use tokora::{Source, diagnostic::Diagnose};

#[derive(Debug)]
struct M;

impl Source<usize> for M {
  type Slice<'s>
    = &'s str
  where
    Self: 's;

  fn is_empty(&self) -> bool {
    unimplemented!()
  }

  fn len(&self) -> usize {
    unimplemented!()
  }

  fn as_slice(&self) -> Self::Slice<'_> {
    unimplemented!()
  }

  fn slice<R>(&self, _: R) -> Option<Self::Slice<'_>>
  where
    R: core::ops::RangeBounds<usize>,
  {
    unimplemented!()
  }

  fn is_boundary(&self, _: usize) -> bool {
    unimplemented!()
  }
}

trait Mirror<'src, Src: ?Sized = str> {
  type Error: Diagnose;
  type Failure;
  type Tree;

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

impl<'a> Mirror<'a, M> for graphqlx::Syntactic {
  type Error = GraphqlxError<&'a str>;
  type Failure = ();
  type Tree = ();

  fn type_system_document<K>(
    &self,
    _: &'a M,
    _: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<<M as Source<usize>>::Slice<'a>>, ()>, ()>
  where
    K: Report<Self::Error> + ?Sized,
  {
    unimplemented!()
  }
}
```
"##
)]
#[cfg_attr(
  all(feature = "graphqlx", feature = "rowan_0_17", feature = "std"),
  doc = r##"
# The lossless layer reads `str` and nothing else

Its one impl decides the source, so bytes are a mismatched type. Through the inherent method:

```compile_fail,E0308
use smear_parser::graphqlx::Lossless;

let bytes: &[u8] = b"type T { f: Int }";
let _ = Lossless::new().type_system_document(bytes, &mut ());
```

and through the trait:

```compile_fail,E0308
use smear_parser::graphqlx::{Layer, Lossless};

let bytes: &[u8] = b"type T { f: Int }";
let _ = Layer::type_system_document(&Lossless::new(), bytes, &mut ());
```

The two, with the bytes decoded first:

```
use smear_parser::graphqlx::{Layer, Lossless};

let bytes: &[u8] = b"type T { f: Int }";
let text = core::str::from_utf8(bytes).expect("UTF-8");
let parsed = Lossless::new()
  .type_system_document(text, &mut ())
  .expect("short enough for a tree");
assert!(parsed.is_valid());
let parsed = Layer::type_system_document(&Lossless::new(), text, &mut ())
  .expect("short enough for a tree");
assert!(parsed.is_valid());
```

# The lossless layer is sealed over `str` alone

Another crate cannot implement the trait for it over a source of its own:

```compile_fail,E0277
use smear_parser::{
  graphqlx::{self, ast::TypeSystemDocument, error::GraphqlxError},
  layer::{Parsed, Report},
};
use tokora::{Source, diagnostic::Diagnose};

#[derive(Debug)]
struct M;

impl Source<usize> for M {
  type Slice<'s>
    = &'s str
  where
    Self: 's;

  fn is_empty(&self) -> bool {
    unimplemented!()
  }

  fn len(&self) -> usize {
    unimplemented!()
  }

  fn as_slice(&self) -> Self::Slice<'_> {
    unimplemented!()
  }

  fn slice<R>(&self, _: R) -> Option<Self::Slice<'_>>
  where
    R: core::ops::RangeBounds<usize>,
  {
    unimplemented!()
  }

  fn is_boundary(&self, _: usize) -> bool {
    unimplemented!()
  }
}

trait Mirror<'src, Src: ?Sized = str> {
  type Error: Diagnose;
  type Failure;
  type Tree;

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

impl<'a> graphqlx::Layer<'a, M> for graphqlx::Lossless {
  type Error = GraphqlxError<&'a str>;
  type Failure = ();
  type Tree = ();

  fn type_system_document<K>(
    &self,
    _: &'a M,
    _: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<<M as Source<usize>>::Slice<'a>>, ()>, ()>
  where
    K: Report<Self::Error> + ?Sized,
  {
    unimplemented!()
  }
}
```

The same block, for the local trait:

```
use smear_parser::{
  graphqlx::{self, ast::TypeSystemDocument, error::GraphqlxError},
  layer::{Parsed, Report},
};
use tokora::{Source, diagnostic::Diagnose};

#[derive(Debug)]
struct M;

impl Source<usize> for M {
  type Slice<'s>
    = &'s str
  where
    Self: 's;

  fn is_empty(&self) -> bool {
    unimplemented!()
  }

  fn len(&self) -> usize {
    unimplemented!()
  }

  fn as_slice(&self) -> Self::Slice<'_> {
    unimplemented!()
  }

  fn slice<R>(&self, _: R) -> Option<Self::Slice<'_>>
  where
    R: core::ops::RangeBounds<usize>,
  {
    unimplemented!()
  }

  fn is_boundary(&self, _: usize) -> bool {
    unimplemented!()
  }
}

trait Mirror<'src, Src: ?Sized = str> {
  type Error: Diagnose;
  type Failure;
  type Tree;

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

impl<'a> Mirror<'a, M> for graphqlx::Lossless {
  type Error = GraphqlxError<&'a str>;
  type Failure = ();
  type Tree = ();

  fn type_system_document<K>(
    &self,
    _: &'a M,
    _: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<<M as Source<usize>>::Slice<'a>>, ()>, ()>
  where
    K: Report<Self::Error> + ?Sized,
  {
    unimplemented!()
  }
}
```
"##
)]
#[cfg_attr(
  all(
    feature = "graphqlx",
    feature = "rowan_0_17",
    feature = "std",
    feature = "bytes_1"
  ),
  doc = r##"
# The lossless layer does not read a `Bytes` body

Through the inherent method:

```compile_fail,E0308
use smear_parser::graphqlx::Lossless;

let body = bytes_1::Bytes::from_static(b"type T { f: Int }");
let _ = Lossless::new().type_system_document(&body, &mut ());
```

and through the trait:

```compile_fail,E0308
use smear_parser::graphqlx::{Layer, Lossless};

let body = bytes_1::Bytes::from_static(b"type T { f: Int }");
let _ = Layer::type_system_document(&Lossless::new(), &body, &mut ());
```

The two, with the body decoded first:

```
use smear_parser::graphqlx::{Layer, Lossless};

let body = bytes_1::Bytes::from_static(b"type T { f: Int }");
let text = core::str::from_utf8(&body).expect("UTF-8");
let parsed = Lossless::new()
  .type_system_document(text, &mut ())
  .expect("short enough for a tree");
assert!(parsed.is_valid());
let parsed = Layer::type_system_document(&Lossless::new(), text, &mut ())
  .expect("short enough for a tree");
assert!(parsed.is_valid());
```
"##
)]
#[cfg_attr(
  all(any(feature = "std", feature = "alloc"), feature = "graphql"),
  doc = r##"
# GraphQL's trait is sealed for each layer and source

The seal is a supertrait of each dialect's own trait, so each dialect has its pin. Another crate
cannot implement GraphQL's trait for GraphQL's syntactic layer over a source of its own:

```compile_fail,E0277
use smear_parser::{
  graphql::{
    self,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlError,
  },
  layer::{Parsed, Report},
};
use tokora::{Source, diagnostic::Diagnose};

#[derive(Debug)]
struct M;

impl Source<usize> for M {
  type Slice<'s>
    = &'s str
  where
    Self: 's;

  fn is_empty(&self) -> bool {
    unimplemented!()
  }

  fn len(&self) -> usize {
    unimplemented!()
  }

  fn as_slice(&self) -> Self::Slice<'_> {
    unimplemented!()
  }

  fn slice<R>(&self, _: R) -> Option<Self::Slice<'_>>
  where
    R: core::ops::RangeBounds<usize>,
  {
    unimplemented!()
  }

  fn is_boundary(&self, _: usize) -> bool {
    unimplemented!()
  }
}

trait Mirror<'src, Src: ?Sized = str> {
  type Error: Diagnose;
  type Failure;
  type Tree;

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

impl<'a> graphql::Layer<'a, M> for graphql::Syntactic {
  type Error = GraphqlError<&'a str>;
  type Failure = ();
  type Tree = ();

  fn type_system_document<K>(
    &self,
    _: &'a M,
    _: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<<M as Source<usize>>::Slice<'a>>, ()>, ()>
  where
    K: Report<Self::Error> + ?Sized,
  {
    unimplemented!()
  }

  fn executable_document<K>(
    &self,
    _: &'a M,
    _: &mut K,
  ) -> Result<Parsed<ExecutableDocument<<M as Source<usize>>::Slice<'a>>, ()>, ()>
  where
    K: Report<Self::Error> + ?Sized,
  {
    unimplemented!()
  }
}
```

The same block, for the local trait:

```
use smear_parser::{
  graphql::{
    self,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlError,
  },
  layer::{Parsed, Report},
};
use tokora::{Source, diagnostic::Diagnose};

#[derive(Debug)]
struct M;

impl Source<usize> for M {
  type Slice<'s>
    = &'s str
  where
    Self: 's;

  fn is_empty(&self) -> bool {
    unimplemented!()
  }

  fn len(&self) -> usize {
    unimplemented!()
  }

  fn as_slice(&self) -> Self::Slice<'_> {
    unimplemented!()
  }

  fn slice<R>(&self, _: R) -> Option<Self::Slice<'_>>
  where
    R: core::ops::RangeBounds<usize>,
  {
    unimplemented!()
  }

  fn is_boundary(&self, _: usize) -> bool {
    unimplemented!()
  }
}

trait Mirror<'src, Src: ?Sized = str> {
  type Error: Diagnose;
  type Failure;
  type Tree;

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

impl<'a> Mirror<'a, M> for graphql::Syntactic {
  type Error = GraphqlError<&'a str>;
  type Failure = ();
  type Tree = ();

  fn type_system_document<K>(
    &self,
    _: &'a M,
    _: &mut K,
  ) -> Result<Parsed<TypeSystemDocument<<M as Source<usize>>::Slice<'a>>, ()>, ()>
  where
    K: Report<Self::Error> + ?Sized,
  {
    unimplemented!()
  }

  fn executable_document<K>(
    &self,
    _: &'a M,
    _: &mut K,
  ) -> Result<Parsed<ExecutableDocument<<M as Source<usize>>::Slice<'a>>, ()>, ()>
  where
    K: Report<Self::Error> + ?Sized,
  {
    unimplemented!()
  }
}
```
"##
)]
#[doc(hidden)]
pub mod __pins {}
