//! The GraphQL dialect's grammar productions.
//!
//! Productions in this module are specialized to the concrete
//! [`GraphqlLexer`] and [`GraphQL`](super::GraphQL) marker. The public aliases
//! below keep their source, token, input, and error signatures consistent while
//! allowing the lexer to run over `str`, `[u8]`, and the source wrappers it
//! supports.
//!
//! The module name reflects the driving lexer the syntactic suite pairs these
//! productions with; the productions themselves are purely syntactic — lossless/CST
//! structure is a separate `lossless` module's concern (a later wave).

use smear_lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};
use tokora::{ErrorOf, InputRef, Lexer, ParseContext, Source};

use super::GraphQL;

/// The concrete lexer used by GraphQL syntactic productions over `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlLexer<'inp, Src: ?Sized> = SyntacticLexer<'inp, Src>;

/// The source slice emitted by [`GraphqlLexer`] for `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlSlice<'inp, Src: Source<usize> + ?Sized> = <Src as Source<usize>>::Slice<'inp>;

/// The concrete token emitted by [`GraphqlLexer`] for `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlToken<'inp, Src: Source<usize> + ?Sized> = SyntacticToken<GraphqlSlice<'inp, Src>>;

/// The parser error emitted by a GraphQL syntactic production.
#[allow(type_alias_bounds)]
pub type GraphqlError<'inp, Src: ?Sized, Ctx>
where
  GraphqlLexer<'inp, Src>: Lexer<'inp>,
  Ctx: ParseContext<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
= ErrorOf<'inp, GraphqlLexer<'inp, Src>, Ctx, GraphQL>;

/// A mutable GraphQL syntactic parser input over `Src` and parser context `Ctx`.
#[allow(type_alias_bounds)]
pub type GraphqlInput<'inp, 'input, Src: ?Sized, Ctx>
where
  GraphqlLexer<'inp, Src>: Lexer<'inp>,
  Ctx: ParseContext<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
= InputRef<'inp, 'input, GraphqlLexer<'inp, Src>, Ctx, GraphQL>;

pub mod argument;
pub mod ty;
pub mod value;
