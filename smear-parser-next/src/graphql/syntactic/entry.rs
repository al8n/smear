//! The GraphQL syntactic entry: [`run_parse_source`] and the
//! [`ParseSource`] implementations for the dialect's parse roots.
//!
//! [`run_parse_source`] is the one place the dialect names a concrete lexer
//! (Lego-legal: entry runners and tests own that right): it drives a production
//! over [`SyntacticLexer`] under a fail-fast [`FatalContext`] carrying
//! [`GraphqlErrors`], generic over the source representation `S` — the emitter and
//! context obligations resolve at each concrete call site, so any
//! `ScanSource`-backed source (including the feature-gated owned ones) can drive a
//! production through it. The [`ParseSource`] impls hand each frozen-parity parse
//! root — the three documents, the executable/SDL standalone definitions, the
//! described standalones, and the seven extensions — to that runner over `str` and
//! `[u8]` (the [`ParseBytes`](crate::entry::ParseBytes) convenience covers
//! `bytes::Bytes` through the `[u8]` impls, exactly as the production test suites
//! drive their matrix).
//!
//! # Prefix semantics
//!
//! See [`entry`](crate::entry): a root parses a LEADING `Self`; the document roots
//! consume the whole source.

use smear_lexer::graphql::syntactic::SyntacticLexer;
use tokora::{FatalContext, Lexer, Parse, ParseInput, Parser};

use super::{
  definition::{
    arguments_definition, described_enum_type_definition, described_input_object_type_definition,
    described_interface_type_definition, described_object_type_definition, directive_definition,
    object_type_definition, schema_definition,
  },
  document::{document, type_system_document},
  executable::{executable_document, fragment_definition, operation_definition},
  extension::{
    enum_type_extension, input_object_type_extension, interface_type_extension,
    object_type_extension, scalar_type_extension, schema_extension,
  },
  selection::selection_set,
};
use crate::{
  combinator::SliceOf,
  entry::ParseSource,
  graphql::{
    ast::{
      ArgumentsDefinition, DescribedEnumTypeDefinition, DescribedInputObjectTypeDefinition,
      DescribedInterfaceTypeDefinition, DescribedObjectTypeDefinition, DirectiveDefinition,
      Document, EnumTypeExtension, ExecutableDocument, FragmentDefinition,
      InputObjectTypeExtension, InterfaceTypeExtension, ObjectTypeDefinition, ObjectTypeExtension,
      OperationDefinition, ScalarTypeExtension, SchemaDefinition, SchemaExtension, SelectionSet,
      TypeSystemDocument,
    },
    error::GraphqlErrors,
  },
};

/// The source slice a syntactic GraphQL parse of `S` yields.
pub type SyntacticSliceOf<'inp, S> = SliceOf<'inp, SyntacticLexer<'inp, S>>;

/// The dialect errors a syntactic GraphQL parse of `S` reports.
pub type SyntacticErrorsOf<'inp, S> = GraphqlErrors<SyntacticSliceOf<'inp, S>>;

/// The fail-fast context a syntactic GraphQL entry parse runs under.
pub type SyntacticCtx<'inp, S> =
  FatalContext<'inp, SyntacticLexer<'inp, S>, SyntacticErrorsOf<'inp, S>>;

/// Drives a production over [`SyntacticLexer`] under a fail-fast
/// [`FatalContext`], generic over the source representation.
///
/// This is the syntactic entry chokepoint: every [`ParseSource`] impl routes
/// through it, so the eventual `Lang`-marker pinning is a one-line change here.
/// The context/emitter obligations are carried by the final [`Parse`] bound and
/// discharge at each concrete call site, where the token, slice, and error types
/// all normalize.
pub fn run_parse_source<'inp, S, O, F>(
  f: F,
  source: &'inp S,
) -> Result<O, SyntacticErrorsOf<'inp, S>>
where
  S: tokora::Source<usize> + core::fmt::Debug + ?Sized,
  SyntacticLexer<'inp, S>: Lexer<
      'inp,
      Source = S,
      Offset = usize,
      Span = tokora::SimpleSpan,
      Token = smear_lexer::graphql::syntactic::SyntacticToken<
        <S as tokora::Source<usize>>::Slice<'inp>,
      >,
    >,
  <S as tokora::Source<usize>>::Slice<'inp>: Clone,
  <SyntacticLexer<'inp, S> as Lexer<'inp>>::State: Default,
  F: ParseInput<'inp, SyntacticLexer<'inp, S>, O, SyntacticCtx<'inp, S>>,
  Parser<F, SyntacticLexer<'inp, S>, O, SyntacticCtx<'inp, S>, SyntacticErrorsOf<'inp, S>>:
    Parse<'inp, SyntacticLexer<'inp, S>, O, SyntacticErrorsOf<'inp, S>>,
{
  // 0.3.1: switch to `with_parser_of::<…, GraphQL>` once tokora threads the
  // `Lang` marker through the `Parser` constructors (tokora#71); until then the
  // interim drive pins `Lang = ()`, exactly as every production test suite does.
  Parser::with_parser(f).parse(source)
}

/// Implements [`ParseSource`] for one parse root over `str` and `[u8]`, routing
/// through [`run_parse_source`]; the concrete source lets every production bound
/// normalize with no generic restatement. `bytes::Bytes` rides the `[u8]` impls
/// via [`ParseBytes`](crate::entry::ParseBytes)/`Bytes::as_ref`, exactly as the
/// production test suites drive their source matrix.
macro_rules! impl_parse_source {
  ($($ty:ident => $production:ident),+ $(,)?) => {$(
    impl<'inp> ParseSource<'inp, str> for $ty<&'inp str> {
      type Error = SyntacticErrorsOf<'inp, str>;

      #[inline]
      fn parse_source(source: &'inp str) -> Result<Self, Self::Error> {
        run_parse_source($production, source)
      }
    }

    impl<'inp> ParseSource<'inp, [u8]> for $ty<&'inp [u8]> {
      type Error = SyntacticErrorsOf<'inp, [u8]>;

      #[inline]
      fn parse_source(source: &'inp [u8]) -> Result<Self, Self::Error> {
        run_parse_source($production, source)
      }
    }
  )+};
}

impl_parse_source!(
  Document => document,
  TypeSystemDocument => type_system_document,
  ExecutableDocument => executable_document,
  SelectionSet => selection_set,
  OperationDefinition => operation_definition,
  FragmentDefinition => fragment_definition,
  DirectiveDefinition => directive_definition,
  SchemaDefinition => schema_definition,
  ObjectTypeDefinition => object_type_definition,
  ArgumentsDefinition => arguments_definition,
  DescribedObjectTypeDefinition => described_object_type_definition,
  DescribedInterfaceTypeDefinition => described_interface_type_definition,
  DescribedEnumTypeDefinition => described_enum_type_definition,
  DescribedInputObjectTypeDefinition => described_input_object_type_definition,
  ObjectTypeExtension => object_type_extension,
  InterfaceTypeExtension => interface_type_extension,
  EnumTypeExtension => enum_type_extension,
  InputObjectTypeExtension => input_object_type_extension,
  ScalarTypeExtension => scalar_type_extension,
  SchemaExtension => schema_extension,
);

#[cfg(test)]
mod tests;
