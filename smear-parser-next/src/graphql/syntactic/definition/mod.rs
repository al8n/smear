//! GraphQL SDL definition productions over the concrete syntactic lexer.
//!
//! These parsers follow the same rules as the executable and value suites: each
//! production is concrete over [`GraphqlLexer`], collection openers have a
//! declining `try_` entry point, and type-definition dispatch classifies its
//! contextual-keyword head once before entering a committed branch.

use std::vec::Vec;

use smear_lexer::graphql::{ContextualKeyword, syntactic::SyntacticTokenKind};
use tokora::{
  Accumulator, Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source, Token,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  punct::{Brace, Bracket, Paren, Pipe},
  span::{AsSpan, Spanned},
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use super::{
  GraphqlError, GraphqlInput, GraphqlLexer, GraphqlSlice, GraphqlToken,
  directive::const_directives, executable::operation_type, name as parse_name, ty::ty,
  value::default_value,
};
use crate::{
  combinator::{ParseCtx, at, colon, equal, try_equal},
  graphql::{
    GraphQL,
    ast::{
      ArgumentsDefinition, ConstDirectives, Described, DescribedTypeSystemDefinition,
      DirectiveDefinition, DirectiveLocations, EnumTypeDefinition, EnumTypeExtension,
      EnumValueDefinition, EnumValuesDefinition, FieldDefinition, FieldsDefinition,
      ImplementInterfaces, InputFieldsDefinition, InputObjectTypeDefinition,
      InputObjectTypeExtension, InputValueDefinition, InterfaceTypeDefinition,
      InterfaceTypeExtension, Location, Name, ObjectTypeDefinition, ObjectTypeExtension,
      RootOperationTypeDefinition, RootOperationTypesDefinition, ScalarTypeDefinition,
      ScalarTypeExtension, SchemaDefinition, SchemaExtension, StringValue, TypeDefinition,
      TypeExtension, TypeSystemDefinition, TypeSystemDefinitionOrExtension, TypeSystemDocument,
      TypeSystemExtension, UnionMemberTypes, UnionTypeDefinition, UnionTypeExtension,
    },
    error::{Expectation, GraphqlError as DialectGraphqlError},
    keyword::{try_implements as try_implements_keyword, try_repeatable as try_repeatable_keyword},
  },
  type_system::{
    EnumValueDefinition as EnumValueDefinitionCore, ExecutableDirectiveLocation,
    FieldDefinition as FieldDefinitionCore, InputValueDefinition as InputValueDefinitionCore,
    TypeSystemDirectiveLocation,
  },
};

macro_rules! definition_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [], $body:block) => {
    definition_parser!(@impl $(#[$meta])* $visibility $name, $input, $output, [], $body);
  };
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [contextual], $body:block) => {
    definition_parser!(
      @impl
      $(#[$meta])*
      $visibility $name,
      $input,
      $output,
      [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
      $body
    );
  };
  (@impl $(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [$($bounds:tt)*], $body:block) => {
    $(#[$meta])*
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
      $($bounds)*
      GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
        + From<
          UnexpectedToken<
            'inp,
            GraphqlToken<'inp, Src>,
            <GraphqlToken<'inp, Src> as Token<'inp>>::Kind,
            SimpleSpan,
            GraphQL,
          >,
        >
        + From<Unclosed<Paren, SimpleSpan, GraphQL>>
        + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
        + From<Unclosed<Brace, SimpleSpan, GraphQL>>
        + From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
    $body
  };
}

macro_rules! impl_definition_api {
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, []) => {
    impl_definition_api!(@impl $(#[$meta])* $slice, $node, $parser, []);
  };
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [contextual]) => {
    impl_definition_api!(
      @impl
      $(#[$meta])*
      $slice,
      $node,
      $parser,
      [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
    );
  };
  (@impl $(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [$($bounds:tt)*]) => {
    impl<$slice> $node {
      $(#[$meta])*
      ///
      /// The lexer source is inferred from `inp`.
      pub fn graphql<'inp, Src, Ctx>(
        inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp,
        GraphqlLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
        Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
        $($bounds)*
        GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
          + From<
            UnexpectedToken<
              'inp,
              GraphqlToken<'inp, Src>,
              <GraphqlToken<'inp, Src> as Token<'inp>>::Kind,
              SimpleSpan,
              GraphQL,
            >,
          >
          + From<Unclosed<Paren, SimpleSpan, GraphQL>>
          + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
          + From<Unclosed<Brace, SimpleSpan, GraphQL>>
          + From<DialectGraphqlError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

macro_rules! impl_definition_try_api {
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, []) => {
    impl_definition_try_api!(@impl $(#[$meta])* $slice, $node, $parser, []);
  };
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [contextual]) => {
    impl_definition_try_api!(
      @impl
      $(#[$meta])*
      $slice,
      $node,
      $parser,
      [GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
    );
  };
  (@impl $(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [$($bounds:tt)*]) => {
    impl<$slice> $node {
      $(#[$meta])*
      ///
      /// The lexer source is inferred from `inp`.
      pub fn try_graphql<'inp, Src, Ctx>(
        inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
      ) -> Result<ParseAttempt<Self>, GraphqlError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp,
        GraphqlLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
        Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
        $($bounds)*
        GraphqlError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
          + From<
            UnexpectedToken<
              'inp,
              GraphqlToken<'inp, Src>,
              <GraphqlToken<'inp, Src> as Token<'inp>>::Kind,
              SimpleSpan,
              GraphQL,
            >,
          >
          + From<Unclosed<Paren, SimpleSpan, GraphQL>>
          + From<Unclosed<Bracket, SimpleSpan, GraphQL>>
          + From<Unclosed<Brace, SimpleSpan, GraphQL>>
          + From<DialectGraphqlError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

mod support;
use support::*;

mod description;
pub use description::description;

mod input_value;
pub use input_value::input_value_definition;

mod arguments;
pub use arguments::{arguments_definition, try_arguments_definition};

mod field;
pub use field::{field_definition, fields_definition, try_fields_definition};

mod implements;
pub use implements::{implements, try_implements};

mod scalar;
pub use scalar::scalar_type_definition;

mod object;
pub use object::object_type_definition;

mod interface;
pub use interface::interface_type_definition;

mod union;
pub use union::{try_union_members, union_members, union_type_definition};

mod enum_type;
pub use enum_type::{
  enum_type_definition, enum_value_definition, enum_values_definition, try_enum_values_definition,
};

mod input_object;
pub use input_object::{
  input_fields_definition, input_object_type_definition, try_input_fields_definition,
};

mod directive;
pub use directive::{directive_definition, directive_locations, location};

mod schema;
pub use schema::{
  root_operation_type_definition, root_operation_types_definition, schema_definition,
};

mod type_definition;
pub use type_definition::{described_type_definition, type_definition};

mod extension;
pub(crate) use extension::type_system_extension_after_extend;
pub use extension::{
  enum_type_extension, input_object_type_extension, interface_type_extension,
  object_type_extension, scalar_type_extension, schema_extension, type_extension,
  type_system_extension, union_type_extension,
};

mod document;
pub(crate) use document::type_system_definition_after_keyword;
pub use document::{
  described_type_system_definition, type_system_definition, type_system_definition_or_extension,
  type_system_document,
};

#[cfg(test)]
mod tests;
