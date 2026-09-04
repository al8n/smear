//! GraphQLx SDL definition productions.
//!
//! The module mirrors the standard GraphQL SDL surface while binding every
//! name-bearing position to GraphQLx paths and generic carriers.  Definition
//! and extension dispatch consume their contextual keyword once, then enter a
//! committed tail; none of the syntactic fast paths use transactions.

use std::vec::Vec;

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};
use tokora::{
  Accumulator, EmitterView, Lexer, ParseInput, ParseTokenChoice, SimpleSpan, Slice, Source, Token,
  TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  parser::Action,
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::{DowncastRef, typenum::U1},
};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken,
  generic::{definition_name, extension_name, try_where_clause, type_path},
  keyword_of,
  ty::ty,
  unexpected_here,
  value::default_value,
};
use crate::{
  combinator::{
    ParseCtx, TokenSpannedExt, extent_end, extent_since, extent_start, try_at, try_colon, try_equal,
  },
  graphqlx::{
    GraphQLx,
    ast::{
      ArgumentsDefinition, ConstDirectives, Constrained, Described, DescribedTypeSystemDefinition,
      DirectiveDefinition, DirectiveLocations, EnumTypeDefinition, EnumTypeExtension,
      EnumValueDefinition, EnumValuesDefinition, FieldDefinition, FieldsDefinition,
      ImplementInterfaces, InputFieldsDefinition, InputObjectTypeDefinition,
      InputObjectTypeExtension, InputValueDefinition, InterfaceTypeDefinition,
      InterfaceTypeExtension, Location, Name, ObjectTypeDefinition, ObjectTypeExtension,
      RootOperationTypeDefinition, RootOperationTypesDefinition, ScalarTypeDefinition,
      ScalarTypeExtension, SchemaDefinition, SchemaExtension, StringValue, TypeDefinition,
      TypeExtension, TypePath, TypeSystemDefinition, TypeSystemDefinitionOrExtension,
      TypeSystemDocument, TypeSystemExtension, UnionMemberTypes, UnionTypeDefinition,
      UnionTypeExtension, WhereClause,
    },
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
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
      [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
      $body
    );
  };
  (@impl $(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, [$($bounds:tt)*], $body:block) => {
    $(#[$meta])*
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
      $($bounds)*
      GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
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
      [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,]
    );
  };
  (@impl $(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident, [$($bounds:tt)*]) => {
    impl<$slice> $node {
      $(#[$meta])*
      /// The lexer source is inferred from `inp`.
      pub fn graphqlx<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp + $crate::value::Leaf,
        GraphqlxLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlxToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
        Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
        $($bounds)*
        GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<$slice>>,
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
  described_type_system_definition, import_or_type_system_definition_or_extension,
  type_system_definition, type_system_definition_or_extension, type_system_document,
};

#[cfg(test)]
mod tests;
