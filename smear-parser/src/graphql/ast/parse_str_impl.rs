//! Implementation of [`ParseStr`] for all GraphQL AST types.
//!
//! Uses the generic combinator parsers from [`super::combinators`].

use smear_lexer::tokit::Parse;

use super::{
  ParseStr, SyntacticTokenErrors, combinators::*, default::*, fragment::*, run_parse_str,
  type_system::*,
};

macro_rules! impl_parse_str {
  ($ty:ty, $parse_fn:ident) => {
    impl<'a> ParseStr<'a> for $ty {
      fn parse_str(input: &'a str) -> Result<Self, SyntacticTokenErrors<&'a str>> {
        run_parse_str($parse_fn, input)
      }
    }
  };
}

impl_parse_str!(Document<&'a str>, parse_document);
impl_parse_str!(TypeSystemDocument<&'a str>, parse_type_system_document);
impl_parse_str!(ExecutableDocument<&'a str>, parse_executable_document);
impl_parse_str!(SelectionSet<&'a str>, parse_selection_set);
impl_parse_str!(OperationDefinition<&'a str>, parse_operation_definition);
impl_parse_str!(FragmentDefinition<&'a str>, parse_fragment_definition);
impl_parse_str!(DirectiveDefinition<&'a str>, parse_directive_definition);
impl_parse_str!(SchemaDefinition<&'a str>, parse_schema_definition);
impl_parse_str!(ObjectTypeDefinition<&'a str>, parse_object_type_definition);
impl_parse_str!(ArgumentsDefinition<&'a str>, parse_arguments_definition);
impl_parse_str!(
  DescribedObjectTypeDefinition<&'a str>,
  parse_described_object_type_definition
);
impl_parse_str!(
  DescribedInterfaceTypeDefinition<&'a str>,
  parse_described_interface_type_definition
);
impl_parse_str!(
  DescribedEnumTypeDefinition<&'a str>,
  parse_described_enum_type_definition
);
impl_parse_str!(
  DescribedInputObjectTypeDefinition<&'a str>,
  parse_described_input_object_type_definition
);
impl_parse_str!(ObjectTypeExtension<&'a str>, parse_object_type_extension);
impl_parse_str!(
  InterfaceTypeExtension<&'a str>,
  parse_interface_type_extension
);
impl_parse_str!(EnumTypeExtension<&'a str>, parse_enum_type_extension);
impl_parse_str!(
  InputObjectTypeExtension<&'a str>,
  parse_input_object_type_extension
);
impl_parse_str!(ScalarTypeExtension<&'a str>, parse_scalar_type_extension);
impl_parse_str!(SchemaExtension<&'a str>, parse_schema_extension);
