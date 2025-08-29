
macro_rules! define_module {
  ($($filename:ident),+$(,)?) => {
    $(
      paste::paste! {
        #[path = "ok/" $filename ".rs"]
        mod $filename;
      }
    )*
  };
}

define_module!(
  ok_0001_input_type_definition_without_input_values,
  ok_0002_selection_simple,
  ok_0003_selection_with_fields,
  ok_0004_selection_with_fields_aliases_arguments,
  ok_0005_selection_with_inline_fragments,
  ok_0006_selection_with_fragment_spread,
  ok_0007_directive_definition,
  ok_0008_directive_definition_with_arguments,
  ok_0009_directive_definition_repeatable,
  ok_0010_enum_type_definition,
  ok_0011_enum_type_extension,
  ok_0012_fragment_definition,
  ok_0013_fragment_definition_with_fragment_spread,
  ok_0014_input_definition,
  ok_0015_input_extension,
  ok_0016_interface_definition,
  ok_0017_interface_extension,
  ok_0018_object_type_definition,
  ok_0019_object_type_extension,
  ok_0020_operation_type_definition,
  ok_0021_operation_type_definition_with_arguments,
  ok_0022_operation_type_definition_with_arguments_and_directives,
  ok_0023_scalar_definition,
  ok_0024_scalar_extension,
  ok_0025_schema_definition,
  ok_0026_schema_extension,
  ok_0027_union_type_definition,
  ok_0028_union_type_definition_followed_by_object_definition,
  ok_0029_union_type_extension,
  ok_0030_values,
  ok_0031_variables_with_default,
  ok_0032_supergraph,
  ok_0033_directive_on_argument_definition,
  ok_0034_query_shorthand_followed_by_fragment_definition,
  ok_0035_query_with_variables,
  ok_0036_parses_variable_definition_with_list_type,
  ok_0037_operation_type_definition_with_inline_fragment,
  ok_0038_wrapped_named_types,
  ok_0039_variable_with_directives,
  ok_0040_type_token_order,
  ok_0041_implements_list,
  ok_0042_object_type_definition_without_fields,
  ok_0043_interface_type_definition_without_fields,
  ok_0044_parse_services,
  ok_0045_parse_executables,
  ok_0046_parse_queries,
  ok_0047_parse_schemas,
);

