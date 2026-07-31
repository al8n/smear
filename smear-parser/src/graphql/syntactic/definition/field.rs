//! SDL field-definition parsing.

use super::*;

definition_parser!(
  /// Parses an SDL field definition.
  ///
  /// See the [GraphQL Field Definition specification](https://spec.graphql.org/draft/#FieldDefinition).
  pub field_definition,
  inp,
  FieldDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let cursor = *inp.cursor();
    let description = description(inp)?;
    let name = take_name(inp)?;
    let arguments_definition = try_arguments_definition(inp)?.into();
    take_colon(inp)?;
    let ty = take_type(inp)?;
    let directives = optional_const_directives(inp)?;
    let span = inp.span_since(&cursor);
    Ok(Described::new(
      span,
      description,
      FieldDefinitionCore::new(span, name, arguments_definition, ty, directives),
    ))
  }
);

definition_parser!(
  /// Parses a nonempty SDL fields definition.
  ///
  /// See the [GraphQL Fields Definition specification](https://spec.graphql.org/draft/#FieldsDefinition).
  pub fields_definition,
  inp,
  FieldsDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    field_definition
      .repeated_while::<_, U1>(decide_brace_tail::<Src, Ctx>)
      .at_least(1)
      .delimited_by_braces()
      .collect_with(Vec::new())
      .spanned()
      .parse_input(inp)
      .map(|Spanned { span, data }| FieldsDefinition::new(span, data))
  }
);

definition_parser!(
  /// Attempts an SDL fields definition, declining when `{` is absent.
  pub try_fields_definition,
  inp,
  ParseAttempt<FieldsDefinition<GraphqlSlice<'inp, Src>>>,
  [contextual],
  {
    fields_definition
      .peek_then_try::<_, U1>(decide_lbrace_opener::<Src, Ctx>)
      .try_parse_input(inp)
  }
);

impl_definition_api!(
  /// Parses an SDL field definition.
  ///
  /// See the [GraphQL Field Definition specification](https://spec.graphql.org/draft/#FieldDefinition).
  S,
  FieldDefinition<S>,
  field_definition,
  [contextual]
);
impl_definition_api!(
  /// Parses a nonempty SDL fields definition.
  ///
  /// See the [GraphQL Fields Definition specification](https://spec.graphql.org/draft/#FieldsDefinition).
  S,
  FieldsDefinition<S>,
  fields_definition,
  [contextual]
);
impl_definition_try_api!(
  /// Attempts an SDL fields definition without consuming when `{` is absent.
  ///
  /// See the [GraphQL Fields Definition specification](https://spec.graphql.org/draft/#FieldsDefinition).
  S,
  FieldsDefinition<S>,
  try_fields_definition,
  [contextual]
);
