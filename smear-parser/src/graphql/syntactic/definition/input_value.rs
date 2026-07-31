//! SDL input-value definition parsing.

use super::*;

definition_parser!(
  /// Parses an SDL input value definition.
  ///
  /// See the [GraphQL Input Value Definition specification](https://spec.graphql.org/draft/#InputValueDefinition).
  pub input_value_definition,
  inp,
  InputValueDefinition<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let cursor = *inp.cursor();
    let description = description(inp)?;
    let name = take_name(inp)?;
    take_colon(inp)?;
    let ty = take_type(inp)?;
    let default_value = default_value(inp)?;
    let directives = optional_const_directives(inp)?;
    let span = inp.span_since(&cursor);
    Ok(Described::new(
      span,
      description,
      InputValueDefinitionCore::new(span, name, ty, default_value, directives),
    ))
  }
);

impl_definition_api!(
  /// Parses an SDL input value definition.
  ///
  /// See the [GraphQL Input Value Definition specification](https://spec.graphql.org/draft/#InputValueDefinition).
  S,
  InputValueDefinition<S>,
  input_value_definition,
  [contextual]
);
