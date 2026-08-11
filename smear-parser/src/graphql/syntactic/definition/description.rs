//! SDL description parsing.

use super::*;

definition_parser!(
  /// Parses an optional SDL description.
  ///
  /// A non-string head is left available for the following committed production.
  /// See the [GraphQL Description specification](https://spec.graphql.org/draft/#Description).
  pub description,
  inp,
  Option<StringValue<GraphqlSlice<'inp, Src>>>,
  [],
  {
    match StringValue::try_graphql(inp)? {
      ParseAttempt::Accept(value) => Ok(Some(value)),
      ParseAttempt::Decline => Ok(None),
    }
  }
);
