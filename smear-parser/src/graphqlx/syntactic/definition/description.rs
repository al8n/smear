//! GraphQLx SDL descriptions.

use super::*;

definition_parser!(
  /// Parses an optional GraphQLx SDL description.
  ///
  /// See the [GraphQL Description specification](https://spec.graphql.org/draft/#Description).
  pub description,
  inp,
  Option<StringValue<GraphqlxSlice<'inp, Src>>>,
  [],
  {
    Ok(match inp.try_expect_map_or_stop(|token| {
      matches!(
        token.data(),
        GraphqlxToken::<'inp, Src>::LitInlineStr(_) | GraphqlxToken::<'inp, Src>::LitBlockStr(_)
      )
      .then_some(())
    })? {
      Some(((), Spanned { span, data: GraphqlxToken::<'inp, Src>::LitInlineStr(value) })) => {
        Some(StringValue::new(span, value.into()))
      }
      Some(((), Spanned { span, data: GraphqlxToken::<'inp, Src>::LitBlockStr(value) })) => {
        Some(StringValue::new(span, value.into()))
      }
      Some(_) => unreachable!("description probe consumed a non-string token"),
      None => None,
    })
  }
);
