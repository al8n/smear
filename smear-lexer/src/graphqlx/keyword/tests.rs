use super::*;

fn assert_contextual_keyword<S>(source: S, expected: ContextualKeyword)
where
  S: AsRef<[u8]> + Clone,
{
  assert_eq!(
    SyntacticToken::Identifier(source.clone()).downcast_ref(),
    Some(expected)
  );
  assert_eq!(
    LosslessToken::Identifier(source).downcast_ref(),
    Some(expected)
  );
}

#[test]
fn classifies_every_contextual_spelling() {
  const KEYWORDS: &[ContextualKeyword] = &[
    ContextualKeyword::Type,
    ContextualKeyword::Interface,
    ContextualKeyword::Union,
    ContextualKeyword::Enum,
    ContextualKeyword::Input,
    ContextualKeyword::Scalar,
    ContextualKeyword::Extend,
    ContextualKeyword::Schema,
    ContextualKeyword::Directive,
    ContextualKeyword::Fragment,
    ContextualKeyword::Query,
    ContextualKeyword::Mutation,
    ContextualKeyword::Subscription,
    ContextualKeyword::Implements,
    ContextualKeyword::Repeatable,
    ContextualKeyword::On,
    ContextualKeyword::True,
    ContextualKeyword::False,
    ContextualKeyword::Null,
    ContextualKeyword::QueryLocation,
    ContextualKeyword::MutationLocation,
    ContextualKeyword::SubscriptionLocation,
    ContextualKeyword::FieldLocation,
    ContextualKeyword::FragmentDefinitionLocation,
    ContextualKeyword::FragmentSpreadLocation,
    ContextualKeyword::InlineFragmentLocation,
    ContextualKeyword::VariableDefinitionLocation,
    ContextualKeyword::SchemaLocation,
    ContextualKeyword::ScalarLocation,
    ContextualKeyword::ObjectLocation,
    ContextualKeyword::FieldDefinitionLocation,
    ContextualKeyword::ArgumentDefinitionLocation,
    ContextualKeyword::InterfaceLocation,
    ContextualKeyword::UnionLocation,
    ContextualKeyword::EnumLocation,
    ContextualKeyword::EnumValueLocation,
    ContextualKeyword::InputObjectLocation,
    ContextualKeyword::InputFieldDefinitionLocation,
    ContextualKeyword::Import,
    ContextualKeyword::From,
    ContextualKeyword::As,
    ContextualKeyword::Where,
    ContextualKeyword::Set,
    ContextualKeyword::Map,
  ];

  for keyword in KEYWORDS {
    assert_contextual_keyword(keyword.as_str(), *keyword);
  }
}

#[test]
fn rejects_ordinary_identifiers_and_punctuators() {
  assert_eq!(
    SyntacticToken::Identifier("field").downcast_ref(),
    Option::<ContextualKeyword>::None
  );
  assert_eq!(
    LosslessToken::Identifier("field").downcast_ref(),
    Option::<ContextualKeyword>::None
  );
  assert_eq!(
    SyntacticToken::<&str>::LBrace.downcast_ref(),
    Option::<ContextualKeyword>::None
  );
  assert_eq!(
    LosslessToken::<&str>::LBrace.downcast_ref(),
    Option::<ContextualKeyword>::None
  );
}

#[test]
fn classifies_byte_slices() {
  assert_contextual_keyword::<&[u8]>(b"import", ContextualKeyword::Import);
  assert_contextual_keyword::<&[u8]>(b"FIELD", ContextualKeyword::FieldLocation);
}

#[cfg(feature = "bytes")]
#[test]
fn classifies_bytes() {
  assert_contextual_keyword(bytes::Bytes::from_static(b"map"), ContextualKeyword::Map);
}
