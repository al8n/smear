//! GraphQLx contextual-keyword classification.
//!
//! GraphQLx keeps GraphQL's soft-keyword model: lexer tokens remain identifiers
//! and this module classifies their spellings only where a grammar production
//! asks for a keyword.

use tokora::utils::DowncastRef;

use super::{lossless::LosslessToken, syntactic::SyntacticToken};

/// A GraphQLx contextual spelling recognized from an identifier.
///
/// This includes every standard GraphQL contextual spelling and directive
/// location, followed by the GraphQLx-only import and collection spellings.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ContextualKeyword {
  /// `type`
  Type,
  /// `interface`
  Interface,
  /// `union`
  Union,
  /// `enum`
  Enum,
  /// `input`
  Input,
  /// `scalar`
  Scalar,
  /// `extend`
  Extend,
  /// `schema`
  Schema,
  /// `directive`
  Directive,
  /// `fragment`
  Fragment,
  /// `query`
  Query,
  /// `mutation`
  Mutation,
  /// `subscription`
  Subscription,
  /// `implements`
  Implements,
  /// `repeatable`
  Repeatable,
  /// `on`
  On,
  /// `true`
  True,
  /// `false`
  False,
  /// `null`
  Null,
  /// `QUERY`
  QueryLocation,
  /// `MUTATION`
  MutationLocation,
  /// `SUBSCRIPTION`
  SubscriptionLocation,
  /// `FIELD`
  FieldLocation,
  /// `FRAGMENT_DEFINITION`
  FragmentDefinitionLocation,
  /// `FRAGMENT_SPREAD`
  FragmentSpreadLocation,
  /// `INLINE_FRAGMENT`
  InlineFragmentLocation,
  /// `VARIABLE_DEFINITION`
  VariableDefinitionLocation,
  /// `SCHEMA`
  SchemaLocation,
  /// `SCALAR`
  ScalarLocation,
  /// `OBJECT`
  ObjectLocation,
  /// `FIELD_DEFINITION`
  FieldDefinitionLocation,
  /// `ARGUMENT_DEFINITION`
  ArgumentDefinitionLocation,
  /// `INTERFACE`
  InterfaceLocation,
  /// `UNION`
  UnionLocation,
  /// `ENUM`
  EnumLocation,
  /// `ENUM_VALUE`
  EnumValueLocation,
  /// `INPUT_OBJECT`
  InputObjectLocation,
  /// `INPUT_FIELD_DEFINITION`
  InputFieldDefinitionLocation,
  /// `import`
  Import,
  /// `from`
  From,
  /// `as`
  As,
  /// `where`
  Where,
  /// `set`
  Set,
  /// `map`
  Map,
}

impl ContextualKeyword {
  /// Returns the contextual spelling.
  #[inline]
  pub const fn as_str(self) -> &'static str {
    match self {
      Self::Type => "type",
      Self::Interface => "interface",
      Self::Union => "union",
      Self::Enum => "enum",
      Self::Input => "input",
      Self::Scalar => "scalar",
      Self::Extend => "extend",
      Self::Schema => "schema",
      Self::Directive => "directive",
      Self::Fragment => "fragment",
      Self::Query => "query",
      Self::Mutation => "mutation",
      Self::Subscription => "subscription",
      Self::Implements => "implements",
      Self::Repeatable => "repeatable",
      Self::On => "on",
      Self::True => "true",
      Self::False => "false",
      Self::Null => "null",
      Self::QueryLocation => "QUERY",
      Self::MutationLocation => "MUTATION",
      Self::SubscriptionLocation => "SUBSCRIPTION",
      Self::FieldLocation => "FIELD",
      Self::FragmentDefinitionLocation => "FRAGMENT_DEFINITION",
      Self::FragmentSpreadLocation => "FRAGMENT_SPREAD",
      Self::InlineFragmentLocation => "INLINE_FRAGMENT",
      Self::VariableDefinitionLocation => "VARIABLE_DEFINITION",
      Self::SchemaLocation => "SCHEMA",
      Self::ScalarLocation => "SCALAR",
      Self::ObjectLocation => "OBJECT",
      Self::FieldDefinitionLocation => "FIELD_DEFINITION",
      Self::ArgumentDefinitionLocation => "ARGUMENT_DEFINITION",
      Self::InterfaceLocation => "INTERFACE",
      Self::UnionLocation => "UNION",
      Self::EnumLocation => "ENUM",
      Self::EnumValueLocation => "ENUM_VALUE",
      Self::InputObjectLocation => "INPUT_OBJECT",
      Self::InputFieldDefinitionLocation => "INPUT_FIELD_DEFINITION",
      Self::Import => "import",
      Self::From => "from",
      Self::As => "as",
      Self::Where => "where",
      Self::Set => "set",
      Self::Map => "map",
    }
  }
}

/// Classifies a spelling with one exact byte match.
#[inline]
fn contextual_keyword(source: &[u8]) -> Option<ContextualKeyword> {
  match source {
    b"type" => Some(ContextualKeyword::Type),
    b"interface" => Some(ContextualKeyword::Interface),
    b"union" => Some(ContextualKeyword::Union),
    b"enum" => Some(ContextualKeyword::Enum),
    b"input" => Some(ContextualKeyword::Input),
    b"scalar" => Some(ContextualKeyword::Scalar),
    b"extend" => Some(ContextualKeyword::Extend),
    b"schema" => Some(ContextualKeyword::Schema),
    b"directive" => Some(ContextualKeyword::Directive),
    b"fragment" => Some(ContextualKeyword::Fragment),
    b"query" => Some(ContextualKeyword::Query),
    b"mutation" => Some(ContextualKeyword::Mutation),
    b"subscription" => Some(ContextualKeyword::Subscription),
    b"implements" => Some(ContextualKeyword::Implements),
    b"repeatable" => Some(ContextualKeyword::Repeatable),
    b"on" => Some(ContextualKeyword::On),
    b"true" => Some(ContextualKeyword::True),
    b"false" => Some(ContextualKeyword::False),
    b"null" => Some(ContextualKeyword::Null),
    b"QUERY" => Some(ContextualKeyword::QueryLocation),
    b"MUTATION" => Some(ContextualKeyword::MutationLocation),
    b"SUBSCRIPTION" => Some(ContextualKeyword::SubscriptionLocation),
    b"FIELD" => Some(ContextualKeyword::FieldLocation),
    b"FRAGMENT_DEFINITION" => Some(ContextualKeyword::FragmentDefinitionLocation),
    b"FRAGMENT_SPREAD" => Some(ContextualKeyword::FragmentSpreadLocation),
    b"INLINE_FRAGMENT" => Some(ContextualKeyword::InlineFragmentLocation),
    b"VARIABLE_DEFINITION" => Some(ContextualKeyword::VariableDefinitionLocation),
    b"SCHEMA" => Some(ContextualKeyword::SchemaLocation),
    b"SCALAR" => Some(ContextualKeyword::ScalarLocation),
    b"OBJECT" => Some(ContextualKeyword::ObjectLocation),
    b"FIELD_DEFINITION" => Some(ContextualKeyword::FieldDefinitionLocation),
    b"ARGUMENT_DEFINITION" => Some(ContextualKeyword::ArgumentDefinitionLocation),
    b"INTERFACE" => Some(ContextualKeyword::InterfaceLocation),
    b"UNION" => Some(ContextualKeyword::UnionLocation),
    b"ENUM" => Some(ContextualKeyword::EnumLocation),
    b"ENUM_VALUE" => Some(ContextualKeyword::EnumValueLocation),
    b"INPUT_OBJECT" => Some(ContextualKeyword::InputObjectLocation),
    b"INPUT_FIELD_DEFINITION" => Some(ContextualKeyword::InputFieldDefinitionLocation),
    b"import" => Some(ContextualKeyword::Import),
    b"from" => Some(ContextualKeyword::From),
    b"as" => Some(ContextualKeyword::As),
    b"where" => Some(ContextualKeyword::Where),
    b"set" => Some(ContextualKeyword::Set),
    b"map" => Some(ContextualKeyword::Map),
    _ => None,
  }
}

impl<S> DowncastRef<ContextualKeyword> for SyntacticToken<S>
where
  S: AsRef<[u8]>,
{
  #[inline]
  fn downcast_ref(&self) -> Option<ContextualKeyword> {
    match self {
      Self::Identifier(source) => contextual_keyword(source.as_ref()),
      _ => None,
    }
  }
}

impl<S> DowncastRef<ContextualKeyword> for LosslessToken<S>
where
  S: AsRef<[u8]>,
{
  #[inline]
  fn downcast_ref(&self) -> Option<ContextualKeyword> {
    match self {
      Self::Identifier(source) => contextual_keyword(source.as_ref()),
      _ => None,
    }
  }
}

#[cfg(test)]
mod tests {
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
}
