//! GraphQL contextual-keyword classification.

use tokora::utils::DowncastRef;

use super::{lossless::LosslessToken, syntactic::SyntacticToken};

/// A GraphQL contextual spelling recognized from an identifier.
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
    }
  }
}

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

/// Every contextual keyword this dialect declares, in declaration order.
///
/// An **array**, so the length is a compile-time pin: a variant added to the enum without a row
/// here does not compile. Hoisted out of `classifies_every_contextual_spelling` so
/// `crate::lexer::keyword_prefix` can compare it against GraphQLx's without either list being derived
/// from the other.
#[cfg(test)]
pub(crate) const KEYWORDS: [ContextualKeyword; 38] = [
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
];

#[cfg(test)]
impl ContextualKeyword {
  /// Asserts that [`KEYWORDS`] lists **every** variant this enum declares.
  ///
  /// **The list's length pins nothing on its own.** `KEYWORDS` is hand-written, so a variant added
  /// to the enum together with its `as_str` arm — which is what a real change looks like —
  /// compiles, classifies, and leaves every count in `crate::lexer::keyword_prefix` agreeing with itself.
  /// Task 7 measured exactly that: the mutation passed the whole suite.
  ///
  /// The `match` below is what closes it. It is exhaustive by construction, so the *next* variant
  /// is an `E0004` here, pointing at the one file that has to learn about it. The body then checks
  /// the weaker property the match cannot express — that the variant is actually in the array.
  fn assert_listed(self) {
    match self {
      Self::Type
      | Self::Interface
      | Self::Union
      | Self::Enum
      | Self::Input
      | Self::Scalar
      | Self::Extend
      | Self::Schema
      | Self::Directive
      | Self::Fragment
      | Self::Query
      | Self::Mutation
      | Self::Subscription
      | Self::Implements
      | Self::Repeatable
      | Self::On
      | Self::True
      | Self::False
      | Self::Null
      | Self::QueryLocation
      | Self::MutationLocation
      | Self::SubscriptionLocation
      | Self::FieldLocation
      | Self::FragmentDefinitionLocation
      | Self::FragmentSpreadLocation
      | Self::InlineFragmentLocation
      | Self::VariableDefinitionLocation
      | Self::SchemaLocation
      | Self::ScalarLocation
      | Self::ObjectLocation
      | Self::FieldDefinitionLocation
      | Self::ArgumentDefinitionLocation
      | Self::InterfaceLocation
      | Self::UnionLocation
      | Self::EnumLocation
      | Self::EnumValueLocation
      | Self::InputObjectLocation
      | Self::InputFieldDefinitionLocation => {}
    }
    assert!(
      KEYWORDS.contains(&self),
      "graphql's KEYWORDS is missing {self:?}"
    );
  }
}

#[cfg(test)]
#[test]
fn keywords_lists_every_variant() {
  for keyword in KEYWORDS {
    keyword.assert_listed();
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use tokora::token::KeywordToken;

  use crate::lexer::graphql::syntactic::graphql_keyword;

  fn assert_contextual_keyword<S>(source: S, expected: ContextualKeyword)
  where
    S: AsRef<[u8]> + Clone,
  {
    let syntactic = SyntacticToken::Identifier(source.clone());
    assert_eq!(syntactic.downcast_ref(), Some(expected));

    let lossless = LosslessToken::Identifier(source.clone());
    assert_eq!(lossless.downcast_ref(), Some(expected));
  }

  #[test]
  fn classifies_every_contextual_spelling() {
    for keyword in super::KEYWORDS {
      assert_contextual_keyword(keyword.as_str(), keyword);
    }
  }

  #[test]
  fn rejects_ordinary_identifiers() {
    let syntactic = SyntacticToken::Identifier("field");
    assert_eq!(syntactic.downcast_ref(), None);

    let lossless = LosslessToken::Identifier("field");
    assert_eq!(lossless.downcast_ref(), None);
  }

  #[test]
  fn distinguishes_lowercase_keywords_from_uppercase_locations() {
    const OVERLAPS: [(&str, ContextualKeyword, &str, ContextualKeyword); 8] = [
      (
        "query",
        ContextualKeyword::Query,
        "QUERY",
        ContextualKeyword::QueryLocation,
      ),
      (
        "mutation",
        ContextualKeyword::Mutation,
        "MUTATION",
        ContextualKeyword::MutationLocation,
      ),
      (
        "subscription",
        ContextualKeyword::Subscription,
        "SUBSCRIPTION",
        ContextualKeyword::SubscriptionLocation,
      ),
      (
        "schema",
        ContextualKeyword::Schema,
        "SCHEMA",
        ContextualKeyword::SchemaLocation,
      ),
      (
        "scalar",
        ContextualKeyword::Scalar,
        "SCALAR",
        ContextualKeyword::ScalarLocation,
      ),
      (
        "interface",
        ContextualKeyword::Interface,
        "INTERFACE",
        ContextualKeyword::InterfaceLocation,
      ),
      (
        "union",
        ContextualKeyword::Union,
        "UNION",
        ContextualKeyword::UnionLocation,
      ),
      (
        "enum",
        ContextualKeyword::Enum,
        "ENUM",
        ContextualKeyword::EnumLocation,
      ),
    ];

    for (keyword_source, keyword, location_source, location) in OVERLAPS {
      assert_contextual_keyword(keyword_source, keyword);
      assert_contextual_keyword(location_source, location);
    }
  }

  #[test]
  fn rejects_punctuators() {
    assert_eq!(
      SyntacticToken::<&str>::LBrace.downcast_ref(),
      Option::<ContextualKeyword>::None,
    );
    assert_eq!(
      LosslessToken::<&str>::LBrace.downcast_ref(),
      Option::<ContextualKeyword>::None,
    );
  }

  #[test]
  fn graphql_keyword_only_classifies_identifier_payloads() {
    let keyword = SyntacticToken::Identifier("fragment");
    assert_eq!(graphql_keyword(&keyword), Some("fragment"));
    assert_eq!(KeywordToken::keyword(&keyword), Some("fragment"));

    let identifier = SyntacticToken::Identifier("field");
    assert_eq!(graphql_keyword(&identifier), None);
    assert_eq!(KeywordToken::keyword(&identifier), None);

    let punctuator = SyntacticToken::<&str>::LBrace;
    assert_eq!(graphql_keyword(&punctuator), None);
    assert_eq!(KeywordToken::keyword(&punctuator), None);
  }

  #[test]
  fn classifies_byte_slices() {
    assert_contextual_keyword::<&[u8]>(b"query", ContextualKeyword::Query);
    assert_contextual_keyword::<&[u8]>(b"QUERY", ContextualKeyword::QueryLocation);
  }

  #[cfg(feature = "bytes")]
  #[test]
  fn classifies_bytes() {
    assert_contextual_keyword(
      bytes::Bytes::from_static(b"query"),
      ContextualKeyword::Query,
    );
    assert_contextual_keyword(
      bytes::Bytes::from_static(b"QUERY"),
      ContextualKeyword::QueryLocation,
    );
  }

  #[cfg(feature = "bstr")]
  #[test]
  fn classifies_bstr() {
    assert_contextual_keyword(bstr::BStr::new(b"query"), ContextualKeyword::Query);
    assert_contextual_keyword(bstr::BStr::new(b"QUERY"), ContextualKeyword::QueryLocation);
  }

  #[cfg(feature = "hipstr")]
  #[test]
  fn classifies_hipstr_sources() {
    assert_contextual_keyword(hipstr::HipStr::from("query"), ContextualKeyword::Query);
    assert_contextual_keyword(
      hipstr::HipStr::from("QUERY"),
      ContextualKeyword::QueryLocation,
    );
    assert_contextual_keyword(
      hipstr::HipByt::from(b"query" as &[u8]),
      ContextualKeyword::Query,
    );
    assert_contextual_keyword(
      hipstr::HipByt::from(b"QUERY" as &[u8]),
      ContextualKeyword::QueryLocation,
    );
  }

  #[cfg(feature = "smol-bytes")]
  #[test]
  fn classifies_smol_bytes_sources() {
    assert_contextual_keyword(
      smol_bytes::shared::Bytes::from_static(b"query"),
      ContextualKeyword::Query,
    );
    assert_contextual_keyword(
      smol_bytes::shared::Bytes::from_static(b"QUERY"),
      ContextualKeyword::QueryLocation,
    );
    assert_contextual_keyword(
      smol_bytes::compact::Bytes::from_static(b"query"),
      ContextualKeyword::Query,
    );
    assert_contextual_keyword(
      smol_bytes::compact::Bytes::from_static(b"QUERY"),
      ContextualKeyword::QueryLocation,
    );
    assert_contextual_keyword(
      smol_bytes::Utf8Bytes::from_static("query"),
      ContextualKeyword::Query,
    );
    assert_contextual_keyword(
      smol_bytes::Utf8Bytes::from_static("QUERY"),
      ContextualKeyword::QueryLocation,
    );
    assert_contextual_keyword(
      smol_bytes::compact::Utf8Bytes::from_static("query"),
      ContextualKeyword::Query,
    );
    assert_contextual_keyword(
      smol_bytes::compact::Utf8Bytes::from_static("QUERY"),
      ContextualKeyword::QueryLocation,
    );
  }
}
