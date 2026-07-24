//! GraphQL contextual-keyword classification.

use tokora::utils::DowncastRef;

use super::{lossless::LosslessToken, syntactic::SyntacticToken};

/// A GraphQL keyword recognized from an identifier spelling.
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
}

impl ContextualKeyword {
  /// Returns the keyword's raw GraphQL spelling.
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
  use tokora::token::KeywordToken;

  use crate::graphql::syntactic::graphql_keyword;

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
  fn classifies_every_graphql_keyword() {
    const KEYWORDS: [ContextualKeyword; 19] = [
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
    ];

    for keyword in KEYWORDS {
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
  }

  #[cfg(feature = "bytes")]
  #[test]
  fn classifies_bytes() {
    assert_contextual_keyword(
      bytes::Bytes::from_static(b"query"),
      ContextualKeyword::Query,
    );
  }

  #[cfg(feature = "bstr")]
  #[test]
  fn classifies_bstr() {
    assert_contextual_keyword(bstr::BStr::new(b"query"), ContextualKeyword::Query);
  }

  #[cfg(feature = "hipstr")]
  #[test]
  fn classifies_hipstr_sources() {
    assert_contextual_keyword(hipstr::HipStr::from("query"), ContextualKeyword::Query);
    assert_contextual_keyword(
      hipstr::HipByt::from(b"query" as &[u8]),
      ContextualKeyword::Query,
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
      smol_bytes::compact::Bytes::from_static(b"query"),
      ContextualKeyword::Query,
    );
    assert_contextual_keyword(
      smol_bytes::Utf8Bytes::from_static("query"),
      ContextualKeyword::Query,
    );
    assert_contextual_keyword(
      smol_bytes::compact::Utf8Bytes::from_static("query"),
      ContextualKeyword::Query,
    );
  }
}
