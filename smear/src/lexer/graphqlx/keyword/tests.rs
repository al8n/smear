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
  for keyword in super::KEYWORDS {
    assert_contextual_keyword(keyword.as_str(), keyword);
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
