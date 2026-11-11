#![cfg(all(
  feature = "smol-bytes_0_2",
  any(feature = "graphql", feature = "graphqlx")
))]

//! Both dialects' syntactic lexers and parsers take smol-bytes' four types as sources.
//!
//! `shared::Bytes` and `compact::Bytes` are byte sources, `Utf8Bytes` and `compact::Utf8Bytes` are
//! text sources, and each is a `Source` of its own rather than a view the test took first. A
//! document must lex and parse through every one of them to what the same text gives as `&str`,
//! and a byte source must refuse the strings and comments that are not well-formed UTF-8 exactly
//! as `&[u8]` does, which is the rule `DelegateStringError::UTF8` selects (smear#246, smear#251).
//! A text source cannot hold such bytes, so only the byte forms are held to the refusal.
//!
//! What each cell compares is a projection both sides can produce: the kind and the span of every
//! item a lexer yields, and the span and the entry count of a parsed document. The token payloads
//! and the error values are compared by `tests/oracle.rs`, which drives the same sources over the
//! whole fixture corpus.

/// The kind and the span of every item a syntactic lexer yields, `None` for an item that is an
/// error.
macro_rules! lex_all {
  ($lexer:expr) => {{
    let mut lexer = $lexer;
    let mut items = Vec::new();
    while let Some(item) = lexer.lex() {
      let span = lexer.span();
      items.push((
        item.ok().map(|token| format!("{:?}", token.kind())),
        span.start(),
        span.end(),
      ));
    }
    items
  }};
}

/// The errors of the first item of a syntactic lexer, which must be a refusal.
macro_rules! refusal {
  ($lexer:expr) => {
    match $lexer.lex() {
      Some(Err(errors)) => errors,
      other => panic!("expected a refusal, got {other:?}"),
    }
  };
}

#[cfg(feature = "graphql")]
mod graphql {
  use smear::lexer::{graphql::syntactic::SyntacticLexer, tokora::Lexer as _};

  /// A document with every entry kind, and non-ASCII where a source type's text rule matters: a
  /// comment, a block-string description and inline strings.
  const DOC: &str = r#"# café — a comment
"""A block description, with a multi-byte dash — and 中文."""
type Query { user(id: ID!, tag: String = "naïve"): User @deprecated(reason: "é") }
query Q($v: Int = 3, $f: Float = 1.5e3) { user(id: "1") { ...F, id } }
fragment F on User { name }
extend type Query { other(list: [Int] = [1, 2, 3]): Int }
"#;

  /// An inline string, a block string and a comment that are not well-formed UTF-8.
  const ILL_FORMED: [&[u8]; 3] = [b"\"a\xffb\"", b"\"\"\"a\xffb\"\"\"", b"#a\xffb\nx"];

  #[test]
  fn the_four_texts_lex_like_str() {
    let want = lex_all!(SyntacticLexer::<str>::new(DOC));
    assert!(
      want.iter().all(|(kind, _, _)| kind.is_some()),
      "the document must lex without an error item: {want:?}"
    );
    assert!(want.len() > 50, "only {} items", want.len());

    macro_rules! lexes_like_str {
      ($source:ty, $label:literal) => {{
        let owned = <$source>::from(DOC);
        assert_eq!(
          lex_all!(SyntacticLexer::<$source>::new(&owned)),
          want,
          $label
        );
      }};
    }

    lexes_like_str!(smol_bytes_0_2::shared::Bytes, "shared Bytes");
    lexes_like_str!(smol_bytes_0_2::compact::Bytes, "compact Bytes");
    lexes_like_str!(smol_bytes_0_2::Utf8Bytes, "shared Utf8Bytes");
    lexes_like_str!(smol_bytes_0_2::compact::Utf8Bytes, "compact Utf8Bytes");
  }

  #[test]
  fn the_two_byte_texts_refuse_what_a_byte_slice_refuses() {
    for doc in ILL_FORMED {
      let want = refusal!(SyntacticLexer::<[u8]>::new(doc));

      let owned = smol_bytes_0_2::shared::Bytes::from(doc);
      assert_eq!(
        refusal!(SyntacticLexer::<smol_bytes_0_2::shared::Bytes>::new(&owned)),
        want,
        "shared Bytes: {}",
        doc.escape_ascii()
      );

      let owned = smol_bytes_0_2::compact::Bytes::from(doc);
      assert_eq!(
        refusal!(SyntacticLexer::<smol_bytes_0_2::compact::Bytes>::new(
          &owned
        )),
        want,
        "compact Bytes: {}",
        doc.escape_ascii()
      );
    }
  }

  #[cfg(feature = "parser")]
  mod parse {
    use smear::{
      lexer::tokora::{Parse as _, Parser},
      parser::graphql::{
        GraphQL,
        ast::Document,
        error::GraphqlErrors,
        syntactic::{GraphqlLexer, document},
      },
    };

    use super::DOC;

    /// The span and the entry count of the document the syntactic parser reads from `DOC` as the
    /// source type `$source`.
    macro_rules! outline {
      ($source:ty) => {{
        let owned = <$source>::from(DOC);
        let parsed = Parser::with_parser::<
          GraphqlLexer<'_, $source>,
          Document<$source>,
          GraphqlErrors<$source>,
          _,
          GraphQL,
        >(document)
        .parse(&owned)
        .expect("the document parses");
        (
          parsed.span().start(),
          parsed.span().end(),
          parsed.definitions().len(),
        )
      }};
    }

    #[test]
    fn the_four_texts_parse_like_str() {
      let parsed = Parser::with_parser::<
        GraphqlLexer<'_, str>,
        Document<&str>,
        GraphqlErrors<&str>,
        _,
        GraphQL,
      >(document)
      .parse_str(DOC)
      .expect("the document parses");
      let want = (
        parsed.span().start(),
        parsed.span().end(),
        parsed.definitions().len(),
      );
      assert_eq!(want.2, 4, "a type, a query, a fragment and an extension");

      assert_eq!(
        outline!(smol_bytes_0_2::shared::Bytes),
        want,
        "shared Bytes"
      );
      assert_eq!(
        outline!(smol_bytes_0_2::compact::Bytes),
        want,
        "compact Bytes"
      );
      assert_eq!(
        outline!(smol_bytes_0_2::Utf8Bytes),
        want,
        "shared Utf8Bytes"
      );
      assert_eq!(
        outline!(smol_bytes_0_2::compact::Utf8Bytes),
        want,
        "compact Utf8Bytes"
      );
    }
  }
}

#[cfg(feature = "graphqlx")]
mod graphqlx {
  use smear::lexer::{graphqlx::syntactic::SyntacticLexer, tokora::Lexer as _};

  /// A document with every entry kind, and non-ASCII where a source type's text rule matters: a
  /// comment and a block-string description.
  const DOC: &str = r#"# café — a comment
import { User } from "types.graphqlx"
"""A documented type — and 中文.""" type User<T> where T: Node { id: ID }
query GetUser { user { id } }
extend type User<String> { displayName: String }
"#;

  /// An inline string, a block string and a comment that are not well-formed UTF-8.
  const ILL_FORMED: [&[u8]; 3] = [b"\"a\xffb\"", b"\"\"\"a\xffb\"\"\"", b"#a\xffb\nx"];

  #[test]
  fn the_four_texts_lex_like_str() {
    let want = lex_all!(SyntacticLexer::<str>::new(DOC));
    assert!(
      want.iter().all(|(kind, _, _)| kind.is_some()),
      "the document must lex without an error item: {want:?}"
    );
    assert!(want.len() > 30, "only {} items", want.len());

    macro_rules! lexes_like_str {
      ($source:ty, $label:literal) => {{
        let owned = <$source>::from(DOC);
        assert_eq!(
          lex_all!(SyntacticLexer::<$source>::new(&owned)),
          want,
          $label
        );
      }};
    }

    lexes_like_str!(smol_bytes_0_2::shared::Bytes, "shared Bytes");
    lexes_like_str!(smol_bytes_0_2::compact::Bytes, "compact Bytes");
    lexes_like_str!(smol_bytes_0_2::Utf8Bytes, "shared Utf8Bytes");
    lexes_like_str!(smol_bytes_0_2::compact::Utf8Bytes, "compact Utf8Bytes");
  }

  #[test]
  fn the_two_byte_texts_refuse_what_a_byte_slice_refuses() {
    for doc in ILL_FORMED {
      let want = refusal!(SyntacticLexer::<[u8]>::new(doc));

      let owned = smol_bytes_0_2::shared::Bytes::from(doc);
      assert_eq!(
        refusal!(SyntacticLexer::<smol_bytes_0_2::shared::Bytes>::new(&owned)),
        want,
        "shared Bytes: {}",
        doc.escape_ascii()
      );

      let owned = smol_bytes_0_2::compact::Bytes::from(doc);
      assert_eq!(
        refusal!(SyntacticLexer::<smol_bytes_0_2::compact::Bytes>::new(
          &owned
        )),
        want,
        "compact Bytes: {}",
        doc.escape_ascii()
      );
    }
  }

  #[cfg(feature = "parser")]
  mod parse {
    use smear::{
      lexer::tokora::{Parse as _, Parser},
      parser::graphqlx::{
        GraphQLx,
        ast::Document,
        error::GraphqlxErrors,
        syntactic::{GraphqlxLexer, document},
      },
    };

    use super::DOC;

    /// The span and the entry count of the document the syntactic parser reads from `DOC` as the
    /// source type `$source`.
    macro_rules! outline {
      ($source:ty) => {{
        let owned = <$source>::from(DOC);
        let parsed = Parser::with_parser::<
          GraphqlxLexer<'_, $source>,
          Document<$source>,
          GraphqlxErrors<$source>,
          _,
          GraphQLx,
        >(document)
        .parse(&owned)
        .expect("the document parses");
        (
          parsed.span().start(),
          parsed.span().end(),
          parsed.definitions().len(),
        )
      }};
    }

    #[test]
    fn the_four_texts_parse_like_str() {
      let parsed = Parser::with_parser::<
        GraphqlxLexer<'_, str>,
        Document<&str>,
        GraphqlxErrors<&str>,
        _,
        GraphQLx,
      >(document)
      .parse_str(DOC)
      .expect("the document parses");
      let want = (
        parsed.span().start(),
        parsed.span().end(),
        parsed.definitions().len(),
      );
      assert_eq!(want.2, 4, "an import, a type, a query and an extension");

      assert_eq!(
        outline!(smol_bytes_0_2::shared::Bytes),
        want,
        "shared Bytes"
      );
      assert_eq!(
        outline!(smol_bytes_0_2::compact::Bytes),
        want,
        "compact Bytes"
      );
      assert_eq!(
        outline!(smol_bytes_0_2::Utf8Bytes),
        want,
        "shared Utf8Bytes"
      );
      assert_eq!(
        outline!(smol_bytes_0_2::compact::Utf8Bytes),
        want,
        "compact Utf8Bytes"
      );
    }
  }
}
