//! Punctuation tokens used in GraphQL and GraphQLx.
//!
//! These are type aliases over [`tokora::punct`] types, parameterised with
//! [`SimpleSpan`](tokora::SimpleSpan) so they carry positional information
//! without source content.

use crate::__private::tokora;

/// The `@` punctuator.
pub type At = tokora::punct::At<tokora::SimpleSpan>;

/// The `&` punctuator.
pub type Ampersand = tokora::punct::Ampersand<tokora::SimpleSpan>;

/// The `*` punctuator.
pub type Asterisk = tokora::punct::Asterisk<tokora::SimpleSpan>;

/// The `!` punctuator.
pub type Bang = tokora::punct::Exclamation<tokora::SimpleSpan>;

/// The `,` punctuator.
pub type Comma = tokora::punct::Comma<tokora::SimpleSpan>;

/// The `:` punctuator.
pub type Colon = tokora::punct::Colon<tokora::SimpleSpan>;

/// The `$` punctuator.
pub type Dollar = tokora::punct::Dollar<tokora::SimpleSpan>;

/// The `=` punctuator.
pub type Equal = tokora::punct::Equal<tokora::SimpleSpan>;

/// The `|` punctuator.
pub type Pipe = tokora::punct::Pipe<tokora::SimpleSpan>;

/// The `...` punctuator.
pub type Spread = tokora::punct::Spread<tokora::SimpleSpan>;

/// The `[` punctuator.
pub type LBracket = tokora::punct::OpenBracket<tokora::SimpleSpan>;

/// The `]` punctuator.
pub type RBracket = tokora::punct::CloseBracket<tokora::SimpleSpan>;

/// The `{` punctuator.
pub type LBrace = tokora::punct::OpenBrace<tokora::SimpleSpan>;

/// The `}` punctuator.
pub type RBrace = tokora::punct::CloseBrace<tokora::SimpleSpan>;

/// The `(` punctuator.
pub type LParen = tokora::punct::OpenParen<tokora::SimpleSpan>;

/// The `)` punctuator.
pub type RParen = tokora::punct::CloseParen<tokora::SimpleSpan>;

/// The `<` punctuator.
pub type LAngle = tokora::punct::OpenAngle<tokora::SimpleSpan>;

/// The `>` punctuator.
pub type RAngle = tokora::punct::CloseAngle<tokora::SimpleSpan>;

/// The `=>` punctuator.
pub type FatArrow = tokora::punct::FatArrow<tokora::SimpleSpan>;

/// The `->` punctuator.
pub type ThinArrow = tokora::punct::Arrow<tokora::SimpleSpan>;

/// The `::` punctuator.
pub type PathSeparator = tokora::punct::DoubleColon<tokora::SimpleSpan>;
