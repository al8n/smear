//! Punctuation tokens used in GraphQL and GraphQLx.
//!
//! These are type aliases over [`tokit::punct`] types, parameterised with
//! [`SimpleSpan`](tokit::SimpleSpan) so they carry positional information
//! without source content.

use crate::__private::tokit;

/// The `@` punctuator.
pub type At = tokit::punct::At<tokit::SimpleSpan>;

/// The `&` punctuator.
pub type Ampersand = tokit::punct::Ampersand<tokit::SimpleSpan>;

/// The `*` punctuator.
pub type Asterisk = tokit::punct::Asterisk<tokit::SimpleSpan>;

/// The `!` punctuator.
pub type Bang = tokit::punct::Exclamation<tokit::SimpleSpan>;

/// The `,` punctuator.
pub type Comma = tokit::punct::Comma<tokit::SimpleSpan>;

/// The `:` punctuator.
pub type Colon = tokit::punct::Colon<tokit::SimpleSpan>;

/// The `$` punctuator.
pub type Dollar = tokit::punct::Dollar<tokit::SimpleSpan>;

/// The `=` punctuator.
pub type Equal = tokit::punct::Equal<tokit::SimpleSpan>;

/// The `|` punctuator.
pub type Pipe = tokit::punct::Pipe<tokit::SimpleSpan>;

/// The `...` punctuator.
pub type Spread = tokit::punct::Spread<tokit::SimpleSpan>;

/// The `[` punctuator.
pub type LBracket = tokit::punct::OpenBracket<tokit::SimpleSpan>;

/// The `]` punctuator.
pub type RBracket = tokit::punct::CloseBracket<tokit::SimpleSpan>;

/// The `{` punctuator.
pub type LBrace = tokit::punct::OpenBrace<tokit::SimpleSpan>;

/// The `}` punctuator.
pub type RBrace = tokit::punct::CloseBrace<tokit::SimpleSpan>;

/// The `(` punctuator.
pub type LParen = tokit::punct::OpenParen<tokit::SimpleSpan>;

/// The `)` punctuator.
pub type RParen = tokit::punct::CloseParen<tokit::SimpleSpan>;

/// The `<` punctuator.
pub type LAngle = tokit::punct::OpenAngle<tokit::SimpleSpan>;

/// The `>` punctuator.
pub type RAngle = tokit::punct::CloseAngle<tokit::SimpleSpan>;

/// The `=>` punctuator.
pub type FatArrow = tokit::punct::FatArrow<tokit::SimpleSpan>;

/// The `->` punctuator.
pub type ThinArrow = tokit::punct::Arrow<tokit::SimpleSpan>;

/// The `::` punctuator.
pub type PathSeparator = tokit::punct::DoubleColon<tokit::SimpleSpan>;
