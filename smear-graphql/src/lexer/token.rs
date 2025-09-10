/// Lossless GraphQL token, produce all tokens including [ignored tokens](https://spec.graphql.org/draft/#Ignored).
/// Use this if you need to preserve all original source information, e.g. for formatting or linting.
pub mod lossless;

/// Fast GraphQL token, skip [ignored tokens](https://spec.graphql.org/draft/#Ignored) directly while lexing,
/// and try to lex as much as possible in one pass.
pub mod fast;

mod handlers;
mod string_token;

#[cfg(test)]
mod tests;