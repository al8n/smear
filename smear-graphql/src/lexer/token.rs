
/// Lossless GraphQL token
pub mod lossless;

/// Fast GraphQL token, skip [ignored tokens](https://spec.graphql.org/draft/#Ignored) directly while lexing.
pub mod fast;

// [ \t\f\u{FEFF}]+|#([^\n\r]*(\r\n|\r|\n))*
