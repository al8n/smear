/// Parser combinators for standard GraphQL
#[cfg(feature = "graphql")]
#[cfg_attr(docsrs, doc(cfg(feature = "graphql")))]
pub mod graphql;

/// Parser combinators for GraphQL extension
#[cfg(feature = "graphqlx")]
#[cfg_attr(docsrs, doc(cfg(feature = "graphqlx")))]
pub mod graphqlx;

/// common values
mod value;

/// The identifier
mod ident;
