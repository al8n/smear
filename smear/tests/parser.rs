// Note: graphqlx ParseStr is not yet implemented (pending tokit migration of recursive-descent parser).
// GraphQL tests include graphqlx variants gated by #[cfg(feature = "graphqlx")], but those now use
// #[cfg(feature = "_graphqlx_parse_str")] to be disabled until the graphqlx parser is implemented.
#[cfg(feature = "graphql")]
#[path = "parser/graphql.rs"]
mod graphql;

// TODO: Re-enable when graphqlx ParseStr is implemented.
// #[cfg(feature = "graphqlx")]
// #[path = "parser/graphqlx.rs"]
// mod graphqlx;
