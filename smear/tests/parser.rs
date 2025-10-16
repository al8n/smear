#[cfg(any(feature = "graphql", feature = "graphqlx"))]
#[path = "parser/graphql.rs"]
mod graphql;

#[cfg(feature = "graphqlx")]
#[path = "parser/graphqlx.rs"]
mod graphqlx;
