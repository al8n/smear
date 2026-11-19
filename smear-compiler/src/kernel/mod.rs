//! The algorithms smear's two semantic layers share: the GraphQL schema builder and the GraphQLx
//! analyzer.
//!
//! [`interner`] turns byte spellings into dense symbols, [`duplicates`] finds the earlier position
//! of a name list that wrote a given name, and [`scc`] finds which nodes of a directed graph lie on
//! a cycle and which strongly connected component each node is in. Each is generic over its
//! caller's symbol type, and all three need `std` or `alloc`.

pub mod duplicates;
pub mod interner;
pub mod scc;
