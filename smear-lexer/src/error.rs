pub use string::*;
pub use traits::*;

mod string;
mod traits;

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) trait Wrapper {
  type Underlying;

  fn from_underlying(underlying: Self::Underlying) -> Self;
}
