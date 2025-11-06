pub use string::*;
pub use traits::*;

mod string;
mod traits;

pub(crate) trait Wrapper {
  type Underlying;

  fn from_underlying(underlying: Self::Underlying) -> Self;
}
