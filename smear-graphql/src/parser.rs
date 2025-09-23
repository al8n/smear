pub use impls::*;

mod impls;

pub mod ast {
  pub use smear_parser::lang::{minized::Name, punctuator::*};
}

pub use logosky::Parseable;
