#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]

mod db;
mod diagnostics;
pub use diagnostics::*;

pub mod directive {
  pub use smear_types::directive::*;
}
pub mod error {
  pub use smear_types::error::*;
}

pub mod utils {
  pub use smear_types::utils::*;
}
pub mod value {
  pub use smear_types::value::*;
}

pub use apollo_parser;
pub use codespan_reporting;
pub use derive_more;
pub use smear_derive::*;
pub use smear_types::Deprecated;

#[doc(hidden)]
pub mod __exports {
  pub use once_cell;
}
