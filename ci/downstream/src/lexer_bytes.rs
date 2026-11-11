//! `smear/bytes_1` claims to gate the `bytes_1::Bytes` source integration.
//!
//! A TRAIT IMPL, not a path, and that is why it is here: an impl is not namespaced, so no facade
//! module could ever gate it. Only the equivalence can.
pub fn gated() {
  fn is_source<S: smear::lexer::tokora::Source<usize>>() {}
  is_source::<bytes_1::Bytes>();
}
