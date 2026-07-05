//! Proves each `AsBytes` impl decodes to the right bytes. These do not drive a
//! full SIMD lexer over the type — the end-to-end source coverage lives in
//! `tests/oracle.rs`'s SIMD source matrix; here we only check the trait impls
//! are individually correct.

use super::AsBytes;

#[cfg(feature = "bytes")]
#[test]
fn bytes_as_bytes() {
  let b = bytes::Bytes::from_static(b"hello");
  assert_eq!(AsBytes::as_bytes(&b), b"hello");
}

#[cfg(feature = "bstr")]
#[test]
fn bstr_as_bytes() {
  let b = bstr::BStr::new(b"hello");
  assert_eq!(AsBytes::as_bytes(b), b"hello");
}

#[cfg(feature = "hipstr")]
#[test]
fn hipstr_as_bytes() {
  let s = hipstr::HipStr::from("hello");
  assert_eq!(AsBytes::as_bytes(&s), b"hello");
}

#[cfg(feature = "hipstr")]
#[test]
fn hipbyt_as_bytes() {
  let b = hipstr::HipByt::from(b"hello" as &[u8]);
  assert_eq!(AsBytes::as_bytes(&b), b"hello");
}
