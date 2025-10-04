super::token!(slice_token<'a>(&'a [u8], u8, slice, [u8], |val: &'a [u8]| val.iter().copied()));

#[cfg(feature = "bytes")]
super::token!(
  bytes_token(
    bytes::Bytes,
    u8,
    slice,
    logosky::source::CustomSource<bytes::Bytes>,
    super::iter_bytes_bytes,
  )
);

#[cfg(feature = "bytes")]
fn iter_bytes_bytes<'a>(s: &'a bytes::Bytes) -> impl Iterator<Item = u8> + 'a {
  s.iter().copied()
}

#[cfg(feature = "hipstr")]
super::token!(hipstr_token<'a>(hipstr::HipByt<'a>, u8, slice, logosky::source::CustomSource<hipstr::HipByt<'static>>, super::iter_hipbyt_bytes));

#[cfg(feature = "hipstr")]
fn iter_hipbyt_bytes<'a>(s: &'a hipstr::HipByt<'static>) -> impl Iterator<Item = u8> + 'a {
  s.iter().copied()
}
