super::token!(slice_token<'a>(&'a [u8], u8, slice, [u8], |val: &'a [u8]| val.iter().copied()));

// #[cfg(feature = "bytes")]
// super::token!(bytes_token(bytes::Bytes, u8, slice, logosky::source::CustomSource<bytes::Bytes>));

// #[cfg(feature = "hipstr")]
// super::token!(hipstr_token<'a>(hipstr::HipByt<'a>, u8, slice, logosky::source::CustomSource<hipstr::HipByt<'static>>));
