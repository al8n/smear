super::token!(slice_token<'a>(&'a [u8], u8, slice, false));

#[cfg(feature = "bytes")]
super::token!(bytes_token(bytes::Bytes, u8, slice, false));

#[cfg(feature = "hipstr")]
super::token!(hipstr_token<'a>(hipstr::HipByt<'a>, u8, slice, false));
