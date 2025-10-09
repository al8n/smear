super::token!(str_token<'a>(&'a str, char, str, str,));

#[cfg(feature = "hipstr")]
super::token!(hipstr_token<'a>(hipstr::HipStr<'a>, char, str, logosky::source::CustomSource<hipstr::HipStr<'static>>,));
