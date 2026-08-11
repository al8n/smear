super::token!(str_token<'a>(&'a str, char, str, true));

#[cfg(feature = "hipstr")]
super::token!(hipstr_token<'a>(hipstr::HipStr<'a>, char, str, true, &'a str));
