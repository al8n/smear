super::token!(str_token<'a>(&'a str, char, str, str, str::chars));

#[cfg(feature = "hipstr")]
super::token!(hipstr_token<'a>(hipstr::HipStr<'a>, char, str, logosky::source::CustomSource<hipstr::HipStr<'static>>, super::iter_hipstr_chars));

#[cfg(feature = "hipstr")]
fn iter_hipstr_chars<'a>(s: &'a hipstr::HipStr<'static>) -> core::str::Chars<'a> {
  s.chars()
}
