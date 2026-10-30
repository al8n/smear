// `br"#[^\n\r]*?"` is the `Comment` rule's pattern over a byte source. The rule matches the whole
// comment before its hook runs, so the token is the whole comment even when the token ceiling
// refuses it. A byte-string literal compiles without Unicode, so the class takes every byte but
// `\n` and `\r`, and an ill-formed body is still one comment for the callback to refuse. It stays
// non-greedy: logos refuses an unbounded greedy repetition of a class that is all but a line
// terminator, and its DFA matches longest either way.

super::token!(slice_token<'a>(&'a [u8], u8, slice, false, br"#[^\n\r]*?"));

#[cfg(feature = "bytes")]
super::token!(bytes_token(bytes::Bytes, u8, slice, false, br"#[^\n\r]*?"));

#[cfg(feature = "hipstr")]
super::token!(hipstr_token<'a>(hipstr::HipByt<'a>, u8, slice, false, br"#[^\n\r]*?", &'a [u8]));
