// `"#[^\n\r]*?"` is the `Comment` rule's pattern over a text source. The rule matches the whole
// comment before its hook runs, so the token is the whole comment even when the token ceiling
// refuses it. It stays non-greedy: logos refuses an unbounded greedy repetition of a class that is
// all but a line terminator, and its DFA matches longest either way.

super::token!(str_token<'a>(&'a str, char, str, true, "#[^\n\r]*?",));

#[cfg(feature = "hipstr")]
super::token!(hipstr_token<'a>(hipstr::HipStr<'a>, char, str, true, "#[^\n\r]*?", &'a str));
