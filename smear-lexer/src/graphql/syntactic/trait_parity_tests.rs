//! End-to-end `Lexer`-trait parity for the SIMD lexer, driven over a diverse
//! input set.
//!
//! Each input freezes the full-drive render a `logos::Lexer` over the full
//! `SyntacticToken<S>` grammar yields — *every* observable at *every* step: the
//! `lex()` result, `span()`, and `slice()` at each token, then — the step the
//! narrower `error_parity_tests`/`bump_parity_tests` skip — `span()`/`slice()`
//! after EOF, and whether a post-EOF `bump` panics. The tests drive the SIMD
//! lexer over the same inputs and assert every observable against the frozen
//! render.
//!
//! Logos resets its span to `cursor..cursor` (EOF..EOF) once `next()` returns
//! `None`, including after trailing trivia/comments, so the frozen renders
//! capture that reset explicitly — it is the reason the SIMD layer must do
//! the same, and a stale span there would make its own post-EOF `bump` grow
//! from the wrong base (see the panic check in each test below).

#[cfg(feature = "std")]
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::{format, string::String};

use tokora::Lexer;

use crate::limits::SyntacticLimits;

use crate::graphql::syntactic::SyntacticLexer;

/// Run `f`, returning `true` if it panicked, with the panic message suppressed
/// so an expected panic doesn't clutter test output.
///
/// `std`-only. Observing a panic needs `catch_unwind` and the panic hook, and
/// both live in `std` because both need an unwinding runtime — `core`/`alloc`
/// have no equivalent. Only the post-EOF-`bump` probe in each test below is
/// gated on `std`; the full-drive render assertion — the bulk of what these
/// tests check — runs in both configurations.
#[cfg(feature = "std")]
fn panics<F: FnOnce()>(f: F) -> bool {
  let prev = std::panic::take_hook();
  std::panic::set_hook(Box::new(|_| {}));
  let caught = catch_unwind(AssertUnwindSafe(f));
  std::panic::set_hook(prev);
  caught.is_err()
}

/// Render a full drive: `lex()` (Debug), `span()`, and `slice()` at every
/// step — including the terminal step where `lex()` returns `None`, which is
/// where the EOF span/slice reset is observed.
macro_rules! render_full {
  ($lexer:expr) => {{
    let mut lex = $lexer;
    let mut out = String::new();
    let mut idx = 0usize;
    loop {
      let item = lex.lex();
      out.push_str(&format!(
        "#{idx} lex={:?} span={:?} slice={:?}\n",
        item,
        lex.span(),
        lex.slice()
      ));
      if item.is_none() {
        break;
      }
      idx += 1;
    }
    out
  }};
}

/// Inputs spanning every dispatch path — fast-path identifiers and
/// punctuation, delegated numbers/strings/block strings, comments, a leading
/// BOM, trailing trivia, whitespace-only, empty, and each malformed shape —
/// paired with their frozen `<str>`- and `<[u8]>`-sourced renders.
const INPUTS: &[(&str, &str, &str)] = &[
  (
    "{ user(id: 4) { name, ...Frag @skip(if: true) } }",
    r#"#0 lex=Some(Ok(LBrace)) span=SimpleSpan { start: 0, end: 1 } slice="{"
#1 lex=Some(Ok(Identifier("user"))) span=SimpleSpan { start: 2, end: 6 } slice="user"
#2 lex=Some(Ok(LParen)) span=SimpleSpan { start: 6, end: 7 } slice="("
#3 lex=Some(Ok(Identifier("id"))) span=SimpleSpan { start: 7, end: 9 } slice="id"
#4 lex=Some(Ok(Colon)) span=SimpleSpan { start: 9, end: 10 } slice=":"
#5 lex=Some(Ok(LitInt("4"))) span=SimpleSpan { start: 11, end: 12 } slice="4"
#6 lex=Some(Ok(RParen)) span=SimpleSpan { start: 12, end: 13 } slice=")"
#7 lex=Some(Ok(LBrace)) span=SimpleSpan { start: 14, end: 15 } slice="{"
#8 lex=Some(Ok(Identifier("name"))) span=SimpleSpan { start: 16, end: 20 } slice="name"
#9 lex=Some(Ok(Spread)) span=SimpleSpan { start: 22, end: 25 } slice="..."
#10 lex=Some(Ok(Identifier("Frag"))) span=SimpleSpan { start: 25, end: 29 } slice="Frag"
#11 lex=Some(Ok(At)) span=SimpleSpan { start: 30, end: 31 } slice="@"
#12 lex=Some(Ok(Identifier("skip"))) span=SimpleSpan { start: 31, end: 35 } slice="skip"
#13 lex=Some(Ok(LParen)) span=SimpleSpan { start: 35, end: 36 } slice="("
#14 lex=Some(Ok(Identifier("if"))) span=SimpleSpan { start: 36, end: 38 } slice="if"
#15 lex=Some(Ok(Colon)) span=SimpleSpan { start: 38, end: 39 } slice=":"
#16 lex=Some(Ok(Identifier("true"))) span=SimpleSpan { start: 40, end: 44 } slice="true"
#17 lex=Some(Ok(RParen)) span=SimpleSpan { start: 44, end: 45 } slice=")"
#18 lex=Some(Ok(RBrace)) span=SimpleSpan { start: 46, end: 47 } slice="}"
#19 lex=Some(Ok(RBrace)) span=SimpleSpan { start: 48, end: 49 } slice="}"
#20 lex=None span=SimpleSpan { start: 49, end: 49 } slice=""
"#,
    r#"#0 lex=Some(Ok(LBrace)) span=SimpleSpan { start: 0, end: 1 } slice=[123]
#1 lex=Some(Ok(Identifier([117, 115, 101, 114]))) span=SimpleSpan { start: 2, end: 6 } slice=[117, 115, 101, 114]
#2 lex=Some(Ok(LParen)) span=SimpleSpan { start: 6, end: 7 } slice=[40]
#3 lex=Some(Ok(Identifier([105, 100]))) span=SimpleSpan { start: 7, end: 9 } slice=[105, 100]
#4 lex=Some(Ok(Colon)) span=SimpleSpan { start: 9, end: 10 } slice=[58]
#5 lex=Some(Ok(LitInt([52]))) span=SimpleSpan { start: 11, end: 12 } slice=[52]
#6 lex=Some(Ok(RParen)) span=SimpleSpan { start: 12, end: 13 } slice=[41]
#7 lex=Some(Ok(LBrace)) span=SimpleSpan { start: 14, end: 15 } slice=[123]
#8 lex=Some(Ok(Identifier([110, 97, 109, 101]))) span=SimpleSpan { start: 16, end: 20 } slice=[110, 97, 109, 101]
#9 lex=Some(Ok(Spread)) span=SimpleSpan { start: 22, end: 25 } slice=[46, 46, 46]
#10 lex=Some(Ok(Identifier([70, 114, 97, 103]))) span=SimpleSpan { start: 25, end: 29 } slice=[70, 114, 97, 103]
#11 lex=Some(Ok(At)) span=SimpleSpan { start: 30, end: 31 } slice=[64]
#12 lex=Some(Ok(Identifier([115, 107, 105, 112]))) span=SimpleSpan { start: 31, end: 35 } slice=[115, 107, 105, 112]
#13 lex=Some(Ok(LParen)) span=SimpleSpan { start: 35, end: 36 } slice=[40]
#14 lex=Some(Ok(Identifier([105, 102]))) span=SimpleSpan { start: 36, end: 38 } slice=[105, 102]
#15 lex=Some(Ok(Colon)) span=SimpleSpan { start: 38, end: 39 } slice=[58]
#16 lex=Some(Ok(Identifier([116, 114, 117, 101]))) span=SimpleSpan { start: 40, end: 44 } slice=[116, 114, 117, 101]
#17 lex=Some(Ok(RParen)) span=SimpleSpan { start: 44, end: 45 } slice=[41]
#18 lex=Some(Ok(RBrace)) span=SimpleSpan { start: 46, end: 47 } slice=[125]
#19 lex=Some(Ok(RBrace)) span=SimpleSpan { start: 48, end: 49 } slice=[125]
#20 lex=None span=SimpleSpan { start: 49, end: 49 } slice=[]
"#,
  ),
  (
    "\"\"\"desc\"\"\" type Query { id: ID! name: String }",
    r#"#0 lex=Some(Ok(LitBlockStr(Plain(LitPlainStr { source: "\"\"\"desc\"\"\"" })))) span=SimpleSpan { start: 0, end: 10 } slice="\"\"\"desc\"\"\""
#1 lex=Some(Ok(Identifier("type"))) span=SimpleSpan { start: 11, end: 15 } slice="type"
#2 lex=Some(Ok(Identifier("Query"))) span=SimpleSpan { start: 16, end: 21 } slice="Query"
#3 lex=Some(Ok(LBrace)) span=SimpleSpan { start: 22, end: 23 } slice="{"
#4 lex=Some(Ok(Identifier("id"))) span=SimpleSpan { start: 24, end: 26 } slice="id"
#5 lex=Some(Ok(Colon)) span=SimpleSpan { start: 26, end: 27 } slice=":"
#6 lex=Some(Ok(Identifier("ID"))) span=SimpleSpan { start: 28, end: 30 } slice="ID"
#7 lex=Some(Ok(Bang)) span=SimpleSpan { start: 30, end: 31 } slice="!"
#8 lex=Some(Ok(Identifier("name"))) span=SimpleSpan { start: 32, end: 36 } slice="name"
#9 lex=Some(Ok(Colon)) span=SimpleSpan { start: 36, end: 37 } slice=":"
#10 lex=Some(Ok(Identifier("String"))) span=SimpleSpan { start: 38, end: 44 } slice="String"
#11 lex=Some(Ok(RBrace)) span=SimpleSpan { start: 45, end: 46 } slice="}"
#12 lex=None span=SimpleSpan { start: 46, end: 46 } slice=""
"#,
    r#"#0 lex=Some(Ok(LitBlockStr(Plain(LitPlainStr { source: [34, 34, 34, 100, 101, 115, 99, 34, 34, 34] })))) span=SimpleSpan { start: 0, end: 10 } slice=[34, 34, 34, 100, 101, 115, 99, 34, 34, 34]
#1 lex=Some(Ok(Identifier([116, 121, 112, 101]))) span=SimpleSpan { start: 11, end: 15 } slice=[116, 121, 112, 101]
#2 lex=Some(Ok(Identifier([81, 117, 101, 114, 121]))) span=SimpleSpan { start: 16, end: 21 } slice=[81, 117, 101, 114, 121]
#3 lex=Some(Ok(LBrace)) span=SimpleSpan { start: 22, end: 23 } slice=[123]
#4 lex=Some(Ok(Identifier([105, 100]))) span=SimpleSpan { start: 24, end: 26 } slice=[105, 100]
#5 lex=Some(Ok(Colon)) span=SimpleSpan { start: 26, end: 27 } slice=[58]
#6 lex=Some(Ok(Identifier([73, 68]))) span=SimpleSpan { start: 28, end: 30 } slice=[73, 68]
#7 lex=Some(Ok(Bang)) span=SimpleSpan { start: 30, end: 31 } slice=[33]
#8 lex=Some(Ok(Identifier([110, 97, 109, 101]))) span=SimpleSpan { start: 32, end: 36 } slice=[110, 97, 109, 101]
#9 lex=Some(Ok(Colon)) span=SimpleSpan { start: 36, end: 37 } slice=[58]
#10 lex=Some(Ok(Identifier([83, 116, 114, 105, 110, 103]))) span=SimpleSpan { start: 38, end: 44 } slice=[83, 116, 114, 105, 110, 103]
#11 lex=Some(Ok(RBrace)) span=SimpleSpan { start: 45, end: 46 } slice=[125]
#12 lex=None span=SimpleSpan { start: 46, end: 46 } slice=[]
"#,
  ),
  (
    "foo , \t\r\n  ",
    r#"#0 lex=Some(Ok(Identifier("foo"))) span=SimpleSpan { start: 0, end: 3 } slice="foo"
#1 lex=None span=SimpleSpan { start: 11, end: 11 } slice=""
"#,
    r#"#0 lex=Some(Ok(Identifier([102, 111, 111]))) span=SimpleSpan { start: 0, end: 3 } slice=[102, 111, 111]
#1 lex=None span=SimpleSpan { start: 11, end: 11 } slice=[]
"#,
  ),
  (
    "foo # trailing comment",
    r#"#0 lex=Some(Ok(Identifier("foo"))) span=SimpleSpan { start: 0, end: 3 } slice="foo"
#1 lex=None span=SimpleSpan { start: 22, end: 22 } slice=""
"#,
    r#"#0 lex=Some(Ok(Identifier([102, 111, 111]))) span=SimpleSpan { start: 0, end: 3 } slice=[102, 111, 111]
#1 lex=None span=SimpleSpan { start: 22, end: 22 } slice=[]
"#,
  ),
  (
    "\u{feff}foo",
    r#"#0 lex=Some(Ok(Identifier("foo"))) span=SimpleSpan { start: 3, end: 6 } slice="foo"
#1 lex=None span=SimpleSpan { start: 6, end: 6 } slice=""
"#,
    r#"#0 lex=Some(Ok(Identifier([102, 111, 111]))) span=SimpleSpan { start: 3, end: 6 } slice=[102, 111, 111]
#1 lex=None span=SimpleSpan { start: 6, end: 6 } slice=[]
"#,
  ),
  (
    "  \t\n , ",
    "#0 lex=None span=SimpleSpan { start: 7, end: 7 } slice=\"\"\n",
    "#0 lex=None span=SimpleSpan { start: 7, end: 7 } slice=[]\n",
  ),
  (
    "",
    "#0 lex=None span=SimpleSpan { start: 0, end: 0 } slice=\"\"\n",
    "#0 lex=None span=SimpleSpan { start: 0, end: 0 } slice=[]\n",
  ),
  (
    "\"unterminated",
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 13 }, data: String(StringErrors([Unterminated(UnexpectedEnd { offset: 0, name: Some(CowStr { inner: "string value" }), hint: Quote, expected: None, terminal: false, _lang: PhantomData<()> })])) }]))) span=SimpleSpan { start: 0, end: 13 } slice="\"unterminated"
#1 lex=None span=SimpleSpan { start: 13, end: 13 } slice=""
"#,
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 13 }, data: String(StringErrors([Unterminated(UnexpectedEnd { offset: 0, name: Some(CowStr { inner: "string value" }), hint: Quote, expected: None, terminal: false, _lang: PhantomData<()> })])) }]))) span=SimpleSpan { start: 0, end: 13 } slice=[34, 117, 110, 116, 101, 114, 109, 105, 110, 97, 116, 101, 100]
#1 lex=None span=SimpleSpan { start: 13, end: 13 } slice=[]
"#,
  ),
  (
    "007",
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 3 }, data: Int(LeadingZeros(Range(SimpleSpan { start: 0, end: 2 }))) }]))) span=SimpleSpan { start: 0, end: 3 } slice="007"
#1 lex=None span=SimpleSpan { start: 3, end: 3 } slice=""
"#,
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 3 }, data: Int(LeadingZeros(Range(SimpleSpan { start: 0, end: 2 }))) }]))) span=SimpleSpan { start: 0, end: 3 } slice=[48, 48, 55]
#1 lex=None span=SimpleSpan { start: 3, end: 3 } slice=[]
"#,
  ),
  (
    "1e",
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 2 }, data: Float(UnexpectedEnd(UnexpectedEnd { offset: 0, name: Some(CowStr { inner: "float" }), hint: Exponent(SignOrDigit), expected: None, terminal: false, _lang: PhantomData<()> })) }]))) span=SimpleSpan { start: 0, end: 2 } slice="1e"
#1 lex=None span=SimpleSpan { start: 2, end: 2 } slice=""
"#,
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 2 }, data: Float(UnexpectedEnd(UnexpectedEnd { offset: 0, name: Some(CowStr { inner: "float" }), hint: Exponent(SignOrDigit), expected: None, terminal: false, _lang: PhantomData<()> })) }]))) span=SimpleSpan { start: 0, end: 2 } slice=[49, 101]
#1 lex=None span=SimpleSpan { start: 2, end: 2 } slice=[]
"#,
  ),
  (
    "foo ? bar",
    r#"#0 lex=Some(Ok(Identifier("foo"))) span=SimpleSpan { start: 0, end: 3 } slice="foo"
#1 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 4, end: 5 }, data: UnknownLexeme(Char(PositionedChar { char: '?', position: 4 })) }]))) span=SimpleSpan { start: 4, end: 5 } slice="?"
#2 lex=Some(Ok(Identifier("bar"))) span=SimpleSpan { start: 6, end: 9 } slice="bar"
#3 lex=None span=SimpleSpan { start: 9, end: 9 } slice=""
"#,
    r#"#0 lex=Some(Ok(Identifier([102, 111, 111]))) span=SimpleSpan { start: 0, end: 3 } slice=[102, 111, 111]
#1 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 4, end: 5 }, data: UnknownLexeme(Char(PositionedChar { char: 63, position: 4 })) }]))) span=SimpleSpan { start: 4, end: 5 } slice=[63]
#2 lex=Some(Ok(Identifier([98, 97, 114]))) span=SimpleSpan { start: 6, end: 9 } slice=[98, 97, 114]
#3 lex=None span=SimpleSpan { start: 9, end: 9 } slice=[]
"#,
  ),
];

#[test]
fn full_trait_parity_str() {
  for (src, expected, _) in INPUTS {
    assert_eq!(
      &render_full!(SyntacticLexer::<str>::new(src)),
      expected,
      "mismatch for {src:?}"
    );

    // Frozen reference: a drained lexer sits at span EOF..EOF (see the render
    // above), so its end equals the source length and a post-EOF `bump(1)`
    // always lands past the last byte — logos always panicked on this
    // boundary check, for every input, when this ran against a live
    // comparator. `std`-only — see `panics`.
    #[cfg(feature = "std")]
    {
      let simd_panicked = panics(|| {
        let mut simd = SyntacticLexer::<str>::new(src);
        while simd.lex().is_some() {}
        simd.bump(&1usize);
      });
      assert!(simd_panicked, "post-EOF bump(1) must panic for {src:?}");
    }
  }
}

#[test]
fn full_trait_parity_bytes() {
  // The same drive over `<[u8]>` (SIMD) vs `<&[u8]>` (Logos): they share the
  // `SyntacticToken<&[u8]>` token, so every observable is directly comparable,
  // and the frozen renders below are the byte-side ones.
  for (src, _, expected) in INPUTS {
    assert_eq!(
      &render_full!(SyntacticLexer::<[u8]>::new(src.as_bytes())),
      expected,
      "mismatch for {src:?}"
    );

    // `std`-only — see `panics`.
    #[cfg(feature = "std")]
    {
      let simd_panicked = panics(|| {
        let mut simd = SyntacticLexer::<[u8]>::new(src.as_bytes());
        while simd.lex().is_some() {}
        simd.bump(&1usize);
      });
      assert!(simd_panicked, "post-EOF bump(1) must panic for {src:?}");
    }
  }
}

#[test]
fn full_trait_parity_low_recursion_limit() {
  // Deep nesting past a low limit drives the over-limit region (recursion
  // errors in the token's place, plus the finish!/decrease paths) all the way
  // to EOF. Frozen reference captured the same way as `INPUTS` above.
  let depth = 8;
  let big = "(".repeat(depth) + "x" + &")".repeat(depth);
  let src: &str = &big;
  let limit = 3;
  let expected = r#"#0 lex=Some(Ok(LParen)) span=SimpleSpan { start: 0, end: 1 } slice="("
#1 lex=Some(Ok(LParen)) span=SimpleSpan { start: 1, end: 2 } slice="("
#2 lex=Some(Ok(LParen)) span=SimpleSpan { start: 2, end: 3 } slice="("
#3 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 3, end: 4 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 4 })) }]))) span=SimpleSpan { start: 3, end: 4 } slice="("
#4 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 4, end: 5 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 5 })) }]))) span=SimpleSpan { start: 4, end: 5 } slice="("
#5 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 5, end: 6 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 6 })) }]))) span=SimpleSpan { start: 5, end: 6 } slice="("
#6 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 6, end: 7 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 7 })) }]))) span=SimpleSpan { start: 6, end: 7 } slice="("
#7 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 7, end: 8 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 8 })) }]))) span=SimpleSpan { start: 7, end: 8 } slice="("
#8 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 8 })) }]))) span=SimpleSpan { start: 8, end: 9 } slice="x"
#9 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 7 })) }]))) span=SimpleSpan { start: 9, end: 10 } slice=")"
#10 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 6 })) }]))) span=SimpleSpan { start: 10, end: 11 } slice=")"
#11 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 5 })) }]))) span=SimpleSpan { start: 11, end: 12 } slice=")"
#12 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 3, current: 4 })) }]))) span=SimpleSpan { start: 12, end: 13 } slice=")"
#13 lex=Some(Ok(RParen)) span=SimpleSpan { start: 13, end: 14 } slice=")"
#14 lex=Some(Ok(RParen)) span=SimpleSpan { start: 14, end: 15 } slice=")"
#15 lex=Some(Ok(RParen)) span=SimpleSpan { start: 15, end: 16 } slice=")"
#16 lex=Some(Ok(RParen)) span=SimpleSpan { start: 16, end: 17 } slice=")"
#17 lex=None span=SimpleSpan { start: 17, end: 17 } slice=""
"#;
  assert_eq!(
    render_full!(SyntacticLexer::<str>::with_state(
      src,
      SyntacticLimits::with_max_nesting_depth(limit)
    )),
    expected
  );

  // `std`-only — see `panics`.
  #[cfg(feature = "std")]
  {
    let simd_panicked = panics(|| {
      let mut simd =
        SyntacticLexer::<str>::with_state(src, SyntacticLimits::with_max_nesting_depth(limit));
      while simd.lex().is_some() {}
      simd.bump(&1usize);
    });
    assert!(simd_panicked, "post-EOF bump(1) must panic for {src:?}");
  }
}
