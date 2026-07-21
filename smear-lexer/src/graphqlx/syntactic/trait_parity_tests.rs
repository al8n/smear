//! End-to-end `Lexer`-trait parity for the GraphQLx SIMD lexer, mirroring
//! `graphql/syntactic/trait_parity_tests.rs`, driven over a diverse input set.
//!
//! Each input freezes the full-drive render a `logos::Lexer` over the
//! `SyntacticToken<S>` grammar yields — *every* observable at *every* step: the
//! `lex()` result, `span()`, and `slice()` at each token, then — the step the
//! narrower `error_parity_tests`/`bump_parity_tests` skip — `span()`/`slice()`
//! after EOF, and whether a post-EOF `bump` panics. The tests drive the SIMD
//! lexer over the same inputs and assert every observable against the frozen
//! render.
//!
//! Logos resets its span to `cursor..cursor` (EOF..EOF) once `next()` returns
//! `None`, including after trailing trivia/comments, so the frozen renders
//! capture that reset explicitly — it is the reason the SIMD layer must do the
//! same, and a stale span there would make its own post-EOF `bump` grow from
//! the wrong base (see the panic check in each test below).

use std::panic::{AssertUnwindSafe, catch_unwind};

use tokora::{Lexer, state::recursion_tracker::RecursionLimiter};

use crate::graphqlx::syntactic::SyntacticLexer;

/// Run `f`, returning `true` if it panicked, with the panic message suppressed
/// so an expected panic doesn't clutter test output.
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
/// punctuation (including the GraphQLx-only `::`/`=>`/`<>`), delegated
/// numbers/strings/block strings, comments, a leading BOM, trailing trivia,
/// whitespace-only, empty, and each malformed shape — paired with their
/// frozen `<str>`- and `<[u8]>`-sourced renders.
const INPUTS: &[(&str, &str, &str)] = &[
  (
    "a::b<K => V>(x: 1, y: \"s\") { z }",
    r#"#0 lex=Some(Ok(Identifier("a"))) span=SimpleSpan { start: 0, end: 1 } slice="a"
#1 lex=Some(Ok(PathSeparator)) span=SimpleSpan { start: 1, end: 3 } slice="::"
#2 lex=Some(Ok(Identifier("b"))) span=SimpleSpan { start: 3, end: 4 } slice="b"
#3 lex=Some(Ok(LAngle)) span=SimpleSpan { start: 4, end: 5 } slice="<"
#4 lex=Some(Ok(Identifier("K"))) span=SimpleSpan { start: 5, end: 6 } slice="K"
#5 lex=Some(Ok(FatArrow)) span=SimpleSpan { start: 7, end: 9 } slice="=>"
#6 lex=Some(Ok(Identifier("V"))) span=SimpleSpan { start: 10, end: 11 } slice="V"
#7 lex=Some(Ok(RAngle)) span=SimpleSpan { start: 11, end: 12 } slice=">"
#8 lex=Some(Ok(LParen)) span=SimpleSpan { start: 12, end: 13 } slice="("
#9 lex=Some(Ok(Identifier("x"))) span=SimpleSpan { start: 13, end: 14 } slice="x"
#10 lex=Some(Ok(Colon)) span=SimpleSpan { start: 14, end: 15 } slice=":"
#11 lex=Some(Ok(LitInt(Decimal("1")))) span=SimpleSpan { start: 16, end: 17 } slice="1"
#12 lex=Some(Ok(Identifier("y"))) span=SimpleSpan { start: 19, end: 20 } slice="y"
#13 lex=Some(Ok(Colon)) span=SimpleSpan { start: 20, end: 21 } slice=":"
#14 lex=Some(Ok(LitInlineStr(Plain(LitPlainStr { source: "\"s\"" })))) span=SimpleSpan { start: 22, end: 25 } slice="\"s\""
#15 lex=Some(Ok(RParen)) span=SimpleSpan { start: 25, end: 26 } slice=")"
#16 lex=Some(Ok(LBrace)) span=SimpleSpan { start: 27, end: 28 } slice="{"
#17 lex=Some(Ok(Identifier("z"))) span=SimpleSpan { start: 29, end: 30 } slice="z"
#18 lex=Some(Ok(RBrace)) span=SimpleSpan { start: 31, end: 32 } slice="}"
#19 lex=None span=SimpleSpan { start: 32, end: 32 } slice=""
"#,
    r#"#0 lex=Some(Ok(Identifier([97]))) span=SimpleSpan { start: 0, end: 1 } slice=[97]
#1 lex=Some(Ok(PathSeparator)) span=SimpleSpan { start: 1, end: 3 } slice=[58, 58]
#2 lex=Some(Ok(Identifier([98]))) span=SimpleSpan { start: 3, end: 4 } slice=[98]
#3 lex=Some(Ok(LAngle)) span=SimpleSpan { start: 4, end: 5 } slice=[60]
#4 lex=Some(Ok(Identifier([75]))) span=SimpleSpan { start: 5, end: 6 } slice=[75]
#5 lex=Some(Ok(FatArrow)) span=SimpleSpan { start: 7, end: 9 } slice=[61, 62]
#6 lex=Some(Ok(Identifier([86]))) span=SimpleSpan { start: 10, end: 11 } slice=[86]
#7 lex=Some(Ok(RAngle)) span=SimpleSpan { start: 11, end: 12 } slice=[62]
#8 lex=Some(Ok(LParen)) span=SimpleSpan { start: 12, end: 13 } slice=[40]
#9 lex=Some(Ok(Identifier([120]))) span=SimpleSpan { start: 13, end: 14 } slice=[120]
#10 lex=Some(Ok(Colon)) span=SimpleSpan { start: 14, end: 15 } slice=[58]
#11 lex=Some(Ok(LitInt(Decimal([49])))) span=SimpleSpan { start: 16, end: 17 } slice=[49]
#12 lex=Some(Ok(Identifier([121]))) span=SimpleSpan { start: 19, end: 20 } slice=[121]
#13 lex=Some(Ok(Colon)) span=SimpleSpan { start: 20, end: 21 } slice=[58]
#14 lex=Some(Ok(LitInlineStr(Plain(LitPlainStr { source: [34, 115, 34] })))) span=SimpleSpan { start: 22, end: 25 } slice=[34, 115, 34]
#15 lex=Some(Ok(RParen)) span=SimpleSpan { start: 25, end: 26 } slice=[41]
#16 lex=Some(Ok(LBrace)) span=SimpleSpan { start: 27, end: 28 } slice=[123]
#17 lex=Some(Ok(Identifier([122]))) span=SimpleSpan { start: 29, end: 30 } slice=[122]
#18 lex=Some(Ok(RBrace)) span=SimpleSpan { start: 31, end: 32 } slice=[125]
#19 lex=None span=SimpleSpan { start: 32, end: 32 } slice=[]
"#,
  ),
  (
    "\"\"\"doc\"\"\" type T { f(a: Int = -1): [U!]! }",
    r#"#0 lex=Some(Ok(LitBlockStr(Plain(LitPlainStr { source: "\"\"\"doc\"\"\"" })))) span=SimpleSpan { start: 0, end: 9 } slice="\"\"\"doc\"\"\""
#1 lex=Some(Ok(Identifier("type"))) span=SimpleSpan { start: 10, end: 14 } slice="type"
#2 lex=Some(Ok(Identifier("T"))) span=SimpleSpan { start: 15, end: 16 } slice="T"
#3 lex=Some(Ok(LBrace)) span=SimpleSpan { start: 17, end: 18 } slice="{"
#4 lex=Some(Ok(Identifier("f"))) span=SimpleSpan { start: 19, end: 20 } slice="f"
#5 lex=Some(Ok(LParen)) span=SimpleSpan { start: 20, end: 21 } slice="("
#6 lex=Some(Ok(Identifier("a"))) span=SimpleSpan { start: 21, end: 22 } slice="a"
#7 lex=Some(Ok(Colon)) span=SimpleSpan { start: 22, end: 23 } slice=":"
#8 lex=Some(Ok(Identifier("Int"))) span=SimpleSpan { start: 24, end: 27 } slice="Int"
#9 lex=Some(Ok(Equal)) span=SimpleSpan { start: 28, end: 29 } slice="="
#10 lex=Some(Ok(LitInt(Decimal("-1")))) span=SimpleSpan { start: 30, end: 32 } slice="-1"
#11 lex=Some(Ok(RParen)) span=SimpleSpan { start: 32, end: 33 } slice=")"
#12 lex=Some(Ok(Colon)) span=SimpleSpan { start: 33, end: 34 } slice=":"
#13 lex=Some(Ok(LBracket)) span=SimpleSpan { start: 35, end: 36 } slice="["
#14 lex=Some(Ok(Identifier("U"))) span=SimpleSpan { start: 36, end: 37 } slice="U"
#15 lex=Some(Ok(Bang)) span=SimpleSpan { start: 37, end: 38 } slice="!"
#16 lex=Some(Ok(RBracket)) span=SimpleSpan { start: 38, end: 39 } slice="]"
#17 lex=Some(Ok(Bang)) span=SimpleSpan { start: 39, end: 40 } slice="!"
#18 lex=Some(Ok(RBrace)) span=SimpleSpan { start: 41, end: 42 } slice="}"
#19 lex=None span=SimpleSpan { start: 42, end: 42 } slice=""
"#,
    r#"#0 lex=Some(Ok(LitBlockStr(Plain(LitPlainStr { source: [34, 34, 34, 100, 111, 99, 34, 34, 34] })))) span=SimpleSpan { start: 0, end: 9 } slice=[34, 34, 34, 100, 111, 99, 34, 34, 34]
#1 lex=Some(Ok(Identifier([116, 121, 112, 101]))) span=SimpleSpan { start: 10, end: 14 } slice=[116, 121, 112, 101]
#2 lex=Some(Ok(Identifier([84]))) span=SimpleSpan { start: 15, end: 16 } slice=[84]
#3 lex=Some(Ok(LBrace)) span=SimpleSpan { start: 17, end: 18 } slice=[123]
#4 lex=Some(Ok(Identifier([102]))) span=SimpleSpan { start: 19, end: 20 } slice=[102]
#5 lex=Some(Ok(LParen)) span=SimpleSpan { start: 20, end: 21 } slice=[40]
#6 lex=Some(Ok(Identifier([97]))) span=SimpleSpan { start: 21, end: 22 } slice=[97]
#7 lex=Some(Ok(Colon)) span=SimpleSpan { start: 22, end: 23 } slice=[58]
#8 lex=Some(Ok(Identifier([73, 110, 116]))) span=SimpleSpan { start: 24, end: 27 } slice=[73, 110, 116]
#9 lex=Some(Ok(Equal)) span=SimpleSpan { start: 28, end: 29 } slice=[61]
#10 lex=Some(Ok(LitInt(Decimal([45, 49])))) span=SimpleSpan { start: 30, end: 32 } slice=[45, 49]
#11 lex=Some(Ok(RParen)) span=SimpleSpan { start: 32, end: 33 } slice=[41]
#12 lex=Some(Ok(Colon)) span=SimpleSpan { start: 33, end: 34 } slice=[58]
#13 lex=Some(Ok(LBracket)) span=SimpleSpan { start: 35, end: 36 } slice=[91]
#14 lex=Some(Ok(Identifier([85]))) span=SimpleSpan { start: 36, end: 37 } slice=[85]
#15 lex=Some(Ok(Bang)) span=SimpleSpan { start: 37, end: 38 } slice=[33]
#16 lex=Some(Ok(RBracket)) span=SimpleSpan { start: 38, end: 39 } slice=[93]
#17 lex=Some(Ok(Bang)) span=SimpleSpan { start: 39, end: 40 } slice=[33]
#18 lex=Some(Ok(RBrace)) span=SimpleSpan { start: 41, end: 42 } slice=[125]
#19 lex=None span=SimpleSpan { start: 42, end: 42 } slice=[]
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
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 13 }, data: String(StringErrors([Unterminated(UnexpectedEnd { offset: 0, name: Some(CowStr { inner: "string value" }), hint: Quote, _lang: PhantomData<()> })])) }]))) span=SimpleSpan { start: 0, end: 13 } slice="\"unterminated"
#1 lex=None span=SimpleSpan { start: 13, end: 13 } slice=""
"#,
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 13 }, data: String(StringErrors([Unterminated(UnexpectedEnd { offset: 0, name: Some(CowStr { inner: "string value" }), hint: Quote, _lang: PhantomData<()> })])) }]))) span=SimpleSpan { start: 0, end: 13 } slice=[34, 117, 110, 116, 101, 114, 109, 105, 110, 97, 116, 101, 100]
#1 lex=None span=SimpleSpan { start: 13, end: 13 } slice=[]
"#,
  ),
  (
    "0xZZ",
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 4 }, data: Hex(UnexpectedSuffix(Range(SimpleSpan { start: 2, end: 4 }))) }]))) span=SimpleSpan { start: 0, end: 4 } slice="0xZZ"
#1 lex=None span=SimpleSpan { start: 4, end: 4 } slice=""
"#,
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 4 }, data: Hex(UnexpectedSuffix(Range(SimpleSpan { start: 2, end: 4 }))) }]))) span=SimpleSpan { start: 0, end: 4 } slice=[48, 120, 90, 90]
#1 lex=None span=SimpleSpan { start: 4, end: 4 } slice=[]
"#,
  ),
  (
    "1e",
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 2 }, data: Float(UnexpectedEnd(UnexpectedEnd { offset: 0, name: Some(CowStr { inner: "float" }), hint: Exponent(SignOrDigit), _lang: PhantomData<()> })) }]))) span=SimpleSpan { start: 0, end: 2 } slice="1e"
#1 lex=None span=SimpleSpan { start: 2, end: 2 } slice=""
"#,
    r#"#0 lex=Some(Err(LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 2 }, data: Float(UnexpectedEnd(UnexpectedEnd { offset: 0, name: Some(CowStr { inner: "float" }), hint: Exponent(SignOrDigit), _lang: PhantomData<()> })) }]))) span=SimpleSpan { start: 0, end: 2 } slice=[49, 101]
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

    // A drained lexer sits at span EOF..EOF (see the render above), so its end
    // equals the source length and a post-EOF `bump(1)` always lands past the
    // last byte — the boundary check must panic, for every input.
    let simd_panicked = panics(|| {
      let mut simd = SyntacticLexer::<str>::new(src);
      while simd.lex().is_some() {}
      simd.bump(&1usize);
    });
    assert!(simd_panicked, "post-EOF bump(1) must panic for {src:?}");
  }
}

#[test]
fn full_trait_parity_bytes() {
  // The same drive over the `<[u8]>` source, asserted against the
  // byte-flavored frozen renders.
  for (src, _, expected) in INPUTS {
    assert_eq!(
      &render_full!(SyntacticLexer::<[u8]>::new(src.as_bytes())),
      expected,
      "mismatch for {src:?}"
    );

    let simd_panicked = panics(|| {
      let mut simd = SyntacticLexer::<[u8]>::new(src.as_bytes());
      while simd.lex().is_some() {}
      simd.bump(&1usize);
    });
    assert!(simd_panicked, "post-EOF bump(1) must panic for {src:?}");
  }
}

#[test]
fn full_trait_parity_low_recursion_limit() {
  // Deep nesting past a low limit drives the over-limit region (recursion
  // errors in the token's place, plus the finish!/decrease paths) all the way
  // to EOF. This region is `(`/`)`-only, so it renders identically to the
  // GraphQL counterpart's; GraphQLx-specific recursion coverage over `<`/`>`
  // lives in `tests/oracle.rs`'s low-recursion parity test.
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
      RecursionLimiter::with_limitation(limit)
    )),
    expected
  );

  let simd_panicked = panics(|| {
    let mut simd = SyntacticLexer::<str>::with_state(src, RecursionLimiter::with_limitation(limit));
    while simd.lex().is_some() {}
    simd.bump(&1usize);
  });
  assert!(simd_panicked, "post-EOF bump(1) must panic for {src:?}");
}
