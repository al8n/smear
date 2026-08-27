//! What the arena does at its ceilings, and what an ordinary build pays for the question.
//!
//! # Why the ceilings are injected rather than reached
//!
//! [`MAX_ARENA_BYTES`] is four gigabytes of interned bytes. No suite allocates that, so every cell
//! below drives [`Interner::intern_within`] — the whole mechanism, with the two ceilings as
//! parameters — at a ceiling it can stand on. [`Interner::intern`] adds only the two constants, and
//! `the_shipped_ceilings_are_the_widths_they_address` is what keeps those two numbers from drifting
//! away from the widths they are derived from.
//!
//! The unchecked arithmetic these replaced was measured at the real boundary, at `fcac941`, by
//! padding `strings` directly; the two rows are in the [`Interner`] header. They are not cells here
//! because a 4 GiB resize is not something a test run should do.

use smear_parser::graphql::ast::{
  ConstArgument, ConstArguments, ConstDirective, ConstDirectives, ConstList, Described, IntValue,
  Nested,
};

use super::*;

/// `type Query { ok: Int }` and friends, through the same door `Schema::build` uses.
fn parse(sdl: &'static str) -> TypeSystemDocument<&'static str> {
  Parser::with_parser::<
    GraphqlLexer<'static, str>,
    TypeSystemDocument<&'static str>,
    GraphqlErrors<&'static str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .expect("the fixture SDL parses")
}

/// The two shipped ceilings are the widths their derivations name.
///
/// A constant that drifts off the width it is derived from is the failure the derivation cannot
/// see: the guard still refuses, at a number that no longer means anything.
#[test]
fn the_shipped_ceilings_are_the_widths_they_address() {
  assert_eq!(MAX_ARENA_BYTES, u32::MAX, "a span's offsets are `u32`");
  assert_eq!(MAX_ARENA_SYMBOLS, u32::MAX, "a `Sym` is a `u32`");
}

/// Exactly at the byte ceiling a spelling is interned; one byte past it the arena refuses.
///
/// Both halves, because a ceiling that only ever refuses is indistinguishable from a prohibition —
/// and because the accepting half is what says the guard is `<=` rather than `<`.
#[test]
fn the_byte_ceiling_is_a_price_at_it_and_a_refusal_one_past_it() {
  let mut interner = Interner::default();
  let alpha = interner.intern_within(b"alpha", 8, MAX_ARENA_SYMBOLS);
  let beta = interner.intern_within(b"bet", 8, MAX_ARENA_SYMBOLS);

  assert_eq!(interner.bytes(alpha), b"alpha");
  assert_eq!(interner.bytes(beta), b"bet");
  assert_eq!(
    interner.spans[beta.get() as usize],
    (5, 8),
    "a spelling ending exactly at the ceiling is interned"
  );
  assert!(interner.refused.is_none());

  let past = interner.intern_within(b"c", 8, MAX_ARENA_SYMBOLS);
  assert_eq!(
    interner.refused,
    Some(past),
    "one byte past the ceiling is a refusal, and the arena says so"
  );
}

/// A refused spelling answers an empty one, and nothing that was already interned moves.
///
/// This is the differential against `fcac941`, at an injected ceiling instead of at four
/// gigabytes: the wrapped cast recorded `(4294967295, 3)` and `(0, 4)` there, so `bytes()` either
/// panicked on the inverted range or handed back a *different* name's bytes. Here the span is
/// never recorded at all.
#[test]
fn a_refused_spelling_is_empty_rather_than_a_slice_of_another_one() {
  let mut interner = Interner::default();
  let alpha = interner.intern_within(b"alpha", 5, MAX_ARENA_SYMBOLS);
  let refused = interner.intern_within(b"beta", 5, MAX_ARENA_SYMBOLS);

  assert_eq!(
    interner.bytes(refused),
    b"",
    "the placeholder spells nothing; wrapping spelled `alph`"
  );
  assert_eq!(
    interner.strings.len(),
    5,
    "a refusal grows the byte arena by nothing"
  );
  assert_eq!(
    interner.bytes(alpha),
    b"alpha",
    "and leaves every span already recorded where it was"
  );
  assert_eq!(
    interner.lookup(b"beta"),
    None,
    "a refused spelling is not in the arena, so nothing can find it there"
  );
}

/// However many spellings an arena turns away, it mints one placeholder.
#[test]
fn every_refusal_after_the_first_answers_the_same_symbol() {
  let mut interner = Interner::default();
  interner.intern_within(b"alpha", 5, MAX_ARENA_SYMBOLS);
  let first = interner.intern_within(b"beta", 5, MAX_ARENA_SYMBOLS);
  let spans = interner.spans.len();

  for spelling in [&b"gamma"[..], b"delta", b"epsilon"] {
    assert_eq!(
      interner.intern_within(spelling, 5, MAX_ARENA_SYMBOLS),
      first
    );
  }
  assert_eq!(
    interner.spans.len(),
    spans,
    "a refused arena stops growing rather than growing by a span per refusal"
  );
}

/// The symbol count is the other cast, and it refuses on its own ceiling.
///
/// Unreachable through [`Interner::intern`] — a spelling costs a byte, so [`MAX_ARENA_BYTES`] is
/// hit first — and guarded anyway, which is what this cell is for: the guard is a property of the
/// type rather than of the call sites it has today.
#[test]
fn the_symbol_ceiling_refuses_rather_than_wrapping_the_index() {
  let mut interner = Interner::default();
  let alpha = interner.intern_within(b"alpha", MAX_ARENA_BYTES, 2);
  let beta = interner.intern_within(b"beta", MAX_ARENA_BYTES, 2);
  assert_eq!(interner.bytes(alpha), b"alpha");
  assert_eq!(interner.bytes(beta), b"beta");
  assert!(interner.refused.is_none(), "two symbols is the ceiling");

  let past = interner.intern_within(b"gamma", MAX_ARENA_BYTES, 2);
  assert_eq!(interner.refused, Some(past));
  assert_eq!(interner.bytes(past), b"");
  assert_eq!(
    interner.strings.len(),
    9,
    "the refusal is asked before anything grows, so the bytes are not spent either"
  );
}

/// A build whose arena refused answers a typed refusal, not a wrong name and not a panic.
///
/// The document is ordinary and the ceiling is not: a real one takes 92 KB of overlapping suffixes
/// through the public `Name::new`, and what this pins is the answer rather than the route to it.
#[test]
fn a_refused_arena_is_a_typed_refusal() {
  let mut builder = SchemaBuilder::new();
  builder.document(&parse("type Query { ok: Int }"));
  // Zero bytes of room, which is the arena four gigabytes in.
  let refused = builder
    .interner
    .intern_within(b"Whatever", 0, MAX_ARENA_SYMBOLS);
  assert_eq!(builder.interner.bytes(refused), b"");

  let errors = builder
    .finish()
    .expect_err("an arena that refused a spelling is not a schema");
  assert_eq!(
    errors.kinds(),
    std::vec![SchemaErrorKind::TooManyInternedBytes],
    "the refusal is the only thing the build has to say: {errors}"
  );
}

/// A refusal ends the document rather than joining the diagnostics computed against it.
///
/// The document below is refused on its own merits before the arena stops growing, and the answer
/// is still the one error: a refused arena hands every later spelling the same placeholder, so
/// nothing a rule computes over it is about anything the caller wrote. Padding an arena to
/// `u32::MAX` under a one-type document produced fifteen duplicate-definition diagnostics from the
/// built-in injection alone.
#[test]
fn a_refused_arena_ends_the_document_rather_than_joining_its_diagnostics() {
  let mut builder = SchemaBuilder::new();
  builder.document(&parse("type Query { ok: Int }\ntype Query { ok: Int }"));
  assert!(
    !builder.errors.is_empty(),
    "the fixture is refused on its own merits, which is the point of it"
  );

  builder
    .interner
    .intern_within(b"Whatever", 0, MAX_ARENA_SYMBOLS);
  let errors = builder
    .finish()
    .expect_err("an arena that refused a spelling is not a schema");
  assert_eq!(
    errors.kinds(),
    std::vec![SchemaErrorKind::TooManyInternedBytes],
    "{errors}"
  );
}

/// The literal arena is the second one, and the guard is on the type, so it is covered too.
#[test]
fn the_literal_arena_refuses_on_the_same_guard() {
  let mut builder = SchemaBuilder::new();
  builder.document(&parse(
    "type Query { ok: Int }\nscalar Custom\ndirective @x(a: Custom = 42) on SCALAR",
  ));
  let refused = builder.literals.intern_within(b"43", 0, MAX_ARENA_SYMBOLS);
  assert_eq!(builder.literals.bytes(refused), b"");

  let errors = builder
    .finish()
    .expect_err("a literal arena that refused a spelling is not a schema either");
  assert_eq!(
    errors.kinds(),
    std::vec![SchemaErrorKind::TooManyInternedBytes],
    "{errors}"
  );
}

/// The control: an ordinary schema reaches neither ceiling and reads back exactly what it interned.
///
/// The guard is two comparisons on a path that already allocates a `Box<[u8]>` and inserts it into
/// a `BTreeMap`, so what this cell says is that the answer did not move; the cost is measured out
/// of suite and recorded in the pull request.
#[test]
fn an_ordinary_schema_reaches_neither_ceiling() {
  const SDL: &str = "type Query { hero(episode: Episode = NEWHOPE): Character }
     interface Character { name: String! friends: [Character] }
     type Human implements Character { name: String! friends: [Character] height: Float }
     enum Episode { NEWHOPE EMPIRE JEDI }
     input Filter { since: Int = 1977 name: String }";

  let mut builder = SchemaBuilder::new();
  builder.document(&parse(SDL));
  builder.document(&parse("scalar Custom"));

  for (at, &(start, end)) in builder.interner.spans.iter().enumerate() {
    let sym = Sym::new(at as u32);
    assert!(start <= end, "span {at} is inverted: {start}..{end}");
    assert_eq!(
      builder.interner.lookup(builder.interner.bytes(sym)),
      Some(sym),
      "symbol {at} does not read back as itself"
    );
  }
  assert!(builder.interner.refused.is_none());
  assert!(builder.literals.refused.is_none());

  let schema = builder.finish().expect("an ordinary schema builds");
  assert!(schema.type_by_name(b"Human").is_some());
  assert!(schema.type_by_name(b"Episode").is_some());
}

/// A *distinct* literal spelling does **not** cost its own source bytes, and this is the door.
///
/// # What this cell is for
///
/// Two review rounds rested on the opposite claim, which read: the three leaf constructors are
/// `pub(crate)`, so a caller's only route to one of these arms is a parse or a clone, so a distinct
/// spelling costs its own bytes in some source. The premise is true — `IntValue::new` is
/// `pub(crate)`, `FromComponents` is implemented in `smear-parser` only for `Name`, there is no
/// `DerefMut` — and the conclusion still does not follow, because a parse is not injective into the
/// bytes it reads. `IntValue::graphql` is a public associated parser, so `B` parses of the `B`
/// suffixes of one buffer yield `B` distinct spellings borrowing the same `B` bytes.
///
/// The claim had never had a cell in either direction. It has one now, and it fires the way the
/// claim said it could not: the arena is `B(B+1)/2` bytes over a `B`-byte buffer, exactly.
/// [`RawShape`] carries the same numbers as a table, and [`MAX_ARENA_BYTES`] is what makes the
/// growth harmless.
#[test]
fn a_distinct_literal_spelling_does_not_cost_its_own_source_bytes() {
  /// `scalar Foo @x(a: [<every suffix of `buffer`>])`, assembled rather than parsed.
  fn document(buffer: &str) -> TypeSystemDocument<&str> {
    let span = SimpleSpan::const_new(0, 0);
    let leaves = (0..buffer.len())
      .map(|at| {
        let spelling = &buffer[at..];
        let leaf = Parser::with_parser::<
          GraphqlLexer<'_, str>,
          IntValue<&str>,
          GraphqlErrors<&str>,
          _,
          GraphQL,
        >(IntValue::graphql)
        .parse_str(spelling)
        .expect("a run of nonzero digits is an IntValue");
        ConstInputValue::Int(leaf)
      })
      .collect::<Vec<_>>();
    let directive = ConstDirective::new(
      span,
      Name::new(span, "x"),
      Some(ConstArguments::new(
        span,
        vec![ConstArgument::new(
          span,
          Name::new(span, "a"),
          ConstInputValue::List(ConstList::new(span, Nested::new(leaves))),
        )],
      )),
    );
    TypeSystemDocument::new(
      span,
      vec![TypeSystemDefinitionOrExtension::Definition(Described::new(
        span,
        None,
        TypeSystemDefinition::Type(TypeDefinition::Scalar(ScalarTypeDefinition::new(
          span,
          Name::new(span, "Foo"),
          Some(ConstDirectives::new(span, vec![directive])),
        ))),
      ))],
    )
  }

  for width in [10usize, 30, 100, 300] {
    // Nonzero digits only: a leading zero is not an `IntValue`, so every suffix has to start with
    // one for the parse to succeed.
    let buffer: String = "123456789".chars().cycle().take(width).collect();
    let mut builder = SchemaBuilder::new();
    builder.document(&document(&buffer));

    assert_eq!(
      builder.literals.spans.len(),
      width,
      "every suffix is a distinct spelling, so none of them deduplicates"
    );
    assert_eq!(
      builder.literals.strings.len(),
      width * (width + 1) / 2,
      "{width} bytes of source retained {} bytes of literal arena; the claim was that a distinct \
       spelling costs its own source bytes",
      builder.literals.strings.len()
    );
    assert!(
      builder.literals.refused.is_none(),
      "and none of this is anywhere near the ceiling that bounds it"
    );
  }
}
