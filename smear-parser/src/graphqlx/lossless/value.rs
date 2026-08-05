//! Value productions. Each opens exactly one node; leaf tokens flow in on their own via the sink's
//! committed-token path, so no production wraps a bare token in a node.
//!
//! The conventions are GraphQL's and are not re-derived here: node kinds are
//! [`SyntaxKind`](crate::graphqlx::kinds::SyntaxKind) (`K::…`) while every `expect(…)` argument is
//! a `LosslessTokenKind` (`Kind::…`); every generic call carries `::<Src, Ctx>`, because `Src`
//! reaches the input only through the associated-type projection `Src::Slice<'inp>` and is
//! therefore not inferable; every `node(…)` closure spells its parameter in full; and
//! `Value[Const]` is **threaded** through a private `Constness` argument rather than duplicated
//! into a second set of productions.
//!
//! # The four places GraphQLx's value grammar is not GraphQL's
//!
//! **1. An enum value is a whole path.** `graphqlx/syntactic/value.rs`'s `enum_after_first` builds
//! `EnumValue::new(span, path)`, so `::ns::Colour` and `Colour` are both enum values and the node
//! is [`EnumValue`](crate::graphqlx::kinds::SyntaxKind::EnumValue) wrapping a
//! [`Path`](crate::graphqlx::kinds::SyntaxKind::Path). The nesting is not decoration: it is the
//! shape the AST has, so a typed wrapper can ask an enum value for its segments.
//!
//! **2. A bare `::` is a value head.** `::ns::Colour` in value position is a fully qualified enum
//! value (`value.rs:843-845`), which is why `recover::VALUE_HEADS` carries `PathSeparator` and
//! GraphQL's does not.
//!
//! **3. `set` and `map` are contextual, and the test is the *immediately following* token.**
//! `set { 1, 2 }` is a [`SetValue`](crate::graphqlx::kinds::SyntaxKind::SetValue); `set` alone,
//! and `set::Thing`, are ordinary enum values (`value.rs:791-806, 902-917`). See this module's
//! private `collection_or_enum` for why the test needs a retro-wrap rather than a two-token peek.
//!
//! **4. `MapEntry` is `Value => Value`,** with both halves full values — a map key is not
//! restricted to a name.
//!
//! What did **not** widen: an `ObjectField`'s key is still a plain `Name`
//! (`value.rs:519-540`, `name then_ignore(colon) then(value)`). Only the value side moved.
//!
//! # A `VariableValue` in a const position is reported **and still built**
//!
//! The rejection is a diagnostic, not a refusal to parse, exactly as in GraphQL: every byte is
//! kept, `tree.text() == source` still holds, and the node a diagnostic wants to point at is there.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::ParseInput as _;

use crate::graphqlx::kinds::SyntaxKind as K;

// `node`/`node_at` come from `coverage`, not from `tokora::parser`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being counted;
// without the feature they are tokora's own, re-exported unchanged.
use super::coverage::{node, node_at};

use super::{
  GraphqlxLosslessInput, Keyword, recover,
  recover::{CONST_VALUE_HEADS, OBJECT_FIELD_HEADS, VALUE_HEADS, opener_span, starts_value},
  trivia::{eat_if, expect, peek_as, peek_kind},
  ty::{path, path_tail},
};

/// The spec's `[Const]` grammar parameter, as an argument.
///
/// Threaded through every production that can reach a value, so one set of productions serves both
/// flavours. `lossless_drivers!` imports this by a literal path (`super::super::value::Constness`),
/// which is why the name and the module are fixed rather than free.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Constness {
  /// A `Value[Const]` position: `DefaultValue`, and every `Directives[Const]` context. A
  /// `VariableValue` here is reported.
  Const,
  /// An ordinary `Value` position. A `VariableValue` here is exactly what the grammar is for.
  NonConst,
}

impl Constness {
  /// Whether a variable in this position is a grammar error.
  #[inline]
  pub(crate) const fn forbids_variable(self) -> bool {
    matches!(self, Self::Const)
  }

  /// The head set a diagnostic raised in this position names — [`VALUE_HEADS`] without the `$` when
  /// a variable is not a production here.
  #[inline]
  pub(crate) const fn value_heads(self) -> &'static [Kind] {
    match self {
      Self::Const => CONST_VALUE_HEADS,
      Self::NonConst => VALUE_HEADS,
    }
  }
}

/// Which brace-delimited collection constructor a contextual keyword opens.
///
/// A parameter rather than two near-identical productions: the two differ only in the node kind
/// they wrap with and in whether their members are values or entries, and the *decision* — is the
/// next token a `{`? — is one piece of code either way.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Collection {
  /// `set { Value* }`.
  Set,
  /// `map { MapEntry* }`.
  Map,
}

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphqlx::lossless;

  /// `$ Name`
  ///
  /// The node kind is [`VariableValue`](K::VariableValue), where GraphQL's is `Variable`: this
  /// space takes its node names from GraphQLx's own AST carriers, and `graphqlx::ast` calls it
  /// `VariableValue`.
  fn variable<'inp, Src, Ctx>(inp) {
    node(
      K::VariableValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Dollar)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)
      },
    )
    .parse_input(inp)
  }

  /// `IntValue` — one node over all four radices.
  ///
  /// `LitInt<S>` is `Decimal | Hex | Binary | Octal`, and the kind space has a single `Int` image,
  /// so the radix stays readable from the token's text exactly as a line terminator's spelling
  /// does. `kind_map`'s docs carry the ruling.
  fn int_value<'inp, Src, Ctx>(inp) {
    node(
      K::IntValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| expect::<Src, Ctx>(inp, Kind::Int),
    )
    .parse_input(inp)
  }

  /// `FloatValue` — one node over both radices, for the reason [`int_value`] records.
  fn float_value<'inp, Src, Ctx>(inp) {
    node(
      K::FloatValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| expect::<Src, Ctx>(inp, Kind::Float),
    )
    .parse_input(inp)
  }

  /// `StringValue` — one node kind over two token kinds.
  ///
  /// The block/inline distinction is the *token*'s, and it survives on the token in the tree; the
  /// node above it is `StringValue` either way. GraphQLx's AST names `InlineStringValue` and
  /// `BlockStringValue` aliases, and the kind space deliberately gives neither its own kind — the
  /// one position that narrows (an import's `from "…"`) narrows on the token, not on the node.
  fn string_value<'inp, Src, Ctx>(inp) {
    node(
      K::StringValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if eat_if::<Src, Ctx>(inp, Kind::InlineString)? {
          return Ok(());
        }
        expect::<Src, Ctx>(inp, Kind::BlockString)
      },
    )
    .parse_input(inp)
  }

  /// `true` | `false`
  ///
  /// **Precondition: the head is an `Identifier` spelled `true` or `false`.** The spelling is
  /// decided once, by [`value`], and is not re-checked here.
  fn boolean_value<'inp, Src, Ctx>(inp) {
    node(
      K::BooleanValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)
      },
    )
    .parse_input(inp)
  }

  /// `null`, with the precondition [`boolean_value`] records.
  fn null_value<'inp, Src, Ctx>(inp) {
    node(
      K::NullValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)
      },
    )
    .parse_input(inp)
  }

  /// `Path` *but not* `true`, `false` or `null` in its first position.
  ///
  /// **The divergence that shows up in every tree.** GraphQL's `EnumValue` wraps one `Name` token;
  /// this one wraps a whole [`Path`](K::Path) node, so `Colour` is
  /// `EnumValue > Path > Name` and `::ns::Colour` is `EnumValue > Path > [PathSeparator, Name,
  /// PathSeparator, Name]`. The extra level is the AST's — `EnumValue::new(span, path)` — and it is
  /// what lets a typed wrapper ask an enum value for its segments instead of re-lexing its text.
  ///
  /// The three reserved spellings are excluded by [`value`]'s dispatch rather than by a check here,
  /// exactly as in GraphQL: the exclusion is the dispatch, not a re-test inside the arm dispatched
  /// to. Their reservation is only over the **first** segment — `x::null` is a perfectly good enum
  /// value, and `path` never looks at a spelling.
  fn enum_value<'inp, Src, Ctx>(inp) {
    // The leading trivia is crossed here, before the node opens, so `EnumValue` starts at its own
    // first token rather than at the whitespace in front of it. `path` makes the same guarantee
    // for itself; the peek here is what keeps the *outer* node honest, and it is served straight
    // out of the cache when the caller already peeked.
    peek_kind::<Src, Ctx>(inp)?;
    node(
      K::EnumValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| path::<Src, Ctx>(inp),
    )
    .parse_input(inp)
  }

  /// `[ Value* ]` — `[ Value[Const]* ]` when `konst` says so, the parameter riding down to every
  /// element.
  fn list_value<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::ListValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBracket)?;
        let open = opener_span(inp.span().end());
        while !eat_if::<Src, Ctx>(inp, Kind::RBracket)? {
          if peek_kind::<Src, Ctx>(inp)?.is_none() {
            // Unterminated at end of input: report and return `Ok`, so the enclosing `node` still
            // closes and the rest of the file keeps its structure.
            return recover::unclosed_list::<Src, Ctx>(inp, open);
          }
          // `value` recovers on a head that starts no value, and that recovery is guaranteed to
          // consume — which is this loop's only termination argument.
          value::<Src, Ctx>(inp, konst)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `{ ObjectField* }` — const-parameterised exactly as [`list_value`] is.
  fn object_value<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::ObjectValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_object::<Src, Ctx>(inp, open),
            Some(Kind::Identifier) => object_field::<Src, Ctx>(inp, konst)?,
            // The head is checked here rather than left to `object_field`'s own `expect`, because
            // that `expect` would return `Err` and abort the whole value — the list's recovery
            // would then have no counterpart inside an object.
            Some(_) => recover::unexpected::<Src, Ctx>(inp, OBJECT_FIELD_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `Name : Value`
  ///
  /// **The key is a `Name`, not a `Path`.** GraphQLx widened the enum *value* to a path and left
  /// the object field's key alone (`graphqlx/syntactic/value.rs:519-540`), so `{ ns::a: 1 }` is not
  /// a field with a qualified key — the `::` ends the field and is recovered.
  fn object_field<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::ObjectField.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        value::<Src, Ctx>(inp, konst)
      },
    )
    .parse_input(inp)
  }

  /// `Value => Value` — a map entry, and both halves are full values.
  ///
  /// GraphQLx only; there is no GraphQL production to port. The key is not restricted to a name, so
  /// `map { [1] => "a" }` is well-formed.
  fn map_entry<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::MapEntry.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        value::<Src, Ctx>(inp, konst)?;
        expect::<Src, Ctx>(inp, Kind::FatArrow)?;
        value::<Src, Ctx>(inp, konst)
      },
    )
    .parse_input(inp)
  }

  /// The `{ … }` body of a `set` or `map`, retro-wrapped so the keyword is inside the node.
  ///
  /// `mark` is minted by [`collection_or_enum`] **before** the keyword is consumed, which is what
  /// puts `set` and `map` inside their own node — the AST spans from the keyword
  /// (`Set::new(SimpleSpan::new(start, span.end()), …)` where `start` is the keyword's), and a node
  /// that began at the `{` would lose the only token that says which collection it is.
  fn collection_body<'inp, Src, Ctx>(
    inp,
    mark: tokora::cst::event::EventMark,
    konst: Constness,
    collection: Collection,
  ) {
    let kind = match collection {
      Collection::Set => K::SetValue,
      Collection::Map => K::MapValue,
    };
    node_at(
      mark,
      kind.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_object::<Src, Ctx>(inp, open),
            Some(head) if starts_value(head) => match collection {
              Collection::Set => value::<Src, Ctx>(inp, konst)?,
              // A member whose head starts a value but which then has no `=>` returns `Err` from
              // `map_entry`; guarding the head is what keeps a *non-value* member — a stray `)` —
              // from doing the same, which is the same trade `object_value` makes.
              Collection::Map => map_entry::<Src, Ctx>(inp, konst)?,
            },
            Some(_) => recover::unexpected::<Src, Ctx>(inp, VALUE_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `set`/`map` as a collection constructor, or as an ordinary path segment.
  ///
  /// # Why this needs a retro-wrap and not a two-token peek
  ///
  /// The grammar's test is *"is the immediately following token a `{`?"* — and over a
  /// trivia-surfacing stream "immediately following" means the next **significant** token, which
  /// may be any number of trivia tokens away. `set /* c */ { 1 }` is a set. A fixed `peek::<U2>()`
  /// sees the raw stream and would answer `Space`, so the only way to ask the question is to
  /// consume the keyword and then peek — after which, if the answer is "no", the keyword has
  /// already been committed outside any node.
  ///
  /// Two marks are minted before that commit, so either answer can still open its node over the
  /// keyword: the outer becomes [`SetValue`](K::SetValue)/[`MapValue`](K::MapValue) or
  /// [`EnumValue`](K::EnumValue), the inner becomes the [`Path`](K::Path). `cst_mark` pushes a
  /// tombstone, so two consecutive calls name two distinct slots and the wraps nest in slot order
  /// rather than in the order they were spent. **An unspent mark materializes into nothing**, so
  /// the branch not taken leaves no trace in the tree.
  fn collection_or_enum<'inp, Src, Ctx>(inp, konst: Constness, collection: Collection) {
    // Before the keyword: the enclosing node is still the innermost one open, so the leading
    // trivia the peek in `value` already crossed sits outside whatever this opens.
    let outer = inp.cst_mark();
    let inner = inp.cst_mark();
    expect::<Src, Ctx>(inp, Kind::Identifier)?;

    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
      return collection_body::<Src, Ctx>(inp, outer, konst, collection);
    }

    // An ordinary enum value whose first segment happens to spell `set` or `map`. The path may
    // continue — `set::Thing` is one path of two segments — and `path_tail` is `path`'s loop with
    // the first name already consumed.
    node_at(
      outer,
      K::EnumValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        node_at(
          inner,
          K::Path.raw(),
          |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| path_tail::<Src, Ctx>(inp),
        )
        .parse_input(inp)
      },
    )
    .parse_input(inp)
  }

  /// `= Value[Const]`
  ///
  /// **Takes no [`Constness`] argument, and must not.** `DefaultValue` is const in both positions
  /// the grammar puts it in, so the flavour is a property of this production rather than of its
  /// call site.
  fn default_value<'inp, Src, Ctx>(inp) {
    node(
      K::DefaultValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Equal)?;
        value::<Src, Ctx>(inp, Constness::Const)
      },
    )
    .parse_input(inp)
  }

  /// Dispatch on the value head. Opens **no** node of its own; the chosen production opens its own.
  ///
  /// Two arms have no GraphQL counterpart. `PathSeparator` opens a fully qualified enum value, and
  /// the `set`/`map` spellings inside the `Identifier` arm route to [`collection_or_enum`] rather
  /// than straight to [`enum_value`] — everything else about the identifier arm is GraphQL's, since
  /// `true`, `false` and `null` are reserved in both dialects and reserved only in a path's first
  /// segment.
  fn value<'inp, Src, Ctx>(inp, konst: Constness) {
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Dollar) => {
        if konst.forbids_variable() {
          recover::report_unexpected::<Src, Ctx>(inp, CONST_VALUE_HEADS)?;
        }
        variable::<Src, Ctx>(inp)
      }
      Some(Kind::Int) => int_value::<Src, Ctx>(inp),
      Some(Kind::Float) => float_value::<Src, Ctx>(inp),
      Some(Kind::InlineString | Kind::BlockString) => string_value::<Src, Ctx>(inp),
      Some(Kind::LBracket) => list_value::<Src, Ctx>(inp, konst),
      Some(Kind::LBrace) => object_value::<Src, Ctx>(inp, konst),
      // GraphQLx only: a leading `::` is a fully qualified enum value.
      Some(Kind::PathSeparator) => enum_value::<Src, Ctx>(inp),
      Some(Kind::Identifier) => match peek_as::<Src, Ctx, Keyword>(inp)? {
        Some(Keyword::True | Keyword::False) => boolean_value::<Src, Ctx>(inp),
        Some(Keyword::Null) => null_value::<Src, Ctx>(inp),
        Some(Keyword::Set) => {
          collection_or_enum::<Src, Ctx>(inp, konst, Collection::Set)
        }
        Some(Keyword::Map) => {
          collection_or_enum::<Src, Ctx>(inp, konst, Collection::Map)
        }
        // Every other spelling — including the SDL keywords and the directive-location spellings
        // `ContextualKeyword` also carries — is a perfectly good first path segment.
        _ => enum_value::<Src, Ctx>(inp),
      },
      _ => recover::unexpected::<Src, Ctx>(inp, konst.value_heads()),
    }
  }
}

lossless_drivers! {
  dialect = graphqlx::lossless;

  /// Drivers that run one value production over a `&str` and hand back the tree it built, for
  /// `tests/lossless_x_value.rs`.
  mod test_support;

  /// `super::value` over `src`, in an ordinary (non-const) position.
  fn parse_value => value [Constness::NonConst];

  /// `super::value` over `src`, in a **const** position — the flavour `DefaultValue` and every SDL
  /// directive argument reach, and the only door to the `$` rejection below the document level.
  fn parse_const_value => value [Constness::Const];

  /// `super::default_value` over `src` — the one value production `value` cannot reach.
  fn parse_default_value => default_value;

  /// `super::map_entry` over `src`, on its own: the production a malformed `map` body would
  /// otherwise only be observable through.
  fn parse_map_entry => map_entry [Constness::NonConst];
}
