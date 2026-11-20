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
//! # The seven places GraphQLx's value grammar is not GraphQL's
//!
//! **1. An enum value is a whole path.** `graphqlx/syntactic/value.rs`'s `enum_after_first` builds
//! `EnumValue::new(span, path)`, so `^::ns::Colour` and `Colour` are both enum values and the node
//! is [`EnumValue`](crate::graphqlx::kinds::SyntaxKind::EnumValue) wrapping a
//! [`Path`](crate::graphqlx::kinds::SyntaxKind::Path). The nesting is not decoration: it is the
//! shape the AST has, so a typed wrapper can ask an enum value for its segments.
//!
//! **2. A root's sigil is a value head.** `~::ns::Colour`, `^::ns::Colour` and `..::Colour` in
//! value position are rooted enum values (the declined arm of `graphqlx/syntactic/value.rs`'s
//! `value`), which is why `recover::VALUE_HEADS` carries `Tilde`, `DotDot` and `Caret` and
//! GraphQL's does not. A `::` is not a head: it only joins, and a value that starts with one is
//! refused.
//!
//! **3. `set` and `map` are contextual, and the test is the *immediately following* token.**
//! `set!{ 1, 2 }` is a [`SetValue`](crate::graphqlx::kinds::SyntaxKind::SetValue); `set` alone and
//! `set::Thing` are ordinary enum values (the `set` and `map` arms of `value` and `const_value` in
//! `graphqlx/syntactic/value.rs`), and so is the `set` of `set { a: 1 }` everywhere but a schema
//! position, where point 6 makes it a typed object's type. See this module's private
//! `collection_or_enum` for why the test needs a retro-wrap rather than a two-token peek.
//!
//! **4. `MapEntry` is `Value => Value`,** with both halves full values — a map key is not
//! restricted to a name.
//!
//! **5. A `char` literal is a value.** `'a'` is one token, and its node is
//! [`CharValue`](crate::graphqlx::kinds::SyntaxKind::CharValue) wherever GraphQL takes a scalar:
//! an executable value, a constant one, and a member of every collection.
//!
//! **6. A schema position reads the typed forms.** A path followed by `{` is a
//! [`TypedObjectValue`](crate::graphqlx::kinds::SyntaxKind::TypedObjectValue), `Point { x: 1 }`, and
//! a path followed by `(` a [`TypedValue`](crate::graphqlx::kinds::SyntaxKind::TypedValue),
//! `Url("x")`, each with type arguments between when `<` follows the path. The next significant
//! token decides, whatever trivia stands before it, so `[RED {a: 1}]` is one typed object there.
//! An executable document reads neither, and there the same texts are a path and an object.
//!
//! **7. A schema position reads a `const { … }` block,** when `const` is followed by `{`. Its
//! production moves the lexer into the block's mode at the `{` and back at the `}`, so a comma
//! between the braces is a [`Separator`](crate::graphqlx::kinds::SyntaxKind::Separator) and a
//! number carries no sign. It holds one
//! [`Expression`](crate::graphqlx::kinds::SyntaxKind::Expression), a lone operand's too, whose
//! leading trivia is its caller's and whose trailing trivia is its own. Inside it stand a
//! parenthesis, the prefixed strings `b"…"` and `i"…"`, a block inside the block, which switches
//! nothing, and a list, a set, a map, an object and the typed forms, each of whose members is an
//! `Expression` of its own, with a `Separator` between two.
//!
//! What did **not** widen: an `ObjectField`'s key is still a plain `Name` (`object_field` in
//! `graphqlx/syntactic/value.rs`, `name then_ignore(colon) then(value)`). Only the value side
//! moved.
//!
//! # A `VariableValue` in a const position is reported **and still built**
//!
//! The rejection is a diagnostic, not a refusal to parse, exactly as in GraphQL: every byte is
//! kept, `tree.text() == source` still holds, and the node a diagnostic wants to point at is there.

use smear_lexer::graphqlx::{
  StrPrefix,
  lossless::{LosslessToken, LosslessTokenKind as Kind},
};
use tokora::{ParseInput as _, error::MaybeTerminal as _};

use crate::graphqlx::kinds::SyntaxKind as K;

// `node`/`node_at` come from `coverage`, not from `tokora::combinator`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being counted;
// without the feature they are tokora's own, re-exported unchanged.
use super::coverage::{node, node_at};

use super::{
  GraphqlxLosslessError, GraphqlxLosslessInput, Keyword, recover,
  recover::{
    BLOCK_OPERAND_HEADS, BRACE_CLOSER_HEADS, CONST_VALUE_HEADS, LIST_CLOSER_HEADS,
    OBJECT_FIELD_HEADS, OPERATOR_OR_SEPARATOR_HEADS, PARENS_CLOSER_HEADS, TYPED_FORM_HEADS,
    VALUE_HEADS, closes_enclosing_block, is_closer, opener_span, starts_value,
  },
  trivia::{eat_if, expect, peek_as, peek_kind},
  ty::{path, path_root, path_tail, type_generics},
};

/// The spec's `[Const]` grammar parameter, as an argument, and the schema position beside it.
///
/// Threaded through every production that can reach a value, so one set of productions serves
/// every flavour. `lossless_drivers!` imports this by a literal path
/// (`super::super::value::Constness`), which is why the name and the module are fixed rather than
/// free.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Constness {
  /// A `Value[Const]` position of an executable document: a variable's default and its
  /// directives. A `VariableValue` here is reported.
  Const,
  /// An ordinary `Value` position. A `VariableValue` here is exactly what the grammar is for.
  NonConst,
  /// A schema position: a constant position of a type-system definition or extension, which is a
  /// member's default and every directive such a definition applies. It reads every form a
  /// [`Const`](Self::Const) position reads, and a `VariableValue` here is reported as there.
  Schema,
}

impl Constness {
  /// Whether a variable in this position is a grammar error.
  #[inline]
  pub(crate) const fn forbids_variable(self) -> bool {
    matches!(self, Self::Const | Self::Schema)
  }

  /// The head set a diagnostic raised in this position names — [`VALUE_HEADS`] without the `$` when
  /// a variable is not a production here.
  #[inline]
  pub(crate) const fn value_heads(self) -> &'static [Kind] {
    match self {
      Self::Const | Self::Schema => CONST_VALUE_HEADS,
      Self::NonConst => VALUE_HEADS,
    }
  }
}

/// Which brace-delimited collection constructor a contextual keyword opens.
///
/// A parameter rather than two near-identical productions: the two differ only in the node kind
/// they wrap with and in whether their members are values or entries, and the *decision* — is the
/// next token a `!`? — is one piece of code either way.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Collection {
  /// `set!{ Value* }`.
  Set,
  /// `map!{ MapEntry* }`.
  Map,
}

/// The contextual keyword at a schema value's head that opens something when the next token is
/// its own: `set` and `map` before `!`, and `const` before `{`. Before anything else each is a
/// path's first segment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Opener {
  /// `set` or `map`, and the collection each opens.
  Collection(Collection),
  /// `const`, which opens a block.
  Block,
}

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphqlx::lossless;

  /// `$ Name`
  ///
  /// The node kind is [`VariableValue`](K::VariableValue), where GraphQL's is `Variable`: this
  /// space takes its node names from GraphQLx's own AST carriers, and `graphqlx::ast` calls it
  /// `VariableValue`.
  ///
  /// # The head peek is not redundant, and gate 5 is what proved it
  ///
  /// `expect` skips trivia before it matches, so without the peek that skip runs **inside** a node
  /// that is already open and the node's start lands on the wrong side of the space. Every caller in
  /// *value* position dispatches on a peek of its own, which is why the omission survived every
  /// other gate — the trivia was already committed by the time this ran.
  ///
  /// [`super::executable::variable_definition`] is the exception, and the one the golden trees
  /// caught: it parses an optional description and then calls this with nothing in between, so in
  /// `query Q("the id" $id: ID!)` the `VariableValue` spanned `" $id"` rather than `"$id"` and a
  /// consumer renaming through that range took the space with it. Two sites, in one corpus entry.
  ///
  /// The fix is here rather than at that call site, following the ruling Phase A recorded for
  /// `named_type`: the guarantee is the production's own rather than a precondition each caller has
  /// to remember. `path` and `type_ref` in `ty.rs` carry the same leading peek for the same reason.
  fn variable<'inp, Src, Ctx>(inp) {
    peek_kind::<Src, Ctx>(inp)?;
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
  /// one position that narrows (a `use` tree's string source) narrows on the token, not on the
  /// node.
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

  /// `CharValue` — a `'…'` literal, GraphQLx only: one character or one escape.
  fn char_value<'inp, Src, Ctx>(inp) {
    node(
      K::CharValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| expect::<Src, Ctx>(inp, Kind::Char),
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
  /// this one wraps a whole [`Path`](K::Path) node, so `Colour` is `EnumValue > Path > Name` and
  /// `^::ns::Colour` is `EnumValue > Path > [Caret, PathSeparator, Name, PathSeparator, Name]`. The
  /// extra level is the AST's — `EnumValue::new(span, path)` — and it is what lets a typed wrapper
  /// ask an enum value for its segments instead of re-lexing its text.
  ///
  /// The three reserved spellings are excluded by [`value`]'s dispatch rather than by a check here,
  /// exactly as in GraphQL: the exclusion is the dispatch, not a re-test inside the arm dispatched
  /// to. Their reservation is only over the **first** segment of a path with no root — `x::null`
  /// and `^::null` are perfectly good enum values, and `path` never looks at a spelling.
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
  ///
  /// A `}` of a block around the list ends it unclosed, by [`closes_enclosing_block`]. The head is
  /// asked before `value` is, because `value` would take that `}` as junk.
  fn list_value<'inp, Src, Ctx>(inp, konst: Constness) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::ListValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBracket)?;
        let open = opener_span(inp.span().end());
        let open_at: usize = inp.state().brace_depth();
        while !eat_if::<Src, Ctx>(inp, Kind::RBracket)? {
          match peek_kind::<Src, Ctx>(inp)? {
            // Unterminated at end of input: report and return `Ok`, so the enclosing `node` still
            // closes and the rest of the file keeps its structure.
            None => return recover::unclosed_list::<Src, Ctx>(inp, open),
            Some(Kind::RBrace) if closes_enclosing_block(open_at, inp.state().brace_depth()) => {
              return recover::report_unexpected::<Src, Ctx>(inp, LIST_CLOSER_HEADS);
            }
            // `value` recovers on a head that starts no value, and that recovery is guaranteed to
            // consume. The two arms above return, so every turn returns or consumes — which is
            // this loop's only termination argument.
            Some(_) => value::<Src, Ctx>(inp, konst)?,
          }
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `{ ObjectField* }` — const-parameterised exactly as [`list_value`] is.
  fn object_value<'inp, Src, Ctx>(inp, konst: Constness) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::ObjectValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| object_fields::<Src, Ctx>(inp, konst),
    )
    .parse_input(inp)
  }

  /// `{ ObjectField* }` inside whichever node its caller opened: an object value's, and a typed
  /// object's after its type path. Opens no node, and is always inlined, so an object costs the
  /// frames it did when its loop was written in its node's closure.
  #[inline(always)]
  fn object_fields<'inp, Src, Ctx>(inp, konst: Constness) {
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
  }

  /// `Name : Value`
  ///
  /// **The key is a `Name`, not a `Path`.** GraphQLx widened the enum *value* to a path and left
  /// the object field's key alone (`graphqlx/syntactic/value.rs`'s `object_field`), so
  /// `{ ns::a: 1 }` is not a field with a qualified key — the `::` ends the field and is
  /// recovered.
  fn object_field<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::ObjectField.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        required_value::<Src, Ctx>(inp, konst)
      },
    )
    .parse_input(inp)
  }

  /// `Value => Value` — a map entry, and both halves are full values.
  ///
  /// GraphQLx only; there is no GraphQL production to port. The key is not restricted to a name, so
  /// `map!{ [1] => "a" }` is well-formed.
  fn map_entry<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::MapEntry.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        value::<Src, Ctx>(inp, konst)?;
        expect::<Src, Ctx>(inp, Kind::FatArrow)?;
        required_value::<Src, Ctx>(inp, konst)
      },
    )
    .parse_input(inp)
  }

  /// The `! { … }` body of a `set` or `map`, retro-wrapped so the keyword is inside the node.
  ///
  /// `mark` is minted by [`collection_or_enum`] **before** the keyword is consumed, which is what
  /// puts `set` and `map` inside their own node — the AST spans from the keyword
  /// (`Set::new(SimpleSpan::new(start, span.end()), …)` where `start` is the keyword's), and a node
  /// that began at the `{` would lose the only token that says which collection it is. The `!` is
  /// consumed inside the wrap too, so it is a child of the node, and any trivia between the keyword,
  /// the `!` and the `{` lands between the node's own tokens.
  fn collection_body<'inp, Src, Ctx>(
    inp,
    mark: tokora::cst::EventMark,
    konst: Constness,
    collection: Collection,
  ) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    let kind = match collection {
      Collection::Set => K::SetValue,
      Collection::Map => K::MapValue,
    };
    node_at(
      mark,
      kind.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Bang)?;
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

  /// `set`/`map` as a collection constructor or as an ordinary path segment, and in a schema
  /// position every path at a value's head, which may be the type of a typed form.
  ///
  /// `opener` names the keyword at the head, when the head is `set`, `map` or `const`; with none,
  /// the head is a name or a root's sigil, which the path production reads.
  ///
  /// With `in_block` the head is an operand's inside a `const { … }` block, and what it opens is the
  /// block's own: a set's and a map's members are expressions and a `,` separates them, a typed
  /// form holds the block's members, and a block after `const` is a block inside the block, which
  /// switches nothing.
  ///
  /// # Why this needs a retro-wrap and not a two-token peek
  ///
  /// The grammar's test is *"is the immediately following token a `!`?"* — and over a
  /// trivia-surfacing stream "immediately following" means the next **significant** token, which
  /// may be any number of trivia tokens away. `set /* c */ ! { 1 }` is a set. A fixed
  /// `peek::<U2>()` sees the raw stream and would answer `Space`, so the only way to ask the
  /// question is to consume the keyword and then peek — after which, if the answer is "no", the
  /// keyword has already been committed outside any node. A typed form is the same question asked
  /// after a whole path: `Point /* c */ { x: 1 }` is one typed object.
  ///
  /// Three marks are minted before anything is committed, so every answer can still open its node
  /// over what came before it: the outer becomes [`SetValue`](K::SetValue)/[`MapValue`](K::MapValue),
  /// [`EnumValue`](K::EnumValue), [`TypedObjectValue`](K::TypedObjectValue) or
  /// [`TypedValue`](K::TypedValue); the middle becomes a typed form's [`TypePath`](K::TypePath); and
  /// the inner becomes the [`Path`](K::Path). `cst_mark` pushes a tombstone, so consecutive calls
  /// name distinct slots and the wraps nest in slot order rather than in the order they were spent.
  /// **An unspent mark materializes into nothing**, so the branch not taken leaves no trace in the
  /// tree.
  ///
  /// # What is read after the path, and what it moves
  ///
  /// The path's own probe for `::` crossed the trivia after the path while the `Path` was open, so
  /// that trivia is the path's, as it is wherever a path ends. The read that decides a typed form
  /// is a second look at the same staged token and crosses nothing: an enum value is the tree it
  /// was before the typed forms existed, and so is every value outside a schema position, which
  /// never asks.
  fn collection_or_enum<'inp, Src, Ctx>(
    inp,
    konst: Constness,
    opener: Option<Opener>,
    in_block: bool,
  ) {
    // Before the head: the enclosing node is still the innermost one open, so the leading trivia
    // the peek in `value` already crossed sits outside whatever this opens.
    let outer = inp.cst_mark();
    let typed = inp.cst_mark();
    let inner = inp.cst_mark();

    match opener {
      Some(opener) => {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        match (opener, peek_kind::<Src, Ctx>(inp)?) {
          (Opener::Collection(collection), Some(Kind::Bang)) => {
            return if in_block {
              block_collection_body::<Src, Ctx>(inp, outer, collection)
            } else {
              collection_body::<Src, Ctx>(inp, outer, konst, collection)
            };
          }
          // The block's mark is the one before `const`, so the keyword and the trivia the peek
          // just crossed are the block's own first children. Only a block outside a block
          // switches the lexer's mode.
          (Opener::Block, Some(Kind::LBrace)) => {
            return block_braces::<Src, Ctx>(inp, outer, !in_block);
          }
          _ => {}
        }
        // A path whose first segment happens to spell `set`, `map` or `const`. It may continue —
        // `set::Thing` is one path of two segments — and `path_tail` is `path`'s loop with the
        // first name already consumed.
        node_at(
          inner,
          K::Path.raw(),
          |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| path_tail::<Src, Ctx>(inp),
        )
        .parse_input(inp)?;
      }
      None => {
        // `path`'s body, under the mark rather than a node of its own.
        node_at(
          inner,
          K::Path.raw(),
          |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
            path_root::<Src, Ctx>(inp)?;
            expect::<Src, Ctx>(inp, Kind::Identifier)?;
            path_tail::<Src, Ctx>(inp)
          },
        )
        .parse_input(inp)?;
      }
    }

    let opens_a_typed_form = match konst {
      Constness::Schema => matches!(
        peek_kind::<Src, Ctx>(inp)?,
        Some(Kind::LAngle | Kind::LBrace | Kind::LParen)
      ),
      Constness::Const | Constness::NonConst => false,
    };
    if opens_a_typed_form {
      return typed_form::<Src, Ctx>(inp, outer, typed, konst, in_block);
    }
    node_at(
      outer,
      K::EnumValue.raw(),
      |_: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| -> Result<(), GraphqlxLosslessError<'inp, Src, Ctx>> { Ok(()) },
    )
    .parse_input(inp)
  }

  /// A typed form after the path that names its type: `Point { x: 1 }`, `Box<i32> {}` or
  /// `Url("x")`, wrapped from the marks [`collection_or_enum`] minted before the path.
  ///
  /// The [`TypePath`](K::TypePath) is always there, with type arguments or without, since the
  /// form's type is one. `<` and then neither bracket is reported where the bracket should be, and
  /// the form is built as a typed object with no body: the type was written, and what is missing is
  /// the rest. With `in_block` the form's members are a block's.
  fn typed_form<'inp, Src, Ctx>(
    inp,
    value: tokora::cst::EventMark,
    ty: tokora::cst::EventMark,
    konst: Constness,
    in_block: bool,
  ) {
    let has_arguments = peek_kind::<Src, Ctx>(inp)? == Some(Kind::LAngle);
    node_at(
      ty,
      K::TypePath.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if has_arguments {
          type_generics::<Src, Ctx>(inp)
        } else {
          Ok(())
        }
      },
    )
    .parse_input(inp)?;
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::LBrace) if in_block => block_typed_object_body::<Src, Ctx>(inp, value),
      Some(Kind::LParen) if in_block => block_typed_value_body::<Src, Ctx>(inp, value),
      Some(Kind::LBrace) => typed_object_body::<Src, Ctx>(inp, value, konst),
      Some(Kind::LParen) => typed_value_body::<Src, Ctx>(inp, value, konst),
      _ => {
        recover::report_unexpected::<Src, Ctx>(inp, TYPED_FORM_HEADS)?;
        node_at(
          value,
          K::TypedObjectValue.raw(),
          |_: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| -> Result<(), GraphqlxLosslessError<'inp, Src, Ctx>> { Ok(()) },
        )
        .parse_input(inp)
      }
    }
  }

  /// `{ ObjectField* }` after a typed object's type path, its loop [`object_value`]'s.
  fn typed_object_body<'inp, Src, Ctx>(inp, mark: tokora::cst::EventMark, konst: Constness) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node_at(
      mark,
      K::TypedObjectValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| object_fields::<Src, Ctx>(inp, konst),
    )
    .parse_input(inp)
  }

  /// `( Value )` after a typed value's type path: one value, and its closer tested as a list's is.
  ///
  /// A `}` of a block around the form ends it unclosed, by [`closes_enclosing_block`], and anything
  /// else before the `)` is reported once and skipped, so a second value is one diagnostic and the
  /// form still closes on its own `)`.
  fn typed_value_body<'inp, Src, Ctx>(inp, mark: tokora::cst::EventMark, konst: Constness) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node_at(
      mark,
      K::TypedValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LParen)?;
        let open = opener_span(inp.span().end());
        let open_at: usize = inp.state().brace_depth();
        required_value::<Src, Ctx>(inp, konst)?;
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RParen)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_parens::<Src, Ctx>(inp, open),
            Some(Kind::RBrace) if closes_enclosing_block(open_at, inp.state().brace_depth()) => {
              return recover::report_unexpected::<Src, Ctx>(inp, PARENS_CLOSER_HEADS);
            }
            // `unexpected` reports and consumes, so every turn returns or consumes.
            Some(_) => recover::unexpected::<Src, Ctx>(inp, PARENS_CLOSER_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `{ Expression }` after a block's `const`, retro-wrapped from the mark minted before the
  /// keyword so the keyword is the block's first child.
  ///
  /// With `switch`, this is the one production that moves the lexer into a block's mode and back:
  /// a schema position's block. Seven steps, written out so that no read crosses a switch. The
  /// caller's peek found the `{` and crossed the trivia after `const`, which is inside the node.
  /// The `{` is taken by `expect`, with nothing cached behind it; the lexer enters the block; the
  /// expression's first peek is the first lex in the mode, so the trivia after the `{` is lexed in
  /// it; the body runs to its `}`; the lexer leaves the block, so the trivia after the `}` is the
  /// first thing lexed out of the mode; and the node is the mark's. An error the parse may go on
  /// from leaves the block before it is returned. A terminal stop writes no state, so the stop
  /// stays one.
  ///
  /// Without `switch` it is a block inside a block, whose lexer is in the mode already: it reads
  /// the same tokens and touches no state.
  fn block_braces<'inp, Src, Ctx>(inp, mark: tokora::cst::EventMark, switch: bool) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node_at(
      mark,
      K::ConstBlock.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        if switch {
          inp.state_mut().enter_const_block();
        }
        let body = block_body::<Src, Ctx>(inp, open);
        if switch {
          let stopped = match &body {
            Ok(()) => false,
            Err(error) => error.is_terminal() || inp.at_scanner_stop(),
          };
          if !stopped {
            inp.state_mut().leave_const_block();
          }
        }
        body
      },
    )
    .parse_input(inp)
  }

  /// A block's inside, from the expression to its `}`: an object value's loop around the one
  /// expression. A `}` ends it; the end of input leaves it unclosed at the `{`; and anything else,
  /// what ended the expression without being the block's, is reported against the `}` and
  /// skipped, so every turn returns or consumes.
  #[inline(always)]
  fn block_body<'inp, Src, Ctx>(inp, open: tokora::SimpleSpan) {
    block_expression::<Src, Ctx>(inp)?;
    loop {
      if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
        return Ok(());
      }
      match peek_kind::<Src, Ctx>(inp)? {
        None => return recover::unclosed_object::<Src, Ctx>(inp, open),
        Some(_) => recover::unexpected::<Src, Ctx>(inp, BRACE_CLOSER_HEADS)?,
      }
    }
  }

  /// The expression a block, a parenthesis or a block's member holds: one
  /// [`Expression`](K::Expression) node around it, always, a lone operand's too.
  ///
  /// Where its trivia lands is the rule, and it is the substrate's own: a committed token lands in
  /// the innermost node open at its commit. The leading trivia is crossed by a peek made before the
  /// node opens, so it is the caller's. The trailing trivia is crossed by the peek made inside the
  /// node after the operand, the read that finds whether the expression goes on, so it is the
  /// expression's. What ends the expression is judged after the node has closed: a `,`, a closer,
  /// a `=>` or the end of input ends it, and anything else is reported once, against an operator
  /// or a `,`, and skipped into an `Error` that stands beside the node, in its caller's.
  fn block_expression<'inp, Src, Ctx>(inp) {
    peek_kind::<Src, Ctx>(inp)?;
    node(
      K::Expression.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        block_operand::<Src, Ctx>(inp)?;
        peek_kind::<Src, Ctx>(inp)?;
        Ok(())
      },
    )
    .parse_input(inp)?;
    match peek_kind::<Src, Ctx>(inp)? {
      None
      | Some(
        Kind::Separator | Kind::RBrace | Kind::RBracket | Kind::RParen | Kind::FatArrow,
      ) => Ok(()),
      Some(_) => recover::unexpected::<Src, Ctx>(inp, OPERATOR_OR_SEPARATOR_HEADS),
    }
  }

  /// One operand inside a block, read wherever it stands and reported through
  /// [`BLOCK_OPERAND_HEADS`] where none begins.
  ///
  /// A head that begins no operand is skipped into an `Error`, inside the expression. A closer or
  /// the end of input is reported and left, as a required value's is, and nothing is built there:
  /// the expression is then empty. A `$` is reported and still built, as in a constant position.
  fn block_operand<'inp, Src, Ctx>(inp) {
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Dollar) => {
        recover::report_unexpected::<Src, Ctx>(inp, BLOCK_OPERAND_HEADS)?;
        variable::<Src, Ctx>(inp)
      }
      Some(Kind::Int) => int_value::<Src, Ctx>(inp),
      Some(Kind::Float) => float_value::<Src, Ctx>(inp),
      Some(Kind::InlineString | Kind::BlockString) => string_value::<Src, Ctx>(inp),
      Some(Kind::Char) => char_value::<Src, Ctx>(inp),
      Some(Kind::PrefixedString) => prefixed_string_value::<Src, Ctx>(inp),
      Some(Kind::LParen) => paren_value::<Src, Ctx>(inp),
      Some(Kind::LBracket) => block_list_value::<Src, Ctx>(inp),
      Some(Kind::LBrace) => block_object_value::<Src, Ctx>(inp),
      Some(Kind::Tilde | Kind::DotDot | Kind::Caret) => {
        collection_or_enum::<Src, Ctx>(inp, Constness::Schema, None, true)
      }
      Some(Kind::Identifier) => block_name::<Src, Ctx>(inp),
      None => recover::report_unexpected::<Src, Ctx>(inp, BLOCK_OPERAND_HEADS),
      Some(head) if is_closer(head) => {
        recover::report_unexpected::<Src, Ctx>(inp, BLOCK_OPERAND_HEADS)
      }
      Some(_) => recover::unexpected::<Src, Ctx>(inp, BLOCK_OPERAND_HEADS),
    }
  }

  /// A name at an operand's head inside a block: `true`, `false`, `null`, a block inside the
  /// block when `const` is followed by `{`, a set or a map when `set` or `map` is followed by `!`,
  /// and otherwise a path, which `<`, `{` or `(` after it makes the type of a typed form. All but
  /// the first three are [`collection_or_enum`]'s, as at a schema value's head.
  fn block_name<'inp, Src, Ctx>(inp) {
    let opener = match peek_as::<Src, Ctx, Keyword>(inp)? {
      Some(Keyword::True | Keyword::False) => return boolean_value::<Src, Ctx>(inp),
      Some(Keyword::Null) => return null_value::<Src, Ctx>(inp),
      Some(Keyword::Set) => Some(Opener::Collection(Collection::Set)),
      Some(Keyword::Map) => Some(Opener::Collection(Collection::Map)),
      Some(Keyword::Const) => Some(Opener::Block),
      _ => None,
    };
    collection_or_enum::<Src, Ctx>(inp, Constness::Schema, opener, true)
  }

  /// `b"…"` or `i"…"`: one token, whose node is [`BytesValue`](K::BytesValue) or
  /// [`IdValue`](K::IdValue) by the literal's prefix.
  fn prefixed_string_value<'inp, Src, Ctx>(inp) {
    let prefix = inp.peek_head_map(|token| match token.data {
      LosslessToken::LitPrefixedStr(literal) => Some(literal.prefix()),
      _ => None,
    })?;
    let kind = match prefix.flatten() {
      Some(StrPrefix::Id) => K::IdValue,
      // The lexer makes `b` and `i` alone; every other letter before a string is refused.
      _ => K::BytesValue,
    };
    node(
      kind.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::PrefixedString)
      },
    )
    .parse_input(inp)
  }

  /// `( Expression )` inside a block: one expression, and its closer tested as a list's is. A
  /// separator is not its own, so `(1,)` is one report, at the comma, against the `)`.
  fn paren_value<'inp, Src, Ctx>(inp) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::ParenValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| parenthesized::<Src, Ctx>(inp),
    )
    .parse_input(inp)
  }

  /// `( Expression )` inside whichever node its caller opened: a parenthesis's, and a typed value's
  /// inside a block. Opens no node, and is always inlined.
  ///
  /// A `}` of a block around it ends it unclosed, by [`closes_enclosing_block`], and anything else
  /// before the `)` is reported once, against the `)`, and skipped.
  #[inline(always)]
  fn parenthesized<'inp, Src, Ctx>(inp) {
    expect::<Src, Ctx>(inp, Kind::LParen)?;
    let open = opener_span(inp.span().end());
    let open_at: usize = inp.state().brace_depth();
    block_expression::<Src, Ctx>(inp)?;
    loop {
      if eat_if::<Src, Ctx>(inp, Kind::RParen)? {
        return Ok(());
      }
      match peek_kind::<Src, Ctx>(inp)? {
        None => return recover::unclosed_parens::<Src, Ctx>(inp, open),
        Some(Kind::RBrace) if closes_enclosing_block(open_at, inp.state().brace_depth()) => {
          return recover::report_unexpected::<Src, Ctx>(inp, PARENS_CLOSER_HEADS);
        }
        // `unexpected` reports and consumes, so every turn returns or consumes.
        Some(_) => recover::unexpected::<Src, Ctx>(inp, PARENS_CLOSER_HEADS)?,
      }
    }
  }

  /// `[ Expression (, Expression)* ,? ]` inside a block: each element one expression, and a `,`
  /// between two.
  ///
  /// A `}` of the block around the list ends it unclosed, by [`closes_enclosing_block`], as it ends
  /// a list outside one. After an element a `,` goes on and the `]` ends the list; anything else
  /// ended the element's expression without being the list's own, and is reported once, against
  /// the `]`, and skipped. An element's head is the expression's own, so a missing element is the
  /// operand's report. A turn that reads an element at a stray closer consumes nothing, and the turn
  /// after it skips that closer, so the loop ends.
  fn block_list_value<'inp, Src, Ctx>(inp) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::ListValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBracket)?;
        let open = opener_span(inp.span().end());
        let open_at: usize = inp.state().brace_depth();
        let mut at_element = true;
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBracket)? {
            return Ok(());
          }
          if !at_element && eat_if::<Src, Ctx>(inp, Kind::Separator)? {
            at_element = true;
            continue;
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_list::<Src, Ctx>(inp, open),
            Some(Kind::RBrace) if closes_enclosing_block(open_at, inp.state().brace_depth()) => {
              return recover::report_unexpected::<Src, Ctx>(inp, LIST_CLOSER_HEADS);
            }
            Some(_) if at_element => {
              block_expression::<Src, Ctx>(inp)?;
              at_element = false;
            }
            Some(_) => {
              recover::unexpected::<Src, Ctx>(inp, LIST_CLOSER_HEADS)?;
              at_element = true;
            }
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `{ ObjectField (, ObjectField)* ,? }` inside a block: each field's value one expression.
  fn block_object_value<'inp, Src, Ctx>(inp) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::ObjectValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| block_object_fields::<Src, Ctx>(inp),
    )
    .parse_input(inp)
  }

  /// The `{`, the fields and the `}` of an object or a typed object inside a block, inside
  /// whichever node its caller opened. Opens no node, and is always inlined.
  #[inline(always)]
  fn block_object_fields<'inp, Src, Ctx>(inp) {
    expect::<Src, Ctx>(inp, Kind::LBrace)?;
    let open = opener_span(inp.span().end());
    block_brace_members::<Src, Ctx>(inp, open, None)
  }

  /// `Name : Expression` — an object field inside a block, its value an expression.
  fn block_object_field<'inp, Src, Ctx>(inp) {
    node(
      K::ObjectField.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        block_expression::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `Expression => Expression` — a map entry inside a block, both halves expressions.
  fn block_map_entry<'inp, Src, Ctx>(inp) {
    node(
      K::MapEntry.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        block_expression::<Src, Ctx>(inp)?;
        expect::<Src, Ctx>(inp, Kind::FatArrow)?;
        block_expression::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// The `! { … }` body of a `set` or a `map` inside a block, retro-wrapped from the mark
  /// [`collection_or_enum`] minted before the keyword, as [`collection_body`] is: a set's members
  /// are expressions and a map's are entries, and a `,` separates two.
  fn block_collection_body<'inp, Src, Ctx>(
    inp,
    mark: tokora::cst::EventMark,
    collection: Collection,
  ) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    let kind = match collection {
      Collection::Set => K::SetValue,
      Collection::Map => K::MapValue,
    };
    node_at(
      mark,
      kind.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Bang)?;
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        block_brace_members::<Src, Ctx>(inp, open, Some(collection))
      },
    )
    .parse_input(inp)
  }

  /// `{ ObjectField* }` after a typed object's type path inside a block, its fields a block's.
  fn block_typed_object_body<'inp, Src, Ctx>(inp, mark: tokora::cst::EventMark) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node_at(
      mark,
      K::TypedObjectValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| block_object_fields::<Src, Ctx>(inp),
    )
    .parse_input(inp)
  }

  /// `( Expression )` after a typed value's type path inside a block: one expression, as a
  /// parenthesis holds.
  fn block_typed_value_body<'inp, Src, Ctx>(inp, mark: tokora::cst::EventMark) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node_at(
      mark,
      K::TypedValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| parenthesized::<Src, Ctx>(inp),
    )
    .parse_input(inp)
  }

  /// The members of a set, a map, an object or a typed object inside a block, its `{` consumed and
  /// up to its `}`: a set's expressions or a map's entries, as `collection` names, or with none an
  /// object's fields, and a `,` between two.
  ///
  /// After a member a `,` goes on and the `}` ends the members; anything else ended the member
  /// without being the aggregate's own, and is reported once, against the `}`, and skipped. The end
  /// of input leaves the aggregate unclosed at its `{`. A field's head is a name and anything else
  /// there is skipped, as an object's is outside a block, and a map entry's head is never a closer,
  /// whose report would otherwise end the whole block at the entry's `=>`.
  #[inline(always)]
  fn block_brace_members<'inp, Src, Ctx>(
    inp,
    open: tokora::SimpleSpan,
    collection: Option<Collection>,
  ) {
    let mut at_member = true;
    loop {
      if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
        return Ok(());
      }
      if !at_member && eat_if::<Src, Ctx>(inp, Kind::Separator)? {
        at_member = true;
        continue;
      }
      match peek_kind::<Src, Ctx>(inp)? {
        None => return recover::unclosed_object::<Src, Ctx>(inp, open),
        Some(head) if at_member => {
          match collection {
            Some(Collection::Set) => block_expression::<Src, Ctx>(inp)?,
            Some(Collection::Map) if is_closer(head) => {
              recover::unexpected::<Src, Ctx>(inp, BRACE_CLOSER_HEADS)?;
              continue;
            }
            Some(Collection::Map) => block_map_entry::<Src, Ctx>(inp)?,
            None if head == Kind::Identifier => block_object_field::<Src, Ctx>(inp)?,
            None => {
              recover::unexpected::<Src, Ctx>(inp, OBJECT_FIELD_HEADS)?;
              continue;
            }
          }
          at_member = false;
        }
        Some(_) => {
          recover::unexpected::<Src, Ctx>(inp, BRACE_CLOSER_HEADS)?;
          at_member = true;
        }
      }
    }
  }

  /// `= Value[Const]`, in the flavour `konst` names.
  ///
  /// A default is constant in both positions the grammar puts it in, and the two read different
  /// value grammars: a member's default is a [`Schema`](Constness::Schema) position, and a
  /// variable's an executable document's [`Const`](Constness::Const) one. So the flavour is the
  /// call site's, as every other value position's is.
  fn default_value<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::DefaultValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Equal)?;
        required_value::<Src, Ctx>(inp, konst)
      },
    )
    .parse_input(inp)
  }

  /// A value where the grammar requires exactly one: after the `:` of an argument or of an object
  /// field, after a map entry's `=>`, after a default's `=`. Opens no node of its own.
  ///
  /// A closing delimiter at the head is the closer of the block this position stands in: it is
  /// reported and left, and the block closes on it. Every other head is [`value`]'s.
  ///
  /// A list and a set call [`value`] itself and must: their loops end only because its recovery
  /// consumes, so a member position that left its head would never end.
  fn required_value<'inp, Src, Ctx>(inp, konst: Constness) {
    if peek_kind::<Src, Ctx>(inp)?.is_some_and(is_closer) {
      return recover::report_unexpected::<Src, Ctx>(inp, konst.value_heads());
    }
    value::<Src, Ctx>(inp, konst)
  }

  /// Dispatch on the value head. Opens **no** node of its own; the chosen production opens its own.
  ///
  /// Three arms have no GraphQL counterpart. A `char` literal opens a [`char_value`], a root's
  /// sigil opens a rooted enum value, and the `set`/`map` spellings inside the `Identifier` arm
  /// route to [`collection_or_enum`] rather than straight to [`enum_value`] — everything else
  /// about the identifier arm is GraphQL's, since `true`, `false` and `null` are reserved in both
  /// dialects and reserved only in a path's first segment. In a schema position every path goes
  /// to [`collection_or_enum`], which decides whether it names a typed form's type.
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
      Some(Kind::Char) => char_value::<Src, Ctx>(inp),
      Some(Kind::LBracket) => list_value::<Src, Ctx>(inp, konst),
      Some(Kind::LBrace) => object_value::<Src, Ctx>(inp, konst),
      // GraphQLx only: a root's sigil opens a rooted enum value, or in a schema position a typed
      // form's type as well. A `::` opens nothing and falls to the recovery below.
      Some(Kind::Tilde | Kind::DotDot | Kind::Caret) => match konst {
        Constness::Schema => collection_or_enum::<Src, Ctx>(inp, konst, None, false),
        Constness::Const | Constness::NonConst => enum_value::<Src, Ctx>(inp),
      },
      Some(Kind::Identifier) => match peek_as::<Src, Ctx, Keyword>(inp)? {
        Some(Keyword::True | Keyword::False) => boolean_value::<Src, Ctx>(inp),
        Some(Keyword::Null) => null_value::<Src, Ctx>(inp),
        Some(Keyword::Set) => collection_or_enum::<Src, Ctx>(
          inp,
          konst,
          Some(Opener::Collection(Collection::Set)),
          false,
        ),
        Some(Keyword::Map) => collection_or_enum::<Src, Ctx>(
          inp,
          konst,
          Some(Opener::Collection(Collection::Map)),
          false,
        ),
        // A schema position's block, before `{`; everywhere else `const` is a name.
        Some(Keyword::Const) => match konst {
          Constness::Schema => {
            collection_or_enum::<Src, Ctx>(inp, konst, Some(Opener::Block), false)
          }
          Constness::Const | Constness::NonConst => enum_value::<Src, Ctx>(inp),
        },
        // Every other spelling — including the SDL keywords and the directive-location spellings
        // `ContextualKeyword` also carries — is a perfectly good first path segment, and in a
        // schema position the type a typed form may name.
        _ => match konst {
          Constness::Schema => collection_or_enum::<Src, Ctx>(inp, konst, None, false),
          Constness::Const | Constness::NonConst => enum_value::<Src, Ctx>(inp),
        },
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

  /// `super::value` over `src`, in an executable document's **const** position — the flavour a
  /// variable's default and its directives reach.
  fn parse_const_value => value [Constness::Const];

  /// `super::value` over `src`, in a **schema** position — the flavour a member's default and every
  /// SDL directive argument reach.
  fn parse_schema_value => value [Constness::Schema];

  /// `super::default_value` over `src`, in a const position — the one value production `value`
  /// cannot reach.
  fn parse_default_value => default_value [Constness::Const];

  /// `super::map_entry` over `src`, on its own: the production a malformed `map` body would
  /// otherwise only be observable through.
  fn parse_map_entry => map_entry [Constness::NonConst];
}
