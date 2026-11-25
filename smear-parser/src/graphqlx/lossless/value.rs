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
//! **8. Outside a block, an expression is refused at its first token,** against the block it
//! offers: an operator's token or a `(` at a schema value's head, and an operator's token, a `>`
//! or a `^` after a value where no second value may follow. One report, and then the run reads on
//! for as long as the text is an expression, into the node the refusal stands in.
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
    BLOCK_OPERAND_HEADS, BRACE_CLOSER_HEADS, COLLECTION_BODY_HEADS, COLON_HEADS, CONST_BLOCK_HEADS,
    CONST_VALUE_HEADS, FAT_ARROW_HEADS, LIST_CLOSER_HEADS, OBJECT_FIELD_HEADS,
    OPERATOR_OR_SEPARATOR_HEADS, Owners, PARENS_CLOSER_HEADS, TYPED_FORM_HEADS, VALUE_HEADS,
    begins_block_operand, closes_enclosing_block, continues_an_expression, is_closer, is_number,
    is_operator, opener_span, starts_value,
  },
  trivia::{eat_if, expect, peek_as, peek_kind, peek_second_kind, peek_signed_number},
  ty::{path, path_root, path_tail, type_generics},
};

use crate::lossless::depth::{self, JudgedTurn, RootTurn};

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

/// Where a schema position's run of an expression outside a block begins, which decides what the
/// run reads first and what it leaves: see `expression_run`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RunAt {
  /// At a value's head, in the value's place.
  Value,
  /// At a map entry's key, in the key's place.
  Key,
  /// After a map entry's key, from which the run reads on to the entry's `=>`.
  AfterKey,
  /// After a value at a member position: an argument's, an object field's, or a member's default.
  /// The next member's name, which a `:` follows, is the member loop's.
  AfterMember,
  /// After the one value a typed value holds, in front of its `)`.
  AfterValue,
}

impl RunAt {
  /// Whether the run's first token directly follows a value.
  const fn after_value(self) -> bool {
    !matches!(self, Self::Value | Self::Key)
  }

  /// Whether the run stands in a map entry's key, whose `=>` is the entry's.
  const fn at_key(self) -> bool {
    matches!(self, Self::Key | Self::AfterKey)
  }
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
        // At a schema position's member position, what goes on with an expression offers a block,
        // and the run consumes, up to the next field's name.
        Some(head)
          if konst == Constness::Schema
            && (continues_an_expression(head)
              || (is_number(head) && peek_signed_number::<Src, Ctx>(inp)?)) =>
        {
          expression_run::<Src, Ctx>(inp, RunAt::AfterMember)?
        }
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
  ///
  /// # An entry an expression stands in does not raise
  ///
  /// In a schema position a key that begins with an operator's token or a `(`, and a key that an
  /// operator's token follows, is an expression the block is offered for, and its run has reported
  /// it. A `=>` that run does not reach then ends the entry with what it has, rather than returning
  /// `Err` for the `=>` and costing the definition around the map: the refusal is already the
  /// run's, and the map's loop reads on. Any other key whose `=>` is missing returns `Err` from the
  /// `expect`, as main's entry does.
  fn map_entry<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::MapEntry.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        // A schema key that begins with an operator's token or a `(` is an expression the block is
        // offered for, and its run reads a `(` to its own `)`, so the entry keeps its `=>` and its
        // value.
        let mut ran = konst == Constness::Schema
          && peek_kind::<Src, Ctx>(inp)?
            .is_some_and(|head| is_operator(head) || head == Kind::LParen);
        if ran {
          expression_run::<Src, Ctx>(inp, RunAt::Key)?;
        } else {
          value::<Src, Ctx>(inp, konst)?;
        }
        // After a schema position's key, what goes on with an expression offers a block, and the
        // run reads on to the `=>`. The read is the one `expect` makes, in the same node.
        if konst == Constness::Schema
          && let Some(head) = peek_kind::<Src, Ctx>(inp)?
          && (continues_an_expression(head)
            || (is_number(head) && peek_signed_number::<Src, Ctx>(inp)?))
        {
          expression_run::<Src, Ctx>(inp, RunAt::AfterKey)?;
          ran = true;
        }
        if ran {
          if !eat_if::<Src, Ctx>(inp, Kind::FatArrow)? {
            return Ok(());
          }
        } else {
          expect::<Src, Ctx>(inp, Kind::FatArrow)?;
        }
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
            // A member in a schema position: an operator's token or a `(` begins an expression,
            // which `value` offers a block for, and its run consumes. A map's member is an entry
            // whose key the expression is, so the entry keeps its `=>` and its value, and a `=>`
            // the run does not reach ends the entry: see `map_entry`.
            Some(head)
              if konst == Constness::Schema && (is_operator(head) || head == Kind::LParen) =>
            {
              match collection {
                Collection::Set => value::<Src, Ctx>(inp, konst)?,
                Collection::Map => map_entry::<Src, Ctx>(inp, konst)?,
              }
            }
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
  /// With `block` the head is an operand's inside a `const { … }` block, which the productions it
  /// names read next, and what it opens is the block's own: a set's and a map's members are
  /// expressions and a `,` separates them, a typed form holds the block's members, and a block
  /// after `const` is a block inside the block, which switches nothing.
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
    block: Option<Owners>,
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
            return if block.is_some() {
              block_collection_body::<Src, Ctx>(inp, outer, collection)
            } else {
              collection_body::<Src, Ctx>(inp, outer, konst, collection)
            };
          }
          // The block's mark is the one before `const`, so the keyword and the trivia the peek
          // just crossed are the block's own first children. Only a block outside a block
          // switches the lexer's mode.
          (Opener::Block, Some(Kind::LBrace)) => {
            return block_braces::<Src, Ctx>(inp, outer, block.is_none());
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
      return typed_form::<Src, Ctx>(inp, outer, typed, konst, block);
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
  /// the rest. With `block` the form's members are a block's, and its type arguments and its
  /// parenthesis leave what the productions `block` names read next.
  fn typed_form<'inp, Src, Ctx>(
    inp,
    value: tokora::cst::EventMark,
    ty: tokora::cst::EventMark,
    konst: Constness,
    block: Option<Owners>,
  ) {
    let has_arguments = peek_kind::<Src, Ctx>(inp)? == Some(Kind::LAngle);
    node_at(
      ty,
      K::TypePath.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if has_arguments {
          type_generics::<Src, Ctx>(inp, block)
        } else {
          Ok(())
        }
      },
    )
    .parse_input(inp)?;
    match (peek_kind::<Src, Ctx>(inp)?, block) {
      (Some(Kind::LBrace), Some(_)) => block_typed_object_body::<Src, Ctx>(inp, value),
      (Some(Kind::LParen), Some(owners)) => {
        block_typed_value_body::<Src, Ctx>(inp, value, owners)
      }
      (Some(Kind::LBrace), None) => typed_object_body::<Src, Ctx>(inp, value, konst),
      (Some(Kind::LParen), None) => typed_value_body::<Src, Ctx>(inp, value, konst),
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
            // After the one value a schema position's typed value holds, what goes on with an
            // expression offers a block, and the run consumes.
            Some(head)
              if konst == Constness::Schema
                && (continues_an_expression(head)
                  || (is_number(head) && peek_signed_number::<Src, Ctx>(inp)?)) =>
            {
              expression_run::<Src, Ctx>(inp, RunAt::AfterValue)?
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
  /// first thing lexed out of the mode; and the node is the mark's. A terminal stop writes no
  /// state, so the stop stays one.
  ///
  /// # Whether a stop is latched, not where
  ///
  /// An error is judged a stop by its own mark, by a stop on record at the cursor, or by the
  /// witness: a trip recorded while the body ran with a stop still latched, wherever it latched.
  /// `state_mut` re-keys the input, which drops a latched stop, and `at_scanner_stop` is blind to
  /// one latched ahead of the cursor by a read past the staged head; the witness, its baseline
  /// taken before the body, is not, so a read of any width inside the block can never have its
  /// stop cleared by the leave. Under a rejecting emitter it still answers: the trip is counted
  /// and its boundary latched before the diagnostic is offered.
  ///
  /// Under the emitter every door installs, which records a report and goes on, nothing but a
  /// terminal error leaves the body: its expression catches every recoverable error its operand
  /// raises, and every other recovery inside it reports and reads on. So the mode is left at the
  /// block's `}` or at the end of input, with nothing staged. An emitter that refused a report
  /// would send that report's `Err` out of the body, and the lexer leaves the block before it is
  /// returned, so nothing after it is lexed in the mode.
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
        // Read once the block's own `{` is committed, so it counts that brace.
        let floor: usize = inp.state().brace_depth();
        if switch {
          inp.state_mut().enter_const_block();
        }
        let scan = inp.scanner_trip_snapshot();
        let body = block_body::<Src, Ctx>(inp, open, floor);
        if switch {
          let stopped = match &body {
            Ok(()) => false,
            Err(error) => {
              error.is_terminal()
                || inp.at_scanner_stop()
                || inp.scanner_stopped_during_attempt(scan)
            }
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
  /// expression.
  ///
  /// # Where the body ends
  ///
  /// `floor` is the lexer's count of open braces once the block's `{` is committed, and a `}` at
  /// the head with exactly `floor` open is the block's own: the head is only peeked, so it is not
  /// in the count yet. That is a module's body's rule, and every production of a block delimited by
  /// braces ends by it, an object, a typed object, a set and a map too: at its own `}` or at the
  /// end of input, and at nothing else. Two arms guard it, and no text reaches either, since inside
  /// a block no production takes a `}` it does not own: a bracket is read by the production that
  /// looks for its closer, and any other junk is a token at a time. With more braces open, a `}`
  /// closes one that something inside left open, and is one junk token; with fewer, the block's
  /// own was taken, and the block ends where it stands.
  ///
  /// The end of input leaves the block unclosed at its `{`. Anything else, what ended the
  /// expression without being the block's, is reported against the `}` and skipped as
  /// [`block_junk`] skips, so every turn returns or consumes.
  #[inline(always)]
  fn block_body<'inp, Src, Ctx>(inp, open: tokora::SimpleSpan, floor: usize) {
    block_expression::<Src, Ctx>(inp, Owners::NONE)?;
    loop {
      match peek_kind::<Src, Ctx>(inp)? {
        None => return recover::unclosed_object::<Src, Ctx>(inp, open),
        Some(Kind::RBrace) => {
          let now: usize = inp.state().brace_depth();
          if now == floor {
            return expect::<Src, Ctx>(inp, Kind::RBrace);
          }
          if now < floor {
            return Ok(());
          }
          block_stray_brace::<Src, Ctx>(inp)?;
        }
        Some(_) => block_junk::<Src, Ctx>(inp, BRACE_CLOSER_HEADS, Owners::NONE)?,
      }
    }
  }

  /// A `}` at the head of a production of a block delimited by braces, with more braces open than
  /// the production's own count: it closes one that something inside left open, and is one junk
  /// token, reported and taken into an `Error`. No text reaches it: see [`block_body`].
  fn block_stray_brace<'inp, Src, Ctx>(inp) {
    // Through the substrate's door, as every skip's report is: what is skipped is a hole.
    crate::lossless::recover::report_unexpected(inp, &[])?;
    recover::skip_one::<Src, Ctx>(inp)
  }

  /// The expression a block, a parenthesis or a block's member holds: one
  /// [`Expression`](K::Expression) node around it, always, a lone operand's too.
  ///
  /// Where its trivia lands is the rule, and it is the substrate's own: a committed token lands in
  /// the innermost node open at its commit. The leading trivia is crossed by a peek made before the
  /// node opens, so it is the caller's. The trailing trivia is crossed by the peek made inside the
  /// node after the operand, the read that finds whether the expression goes on, so it is the
  /// expression's. What ends the expression is judged after the node has closed: a `,`, a closer,
  /// a `=>`, a token `owners` holds or the end of input ends it, and anything else is reported
  /// once, against an operator or a `,`, and skipped as [`block_junk`] skips, beside the node, in
  /// its caller's.
  ///
  /// `owners` names what the productions around the expression read next, as far out as the
  /// nearest one delimited by braces: a missing operand in front of one is reported and leaves it
  /// to that production, as it leaves a closer.
  ///
  /// # The expression is the net
  ///
  /// The operand is judged as a module's body judges an item, through `depth::judge_turn`. A
  /// failure that ends the document is returned, and nothing is written. Any other has reported
  /// itself where it failed, and only the productions a constant position has raise one: a path's
  /// `::` or name, or a variable's name, each under an operand. The expression then ends where the
  /// failure left the input, and what stands there is judged as anything else that follows an
  /// operand is. So no recoverable error leaves a block, and no definition is lost to a mistake
  /// between a block's braces.
  fn block_expression<'inp, Src, Ctx>(inp, owners: Owners) {
    peek_kind::<Src, Ctx>(inp)?;
    node(
      K::Expression.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        let JudgedTurn { turn, .. }: JudgedTurn<(), GraphqlxLosslessError<'inp, Src, Ctx>> =
          depth::judge_turn(inp, |inp| block_operand::<Src, Ctx>(inp, owners));
        // A recoverable failure has reported itself, and the expression ends where it stands.
        if let RootTurn::EndsTheDocument(error) = turn {
          return Err(error);
        }
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
      Some(head) if owners.owns(head) => Ok(()),
      Some(_) => block_junk::<Src, Ctx>(inp, OPERATOR_OR_SEPARATOR_HEADS, owners),
    }
  }

  /// One operand inside a block, read wherever it stands and reported through
  /// [`BLOCK_OPERAND_HEADS`] where none begins.
  ///
  /// A closer, a token `owners` holds, or the end of input is reported and left, as a required
  /// value's is, and nothing is built there: the expression is then empty. Any other head that
  /// begins no operand is [`block_junk`]'s, inside the expression. A `$` is reported and still
  /// built, as in a constant position.
  fn block_operand<'inp, Src, Ctx>(inp, owners: Owners) {
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
      Some(Kind::LParen) => paren_value::<Src, Ctx>(inp, owners),
      Some(Kind::LBracket) => block_list_value::<Src, Ctx>(inp, owners),
      Some(Kind::LBrace) => block_object_value::<Src, Ctx>(inp),
      Some(Kind::Tilde | Kind::DotDot | Kind::Caret) => {
        collection_or_enum::<Src, Ctx>(inp, Constness::Schema, None, Some(owners))
      }
      Some(Kind::Identifier) => block_name::<Src, Ctx>(inp, owners),
      None => recover::report_unexpected::<Src, Ctx>(inp, BLOCK_OPERAND_HEADS),
      Some(head) if is_closer(head) || owners.owns(head) => {
        recover::report_unexpected::<Src, Ctx>(inp, BLOCK_OPERAND_HEADS)
      }
      Some(_) => block_junk::<Src, Ctx>(inp, BLOCK_OPERAND_HEADS, owners),
    }
  }

  /// A name at an operand's head inside a block: `true`, `false`, `null`, a block inside the
  /// block when `const` is followed by `{`, a set or a map when `set` or `map` is followed by `!`,
  /// and otherwise a path, which `<`, `{` or `(` after it makes the type of a typed form. All but
  /// the first three are [`collection_or_enum`]'s, as at a schema value's head, with `owners`.
  fn block_name<'inp, Src, Ctx>(inp, owners: Owners) {
    let opener = match peek_as::<Src, Ctx, Keyword>(inp)? {
      Some(Keyword::True | Keyword::False) => return boolean_value::<Src, Ctx>(inp),
      Some(Keyword::Null) => return null_value::<Src, Ctx>(inp),
      Some(Keyword::Set) => Some(Opener::Collection(Collection::Set)),
      Some(Keyword::Map) => Some(Opener::Collection(Collection::Map)),
      Some(Keyword::Const) => Some(Opener::Block),
      _ => None,
    };
    collection_or_enum::<Src, Ctx>(inp, Constness::Schema, opener, Some(owners))
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
  fn paren_value<'inp, Src, Ctx>(inp, owners: Owners) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::ParenValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        parenthesized::<Src, Ctx>(inp, owners)
      },
    )
    .parse_input(inp)
  }

  /// `( Expression )` inside whichever node its caller opened: a parenthesis's, and a typed value's
  /// inside a block. Opens no node, and is always inlined.
  ///
  /// A parenthesis is not delimited by braces, so it leaves what the productions around it read
  /// next: a `}`, which is never a parenthesis's own inside a block, and a token `owners` holds.
  /// In front of either it reports its `)` missing and returns. Anything else before the `)` is
  /// reported once, against the `)`, and skipped as [`block_junk`] skips.
  #[inline(always)]
  fn parenthesized<'inp, Src, Ctx>(inp, owners: Owners) {
    expect::<Src, Ctx>(inp, Kind::LParen)?;
    let open = opener_span(inp.span().end());
    let inside = owners.with(Kind::RParen);
    block_expression::<Src, Ctx>(inp, inside)?;
    loop {
      if eat_if::<Src, Ctx>(inp, Kind::RParen)? {
        return Ok(());
      }
      match peek_kind::<Src, Ctx>(inp)? {
        None => return recover::unclosed_parens::<Src, Ctx>(inp, open),
        Some(head) if head == Kind::RBrace || owners.owns(head) => {
          return recover::report_unexpected::<Src, Ctx>(inp, PARENS_CLOSER_HEADS);
        }
        // The head is none of what the skip leaves, so it consumes, and every turn returns or
        // consumes.
        Some(_) => block_junk::<Src, Ctx>(inp, PARENS_CLOSER_HEADS, inside)?,
      }
    }
  }

  /// `[ Expression (, Expression)* ,? ]` inside a block: each element one expression, and a `,`
  /// between two.
  ///
  /// A list is not delimited by braces: in front of a `}` and of a token `owners` holds it reports
  /// its `]` missing and returns, as a parenthesis does. Its own `,` and `]` are asked first, so a
  /// `,` at an element's head is an element missing, which the operand reports, and the turn after
  /// takes the `,`. After an element a `,` goes on and the `]` ends the list; anything else ended
  /// the element's expression without being the list's own, and is reported once, against the
  /// `]`, and skipped as [`block_junk`] skips. So every turn returns or consumes.
  fn block_list_value<'inp, Src, Ctx>(inp, owners: Owners) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::ListValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBracket)?;
        let open = opener_span(inp.span().end());
        let inside = owners.with(Kind::Separator).with(Kind::RBracket);
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
            Some(head)
              if head == Kind::RBrace || (head != Kind::Separator && owners.owns(head)) =>
            {
              return recover::report_unexpected::<Src, Ctx>(inp, LIST_CLOSER_HEADS);
            }
            Some(_) if at_element => {
              block_expression::<Src, Ctx>(inp, inside)?;
              at_element = false;
            }
            Some(_) => {
              block_junk::<Src, Ctx>(inp, LIST_CLOSER_HEADS, inside)?;
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
    // Read once the object's own `{` is committed, so it counts that brace.
    let floor: usize = inp.state().brace_depth();
    block_brace_members::<Src, Ctx>(inp, open, floor, None)
  }

  /// `Name : Expression` — an object field inside a block, its value an expression.
  ///
  /// The `:` is read, not expected. Where it stands it is taken; elsewhere it is reported once, and
  /// what stands in front of it is skipped as [`block_junk`] skips, with no report more, up to the
  /// `:`, or up to what the object reads next, its `,` or its `}`, or the end of input. There the
  /// field ends with what it has: a `:` that is missing costs the field its value, and never the
  /// object or the definition around it.
  fn block_object_field<'inp, Src, Ctx>(inp) {
    node(
      K::ObjectField.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        if !eat_if::<Src, Ctx>(inp, Kind::Colon)? {
          recover::report_unexpected::<Src, Ctx>(inp, COLON_HEADS)?;
          block_skip_to::<Src, Ctx>(inp, Kind::Colon, Owners::MEMBERS)?;
          if !eat_if::<Src, Ctx>(inp, Kind::Colon)? {
            return Ok(());
          }
        }
        block_expression::<Src, Ctx>(inp, Owners::MEMBERS)
      },
    )
    .parse_input(inp)
  }

  /// `Expression => Expression` — a map entry inside a block, both halves expressions.
  ///
  /// The key leaves the `=>` and the map's `,`. The `=>` is read as a field's `:` is, see
  /// [`block_object_field`]: where it is missing the entry ends with its key.
  fn block_map_entry<'inp, Src, Ctx>(inp) {
    node(
      K::MapEntry.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        block_expression::<Src, Ctx>(inp, Owners::MEMBERS.with(Kind::FatArrow))?;
        if !eat_if::<Src, Ctx>(inp, Kind::FatArrow)? {
          recover::report_unexpected::<Src, Ctx>(inp, FAT_ARROW_HEADS)?;
          block_skip_to::<Src, Ctx>(inp, Kind::FatArrow, Owners::MEMBERS)?;
          if !eat_if::<Src, Ctx>(inp, Kind::FatArrow)? {
            return Ok(());
          }
        }
        block_expression::<Src, Ctx>(inp, Owners::MEMBERS)
      },
    )
    .parse_input(inp)
  }

  /// What stands in front of a part a production inside a block reads, `part`, skipped as
  /// [`block_junk`] skips and reported by no one: the production has reported the part missing
  /// once. It stops in front of the part, and in front of what it must leave, the end of input, a
  /// `}` and a token `owners` holds, so every turn consumes or returns; a bracket it reads leaves
  /// the part too.
  fn block_skip_to<'inp, Src, Ctx>(inp, part: Kind, owners: Owners) {
    let leaves = owners.with(part);
    loop {
      match peek_kind::<Src, Ctx>(inp)? {
        None => return Ok(()),
        Some(head) if head == Kind::RBrace || leaves.owns(head) => return Ok(()),
        Some(_) => block_skip::<Src, Ctx>(inp, leaves)?,
      }
    }
  }

  /// The `! { … }` body of a `set` or a `map` inside a block, retro-wrapped from the mark
  /// [`collection_or_enum`] minted before the keyword, as [`collection_body`] is: a set's members
  /// are expressions and a map's are entries, and a `,` separates two.
  ///
  /// The `{` is read, not expected: where it is missing it is reported where it should be, and the
  /// form ends with no body.
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
        // The `!` the caller peeked.
        expect::<Src, Ctx>(inp, Kind::Bang)?;
        if !eat_if::<Src, Ctx>(inp, Kind::LBrace)? {
          return recover::report_unexpected::<Src, Ctx>(inp, COLLECTION_BODY_HEADS);
        }
        let open = opener_span(inp.span().end());
        // Read once the form's own `{` is committed, so it counts that brace.
        let floor: usize = inp.state().brace_depth();
        block_brace_members::<Src, Ctx>(inp, open, floor, Some(collection))
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
  /// parenthesis holds, and what `owners` holds left as a parenthesis leaves it.
  fn block_typed_value_body<'inp, Src, Ctx>(
    inp,
    mark: tokora::cst::EventMark,
    owners: Owners,
  ) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node_at(
      mark,
      K::TypedValue.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        parenthesized::<Src, Ctx>(inp, owners)
      },
    )
    .parse_input(inp)
  }

  /// The members of a set, a map, an object or a typed object inside a block, its `{` consumed and
  /// up to its `}`: a set's expressions or a map's entries, as `collection` names, or with none an
  /// object's fields, and a `,` between two.
  ///
  /// The `}` is the one the lexer's count gives the aggregate, by `floor`, as [`block_body`] reads
  /// the block's. After a member a `,` goes on; anything else ended the member without being the
  /// aggregate's own, and is reported once, against the `}`, and skipped as [`block_junk`] skips.
  /// The end of input leaves the aggregate unclosed at its `{`. A `,` at a member's head is the
  /// aggregate's own: a set's missing member is its operand's report, and a map's or an object's
  /// is reported here, and the `,` is taken, so the next member is read. A field's head is a name
  /// and a map entry's is never a closer: anything else there is skipped, as an object's is outside
  /// a block. So every turn returns or consumes.
  #[inline(always)]
  fn block_brace_members<'inp, Src, Ctx>(
    inp,
    open: tokora::SimpleSpan,
    floor: usize,
    collection: Option<Collection>,
  ) {
    let mut at_member = true;
    loop {
      let Some(head) = peek_kind::<Src, Ctx>(inp)? else {
        return recover::unclosed_object::<Src, Ctx>(inp, open);
      };
      match head {
        Kind::RBrace => {
          let now: usize = inp.state().brace_depth();
          if now == floor {
            return expect::<Src, Ctx>(inp, Kind::RBrace);
          }
          if now < floor {
            return Ok(());
          }
          block_stray_brace::<Src, Ctx>(inp)?;
        }
        Kind::Separator if !at_member => {
          expect::<Src, Ctx>(inp, Kind::Separator)?;
          at_member = true;
        }
        Kind::Separator if collection != Some(Collection::Set) => {
          let heads = match collection {
            Some(_) => BLOCK_OPERAND_HEADS,
            None => OBJECT_FIELD_HEADS,
          };
          recover::report_unexpected::<Src, Ctx>(inp, heads)?;
          expect::<Src, Ctx>(inp, Kind::Separator)?;
        }
        _ if at_member => {
          match collection {
            Some(Collection::Set) => block_expression::<Src, Ctx>(inp, Owners::MEMBERS)?,
            Some(Collection::Map) if is_closer(head) => {
              block_junk::<Src, Ctx>(inp, BRACE_CLOSER_HEADS, Owners::MEMBERS)?;
              continue;
            }
            Some(Collection::Map) => block_map_entry::<Src, Ctx>(inp)?,
            None if head == Kind::Identifier => block_object_field::<Src, Ctx>(inp)?,
            None => {
              block_junk::<Src, Ctx>(inp, OBJECT_FIELD_HEADS, Owners::MEMBERS)?;
              continue;
            }
          }
          at_member = false;
        }
        _ => {
          block_junk::<Src, Ctx>(inp, BRACE_CLOSER_HEADS, Owners::MEMBERS)?;
          at_member = true;
        }
      }
    }
  }

  /// A token nobody asked for, inside a `const { … }` block: the one recovery a production makes
  /// while the lexer is in the block's mode. Reported once, against `expected`, at the first token
  /// of the run, and then skipped as [`block_skip`] skips, with `owners`.
  ///
  /// Every caller has asked about what the skip leaves before it calls, the end of input, a `}`
  /// and a token `owners` holds, so here the skip consumes: a caller's loop turns once for each
  /// run or group it is handed, and returns at what it leaves. Nothing is counted and nothing is
  /// read twice.
  fn block_junk<'inp, Src, Ctx>(inp, expected: &'static [Kind], owners: Owners) {
    // Through the substrate's door, as every skip's report is: what is skipped is a hole.
    crate::lossless::recover::report_unexpected(inp, expected)?;
    block_skip::<Src, Ctx>(inp, owners)
  }

  /// One run of junk inside a block, into an `Error` node, reported by no one.
  ///
  /// It takes nothing at the end of input, at a `}`, or at a token `owners` holds, which a
  /// production around it reads next. A `(`, `[` or `{` is read by that bracket's own production,
  /// [`paren_value`], [`block_list_value`] or [`block_object_value`], with `owners`, so a group is
  /// closed by the production that looks for its closer, and that production leaves an owned token
  /// as every other does. Any other token is taken, and after it every token that neither begins an
  /// operand nor closes or separates anything, nor is held by `owners`: an operator, a `:`, a lone
  /// `<`. So a run of operators is one node and one report.
  ///
  /// The `Error` node may hold a node, a parenthesis, a list or an object read as junk. It is
  /// opened from a mark once the run is in hand, as a skipped token's is.
  fn block_skip<'inp, Src, Ctx>(inp, owners: Owners) {
    let Some(head) = peek_kind::<Src, Ctx>(inp)? else {
      return Ok(());
    };
    if head == Kind::RBrace || owners.owns(head) {
      return Ok(());
    }
    let mark = inp.cst_mark();
    match head {
      Kind::LParen => paren_value::<Src, Ctx>(inp, owners)?,
      Kind::LBracket => block_list_value::<Src, Ctx>(inp, owners)?,
      Kind::LBrace => block_object_value::<Src, Ctx>(inp)?,
      _ => {
        inp.try_expect_or_stop(|_| true)?;
        while let Some(next) = peek_kind::<Src, Ctx>(inp)? {
          if begins_block_operand(next)
            || is_closer(next)
            || matches!(next, Kind::Separator | Kind::FatArrow)
            || owners.owns(next)
          {
            break;
          }
          inp.try_expect_or_stop(|_| true)?;
        }
      }
    }
    inp.cst_start_at(mark, K::Error.raw());
    inp.cst_finish(K::Error.raw());
    Ok(())
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
        Constness::Schema => collection_or_enum::<Src, Ctx>(inp, konst, None, None),
        Constness::Const | Constness::NonConst => enum_value::<Src, Ctx>(inp),
      },
      Some(Kind::Identifier) => match peek_as::<Src, Ctx, Keyword>(inp)? {
        Some(Keyword::True | Keyword::False) => boolean_value::<Src, Ctx>(inp),
        Some(Keyword::Null) => null_value::<Src, Ctx>(inp),
        Some(Keyword::Set) => collection_or_enum::<Src, Ctx>(
          inp,
          konst,
          Some(Opener::Collection(Collection::Set)),
          None,
        ),
        Some(Keyword::Map) => collection_or_enum::<Src, Ctx>(
          inp,
          konst,
          Some(Opener::Collection(Collection::Map)),
          None,
        ),
        // A schema position's block, before `{`; everywhere else `const` is a name.
        Some(Keyword::Const) => match konst {
          Constness::Schema => {
            collection_or_enum::<Src, Ctx>(inp, konst, Some(Opener::Block), None)
          }
          Constness::Const | Constness::NonConst => enum_value::<Src, Ctx>(inp),
        },
        // Every other spelling — including the SDL keywords and the directive-location spellings
        // `ContextualKeyword` also carries — is a perfectly good first path segment, and in a
        // schema position the type a typed form may name.
        _ => match konst {
          Constness::Schema => collection_or_enum::<Src, Ctx>(inp, konst, None, None),
          Constness::Const | Constness::NonConst => enum_value::<Src, Ctx>(inp),
        },
      },
      // In a schema position an operator's token or a `(` at a value's head begins an expression,
      // which only a `const { … }` block reads: the block is offered, and the run reads on.
      Some(head) if konst == Constness::Schema && (is_operator(head) || head == Kind::LParen) => {
        expression_run::<Src, Ctx>(inp, RunAt::Value)
      }
      _ => recover::unexpected::<Src, Ctx>(inp, konst.value_heads()),
    }
  }

  /// An expression written outside a block, at the token a schema position's reader found instead
  /// of a value or of what follows one: reported once there, against a `const { … }` block, and read
  /// on, into the node its caller has open, for as long as the text goes on being an expression.
  ///
  /// `at` says where the run began, and whether that token directly follows a value. The run takes
  /// an expression's tokens, an operator's, a `>`, and a `^` after a value, into one `Error` with
  /// no report of its own, and a value that follows them, read as the value it is. A `(` where the
  /// run would read an operand is skipped to its own `)` into an `Error` of its own, and counts as
  /// a value (R3′), so a map entry's `=>` after it is the entry's. A `(` that begins the run counts
  /// as a value too, and how it is skipped is the position's: in a map entry's key, to its own `)`
  /// the same way, so the entry keeps its `=>`; anywhere else by the shared balanced skip, as junk
  /// at those positions was taken before the block was offered, so a member list keeps the members
  /// and the `)` that skip leaves it. In a key, a group that is never closed stops in front of a
  /// `=>` at its own depth, which is the entry's. A number whose token begins with `-` is a value
  /// wherever it stands, as in `HOUR-1`. Any other value that follows a value ends the run, a `(`
  /// among them, so in a list it is the next element.
  ///
  /// After a value at a member position the run leaves the next member's head: a name that a `:`
  /// follows is not an operand of the run but the next argument's, field's or member's own, and the
  /// run ends in front of it. Telling it apart takes a second token, and the read happens only
  /// there, outside a block, in a run that has already reported.
  fn expression_run<'inp, Src, Ctx>(inp, at: RunAt) {
    let at_key = at.at_key();
    let mut after_value = at.after_value();
    let leading_group = peek_kind::<Src, Ctx>(inp)? == Some(Kind::LParen);
    if leading_group && !at_key {
      recover::unexpected::<Src, Ctx>(inp, CONST_BLOCK_HEADS)?;
      after_value = true;
    } else {
      recover::report_unexpected::<Src, Ctx>(inp, CONST_BLOCK_HEADS)?;
      if leading_group {
        expression_group::<Src, Ctx>(inp, at_key)?;
        after_value = true;
      }
    }
    // Every turn consumes or returns: the tokens' arm takes at least the one it peeked, the group's
    // the `(` it peeked, and the value's arm reads a value whose head it peeked.
    loop {
      match peek_kind::<Src, Ctx>(inp)? {
        Some(head)
          if is_operator(head) || head == Kind::RAngle || (after_value && head == Kind::Caret) =>
        {
          expression_tokens::<Src, Ctx>(inp)?;
          after_value = false;
        }
        Some(Kind::LParen) if !after_value => {
          expression_group::<Src, Ctx>(inp, at_key)?;
          after_value = true;
        }
        // A number written with a `-` is a value wherever it stands in the run.
        Some(head) if is_number(head) && peek_signed_number::<Src, Ctx>(inp)? => {
          value::<Src, Ctx>(inp, Constness::Schema)?;
          after_value = true;
        }
        // The next member's name, which a `:` follows, is the member loop's.
        Some(Kind::Identifier)
          if !after_value
            && at == RunAt::AfterMember
            && peek_second_kind::<Src, Ctx>(inp)? == Some(Kind::Colon) =>
        {
          return Ok(());
        }
        Some(head) if !after_value && starts_value(head) => {
          value::<Src, Ctx>(inp, Constness::Schema)?;
          after_value = true;
        }
        _ => return Ok(()),
      }
    }
  }

  /// A run of an expression's tokens at the head, its first token any of them and every later one
  /// an operator's or a `>`, into one `Error` node, built as a skipped token's is and reported by no
  /// one: the run that reads them reported once, at its first token.
  fn expression_tokens<'inp, Src, Ctx>(inp) {
    let mark = inp.cst_mark();
    loop {
      inp.try_expect_or_stop(|_| true)?;
      match peek_kind::<Src, Ctx>(inp)? {
        Some(head) if is_operator(head) || head == Kind::RAngle => {}
        _ => break,
      }
    }
    inp.cst_start_at(mark, K::Error.raw());
    inp.cst_finish(K::Error.raw());
    Ok(())
  }

  /// A `(` the run meets where it would read an operand, and what it opens, to the `)` that
  /// balances it, into one `Error` node, built as a skipped token's is and reported by no one.
  ///
  /// The `)` is the last token it takes, so the trivia after it is its caller's, and a map entry's
  /// `=>` after it is the entry's. A `]` or a `}` it did not open closes something around the
  /// group, so it stops before one, and at the end of input, with the group unclosed: the run that
  /// reads it has reported once, and what the group leaves open is no report of its own. With
  /// `at_key` the group stands in a map entry's key, and a `=>` at the group's own depth, where
  /// nothing the group opened inside it is open, is the entry's: it stops before that too.
  fn expression_group<'inp, Src, Ctx>(inp, at_key: bool) {
    let mark = inp.cst_mark();
    // The `(` the caller peeked.
    inp.try_expect_or_stop(|_| true)?;
    let (mut parens, mut brackets, mut braces) = (1_usize, 0_usize, 0_usize);
    while parens > 0 {
      match peek_kind::<Src, Ctx>(inp)? {
        None => break,
        Some(Kind::RBracket) if brackets == 0 => break,
        Some(Kind::RBrace) if braces == 0 => break,
        Some(Kind::FatArrow) if at_key && parens == 1 && brackets == 0 && braces == 0 => break,
        Some(head) => {
          inp.try_expect_or_stop(|_| true)?;
          match head {
            Kind::LParen => parens += 1,
            Kind::RParen => parens -= 1,
            Kind::LBracket => brackets += 1,
            Kind::RBracket => brackets -= 1,
            Kind::LBrace => braces += 1,
            Kind::RBrace => braces -= 1,
            _ => {}
          }
        }
      }
    }
    inp.cst_start_at(mark, K::Error.raw());
    inp.cst_finish(K::Error.raw());
    Ok(())
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
