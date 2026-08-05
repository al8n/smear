//! SDL definition productions: everything from an `InputValueDefinition` up to the
//! `type_system_definition` dispatcher.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # Every definition here is a retro-wrap, because of the description
//!
//! `Description? TypeSystemDefinition` cannot be dispatched on its head: a leading string says
//! only that *some* definition follows, and the keyword that decides which one is the token after
//! it. So this module's `type_system_definition` mints a mark, commits the description, reads the
//! keyword, and hands the mark to the production it chose, which spends it on its own node —
//! putting the description **inside** the definition it describes.
//!
//! The described *members* — an input value, a field, an enum value — need no such thing: nothing
//! dispatches on their description, so each simply parses it as the first thing inside its own
//! `node(…)`.
//!
//! In both cases the description reaches the tree as a **bare string token**, because the kind
//! space has no `Description` kind. That is derived, not chosen: the Task 8 census rejects it under
//! *one token is not a region*, recording that a description is a token child of the definition's
//! own node while every string **value** is a [`StringValue`](K::StringValue) node, "so the two can
//! never be confused". `executable.rs`'s `description` is the one production, first needed by
//! Task 11's variable definitions and reached from here by import rather than by a second copy.
//!
//! # Emptiness is decided per production, against `syntactic/`
//!
//! Every `+` shape here is **reported** when it is empty, because `graphqlx/syntactic/definition/`
//! marks each one `.at_least(1)` and gate 1 compares the two suites' verdicts input by input. The
//! report is always [`crate::lossless::recover::report_unexpected`], which consumes nothing, so the
//! closer stays the loop's — the consuming form eats it, and the only witness is an extra `Error`
//! child that the text cannot show.
//!
//! # The three cross-component constraints, which no per-component port can express
//!
//! Each half is individually optional and the *combination* is not, so a production that parses
//! both and stops is wrong on a verdict rather than on a shape:
//!
//! | site | rule | expectation | anchor |
//! |---|---|---|---|
//! | object, interface, input object | a present `where` makes the **following** block mandatory | `LBrace` | `object.rs:33-34`, `interface.rs:33-34`, `input_object.rs:66-67` |
//! | union | a present `where` requires the **preceding** `= members` | `Equal` | `union.rs:87-95` |
//! | directive definition | a trailing `where` forces **nothing** | — | `definition/directive.rs:133-147` |
//!
//! Two more of the first kind live on extensions and are Task 14's
//! (`extension.rs:88`, `extension.rs:342`); the union rule has an extension twin there too
//! (`extension.rs:215-217`).
//!
//! **The union's order is the code's, not the kind space's prose.** `union_after_keyword` parses
//! `try_union_members` and *then* `try_where_clause`; `kinds.rs`'s doc comment for
//! [`UnionTypeDefinition`](K::UnionTypeDefinition) said the reverse and was corrected here rather
//! than followed.
//!
//! # Divergence 20 has no lossless expression, and that is not a gap
//!
//! GraphQLx spells the directive-location expectation `Expectation::Keyword("directive location")`
//! where GraphQL has an `Expectation::DirectiveLocation` variant. Neither reaches this suite: a
//! lossless diagnostic's expectation is derived from the *token kinds* a head set names, through
//! `lossless/mod.rs`'s `expectation_of`, and a multi-kind set falls back to `Expectation::Name`.
//! The two dialects' lossless suites therefore agree here where their syntactic suites differ.
//! Gate 1 compares verdicts and not diagnostic sets, which is the reason that is acceptable and is
//! recorded rather than assumed.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::{ParseInput as _, cst::event::EventMark};

use crate::graphqlx::kinds::SyntaxKind as K;

// `node`/`node_at` come from `coverage`, not from `tokora::parser`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being
// counted; without the feature they are tokora's own, re-exported unchanged.
use super::coverage::{node, node_at};

use super::{
  GraphqlxLosslessInput, Keyword,
  directive::directives,
  executable::{description, operation_type},
  generic::{definition_name, optional_where_clause},
  recover,
  recover::{
    DESCRIBED_MEMBER_HEADS, FIELDS_BLOCK_HEADS, NAME_HEADS, OPERATION_TYPE_HEADS, PATH_HEADS,
    ROOT_OPERATION_TYPES_HEADS, TYPE_SYSTEM_DEFINITION_HEADS, UNION_MEMBERS_HEADS, opener_span,
    starts_description,
  },
  trivia::{eat_if, expect, peek_as, peek_kind},
  ty::{type_path, type_ref},
  value::{Constness, default_value},
};

// The nineteen directive-location spellings, from Task 7's frozen macro. Written out per dialect
// it is the second place a dialect could forget one; generated, the two suites cannot disagree.
// This is the invocation that leaves `crate::lossless`'s macro set with no unused member.
crate::lossless::directive_location_predicate!(smear_lexer::graphqlx::ContextualKeyword);

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphqlx::lossless;

  /// `Description? Name : Type DefaultValue? Directives[Const]?`
  ///
  /// The one member shape shared by an argument definition and an input object's field. The name
  /// is a plain `Name` and **not** a [`DefinitionName`](K::DefinitionName): a member declares no
  /// generics, and `input_value_definition` reaches `take_name` rather than `definition_name`
  /// (`definition/input_value.rs:20`).
  fn input_value_definition<'inp, Src, Ctx>(inp) {
    node(
      K::InputValueDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if starts_description(peek_kind::<Src, Ctx>(inp)?) {
          description::<Src, Ctx>(inp)?;
        }
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        type_ref::<Src, Ctx>(inp)?;
        // Dispatched on a peek rather than attempted, so the `=` is consumed *inside* the
        // `DefaultValue` node and an absent default opens nothing.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Equal) {
          default_value::<Src, Ctx>(inp)?;
        }
        directives::<Src, Ctx>(inp, Constness::Const)
      },
    )
    .parse_input(inp)
  }

  /// `( InputValueDefinition+ )`
  ///
  /// `arguments`' loop with a different member and the opposite emptiness ruling: `syntactic/`
  /// accepts the lenient `()` for `Arguments` and rejects it here.
  fn arguments_definition<'inp, Src, Ctx>(inp) {
    node(
      K::ArgumentsDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LParen)?;
        let open = opener_span(inp.span().end());
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RParen) {
          recover::report_unexpected::<Src, Ctx>(inp, DESCRIBED_MEMBER_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RParen)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_parens::<Src, Ctx>(inp, open),
            Some(Kind::Identifier | Kind::InlineString | Kind::BlockString) => {
              input_value_definition::<Src, Ctx>(inp)?
            }
            // The head is checked here rather than left to the member's own `expect`, because that
            // `expect` would return `Err` and abort the whole list.
            Some(_) => recover::unexpected::<Src, Ctx>(inp, DESCRIBED_MEMBER_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `Description? Name ArgumentsDefinition? : Type Directives[Const]?`
  fn field_definition<'inp, Src, Ctx>(inp) {
    node(
      K::FieldDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if starts_description(peek_kind::<Src, Ctx>(inp)?) {
          description::<Src, Ctx>(inp)?;
        }
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LParen) {
          arguments_definition::<Src, Ctx>(inp)?;
        }
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        type_ref::<Src, Ctx>(inp)?;
        directives::<Src, Ctx>(inp, Constness::Const)
      },
    )
    .parse_input(inp)
  }

  /// `{ FieldDefinition+ }`
  fn fields_definition<'inp, Src, Ctx>(inp) {
    node(
      K::FieldsDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RBrace) {
          recover::report_unexpected::<Src, Ctx>(inp, DESCRIBED_MEMBER_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_object::<Src, Ctx>(inp, open),
            Some(Kind::Identifier | Kind::InlineString | Kind::BlockString) => {
              field_definition::<Src, Ctx>(inp)?
            }
            Some(_) => recover::unexpected::<Src, Ctx>(inp, DESCRIBED_MEMBER_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `{ InputValueDefinition+ }`
  ///
  /// [`fields_definition`]'s loop with the input-object member. A separate node kind rather than a
  /// shared one, because the two blocks admit different members and a typed accessor that had to
  /// distinguish them at run time would be paying for a difference the grammar already makes.
  fn input_fields_definition<'inp, Src, Ctx>(inp) {
    node(
      K::InputFieldsDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RBrace) {
          recover::report_unexpected::<Src, Ctx>(inp, DESCRIBED_MEMBER_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_object::<Src, Ctx>(inp, open),
            Some(Kind::Identifier | Kind::InlineString | Kind::BlockString) => {
              input_value_definition::<Src, Ctx>(inp)?
            }
            Some(_) => recover::unexpected::<Src, Ctx>(inp, DESCRIBED_MEMBER_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `implements &? TypePath (& TypePath)*`
  ///
  /// **Precondition: the head is an `Identifier` spelled `implements`.** Its callers decide that on
  /// the spelling.
  ///
  /// The members are [`TypePath`](K::TypePath)s, not bare names — `implements_after_keyword` calls
  /// `type_path` (`definition/implements.rs:21`) — so `implements ns::Node<Int>` is one interface.
  ///
  /// An undelimited repetition, so the trailing-trivia law applies: the loop's terminating `&` peek
  /// crosses whatever follows while the node is still open.
  fn implements_interfaces<'inp, Src, Ctx>(inp) {
    node(
      K::ImplementInterfaces.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        // A leading `&` is accepted — `implements_after_keyword` opens with `try_ampersand`.
        eat_if::<Src, Ctx>(inp, Kind::Ampersand)?;
        loop {
          if !matches!(
            peek_kind::<Src, Ctx>(inp)?,
            Some(Kind::Identifier | Kind::PathSeparator)
          ) {
            // `implements` or an `&` with nothing after it. Reported, and nothing is consumed:
            // whatever is here belongs to the next component.
            return recover::report_unexpected::<Src, Ctx>(inp, PATH_HEADS);
          }
          type_path::<Src, Ctx>(inp)?;
          // Each turn consumes an `&` before it can loop again, which is the termination argument.
          if !eat_if::<Src, Ctx>(inp, Kind::Ampersand)? {
            return Ok(());
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `= |? TypePath (| TypePath)*`
  ///
  /// **Precondition: the head is `=`.** [`implements_interfaces`]'s shape with a different opener
  /// and separator, and the same trailing-trivia law.
  fn union_member_types<'inp, Src, Ctx>(inp) {
    node(
      K::UnionMemberTypes.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Equal)?;
        eat_if::<Src, Ctx>(inp, Kind::Pipe)?;
        loop {
          if !matches!(
            peek_kind::<Src, Ctx>(inp)?,
            Some(Kind::Identifier | Kind::PathSeparator)
          ) {
            return recover::report_unexpected::<Src, Ctx>(inp, PATH_HEADS);
          }
          type_path::<Src, Ctx>(inp)?;
          if !eat_if::<Src, Ctx>(inp, Kind::Pipe)? {
            return Ok(());
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// One directive location — **no node**, the kind space having no image for one.
  ///
  /// A location is a bare `Name` token inside the enclosing
  /// [`DirectiveLocations`](K::DirectiveLocations); the census gives `Location` no kind under *one
  /// token is not a region*.
  ///
  /// **Precondition: the head is an `Identifier`.** The spelling is checked here, because the check
  /// is the production's whole content, and the token is consumed either way — the name that is
  /// there is still what the author meant by a location, and eating the diagnostic's own subject
  /// would leave nothing to point at.
  fn directive_location<'inp, Src, Ctx>(inp) {
    if !peek_as::<Src, Ctx, Keyword>(inp)?.is_some_and(is_directive_location) {
      recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)?;
    }
    expect::<Src, Ctx>(inp, Kind::Identifier)
  }

  /// `|? DirectiveLocation (| DirectiveLocation)*`
  ///
  /// **Precondition: the head is `|` or an `Identifier`.** [`directive_definition`] decides that,
  /// so this production never opens a zero-width node — the shape it would otherwise build for
  /// `directive @d on` with nothing after it.
  fn directive_locations<'inp, Src, Ctx>(inp) {
    node(
      K::DirectiveLocations.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        eat_if::<Src, Ctx>(inp, Kind::Pipe)?;
        loop {
          if peek_kind::<Src, Ctx>(inp)? != Some(Kind::Identifier) {
            return recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS);
          }
          directive_location::<Src, Ctx>(inp)?;
          if !eat_if::<Src, Ctx>(inp, Kind::Pipe)? {
            return Ok(());
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `Description? Name Directives[Const]?`
  ///
  /// # Divergence 9, the SDL half: the value is a **plain `Name` token**
  ///
  /// In a value position an enum value is a whole [`EnumValue`](K::EnumValue) over a
  /// [`Path`](K::Path); here it is one identifier and nothing more
  /// (`definition/enum_type.rs:5-28`'s `take_enum_value` returns a `Name`). GraphQL routes both
  /// through one production and GraphQLx cannot — porting the shared route nests
  /// `EnumValueDefinition > EnumValue > Path`, which re-prints identically and tells the typed
  /// layer that an SDL enum value may be qualified, which it may not.
  ///
  /// `true`, `false` and `null` are refused, and the token is still consumed for the reason
  /// [`directive_location`] records.
  fn enum_value_definition<'inp, Src, Ctx>(inp) {
    node(
      K::EnumValueDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if starts_description(peek_kind::<Src, Ctx>(inp)?) {
          description::<Src, Ctx>(inp)?;
        }
        if matches!(
          peek_as::<Src, Ctx, Keyword>(inp)?,
          Some(Keyword::True | Keyword::False | Keyword::Null)
        ) {
          recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)?;
        }
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        directives::<Src, Ctx>(inp, Constness::Const)
      },
    )
    .parse_input(inp)
  }

  /// `{ EnumValueDefinition+ }`
  fn enum_values_definition<'inp, Src, Ctx>(inp) {
    node(
      K::EnumValuesDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RBrace) {
          recover::report_unexpected::<Src, Ctx>(inp, DESCRIBED_MEMBER_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_object::<Src, Ctx>(inp, open),
            Some(Kind::Identifier | Kind::InlineString | Kind::BlockString) => {
              enum_value_definition::<Src, Ctx>(inp)?
            }
            Some(_) => recover::unexpected::<Src, Ctx>(inp, DESCRIBED_MEMBER_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `OperationType : TypePath`
  ///
  /// **Precondition: the head is an `Identifier`.** [`root_operation_types_definition`] decides
  /// that; the *spelling* is checked here, because a name that is not an operation type is far
  /// more likely to be a misspelling than junk.
  ///
  /// The target is a [`TypePath`](K::TypePath), not a bare name — `schema.rs:53` calls
  /// `type_path` — so `query: ns::Q` is one root operation type.
  fn root_operation_type_definition<'inp, Src, Ctx>(inp) {
    node(
      K::RootOperationTypeDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if !matches!(
          peek_as::<Src, Ctx, Keyword>(inp)?,
          Some(Keyword::Query | Keyword::Mutation | Keyword::Subscription)
        ) {
          recover::report_unexpected::<Src, Ctx>(inp, OPERATION_TYPE_HEADS)?;
        }
        operation_type::<Src, Ctx>(inp)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        type_path::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `{ RootOperationTypeDefinition+ }`
  fn root_operation_types_definition<'inp, Src, Ctx>(inp) {
    node(
      K::RootOperationTypesDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RBrace) {
          recover::report_unexpected::<Src, Ctx>(inp, OPERATION_TYPE_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_object::<Src, Ctx>(inp, open),
            Some(Kind::Identifier) => root_operation_type_definition::<Src, Ctx>(inp)?,
            Some(_) => recover::unexpected::<Src, Ctx>(inp, OPERATION_TYPE_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `WhereClause? FieldsDefinition?`, where a present clause makes the block **mandatory** —
  /// divergence 16, at the two sites that spell their block with fields.
  ///
  /// Written once and called by [`object_type_definition`] and [`interface_type_definition`],
  /// because a rule stated at N sites is a rule that holds at N-1 of them after the next edit.
  /// The parameter says which block production closes it; the *constraint* is the same either way,
  /// which is why the two share this function rather than a copy each.
  fn constrained_fields<'inp, Src, Ctx>(inp, input_object: bool) {
    let constrained = peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::Where);
    optional_where_clause::<Src, Ctx>(inp)?;
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
      return if input_object {
        input_fields_definition::<Src, Ctx>(inp)
      } else {
        fields_definition::<Src, Ctx>(inp)
      };
    }
    if constrained {
      // The block is absent and a `where` made it mandatory: `(Some(_), Decline) =>
      // Expectation::LBrace`. Reported without consuming, so whatever is here is still the next
      // definition's — the caller is a document loop, not a delimited block.
      return recover::report_unexpected::<Src, Ctx>(inp, FIELDS_BLOCK_HEADS);
    }
    Ok(())
  }

  /// `Description? scalar DefinitionName Directives[Const]?`
  ///
  /// **Precondition: `mark` was minted before the description, which is already committed, and the
  /// head is an `Identifier` spelled `scalar`.** [`type_system_definition`] establishes both.
  fn scalar_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::ScalarTypeDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        definition_name::<Src, Ctx>(inp)?;
        directives::<Src, Ctx>(inp, Constness::Const)
      },
    )
    .parse_input(inp)
  }

  /// `Description? type DefinitionName ImplementInterfaces? Directives[Const]? WhereClause?
  /// FieldsDefinition?`
  fn object_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::ObjectTypeDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        definition_name::<Src, Ctx>(inp)?;
        object_body::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// The `interface` form of [`object_type_definition`]'s shape.
  ///
  /// The body is shared rather than copied: the two grammars are identical after their keyword,
  /// and `syntactic/`'s `object.rs` and `interface.rs` are the same file twice.
  fn interface_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::InterfaceTypeDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        definition_name::<Src, Ctx>(inp)?;
        object_body::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `ImplementInterfaces? Directives[Const]? WhereClause? FieldsDefinition?` — the tail an object
  /// type and an interface type share, verbatim. Opens no node of its own.
  fn object_body<'inp, Src, Ctx>(inp) {
    // `implements` is contextual, so the head kind cannot decide this and the spelling must.
    if peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::Implements) {
      implements_interfaces::<Src, Ctx>(inp)?;
    }
    directives::<Src, Ctx>(inp, Constness::Const)?;
    constrained_fields::<Src, Ctx>(inp, false)
  }

  /// `Description? union DefinitionName Directives[Const]? UnionMemberTypes? WhereClause?`
  ///
  /// # Divergence 17: the clause **follows** the members and requires them
  ///
  /// The mirror image of divergence 16 in both respects. `union_after_keyword` parses
  /// `try_union_members` and *then* `try_where_clause` (`definition/union.rs:87-95`), and a clause
  /// with no members ahead of it is `Expectation::Equal` rather than `LBrace` — the syntactic
  /// parser guards the impossible fourth combination with an
  /// `unreachable!("a where clause requires union members")`, which is the tell that the
  /// constraint is real and not a tidy-up.
  fn union_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::UnionTypeDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        definition_name::<Src, Ctx>(inp)?;
        directives::<Src, Ctx>(inp, Constness::Const)?;
        constrained_union_members::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `UnionMemberTypes? WhereClause?`, where a present clause requires the **members before it** —
  /// divergence 17, at both the definition site and Task 14's extension twin.
  ///
  /// The mirror image of [`constrained_fields`] in both respects: the clause comes *after* what it
  /// constrains rather than before it, and the expectation is `Equal` rather than `LBrace`. The
  /// syntactic parser guards the impossible fourth combination with an
  /// `unreachable!("a where clause requires union members")` (`definition/union.rs:107`), which is
  /// the tell that the constraint is real and not a tidy-up.
  ///
  /// Written once and reached by both sites for the reason [`constrained_fields`] records.
  fn constrained_union_members<'inp, Src, Ctx>(inp) {
    let members = peek_kind::<Src, Ctx>(inp)? == Some(Kind::Equal);
    if members {
      union_member_types::<Src, Ctx>(inp)?;
    }
    if peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::Where) {
      if !members {
        recover::report_unexpected::<Src, Ctx>(inp, UNION_MEMBERS_HEADS)?;
      }
      optional_where_clause::<Src, Ctx>(inp)?;
    }
    Ok(())
  }

  /// `Description? enum DefinitionName Directives[Const]? EnumValuesDefinition?`
  ///
  /// The one type definition with **no** `where` clause at all — `enum_after_keyword` does not call
  /// `try_where_clause` (`definition/enum_type.rs:96-121`).
  fn enum_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::EnumTypeDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        definition_name::<Src, Ctx>(inp)?;
        directives::<Src, Ctx>(inp, Constness::Const)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          enum_values_definition::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `Description? input DefinitionName Directives[Const]? WhereClause? InputFieldsDefinition?`
  ///
  /// The third of divergence 16's five sites; the constraint is [`constrained_fields`]'s, with the
  /// input-object block.
  fn input_object_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::InputObjectTypeDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        definition_name::<Src, Ctx>(inp)?;
        directives::<Src, Ctx>(inp, Constness::Const)?;
        constrained_fields::<Src, Ctx>(inp, true)
      },
    )
    .parse_input(inp)
  }

  /// `Description? directive @ DefinitionName ArgumentsDefinition? repeatable? on
  /// DirectiveLocations WhereClause?`
  ///
  /// The one definition whose tail is mandatory in two places: the `@` before its name, and the
  /// `on` before its locations.
  ///
  /// # Divergence 18: the `where` is a trailing suffix and forces nothing
  ///
  /// It is also the position that makes the `where` continuation's *second* lookahead token
  /// load-bearing — see [`super::generic`]'s module docs. Nothing follows the clause inside this
  /// definition, so the next top-level keyword is what the continuation test must decline on.
  fn directive_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::DirectiveDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::At)?;
        definition_name::<Src, Ctx>(inp)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LParen) {
          arguments_definition::<Src, Ctx>(inp)?;
        }
        // `repeatable` is a contextual keyword and a flag, not a node: the token is in the tree and
        // the kind space has no image for it.
        if peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::Repeatable) {
          expect::<Src, Ctx>(inp, Kind::Identifier)?;
        }
        if peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::On) {
          expect::<Src, Ctx>(inp, Kind::Identifier)?;
        } else {
          // Reported, and nothing is consumed: an absent `on` leaves whatever follows to the
          // locations below.
          recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)?;
        }
        // The head check keeps `directive_locations` from opening a zero-width node for
        // `directive @d on`, and it is what makes the empty form report exactly once.
        match peek_kind::<Src, Ctx>(inp)? {
          Some(Kind::Pipe | Kind::Identifier) => directive_locations::<Src, Ctx>(inp)?,
          _ => recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)?,
        }
        optional_where_clause::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `Description? schema Directives[Const]? { RootOperationTypeDefinition+ }`
  ///
  /// The block is **not** optional — `schema_after_keyword` calls `root_operation_types_definition`
  /// directly rather than through a `try_` wrapper — so an absent one is reported. There is no
  /// `where` clause on a schema.
  fn schema_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::SchemaDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        directives::<Src, Ctx>(inp, Constness::Const)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          root_operation_types_definition::<Src, Ctx>(inp)
        } else {
          recover::report_unexpected::<Src, Ctx>(inp, ROOT_OPERATION_TYPES_HEADS)
        }
      },
    )
    .parse_input(inp)
  }

  /// Dispatch on a type-system definition's keyword, reading an optional leading description
  /// first. Opens **no** node of its own; the chosen production spends the mark on its own.
  fn type_system_definition<'inp, Src, Ctx>(inp) {
    // The head peek first, so the leading trivia it crosses is committed before the mark is minted
    // and therefore lands outside whatever the mark eventually wraps.
    let head = peek_kind::<Src, Ctx>(inp)?;
    let mark = inp.cst_mark();
    if starts_description(head) {
      description::<Src, Ctx>(inp)?;
    }
    type_system_definition_at::<Src, Ctx>(inp, mark)
  }

  /// [`type_system_definition`]'s keyword dispatch, with the description already committed and its
  /// `mark` handed in.
  ///
  /// The split is what lets `document.rs`'s three entry dispatchers share this: each of them has
  /// already read the description — it is the thing divergence 22 makes them branch on — and each
  /// minted its own mark before it.
  fn type_system_definition_at<'inp, Src, Ctx>(inp, mark: EventMark) {
    match peek_as::<Src, Ctx, Keyword>(inp)? {
      Some(Keyword::Scalar) => scalar_type_definition::<Src, Ctx>(inp, mark),
      Some(Keyword::Type) => object_type_definition::<Src, Ctx>(inp, mark),
      Some(Keyword::Interface) => interface_type_definition::<Src, Ctx>(inp, mark),
      Some(Keyword::Union) => union_type_definition::<Src, Ctx>(inp, mark),
      Some(Keyword::Enum) => enum_type_definition::<Src, Ctx>(inp, mark),
      Some(Keyword::Input) => input_object_type_definition::<Src, Ctx>(inp, mark),
      Some(Keyword::Schema) => schema_definition::<Src, Ctx>(inp, mark),
      Some(Keyword::Directive) => directive_definition::<Src, Ctx>(inp, mark),
      // A name that is none of the eight, or no name at all. Reported and skipped — `unexpected`
      // consumes at least one token whenever input remains, which is the document loops' only
      // termination argument. The mark is left unspent, and an unspent mark materializes into
      // nothing.
      _ => recover::unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_DEFINITION_HEADS),
    }
  }
}

lossless_drivers! {
  dialect = graphqlx::lossless;

  /// Drivers that run one SDL definition production over a `&str` and hand back the tree it built,
  /// for `tests/lossless_x_definition.rs`.
  mod test_support;

  /// `super::input_value_definition` over `src`.
  fn parse_input_value_definition => input_value_definition;

  /// `super::arguments_definition` over `src`.
  fn parse_arguments_definition => arguments_definition;

  /// `super::field_definition` over `src`.
  fn parse_field_definition => field_definition;

  /// `super::fields_definition` over `src`.
  fn parse_fields_definition => fields_definition;

  /// `super::input_fields_definition` over `src` — the block whose members are input values.
  fn parse_input_fields_definition => input_fields_definition;

  /// `super::implements_interfaces` over `src`.
  fn parse_implements_interfaces => implements_interfaces;

  /// `super::union_member_types` over `src`.
  fn parse_union_member_types => union_member_types;

  /// `super::directive_locations` over `src` — the only door to `directive_location`.
  fn parse_directive_locations => directive_locations;

  /// `super::enum_values_definition` over `src` — the only door to `enum_value_definition`.
  fn parse_enum_values_definition => enum_values_definition;

  /// `super::root_operation_types_definition` over `src` — the only door to
  /// `root_operation_type_definition`.
  fn parse_root_operation_types_definition => root_operation_types_definition;

  /// `super::schema_definition` over `src`.
  fn parse_schema_definition => schema_definition (mark);

  /// `super::directive_definition` over `src`.
  fn parse_directive_definition => directive_definition (mark);

  /// `super::type_system_definition` over `src` — the entry every SDL definition position uses,
  /// and the only door to the six `*_type_definition` productions.
  fn parse_type_system_definition => type_system_definition;
}
