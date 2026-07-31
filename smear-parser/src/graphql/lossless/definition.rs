//! SDL definition productions: the twenty-two node kinds a type-system definition is built
//! from, from `Description` down to `SchemaDefinition`.
//!
//! The conventions are `value.rs`'s and are not repeated here — the two kind spaces, the
//! `::<Src, Ctx>` on every generic call, and the fully spelled `node(…)` closure parameter.
//!
//! # Every definition here is a retro-wrap, because of the description
//!
//! `Description? TypeSystemDefinition` cannot be dispatched on its head: a leading string says
//! only that *some* definition follows, and the keyword that decides which one is the token
//! after it. The atom set has no two-token peek and deliberately never will — a peek that
//! crossed trivia without committing it would hand the tree's ordering to a second buffering
//! layer beside the sink's own mark/rollback discipline.
//!
//! So the dispatcher does what [`tokora::parser::node_at`] exists for: it mints a mark, commits the
//! description, reads the keyword, and hands the mark to the production it chose, which spends
//! it on its own node. The `Description` lands **inside** the definition it describes rather
//! than beside it, and each production takes exactly one extra argument to say so. Pinned by
//! `a_description_lives_inside_the_definition_it_describes`, which asserts the definition
//! node's *text* — a mark minted one token late leaves the kind vector all but unchanged.
//!
//! `apollo-parser` reaches the same tree by a different road: it peeks two tokens
//! (`peek_data_n(2)`) and then opens the node before the description. Two-token lookahead is
//! cheap there because ignored tokens are buffered rather than committed; here it is not
//! available, and the retro-wrap is the idiom that replaces it.
//!
//! # Emptiness is decided per production, against `syntactic/`
//!
//! Every `+` shape in this file — `ArgumentsDefinition`, `FieldsDefinition`,
//! `InputFieldsDefinition`, `EnumValuesDefinition`, `RootOperationTypeDefinitions`,
//! `ImplementsInterfaces`, `UnionMemberTypes`, `DirectiveLocations` — is **reported** when it is
//! empty, because `syntactic/` marks each one "nonempty" and enforces it with `.at_least(1)`,
//! and gate 1 compares the two suites' verdicts input by input. That is the opposite of Task 6's
//! `Arguments`, where `syntactic/` documents the lenient `()` as accepted. There is no house
//! rule to reach for; each production is decided against its sibling.
//!
//! # Divergences from `apollo-parser`, decided rather than inherited
//!
//! `apollo-parser`'s `grammar/{description,field,input,object,interface,union,enum_,scalar,
//! schema,directive}.rs` are the closest comparable implementations and agree on the shape of
//! every production here. They differ in four places:
//!
//! - **`Description` wraps the string token directly**, where apollo nests a `STRING_VALUE`
//!   inside its `DESCRIPTION`. A description *is* a string — `syntactic/`'s `description`
//!   returns a `StringValue` and nothing else — so the extra node would be a wrapper whose
//!   only content is the token the outer node already covers.
//! - **A location that names no directive location is reported and still consumed.** apollo
//!   reports and pops it too; the load-bearing reason here is that `syntactic/`'s `location`
//!   admits only the nineteen spellings, and gate 1 compares verdicts. Consuming it keeps the
//!   token in the `DirectiveLocations` node a diagnostic wants to point at.
//! - **`true`, `false` and `null` are reported as enum values**, matching `syntactic/`'s
//!   `take_enum_value`; the `EnumValue` node is still built, for the same reason.
//! - **Junk inside a delimited block is attributed to an `Error` node**, and a garbage *run* is
//!   skipped as one nesting-aware region — the same two divergences `value.rs` records for a
//!   list value, for the same reasons.

use smear_lexer::graphql::{ContextualKeyword, lossless::LosslessTokenKind as Kind};
use tokora::{ParseInput as _, cst::event::EventMark};

use crate::graphql::kinds::SyntaxKind as K;

// `node`/`node_at` come from `coverage`, not from `tokora::parser`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being
// counted; without the feature they are tokora's own, re-exported unchanged.
use super::coverage::{node, node_at};

use super::{
  GraphqlLosslessInput,
  directive::directives,
  recover,
  recover::{
    DESCRIBED_MEMBER_HEADS, NAME_HEADS, OPERATION_TYPE_HEADS, ROOT_OPERATION_TYPES_HEADS,
    TYPE_SYSTEM_DEFINITION_HEADS, opener_span,
  },
  trivia::{eat_if, expect, peek_as, peek_kind},
  ty::{named_type, type_ref},
  value::{Constness, default_value, enum_value},
};

/// Whether `head` opens a [`description`] — the one two-kind test this file makes repeatedly,
/// and which `document.rs`'s two dispatchers make once each.
#[inline]
pub(crate) fn starts_description(head: Option<Kind>) -> bool {
  matches!(head, Some(Kind::InlineString | Kind::BlockString))
}

/// Whether `keyword` is one of the **nineteen** spellings `DirectiveLocation` admits: the eight
/// of `ExecutableDirectiveLocation` and the eleven of `TypeSystemDirectiveLocation`.
///
/// The count is spelled out because it was wrong in prose — "eighteen" — from Task 8 until Task
/// 11's gate report; the *membership* has always been right and identical to `syntactic/`'s
/// `is_location_keyword`, so it was a miscount rather than a behaviour bug. The arms below are
/// the authority, and `valid_sdl_directive_locations_every_spelling.graphql` exercises all
/// nineteen.
///
/// The lexer already tells `QUERY` from `query` — they are different `ContextualKeyword`
/// variants — so this is a membership test over the projection and never a string comparison.
#[inline]
fn is_directive_location(keyword: ContextualKeyword) -> bool {
  matches!(
    keyword,
    ContextualKeyword::QueryLocation
      | ContextualKeyword::MutationLocation
      | ContextualKeyword::SubscriptionLocation
      | ContextualKeyword::FieldLocation
      | ContextualKeyword::FragmentDefinitionLocation
      | ContextualKeyword::FragmentSpreadLocation
      | ContextualKeyword::InlineFragmentLocation
      | ContextualKeyword::VariableDefinitionLocation
      | ContextualKeyword::SchemaLocation
      | ContextualKeyword::ScalarLocation
      | ContextualKeyword::ObjectLocation
      | ContextualKeyword::FieldDefinitionLocation
      | ContextualKeyword::ArgumentDefinitionLocation
      | ContextualKeyword::InterfaceLocation
      | ContextualKeyword::UnionLocation
      | ContextualKeyword::EnumLocation
      | ContextualKeyword::EnumValueLocation
      | ContextualKeyword::InputObjectLocation
      | ContextualKeyword::InputFieldDefinitionLocation
  )
}

lossless_production! {
  /// `StringValue` — a definition's leading documentation string.
  ///
  /// **Precondition: the head is an inline or block string.** Every caller decides that on a
  /// peek, because the decision is also what tells a described definition from an undescribed
  /// one.
  fn description<'inp, Src, Ctx>(inp) {
    node(
      K::Description.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        if eat_if::<Src, Ctx>(inp, Kind::InlineString)? {
          return Ok(());
        }
        expect::<Src, Ctx>(inp, Kind::BlockString)
      },
    )
    .parse_input(inp)
  }

  /// `Description? Name : Type DefaultValue? Directives?`
  ///
  /// The one member shape shared by an argument definition and an input object's field.
  fn input_value_definition<'inp, Src, Ctx>(inp) {
    node(
      K::InputValueDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
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
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LParen)?;
        let open = opener_span(inp.span().end());
        // The report consumes nothing, so the `)` is still the loop's to eat; checked here
        // rather than after the loop, where the diagnostic would point past the closer it is
        // about.
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
            // The head is checked here rather than left to the member's own `expect`, because
            // that `expect` would return `Err` and abort the whole list — the ruling
            // `arguments` and `object_value` both record.
            Some(_) => recover::unexpected::<Src, Ctx>(inp, DESCRIBED_MEMBER_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `Description? Name ArgumentsDefinition? : Type Directives?`
  fn field_definition<'inp, Src, Ctx>(inp) {
    node(
      K::FieldDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
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
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
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
  /// [`fields_definition`]'s loop with the input-object member. A separate node kind rather
  /// than a shared one, because the two blocks admit different members and a typed accessor
  /// that had to distinguish them at run time would be paying for a difference the grammar
  /// already makes.
  fn input_fields_definition<'inp, Src, Ctx>(inp) {
    node(
      K::InputFieldsDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
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

  /// `implements &? NamedType (& NamedType)*`
  ///
  /// **Precondition: the head is an `Identifier` spelled `implements`.** Its callers decide
  /// that on the spelling, exactly as `value`'s dispatcher decides `true`/`false`/`null`.
  ///
  /// An undelimited repetition, so Task 6's law applies: the loop's terminating `&` peek must
  /// cross the trailing trivia to learn no further interface follows, and it crosses it while
  /// the node is open. Pinned by `an_undelimited_clause_ends_with_its_trailing_trivia_inside_it`.
  fn implements_interfaces<'inp, Src, Ctx>(inp) {
    node(
      K::ImplementsInterfaces.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        // A leading `&` is accepted — `syntactic/`'s `implements_after_keyword` takes an
        // optional one, and so does the spec's `ImplementsInterfaces`.
        eat_if::<Src, Ctx>(inp, Kind::Ampersand)?;
        if peek_kind::<Src, Ctx>(inp)? != Some(Kind::Identifier) {
          // `implements` with nothing after it. Reported, and nothing is consumed: whatever is
          // here belongs to the next component, not to this one.
          return recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS);
        }
        named_type::<Src, Ctx>(inp)?;
        // Each turn consumes an `&` before it can loop again, which is this loop's whole
        // termination argument.
        while eat_if::<Src, Ctx>(inp, Kind::Ampersand)? {
          if peek_kind::<Src, Ctx>(inp)? != Some(Kind::Identifier) {
            return recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS);
          }
          named_type::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `= |? NamedType (| NamedType)*`
  ///
  /// **Precondition: the head is `=`.** [`implements_interfaces`]'s shape with a different
  /// opener and separator, and the same trailing-trivia law.
  fn union_member_types<'inp, Src, Ctx>(inp) {
    node(
      K::UnionMemberTypes.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Equal)?;
        eat_if::<Src, Ctx>(inp, Kind::Pipe)?;
        if peek_kind::<Src, Ctx>(inp)? != Some(Kind::Identifier) {
          return recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS);
        }
        named_type::<Src, Ctx>(inp)?;
        while eat_if::<Src, Ctx>(inp, Kind::Pipe)? {
          if peek_kind::<Src, Ctx>(inp)? != Some(Kind::Identifier) {
            return recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS);
          }
          named_type::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// One `DirectiveLocation` — **no node**, the kind space having no image for one.
  ///
  /// A location is a bare `Name` token inside the enclosing `DirectiveLocations`, which is the
  /// same ruling `type_condition` records for its `on NamedType`: a node kind that does not
  /// exist is not invented, and the token survives for a formatter either way.
  ///
  /// **Precondition: the head is an `Identifier`.** The spelling is checked here rather than by
  /// the caller, because the check is the production's whole content.
  fn directive_location<'inp, Src, Ctx>(inp) {
    if !peek_as::<Src, Ctx, ContextualKeyword>(inp)?.is_some_and(is_directive_location) {
      recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)?;
    }
    // Consumed either way. The name that is there is still what the author meant by a
    // location, and eating the diagnostic's own subject would leave nothing to point at.
    expect::<Src, Ctx>(inp, Kind::Identifier)
  }

  /// `|? DirectiveLocation (| DirectiveLocation)*`
  ///
  /// **Precondition: the head is `|` or an `Identifier`.** [`directive_definition`] decides
  /// that, so this production never opens a zero-width node — the shape `directive_locations`
  /// would otherwise build for `directive @d on` with nothing after it.
  ///
  /// A leading `|` is accepted: `syntactic/`'s `directive_locations` calls `allow_leading()`.
  fn directive_locations<'inp, Src, Ctx>(inp) {
    node(
      K::DirectiveLocations.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        eat_if::<Src, Ctx>(inp, Kind::Pipe)?;
        if peek_kind::<Src, Ctx>(inp)? != Some(Kind::Identifier) {
          return recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS);
        }
        directive_location::<Src, Ctx>(inp)?;
        while eat_if::<Src, Ctx>(inp, Kind::Pipe)? {
          if peek_kind::<Src, Ctx>(inp)? != Some(Kind::Identifier) {
            return recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS);
          }
          directive_location::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `Description? EnumValue Directives?`
  fn enum_value_definition<'inp, Src, Ctx>(inp) {
    node(
      K::EnumValueDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        if starts_description(peek_kind::<Src, Ctx>(inp)?) {
          description::<Src, Ctx>(inp)?;
        }
        // `syntactic/`'s `take_enum_value` refuses the three reserved spellings, and gate 1
        // compares verdicts. The node is still built from them: constness and reservedness are
        // both validation rules over the tree in this suite, and a rejected token that never
        // reached a node is a token no diagnostic can point at.
        if matches!(
          peek_as::<Src, Ctx, ContextualKeyword>(inp)?,
          Some(ContextualKeyword::True | ContextualKeyword::False | ContextualKeyword::Null)
        ) {
          recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)?;
        }
        // The same `EnumValue` node a value position builds: an enum value is an enum value
        // wherever it appears, and a typed accessor matching two node kinds for it would be
        // paying for a distinction the grammar does not make.
        enum_value::<Src, Ctx>(inp)?;
        directives::<Src, Ctx>(inp, Constness::Const)
      },
    )
    .parse_input(inp)
  }

  /// `{ EnumValueDefinition+ }`
  fn enum_values_definition<'inp, Src, Ctx>(inp) {
    node(
      K::EnumValuesDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
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

  /// `query` | `mutation` | `subscription`
  ///
  /// **Precondition: the head is an `Identifier` spelled as one of the three.** Both callers
  /// decide that on the spelling.
  ///
  /// **This is the production Task 7 deferred.** `operation_definition` consumed its keyword as
  /// a bare `Name` because `OperationType` is this task's kind; both positions are unified here,
  /// so a formatter or a typed accessor finds the same node whether the keyword introduces an
  /// operation or names a root operation type. `apollo-parser` has always wrapped it.
  fn operation_type<'inp, Src, Ctx>(inp) {
    node(
      K::OperationType.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)
      },
    )
    .parse_input(inp)
  }

  /// `OperationType : NamedType`
  ///
  /// **Precondition: the head is an `Identifier`.** [`root_operation_type_definitions`] decides
  /// that; the *spelling* is checked here, because a name that is not an operation type is far
  /// more likely to be a misspelling than junk — the same reading `type_condition` gives a
  /// missing `on`.
  fn root_operation_type_definition<'inp, Src, Ctx>(inp) {
    node(
      K::RootOperationTypeDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        if !matches!(
          peek_as::<Src, Ctx, ContextualKeyword>(inp)?,
          Some(
            ContextualKeyword::Query
              | ContextualKeyword::Mutation
              | ContextualKeyword::Subscription
          )
        ) {
          recover::report_unexpected::<Src, Ctx>(inp, OPERATION_TYPE_HEADS)?;
        }
        operation_type::<Src, Ctx>(inp)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        named_type::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `{ RootOperationTypeDefinition+ }`
  fn root_operation_type_definitions<'inp, Src, Ctx>(inp) {
    node(
      K::RootOperationTypeDefinitions.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
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

  /// `Description? scalar Name Directives?`
  ///
  /// **Precondition: `mark` was minted before the description, and the head is an `Identifier`
  /// spelled `scalar`.** [`type_system_definition`] establishes both; see the module docs for
  /// why the mark is a parameter rather than something this production mints.
  fn scalar_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::ScalarTypeDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        directives::<Src, Ctx>(inp, Constness::Const)
      },
    )
    .parse_input(inp)
  }

  /// `Description? type Name ImplementsInterfaces? Directives? FieldsDefinition?`
  ///
  /// Precondition as for [`scalar_type_definition`], with the keyword `type`.
  fn object_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::ObjectTypeDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        object_body::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `Description? interface Name ImplementsInterfaces? Directives? FieldsDefinition?`
  ///
  /// Precondition as for [`scalar_type_definition`], with the keyword `interface`. The body is
  /// [`object_type_definition`]'s, shared rather than copied: the two grammars are identical
  /// after their keyword, and `syntactic/`'s `object.rs` and `interface.rs` are the same file
  /// twice.
  fn interface_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::InterfaceTypeDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        object_body::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `ImplementsInterfaces? Directives? FieldsDefinition?` — the tail an object type and an
  /// interface type share, verbatim. Opens no node of its own.
  fn object_body<'inp, Src, Ctx>(inp) {
    // `implements` is contextual, so the head kind cannot decide this and the spelling must.
    if peek_as::<Src, Ctx, ContextualKeyword>(inp)? == Some(ContextualKeyword::Implements) {
      implements_interfaces::<Src, Ctx>(inp)?;
    }
    directives::<Src, Ctx>(inp, Constness::Const)?;
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
      fields_definition::<Src, Ctx>(inp)?;
    }
    Ok(())
  }

  /// `Description? union Name Directives? UnionMemberTypes?`
  fn union_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::UnionTypeDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        directives::<Src, Ctx>(inp, Constness::Const)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Equal) {
          union_member_types::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `Description? enum Name Directives? EnumValuesDefinition?`
  fn enum_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::EnumTypeDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        directives::<Src, Ctx>(inp, Constness::Const)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          enum_values_definition::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `Description? input Name Directives? InputFieldsDefinition?`
  fn input_object_type_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::InputObjectTypeDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        directives::<Src, Ctx>(inp, Constness::Const)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          input_fields_definition::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `Description? directive @ Name ArgumentsDefinition? repeatable? on DirectiveLocations`
  ///
  /// The one definition whose tail is mandatory in two places: the `@` before its name, and the
  /// `on` before its locations. `syntactic/` requires both, so both are reported here.
  fn directive_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::DirectiveDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::At)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LParen) {
          arguments_definition::<Src, Ctx>(inp)?;
        }
        // `repeatable` is a contextual keyword and a flag, not a node: the token is in the tree
        // and the kind space has no image for it.
        if peek_as::<Src, Ctx, ContextualKeyword>(inp)? == Some(ContextualKeyword::Repeatable) {
          expect::<Src, Ctx>(inp, Kind::Identifier)?;
        }
        if peek_as::<Src, Ctx, ContextualKeyword>(inp)? == Some(ContextualKeyword::On) {
          expect::<Src, Ctx>(inp, Kind::Identifier)?;
        } else {
          // Reported, and nothing is consumed: an absent `on` leaves whatever follows to the
          // locations below, exactly as `type_condition` leaves its type.
          recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)?;
        }
        // The head check keeps `directive_locations` from opening a zero-width node for
        // `directive @d on` — and it is what makes the empty form report exactly once.
        match peek_kind::<Src, Ctx>(inp)? {
          Some(Kind::Pipe | Kind::Identifier) => directive_locations::<Src, Ctx>(inp),
          _ => recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS),
        }
      },
    )
    .parse_input(inp)
  }

  /// `Description? schema Directives? { RootOperationTypeDefinition+ }`
  ///
  /// The block is **not** optional — `syntactic/`'s `schema_after_keyword` calls
  /// `root_operation_types_definition` directly rather than through a `try_` wrapper — so an
  /// absent one is reported.
  fn schema_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::SchemaDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        directives::<Src, Ctx>(inp, Constness::Const)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          root_operation_type_definitions::<Src, Ctx>(inp)
        } else {
          recover::report_unexpected::<Src, Ctx>(inp, ROOT_OPERATION_TYPES_HEADS)
        }
      },
    )
    .parse_input(inp)
  }

  /// Dispatch on a type-system definition's keyword, spending `mark` on the node it chooses.
  /// Opens **no** node of its own.
  ///
  /// **Precondition: `mark` was minted after the caller's head peek and any description has
  /// already been committed.** The caller is [`described_type_system_definition`] or the
  /// document-level dispatcher in `document.rs`.
  fn type_system_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    match peek_as::<Src, Ctx, ContextualKeyword>(inp)? {
      Some(ContextualKeyword::Scalar) => scalar_type_definition::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Type) => object_type_definition::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Interface) => interface_type_definition::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Union) => union_type_definition::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Enum) => enum_type_definition::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Input) => input_object_type_definition::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Schema) => schema_definition::<Src, Ctx>(inp, mark),
      Some(ContextualKeyword::Directive) => directive_definition::<Src, Ctx>(inp, mark),
      // A name that is none of the eight, or no name at all. Reported and skipped —
      // `unexpected` consumes at least one token whenever input remains, which is the document
      // loops' only termination argument. The mark is left unspent, and an unspent mark
      // materializes into nothing.
      _ => recover::unexpected::<Src, Ctx>(inp, TYPE_SYSTEM_DEFINITION_HEADS),
    }
  }
}
