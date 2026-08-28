//! The draft §4 introspection shape, as records **borrowed from the response**.
//!
//! One `struct` per meta-schema type, named for it, carrying exactly the fields the door reads.
//! Unknown fields are ignored — deliberately, and it is the difference between a door that works
//! against real servers and one that does not: an introspection response carries whatever the
//! *query* asked for, servers add fields ahead of the specification, and federation gateways add
//! their own.
//!
//! # Every string here is the response's own bytes, except one
//!
//! [`Name`] is a `&str` cut out of the response and proved to spell a GraphQL `Name`. It has no
//! owning variant, so "names are not copied" is a fact about the type rather than a claim about
//! the code that fills it. [`IntrospectedKind`] and `DirectiveLocation` are resolved to their
//! enums while the bytes are in hand, so a `__TypeKind` nobody defines is refused where it is read
//! rather than compared to a string one layer down — and, being enums, they carry nothing away
//! from the response at all.
//!
//! The exception is [`IntrospectedInputValue::default_value`], which is a literal a *server
//! printed* and may perfectly well contain a `"` or a `\`. It is a [`Text`], and a [`Text`]
//! allocates only when the literal actually holds a backslash.
//!
//! **Two paths at this type — names borrow, printed values may not — and that is a statement
//! about the model, not about the reader.** The reader meets [five kinds of string](super::json)
//! and decodes four of them; three of the four leave nothing behind, because a dispatch key and a
//! closed vocabulary are read in order to be *matched* and neither is a field of anything here.
//! `Name` is the one kind read raw, and the reason is exactly the sentence above it: the borrow is
//! unconditional because the type admits no alternative.
//!
//! # Required means non-null in the meta-schema
//!
//! A field that is not an `Option` here, and carries no note below, is one draft §4 declares
//! non-null: its absence is a malformed response and the reader says so before the renderer runs
//! at all. The four exceptions each name a server that omits the field:
//!
//! - `__Field.args` and `__Directive.args` — non-null in the meta-schema, universally emitted, and
//!   defaulted anyway so a hand-written fixture can leave out the empty list.
//! - `__Directive.isRepeatable` — added to the specification in 2020; a server older than that
//!   emits no such field and no such directive.
//! - `__Type.isOneOf` — nullable in the meta-schema and absent from every server predating
//!   OneOf Input Objects.
//!
//! # What is deliberately not here
//!
//! `description`, `isDeprecated`, `deprecationReason` and `specifiedByURL` are read by nothing,
//! because [`Schema`](crate::Schema) retains none of them — it is the substrate
//! validation rules stand on, and no draft §5 rule asks whether a field is deprecated. Declaring
//! them would be declaring that the door drops them, which is
//! [the module header's](super) job, not the model's. It is also why prose has exactly one slot in
//! this file: the only text a response carries that the door keeps is a default value.

use std::{boxed::Box, vec::Vec};

use super::{
  super::repr::{DirectiveLocation, TypeKind, is_name},
  json::Text,
};

/// A GraphQL `Name`, borrowed from the response.
///
/// # Why this cannot allocate, and what that costs
///
/// A `Name` is `/[_A-Za-z][_0-9A-Za-z]*/` — ASCII, and nothing in it needs a JSON escape. So the
/// check runs on **the literal's own bytes**, and a response that spells a name with a `\u` escape
/// or a backslash of any kind fails it and is refused as
/// [`InvalidName`](super::ResponseErrorKind::InvalidName), with the literal as the subject so the
/// message shows why.
///
/// That is the whole price of the borrow being unconditional, and it is the one input the door
/// used to accept and now does not, so it is named here rather than discovered: a name written as
/// a `\u` escape decodes to a name no server has a reason to escape, and honouring it would mean
/// an owning variant on this type and a copy on every name to pay for it.
///
/// **This is the only kind of string the reader matches raw, and the exemption stops here.** A
/// dispatch key and a closed vocabulary are decoded before they are compared, because neither is
/// kept and so neither has a borrow to protect; the price above buys this type's borrow and
/// nothing else's. [The five kinds](super::json) are tabulated where they are read.
///
/// The check itself is not overhead. It is [`is_name`], the interner's own admission rule, run
/// once at the earliest point the bytes exist instead of once per use downstream.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Name<'a>(&'a str);

impl<'a> Name<'a> {
  /// Admits a literal that spells a GraphQL `Name`, and nothing else.
  #[inline]
  pub(super) fn new(literal: &'a str) -> Option<Self> {
    is_name(literal.as_bytes()).then_some(Self(literal))
  }

  /// Returns the name, still borrowed from the response.
  #[inline]
  pub(super) const fn as_str(&self) -> &'a str {
    self.0
  }
}

/// One of `__TypeKind`'s eight values, resolved while the response's bytes are in hand.
///
/// Six of them name a type and two shape a *reference*, and the split is in the type because it is
/// the distinction every use makes: a member of `__Schema.types` and the base of a reference must
/// be [`Named`](Self::Named), and only a reference may be a wrapper.
///
/// The match runs on the literal's **decoded** value, unlike [`Name`]'s: this enum keeps no borrow,
/// so there is nothing here for a raw match to protect and `"OBJECT"` is the kind `OBJECT`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum IntrospectedKind {
  /// One of the six kinds that name a type.
  Named(TypeKind),
  /// `LIST`.
  List,
  /// `NON_NULL`.
  NonNull,
}

impl IntrospectedKind {
  /// Resolves a `__TypeKind` from its decoded spelling.
  pub(super) fn from_name(literal: &str) -> Option<Self> {
    Some(match literal.as_bytes() {
      b"SCALAR" => Self::Named(TypeKind::Scalar),
      b"OBJECT" => Self::Named(TypeKind::Object),
      b"INTERFACE" => Self::Named(TypeKind::Interface),
      b"UNION" => Self::Named(TypeKind::Union),
      b"ENUM" => Self::Named(TypeKind::Enum),
      b"INPUT_OBJECT" => Self::Named(TypeKind::InputObject),
      b"LIST" => Self::List,
      b"NON_NULL" => Self::NonNull,
      _ => return None,
    })
  }
}

/// `__Schema` — the root of an introspection result.
#[derive(Debug)]
pub(super) struct IntrospectedSchema<'a> {
  /// Every type the schema defines, the meta-schema and the built-in scalars included.
  pub(super) types: Vec<IntrospectedType<'a>>,
  /// The query root. Non-null in the meta-schema, and draft §3.3 requires one regardless.
  pub(super) query_type: NamedTypeRef<'a>,
  pub(super) mutation_type: Option<NamedTypeRef<'a>>,
  pub(super) subscription_type: Option<NamedTypeRef<'a>>,
  pub(super) directives: Vec<IntrospectedDirective<'a>>,
}

/// A `__Type` reduced to its name, as `__Schema`'s three root slots return it.
#[derive(Debug)]
pub(super) struct NamedTypeRef<'a> {
  pub(super) name: Option<Name<'a>>,
}

/// `__Type`, as a member of `__Schema.types`.
///
/// The five child lists are `Option` because the meta-schema makes them nullable and returns null
/// for the kinds they do not apply to: `fields` is null on a scalar, `possibleTypes` on an object,
/// and so on. Null and empty are not the same thing, and the difference is a refusal — draft §3
/// rejects an object with no fields, and the door must be able to hand that case to it rather than
/// silently rendering a type with no field block.
#[derive(Debug)]
pub(super) struct IntrospectedType<'a> {
  pub(super) kind: IntrospectedKind,
  pub(super) name: Option<Name<'a>>,
  pub(super) fields: Option<Vec<IntrospectedField<'a>>>,
  pub(super) input_fields: Option<Vec<IntrospectedInputValue<'a>>>,
  pub(super) interfaces: Option<Vec<TypeRef<'a>>>,
  pub(super) enum_values: Option<Vec<IntrospectedEnumValue<'a>>>,
  /// Read for a union and **ignored for an interface**, deliberately.
  ///
  /// A union's members are only in `possibleTypes`, so there is nowhere else to read them. An
  /// interface's implementors are derivable from the objects' own `interfaces`, which is the same
  /// path the SDL door takes and the one the build reads the implementor table out of — reading
  /// `possibleTypes` instead would make the implementor relation a second source of truth that a
  /// response could contradict.
  pub(super) possible_types: Option<Vec<TypeRef<'a>>>,
  /// `@oneOf`, the one applied directive introspection reports that
  /// [`Schema`](crate::Schema) retains.
  pub(super) is_one_of: Option<bool>,
}

/// `__Field`.
#[derive(Debug)]
pub(super) struct IntrospectedField<'a> {
  pub(super) name: Name<'a>,
  pub(super) args: Vec<IntrospectedInputValue<'a>>,
  pub(super) ty: TypeRef<'a>,
}

/// `__InputValue` — a field argument, a directive argument, or an input-object field.
#[derive(Debug)]
pub(super) struct IntrospectedInputValue<'a> {
  pub(super) name: Name<'a>,
  pub(super) ty: TypeRef<'a>,
  /// The server's printed spelling of the default, or null for no default.
  ///
  /// **The one prose slot in the model**, and therefore the one place a [`Text`] can be
  /// `Unescaped` *here*: a printed value may contain a `"` or a `\` and is kept. A dispatch key
  /// and a closed vocabulary may hold an escape too and are decoded for it, but neither is a field
  /// of anything in this file, so neither can leave an allocation behind.
  ///
  /// `Schema` keeps only [`DefaultKind`](crate::DefaultKind) — presence and
  /// null-ness — so the spelling matters exactly twice: it must parse, and it must be
  /// distinguishable from `null`. Both fall out of handing it back to the const-value parser.
  pub(super) default_value: Option<Text<'a>>,
}

/// `__EnumValue`.
#[derive(Debug)]
pub(super) struct IntrospectedEnumValue<'a> {
  pub(super) name: Name<'a>,
}

/// `__Directive`.
#[derive(Debug)]
pub(super) struct IntrospectedDirective<'a> {
  pub(super) name: Name<'a>,
  /// Resolved to the enum while the bytes are in hand, so an unknown location is refused at the
  /// literal that spells it and the renderer has nothing left to compare.
  pub(super) locations: Vec<DirectiveLocation>,
  pub(super) args: Vec<IntrospectedInputValue<'a>>,
  pub(super) is_repeatable: bool,
}

/// `__Type` in its reference position: a wrapper chain bottoming out at a named type.
///
/// `Box` on `of_type` is what makes the recursion representable. Its depth is bounded by the
/// reader's own 128-deep nesting limit, which is enforced before this type is ever constructed, so
/// neither the reader nor the renderer that walks it can be driven off the stack by a hostile
/// response.
#[derive(Debug)]
pub(super) struct TypeRef<'a> {
  pub(super) kind: IntrospectedKind,
  pub(super) name: Option<Name<'a>>,
  pub(super) of_type: Option<Box<TypeRef<'a>>>,
}
