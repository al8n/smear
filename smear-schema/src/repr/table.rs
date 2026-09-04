//! The flat table rows: type definitions, fields, input values and directive definitions.
//!
//! Every row is `Copy` and every "child list" is a [`Range32`] into a sibling table, so the whole
//! schema is a handful of `Box<[T]>`s with no interior pointers and no per-node allocation.

use super::{
  location::DirectiveLocations,
  name::{Range32, Sym},
  ty::{DefaultKind, PackedType, TypeId, TypeKind},
};

/// Per-type flags that do not deserve a field of their own.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct TypeFlags(u8);

impl TypeFlags {
  /// No flags.
  pub const EMPTY: Self = Self(0);
  /// The type carries `@oneOf` (draft §3.10).
  pub const ONE_OF: Self = Self(1 << 0);
  /// The type is part of the built-in or introspection meta-schema rather than the user's SDL.
  pub const BUILT_IN: Self = Self(1 << 1);

  /// Returns the union of two flag sets.
  #[inline]
  pub const fn union(self, other: Self) -> Self {
    Self(self.0 | other.0)
  }

  /// Returns whether every flag in `other` is set.
  #[inline]
  pub const fn contains(self, other: Self) -> bool {
    self.0 & other.0 == other.0
  }

  /// Returns the raw bits.
  #[inline]
  pub const fn bits(self) -> u8 {
    self.0
  }
}

/// A named type definition.
///
/// Which table `fields` indexes depends on `kind`: object and interface types index the schema's
/// field table, input object types index its input-value table, and every other kind leaves it
/// empty. [`Schema::fields_of`] and [`Schema::input_fields_of`] are the accessors that keep the
/// distinction from leaking.
///
/// [`Schema::fields_of`]: super::Schema::fields_of
/// [`Schema::input_fields_of`]: super::Schema::input_fields_of
#[derive(Debug, Clone, Copy)]
pub struct TypeDef {
  name: Sym,
  kind: TypeKind,
  flags: TypeFlags,
  fields: Range32,
  interfaces: Range32,
  members: Range32,
  enum_values: Range32,
  possible_start: u32,
  object_ordinal: u32,
}

impl TypeDef {
  /// The sentinel `possible_start` / `object_ordinal` for "not applicable".
  pub const NONE: u32 = u32::MAX;

  /// Assembles a row. Ranges are relative to the schema tables the row will live in.
  #[allow(clippy::too_many_arguments)]
  #[inline]
  pub const fn new(
    name: Sym,
    kind: TypeKind,
    flags: TypeFlags,
    fields: Range32,
    interfaces: Range32,
    members: Range32,
    enum_values: Range32,
    possible_start: u32,
    object_ordinal: u32,
  ) -> Self {
    Self {
      name,
      kind,
      flags,
      fields,
      interfaces,
      members,
      enum_values,
      possible_start,
      object_ordinal,
    }
  }

  /// Returns the type's interned name.
  #[inline]
  pub const fn name(&self) -> Sym {
    self.name
  }

  /// Returns the type's kind.
  #[inline]
  pub const fn kind(&self) -> TypeKind {
    self.kind
  }

  /// Returns the type's flags.
  #[inline]
  pub const fn flags(&self) -> TypeFlags {
    self.flags
  }

  /// Returns whether the type carries `@oneOf`.
  #[inline]
  pub const fn is_one_of(&self) -> bool {
    self.flags.contains(TypeFlags::ONE_OF)
  }

  /// Returns whether the type belongs to the injected meta-schema rather than the user's SDL.
  #[inline]
  pub const fn is_built_in(&self) -> bool {
    self.flags.contains(TypeFlags::BUILT_IN)
  }

  /// Returns the row's field range — into the field table for objects and interfaces, into the
  /// input-value table for input objects.
  #[inline]
  pub const fn fields(&self) -> Range32 {
    self.fields
  }

  /// Returns the row's interface range, into the interface table.
  #[inline]
  pub const fn interfaces(&self) -> Range32 {
    self.interfaces
  }

  /// Returns the row's union-member range, into the member table.
  #[inline]
  pub const fn members(&self) -> Range32 {
    self.members
  }

  /// Returns the row's enum-value range, into the enum-value table.
  #[inline]
  pub const fn enum_values(&self) -> Range32 {
    self.enum_values
  }

  /// Returns the offset of the type's possible-object bitset, or [`TypeDef::NONE`] for a
  /// non-composite type.
  #[inline]
  pub const fn possible_start(&self) -> u32 {
    self.possible_start
  }

  /// Returns the type's object ordinal — its bit position in every possible-object bitset — or
  /// [`TypeDef::NONE`] when the type is not an object.
  #[inline]
  pub const fn object_ordinal(&self) -> u32 {
    self.object_ordinal
  }
}

/// How many arguments one field definition may declare.
///
/// # It is a bound on a product execution cannot close
///
/// Not a rule of the specification — draft §3.6.1 sets no ceiling on an
/// `ArgumentsDefinition` — and not a packing limit either, which is what separates it from
/// [`MAX_WRAPPERS`](super::MAX_WRAPPERS): nothing in the representation stops the group being
/// longer. It is here because draft §6.4.1 `CoerceArgumentValues` iterates **every** declared
/// argument at **every runtime position** of the field, and only one of those two factors can be
/// bounded at execution time.
///
/// The position count is the driver's, and an executor meters it: `graphql-proto` refuses past
/// `max_response_slots` positions, and every position is charged against `max_selection_visits`
/// on its way in. The declared count is the *deployment's*, written once in SDL, and charging a
/// request for it makes the caller pay for the service's design-time width — measured against
/// that crate's shipped defaults, one unit per declared argument per position refuses a
/// full-occupancy response at about **thirteen** declared arguments, which refuses ordinary
/// input. So the factor that belongs to the deployment is bounded where the deployment states it,
/// once, and from then on the scan is `positions × MAX_FIELD_ARGUMENTS` with both factors bounded.
///
/// # Why sixty-four, and what a schema past it does instead
///
/// Six times the widest field in the public schemas this was checked against — GitHub's
/// `User.repositories` declares ten, Shopify's `products` eight, PostGraphile's generated
/// connections eight — and it holds the worst case to a small constant multiple of what the
/// response already costs: measured, one declared-argument iteration is about 2.7 ns against
/// about 55 ns for the position itself, so sixty-four of them is roughly three times the
/// position's own cost and an unbounded group is unbounded.
///
/// A schema that genuinely wants more says it the way GraphQL already asks for it: one argument
/// of an input object type. An executor hands an input object literal to the driver whole — draft
/// §6.4.1 step 5.j's coercion of a literal's *contents* is the driver's — so an input object's
/// fields are never iterated per position, and no API surface is lost by moving there.
///
/// The refusal is `SchemaErrorKind::TooManyFieldArguments` — named rather than linked, because
/// this module is also compiled standalone by `smear-noatomic`, which has no `build` feature and
/// so no error vocabulary to resolve the link against. It is raised for interface fields as well
/// as object fields. Draft §3.7 makes an interface
/// field's argument list a lower bound on every implementing field's, so an interface field past
/// this ceiling is one no object type could implement without being refused itself.
///
/// It bounds a **field's** list and nothing else. A directive definition's arguments are the
/// other list of the same shape written by the same party, they go through the same builder walk,
/// and §6.4.1 — this constant's whole mechanism — never reaches them; they are bounded by
/// [`MAX_DIRECTIVE_ARGUMENTS`], which states the product that does.
pub const MAX_FIELD_ARGUMENTS: u32 = 64;

/// A field definition on an object or interface type.
#[derive(Debug, Clone, Copy)]
pub struct FieldDef {
  name: Sym,
  ty: PackedType,
  args: Range32,
}

impl FieldDef {
  /// Assembles a field row.
  #[inline]
  pub const fn new(name: Sym, ty: PackedType, args: Range32) -> Self {
    Self { name, ty, args }
  }

  /// Returns the field's interned name.
  #[inline]
  pub const fn name(&self) -> Sym {
    self.name
  }

  /// Returns the field's result type.
  #[inline]
  pub const fn ty(&self) -> PackedType {
    self.ty
  }

  /// Returns the field's argument range, into the input-value table.
  #[inline]
  pub const fn args(&self) -> Range32 {
    self.args
  }
}

/// An input value definition: a field argument, a directive argument, or an input-object field.
#[derive(Debug, Clone, Copy)]
pub struct InputValueDef {
  name: Sym,
  ty: PackedType,
  default: DefaultKind,
}

impl InputValueDef {
  /// Assembles an input-value row.
  #[inline]
  pub const fn new(name: Sym, ty: PackedType, default: DefaultKind) -> Self {
    Self { name, ty, default }
  }

  /// Returns the input value's interned name.
  #[inline]
  pub const fn name(&self) -> Sym {
    self.name
  }

  /// Returns the input value's declared type.
  #[inline]
  pub const fn ty(&self) -> PackedType {
    self.ty
  }

  /// Returns the presence and null-ness of the declared default.
  #[inline]
  pub const fn default_kind(&self) -> DefaultKind {
    self.default
  }

  /// Returns whether the input value must be supplied — non-null type and no default
  /// (draft 5.4.3, 5.6.4).
  #[inline]
  pub const fn is_required(&self) -> bool {
    self.ty.is_non_null() && !self.default.is_present()
  }
}

/// How many arguments one directive definition may declare.
///
/// # The same product, written by the same party, read in a different pass
///
/// [`MAX_FIELD_ARGUMENTS`] bounds a *field*'s declared list because draft §6.4.1's coercion
/// iterates it at every runtime position of the field. A directive definition's list is never
/// coerced per position — an executor never reads it at all — but it is read once per **usage**,
/// and a document chooses how many usages it writes. Draft 5.4.3's presence half walks every
/// declared argument of the definition a usage names, at every directive on every selection,
/// fragment and variable definition of the request, so `usages × declared` is §6.4.1's product
/// with §6.4.1's asymmetry: the count is the client's and the width is the deployment's.
///
/// The validator bills that width to the request today — the scan spends one unit per declared
/// entry in front of the scan, which is what makes the count payable — so a directive definition
/// a thousand arguments wide refuses ordinary documents against a work ceiling for a number the
/// document did not choose. That is the outcome [`MAX_FIELD_ARGUMENTS`] rejected for fields, and
/// the population argument is the same one: bound the deployment's factor where the deployment
/// writes it, once, and the charge left over is a small constant per usage.
///
/// # Why a constant of its own, and why the same number
///
/// Of its own because the two bound different products — coercion per response position, and
/// validation per document usage — in different crates, so a number moved for one has no claim on
/// the other, and a directive's refusal naming a *field* limit would name a mechanism that does
/// not reach it. This crate already splits every field/directive argument diagnostic for the same
/// reason.
///
/// The same sixty-four because the headroom argument is the field one with more room in it: the
/// specification's own directives declare at most one argument each, and the widest directive in
/// the registries checked when the field number was chosen — Apollo Federation's `@link`, with
/// `url`, `as`, `for` and `import` — declares four. The rewrite for a directive that genuinely
/// wants more is the field one unchanged: one argument of an input object type.
///
/// The refusal is `SchemaErrorKind::TooManyDirectiveArguments` — named rather than linked, for
/// the reason [`MAX_FIELD_ARGUMENTS`] gives.
pub const MAX_DIRECTIVE_ARGUMENTS: u32 = 64;

/// A directive definition.
#[derive(Debug, Clone, Copy)]
pub struct DirectiveDef {
  name: Sym,
  locations: DirectiveLocations,
  args: Range32,
  repeatable: bool,
  built_in: bool,
}

impl DirectiveDef {
  /// Assembles a directive row.
  #[inline]
  pub const fn new(
    name: Sym,
    locations: DirectiveLocations,
    args: Range32,
    repeatable: bool,
    built_in: bool,
  ) -> Self {
    Self {
      name,
      locations,
      args,
      repeatable,
      built_in,
    }
  }

  /// Returns the directive's interned name, without the `@`.
  #[inline]
  pub const fn name(&self) -> Sym {
    self.name
  }

  /// Returns the locations the directive is valid at.
  #[inline]
  pub const fn locations(&self) -> DirectiveLocations {
    self.locations
  }

  /// Returns the directive's argument range, into the input-value table.
  #[inline]
  pub const fn args(&self) -> Range32 {
    self.args
  }

  /// Returns whether the directive may appear more than once at one location.
  #[inline]
  pub const fn is_repeatable(&self) -> bool {
    self.repeatable
  }

  /// Returns whether the directive is one of the five the specification defines.
  #[inline]
  pub const fn is_built_in(&self) -> bool {
    self.built_in
  }
}

/// The three root operation slots, in the order [`RootOperation::ALL`] enumerates them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum RootOperation {
  /// The `query` root.
  Query = 0,
  /// The `mutation` root.
  Mutation = 1,
  /// The `subscription` root.
  Subscription = 2,
}

impl RootOperation {
  /// Every root operation, in slot order.
  pub const ALL: [Self; 3] = [Self::Query, Self::Mutation, Self::Subscription];

  /// Returns the operation's slot index in the schema's `roots` array.
  #[inline]
  pub const fn index(&self) -> usize {
    *self as u8 as usize
  }

  /// Returns the operation keyword.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Query => "query",
      Self::Mutation => "mutation",
      Self::Subscription => "subscription",
    }
  }

  /// Returns the type name GraphQL defaults the root to when no `schema` definition names one.
  #[inline]
  pub const fn default_type_name(&self) -> &'static str {
    match self {
      Self::Query => "Query",
      Self::Mutation => "Mutation",
      Self::Subscription => "Subscription",
    }
  }
}

impl core::fmt::Display for RootOperation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.as_str())
  }
}

/// A type-definition row plus its id, used by lookups that return both.
pub type TypeRef<'a> = (TypeId, &'a TypeDef);
