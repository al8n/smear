//! The schema representation: interned names, flat tables, packed type references and
//! possible-object bitsets.
//!
//! # This module is deliberately dependency-free
//!
//! Nothing here names anything outside `core` and `alloc`. Not the parser, not `tokora`, not the
//! rest of `smear` — no `crate::` path appears in any file of this directory. That is not tidiness,
//! it is the load-bearing property the design rests on, and it is checked rather than asserted:
//! the `smear-noatomic` workspace member compiles *these very files* for `thumbv6m-none-eabi`
//! (Cortex-M0+, no native compare-and-swap) as `#![no_std]` + `alloc` + `#![forbid(unsafe_code)]`
//! with an empty `[dependencies]` table. If a `use` in this directory ever reaches outside
//! `core`/`alloc`, that build fails.
//!
//! # No `Arc`, no atomics, no interior mutability
//!
//! A build-once, read-many value needs no shared-ownership primitive: validation takes
//! `&Schema`. So the "may not assume `Arc`" constraint is honoured by being vacuous rather than
//! by a feature tier — on a target with atomics a caller may wrap the schema in
//! `alloc::sync::Arc`, on one without it in `portable_atomic_util::Arc`, and this crate needs to
//! know about neither. The corollary the rest of the validator depends on: [`Schema`] is
//! `Send + Sync` by construction, which is what forbids lazy interning and per-query memoisation
//! inside it.

mod location;
/// Crate-visible rather than private so that the module's hash is reachable from the one other
/// place in `smear` that indexes names by bytes — draft §6.3's collection. Deliberately not a
/// `#[cfg(feature = …)]` re-export: this directory is compiled standalone by `smear-noatomic`,
/// which declares no features at all, and a gate naming one would warn there.
pub(crate) mod name;
mod table;
mod ty;

pub use location::{DirectiveLocation, DirectiveLocations};
pub use name::{MAX_SYMBOLS, NameIndex, Range32, Sym, is_name, is_reserved};
pub use table::{
  DirectiveDef, FieldDef, InputValueDef, MAX_DIRECTIVE_ARGUMENTS, MAX_FIELD_ARGUMENTS,
  RootOperation, TypeDef, TypeFlags, TypeRef,
};
pub use ty::{DefaultKind, MAX_WRAPPERS, PackedType, TypeId, TypeKind};

use std::boxed::Box;

/// The sentinel [`Schema::type_of_sym`] stores for a symbol that names no type.
const NOT_A_TYPE: u32 = u32::MAX;

/// A built, immutable GraphQL schema.
///
/// One owned value with no borrows of the SDL it came from: names are interned into a byte arena,
/// every child list is a range into a flat table, and schema-side default values are reduced to
/// [`DefaultKind`]. That is what lets a `Schema` be built from a `TypeSystemDocument<S>` for *any*
/// source slice type and then outlive it — `'static`-able, `Send + Sync`, and safe to hand across
/// an FFI boundary without keeping the request buffer alive.
///
/// All the cost is in the build. Everything a validation rule would otherwise derive per query is
/// precomputed here: field lookup tables sorted for binary search, interface implementors as
/// possible-object bitsets, directive locations as a `u32` mask.
#[derive(Debug)]
pub struct Schema {
  /// Every interned name's bytes, concatenated.
  pub(crate) strings: Box<[u8]>,
  /// `Sym` to its `(start, end)` range in [`Schema::strings`].
  pub(crate) spans: Box<[(u32, u32)]>,
  /// Name bytes to `Sym`.
  pub(crate) index: NameIndex,
  /// Type definitions; a [`TypeId`] indexes this table.
  pub(crate) types: Box<[TypeDef]>,
  /// `Sym` to [`TypeId`], dense over the symbol space; [`NOT_A_TYPE`] where the name is not a
  /// type's.
  pub(crate) type_of_sym: Box<[u32]>,
  /// Every field row, grouped by owning type, each group sorted by `Sym`.
  pub(crate) fields: Box<[FieldDef]>,
  /// Every input-value row — field arguments, directive arguments and input-object fields —
  /// grouped by owner, each group sorted by `Sym`.
  pub(crate) inputs: Box<[InputValueDef]>,
  /// Interface closures, grouped by implementing type.
  pub(crate) interfaces: Box<[TypeId]>,
  /// Union members, grouped by union.
  pub(crate) members: Box<[TypeId]>,
  /// Enum value symbols, grouped by enum, each group sorted.
  pub(crate) enum_values: Box<[Sym]>,
  /// Concatenated possible-object bitsets, [`Schema::possible_words`] words per composite type.
  pub(crate) possible: Box<[u64]>,
  /// How many `u64` words one possible-object bitset occupies.
  pub(crate) possible_words: u32,
  /// Object ordinal to [`TypeId`] — the inverse of [`TypeDef::object_ordinal`].
  pub(crate) objects: Box<[TypeId]>,
  /// Directive definitions, sorted by `Sym`.
  pub(crate) directives: Box<[DirectiveDef]>,
  /// The query, mutation and subscription roots, indexed by [`RootOperation::index`].
  pub(crate) roots: [Option<TypeId>; 3],
}

impl Schema {
  // -- names --------------------------------------------------------------------------------

  /// Returns an interned name's bytes.
  ///
  /// # Panics
  ///
  /// If `sym` was issued by a different schema and is out of range here. Symbols are only
  /// meaningful against the schema that produced them.
  #[inline]
  pub fn name_bytes(&self, sym: Sym) -> &[u8] {
    let (start, end) = self.spans[sym.get() as usize];
    &self.strings[start as usize..end as usize]
  }

  /// Returns an interned name.
  ///
  /// # Panics
  ///
  /// As [`Schema::name_bytes`].
  #[inline]
  pub fn name(&self, sym: Sym) -> &str {
    // The arena is ASCII by construction: the builder rejects anything outside
    // `/[_A-Za-z][_0-9A-Za-z]*/` before it is interned, so the fallback is unreachable.
    core::str::from_utf8(self.name_bytes(sym)).unwrap_or("")
  }

  /// Resolves source bytes to their symbol, or `None` when the schema never interned that name.
  ///
  /// A miss is the common case on the validation fast path — an undefined field, an undefined
  /// type — and it costs one hash and one probe.
  #[inline]
  pub fn sym(&self, bytes: &[u8]) -> Option<Sym> {
    let strings = &self.strings;
    let spans = &self.spans;
    self.index.get(bytes, |s| {
      let (start, end) = spans[s as usize];
      &strings[start as usize..end as usize]
    })
  }

  /// Returns how many distinct names the schema interned.
  #[inline]
  pub fn symbol_count(&self) -> u32 {
    self.spans.len() as u32
  }

  // -- types --------------------------------------------------------------------------------

  /// Returns every type definition, indexed by [`TypeId`].
  #[inline]
  pub fn types(&self) -> &[TypeDef] {
    &self.types
  }

  /// Returns a type definition by id.
  #[inline]
  pub fn type_def(&self, id: TypeId) -> &TypeDef {
    &self.types[id.get() as usize]
  }

  /// Resolves a type name's symbol to its id.
  #[inline]
  pub fn type_of_sym(&self, sym: Sym) -> Option<TypeId> {
    let id = *self.type_of_sym.get(sym.get() as usize)?;
    (id != NOT_A_TYPE).then(|| TypeId::new(id))
  }

  /// Resolves source bytes to a type id and its row.
  #[inline]
  pub fn type_by_name(&self, bytes: &[u8]) -> Option<TypeRef<'_>> {
    let id = self.type_of_sym(self.sym(bytes)?)?;
    Some((id, self.type_def(id)))
  }

  /// Returns the object types, in object-ordinal order.
  #[inline]
  pub fn object_types(&self) -> &[TypeId] {
    &self.objects
  }

  // -- fields and inputs --------------------------------------------------------------------

  /// Returns a type's field group — empty unless the type is composite.
  ///
  /// A union's group holds exactly `__typename`, which is what makes draft 5.3.1's "only
  /// `__typename` may be selected on a union" fall out of the ordinary field lookup instead of
  /// needing a rule of its own.
  #[inline]
  pub fn fields_of(&self, id: TypeId) -> &[FieldDef] {
    let def = self.type_def(id);
    if def.kind().is_composite() {
      def.fields().slice(&self.fields)
    } else {
      &[]
    }
  }

  /// Returns an input object's field group — empty unless the type is an input object.
  #[inline]
  pub fn input_fields_of(&self, id: TypeId) -> &[InputValueDef] {
    let def = self.type_def(id);
    match def.kind() {
      TypeKind::InputObject => def.fields().slice(&self.inputs),
      _ => &[],
    }
  }

  /// Looks a field up on an object or interface type — binary search over the sym-sorted group.
  #[inline]
  pub fn field(&self, id: TypeId, name: Sym) -> Option<&FieldDef> {
    let group = self.fields_of(id);
    group
      .binary_search_by_key(&name.get(), |f| f.name().get())
      .ok()
      .map(|i| &group[i])
  }

  /// Looks an input-object field up — binary search over the sym-sorted group.
  #[inline]
  pub fn input_field(&self, id: TypeId, name: Sym) -> Option<&InputValueDef> {
    let group = self.input_fields_of(id);
    group
      .binary_search_by_key(&name.get(), |f| f.name().get())
      .ok()
      .map(|i| &group[i])
  }

  /// Returns an argument group named by a [`FieldDef`] or a [`DirectiveDef`].
  #[inline]
  pub fn inputs(&self, args: Range32) -> &[InputValueDef] {
    args.slice(&self.inputs)
  }

  /// Looks an argument up in a group — binary search over the sym-sorted group.
  #[inline]
  pub fn input(&self, args: Range32, name: Sym) -> Option<&InputValueDef> {
    let group = self.inputs(args);
    group
      .binary_search_by_key(&name.get(), |a| a.name().get())
      .ok()
      .map(|i| &group[i])
  }

  /// Returns an enum's value symbols, sorted.
  #[inline]
  pub fn enum_values_of(&self, id: TypeId) -> &[Sym] {
    self.type_def(id).enum_values().slice(&self.enum_values)
  }

  /// Returns whether an enum defines a value — binary search over the sorted group.
  #[inline]
  pub fn has_enum_value(&self, id: TypeId, value: Sym) -> bool {
    self
      .enum_values_of(id)
      .binary_search_by_key(&value.get(), Sym::get)
      .is_ok()
  }

  /// Returns a type's interface closure.
  ///
  /// Draft §3.7 requires every transitively implemented interface to be declared, so in any
  /// schema that builds this is also the declared set.
  #[inline]
  pub fn interfaces_of(&self, id: TypeId) -> &[TypeId] {
    self.type_def(id).interfaces().slice(&self.interfaces)
  }

  /// Returns a union's member types.
  #[inline]
  pub fn members_of(&self, id: TypeId) -> &[TypeId] {
    self.type_def(id).members().slice(&self.members)
  }

  // -- directives ---------------------------------------------------------------------------

  /// Returns every directive definition, sorted by name symbol.
  #[inline]
  pub fn directives(&self) -> &[DirectiveDef] {
    &self.directives
  }

  /// Looks a directive definition up by name symbol.
  #[inline]
  pub fn directive(&self, name: Sym) -> Option<&DirectiveDef> {
    self
      .directives
      .binary_search_by_key(&name.get(), |d| d.name().get())
      .ok()
      .map(|i| &self.directives[i])
  }

  /// Looks a directive definition up by source bytes.
  #[inline]
  pub fn directive_by_name(&self, bytes: &[u8]) -> Option<&DirectiveDef> {
    self.directive(self.sym(bytes)?)
  }

  // -- roots --------------------------------------------------------------------------------

  /// Returns a root operation type, or `None` when the schema does not define one.
  ///
  /// A `None` query root cannot occur: draft §3.3 requires it and the build refuses without it.
  #[inline]
  pub fn root(&self, operation: RootOperation) -> Option<TypeId> {
    self.roots[operation.index()]
  }

  // -- possible-object bitsets --------------------------------------------------------------

  /// Returns how many `u64` words one possible-object bitset occupies.
  #[inline]
  pub fn possible_words(&self) -> u32 {
    self.possible_words
  }

  /// Returns a composite type's possible-object bitset, or `None` for a non-composite type.
  ///
  /// The bit at index `n` is set when the object type with ordinal `n` is a possible runtime type
  /// of this one: an object's set is a singleton, an interface's is its implementors (transitive),
  /// and a union's is its members.
  #[inline]
  pub fn possible_objects(&self, id: TypeId) -> Option<&[u64]> {
    let start = self.type_def(id).possible_start();
    if start == TypeDef::NONE {
      return None;
    }
    let start = start as usize;
    let end = start + self.possible_words as usize;
    self.possible.get(start..end)
  }

  /// Returns whether two composite types share a possible object type.
  ///
  /// This one word-`AND` per 64 object types is the whole of draft 5.5.2.3 — all four of its
  /// subsections — and it also serves 5.3.2's abstract/concrete parent split.
  #[inline]
  pub fn possible_objects_intersect(&self, a: TypeId, b: TypeId) -> bool {
    match (self.possible_objects(a), self.possible_objects(b)) {
      (Some(a), Some(b)) => a.iter().zip(b.iter()).any(|(x, y)| x & y != 0),
      _ => false,
    }
  }

  /// Iterates a composite type's possible object types, in ordinal order.
  pub fn possible_object_ids(&self, id: TypeId) -> impl Iterator<Item = TypeId> + '_ {
    let words = self.possible_objects(id).unwrap_or(&[]);
    words.iter().enumerate().flat_map(move |(word, bits)| {
      (0..64u32)
        .filter(move |bit| bits & (1u64 << bit) != 0)
        .map(move |bit| self.objects[(word as u32 * 64 + bit) as usize])
    })
  }

  /// Returns whether `object` is a possible runtime type of the composite type `id`.
  #[inline]
  pub fn is_possible_object(&self, id: TypeId, object: TypeId) -> bool {
    let ordinal = self.type_def(object).object_ordinal();
    if ordinal == TypeDef::NONE {
      return false;
    }
    match self.possible_objects(id) {
      Some(words) => {
        let word = (ordinal / 64) as usize;
        words
          .get(word)
          .is_some_and(|bits| bits & (1u64 << (ordinal % 64)) != 0)
      }
      None => false,
    }
  }
}

// The three static facts the whole design rests on, checked by the compiler rather than argued.
// A `Cell`, an `Rc` or a raw pointer anywhere in the tables would fail this.
const _: () = {
  const fn assert_send_sync<T: Send + Sync>() {}
  let _ = assert_send_sync::<Schema>;
  let _ = assert_send_sync::<TypeDef>;
  let _ = assert_send_sync::<PackedType>;
};
