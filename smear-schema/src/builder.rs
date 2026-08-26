//! Turning one or more `TypeSystemDocument`s into a [`Schema`], and refusing the ones that are
//! not schemas.
//!
//! # Everything happens here, once
//!
//! The builder interns, merges extensions, injects the built-in and introspection definitions,
//! resolves every type reference, runs draft §3 "Type Validation", and flattens the result into
//! the tables validation reads. A server pays for all of it at startup and nothing per request —
//! which is the whole reason the representation looks the way it does.
//!
//! # The source type stops at the door
//!
//! [`SchemaBuilder::document`] is generic over the document's source slice `S`, bounded by
//! `AsRef<[u8]>` — the bound the entire `tokora` source lattice satisfies, from `&str` through
//! `bytes::Bytes` to `HipStr`. Names are copied into the schema's own arena as they are interned,
//! so the finished [`Schema`] borrows nothing and a single builder may be fed documents with
//! *different* `S` types. The generic parameter never appears on `Schema`.

use std::{
  borrow::ToOwned,
  boxed::Box,
  collections::BTreeMap,
  string::{String, ToString},
  vec,
  vec::Vec,
};

use tokora::{Parse as _, Parser, SimpleSpan, span::AsSpan};

use smear_parser::graphql::{
  GraphQL,
  ast::{
    ConstDirectives, ConstInputValue, DirectiveDefinition, EnumTypeDefinition,
    EnumValuesDefinition, FieldsDefinition, ImplementInterfaces, InputFieldsDefinition,
    InputObjectTypeDefinition, InputValueDefinition, InterfaceTypeDefinition, Location, Name,
    ObjectTypeDefinition, OperationType, ScalarTypeDefinition, SchemaDefinition, Type,
    TypeDefinition, TypeExtension, TypeSystemDefinition, TypeSystemDefinitionOrExtension,
    TypeSystemDocument, TypeSystemExtension, UnionMemberTypes, UnionTypeDefinition,
  },
  error::GraphqlErrors,
  syntactic::{GraphqlLexer, type_system_document},
};

use self::declared::{Args, ArgsMut, Declared};
use super::{
  builtin,
  error::{SchemaError, SchemaErrorKind, SchemaErrors, directive_coordinate, owner_path},
  literal::{BuiltInScalar, LiteralShape},
  repr::{
    DefaultKind, DirectiveDef, DirectiveLocation, DirectiveLocations, FieldDef, InputValueDef,
    MAX_DIRECTIVE_ARGUMENTS, MAX_FIELD_ARGUMENTS, MAX_SYMBOLS, NameIndex, PackedType, Range32,
    RootOperation, Schema, Sym, TypeDef, TypeFlags, TypeId, TypeKind, is_name, is_reserved,
  },
};

/// The placeholder a type reference carries until its base name is resolved.
const UNRESOLVED: TypeId = TypeId::new(u32::MAX);

/// The specified `@deprecated` directive, matched by name off the *applied* list.
///
/// By name rather than by identity, for the reason [`BuiltInScalar::from_name`] reads names: a
/// document may spell `directive @deprecated(…)` out and replace the injected definition — a
/// printed schema does — and it is still the specification's directive.
const DEPRECATED: &str = "deprecated";

/// The specified `@oneOf` directive. Matched by name, for the reason [`DEPRECATED`] is.
const ONE_OF: &str = "oneOf";

// ---------------------------------------------------------------------------------------------
// interning
// ---------------------------------------------------------------------------------------------

/// The growable half of the name arena.
///
/// The finished [`Schema`] keeps `strings` and `spans` and a probe-only [`NameIndex`]; this map
/// exists only while building, which is why the schema has no hash map in it at all.
#[derive(Debug, Default)]
struct Interner {
  strings: Vec<u8>,
  spans: Vec<(u32, u32)>,
  map: BTreeMap<Box<[u8]>, u32>,
}

impl Interner {
  fn intern(&mut self, bytes: &[u8]) -> Sym {
    if let Some(sym) = self.map.get(bytes) {
      return Sym::new(*sym);
    }
    let start = self.strings.len() as u32;
    self.strings.extend_from_slice(bytes);
    let end = self.strings.len() as u32;
    let sym = self.spans.len() as u32;
    self.spans.push((start, end));
    self.map.insert(bytes.to_owned().into_boxed_slice(), sym);
    Sym::new(sym)
  }

  fn lookup(&self, bytes: &[u8]) -> Option<Sym> {
    self.map.get(bytes).copied().map(Sym::new)
  }

  fn bytes(&self, sym: Sym) -> &[u8] {
    let (start, end) = self.spans[sym.get() as usize];
    &self.strings[start as usize..end as usize]
  }

  fn text(&self, sym: Sym) -> &str {
    // ASCII by construction; every name is admitted through `is_name` first.
    core::str::from_utf8(self.bytes(sym)).unwrap_or("")
  }

  fn len(&self) -> u32 {
    self.spans.len() as u32
  }
}

// ---------------------------------------------------------------------------------------------
// the owned intermediate model
// ---------------------------------------------------------------------------------------------

/// An interned name with the position and document it was read from.
#[derive(Debug, Clone, Copy)]
struct Located {
  sym: Sym,
  span: SimpleSpan,
  document: u32,
}

/// The head of an owner path: a name the arena holds, or the `schema` definition, which has none.
#[derive(Debug, Clone, Copy)]
enum Head {
  Schema,
  Named(Sym),
}

/// Where a §3 diagnostic is reported, held as symbols and rendered only if one is reported.
///
/// # Why not the `String` it prints as
///
/// Every owner path §3 prints is a type or directive name, then at most a member of it, then at
/// most an argument of that member, and — for a directive *usage* — the `@name` that keeps a
/// usage's arguments from reading as the element's own. Four symbols say all of them.
///
/// Building the `String` instead costs an allocation and a copy per *candidate*, and the passes
/// have far more candidates than diagnostics: an empty build injects ninety-odd built-in items and
/// reports nothing about any of them. `validate_directive_usages` cost 6.4 µs on a schema with
/// zero directive usages, all of it owner and path strings built before the empty list was
/// looked at.
#[derive(Debug, Clone, Copy)]
struct Coordinate {
  head: Head,
  member: Option<Sym>,
  argument: Option<Sym>,
  /// The `@name` a directive *usage*'s coordinate ends with.
  directive: Option<Sym>,
}

impl Coordinate {
  const fn schema() -> Self {
    Self {
      head: Head::Schema,
      member: None,
      argument: None,
      directive: None,
    }
  }

  const fn named(head: Sym) -> Self {
    Self {
      head: Head::Named(head),
      member: None,
      argument: None,
      directive: None,
    }
  }

  /// The same owner, one segment deeper: a field of a type, or an argument of a field.
  const fn then(self, segment: Sym) -> Self {
    match (self.member, self.argument) {
      (None, _) => Self {
        member: Some(segment),
        ..self
      },
      (Some(_), None) => Self {
        argument: Some(segment),
        ..self
      },
      // Unreachable: no §3 owner is four names deep. Deepening a full path would silently drop a
      // segment, so the last one wins rather than vanishing.
      (Some(_), Some(_)) => Self {
        argument: Some(segment),
        ..self
      },
    }
  }

  /// The coordinate of a directive *usage* written on this element.
  const fn at_directive(self, directive: Sym) -> Self {
    Self {
      directive: Some(directive),
      ..self
    }
  }
}

/// Which input-value list a check is looking at, since it cannot hold a borrow of one.
#[derive(Debug, Clone, Copy)]
enum ArgumentsOf {
  Field { ty: usize, field: usize },
  Directive { index: usize },
}

/// Which directive-usage list a check is looking at, for the reason [`ArgumentsOf`] exists.
#[derive(Debug, Clone, Copy)]
enum DirectivesOf {
  Schema,
  Type { ty: usize },
  Field { ty: usize, field: usize },
  FieldArgument { ty: usize, field: usize, arg: usize },
  InputField { ty: usize, field: usize },
  EnumValue { ty: usize, value: usize },
  DirectiveArgument { directive: usize, arg: usize },
}

/// What one declared argument list's three §3.6.1 rules are called.
///
/// A field's arguments and a directive definition's are the same three rules read twice, under six
/// names: draft §3.6.1(2.1) uniqueness, (2.2) the reserved prefix and (2.4.2) "must be an input
/// type". The caller supplies the names its half of the specification uses, and they travel as one
/// value for the reason [`Blame`] does — three parameters that are always passed together are one
/// parameter.
#[derive(Debug, Clone, Copy)]
struct ArgumentRules {
  duplicate: SchemaErrorKind,
  reserved: SchemaErrorKind,
  not_input: SchemaErrorKind,
}

/// What a failing literal is reported as, in symbols rather than in text.
#[derive(Debug, Clone, Copy)]
struct Blame {
  owner: Coordinate,
  subject: Sym,
  /// What the caller wants "this literal does not fit" called. See
  /// [`check_const_value`](SchemaBuilder::check_const_value).
  mismatch: SchemaErrorKind,
  document: u32,
}

/// The graph node a type reference reaches, dropped when the reference never resolved.
///
/// See [`SchemaBuilder::cyclic_directives`] for what the offset addresses.
fn push_type(base: TypeId, first_type_node: u32, reaches: &mut Vec<u32>) {
  if base != UNRESOLVED {
    reaches.push(first_type_node + base.get());
  }
}

/// The width at or below which a list finds its duplicates by scanning the names before them.
///
/// Sixty-four because that is where this crate has already measured the scan's quadratic term to
/// sit below the per-list overhead around it — the widest permitted argument list built in 0.060
/// ms, and one sixty-four-argument list was *cheaper* than sixty-four one-argument lists — and
/// because it is [`MAX_FIELD_ARGUMENTS`], so a declared argument list, which a ceiling already
/// holds at that width, never builds an index it could not need. al8n/smear#198.
const NARROW_LIST: usize = 64;

/// Which earlier position of a name list wrote the name at a given position.
///
/// # The shape this replaces
///
/// Every duplicate-name rule in draft §3 asks "was this name already written in this list", and
/// the obvious way to answer it is to keep the names written so far and scan them:
/// `seen.iter().find(|(sym, _)| *sym == name)`. That was written eight times in this file, and
/// **one** of the eight has a ceiling on it — [`MAX_FIELD_ARGUMENTS`] and
/// [`MAX_DIRECTIVE_ARGUMENTS`] hold a *declared* argument list at sixty-four. Nothing bounds a
/// type's fields, its `implements` list, a union's members, an enum's values, an input object's
/// fields, the directives written at one location, or the arguments written at one usage, so on
/// each of those seven the scan is `Θ(list²)` over a width the document chooses.
///
/// A ceiling is the wrong instrument there, and it is the argument that chose one for the
/// argument lists read the other way round: four-figure enum-value and field lists are ordinary
/// in real schemas, so a number that refuses them refuses valid input. What is wrong is not the
/// width, it is the cost per unit of width. Measured on `type Query` with N one-argument fields,
/// `Schema::build` alone with the parse outside the clock: 0.639 ms at 1 k, 3.749 at 4 k, 44.128
/// at 16 k and **585.305 at 64 k** — top-step exponent 1.86, against 127.577 ms for a control
/// spreading the same declarations over N types with every list length one. al8n/smear#198.
///
/// # Sorted pairs, and why `first` is still the first
///
/// [`Duplicates::Index`] pairs every position with the name written there and sorts once. Sorting
/// the *pair* orders equal names by ascending position, so the head of each run is the first
/// occurrence rather than merely one of them, and the answer read back at position `p` is the
/// same position the trail would have been holding when the walk reached `p`. That is what keeps
/// the diagnostic still: [`SchemaBuilder::push_related`] relates a duplicate to the span of the
/// **first** occurrence, and these rules report in source order, so an index that resolved `first`
/// by whatever the sort put in front would move a blessed diagnostic while looking like a cost
/// change.
///
/// # What it is not
///
/// **Not a hash map.** A `HashMap<Sym, u32>` per list allocates and hashes where an integer sort
/// does neither, and the finished [`Schema`] deliberately holds no hash map at all — see
/// [`Interner`], which is the one in the workspace and exists only while building.
///
/// **Not a table indexed by [`Sym`].** That is the shape `type_of_sym` and `directive_of_sym`
/// already have here, it is `O(1)` rather than `O(list log list)`, and it does not survive the
/// nesting: a field list is walked *around* its arguments' lists and a directive list around each
/// usage's arguments, so one slot per symbol shared across a nested pair has the inner list erase
/// the outer's record of the same name. A value per list is reentrant because there is nothing to
/// share.
///
/// **Not more memory.** The trail it replaces recorded `(Sym, SimpleSpan)`, twenty-four bytes per
/// name; this is eight bytes per position while the index is built and four while it is read. A
/// position is a `u32` because everything this crate addresses is — [`Sym`] itself, [`Range32`],
/// the flat tables `flatten` builds — so the width is the representation's and not a new one.
enum Duplicates {
  /// The names written so far and the position that wrote each, scanned in order.
  Trail(Vec<(Sym, u32)>),
  /// One entry per position: the position that first wrote that position's name, which is the
  /// position itself when this is the first.
  Index(Vec<u32>),
}

impl Duplicates {
  /// Resolves one list, `name` addressing it by position.
  ///
  /// The accessor is read at most once per position and only on the wide path; a narrow list
  /// never calls it.
  fn over(len: usize, name: impl Fn(usize) -> Sym) -> Self {
    if len <= NARROW_LIST {
      return Self::Trail(Vec::with_capacity(len));
    }

    let mut order: Vec<(Sym, u32)> = (0..len).map(|at| (name(at), at as u32)).collect();
    order.sort_unstable();

    let mut first = vec![0u32; len];
    let mut head = order[0];
    first[head.1 as usize] = head.1;
    for &entry in &order[1..] {
      if entry.0 != head.0 {
        head = entry;
      }
      first[entry.1 as usize] = head.1;
    }
    Self::Index(first)
  }

  /// Answers the earlier position that wrote `name`, or records this one as its first.
  ///
  /// Call it once per position, in source order, and only where the rule actually applies — a
  /// repeatable directive is not recorded, exactly as the trail did not record one.
  fn first(&mut self, position: usize, name: Sym) -> Option<usize> {
    match self {
      Self::Trail(trail) => match trail.iter().find(|(written, _)| *written == name) {
        Some(&(_, at)) => Some(at as usize),
        None => {
          trail.push((name, position as u32));
          None
        }
      },
      Self::Index(first) => {
        let at = first[position] as usize;
        (at != position).then_some(at)
      }
    }
  }
}

/// The position of a name in one type's field list, asked by a name from a *different* list.
///
/// # Why this is not [`Duplicates`], which is the neighbouring question
///
/// [`Duplicates`] is addressed by **position**: `first(position, name)` answers "did an earlier
/// position of *this* list write this name", and its [`Duplicates::Index`] payload is one entry
/// per position with the names already discarded. Draft §3.6.1's conformance pass asks the other
/// question — "which position of the *implementor's* list wrote the name this *interface* field
/// carries" — and the sorted pairs that built that payload are gone by the time it could be
/// asked. So the type is not reusable here, and neither is its `Vec<u32>`.
///
/// # What it replaces
///
/// `model.types[index].fields.iter().position(..)`, restarted for every interface field. A valid
/// interface and object with the same `N` fields in source order did `N(N+1)/2` symbol
/// comparisons, and **nothing bounds a field list** — the argument [`NARROW_LIST`] already makes
/// about enum values and fields is that four-figure lists are ordinary, so a ceiling here would
/// refuse valid documents rather than bound the scan. Fitted at 1.83 in the exponent over
/// 1 k–64 k and 2.00 over the top step of that ladder, 1.507 s at 64 k, on a document
/// `Schema::build` **accepts**; 1.03 and 40 ms with the list indexed. al8n/smear#198.
///
/// # A table indexed by [`Sym`], which [`Duplicates`] rejected for a reason that is not this site's
///
/// That header rules the shape out because it "does not survive the nesting": a field list is
/// walked *around* its arguments' lists, so one slot per symbol shared across a nested pair has
/// the inner list erase the outer's record of the same name. This pass nests no second field
/// index inside the first — it holds one implementor's list while reading interface *names* —
/// so the condition that shape fails under is absent, and what is left is `O(1)` per lookup
/// against the sorted index's `O(log N)`, over one table for the whole pass instead of one
/// allocation per implementing type.
///
/// The table is put back rather than reallocated, and [`Positions::drop`] is what puts it back:
/// a slot left set would answer the *next* type's lookup with a position in a list that type
/// does not have, which is a wrong span on a diagnostic rather than a cost.
///
/// # If you change this, a fixture dump will not tell you
///
/// The first-occurrence rule below was proved by *planting its negation* — recording the last
/// occurrence instead — and the twenty hand-written fixtures this branch had used twice, **wide
/// ones written for exactly this property included**, came back byte-identical. A duplicate field
/// name is benign for every rule here unless the two occurrences differ in a way some rule reads,
/// and hand-picking a fixture that satisfies that is harder than it looks.
///
/// What caught it was four thousand *derived* schemas: 320 923 diagnostics across eighteen kinds,
/// of which the plant moved 197 804 lines. So the instrument for a change here is a derived
/// corpus diffed against the previous commit's binary, not a fixture list — and the plant is what
/// says the corpus can see the change at all. al8n/smear#198.
enum Positions<'f, 's> {
  /// At or below [`NARROW_LIST`]: the scan, whose cost per lookup the ceiling on that constant is
  /// what bounds, exactly as the argument lists' scan is bounded.
  Scan(&'f [RawField]),
  /// Past it: `Sym` to the first position of `fields` that wrote it, [`Positions::ABSENT`]
  /// everywhere else, over a table sized to the whole symbol space.
  Index {
    fields: &'f [RawField],
    of_sym: &'s mut [u32],
  },
}

impl<'f, 's> Positions<'f, 's> {
  /// No position wrote this name. Not a position a document can reach: a field needs bytes, a
  /// span is a `u32`, so a list of `u32::MAX` fields does not fit in a document this crate can
  /// address.
  const ABSENT: u32 = u32::MAX;

  /// Resolves one type's field list, borrowing the pass's table only when the list is wide
  /// enough to want it. `symbols` is the interner's width, which is what the table is addressed
  /// by; it is grown once, on the first wide list, and never for a schema that has none.
  fn over(fields: &'f [RawField], symbols: usize, of_sym: &'s mut Vec<u32>) -> Self {
    if fields.len() <= NARROW_LIST {
      return Self::Scan(fields);
    }
    if of_sym.len() < symbols {
      of_sym.resize(symbols, Self::ABSENT);
    }
    for (at, field) in fields.iter().enumerate() {
      let slot = &mut of_sym[field.name.sym.get() as usize];
      // First occurrence wins, which is what the scan answered. A type that writes one name twice
      // is a `DuplicateFieldName` already, and the span every rule below blames is the first
      // one's; letting the second overwrite it would move a blessed diagnostic while looking
      // like a cost change.
      if *slot == Self::ABSENT {
        *slot = at as u32;
      }
    }
    Self::Index { fields, of_sym }
  }

  /// The first position that wrote `name`, or `None`.
  fn of(&self, name: Sym) -> Option<usize> {
    match self {
      Self::Scan(fields) => fields.iter().position(|field| field.name.sym == name),
      Self::Index { of_sym, .. } => match of_sym[name.get() as usize] {
        Self::ABSENT => None,
        at => Some(at as usize),
      },
    }
  }
}

impl Drop for Positions<'_, '_> {
  /// Clears exactly the slots this list wrote, which is `O(fields)` and not `O(symbols)`.
  fn drop(&mut self) {
    let Self::Index { fields, of_sym } = self else {
      return;
    };
    for field in *fields {
      of_sym[field.name.sym.get() as usize] = Self::ABSENT;
    }
  }
}

/// The first position of a name in one **written** list — a literal's own entries — asked by names
/// from the declaration the reader is walking at the same time.
///
/// # Why this is neither [`Duplicates`] nor [`Positions`], which are the two neighbours
///
/// [`Duplicates`] is addressed by **position** and its [`Duplicates::Index`] payload has discarded
/// the names, so it cannot answer "which position of *this* list holds the name that *other* list
/// carries" at all. [`Positions`] answers exactly that question, and its payload is one slot per
/// [`Sym`] over the whole symbol space — the shape [`Duplicates`]'s header rules out because it
/// "does not survive the nesting", and which [`Positions`] takes only because draft §3.6.1's
/// conformance pass holds one implementor's list at a time and nests no second index inside it.
///
/// A value per list rather than a table, because what these index is a **literal**: its entries
/// belong to the one node that wrote them, so there is nothing for a table to be keyed by. The
/// *declaration* side is the half that is a function of a type, and [`DeclaredNames`] is that half
/// — which is also where the nesting hazard went. [`Positions`]'s payload could not have served
/// either: one slot per [`Sym`] over the whole symbol space is a `&mut` scratch, and the checks
/// that read literals run over [`Model`], the read-only half that carries no scratch to clear.
///
/// So this is one value per list, made out of the sorted pairs [`Duplicates`] already uses, built
/// where the list is read and dropped before the frame that read it returns.
///
/// # Sorting the pair, so `first` is still the first
///
/// Equal names order by ascending position, so the head of a run is the first occurrence and not
/// merely one of them — which is the answer [`Iterator::find`] gave. A type that writes one input
/// field twice is a `DuplicateInputFieldName` already, and a literal that writes one entry twice is
/// checked against the same declaration both times; resolving either by whatever the sort put in
/// front would move a blessed diagnostic while looking like a cost change.
///
/// # The switch is on the number of *asks*, not on the length of the list
///
/// [`Duplicates`] and [`Positions`] both switch on the list they index, because there those two
/// numbers are the same one. Here they are not: `Q` lookups into a list of `D` names cost `Q × D`
/// scanned and `D log D + Q log D` indexed, so the sort pays for itself once `Q` passes `log D` —
/// and it *loses* on a wide declared list asked one question, which is an ordinary small literal
/// offered to a wide input object. [`NARROW_LIST`] is the threshold on `Q` for the reason it is the
/// threshold anywhere else: below it the scan costs a constant times the list, which is the walk
/// the caller was making regardless.
///
/// # What it replaces
///
/// `fields.iter().any(..)`, restarted for every required field an input object declares — draft
/// 5.6.4's SDL twin, `Θ(literal × required)` — and the same scan asked the other way round in
/// [`SchemaBuilder::validate_input_object_default_cycles`], which reads one map node once per
/// declared field. `input Wide` with `N` fields and one default writing those same `N` entries is
/// `O(N)` of source, and both were `Θ(N²)` on it: 1.98 in the exponent over 1 k–64 k, 2.02 over the
/// top step, **3.197 s** at 64 k — on a schema `Schema::build` **accepts**. Indexed, the ladder is
/// 0.99 and **42 ms** at 64 k; read off a low end near one millisecond, so carried out to 256 k
/// where the floor cannot be what is being measured: 1.12 over 64 k–256 k, 196 ms.
///
/// Draft 5.6.2's twin — `declared.iter().find(..)`, restarted for every field the literal writes —
/// was the third, and is [`DeclaredNames`]'s: a scan over the *declaration* is a question one table
/// per type answers for every literal at once, and a value per list answered it once per literal
/// and once per nesting level. al8n/smear#198.
enum Names {
  /// At or below [`NARROW_LIST`] asks: the scan, whose per-lookup cost that ceiling is what bounds,
  /// exactly as it bounds the declared argument lists'.
  Scan,
  /// Past it: `(name, position)` sorted once, so a lookup is a binary search.
  Sorted(Vec<(Sym, u32)>),
}

impl Names {
  /// Resolves one list of `len` positions, `name` addressing it, against the `asks` lookups that
  /// are coming.
  fn over(len: usize, asks: usize, name: impl Fn(usize) -> Sym) -> Self {
    if asks <= NARROW_LIST {
      return Self::Scan;
    }
    let mut order: Vec<(Sym, u32)> = (0..len).map(|at| (name(at), at as u32)).collect();
    order.sort_unstable();
    Self::Sorted(order)
  }

  /// The first position that wrote `wanted`, or `None`. `name` is the accessor `over` was given,
  /// because the narrow arm stores nothing.
  fn first(&self, len: usize, wanted: Sym, name: impl Fn(usize) -> Sym) -> Option<usize> {
    match self {
      Self::Scan => (0..len).find(|&at| name(at) == wanted),
      Self::Sorted(order) => {
        let at = order.partition_point(|&(written, _)| written < wanted);
        match order.get(at) {
          Some(&(written, position)) if written == wanted => Some(position as usize),
          _ => None,
        }
      }
    }
  }
}

/// The first position of each name one **input object declares**, addressed by name, built at most
/// once per type and shared by every literal offered to it.
///
/// # Why the declared side is a table per type and the written side stays a value per list
///
/// [`Names`] is a value per list, and that is what made it correct for a nested literal — and also
/// what made it a cost per level. [`SchemaBuilder::check_const_value`] recurses into a nested
/// entry while the surrounding loop still needs the outer index, so a value built for the outer
/// literal is *live* for the whole of the inner one, and one built for a sibling literal is built
/// again from nothing. A directive default holding an `N`-deep literal of one `D`-wide input type,
/// sixty-five fields per level with the recursive field first, therefore retained `Θ(N × D)` pairs
/// and sorted `Θ(N × D log D)` of them on `Θ(N + D)` of SDL `Schema::build` **accepts** — and
/// `Schema::build` carries no proof of a literal's depth, so a parser-bounded `N` still multiplies
/// a large declaration by whatever that bound is, once per literal that reaches the type.
///
/// A declaration does not change while a literal is read, so the index a nested literal wants is
/// the *same* index: it is a function of the type and not of the literal. One table keyed by type
/// removes the rebuild and the retention together, which is what picks it over materialising each
/// level's resolved fields and dropping the index before recursing — that removes only the
/// retention, and leaves the sort per level standing.
///
/// A *written* list is not a function of any type, so there is nothing for a shared table to be
/// keyed by, and [`Names`] stays what indexes one: the literal's own entries, asked by the names
/// the declaration carries.
///
/// # The switch is on the asks this declaration has answered, not on one literal's
///
/// [`Names::over`] decides per literal, because a value per list knows nothing about the last one.
/// A table does, and the two numbers it can compare are the right ones: `Q` lookups into a list of
/// `D` names cost `Q × D` scanned and `D log D + Q log D` indexed, and `Q` is the number of asks
/// this *declaration* has answered across the whole build rather than the width of whichever
/// literal is asking now. So the sort is paid for once, by whichever ask crosses [`NARROW_LIST`],
/// and a wide declaration asked one question by each of many small literals — the case a
/// per-literal switch reads as narrow every time, and rescans in full every time — crosses it too.
///
/// What that costs before the index exists is at most [`NARROW_LIST`] scans of the declaration,
/// which is the same constant times the same list the caller was walking regardless.
///
/// # What it replaces
///
/// `declared.iter().find(..)`, restarted for every field an input-object literal writes, and then
/// [`Names::over`] rebuilt at every level and every sibling. The nested fixture above was 1.66 in
/// the exponent over 16 k–64 k and 851 ms at 64 k, and is 1.03 and 20 ms. al8n/smear#198.
#[derive(Debug, Default)]
struct DeclaredNames {
  /// One slot per type index, filled on demand: how many asks this declaration has answered by
  /// scanning, and the sorted `(name, position)` index once one has paid for it.
  of_type: Vec<(u32, Option<Vec<(Sym, u32)>>)>,
}

impl DeclaredNames {
  /// The first position of `wanted` in `types[base]`'s declared input fields, or `None`.
  ///
  /// Sorted pairs order equal names by ascending position, so the head of a run is the first
  /// occurrence and not merely one of them — the answer the scan it replaces gave, and the one a
  /// type that declares a field twice needs, because moving it would move a blessed diagnostic
  /// while looking like a cost change.
  fn first(&mut self, types: &[RawType], base: usize, wanted: Sym) -> Option<usize> {
    if self.of_type.len() < types.len() {
      self.of_type.resize_with(types.len(), || (0, None));
    }
    let declared = &types[base].input_fields;
    let (asks, order) = &mut self.of_type[base];
    if order.is_none() {
      if *asks < NARROW_LIST as u32 {
        *asks += 1;
        return declared.iter().position(|field| field.name.sym == wanted);
      }
      let mut sorted: Vec<(Sym, u32)> = declared
        .iter()
        .enumerate()
        .map(|(at, field)| (field.name.sym, at as u32))
        .collect();
      sorted.sort_unstable();
      *order = Some(sorted);
    }
    let order = order.as_ref().expect("the index was just filled");
    let at = order.partition_point(|&(declared, _)| declared < wanted);
    match order.get(at) {
      Some(&(declared, position)) if declared == wanted => Some(position as usize),
      _ => None,
    }
  }
}

/// A type reference whose base has been interned but not yet resolved.
#[derive(Debug, Clone, Copy)]
struct RawTypeRef {
  base: Located,
  span: SimpleSpan,
  packed: PackedType,
  too_deep: bool,
}

/// A directive written on a type-system element, kept whole.
///
/// The definition it names may not have been read yet — it may not even be in this document — so
/// nothing about a usage can be decided at ingest. What is only knowable *here* is the syntactic
/// position, so [`RawDirectiveUse::location`] is recorded at ingest and everything else is
/// deferred to [`SchemaBuilder::validate_directive_usages`].
#[derive(Debug, Clone)]
struct RawDirectiveUse {
  name: Located,
  /// The whole `@name(…)`, which is what a missing argument is blamed on: there is no argument to
  /// point at.
  span: SimpleSpan,
  location: DirectiveLocation,
  args: Vec<RawArgument>,
}

#[derive(Debug, Clone)]
struct RawArgument {
  name: Located,
  value: RawValue,
}

/// A constant literal, reduced to what a type check reads, with the position to blame.
#[derive(Debug, Clone)]
struct RawValue {
  span: SimpleSpan,
  shape: RawShape,
}

/// The literal itself.
///
/// Only the two numeric arms keep their spelling — that is what the range checks read — and only
/// the enum arm keeps its name. Everything else is decided by shape alone, so a `String`'s bytes
/// are dropped rather than copied into the builder.
#[derive(Debug, Clone)]
enum RawShape {
  Null,
  Boolean,
  Int(Box<[u8]>),
  Float(Box<[u8]>),
  String,
  Enum(Box<[u8]>),
  List(Vec<RawValue>),
  Object(Vec<RawObjectField>),
}

impl RawShape {
  /// The shape, as the coercion table names it.
  const fn shape(&self) -> LiteralShape {
    match self {
      Self::Null => LiteralShape::Null,
      Self::Boolean => LiteralShape::Boolean,
      Self::Int(_) => LiteralShape::Int,
      Self::Float(_) => LiteralShape::Float,
      Self::String => LiteralShape::String,
      Self::Enum(_) => LiteralShape::Enum,
      Self::List(_) => LiteralShape::List,
      Self::Object(_) => LiteralShape::Object,
    }
  }

  /// The retained spelling the numeric ranges read, empty for every other shape.
  fn spelling(&self) -> &[u8] {
    match self {
      Self::Int(bytes) | Self::Float(bytes) => bytes,
      _ => &[],
    }
  }
}

#[derive(Debug, Clone)]
struct RawObjectField {
  name: Located,
  value: RawValue,
}

#[derive(Debug, Clone)]
struct RawInput {
  name: Located,
  ty: RawTypeRef,
  default: DefaultKind,
  /// The default literal itself, kept only while building.
  ///
  /// [`DefaultKind`] is the reduction the finished [`Schema`] carries — draft 5.4.3, 5.6.4 and
  /// 5.8.5 never read a default's *value* — but two §3 rules do: 3.6.1(2.4.5) type-checks it, and
  /// 3.10.1(4) walks the graph these literals induce. Both run inside the build, so the literal is
  /// retained here and dropped by [`SchemaBuilder::flatten`] rather than reaching the schema.
  default_value: Option<RawValue>,
  directives: Vec<RawDirectiveUse>,
}

impl RawInput {
  /// Whether a value must be supplied for this input: non-null, with no default to fall back on.
  fn is_required(&self) -> bool {
    self.ty.packed.is_non_null() && !self.default.is_present()
  }
}

/// The declared argument list and the ceiling that decides who may read it.
///
/// A module of its own — inside this file, whose every other item is a sibling — because Rust's
/// privacy is per module and the whole mechanism is the private field. `Declared`'s `Vec` is
/// unreachable from the rest of `builder.rs`, so [`Declared::read`] is the only way to a
/// `&[RawInput]` and there is no second way for a consumer to find.
mod declared {
  use super::RawInput;

  /// A *declared* argument list — a field's or a directive definition's — held behind the ceiling
  /// that decides whether reading it is bounded work.
  ///
  /// # What a reader cannot do
  ///
  /// There is no accessor that hands over the list. [`Declared::read`] answers with [`Args`],
  /// whose two arms have to be destructured, and the over-limit arm carries nothing. A consumer
  /// written next year by someone who has never heard of `MAX_FIELD_ARGUMENTS` cannot scan an
  /// over-limit list by forgetting to check for one: forgetting does not compile.
  ///
  /// # Why the ceilings were not enough on their own
  ///
  /// Each of the two ceilings was first written as a `continue` in front of the one walk that had
  /// been measured, and review then found the next consumer — twice, at two walks that were never
  /// on that path. Every directive *usage* re-scanned the oversized declared list once per written
  /// argument, `Θ(declared × written)`; interface conformance scanned both sides of an
  /// over-limit field, `Θ(own × interface)`. A guard in front of one caller bounds one caller.
  /// The list is what every caller has in common, so the guard belongs on the list, and the next
  /// consumer meets it without anyone having remembered to put it there.
  ///
  /// # And what no reader can reach, because it is no longer here
  ///
  /// A gate on the read is still a gate on *readers*. `Declared` derives [`Clone`], and a derived
  /// `Clone` copies the private field without asking [`Declared::read`] anything — so interface
  /// conformance, which copies a whole `RawField` once per implementor *before* it reaches the
  /// gate, performed `Θ(implementors × declared)` deep [`RawInput`] clones over a document of size
  /// `O(implementors + declared)` and only then refused the schema. The ceiling kept the
  /// diagnostics and lost the resource.
  ///
  /// So the refusal is a *state* rather than a comparison, decided in `Declared::from` and holding
  /// nothing: `args` is `None`, and the `Vec` is dropped at the moment the length is first known.
  /// There is no payload for `Clone` to copy, none for `Debug` to format, and none for a `Deref`,
  /// a serialiser or an iterator adaptor written later to reach. The guarantee stops depending on
  /// every reader remembering, on every derive having been audited, and on this module's boundary
  /// holding against traits nobody has written yet.
  ///
  /// # Why at construction, and not later
  ///
  /// Both construction sites — [`SchemaBuilder::fields`](super::SchemaBuilder::fields) and
  /// [`SchemaBuilder::directive_definition`](super::SchemaBuilder::directive_definition) — hand
  /// over a list that is already whole, and nothing appends to one afterwards:
  /// [`apply_extensions`](super::SchemaBuilder::apply_extensions) extends a type's *fields*,
  /// never an existing field's arguments. The length at `from` is final, so refusing there means
  /// no phase ever holds an over-limit payload, rather than one phase dropping what the phases
  /// before it carried.
  ///
  /// Neither ceiling diagnostic needs anything from the payload to survive it. Both are built from
  /// the *owner's* [`Located`](super::Located) — the field's name, the directive's name — which
  /// lives beside this list and not in it, and neither message renders a count. Nothing had to be
  /// captured on the way past.
  ///
  /// # Why the list is not truncated instead
  ///
  /// Cutting an over-limit list down to `CEILING` would bound every scan with no gate at all, and
  /// it would **invent** diagnostics: an interface field whose arguments are cut to sixty-four
  /// reports `MissingInterfaceFieldArgument` for arguments the document genuinely wrote. Dropping
  /// the list whole is not that, and [`Args::Refused`] is the difference: a refused list answers
  /// `Refused` and never "empty", so every consumer skips the entire check exactly as it did while
  /// the payload was still there, and no diagnostic moves.
  ///
  /// # What the skipped work costs
  ///
  /// Nothing a caller can observe. `SchemaBuilder::finish` returns `Err` for any recorded error,
  /// so a schema carrying `TooManyFieldArguments` or `TooManyDirectiveArguments` is never handed
  /// out; every diagnostic a refused list suppresses is one about a document that is refused
  /// already, for a reason the refusal names. al8n/smear#198.
  #[derive(Debug, Clone)]
  pub(super) struct Declared<const CEILING: u32> {
    /// The declared arguments, or `None` — the ceiling refused this list and the arguments were
    /// dropped where that was decided. The two states are not "long" and "short" but "may be read"
    /// and "does not exist", which is why a field declaring no arguments at all is `Some(&[])`.
    args: Option<Vec<RawInput>>,
  }

  /// What a [`Declared`] list answers when a reader asks for its arguments.
  pub(super) enum Args<'a> {
    /// At or below the ceiling: the scan the reader is about to do is the one the ceiling bounds.
    Bounded(&'a [RawInput]),
    /// Past it, and recorded as such. No list here, on purpose — the reader's very next line is
    /// where the decision to skip has to be written.
    Refused,
  }

  /// [`Args`] for the one pass that resolves the arguments in place.
  pub(super) enum ArgsMut<'a> {
    /// At or below the ceiling.
    Bounded(&'a mut [RawInput]),
    /// Past it.
    Refused,
  }

  impl<const CEILING: u32> Declared<CEILING> {
    /// Whether the ceiling refused this list — which is what the refusal site records the
    /// diagnostic for. No other reader has to ask, because [`Declared::read`] asks on its behalf.
    ///
    /// A length comparison answered the same question one round ago and cannot answer it now:
    /// there is no length left to compare, which is the whole of the repair.
    pub(super) fn refused(&self) -> bool {
      self.args.is_none()
    }

    /// The arguments, if the ceiling admits them.
    pub(super) fn read(&self) -> Args<'_> {
      match &self.args {
        Some(args) => Args::Bounded(args),
        None => Args::Refused,
      }
    }

    /// The arguments to resolve in place, if the ceiling admits them.
    pub(super) fn read_mut(&mut self) -> ArgsMut<'_> {
      match &mut self.args {
        Some(args) => ArgsMut::Bounded(args),
        None => ArgsMut::Refused,
      }
    }
  }

  /// Where the ceiling is applied, and the only way a `Vec<RawInput>` becomes a `Declared`.
  ///
  /// The over-limit arm drops `args` instead of storing it. The copy a derived `Clone` would make,
  /// the walk a reader would have done, and the bytes the list would have occupied for the rest of
  /// the build are then work that does not exist, rather than work every consumer is trusted to
  /// decline.
  impl<const CEILING: u32> From<Vec<RawInput>> for Declared<CEILING> {
    fn from(args: Vec<RawInput>) -> Self {
      match args.len() > CEILING as usize {
        false => Self { args: Some(args) },
        true => Self { args: None },
      }
    }
  }
}

#[derive(Debug, Clone)]
struct RawField {
  name: Located,
  ty: RawTypeRef,
  args: Declared<MAX_FIELD_ARGUMENTS>,
  directives: Vec<RawDirectiveUse>,
}

#[derive(Debug, Clone)]
struct RawEnumValue {
  name: Located,
  directives: Vec<RawDirectiveUse>,
}

#[derive(Debug, Clone)]
struct RawType {
  name: Located,
  kind: TypeKind,
  built_in: bool,
  /// The definition collides with an introspection type, already reported as
  /// `RedefinedBuiltInType`; the reserved-name rule stays quiet so one defect reports once.
  collides_with_built_in: bool,
  implements: Vec<Located>,
  fields: Vec<RawField>,
  input_fields: Vec<RawInput>,
  members: Vec<Located>,
  enum_values: Vec<RawEnumValue>,
  directives: Vec<RawDirectiveUse>,
  /// The interface closure, filled in during validation.
  closure: Vec<u32>,
  /// The positions of [`input_fields`](RawType::input_fields) that [`RawInput::is_required`]
  /// answers `true` for, filled in during validation beside the closure.
  ///
  /// Draft 5.6.4's omitted half asks "which required field did this literal not write", once per
  /// literal offered to this type, and the list is what makes the answer proportional to what it
  /// reports instead of to the declaration. Sieving it at the literal is a walk over every declared
  /// field for a type that may have no required field at all: `input Wide` with `N` fields and `N`
  /// input objects defaulting to `{}` of it is `O(N)` of source, `Θ(N²)` of sieve — 1.97 in the
  /// exponent over the top step of 1 k–16 k and **1.470 s** at 16 k, on a schema `Schema::build`
  /// **accepts**. al8n/smear#198.
  required_input_fields: Vec<u32>,
}

impl RawType {
  fn new(name: Located, kind: TypeKind, built_in: bool) -> Self {
    Self {
      name,
      kind,
      built_in,
      collides_with_built_in: false,
      implements: Vec::new(),
      fields: Vec::new(),
      input_fields: Vec::new(),
      members: Vec::new(),
      enum_values: Vec::new(),
      directives: Vec::new(),
      closure: Vec::new(),
      required_input_fields: Vec::new(),
    }
  }
}

#[derive(Debug, Clone)]
struct RawDirectiveDef {
  name: Located,
  args: Declared<MAX_DIRECTIVE_ARGUMENTS>,
  locations: DirectiveLocations,
  repeatable: bool,
  built_in: bool,
}

/// A type extension, converted to owned form and applied once every document has been read.
#[derive(Debug, Clone, Default)]
struct RawExtension {
  target: Option<Located>,
  kind: Option<TypeKind>,
  implements: Vec<Located>,
  fields: Vec<RawField>,
  input_fields: Vec<RawInput>,
  members: Vec<Located>,
  enum_values: Vec<RawEnumValue>,
  directives: Vec<RawDirectiveUse>,
  /// Root operations, for a `extend schema` rather than a type extension.
  roots: Vec<(RootOperation, Located)>,
}

// ---------------------------------------------------------------------------------------------
// the built-in documents
// ---------------------------------------------------------------------------------------------

/// The three constant SDL documents every build injects, parsed.
///
/// Held together rather than separately so the whole set is one `OnceLock` and one initialisation
/// rather than three. It borrows the `&'static str` constants it was parsed from, so the value is
/// `'static` itself and shareable — every field of the AST over a `&'static str` is `Send + Sync`,
/// which the assertion below is the standing proof of.
#[derive(Debug)]
struct BuiltIns {
  introspection: TypeSystemDocument<&'static str>,
  scalars: TypeSystemDocument<&'static str>,
  directives: TypeSystemDocument<&'static str>,
}

impl BuiltIns {
  fn parse() -> Self {
    Self {
      introspection: SchemaBuilder::parse_builtin(builtin::INTROSPECTION_SDL),
      scalars: SchemaBuilder::parse_builtin(builtin::BUILT_IN_SCALARS_SDL),
      directives: SchemaBuilder::parse_builtin(builtin::BUILT_IN_DIRECTIVES_SDL),
    }
  }
}

/// What lets the parsed documents live in a `static`: a shared reference to one crosses threads.
///
/// Asserted rather than assumed, because the AST offers an `Arc`-backed list-type spelling and a
/// future field that reached for a non-`Sync` cell would otherwise fail at the `OnceLock` with an
/// error that names the lock rather than the type that broke it.
const _: () = {
  const fn sync<T: Sync>() {}
  let _ = sync::<BuiltIns>;
};

// ---------------------------------------------------------------------------------------------
// the read-only half
// ---------------------------------------------------------------------------------------------

/// Everything a draft §3 check reads, borrowed apart from what it writes.
///
/// # What this is for
///
/// A §3 check reads the merged model and appends a diagnostic, and `&mut self` cannot do both at
/// once: `self.push(…)` may not be called while `&self.types[…]` is alive. The other way round it
/// is to copy whatever will be read — but the copy is then made once per *candidate*: one per
/// type, per field, per argument, per directive usage, over a merged schema whose ninety-odd
/// built-in items produce no diagnostic at all. The copies are deep, too, because a [`RawInput`]
/// owns its directives and its default literal.
///
/// Naming the read side as its own set of field borrows makes the two disjoint instead, so a
/// literal is checked where it lies and an owner path rendered only when something is reported.
#[derive(Clone, Copy)]
struct Model<'a> {
  types: &'a [RawType],
  directives: &'a [RawDirectiveDef],
  /// `Sym` to an index into `types`; `u32::MAX` for "not a type".
  type_of_sym: &'a [u32],
  /// `Sym` to an index into `directives`; `u32::MAX` for "not a directive".
  directive_of_sym: &'a [u32],
  schema_directives: &'a [RawDirectiveUse],
  interner: &'a Interner,
}

impl<'a> Model<'a> {
  fn text(&self, sym: Sym) -> &'a str {
    self.interner.text(sym)
  }

  fn owner(&self, at: Coordinate) -> String {
    render_owner(self.interner, at)
  }

  fn type_index(&self, sym: Sym) -> Option<usize> {
    index_of(self.type_of_sym, sym)
  }

  fn directive_index(&self, sym: Sym) -> Option<usize> {
    index_of(self.directive_of_sym, sym)
  }

  /// Draft's `IsValidImplementationFieldType`.
  fn is_valid_implementation_type(&self, field: PackedType, implemented: PackedType) -> bool {
    if field.is_non_null() {
      return self.is_valid_implementation_type(field.nullable(), implemented.nullable());
    }
    if let (Some(item), Some(implemented_item)) = (field.list_item(), implemented.list_item()) {
      return self.is_valid_implementation_type(item, implemented_item);
    }
    if field == implemented {
      return true;
    }
    if field.wrappers() != implemented.wrappers() {
      return false;
    }
    self.is_sub_type(field.base_id(), implemented.base_id())
  }

  /// Whether `candidate` is one of `abstract_type`'s possible types, in the draft's wider sense:
  /// union membership, or the interface closure — which, unlike the runtime possible-object
  /// bitsets, includes interfaces implementing interfaces.
  fn is_sub_type(&self, candidate: TypeId, abstract_type: TypeId) -> bool {
    if candidate == UNRESOLVED || abstract_type == UNRESOLVED {
      return false;
    }
    let target = &self.types[abstract_type.get() as usize];
    match target.kind {
      TypeKind::Union => target
        .members
        .iter()
        .any(|member| self.type_index(member.sym) == Some(candidate.get() as usize)),
      TypeKind::Interface => self.types[candidate.get() as usize]
        .closure
        .contains(&abstract_type.get()),
      _ => false,
    }
  }

  fn render_type(&self, packed: PackedType) -> String {
    struct Rendered<'a>(PackedType, &'a str);
    impl core::fmt::Display for Rendered<'_> {
      fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.write(f, self.1)
      }
    }
    Rendered(packed, self.text(packed.base())).to_string()
  }

  /// One declared argument list, through the ceiling that decides whether it may be walked.
  ///
  /// The only reader of either list in this file, now that `SchemaBuilder` has no accessor of its
  /// own: a `&mut self` pass reaches it through [`SchemaBuilder::split`], which is what the read
  /// and write halves were separated for. See [`Declared`].
  fn arguments(&self, at: ArgumentsOf) -> Args<'a> {
    match at {
      ArgumentsOf::Field { ty, field } => self.types[ty].fields[field].args.read(),
      ArgumentsOf::Directive { index } => self.directives[index].args.read(),
    }
  }

  fn directive_uses(&self, at: DirectivesOf) -> &'a [RawDirectiveUse] {
    match at {
      DirectivesOf::Schema => self.schema_directives,
      DirectivesOf::Type { ty } => &self.types[ty].directives,
      DirectivesOf::Field { ty, field } => &self.types[ty].fields[field].directives,
      // The two argument arms are the only ones whose list is behind a ceiling, and their
      // `Refused` arm is unreachable: an `arg` position exists only because the loop that minted
      // it read the same list through the gate and stopped when the gate refused. An empty list
      // is what an unreachable arm should degrade to here — no directive to check, so nothing
      // reported — rather than a diagnostic invented about a list nobody may look at.
      DirectivesOf::FieldArgument { ty, field, arg } => {
        match self.types[ty].fields[field].args.read() {
          Args::Bounded(args) => &args[arg].directives,
          Args::Refused => &[],
        }
      }
      DirectivesOf::InputField { ty, field } => &self.types[ty].input_fields[field].directives,
      DirectivesOf::EnumValue { ty, value } => &self.types[ty].enum_values[value].directives,
      DirectivesOf::DirectiveArgument { directive, arg } => {
        match self.directives[directive].args.read() {
          Args::Bounded(args) => &args[arg].directives,
          Args::Refused => &[],
        }
      }
    }
  }
}

/// Renders a [`Coordinate`] the way the diagnostics have always spelled one.
///
/// Through [`owner_path`] and [`directive_coordinate`] rather than by concatenating here, so the
/// text is the output of the same two functions every eager caller used to call.
fn render_owner(interner: &Interner, at: Coordinate) -> String {
  let mut segments: [&str; 3] = [""; 3];
  segments[0] = match at.head {
    Head::Schema => "schema",
    Head::Named(sym) => interner.text(sym),
  };
  let mut len = 1;
  if let Some(member) = at.member {
    segments[len] = interner.text(member);
    len += 1;
  }
  if let Some(argument) = at.argument {
    segments[len] = interner.text(argument);
    len += 1;
  }
  let path = owner_path(&segments[..len]);
  match at.directive {
    Some(directive) => directive_coordinate(&path, interner.text(directive)),
    None => path,
  }
}

/// A `Sym` to the index of what it names, over a table that spells "nothing" as `u32::MAX`.
///
/// One body and four callers: [`SchemaBuilder`] and [`Model`] each ask it about types and about
/// directives. Written once because two halves of the same builder answering the same question
/// differently is a defect no test would name — and because the fourth copy of a four-line
/// predicate is where that starts.
fn index_of(table: &[u32], sym: Sym) -> Option<usize> {
  match table.get(sym.get() as usize) {
    Some(&index) if index != u32::MAX => Some(index as usize),
    _ => None,
  }
}

/// Whether `@deprecated` is applied in this directive list.
///
/// A free function over the arena rather than a method, because both halves of
/// [`SchemaBuilder::split`] ask it: the read half while walking a list it is holding, and the
/// `&mut self` passes while walking one they re-address.
fn is_deprecated(interner: &Interner, directives: &[RawDirectiveUse]) -> bool {
  directives
    .iter()
    .any(|used| interner.text(used.name.sym) == DEPRECATED)
}

fn push_owned(
  errors: &mut Vec<SchemaError>,
  kind: SchemaErrorKind,
  subject: &str,
  owner: String,
  at: Located,
) {
  errors.push(
    SchemaError::new(kind, subject, at.span)
      .owned_by(owner)
      .in_document(at.document),
  );
}

/// Reports at a position no interned name stands for — a literal, or a whole directive usage.
fn push_at(
  errors: &mut Vec<SchemaError>,
  kind: SchemaErrorKind,
  subject: &str,
  owner: String,
  span: SimpleSpan,
  document: u32,
) {
  errors.push(
    SchemaError::new(kind, subject, span)
      .owned_by(owner)
      .in_document(document),
  );
}

fn push_related(
  errors: &mut Vec<SchemaError>,
  kind: SchemaErrorKind,
  subject: &str,
  owner: Option<String>,
  at: Located,
  related: SimpleSpan,
) {
  let mut error = SchemaError::new(kind, subject, at.span)
    .in_document(at.document)
    .related_to(related);
  if let Some(owner) = owner {
    error = error.owned_by(owner);
  }
  errors.push(error);
}

// ---------------------------------------------------------------------------------------------
// the builder
// ---------------------------------------------------------------------------------------------

/// Accumulates type-system documents and produces a [`Schema`].
///
/// Documents are read in the order they are given; extensions are applied only once every
/// document has been read, so an `extend type Foo` may precede `type Foo` and may live in a
/// different document.
#[derive(Debug, Default)]
pub struct SchemaBuilder {
  interner: Interner,
  types: Vec<RawType>,
  /// `Sym` to an index into `types`, dense over the symbol space; `u32::MAX` for "not a type".
  type_of_sym: Vec<u32>,
  directives: Vec<RawDirectiveDef>,
  /// `Sym` to an index into `directives`; `u32::MAX` for "not a directive".
  directive_of_sym: Vec<u32>,
  roots: [Option<Located>; 3],
  schema_definition: Option<SimpleSpan>,
  /// `SCHEMA`-location directives, from the `schema` definition and every `extend schema`.
  ///
  /// One list rather than one per definition, because apollo and the draft both treat the schema
  /// and its extensions as a single location for the repeatability rule.
  schema_directives: Vec<RawDirectiveUse>,
  extensions: Vec<RawExtension>,
  errors: Vec<SchemaError>,
  document: u32,
  /// One lookup index per input object, shared by every literal validation. Empty until a
  /// declaration is asked; see [`DeclaredNames`].
  declared_names: DeclaredNames,
}

impl SchemaBuilder {
  /// Creates an empty builder.
  #[inline]
  pub fn new() -> Self {
    Self::default()
  }

  /// Reads one type-system document.
  ///
  /// Definitions are recorded immediately; extensions are deferred to [`SchemaBuilder::finish`].
  /// Errors are accumulated rather than returned, so a caller sees every problem in every
  /// document at the end rather than one at a time.
  pub fn document<S>(&mut self, doc: &TypeSystemDocument<S>) -> &mut Self
  where
    S: AsRef<[u8]>,
  {
    for entry in doc.definitions() {
      match entry {
        TypeSystemDefinitionOrExtension::Definition(described) => {
          self.definition(described.node(), false);
        }
        TypeSystemDefinitionOrExtension::Extension(extension) => {
          self.extension(extension);
        }
      }
    }
    self.document += 1;
    self
  }

  /// Consumes the builder and produces a schema, or every reason it is not one.
  pub fn finish(mut self) -> Result<Schema, SchemaErrors> {
    // Injection precedes extension so that `extend scalar Int @tag(...)` — extending something
    // the specification provided rather than something the document defined — is not reported as
    // an undefined target. Whether a built-in is injected at all is decided by the *definitions*,
    // all of which are already in, so the order does not change what gets replaced.
    self.inject_built_ins();
    self.apply_extensions();
    self.validate();

    if !self.errors.is_empty() {
      return Err(SchemaErrors::new(self.errors));
    }
    self.flatten()
  }

  // -- error helpers --------------------------------------------------------------------------

  fn push(&mut self, kind: SchemaErrorKind, subject: &str, at: Located) {
    self
      .errors
      .push(SchemaError::new(kind, subject, at.span).in_document(at.document));
  }

  fn push_owned(&mut self, kind: SchemaErrorKind, subject: &str, owner: String, at: Located) {
    push_owned(&mut self.errors, kind, subject, owner, at);
  }

  fn push_related(
    &mut self,
    kind: SchemaErrorKind,
    subject: &str,
    owner: Option<String>,
    at: Located,
    related: SimpleSpan,
  ) {
    push_related(&mut self.errors, kind, subject, owner, at, related);
  }

  /// The read side and the write side, borrowed apart. See [`Model`].
  fn split(&mut self) -> (Model<'_>, &mut Vec<SchemaError>) {
    let (model, errors, _) = self.split_indexed();
    (model, errors)
  }

  /// [`split`](SchemaBuilder::split), with the literal-checking index as a third disjoint borrow.
  ///
  /// A third field borrow rather than a local, because the index is a function of the declarations
  /// alone: the checks that read literals are reached from four `&mut self` passes, and an index
  /// per pass would sort one declaration once for each of them. See [`DeclaredNames`].
  fn split_indexed(&mut self) -> (Model<'_>, &mut Vec<SchemaError>, &mut DeclaredNames) {
    (
      Model {
        types: &self.types,
        directives: &self.directives,
        type_of_sym: &self.type_of_sym,
        directive_of_sym: &self.directive_of_sym,
        schema_directives: &self.schema_directives,
        interner: &self.interner,
      },
      &mut self.errors,
      &mut self.declared_names,
    )
  }

  // -- interning ------------------------------------------------------------------------------

  fn located<S>(&mut self, name: &Name<S>) -> Located
  where
    S: AsRef<[u8]>,
  {
    let bytes = name.source().as_ref();
    let span = *name.as_span();
    let document = self.document;
    if !is_name(bytes) {
      // Unreachable from parsed input; reachable from a hand-assembled AST, and the arena's
      // ASCII invariant is what makes `Schema::name` infallible, so it is checked.
      let rendered = String::from_utf8_lossy(bytes).to_string();
      self.errors.push(
        SchemaError::new(SchemaErrorKind::InvalidName, &rendered, span).in_document(document),
      );
      let sym = self.interner.intern(b"__invalid");
      return Located {
        sym,
        span,
        document,
      };
    }
    let sym = self.interner.intern(bytes);
    Located {
      sym,
      span,
      document,
    }
  }

  fn text(&self, sym: Sym) -> &str {
    self.interner.text(sym)
  }

  /// Renders a [`Coordinate`], for the callers that have `&self` rather than a [`Model`].
  fn owner(&self, at: Coordinate) -> String {
    render_owner(&self.interner, at)
  }

  // -- ingest ---------------------------------------------------------------------------------

  fn definition<S>(&mut self, definition: &TypeSystemDefinition<S>, built_in: bool)
  where
    S: AsRef<[u8]>,
  {
    match definition {
      TypeSystemDefinition::Type(ty) => self.type_definition(ty, built_in),
      TypeSystemDefinition::Directive(directive) => self.directive_definition(directive, built_in),
      TypeSystemDefinition::Schema(schema) => self.schema_definition(schema),
    }
  }

  fn type_definition<S>(&mut self, definition: &TypeDefinition<S>, built_in: bool)
  where
    S: AsRef<[u8]>,
  {
    let raw = match definition {
      TypeDefinition::Scalar(def) => self.scalar(def, built_in),
      TypeDefinition::Object(def) => self.object(def, built_in),
      TypeDefinition::Interface(def) => self.interface(def, built_in),
      TypeDefinition::Union(def) => self.union(def, built_in),
      TypeDefinition::Enum(def) => self.enumeration(def, built_in),
      TypeDefinition::InputObject(def) => self.input_object(def, built_in),
    };
    self.record_type(raw);
  }

  fn record_type(&mut self, raw: RawType) {
    let sym = raw.name.sym;
    if let Some(previous) = self.type_index(sym) {
      let related = self.types[previous].name.span;
      let subject = self.text(sym).to_owned();
      self.push_related(
        SchemaErrorKind::DuplicateTypeName,
        &subject,
        None,
        raw.name,
        related,
      );
      return;
    }
    let index = self.types.len() as u32;
    self.set_type_index(sym, index);
    self.types.push(raw);
  }

  fn type_index(&self, sym: Sym) -> Option<usize> {
    index_of(&self.type_of_sym, sym)
  }

  fn set_type_index(&mut self, sym: Sym, index: u32) {
    let slot = sym.get() as usize;
    if self.type_of_sym.len() <= slot {
      self.type_of_sym.resize(slot + 1, u32::MAX);
    }
    self.type_of_sym[slot] = index;
  }

  fn directive_index(&self, sym: Sym) -> Option<usize> {
    index_of(&self.directive_of_sym, sym)
  }

  fn set_directive_index(&mut self, sym: Sym, index: u32) {
    let slot = sym.get() as usize;
    if self.directive_of_sym.len() <= slot {
      self.directive_of_sym.resize(slot + 1, u32::MAX);
    }
    self.directive_of_sym[slot] = index;
  }

  fn scalar<S>(&mut self, def: &ScalarTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::Scalar, built_in);
    raw.directives = self.directive_uses(def.directives(), DirectiveLocation::Scalar);
    raw
  }

  fn object<S>(&mut self, def: &ObjectTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::Object, built_in);
    raw.implements = self.implements(def.implements());
    raw.directives = self.directive_uses(def.directives(), DirectiveLocation::Object);
    raw.fields = self.fields(def.fields_definition());
    raw
  }

  fn interface<S>(&mut self, def: &InterfaceTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::Interface, built_in);
    raw.implements = self.implements(def.implements());
    raw.directives = self.directive_uses(def.directives(), DirectiveLocation::Interface);
    raw.fields = self.fields(def.fields_definition());
    raw
  }

  fn union<S>(&mut self, def: &UnionTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::Union, built_in);
    raw.directives = self.directive_uses(def.directives(), DirectiveLocation::Union);
    raw.members = self.members(def.member_types());
    raw
  }

  fn enumeration<S>(&mut self, def: &EnumTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::Enum, built_in);
    raw.directives = self.directive_uses(def.directives(), DirectiveLocation::Enum);
    raw.enum_values = self.enum_values(def.enum_values_definition());
    raw
  }

  fn input_object<S>(&mut self, def: &InputObjectTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::InputObject, built_in);
    raw.directives = self.directive_uses(def.directives(), DirectiveLocation::InputObject);
    raw.input_fields = self.input_fields(def.fields_definition());
    raw
  }

  fn directive_definition<S>(&mut self, def: &DirectiveDefinition<S>, built_in: bool)
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let args = match def.arguments_definition() {
      Some(arguments) => self.input_values(
        arguments.input_value_definitions(),
        DirectiveLocation::ArgumentDefinition,
      ),
      None => Vec::new(),
    };
    let mut locations = DirectiveLocations::EMPTY;
    for location in def.locations().locations() {
      if let Some(mapped) = map_location(location) {
        locations.insert(mapped);
      }
    }
    let repeatable = def.repeatable();

    if let Some(previous) = self.directive_index(name.sym) {
      let related = self.directives[previous].name.span;
      let subject = self.text(name.sym).to_owned();
      self.push_related(
        SchemaErrorKind::DuplicateDirectiveDefinition,
        &subject,
        None,
        name,
        related,
      );
      return;
    }
    let index = self.directives.len() as u32;
    self.set_directive_index(name.sym, index);
    self.directives.push(RawDirectiveDef {
      name,
      args: args.into(),
      locations,
      repeatable,
      built_in,
    });
  }

  fn schema_definition<S>(&mut self, def: &SchemaDefinition<S>)
  where
    S: AsRef<[u8]>,
  {
    let span = *def.span();
    if let Some(previous) = self.schema_definition {
      self.errors.push(
        SchemaError::new(SchemaErrorKind::DuplicateSchemaDefinition, "schema", span)
          .in_document(self.document)
          .related_to(previous),
      );
      return;
    }
    self.schema_definition = Some(span);
    let directives = self.directive_uses(def.directives(), DirectiveLocation::Schema);
    self.schema_directives.extend(directives);
    for root in def
      .root_operation_types_definition()
      .root_operation_type_definitions()
    {
      let operation = map_operation(root.operation_type());
      let target = self.located(root.name());
      self.set_root(operation, target);
    }
  }

  fn set_root(&mut self, operation: RootOperation, target: Located) {
    if let Some(previous) = self.roots[operation.index()] {
      let subject = operation.as_str();
      self.push_related(
        SchemaErrorKind::DuplicateRootOperationType,
        subject,
        None,
        target,
        previous.span,
      );
      return;
    }
    self.roots[operation.index()] = Some(target);
  }

  fn implements<S, C>(
    &mut self,
    implements: Option<&ImplementInterfaces<Name<S>, C>>,
  ) -> Vec<Located>
  where
    S: AsRef<[u8]>,
    C: AsRef<[Name<S>]>,
  {
    match implements {
      None => Vec::new(),
      Some(list) => list
        .interfaces()
        .iter()
        .map(|name| {
          let bytes = name.source().as_ref().to_owned();
          let span = *name.as_span();
          let document = self.document;
          let sym = self.interner.intern(&bytes);
          Located {
            sym,
            span,
            document,
          }
        })
        .collect(),
    }
  }

  fn members<S, C>(&mut self, members: Option<&UnionMemberTypes<Name<S>, C>>) -> Vec<Located>
  where
    S: AsRef<[u8]>,
    C: AsRef<[Name<S>]>,
  {
    match members {
      None => Vec::new(),
      Some(list) => list
        .members()
        .iter()
        .map(|name| {
          let bytes = name.source().as_ref().to_owned();
          let span = *name.as_span();
          let document = self.document;
          let sym = self.interner.intern(&bytes);
          Located {
            sym,
            span,
            document,
          }
        })
        .collect(),
    }
  }

  /// Converts a directive list to owned form, tagging each usage with the position it was written
  /// at.
  ///
  /// The location is the caller's to supply because it is the one fact the directive node does not
  /// carry: `@d` is the same three tokens on an object type and on an enum value.
  fn directive_uses<S>(
    &mut self,
    directives: Option<&ConstDirectives<S>>,
    location: DirectiveLocation,
  ) -> Vec<RawDirectiveUse>
  where
    S: AsRef<[u8]>,
  {
    let Some(list) = directives else {
      return Vec::new();
    };
    list
      .directives()
      .iter()
      .map(|directive| {
        let name = self.located(directive.name());
        let args = match directive.arguments() {
          None => Vec::new(),
          Some(arguments) => arguments
            .arguments()
            .iter()
            .map(|argument| RawArgument {
              name: self.located(argument.name()),
              value: self.const_value(argument.value()),
            })
            .collect(),
        };
        RawDirectiveUse {
          name,
          span: *directive.span(),
          location,
          args,
        }
      })
      .collect()
  }

  /// Reduces a constant literal to the shape and spelling a type check reads.
  ///
  /// Recursive, and bounded the same way [`SchemaBuilder::type_ref`] is: the AST handed in was
  /// built by a recursive-descent parser and will be dropped recursively, so a literal deep enough
  /// to overflow here could not have been parsed in the first place. That is not true of the type
  /// *graph*, which is why the cycle walks in this file are iterative and this is not.
  fn const_value<S>(&mut self, value: &ConstInputValue<S>) -> RawValue
  where
    S: AsRef<[u8]>,
  {
    let span = *value.as_span();
    let shape = match value {
      ConstInputValue::Null(_) => RawShape::Null,
      ConstInputValue::Boolean(_) => RawShape::Boolean,
      ConstInputValue::String(_) => RawShape::String,
      ConstInputValue::Int(int) => RawShape::Int(int.source().as_ref().into()),
      ConstInputValue::Float(float) => RawShape::Float(float.source().as_ref().into()),
      ConstInputValue::Enum(member) => RawShape::Enum(member.source().as_ref().into()),
      ConstInputValue::List(list) => RawShape::List(
        list
          .values()
          .iter()
          .map(|entry| self.const_value(entry))
          .collect(),
      ),
      ConstInputValue::Object(object) => RawShape::Object(
        object
          .fields()
          .iter()
          .map(|field| RawObjectField {
            name: self.located(field.name()),
            value: self.const_value(field.value()),
          })
          .collect(),
      ),
    };
    RawValue { span, shape }
  }

  fn fields<S>(&mut self, fields: Option<&FieldsDefinition<S>>) -> Vec<RawField>
  where
    S: AsRef<[u8]>,
  {
    let Some(fields) = fields else {
      return Vec::new();
    };
    fields
      .field_definitions()
      .iter()
      .map(|field| {
        let name = self.located(field.name());
        let ty = self.type_ref(field.ty());
        let args = match field.arguments_definition() {
          Some(arguments) => self.input_values(
            arguments.input_value_definitions(),
            DirectiveLocation::ArgumentDefinition,
          ),
          None => Vec::new(),
        };
        let directives =
          self.directive_uses(field.directives(), DirectiveLocation::FieldDefinition);
        RawField {
          name,
          ty,
          args: args.into(),
          directives,
        }
      })
      .collect()
  }

  fn input_fields<S>(&mut self, fields: Option<&InputFieldsDefinition<S>>) -> Vec<RawInput>
  where
    S: AsRef<[u8]>,
  {
    match fields {
      None => Vec::new(),
      Some(fields) => self.input_values(
        fields.input_value_definitions(),
        DirectiveLocation::InputFieldDefinition,
      ),
    }
  }

  /// One `InputValueDefinition` list, owned.
  ///
  /// `location` is what the same production is called in the two places it appears: an argument of
  /// a field or a directive is an `ARGUMENT_DEFINITION`, a field of an input object is an
  /// `INPUT_FIELD_DEFINITION`.
  fn input_values<S>(
    &mut self,
    values: &[InputValueDefinition<S>],
    location: DirectiveLocation,
  ) -> Vec<RawInput>
  where
    S: AsRef<[u8]>,
  {
    values
      .iter()
      .map(|value| {
        let name = self.located(value.name());
        let ty = self.type_ref(value.ty());
        let default_value = value
          .default_value()
          .map(|default| self.const_value(default.value()));
        let default = match &default_value {
          None => DefaultKind::Absent,
          Some(value) if matches!(value.shape, RawShape::Null) => DefaultKind::Null,
          Some(_) => DefaultKind::NonNull,
        };
        let directives = self.directive_uses(value.directives(), location);
        RawInput {
          name,
          ty,
          default,
          default_value,
          directives,
        }
      })
      .collect()
  }

  fn enum_values<S>(&mut self, values: Option<&EnumValuesDefinition<S>>) -> Vec<RawEnumValue>
  where
    S: AsRef<[u8]>,
  {
    let Some(values) = values else {
      return Vec::new();
    };
    values
      .enum_value_definitions()
      .iter()
      .map(|value| {
        let name = self.located(value.value());
        let directives = self.directive_uses(value.directives(), DirectiveLocation::EnumValue);
        RawEnumValue { name, directives }
      })
      .collect()
  }

  /// Flattens a type reference into a base name plus a wrapper word.
  fn type_ref<S>(&mut self, ty: &Type<Name<S>>) -> RawTypeRef
  where
    S: AsRef<[u8]>,
  {
    match ty {
      Type::Name(named) => {
        let base = self.located(named.name());
        let packed = PackedType::named(base.sym, UNRESOLVED);
        let (packed, too_deep) = if named.required() {
          match packed.push_non_null() {
            Some(packed) => (packed, false),
            None => (packed, true),
          }
        } else {
          (packed, false)
        };
        RawTypeRef {
          base,
          span: *named.span(),
          packed,
          too_deep,
        }
      }
      Type::List(list) => {
        let inner = self.type_ref(list.ty());
        let mut too_deep = inner.too_deep;
        let mut packed = inner.packed;
        match packed.push_list() {
          Some(next) => packed = next,
          None => too_deep = true,
        }
        if list.required() {
          match packed.push_non_null() {
            Some(next) => packed = next,
            None => too_deep = true,
          }
        }
        RawTypeRef {
          base: inner.base,
          span: *list.span(),
          packed,
          too_deep,
        }
      }
    }
  }

  fn extension<S>(&mut self, extension: &TypeSystemExtension<S>)
  where
    S: AsRef<[u8]>,
  {
    let mut raw = RawExtension::default();
    match extension {
      TypeSystemExtension::Schema(schema) => {
        raw.directives = self.directive_uses(schema.directives(), DirectiveLocation::Schema);
        if let Some(roots) = schema.root_operation_types_definition() {
          for root in roots.root_operation_type_definitions() {
            let operation = map_operation(root.operation_type());
            let target = self.located(root.name());
            raw.roots.push((operation, target));
          }
        }
      }
      TypeSystemExtension::Type(ty) => match ty {
        TypeExtension::Scalar(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::Scalar);
          raw.directives = self.directive_uses(Some(def.directives()), DirectiveLocation::Scalar);
        }
        TypeExtension::Object(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::Object);
          raw.implements = self.implements(def.implements());
          raw.directives = self.directive_uses(def.directives(), DirectiveLocation::Object);
          raw.fields = self.fields(def.fields_definition());
        }
        TypeExtension::Interface(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::Interface);
          raw.implements = self.implements(def.implements());
          raw.directives = self.directive_uses(def.directives(), DirectiveLocation::Interface);
          raw.fields = self.fields(def.fields_definition());
        }
        TypeExtension::Union(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::Union);
          raw.directives = self.directive_uses(def.directives(), DirectiveLocation::Union);
          raw.members = self.members(def.member_types());
        }
        TypeExtension::Enum(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::Enum);
          raw.directives = self.directive_uses(def.directives(), DirectiveLocation::Enum);
          raw.enum_values = self.enum_values(def.enum_values_definition());
        }
        TypeExtension::InputObject(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::InputObject);
          raw.directives = self.directive_uses(def.directives(), DirectiveLocation::InputObject);
          raw.input_fields = self.input_fields(def.fields_definition());
        }
      },
    }
    self.extensions.push(raw);
  }

  fn apply_extensions(&mut self) {
    let extensions = core::mem::take(&mut self.extensions);
    for extension in extensions {
      let Some(target) = extension.target else {
        // `extend schema`. Its directives join the schema definition's own list, because the two
        // are one location: `schema @d` plus `extend schema @d` repeats a non-repeatable `@d`.
        self.schema_directives.extend(extension.directives);
        for (operation, located) in extension.roots {
          self.set_root(operation, located);
        }
        continue;
      };
      let Some(index) = self.type_index(target.sym) else {
        let subject = self.text(target.sym).to_owned();
        self.push(SchemaErrorKind::UndefinedExtensionTarget, &subject, target);
        continue;
      };
      if self.types[index].kind != extension.kind.unwrap_or(self.types[index].kind) {
        let subject = self.text(target.sym).to_owned();
        let related = self.types[index].name.span;
        self.push_related(
          SchemaErrorKind::ExtensionKindMismatch,
          &subject,
          None,
          target,
          related,
        );
        continue;
      }
      // Draft §3.10.3(5): "the `@oneOf` directive must not be provided by an Input Object type
      // extension". Here and not in `validate_types` because this is the last moment the
      // provenance exists: one line further down the extension's directives join the definition's
      // and nothing can tell which list a `@oneOf` came from.
      //
      // Scoped to an input object target so that one mistake produces one diagnostic:
      // `extend type T @oneOf` is already `UnsupportedDirectiveLocation`, and saying it twice
      // would be worse than saying it once.
      if self.types[index].kind == TypeKind::InputObject {
        let provided: Vec<Located> = extension
          .directives
          .iter()
          .filter(|used| self.text(used.name.sym) == ONE_OF)
          .map(|used| used.name)
          .collect();
        let subject = self.text(target.sym).to_owned();
        for at in provided {
          self.push_owned(
            SchemaErrorKind::OneOfOnInputObjectExtension,
            ONE_OF,
            subject.clone(),
            at,
          );
        }
      }

      let raw = &mut self.types[index];
      raw.implements.extend(extension.implements);
      raw.fields.extend(extension.fields);
      raw.input_fields.extend(extension.input_fields);
      raw.members.extend(extension.members);
      raw.enum_values.extend(extension.enum_values);
      raw.directives.extend(extension.directives);
    }
  }

  // -- built-ins ------------------------------------------------------------------------------

  /// Parses one of the crate's own SDL constants.
  ///
  /// # Panics
  ///
  /// If a constant in [`builtin`] stops parsing, which is a defect in this crate rather than in
  /// any input. `builtin_sdl_parses` in the build test suite is the standing guard.
  fn parse_builtin(sdl: &'static str) -> TypeSystemDocument<&'static str> {
    Parser::with_parser::<
      GraphqlLexer<'static, str>,
      TypeSystemDocument<&'static str>,
      GraphqlErrors<&'static str>,
      _,
      GraphQL,
    >(type_system_document)
    .parse_str(sdl)
    .expect("smear's own built-in SDL must parse; this is a defect in smear, not in the input")
  }

  /// Reads the three built-in documents, parsing them at most once per process.
  ///
  /// The parse is 15.2 µs of a 42.6 µs empty build, and it is the same 15.2 µs every time: the
  /// input is three `&'static str` constants of this crate's own. Caching it is what takes the
  /// build's corpus-independent cost from 8.9x `apollo-compiler`'s to something comparable, since
  /// apollo caches its equivalent the same way.
  ///
  /// # Not on a core without `std`
  ///
  /// [`OnceLock`](std::sync::OnceLock) is not in `alloc`, and the `validator` feature does not
  /// imply `std` — the schema representation reaches a Cortex-M0+ with no compare-and-swap, and a
  /// cache that needed one would take that away for a startup cost such a target pays once. So the
  /// `not(std)` arm parses per build. The two arms call the same
  /// [`parse_builtin`](SchemaBuilder::parse_builtin), so `builtin_sdl_parses` guards both, and
  /// `a_warm_schema_build_allocates_only_for_the_schema` carries a ceiling for each.
  #[cfg(feature = "std")]
  fn built_ins() -> &'static BuiltIns {
    static CACHE: std::sync::OnceLock<BuiltIns> = std::sync::OnceLock::new();
    CACHE.get_or_init(BuiltIns::parse)
  }

  fn inject_built_ins(&mut self) {
    #[cfg(feature = "std")]
    let built_ins = Self::built_ins();
    #[cfg(not(feature = "std"))]
    let built_ins = &BuiltIns::parse();
    self.inject(built_ins);
  }

  fn inject(&mut self, built_ins: &BuiltIns) {
    // Anything injected here belongs to no document the caller supplied; `self.document` is
    // already one past the last real index, which is what an injected definition reports under.

    // The introspection meta-schema is unconditional: an introspection query must validate
    // against every schema. A user definition of one of its types collides.
    for entry in built_ins.introspection.definitions() {
      let TypeSystemDefinitionOrExtension::Definition(described) = entry else {
        continue;
      };
      let TypeSystemDefinition::Type(ty) = described.node() else {
        continue;
      };
      let name = type_definition_name(ty);
      match self.interner.lookup(name.as_bytes()).and_then(|sym| {
        self
          .type_index(sym)
          .map(|index| (sym, self.types[index].name))
      }) {
        Some((sym, at)) => {
          let subject = self.text(sym).to_owned();
          self.push(SchemaErrorKind::RedefinedBuiltInType, &subject, at);
          if let Some(index) = self.type_index(sym) {
            self.types[index].collides_with_built_in = true;
          }
        }
        None => self.type_definition(ty, true),
      }
    }

    // Built-in scalars and directives are replaceable: a document that spells one out keeps its
    // own, which is what makes a printed schema re-readable.
    for entry in built_ins.scalars.definitions() {
      let TypeSystemDefinitionOrExtension::Definition(described) = entry else {
        continue;
      };
      let TypeSystemDefinition::Type(ty) = described.node() else {
        continue;
      };
      let name = type_definition_name(ty);
      let taken = self
        .interner
        .lookup(name.as_bytes())
        .is_some_and(|sym| self.type_index(sym).is_some());
      if !taken {
        self.type_definition(ty, true);
      }
    }

    for entry in built_ins.directives.definitions() {
      let TypeSystemDefinitionOrExtension::Definition(described) = entry else {
        continue;
      };
      let TypeSystemDefinition::Directive(def) = described.node() else {
        continue;
      };
      let taken = self
        .interner
        .lookup(def.name().source().as_bytes())
        .is_some_and(|sym| self.directive_index(sym).is_some());
      if !taken {
        self.directive_definition(def, true);
      }
    }

    // The three meta-field names, interned now so flattening never has to grow the arena.
    self.interner.intern(builtin::TYPENAME_FIELD.as_bytes());
    self.interner.intern(builtin::SCHEMA_FIELD.as_bytes());
    self.interner.intern(builtin::TYPE_FIELD.as_bytes());
    self
      .interner
      .intern(builtin::TYPE_FIELD_ARGUMENT.as_bytes());
  }

  // -- validation (draft §3) --------------------------------------------------------------------

  fn validate(&mut self) {
    self.resolve_roots();
    self.resolve_type_refs();
    self.compute_closures();
    self.collect_required_input_fields();
    self.validate_types();
    self.validate_directive_definitions();
    self.validate_directive_usages();
    self.validate_interface_implementations();
    self.validate_input_object_cycles();
    self.validate_input_object_default_cycles();
    self.validate_directive_cycles();
  }

  fn resolve_roots(&mut self) {
    // With no `schema` definition, the roots default to the conventionally named object types.
    for operation in RootOperation::ALL {
      if self.roots[operation.index()].is_some() {
        continue;
      }
      let name = operation.default_type_name();
      if let Some(sym) = self.interner.lookup(name.as_bytes())
        && let Some(index) = self.type_index(sym)
        && self.types[index].kind == TypeKind::Object
      {
        self.roots[operation.index()] = Some(self.types[index].name);
      }
    }

    for operation in RootOperation::ALL {
      let Some(root) = self.roots[operation.index()] else {
        if operation == RootOperation::Query {
          self.errors.push(
            SchemaError::new(
              SchemaErrorKind::MissingQueryRootOperationType,
              "query",
              self.schema_definition.unwrap_or_default(),
            )
            .in_document(0),
          );
        }
        continue;
      };
      match self.type_index(root.sym) {
        None => {
          let subject = self.text(root.sym).to_owned();
          self.push(SchemaErrorKind::UndefinedRootOperationType, &subject, root);
          self.roots[operation.index()] = None;
        }
        Some(index) if self.types[index].kind != TypeKind::Object => {
          let subject = self.text(root.sym).to_owned();
          self.push(SchemaErrorKind::RootOperationTypeNotObject, &subject, root);
          self.roots[operation.index()] = None;
        }
        Some(_) => {}
      }
    }

    // Draft §3.3: "The `query`, `mutation`, and `subscription` root types must all be different
    // types if provided."
    //
    // Run after the loop above rather than inside it, over what survived: a root that named
    // nothing, or named something that is not an object, has already been reported and cleared, so
    // one mistake still produces one diagnostic. The three defaults cannot collide — they are three
    // distinct names — so this can only fire on a `schema` block or an `extend schema`.
    for operation in RootOperation::ALL {
      let Some(root) = self.roots[operation.index()] else {
        continue;
      };
      let earlier = RootOperation::ALL
        .iter()
        .take_while(|previous| **previous != operation)
        .find_map(|previous| self.roots[previous.index()].filter(|other| other.sym == root.sym));
      if let Some(first) = earlier {
        let subject = self.text(root.sym).to_owned();
        self.push_related(
          SchemaErrorKind::SharedRootOperationType,
          &subject,
          None,
          root,
          first.span,
        );
      }
    }
  }

  /// Resolves every type reference's base to a type index, reporting the ones that name nothing.
  ///
  /// The two report lists carry the offending position's [`Coordinate`] rather than its rendered
  /// path: the loops below visit every field, argument and input field of every type — the
  /// built-in ones included — and all but the handful that fail need no path at all. Rendering is
  /// deferred to the push loops at the end, where `&mut self` is free anyway.
  fn resolve_type_refs(&mut self) {
    let mut unresolved: Vec<(Located, Coordinate, SimpleSpan)> = Vec::new();
    let mut too_deep: Vec<(Located, Coordinate, SimpleSpan)> = Vec::new();

    for index in 0..self.types.len() {
      let owner = Coordinate::named(self.types[index].name.sym);

      for field in 0..self.types[index].fields.len() {
        let path = owner.then(self.types[index].fields[field].name.sym);
        Self::resolve_one(
          &self.type_of_sym,
          &mut self.types[index].fields[field].ty,
          path,
          &mut unresolved,
          &mut too_deep,
        );
        // A refused list is not resolved, for the reason no later pass reads one: the field is
        // already refused, `finish` will not hand the schema out, and an `UndefinedType` naming
        // an argument of a field nobody may declare is a diagnostic about the wrong defect. See
        // [`Declared`].
        if let ArgsMut::Bounded(args) = self.types[index].fields[field].args.read_mut() {
          for arg in args {
            let path = path.then(arg.name.sym);
            Self::resolve_one(
              &self.type_of_sym,
              &mut arg.ty,
              path,
              &mut unresolved,
              &mut too_deep,
            );
          }
        }
      }

      for field in 0..self.types[index].input_fields.len() {
        let path = owner.then(self.types[index].input_fields[field].name.sym);
        Self::resolve_one(
          &self.type_of_sym,
          &mut self.types[index].input_fields[field].ty,
          path,
          &mut unresolved,
          &mut too_deep,
        );
      }
    }

    for index in 0..self.directives.len() {
      let owner = Coordinate::named(self.directives[index].name.sym);
      if let ArgsMut::Bounded(args) = self.directives[index].args.read_mut() {
        for arg in args {
          let path = owner.then(arg.name.sym);
          Self::resolve_one(
            &self.type_of_sym,
            &mut arg.ty,
            path,
            &mut unresolved,
            &mut too_deep,
          );
        }
      }
    }

    for (at, path, _) in unresolved {
      let subject = self.text(at.sym).to_owned();
      let path = self.owner(path);
      self.push_owned(SchemaErrorKind::UndefinedType, &subject, path, at);
    }
    for (at, path, span) in too_deep {
      let subject = self.text(at.sym).to_owned();
      let path = self.owner(path);
      let mut at = at;
      at.span = span;
      self.push_owned(SchemaErrorKind::TypeReferenceTooDeep, &subject, path, at);
    }
  }

  fn resolve_one(
    type_of_sym: &[u32],
    reference: &mut RawTypeRef,
    path: Coordinate,
    unresolved: &mut Vec<(Located, Coordinate, SimpleSpan)>,
    too_deep: &mut Vec<(Located, Coordinate, SimpleSpan)>,
  ) {
    if reference.too_deep {
      too_deep.push((reference.base, path, reference.span));
    }
    let slot = reference.base.sym.get() as usize;
    match type_of_sym.get(slot) {
      Some(&index) if index != u32::MAX => {
        reference.packed = PackedType::from_parts(
          reference.base.sym,
          TypeId::new(index),
          reference.packed.wrappers(),
        );
      }
      _ => unresolved.push((reference.base, path, reference.span)),
    }
  }

  /// Fills every type's interface closure, reporting interfaces that implement themselves.
  fn compute_closures(&mut self) {
    let count = self.types.len();
    for index in 0..count {
      if self.types[index].implements.is_empty() {
        continue;
      }
      let mut closure: Vec<u32> = Vec::new();
      let mut stack: Vec<u32> = Vec::new();
      for declared in &self.types[index].implements {
        if let Some(target) = self.type_index(declared.sym) {
          stack.push(target as u32);
        }
      }
      while let Some(next) = stack.pop() {
        if closure.contains(&next) {
          continue;
        }
        closure.push(next);
        for declared in &self.types[next as usize].implements {
          if let Some(target) = self.type_index(declared.sym) {
            stack.push(target as u32);
          }
        }
      }
      closure.sort_unstable();
      self.types[index].closure = closure;
    }

    for index in 0..count {
      if self.types[index].kind != TypeKind::Interface {
        continue;
      }
      if self.types[index].closure.contains(&(index as u32)) {
        let at = self.types[index].name;
        let subject = self.text(at.sym).to_owned();
        self.push(SchemaErrorKind::SelfImplementingInterface, &subject, at);
        // Break the cycle so nothing downstream walks it.
        self.types[index].closure.retain(|id| *id != index as u32);
      }
    }
  }

  /// Fills [`RawType::required_input_fields`], one pass over the input fields the whole schema
  /// declares.
  ///
  /// Here rather than at ingest because an `extend input` appends to `input_fields` after the
  /// definition was read, and here rather than at the literal because the literal is the site the
  /// field exists to keep proportional. Nothing it reads changes during validation:
  /// [`RawInput::is_required`] is a `Non-Null` wrapper and a `DefaultKind`, both fixed when the
  /// declaration was ingested.
  fn collect_required_input_fields(&mut self) {
    for index in 0..self.types.len() {
      if self.types[index].kind != TypeKind::InputObject {
        continue;
      }
      let required: Vec<u32> = self.types[index]
        .input_fields
        .iter()
        .enumerate()
        .filter(|(_, field)| field.is_required())
        .map(|(at, _)| at as u32)
        .collect();
      self.types[index].required_input_fields = required;
    }
  }

  fn validate_types(&mut self) {
    for index in 0..self.types.len() {
      let kind = self.types[index].kind;
      let built_in = self.types[index].built_in;
      let at = self.types[index].name;
      let owner = Coordinate::named(at.sym);

      if !built_in
        && !self.types[index].collides_with_built_in
        && is_reserved(self.text(at.sym).as_bytes())
      {
        let name = self.text(at.sym).to_owned();
        self.push(SchemaErrorKind::ReservedTypeName, &name, at);
      }

      // `@oneOf` is read here rather than at ingest so an extension that adds it is seen. Adding
      // it *is* draft §3.10.3(5)'s refusal, reported by `apply_extensions`; the flag is still set
      // from the merged list so that §3.10.3(6) — the OneOf constraints over the extension's own
      // fields — is checked rather than hidden behind that refusal.
      let one_of = self.has_directive(index, ONE_OF);

      match kind {
        TypeKind::Scalar => {}
        TypeKind::Object | TypeKind::Interface => {
          self.validate_fields(index, owner);
          self.validate_implements(index, owner);
        }
        TypeKind::Union => self.validate_union(index, owner),
        TypeKind::Enum => self.validate_enum(index, owner),
        TypeKind::InputObject => self.validate_input_object(index, owner, one_of),
      }
    }
  }

  fn has_directive(&self, index: usize, name: &str) -> bool {
    self.types[index]
      .directives
      .iter()
      .any(|used| self.text(used.name.sym) == name)
  }

  fn validate_fields(&mut self, index: usize, owner: Coordinate) {
    if self.types[index].fields.is_empty() {
      let at = self.types[index].name;
      let subject = self.owner(owner);
      self.push(SchemaErrorKind::EmptyFieldsDefinition, &subject, at);
      return;
    }

    let mut seen = Duplicates::over(self.types[index].fields.len(), |at| {
      self.types[index].fields[at].name.sym
    });
    for field in 0..self.types[index].fields.len() {
      let at = self.types[index].fields[field].name;
      let path = owner.then(at.sym);

      if let Some(earlier) = seen.first(field, at.sym) {
        let first = self.types[index].fields[earlier].name.span;
        let name = self.text(at.sym).to_owned();
        let owner = self.owner(owner);
        self.push_related(
          SchemaErrorKind::DuplicateFieldName,
          &name,
          Some(owner),
          at,
          first,
        );
      }

      if !self.types[index].built_in && is_reserved(self.text(at.sym).as_bytes()) {
        let name = self.text(at.sym).to_owned();
        let owner = self.owner(owner);
        self.push_owned(SchemaErrorKind::ReservedFieldName, &name, owner, at);
      }

      let base = self.types[index].fields[field].ty.packed.base_id();
      if base != UNRESOLVED && !self.types[base.get() as usize].kind.is_output() {
        let subject = self
          .text(self.types[base.get() as usize].name.sym)
          .to_owned();
        let path = self.owner(path);
        let mut where_ = at;
        where_.span = self.types[index].fields[field].ty.span;
        self.push_owned(
          SchemaErrorKind::FieldTypeNotOutputType,
          &subject,
          path,
          where_,
        );
      }

      // Draft §6.4.1 `CoerceArgumentValues` iterates this whole list at every runtime position of
      // the field, so `positions × declared` is a product an executor meets once per response. It
      // can charge the positions — they are the driver's — and it cannot charge `declared` without
      // billing a request for the service's own design-time width. So the deployment's factor is
      // bounded where the deployment writes it. See `MAX_FIELD_ARGUMENTS`. al8n/smear#198.
      //
      // The refusal is RECORDED here and ENFORCED by [`Declared`], and that split is what
      // distinguishes this from the two shapes that came before it. Ordering the check in front of
      // the argument walk settled only the diagnostics — a field a thousand arguments wide would
      // otherwise report a thousand argument diagnostics ahead of the one saying why the field is
      // refused. Adding a `continue` bounded the one walk the check stood in front of. Review then
      // found two more walks it did not stand in front of at all: a directive usage's scan of its
      // definition's declared list, and interface conformance's scan of both sides of a field.
      // A guard in front of one caller bounds one caller.
      //
      // So there is no `continue` here any more. Below this line the list is reached only through
      // `Declared::read`, which hands an over-limit list to nobody — this pass included, and a
      // pass written next year included, without either having been told about the ceiling.
      //
      // And the list is gone by the time this asks. `Declared::from` decided the refusal and
      // dropped the arguments; what survives to here is the state, plus the field's own name and
      // span, which is all either half of this diagnostic ever read.
      if self.types[index].fields[field].args.refused() {
        let name = self.text(at.sym).to_owned();
        let owner = self.owner(owner);
        self.push_owned(SchemaErrorKind::TooManyFieldArguments, &name, owner, at);
      }

      let built_in = self.types[index].built_in;
      let (model, errors, names) = self.split_indexed();
      Self::validate_arguments(
        model,
        errors,
        names,
        ArgumentsOf::Field { ty: index, field },
        path,
        built_in,
        ArgumentRules {
          duplicate: SchemaErrorKind::DuplicateArgumentName,
          reserved: SchemaErrorKind::ReservedArgumentName,
          not_input: SchemaErrorKind::ArgumentTypeNotInputType,
        },
      );
    }
  }

  /// One declared argument list, against the §3.6.1 rules that read the arguments themselves.
  ///
  /// Over a [`Model`] and the error list rather than `&mut self`, which is what lets the gated
  /// list be read **once** and held for the whole walk: every rule below reports through `errors`,
  /// the half [`SchemaBuilder::split`] keeps disjoint from the half the list lives in. The shape
  /// this replaces re-addressed the list at each of seven reads, because a `&self.types[…].args`
  /// could not survive a `self.push*` — and each of those reads would now be a separate encounter
  /// with the gate, saying seven times what the first `let` says once.
  fn validate_arguments(
    model: Model<'_>,
    errors: &mut Vec<SchemaError>,
    names: &mut DeclaredNames,
    args: ArgumentsOf,
    owner: Coordinate,
    built_in: bool,
    rules: ArgumentRules,
  ) {
    // The gate, and the whole of the bound on this pass. Its caller records the refusal; nothing
    // here needs to know that, because a refused list hands out no arguments to walk. `Duplicates`
    // is the scan the previous round kept, and this is the one §3 list a ceiling holds at
    // sixty-four — written through the shared type all the same, because a copy of the shape kept
    // because a ceiling happens to hold this one list today is the copy the next ceiling change
    // re-opens.
    let Args::Bounded(declared) = model.arguments(args) else {
      return;
    };

    let mut seen = Duplicates::over(declared.len(), |at| declared[at].name.sym);
    for (index, argument) in declared.iter().enumerate() {
      let at = argument.name;
      let ty = argument.ty;
      let required = argument.is_required();

      if let Some(earlier) = seen.first(index, at.sym) {
        let first = declared[earlier].name.span;
        let name = model.text(at.sym).to_owned();
        let owner = model.owner(owner);
        push_related(errors, rules.duplicate, &name, Some(owner), at, first);
      }

      if !built_in && is_reserved(model.text(at.sym).as_bytes()) {
        let name = model.text(at.sym).to_owned();
        let owner = model.owner(owner);
        push_owned(errors, rules.reserved, &name, owner, at);
      }

      let base = ty.packed.base_id();
      if base != UNRESOLVED && !model.types[base.get() as usize].kind.is_input() {
        let subject = model
          .text(model.types[base.get() as usize].name.sym)
          .to_owned();
        let path = model.owner(owner.then(at.sym));
        let mut where_ = at;
        where_.span = ty.span;
        push_owned(errors, rules.not_input, &subject, path, where_);
      }

      // Draft §3.6.1(2.4.4.1): "if argument type is Non-Null and a default value is not defined,
      // the `@deprecated` directive must not be applied to this argument" — which is exactly
      // `is_required`.
      if required && is_deprecated(model.interner, &argument.directives) {
        let name = model.text(at.sym).to_owned();
        let owner = model.owner(owner);
        push_owned(
          errors,
          SchemaErrorKind::DeprecatedRequiredArgument,
          &name,
          owner,
          at,
        );
      }

      // Draft §3.6.1(2.4.5): "if the argument has a default value it must be compatible with
      // `argumentType` as per the coercion rules for that type" — the same coercion procedure a
      // directive argument's *supplied* value goes through, so the two cannot answer differently.
      if let Some(default) = argument.default_value.as_ref() {
        Self::check_const_value(
          model,
          errors,
          names,
          default,
          ty.packed,
          Blame {
            owner,
            subject: at.sym,
            mismatch: SchemaErrorKind::InvalidDefaultValue,
            document: at.document,
          },
        );
      }
    }
  }

  fn validate_implements(&mut self, index: usize, owner: Coordinate) {
    let mut seen = Duplicates::over(self.types[index].implements.len(), |at| {
      self.types[index].implements[at].sym
    });
    for position in 0..self.types[index].implements.len() {
      let declared = self.types[index].implements[position];
      if let Some(earlier) = seen.first(position, declared.sym) {
        let first = self.types[index].implements[earlier].span;
        let name = self.text(declared.sym).to_owned();
        let owner = self.owner(owner);
        self.push_related(
          SchemaErrorKind::DuplicateImplementsInterface,
          &name,
          Some(owner),
          declared,
          first,
        );
        continue;
      }

      let kind = match self.type_index(declared.sym) {
        None => Some(SchemaErrorKind::UndefinedImplementsInterface),
        Some(target) if self.types[target].kind != TypeKind::Interface => {
          Some(SchemaErrorKind::ImplementsNonInterface)
        }
        Some(_) => None,
      };
      if let Some(kind) = kind {
        let name = self.text(declared.sym).to_owned();
        let owner = self.owner(owner);
        self.push_owned(kind, &name, owner, declared);
      }
    }
  }

  fn validate_union(&mut self, index: usize, owner: Coordinate) {
    if self.types[index].members.is_empty() {
      let at = self.types[index].name;
      let subject = self.owner(owner);
      self.push(SchemaErrorKind::EmptyUnionMembers, &subject, at);
      return;
    }
    let mut seen = Duplicates::over(self.types[index].members.len(), |at| {
      self.types[index].members[at].sym
    });
    for position in 0..self.types[index].members.len() {
      let member = self.types[index].members[position];
      if let Some(earlier) = seen.first(position, member.sym) {
        let first = self.types[index].members[earlier].span;
        let name = self.text(member.sym).to_owned();
        let owner = self.owner(owner);
        self.push_related(
          SchemaErrorKind::DuplicateUnionMember,
          &name,
          Some(owner),
          member,
          first,
        );
        continue;
      }

      let kind = match self.type_index(member.sym) {
        None => Some(SchemaErrorKind::UndefinedUnionMember),
        Some(target) if self.types[target].kind != TypeKind::Object => {
          Some(SchemaErrorKind::UnionMemberNotObject)
        }
        Some(_) => None,
      };
      if let Some(kind) = kind {
        let name = self.text(member.sym).to_owned();
        let owner = self.owner(owner);
        self.push_owned(kind, &name, owner, member);
      }
    }
  }

  fn validate_enum(&mut self, index: usize, owner: Coordinate) {
    if self.types[index].enum_values.is_empty() {
      let at = self.types[index].name;
      let subject = self.owner(owner);
      self.push(SchemaErrorKind::EmptyEnumValues, &subject, at);
      return;
    }
    let built_in = self.types[index].built_in;
    let mut seen = Duplicates::over(self.types[index].enum_values.len(), |at| {
      self.types[index].enum_values[at].name.sym
    });
    for position in 0..self.types[index].enum_values.len() {
      let value = self.types[index].enum_values[position].name;
      if let Some(earlier) = seen.first(position, value.sym) {
        let first = self.types[index].enum_values[earlier].name.span;
        let name = self.text(value.sym).to_owned();
        let owner = self.owner(owner);
        self.push_related(
          SchemaErrorKind::DuplicateEnumValue,
          &name,
          Some(owner),
          value,
          first,
        );
      }
      if !built_in && is_reserved(self.text(value.sym).as_bytes()) {
        let name = self.text(value.sym).to_owned();
        let owner = self.owner(owner);
        self.push_owned(SchemaErrorKind::ReservedEnumValueName, &name, owner, value);
      }
    }
  }

  fn validate_input_object(&mut self, index: usize, owner: Coordinate, one_of: bool) {
    if self.types[index].input_fields.is_empty() {
      let at = self.types[index].name;
      let subject = self.owner(owner);
      self.push(SchemaErrorKind::EmptyInputFields, &subject, at);
      return;
    }
    let built_in = self.types[index].built_in;
    let mut seen = Duplicates::over(self.types[index].input_fields.len(), |at| {
      self.types[index].input_fields[at].name.sym
    });
    for position in 0..self.types[index].input_fields.len() {
      let field = &self.types[index].input_fields[position];
      let at = field.name;
      let ty = field.ty;
      let required = field.is_required();
      let has_default = field.default.is_present();

      if let Some(earlier) = seen.first(position, at.sym) {
        let first = self.types[index].input_fields[earlier].name.span;
        let name = self.text(at.sym).to_owned();
        let owner = self.owner(owner);
        self.push_related(
          SchemaErrorKind::DuplicateInputFieldName,
          &name,
          Some(owner),
          at,
          first,
        );
      }

      if !built_in && is_reserved(self.text(at.sym).as_bytes()) {
        let name = self.text(at.sym).to_owned();
        let owner = self.owner(owner);
        self.push_owned(SchemaErrorKind::ReservedInputFieldName, &name, owner, at);
      }

      let base = ty.packed.base_id();
      if base != UNRESOLVED && !self.types[base.get() as usize].kind.is_input() {
        let subject = self
          .text(self.types[base.get() as usize].name.sym)
          .to_owned();
        let path = self.owner(owner.then(at.sym));
        let mut where_ = at;
        where_.span = ty.span;
        self.push_owned(
          SchemaErrorKind::InputFieldTypeNotInputType,
          &subject,
          path,
          where_,
        );
      }

      // Draft §3.10.1(2.4.1), the input-field twin of §3.6.1(2.4.4.1).
      if required
        && is_deprecated(
          &self.interner,
          &self.types[index].input_fields[position].directives,
        )
      {
        let name = self.text(at.sym).to_owned();
        let owner = self.owner(owner);
        self.push_owned(
          SchemaErrorKind::DeprecatedRequiredInputField,
          &name,
          owner,
          at,
        );
      }

      // The same coercion §3.6.1(2.4.5) demands of an argument's default, at the position the
      // draft's numbered list leaves implicit. See `SchemaErrorKind::InvalidDefaultValue`.
      //
      // Read in place rather than taken: an input object may declare a default of its own type,
      // and the check reads `input_fields` — the very list this loop is walking.
      if self.types[index].input_fields[position]
        .default_value
        .is_some()
      {
        let (model, errors, names) = self.split_indexed();
        let default = model.types[index].input_fields[position]
          .default_value
          .as_ref()
          .expect("just observed to be present");
        Self::check_const_value(
          model,
          errors,
          names,
          default,
          ty.packed,
          Blame {
            owner,
            subject: at.sym,
            mismatch: SchemaErrorKind::InvalidDefaultValue,
            document: at.document,
          },
        );
      }

      if one_of {
        if ty.packed.is_non_null() {
          let name = self.text(at.sym).to_owned();
          let owner = self.owner(owner);
          self.push_owned(SchemaErrorKind::OneOfFieldNotNullable, &name, owner, at);
        }
        if has_default {
          let name = self.text(at.sym).to_owned();
          let owner = self.owner(owner);
          self.push_owned(SchemaErrorKind::OneOfFieldHasDefault, &name, owner, at);
        }
      }
    }
  }

  fn validate_directive_definitions(&mut self) {
    for index in 0..self.directives.len() {
      let at = self.directives[index].name;
      let built_in = self.directives[index].built_in;

      if !built_in && is_reserved(self.text(at.sym).as_bytes()) {
        let name = self.text(at.sym).to_owned();
        self.push(SchemaErrorKind::ReservedDirectiveName, &name, at);
      }

      // The second population of the same list, refused the same way and enforced by the same
      // gate. A directive definition's arguments go through `validate_arguments` exactly as a
      // field's do; and the width is read again at every usage a document writes, which is the
      // product `MAX_DIRECTIVE_ARGUMENTS` states and the scan review found second. The field
      // ceiling did not reach here — one constant enforced at one site — which is the shape
      // [`Declared`] retires: the ceiling now travels with the list rather than with the site,
      // and past it the list is not carried at all. al8n/smear#198.
      if self.directives[index].args.refused() {
        let name = self.text(at.sym).to_owned();
        self.push(SchemaErrorKind::TooManyDirectiveArguments, &name, at);
      }

      let (model, errors, names) = self.split_indexed();
      Self::validate_arguments(
        model,
        errors,
        names,
        ArgumentsOf::Directive { index },
        Coordinate::named(at.sym),
        built_in,
        ArgumentRules {
          duplicate: SchemaErrorKind::DuplicateDirectiveArgumentName,
          reserved: SchemaErrorKind::ReservedDirectiveArgumentName,
          not_input: SchemaErrorKind::DirectiveArgumentTypeNotInputType,
        },
      );
    }
  }

  // -- validation: directive usages (draft §3.13, at a use site) ------------------------------

  /// Every directive written on a type-system element, against the definition it names.
  ///
  /// This is the SDL half of six rules the executable side already runs per request — draft 5.7.1,
  /// 5.7.2, 5.7.3, 5.4.1, 5.4.3 and 5.6.1 — and it asks them of the one place they would otherwise
  /// never be asked. A server builds its schema once and then trusts it, so a directive misuse
  /// that survives the build is never diagnosed at all.
  ///
  /// It runs after [`SchemaBuilder::resolve_type_refs`] because an argument's declared type has to
  /// be resolved before a value can be checked against it, and after
  /// [`SchemaBuilder::apply_extensions`] because a type and its extensions are one location.
  fn validate_directive_usages(&mut self) {
    let (model, errors, names) = self.split_indexed();
    Self::check_directive_uses(model, errors, names, DirectivesOf::Schema, Coordinate::schema());

    for ty in 0..model.types.len() {
      let owner = Coordinate::named(model.types[ty].name.sym);
      Self::check_directive_uses(model, errors, names, DirectivesOf::Type { ty }, owner);

      for field in 0..model.types[ty].fields.len() {
        let path = owner.then(model.types[ty].fields[field].name.sym);
        Self::check_directive_uses(model, errors, names, DirectivesOf::Field { ty, field }, path);

        // The gate decides whether this field's arguments are visited at all, which is also
        // what makes `DirectivesOf::FieldArgument`'s own refused arm unreachable: an `arg`
        // position exists only for a list admitted here.
        if let Args::Bounded(declared) = model.arguments(ArgumentsOf::Field { ty, field }) {
          for (arg, argument) in declared.iter().enumerate() {
            let path = path.then(argument.name.sym);
            Self::check_directive_uses(
              model,
              errors,
              names,
              DirectivesOf::FieldArgument { ty, field, arg },
              path,
            );
          }
        }
      }

      for field in 0..model.types[ty].input_fields.len() {
        let path = owner.then(model.types[ty].input_fields[field].name.sym);
        Self::check_directive_uses(model, errors, names, DirectivesOf::InputField { ty, field }, path);
      }

      for value in 0..model.types[ty].enum_values.len() {
        let path = owner.then(model.types[ty].enum_values[value].name.sym);
        Self::check_directive_uses(model, errors, names, DirectivesOf::EnumValue { ty, value }, path);
      }
    }

    for directive in 0..model.directives.len() {
      let owner = Coordinate::named(model.directives[directive].name.sym);
      if let Args::Bounded(declared) = model.arguments(ArgumentsOf::Directive { index: directive })
      {
        for (arg, argument) in declared.iter().enumerate() {
          let path = owner.then(argument.name.sym);
          Self::check_directive_uses(
            model,
            errors,
            names,
            DirectivesOf::DirectiveArgument { directive, arg },
            path,
          );
        }
      }
    }
  }

  /// One element's directive list: defined, allowed here, not repeated, and correctly argued.
  ///
  /// Over a [`Model`] rather than `&mut self` so that the list can be read where it lies. The
  /// owner arrives as a [`Coordinate`]: this runs once per type, field, argument, input field,
  /// enum value and directive argument in the merged schema, and most of those carry no directive
  /// at all — the pass cost 6.4 µs on a document with none, all of it owner paths built before the
  /// empty list was looked at.
  fn check_directive_uses(
    model: Model<'_>,
    errors: &mut Vec<SchemaError>,
    names: &mut DeclaredNames,
    at: DirectivesOf,
    owner: Coordinate,
  ) {
    // Only a definition the schema knows can be known non-repeatable, so an undefined directive
    // written twice is the undefined-directive mistake twice over and not also this one.
    let mut seen = Duplicates::over(model.directive_uses(at).len(), |position| {
      model.directive_uses(at)[position].name.sym
    });

    for index in 0..model.directive_uses(at).len() {
      let used = &model.directive_uses(at)[index];
      let Some(definition) = model.directive_index(used.name.sym) else {
        let name = model.text(used.name.sym).to_owned();
        push_owned(
          errors,
          SchemaErrorKind::UndefinedDirective,
          &name,
          model.owner(owner),
          used.name,
        );
        continue;
      };

      // The `&&` is what keeps a repeatable directive out of the record, exactly as the `match`
      // arm it replaced did: `Duplicates::first` is asked only where the rule applies.
      if !model.directives[definition].repeatable
        && let Some(earlier) = seen.first(index, used.name.sym)
      {
        let first = model.directive_uses(at)[earlier].name.span;
        let name = model.text(used.name.sym).to_owned();
        push_related(
          errors,
          SchemaErrorKind::DuplicateDirectiveUse,
          &name,
          Some(model.owner(owner)),
          used.name,
          first,
        );
      }

      // The whole of the location rule: one shift and one `AND` against the word the definition
      // was reduced to at ingest — the same `DirectiveLocations::contains` the executable rules
      // call, so the two cannot answer differently.
      if !model.directives[definition]
        .locations
        .contains(used.location)
      {
        let name = model.text(used.name.sym).to_owned();
        push_owned(
          errors,
          SchemaErrorKind::UnsupportedDirectiveLocation,
          &name,
          model.owner(owner),
          used.name,
        );
      }

      Self::check_directive_arguments(model, errors, names, at, index, definition, owner);
    }
  }

  /// One usage's arguments, against the ones its definition declares.
  fn check_directive_arguments(
    model: Model<'_>,
    errors: &mut Vec<SchemaError>,
    names: &mut DeclaredNames,
    at: DirectivesOf,
    index: usize,
    definition: usize,
    owner: Coordinate,
  ) {
    let used = &model.directive_uses(at)[index];
    // Rendered only where one is reported. `check_directive_arguments` runs for every well-formed
    // usage too, and a supergraph has thousands of them.
    let coordinate = owner.at_directive(used.name.sym);

    // Draft 5.4.2's SDL twin. Unlike the repeatability rule above, this one needs nothing from the
    // definition — an argument written twice is a mistake whether or not the directive declares it
    // — so it is checked for every written argument, including one that is about to be reported
    // as undefined. It is above the gate for that same reason: it reads the usage, which no
    // ceiling holds, and `Duplicates` is what bounds it.
    let mut seen = Duplicates::over(used.args.len(), |at| used.args[at].name.sym);
    for (position, argument) in used.args.iter().enumerate() {
      if let Some(earlier) = seen.first(position, argument.name.sym) {
        let first = used.args[earlier].name.span;
        let name = model.text(argument.name.sym).to_owned();
        push_related(
          errors,
          SchemaErrorKind::DuplicateDirectiveArgumentUse,
          &name,
          Some(model.owner(coordinate)),
          argument.name,
          first,
        );
      }
    }

    // Everything below reads the DEFINITION's declared list once per written argument, and that
    // is the `Θ(declared × written)` review found here: a definition past its ceiling is refused
    // at build, and every usage of it then re-scanned the oversized list anyway. The refusal is
    // recorded where the definition is validated; this is the reader being told, by the list.
    let Args::Bounded(declared) = model.arguments(ArgumentsOf::Directive { index: definition })
    else {
      return;
    };

    for argument in &used.args {
      let Some(expected) = declared.iter().find(|d| d.name.sym == argument.name.sym) else {
        let name = model.text(argument.name.sym).to_owned();
        push_owned(
          errors,
          SchemaErrorKind::UndefinedDirectiveArgument,
          &name,
          model.owner(coordinate),
          argument.name,
        );
        continue;
      };

      // An explicit `null` for a required argument is the required-argument mistake, not a
      // value-type one: one defect, one diagnostic, and it is the one that names the obligation.
      if expected.is_required() && matches!(argument.value.shape, RawShape::Null) {
        let name = model.text(argument.name.sym).to_owned();
        push_owned(
          errors,
          SchemaErrorKind::MissingRequiredDirectiveArgument,
          &name,
          model.owner(coordinate),
          argument.name,
        );
        continue;
      }

      Self::check_const_value(
        model,
        errors,
        names,
        &argument.value,
        expected.ty.packed,
        Blame {
          owner: coordinate,
          subject: argument.name.sym,
          mismatch: SchemaErrorKind::InvalidDirectiveArgumentValue,
          document: argument.name.document,
        },
      );
    }

    for expected in declared {
      if !expected.is_required() {
        continue;
      }
      let supplied = used
        .args
        .iter()
        .any(|argument| argument.name.sym == expected.name.sym);
      if supplied {
        continue;
      }
      // Nothing was written, so the usage itself is what the omission is blamed on.
      let name = model.text(expected.name.sym).to_owned();
      push_at(
        errors,
        SchemaErrorKind::MissingRequiredDirectiveArgument,
        &name,
        model.owner(coordinate),
        used.span,
        used.name.document,
      );
    }
  }

  /// One constant literal against the type declared for its position.
  ///
  /// The decision procedure is draft 5.6.1's, and the coercion table it consults is the one the
  /// executable rules consult: `null` only where the position is nullable, a non-list value
  /// standing for the one-element list containing it at any depth, an enum literal that must name
  /// a member, and a scalar that must be spelled the way its built-in says — with a custom scalar
  /// accepting anything, because only the service knows how to read one.
  ///
  /// What it deliberately does not report is a mistake some *other* rule already owns: an
  /// unresolved base type is the undefined-type refusal, and an output type in an input position
  /// is the directive-argument-type refusal. Reporting either again would make one defect print
  /// twice.
  ///
  /// # Two callers, one procedure, two names for the same verdict
  ///
  /// A directive usage's *supplied* argument and an `InputValueDefinition`'s *default* are the
  /// same question asked of two positions, so `mismatch` is what the caller wants the "this
  /// literal does not fit" verdict called —
  /// [`InvalidDirectiveArgumentValue`](SchemaErrorKind::InvalidDirectiveArgumentValue) for the
  /// first, [`InvalidDefaultValue`](SchemaErrorKind::InvalidDefaultValue) for the second — and it
  /// is the only thing that differs. The two input-object kinds are *not* parameterised: an
  /// input-object literal naming a field the type does not declare, or omitting a required one, is
  /// the same mistake with the same coordinate wherever the literal is written, and giving it two
  /// names would say otherwise.
  fn check_const_value(
    model: Model<'_>,
    errors: &mut Vec<SchemaError>,
    names: &mut DeclaredNames,
    value: &RawValue,
    expected: PackedType,
    blame: Blame,
  ) {
    let reject = |errors: &mut Vec<SchemaError>, span| {
      let subject = model.text(blame.subject).to_owned();
      push_at(
        errors,
        blame.mismatch,
        &subject,
        model.owner(blame.owner),
        span,
        blame.document,
      );
    };

    if matches!(value.shape, RawShape::Null) {
      if expected.is_non_null() {
        reject(errors, value.span);
      }
      return;
    }

    // Strip the outer non-null, then apply the singleton-to-list coercion: a non-list value in a
    // list position is the one-element list containing it, at any depth.
    let mut expected = expected.nullable();
    while expected.is_list() && !matches!(value.shape, RawShape::List(_)) {
      let Some(item) = expected.list_item() else {
        break;
      };
      expected = item.nullable();
    }

    if expected.is_list() {
      let (Some(item), RawShape::List(entries)) = (expected.list_item(), &value.shape) else {
        return;
      };
      for entry in entries {
        Self::check_const_value(model, errors, names, entry, item, blame);
      }
      return;
    }

    let base = expected.base_id();
    if base == UNRESOLVED {
      return;
    }
    let base = base.get() as usize;

    match model.types[base].kind {
      TypeKind::InputObject => {
        let RawShape::Object(fields) = &value.shape else {
          reject(errors, value.span);
          return;
        };
        // The literal is named by the input object it is being offered to, not by the argument
        // that carries it: `In.y` is what apollo's `UndefinedInputValue` says and what a nested
        // literal needs, because `Query.@v.p` cannot tell an offending field of the outer object
        // from one of the inner. The span still points at the field, so the usage is one lookup
        // away.
        let object = Coordinate::named(model.types[base].name.sym);
        let declared = &model.types[base].input_fields;
        // One index per input OBJECT, held by the caller and shared by every literal offered to
        // this type — the nested entry the recursion below is about to offer to a second one
        // included. A value per literal is what this was, and it was live across that recursion
        // and built again for every sibling; [`DeclaredNames`] carries what that cost.
        for field in fields {
          let Some(expected) = names
            .first(model.types, base, field.name.sym)
            .map(|at| &declared[at])
          else {
            // Draft 5.6.2's SDL twin.
            let name = model.text(field.name.sym).to_owned();
            push_owned(
              errors,
              SchemaErrorKind::UndefinedInputObjectField,
              &name,
              model.owner(object),
              field.name,
            );
            continue;
          };
          // Draft 5.6.4's SDL twin, the explicit-`null` half. Reported here rather than let
          // through to the value check below so that `{ x: null }` for a required `x` produces the
          // obligation once, and not also a non-null coercion failure.
          if expected.is_required() && matches!(field.value.shape, RawShape::Null) {
            let name = model.text(field.name.sym).to_owned();
            push_at(
              errors,
              SchemaErrorKind::MissingRequiredInputObjectField,
              &name,
              model.owner(object),
              field.value.span,
              blame.document,
            );
            continue;
          }
          let expected = expected.ty.packed;
          Self::check_const_value(model, errors, names, &field.value, expected, blame);
        }

        // Draft 5.6.4's SDL twin, the omitted half. Nothing was written, so the literal itself is
        // what the omission is blamed on — the same choice `check_directive_arguments` makes for
        // an omitted required argument.
        //
        // Over the required positions rather than over the declaration, and the literal indexed
        // rather than rescanned: the two factors of the same product, one bounded by what this
        // reports and the other by [`Names`] — which stays a value per list here, because what it
        // indexes is the literal's own entries and no type keys those. It is built after the
        // recursion above and dropped before this frame returns, so no second one is ever live
        // inside it. Source order is the declaration's either way, because
        // `required_input_fields` is filled by ascending position.
        let required = &model.types[base].required_input_fields;
        let of_written = Names::over(fields.len(), required.len(), |at| fields[at].name.sym);
        for &position in required {
          let expected = &declared[position as usize];
          if of_written
            .first(fields.len(), expected.name.sym, |at| fields[at].name.sym)
            .is_some()
          {
            continue;
          }
          let name = model.text(expected.name.sym).to_owned();
          push_at(
            errors,
            SchemaErrorKind::MissingRequiredInputObjectField,
            &name,
            model.owner(object),
            value.span,
            blame.document,
          );
        }
      }
      TypeKind::Enum => {
        let member = match &value.shape {
          RawShape::Enum(bytes) => model.interner.lookup(bytes).is_some_and(|sym| {
            model.types[base]
              .enum_values
              .iter()
              .any(|value| value.name.sym == sym)
          }),
          _ => false,
        };
        if !member {
          reject(errors, value.span);
        }
      }
      TypeKind::Scalar => {
        // A custom scalar accepts every literal, so only the five built-ins have anything to say.
        let name = model.text(model.types[base].name.sym);
        let accepted = BuiltInScalar::from_name(name.as_bytes())
          .is_none_or(|scalar| scalar.accepts(value.shape.shape(), value.shape.spelling()));
        if !accepted {
          reject(errors, value.span);
        }
      }
      // An object, interface or union declared as an argument type is already the
      // `DirectiveArgumentTypeNotInputType` refusal; there is no value that would fit it.
      TypeKind::Object | TypeKind::Interface | TypeKind::Union => {}
    }
  }

  /// Draft §3.6.1/§3.7.1: transitivity, covariance of field types, invariance of argument types.
  ///
  /// # Over the split, and this pass is what [`Model`] was separated for
  ///
  /// Every list this reads belongs to a type it is *not* reporting on — the interface's fields,
  /// the interface's closure — and `self.push_*` cannot be called while one of them is borrowed.
  /// The shape that stood here bought its way past that with `clone`, once per implementor, and
  /// the copies were deep: a [`RawField`] owns its directives, and every [`RawInput`] under it
  /// owns its own directives and its default literal, each an arbitrarily large tree.
  ///
  /// Two products came out of that, and only one of them is the ceiling's.
  ///
  /// - `Θ(implementors × declared arguments)`, from cloning an interface field whose argument
  ///   list a ceiling had *already* refused. [`Declared`] now drops a refused list where the
  ///   refusal is decided, so there is nothing left of it to copy.
  /// - `Θ(implementors × literal size)`, from cloning the default literals and directive
  ///   arguments of a field whose declared list is perfectly legal. **No ceiling bounds this
  ///   one**: *one* argument under the limit, carrying one large default, is enough — so neither
  ///   this ceiling nor a wider one could have closed it. Fitted at 2.00 in the exponent over
  ///   1k–64k and 85 s at the top of that ladder, on a document `Schema::build` **accepts**.
  ///
  /// So the copies are gone rather than bounded: the pass reads through [`Model`] and reports
  /// through the error list, the two halves borrowed apart. Nothing about which diagnostic is
  /// reported, in what order, or with which span depends on that — the loops below are the ones
  /// that were here, reading `&model.types[…]` where they read a copy. al8n/smear#198.
  ///
  /// # The third product, which is neither a copy nor a ceiling's
  ///
  /// Finding the implementor's field restarted a linear scan of its **whole** field list for
  /// every interface field, so a valid interface and object with the same `N` fields in source
  /// order — the honest case, not an adversarial one — did `N(N+1)/2` symbol comparisons.
  /// [`Positions`] indexes that list once per implementing type and the loop reads it, at 1.83
  /// in the exponent over 1 k–64 k before and 1.03 after. Its header carries why the shape
  /// [`Duplicates`] rejected is the right one *here*, and why the first occurrence still wins.
  fn validate_interface_implementations(&mut self) {
    let (model, errors) = self.split();
    let symbols = model.interner.len() as usize;
    // One table for the whole pass, lazily grown by the first implementing type wide enough to
    // index and handed back to the next one by [`Positions::drop`]. A schema whose types
    // implement nothing, or implement narrowly, never allocates it.
    let mut of_sym: Vec<u32> = Vec::new();
    for index in 0..model.types.len() {
      if !matches!(
        model.types[index].kind,
        TypeKind::Object | TypeKind::Interface
      ) {
        continue;
      }
      // Nothing below has anything to say about a type that implements nothing, and every
      // introspection type is one — so the owner name is rendered after the question is asked
      // rather than before it.
      if model.types[index].implements.is_empty() {
        continue;
      }
      let owner = model.text(model.types[index].name.sym).to_owned();
      let declared: &[Located] = &model.types[index].implements;
      // Built once per implementing type and read by every interface it declares, which is the
      // whole of the repair: the loop below is the one that was here, in the order it was in.
      let positions = Positions::over(&model.types[index].fields, symbols, &mut of_sym);

      for entry in declared {
        let Some(interface) = model.type_index(entry.sym) else {
          continue;
        };
        if model.types[interface].kind != TypeKind::Interface {
          continue;
        }

        // Transitivity: every interface the interface implements must also be declared here.
        let required: &[u32] = &model.types[interface].closure;
        for &needed in required {
          if needed as usize == index {
            continue;
          }
          let is_declared = declared
            .iter()
            .any(|d| model.type_index(d.sym) == Some(needed as usize));
          if !is_declared {
            let subject = model.text(model.types[needed as usize].name.sym).to_owned();
            push_owned(
              errors,
              SchemaErrorKind::MissingTransitiveInterface,
              &subject,
              owner.clone(),
              *entry,
            );
          }
        }

        // Field coverage, covariance, and argument invariance.
        let interface_fields: &[RawField] = &model.types[interface].fields;
        for interface_field in interface_fields {
          let field_name = model.text(interface_field.name.sym).to_owned();
          let Some(position) = positions.of(interface_field.name.sym) else {
            push_owned(
              errors,
              SchemaErrorKind::MissingInterfaceField,
              &field_name,
              owner.clone(),
              *entry,
            );
            continue;
          };
          let own: &RawField = &model.types[index].fields[position];

          if !model.is_valid_implementation_type(own.ty.packed, interface_field.ty.packed) {
            let path = owner_path(&[&owner, &field_name]);
            let expected = model.render_type(interface_field.ty.packed);
            push_owned(
              errors,
              SchemaErrorKind::InvalidInterfaceFieldType,
              &expected,
              path,
              own.name,
            );
          }

          // `IsValidImplementation` 2.6: "if `field` is deprecated then `implementedField` must
          // also be deprecated". The span is the implementing field, which is where the edit goes;
          // `related` points at the interface field, which is the other half of the obligation.
          if is_deprecated(model.interner, &own.directives)
            && !is_deprecated(model.interner, &interface_field.directives)
          {
            push_related(
              errors,
              SchemaErrorKind::InterfaceFieldNotDeprecated,
              &field_name,
              Some(owner.clone()),
              own.name,
              interface_field.name.span,
            );
          }

          // Draft `IsValidImplementation` 2.4 and 2.5 read both argument lists, and each loop
          // scans the other list once per entry — `Θ(own × interface)`, which is the third
          // consumer review found walking a list a ceiling had already refused. Both sides are
          // asked, and it has to be both: a refused list is not an empty one, so pairing a
          // refused side against a live one would report every argument of the live side as
          // missing from a list nobody may look at. That is exactly the diagnostic truncating an
          // over-limit list would have invented, reached from the other direction.
          let (Args::Bounded(interface_args), Args::Bounded(own_args)) =
            (interface_field.args.read(), own.args.read())
          else {
            continue;
          };

          for interface_arg in interface_args {
            let arg_name = model.text(interface_arg.name.sym).to_owned();
            match own_args
              .iter()
              .find(|a| a.name.sym == interface_arg.name.sym)
            {
              None => {
                let path = owner_path(&[&owner, &field_name]);
                push_owned(
                  errors,
                  SchemaErrorKind::MissingInterfaceFieldArgument,
                  &arg_name,
                  path,
                  own.name,
                );
              }
              Some(own_arg) => {
                if own_arg.ty.packed != interface_arg.ty.packed {
                  let path = owner_path(&[&owner, &field_name, &arg_name]);
                  let expected = model.render_type(interface_arg.ty.packed);
                  push_owned(
                    errors,
                    SchemaErrorKind::InvalidInterfaceFieldArgumentType,
                    &expected,
                    path,
                    own_arg.name,
                  );
                }
              }
            }
          }

          for own_arg in own_args {
            let declared_by_interface = interface_args
              .iter()
              .any(|a| a.name.sym == own_arg.name.sym);
            if declared_by_interface {
              continue;
            }
            if own_arg.ty.packed.is_non_null() && !own_arg.default.is_present() {
              let arg_name = model.text(own_arg.name.sym).to_owned();
              let path = owner_path(&[&owner, &field_name]);
              push_owned(
                errors,
                SchemaErrorKind::UnexpectedRequiredArgument,
                &arg_name,
                path,
                own_arg.name,
              );
            }
          }
        }
      }
    }
  }

  /// Draft §3.10.1: an input-object cycle must have at least one nullable or list link.
  fn validate_input_object_cycles(&mut self) {
    let count = self.types.len();
    // 0 = unvisited, 1 = on the current path, 2 = settled.
    let mut colour = vec![0u8; count];
    let mut reported = vec![false; count];

    for start in 0..count {
      if self.types[start].kind != TypeKind::InputObject || colour[start] != 0 {
        continue;
      }
      // Explicit stack: (type, next field to examine).
      let mut stack: Vec<(usize, usize)> = vec![(start, 0)];
      colour[start] = 1;
      while let Some((current, cursor)) = stack.pop() {
        if cursor >= self.types[current].input_fields.len() {
          colour[current] = 2;
          continue;
        }
        stack.push((current, cursor + 1));
        let field = &self.types[current].input_fields[cursor];
        let packed = field.ty.packed;
        // The only link that cannot be broken by a `null` or an empty list is a bare `X!`.
        if packed.depth() != 1 || !packed.is_non_null() {
          continue;
        }
        let target = packed.base_id();
        if target == UNRESOLVED {
          continue;
        }
        let target = target.get() as usize;
        if self.types[target].kind != TypeKind::InputObject {
          continue;
        }
        if colour[target] == 1 {
          if !reported[current] {
            reported[current] = true;
            let owner = self.text(self.types[current].name.sym).to_owned();
            let name = self.text(field.name.sym).to_owned();
            let at = field.name;
            self.push_owned(SchemaErrorKind::CircularNonNullInputField, &name, owner, at);
          }
          continue;
        }
        if colour[target] == 0 {
          colour[target] = 1;
          stack.push((target, 0));
        }
      }
    }
  }

  /// Draft §3.10.1(4): `InputObjectDefaultValueHasCycle(inputObject)` must be false.
  ///
  /// # What the rule is actually about
  ///
  /// Not the type graph — [`SchemaBuilder::validate_input_object_cycles`] owns that, and
  /// `input A { b: B = {} } input B { a: A = {} }` passes it, because every link is nullable. It is
  /// the *defaults*: coercing `{}` for an `A` has to supply `A.b`, whose default is `{}`, whose
  /// coercion has to supply `B.a`, whose default is `{}`… A service that tried to materialise the
  /// value would not stop.
  ///
  /// # The draft's two mutually recursive functions, as one iterative walk
  ///
  /// `InputObjectDefaultValueHasCycle(object, value, visited)` asks each field of `object` about
  /// `value`; `InputFieldDefaultValueHasCycle(field, value, visited)` descends either into the
  /// entry `value` supplies for that field — leaving `visited` alone, because the caller wrote
  /// that literal out — or, when it supplies none, into the field's *own* default, adding the
  /// field to `visited` first and returning true if it was already there.
  ///
  /// Only the second kind of descent grows `visited`, so the walk's depth is bounded by the number
  /// of defaulted fields in the document — which is bounded by nothing but the document. That is
  /// the same reason [`SchemaBuilder::validate_directive_cycles`] and the type-graph walk are
  /// iterative, and `a_deep_input_chain_does_not_recurse` is the standing guard: a recursive
  /// implementation would put an SDL's input-object chain on the call stack. Recursion over a
  /// *literal* is still fine — [`SchemaBuilder::const_value`] argues why — so unwrapping the list
  /// nesting in `value` is the one recursive step, in [`map_nodes`].
  ///
  /// # One diagnostic per cycle
  ///
  /// Every input object on the path when a cycle is found is marked, and a marked object is not
  /// walked again from the top. Without that, `A → B → A` reports twice, once from each end. The
  /// cost is the same one [`SchemaBuilder::validate_input_object_cycles`] pays for its colouring:
  /// a second, independent cycle through an already-implicated object waits for the next build.
  ///
  /// # Settling, and what "explored" has to mean for it to be sound
  ///
  /// A frame retires only when its whole sub-exploration finished, and nothing prunes that
  /// exploration except the cycle test itself — which does not prune, it `break`s the entire walk.
  /// So a retired frame's object has had every path below it followed to the end with no repeat,
  /// and starting a fresh walk there would re-derive that at the cost of the whole subtree over
  /// again. Without the skip a chain of `N` defaulted input objects costs `O(N²)`:
  /// `a_deep_defaulted_input_chain_does_not_recurse` is twenty thousand links long and takes
  /// **41.5 s** with it removed against **under one second** with it — measured, not estimated. It
  /// is the standing guard on this and on the iterative shape both.
  ///
  /// **What that argument leaves out is *which* paths "every path below it" is.** A frame reached
  /// through a caller's *supplied* literal descends into that literal, and the draft is explicit
  /// that the field's own default is then never consulted — so such a frame explores a different
  /// question from `InputObjectDefaultValueHasCycle(object, {})`, and finishing it establishes
  /// nothing about the empty-map one. Settling from it made **declaration order decide the
  /// verdict**: with `input Outer { b: Bad = { loop: null } }` read before `input Bad { loop: Bad =
  /// {} }`, `Outer`'s walk marked `Bad` settled through the supplied `{ loop: null }`, the
  /// canonical start at `Bad` was then skipped, and `SchemaBuilder::finish` **accepted a schema
  /// carrying the default cycle this rule exists to refuse**. The same two definitions in the other
  /// order were refused. `input_object_default_cycle_verdict_is_declaration_order_independent` is
  /// the guard.
  ///
  /// So a frame settles its object only when its work list **covers** the canonical exploration:
  /// every declared field either asked with nothing supplied at least once — which is what makes
  /// the walk descend into that field's *own* default — or inert, meaning asking would have
  /// returned immediately because the field names no input object or carries no default. The
  /// top-level start frame is canonical by construction, and so is a descent into `{}`, which is
  /// what the chain guard above is made of; a supplied entry for a field whose default matters is
  /// what stops being enough. `settled` is read at two places and both take this flag: the start
  /// loop, and the descent, which is why `N` input objects defaulting to `{}` of one wide type cost
  /// `O(N)` rather than a fresh `Θ(width)` work list each.
  ///
  /// **How that rule is decided is not the rule.** Written out as "every declared field", it read
  /// the whole declaration once to clear `asked` and once again to answer — *whatever the descent
  /// was about to do*, the descent that pushes no work at all included. Its complement is the
  /// subset that decides it: `coverable` holds, per object, the fields that are not inert, so
  /// "every declared field was asked or is inert" is "every coverable field was asked", and an
  /// object with none of them is canonical for nothing having been asked. That is what makes the
  /// no-map descent `O(1)` — a value unwrapping to no map node runs the draft's "for each field in
  /// `inputObject`" zero times, so it asks nothing, and it covers `target` exactly when `target`
  /// has nothing to cover. `asked` is stamped with the descent that wrote it rather than cleared
  /// for it, so the other width-proportional pass goes too.
  ///
  /// The population that stood in for the rule before this was **`N` input types with a valid
  /// `w: [Wide] = []` in front of a `D`-field `Wide` holding one defaulted input-object field**:
  /// each empty list produces no map, so it can settle nothing, and it repeated both `D`-wide
  /// passes. `Θ(N + D)` of accepted SDL, `Θ(N × D)` of build — 1.90 in the exponent over
  /// 16 k–128 k and **34.9 s** at 128 k, against 1.06 and **160 ms**. al8n/smear#198.
  ///
  /// # Over the split, and the factor here is not a count of fields
  ///
  /// Every literal this walk descends into belongs to a field it is *not* reporting on, and the
  /// shape that stood here bought its way past `&mut self` with `clone` three times over: the
  /// field's own default, the entry a map node supplies for a declared field, and the work entry
  /// again as it is consumed. Each is a deep copy of a [`RawValue`] tree, which the document
  /// alone bounds — the sentence [`SchemaBuilder::validate_interface_implementations`] was
  /// repaired under, and this is the rest of the set it covers.
  ///
  /// What multiplies it here is not a field count. A level's work list holds one entry per (map
  /// node, field) pair, so a literal unwrapping to `M` map nodes asks each of the target's fields
  /// `M` times and the copies are made per ask, not per field. `input Outer { o: [Mid] = [{} {}
  /// …] }` in front of a single `Mid.m` whose own default is one large literal is therefore
  /// `Θ(M × literal)`: one field, one default, on a document `Schema::build` **accepts**. Fitted
  /// at 1.99 in the exponent over 1k–64k and **29.6 s** at the top of that ladder, against 1.01
  /// and **5.6 ms** with the work list holding `&RawValue`. No ceiling stands in front of either
  /// factor, so no ceiling could have closed it.
  ///
  /// The walk therefore reads through [`Model`] and reports through the error list, borrowed
  /// apart. The frames, their order, the spans and the settling are the ones that were here;
  /// only the ownership moved. al8n/smear#198.
  fn validate_input_object_default_cycles(&mut self) {
    /// One level of `InputObjectDefaultValueHasCycle`: an input object, and the work its
    /// `defaultValue` produced.
    struct Frame<'a> {
      /// The field of the enclosing object whose *own* default this frame descended into, if that
      /// is why it exists. Popped off the path when the frame retires.
      pushed: Option<usize>,
      /// `(field index in `object`, the value the caller supplied for it)`, one entry per
      /// (map node, field) pair — which is exactly the draft's "for each field in inputObject",
      /// run once per map node the level's value unwraps to.
      ///
      /// Borrowed out of the model rather than owned; the header says why.
      work: Vec<(usize, Option<&'a RawValue>)>,
      cursor: usize,
      /// The object whose fields `work` indexes, so a frame can name the field it blames.
      object: usize,
      /// Whether `work` covers `InputObjectDefaultValueHasCycle(object, {})` — the question the
      /// start loop and the descent both skip a settled object on the strength of. See the header.
      canonical: bool,
    }

    let (model, errors) = self.split();
    let count = model.types.len();
    // A dense id per input field, so path membership is a bit rather than a search.
    let mut field_base: Vec<usize> = vec![0; count + 1];
    for index in 0..count {
      field_base[index + 1] = field_base[index] + model.types[index].input_fields.len();
    }
    let total_fields = field_base[count];

    // The fields a frame has to have ASKED for its work to cover `(object, {})`, per object, as
    // one flat list with `coverable_base` addressing it — the shape `field_base` above already
    // uses. A field naming no input object, or carrying no default, returns immediately whichever
    // way it is asked, so it is inert and covering it is nothing to cover; every other declared
    // field is here, and `canonical` is exactly "every one of mine was asked".
    //
    // Derived once, in `Θ(input fields)`, so that a descent's two questions are answered over
    // this subset rather than over the whole declaration. What that replaces is the reason:
    // every descent cleared and resized `asked` to the target's full declared width and then
    // scanned every declaration, EVEN WHEN `map_nodes` produced no map at all and the frame it
    // pushed had no work in it. `N` input types with a valid `w: [Wide] = []` in front of a
    // `D`-field `Wide` is `Θ(N + D)` of SDL `Schema::build` ACCEPTS: an empty list unwraps to no
    // map, so it can settle nothing and repeats both `D`-wide passes `N` times. 1.90 in the
    // exponent over 16 k–128 k and 34.9 s at 128 k. al8n/smear#198.
    let mut coverable_base: Vec<usize> = vec![0; count + 1];
    let mut coverable: Vec<u32> = Vec::new();
    for index in 0..count {
      for (at, field) in model.types[index].input_fields.iter().enumerate() {
        let base = field.ty.packed.base_id();
        if base != UNRESOLVED
          && model.types[base.get() as usize].kind == TypeKind::InputObject
          && field.default_value.is_some()
        {
          coverable.push(at as u32);
        }
      }
      coverable_base[index + 1] = coverable.len();
    }

    let mut implicated = vec![false; count];
    // Objects a completed *canonical* exploration proved clean; the header says why only that one
    // counts, and why what it proves is transitive.
    let mut settled = vec![false; count];
    let mut on_path = vec![false; total_fields];
    // Which input field this descent asked with nothing supplied, addressed by the same dense id
    // `on_path` uses, and STAMPED with the descent that asked rather than cleared for it: the
    // answer is read over the coverable subset, which may be empty while the declaration is wide,
    // so a clear proportional to the declaration is one of the two passes this removes. `u64`
    // because a stamp that wraps is a stamp that answers for a descent that did not ask, and no
    // walk can make 2^64 of them; zero is therefore never a live stamp, which is what lets the
    // whole array start there.
    let mut asked: Vec<u64> = vec![0; total_fields];
    let mut descent: u64 = 0;

    for start in 0..count {
      if model.types[start].kind != TypeKind::InputObject || implicated[start] || settled[start] {
        continue;
      }
      // The draft's top-level call: `defaultValue` is an empty map, so every field is asked, and
      // none of them is supplied a value.
      let mut stack = vec![Frame {
        pushed: None,
        work: (0..model.types[start].input_fields.len())
          .map(|field| (field, None))
          .collect(),
        cursor: 0,
        object: start,
        canonical: true,
      }];

      let mut found: Option<(usize, usize)> = None;
      while let Some(frame) = stack.last_mut() {
        let Some(&(field_index, supplied)) = frame.work.get(frame.cursor) else {
          if let Some(id) = frame.pushed {
            on_path[id] = false;
          }
          if frame.canonical {
            settled[frame.object] = true;
          }
          stack.pop();
          continue;
        };
        frame.cursor += 1;
        let object = frame.object;

        // `InputFieldDefaultValueHasCycle`. A field whose named type is not an input object can
        // hold no cycle, whatever its default says.
        let field = &model.types[object].input_fields[field_index];
        let base = field.ty.packed.base_id();
        if base == UNRESOLVED {
          continue;
        }
        let target = base.get() as usize;
        if model.types[target].kind != TypeKind::InputObject {
          continue;
        }

        let descend_into = match supplied {
          // The caller's literal named this field, so the field's own default is never consulted
          // and `visited` does not grow.
          Some(value) => value,
          // The field's own default, which is the descent `visited` grows for.
          None => match field.default_value.as_ref() {
            Some(default) => default,
            None => continue,
          },
        };

        let declared = &model.types[target].input_fields;
        let mut maps: Vec<&[RawObjectField]> = Vec::new();
        map_nodes(descend_into, &mut maps);

        // A descent into `{}` *is* `InputObjectDefaultValueHasCycle(target, {})`, so a target some
        // canonical frame already retired has nothing left to say and the work list is not built
        // at all. Ahead of the cycle test on purpose, and the two cannot both apply: reaching this
        // field while it is already on the path means the exploration below it comes back here, and
        // an exploration that comes back to a field it pushed is one that `break`s rather than
        // retires — so no canonical frame for `target` could have settled it.
        //
        // Without the skip, `N` input objects each defaulting to `{}` of one `N`-field input object
        // build a fresh `Θ(N)` work list apiece: `O(N)` of source, `Θ(N²)` of walk, on a schema
        // `Schema::build` **accepts**. 1.97 in the exponent over the top step of 1 k–16 k and
        // **1.470 s** at 16 k, against 1.08 and 23 ms — and 1.10 carried out to 256 k, where a
        // millisecond floor cannot be what is being read. al8n/smear#198.
        if settled[target] && matches!(maps.as_slice(), [entries] if entries.is_empty()) {
          continue;
        }

        let pushed = match supplied {
          Some(_) => None,
          None => {
            let id = field_base[object] + field_index;
            if on_path[id] {
              found = Some((object, field_index));
              break;
            }
            on_path[id] = true;
            Some(id)
          }
        };

        // The header's condition, over the subset that can fail it. A field asked with nothing
        // supplied is the one whose *own* default this frame descends into, which is what the
        // empty-map call does to every field; an inert field is covered by being inert. So this
        // frame's work is the canonical exploration of `target` exactly when every coverable
        // field of `target` was asked — and only then may retiring it settle `target` for the
        // starts and descents that follow. That is the rule 1260027 wrote, unchanged; what
        // changed is that it is decided over `coverable` instead of by rescanning `declared`.
        let coverable_here = &coverable[coverable_base[target]..coverable_base[target + 1]];
        let mut work = Vec::new();
        let canonical = if maps.is_empty() {
          // The draft's "for each field in inputObject" runs once per map node, so no map is no
          // work — and no work asks nothing, which covers `target` only when there is nothing of
          // `target` to cover. Decided here in `O(1)` rather than by two passes over a
          // declaration this frame is not going to read.
          coverable_here.is_empty()
        } else {
          descent += 1;
          for map in &maps {
            // The scan [`Names`] was written for, asked the other way round: one lookup per
            // declared field into one map node, so a wide input object in front of a literal that
            // writes its fields was `Θ(declared × written)` here as well as at
            // [`SchemaBuilder::check_const_value`].
            let of_entry = Names::over(map.len(), declared.len(), |at| map[at].name.sym);
            for (index, declared_field) in declared.iter().enumerate() {
              let supplied = of_entry
                .first(map.len(), declared_field.name.sym, |at| map[at].name.sym)
                .map(|at| &map[at].value);
              if supplied.is_none() {
                asked[field_base[target] + index] = descent;
              }
              work.push((index, supplied));
            }
          }
          coverable_here
            .iter()
            .all(|&at| asked[field_base[target] + at as usize] == descent)
        };
        stack.push(Frame {
          pushed,
          work,
          cursor: 0,
          object: target,
          canonical,
        });
      }

      // The path is per-start, so a walk that stopped early has to put the bits back: `break`
      // skips the retire step that clears them, and a stale `true` would make the *next* start
      // report a cycle it never walked into.
      for frame in &stack {
        if let Some(id) = frame.pushed {
          on_path[id] = false;
        }
      }

      let Some((object, field_index)) = found else {
        debug_assert!(stack.is_empty(), "a completed walk retires every frame");
        continue;
      };
      for frame in &stack {
        implicated[frame.object] = true;
      }
      implicated[object] = true;
      let owner = model.text(model.types[object].name.sym).to_owned();
      let at = model.types[object].input_fields[field_index].name;
      let name = model.text(at.sym).to_owned();
      push_owned(
        errors,
        SchemaErrorKind::InputObjectDefaultValueCycle,
        &name,
        owner,
        at,
      );
    }
  }

  /// Draft §3.13.1: a directive definition must not refer to itself, directly or indirectly.
  ///
  /// The rule is exactly "is this directive reachable from itself", over one graph with two kinds
  /// of node — a directive, and a type an argument accepts — because the path between two
  /// directives runs through the types their arguments name.
  ///
  /// # What a walk per directive cost, on a document `Schema::build` accepts
  ///
  /// The shape that stood here ran one reachability walk from every directive, and allocated the
  /// two schema-wide visitation vectors **inside** that loop. Two independent `Θ(N²)` terms came
  /// out of that, and both were measured on documents `Schema::build` **accepts**:
  ///
  /// - The **initialisation**, which `N` distinct zero-argument directives with no dependency
  ///   edge at all — nothing to walk, nothing to find — paid in full. It is the quieter of the
  ///   two, because ingesting `N` definitions is itself linear and hides it: 1.39 in the exponent
  ///   over 1 k–256 k, and only over the ladder's top step, 1.62, does the square show. 793 ms at
  ///   256 k, against 106 ms and 1.04 now.
  /// - The **traversal**, which a long *acyclic* chain `@d0 ← @d1 ← … ← @dN` paid by being walked
  ///   again from every starting directive — a term stamping the visitation would have left
  ///   exactly where it was. 1.97 in the exponent over 1 k–64 k, 2.04 over the top step, and
  ///   26.1 s at 64 k, against 56 ms and 1.07 now.
  ///
  /// al8n/smear#198.
  ///
  /// # One graph, one pass
  ///
  /// [`SchemaBuilder::cyclic_directives`] builds that graph once and runs Tarjan over it, so
  /// every directive's answer comes out of a single `O(nodes + edges)` traversal. "Reachable from
  /// itself" is "in a strongly connected component with a second member, or carrying an edge to
  /// itself", and the second disjunct is not a special case to be tidied away: a directive naming
  /// itself directly is a component of one, which is otherwise indistinguishable from an acyclic
  /// node.
  ///
  /// # What does not change
  ///
  /// The cycle is still reported only on the directive that is on it. A directive `@b` that
  /// merely uses a self-referential `@a` is not itself self-referential — `@a` is not reachable
  /// *back* to `@b`, so they are not in one component — and blaming it would put the diagnostic
  /// on the wrong definition, the same refinement apollo-compiler makes.
  ///
  /// Diagnostics are emitted by ascending directive index, which is the order the definitions
  /// were read in, which is the order the walk-per-directive loop produced them in.
  fn validate_directive_cycles(&mut self) {
    let directives = self.directives.len();
    if directives == 0 {
      return;
    }
    let cyclic = self.cyclic_directives();
    for (index, &on_cycle) in cyclic.iter().enumerate() {
      if !on_cycle {
        continue;
      }
      let at = self.directives[index].name;
      let name = self.text(at.sym).to_owned();
      self.push(SchemaErrorKind::SelfReferentialDirective, &name, at);
    }
  }

  /// Which directives are reachable from themselves, one bit per directive index.
  ///
  /// # The graph
  ///
  /// Node `d` below [`SchemaBuilder::directives`]`.len()` is that directive; node
  /// `directives + t` is type `t`. The edges are the steps the walk this replaces took, and they
  /// are the same steps: a directive reaches the directives written on its arguments and the base
  /// types its arguments accept; a type reaches the directives written on it, on its input fields
  /// and on its enum values, and the base types its input fields accept.
  ///
  /// A directive whose argument list a ceiling refused reaches **nothing**, exactly as it seeded
  /// nothing before. A cycle through a directive that will not build is a second diagnostic about
  /// a schema the first one has already refused.
  ///
  /// # Compressed rows, and why the edges are counted before they are written
  ///
  /// [`SchemaBuilder::dependencies`] is asked twice — once to size each row, once to fill it —
  /// rather than pushing into a vector per node, because one row per node is `nodes` allocations
  /// and this is two. The scratch it writes through is reused across both passes and every node.
  fn cyclic_directives(&self) -> Vec<bool> {
    let directives = self.directives.len();
    let nodes = directives + self.types.len();

    let mut reaches: Vec<u32> = Vec::new();
    let mut offset: Vec<usize> = vec![0; nodes + 1];
    for node in 0..nodes {
      self.dependencies(node, &mut reaches);
      offset[node + 1] = reaches.len();
    }
    for node in 0..nodes {
      offset[node + 1] += offset[node];
    }
    let mut edges: Vec<u32> = vec![0; offset[nodes]];
    for node in 0..nodes {
      self.dependencies(node, &mut reaches);
      edges[offset[node]..offset[node + 1]].copy_from_slice(&reaches);
    }

    // Tarjan, iterative for the reason the walk it replaces was iterative: the type graph's depth
    // is bounded by nothing but the document, and a recursive walk would put an SDL's
    // input-object chain on the call stack.
    //
    // Discovery indices start at one so that zero means "not yet visited". A sentinel at the top
    // of the range would be a second thing the counter must never reach; this way the counter has
    // only its own width to respect.
    const UNVISITED: u32 = 0;
    let mut discovered: Vec<u32> = vec![UNVISITED; nodes];
    let mut low: Vec<u32> = vec![0; nodes];
    let mut stacked: Vec<bool> = vec![false; nodes];
    let mut component: Vec<u32> = Vec::new();
    /// A node and the edge of its row to read next.
    type Descent = (u32, usize);
    let mut descent: Vec<Descent> = Vec::new();
    let mut next_discovery: u32 = 1;
    let mut cyclic = vec![false; directives];

    for root in 0..nodes {
      if discovered[root] != UNVISITED {
        continue;
      }
      discovered[root] = next_discovery;
      low[root] = next_discovery;
      next_discovery += 1;
      component.push(root as u32);
      stacked[root] = true;
      descent.push((root as u32, offset[root]));

      while let Some(&(node, cursor)) = descent.last() {
        let node = node as usize;
        if cursor < offset[node + 1] {
          descent
            .last_mut()
            .expect("the frame just read is still on the descent")
            .1 = cursor + 1;
          let next = edges[cursor] as usize;
          if next == node && node < directives {
            // A component of one, which nothing below can tell from an acyclic node.
            cyclic[node] = true;
          }
          if discovered[next] == UNVISITED {
            discovered[next] = next_discovery;
            low[next] = next_discovery;
            next_discovery += 1;
            component.push(next as u32);
            stacked[next] = true;
            descent.push((next as u32, offset[next]));
          } else if stacked[next] {
            low[node] = low[node].min(discovered[next]);
          }
          continue;
        }

        descent.pop();
        if let Some(&(parent, _)) = descent.last() {
          low[parent as usize] = low[parent as usize].min(low[node]);
        }
        if low[node] != discovered[node] {
          continue;
        }
        // The component is the tail of the stack above this node, which is its root.
        let at = component
          .iter()
          .rposition(|&member| member as usize == node)
          .expect("a component's root is on the component stack");
        // Two members mean every member reaches every other, so every directive among them
        // reaches itself.
        let closed = component.len() - at > 1;
        for &member in &component[at..] {
          stacked[member as usize] = false;
          if closed && (member as usize) < directives {
            cyclic[member as usize] = true;
          }
        }
        component.truncate(at);
      }
    }

    cyclic
  }

  /// The nodes one node of [`SchemaBuilder::cyclic_directives`]'s graph reaches in one step,
  /// written into `reaches`, which is cleared first.
  fn dependencies(&self, node: usize, reaches: &mut Vec<u32>) {
    reaches.clear();
    let directives = self.directives.len();
    let first_type_node = directives as u32;

    if node < directives {
      // Unless the ceiling holds this list, in which case the definition reaches nothing.
      let Args::Bounded(args) = self.directives[node].args.read() else {
        return;
      };
      for arg in args {
        self.push_directive_uses(&arg.directives, reaches);
        push_type(arg.ty.packed.base_id(), first_type_node, reaches);
      }
      return;
    }

    let raw = &self.types[node - directives];
    self.push_directive_uses(&raw.directives, reaches);
    for field in &raw.input_fields {
      self.push_directive_uses(&field.directives, reaches);
      push_type(field.ty.packed.base_id(), first_type_node, reaches);
    }
    for value in &raw.enum_values {
      self.push_directive_uses(&value.directives, reaches);
    }
  }

  fn push_directive_uses(&self, uses: &[RawDirectiveUse], reaches: &mut Vec<u32>) {
    for used in uses {
      if let Some(index) = self.directive_index(used.name.sym) {
        reaches.push(index as u32);
      }
    }
  }

  // -- flattening -------------------------------------------------------------------------------

  fn flatten(self) -> Result<Schema, SchemaErrors> {
    let Self {
      interner,
      types: raw_types,
      type_of_sym: raw_type_of_sym,
      directives: raw_directives,
      roots: raw_roots,
      ..
    } = self;

    let symbol_count = interner.len();
    // `raw_type_of_sym` is the builder's own symbol-to-index map; reusing it keeps flattening
    // linear instead of rescanning the type table for every member and root.
    let id_of = |sym: Sym| -> Option<TypeId> {
      match raw_type_of_sym.get(sym.get() as usize) {
        Some(&index) if index != u32::MAX => Some(TypeId::new(index)),
        _ => None,
      }
    };
    if symbol_count > MAX_SYMBOLS {
      return Err(SchemaErrors::new(vec![SchemaError::new(
        SchemaErrorKind::TooManyNames,
        "schema",
        SimpleSpan::default(),
      )]));
    }

    // Object ordinals, in type-id order.
    let mut objects: Vec<TypeId> = Vec::new();
    let mut ordinal_of: Vec<u32> = vec![TypeDef::NONE; raw_types.len()];
    for (index, raw) in raw_types.iter().enumerate() {
      if raw.kind == TypeKind::Object {
        ordinal_of[index] = objects.len() as u32;
        objects.push(TypeId::new(index as u32));
      }
    }
    let possible_words = objects.len().div_ceil(64).max(1) as u32;

    let string_id = interner
      .lookup(b"String")
      .and_then(id_of)
      .unwrap_or(UNRESOLVED);
    let schema_type_id = interner
      .lookup(b"__Schema")
      .and_then(id_of)
      .unwrap_or(UNRESOLVED);
    let type_type_id = interner
      .lookup(b"__Type")
      .and_then(id_of)
      .unwrap_or(UNRESOLVED);
    let typename_sym = interner.lookup(builtin::TYPENAME_FIELD.as_bytes());
    let schema_field_sym = interner.lookup(builtin::SCHEMA_FIELD.as_bytes());
    let type_field_sym = interner.lookup(builtin::TYPE_FIELD.as_bytes());
    let type_arg_sym = interner.lookup(builtin::TYPE_FIELD_ARGUMENT.as_bytes());

    let query_root = raw_roots[RootOperation::Query.index()]
      .and_then(|root| id_of(root.sym))
      .unwrap_or(UNRESOLVED);

    let mut types: Vec<TypeDef> = Vec::with_capacity(raw_types.len());
    let mut fields: Vec<FieldDef> = Vec::new();
    let mut inputs: Vec<InputValueDef> = Vec::new();
    let mut interfaces: Vec<TypeId> = Vec::new();
    let mut members: Vec<TypeId> = Vec::new();
    let mut enum_values: Vec<Sym> = Vec::new();
    let mut possible: Vec<u64> = Vec::new();

    for (index, raw) in raw_types.iter().enumerate() {
      let id = TypeId::new(index as u32);

      let interfaces_range = {
        let start = interfaces.len() as u32;
        interfaces.extend(raw.closure.iter().map(|i| TypeId::new(*i)));
        Range32::new(start, interfaces.len() as u32)
      };

      let members_range = {
        let start = members.len() as u32;
        for member in &raw.members {
          if let Some(target) = id_of(member.sym) {
            members.push(target);
          }
        }
        Range32::new(start, members.len() as u32)
      };

      let enum_range = {
        let start = enum_values.len() as u32;
        let mut values: Vec<Sym> = raw.enum_values.iter().map(|value| value.name.sym).collect();
        values.sort_unstable();
        enum_values.extend(values);
        Range32::new(start, enum_values.len() as u32)
      };

      let fields_range = match raw.kind {
        TypeKind::Object | TypeKind::Interface | TypeKind::Union => {
          let mut rows: Vec<FieldDef> = Vec::with_capacity(raw.fields.len() + 3);
          for field in &raw.fields {
            // Unreachable, and reported rather than assumed. `finish` returns `Err` for any
            // recorded error and an over-limit list records one, so flattening never meets a
            // refused list. What the arm must not do is degrade quietly: an empty argument range
            // is a schema that silently drops arguments the document wrote, which is the one
            // outcome worse than the refusal this returns.
            let Args::Bounded(declared) = field.args.read() else {
              return Err(SchemaErrors::new(vec![
                SchemaError::new(
                  SchemaErrorKind::TooManyFieldArguments,
                  interner.text(field.name.sym),
                  field.name.span,
                )
                .in_document(field.name.document),
              ]));
            };
            let args_start = inputs.len() as u32;
            let mut args: Vec<InputValueDef> = declared
              .iter()
              .map(|arg| InputValueDef::new(arg.name.sym, arg.ty.packed, arg.default))
              .collect();
            args.sort_unstable_by_key(|arg| arg.name().get());
            inputs.extend(args);
            let args_range = Range32::new(args_start, inputs.len() as u32);
            rows.push(FieldDef::new(field.name.sym, field.ty.packed, args_range));
          }

          // The meta-fields, on the types draft §4.4 puts them on.
          if let (Some(typename), true) = (typename_sym, string_id != UNRESOLVED)
            && !rows.iter().any(|row| row.name() == typename)
          {
            let ty = PackedType::named(raw_types[string_id.get() as usize].name.sym, string_id);
            let ty = ty.push_non_null().unwrap_or(ty);
            rows.push(FieldDef::new(typename, ty, Range32::EMPTY));
          }
          if id == query_root {
            if let (Some(name), true) = (schema_field_sym, schema_type_id != UNRESOLVED)
              && !rows.iter().any(|row| row.name() == name)
            {
              let ty = PackedType::named(
                raw_types[schema_type_id.get() as usize].name.sym,
                schema_type_id,
              );
              let ty = ty.push_non_null().unwrap_or(ty);
              rows.push(FieldDef::new(name, ty, Range32::EMPTY));
            }
            if let (Some(name), Some(arg), true) = (
              type_field_sym,
              type_arg_sym,
              type_type_id != UNRESOLVED && string_id != UNRESOLVED,
            ) && !rows.iter().any(|row| row.name() == name)
            {
              let args_start = inputs.len() as u32;
              let arg_ty =
                PackedType::named(raw_types[string_id.get() as usize].name.sym, string_id);
              let arg_ty = arg_ty.push_non_null().unwrap_or(arg_ty);
              inputs.push(InputValueDef::new(arg, arg_ty, DefaultKind::Absent));
              let args_range = Range32::new(args_start, inputs.len() as u32);
              let ty = PackedType::named(
                raw_types[type_type_id.get() as usize].name.sym,
                type_type_id,
              );
              rows.push(FieldDef::new(name, ty, args_range));
            }
          }

          rows.sort_unstable_by_key(|row| row.name().get());
          let start = fields.len() as u32;
          fields.extend(rows);
          Range32::new(start, fields.len() as u32)
        }
        TypeKind::InputObject => {
          let mut rows: Vec<InputValueDef> = raw
            .input_fields
            .iter()
            .map(|field| InputValueDef::new(field.name.sym, field.ty.packed, field.default))
            .collect();
          rows.sort_unstable_by_key(|row| row.name().get());
          let start = inputs.len() as u32;
          inputs.extend(rows);
          Range32::new(start, inputs.len() as u32)
        }
        _ => Range32::EMPTY,
      };

      let possible_start = if raw.kind.is_composite() {
        let start = possible.len() as u32;
        possible.resize(possible.len() + possible_words as usize, 0);
        match raw.kind {
          TypeKind::Object => {
            set_bit(&mut possible, start, ordinal_of[index]);
          }
          TypeKind::Union => {
            for member in &raw.members {
              if let Some(target) = id_of(member.sym) {
                let ordinal = ordinal_of[target.get() as usize];
                set_bit(&mut possible, start, ordinal);
              }
            }
          }
          TypeKind::Interface => {
            for (candidate, other) in raw_types.iter().enumerate() {
              if other.kind == TypeKind::Object && other.closure.contains(&(index as u32)) {
                set_bit(&mut possible, start, ordinal_of[candidate]);
              }
            }
          }
          _ => {}
        }
        start
      } else {
        TypeDef::NONE
      };

      let mut flags = TypeFlags::EMPTY;
      if raw.built_in {
        flags = flags.union(TypeFlags::BUILT_IN);
      }
      if raw
        .directives
        .iter()
        .any(|used| interner.text(used.name.sym) == "oneOf")
      {
        flags = flags.union(TypeFlags::ONE_OF);
      }

      types.push(TypeDef::new(
        raw.name.sym,
        raw.kind,
        flags,
        fields_range,
        interfaces_range,
        members_range,
        enum_range,
        possible_start,
        ordinal_of[index],
      ));
    }

    let mut directives: Vec<DirectiveDef> = Vec::with_capacity(raw_directives.len());
    for raw in &raw_directives {
      // Unreachable for the reason the field arm above states, and refused rather than emptied
      // for the same one.
      let Args::Bounded(declared) = raw.args.read() else {
        return Err(SchemaErrors::new(vec![
          SchemaError::new(
            SchemaErrorKind::TooManyDirectiveArguments,
            interner.text(raw.name.sym),
            raw.name.span,
          )
          .in_document(raw.name.document),
        ]));
      };
      let args_start = inputs.len() as u32;
      let mut args: Vec<InputValueDef> = declared
        .iter()
        .map(|arg| InputValueDef::new(arg.name.sym, arg.ty.packed, arg.default))
        .collect();
      args.sort_unstable_by_key(|arg| arg.name().get());
      inputs.extend(args);
      let args_range = Range32::new(args_start, inputs.len() as u32);
      directives.push(DirectiveDef::new(
        raw.name.sym,
        raw.locations,
        args_range,
        raw.repeatable,
        raw.built_in,
      ));
    }
    directives.sort_unstable_by_key(|directive| directive.name().get());

    let mut type_of_sym = vec![u32::MAX; symbol_count as usize];
    for (index, raw) in raw_types.iter().enumerate() {
      type_of_sym[raw.name.sym.get() as usize] = index as u32;
    }

    let Interner {
      strings,
      spans,
      map: _,
    } = interner;
    let strings = strings.into_boxed_slice();
    let spans = spans.into_boxed_slice();
    let index = {
      let spans_ref = &spans;
      let strings_ref = &strings;
      NameIndex::build(symbol_count, |sym| {
        let (start, end) = spans_ref[sym as usize];
        &strings_ref[start as usize..end as usize]
      })
    };
    let Some(index) = index else {
      return Err(SchemaErrors::new(vec![SchemaError::new(
        SchemaErrorKind::TooManyNames,
        "schema",
        SimpleSpan::default(),
      )]));
    };

    let roots = [
      raw_roots[0].and_then(|root| id_of(root.sym)),
      raw_roots[1].and_then(|root| id_of(root.sym)),
      raw_roots[2].and_then(|root| id_of(root.sym)),
    ];

    Ok(Schema {
      strings,
      spans,
      index,
      types: types.into_boxed_slice(),
      type_of_sym: type_of_sym.into_boxed_slice(),
      fields: fields.into_boxed_slice(),
      inputs: inputs.into_boxed_slice(),
      interfaces: interfaces.into_boxed_slice(),
      members: members.into_boxed_slice(),
      enum_values: enum_values.into_boxed_slice(),
      possible: possible.into_boxed_slice(),
      possible_words,
      objects: objects.into_boxed_slice(),
      directives: directives.into_boxed_slice(),
      roots,
    })
  }
}

fn set_bit(words: &mut [u64], start: u32, ordinal: u32) {
  if ordinal == TypeDef::NONE {
    return;
  }
  let word = start as usize + (ordinal / 64) as usize;
  if let Some(slot) = words.get_mut(word) {
    *slot |= 1u64 << (ordinal % 64);
  }
}

/// Every map node a default value unwraps to, in the order the draft would reach them.
///
/// `InputObjectDefaultValueHasCycle` handles a list by recursing into each item against the *same*
/// input object, and a map by asking each of that object's fields about it; every other shape
/// contributes nothing. Flattening the list nesting up front is what lets
/// [`SchemaBuilder::validate_input_object_default_cycles`] hold one work list per level instead of
/// a second stack for it.
///
/// Recursive — but over a *literal*, whose depth the parser has already bounded, and not over the
/// type graph, whose depth it has not. That is the same line [`SchemaBuilder::const_value`] draws.
fn map_nodes<'a>(value: &'a RawValue, out: &mut Vec<&'a [RawObjectField]>) {
  match &value.shape {
    RawShape::List(items) => {
      for item in items {
        map_nodes(item, out);
      }
    }
    RawShape::Object(fields) => out.push(fields),
    _ => {}
  }
}

fn map_operation(operation: &OperationType) -> RootOperation {
  match operation {
    OperationType::Query(_) => RootOperation::Query,
    OperationType::Mutation(_) => RootOperation::Mutation,
    _ => RootOperation::Subscription,
  }
}

fn map_location(location: &Location) -> Option<DirectiveLocation> {
  DirectiveLocation::from_name(location.as_str())
}

/// The name of a type definition, as a `&str` over the built-in SDL's `&'static str` source.
///
/// Used for both the introspection types and the built-in scalars; the two differ in what the
/// caller does when the name is already taken, not in how it is read.
fn type_definition_name(definition: &TypeDefinition<&'static str>) -> &'static str {
  match definition {
    TypeDefinition::Scalar(def) => def.name().source(),
    TypeDefinition::Object(def) => def.name().source(),
    TypeDefinition::Interface(def) => def.name().source(),
    TypeDefinition::Union(def) => def.name().source(),
    TypeDefinition::Enum(def) => def.name().source(),
    TypeDefinition::InputObject(def) => def.name().source(),
  }
}

impl Schema {
  /// Builds a schema from one type-system document.
  ///
  /// Draft §3 "Type Validation" runs here: a document that is not a valid schema comes back as
  /// [`SchemaErrors`] rather than as a `Schema` nobody checked.
  ///
  /// Use [`SchemaBuilder`] directly when the schema spans several documents.
  #[inline]
  pub fn build<S>(document: &TypeSystemDocument<S>) -> Result<Self, SchemaErrors>
  where
    S: AsRef<[u8]>,
  {
    let mut builder = SchemaBuilder::new();
    builder.document(document);
    builder.finish()
  }
}
