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
    ConstDirectives, ConstInputValue, ConstObjectField, DirectiveDefinition, EnumTypeDefinition,
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
    MAX_DIRECTIVE_ARGUMENTS, MAX_FIELD_ARGUMENTS, MAX_SYMBOLS, MAX_WRAPPERS, NameIndex, PackedType,
    Range32, RootOperation, Schema, Sym, TypeDef, TypeFlags, TypeId, TypeKind, is_name,
    is_reserved,
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

/// The largest arena an [`Interner`] will fill, in bytes.
///
/// # Why `u32::MAX` — the interval, then the pick inside it
///
/// | bound | from | value |
/// |---|---|---|
/// | lower: the arena a **parsed** document needs | one copy per distinct spelling, and a parsed document's leaves hold disjoint token spans | **that document's own source** |
/// | upper: the last offset a span can name | [`Interner::spans`] is a `(u32, u32)` pair of byte offsets into [`Interner::strings`] | **`u32::MAX`** |
///
/// The lower row is why a ceiling at the top of the interval refuses nothing this workspace
/// parses. A `TypeSystemDocument` that `type_system_document` produced over one source has one
/// leaf per token and the tokens are disjoint, so the sum of its spellings is at most that source
/// — and a source this workspace accepts is itself addressed by `u32` spans. The two rows meet,
/// so there is no interval to pick inside.
///
/// **For a tree a caller assembles the row has no bottom, and that is what the ceiling is for.**
/// `Name::new` is public, and `IntValue::graphql` and its two siblings are public associated
/// parsers, so `N` leaves may be `N` overlapping suffixes of one `B`-byte buffer: `B(B+1)/2`
/// interned bytes from `B` of input, which reaches four gigabytes at `B ≈ 92 682` — a 92 KB
/// input. [`RawShape`] carries the measurement. So this is not slack a caller can plan around; it
/// is the width of the offset, and refusing at it is what the offset is worth.
///
/// The upper row is the representation and not a preference. `bytes()` answers
/// `strings[start..end]`, and past `u32::MAX` bytes there is no `start` to record — the cast that
/// used to record one wrapped, which is the defect this constant closes rather than a capacity it
/// buys. See [`SchemaErrorKind::TooManyInternedBytes`] for what a build past it sees, and the
/// [`Interner`] header for the two failures wrapping produced.
const MAX_ARENA_BYTES: u32 = u32::MAX;

/// The largest number of distinct spellings an [`Interner`] will hold.
///
/// The width of the [`Sym`] that addresses them, for the same reason [`MAX_ARENA_BYTES`] is the
/// width of an offset — and it is the *slack* bound of the pair. A spelling costs at least one
/// byte of arena, so `spans.len() <= strings.len() + 1` always holds and [`MAX_ARENA_BYTES`] is
/// reached first from every direction but one: the single empty spelling, which the map admits
/// once and then deduplicates. It is checked anyway, because "the other guard covers it" is a
/// property of today's call sites rather than of this type.
///
/// **Not [`MAX_SYMBOLS`].** That is the bound [`NameIndex::build`] can *index*, it is `1 << 30`,
/// and `flatten` still refuses against it as [`SchemaErrorKind::TooManyNames`]. This one is the
/// bound a `Sym` can *address*, which is the cast's own question.
const MAX_ARENA_SYMBOLS: u32 = u32::MAX;

/// The growable half of the name arena.
///
/// The finished [`Schema`] keeps `strings` and `spans` and a probe-only [`NameIndex`]; this map
/// exists only while building, which is why the schema has no hash map in it at all.
///
/// # The offsets are checked, and here is what they did when they were not
///
/// A span is a pair of `u32` **byte offsets** into `strings`, and the three that record one were
/// `… .len() as u32`. Two of them index `strings`, so they wrap at four gigabytes of interned
/// bytes; the third counts symbols and needs four billion of them, which is the bar a review of
/// this type read all three against. Measured on this type at `fcac941`,
/// `aarch64-apple-darwin`, release, by padding `strings` and interning `"beta"` after `"alpha"`:
///
/// | `strings.len()` | span recorded | `bytes()` |
/// |---|---|---|
/// | `u32::MAX` | `(4294967295, 3)` | **panic**: `slice index starts at 4294967295 but ends at 3` |
/// | `2^32` | `(0, 4)` | **`"alph"`** — the wrong spelling, handed back through `Schema::name` |
///
/// Neither is a capacity limit a caller can plan around: the first ends the process and the second
/// answers a question wrongly, both on a path that returns `Result`. Four gigabytes arrives from a
/// 92 KB input, through either arena: `Name::new` is public and `IntValue::graphql` and its two
/// siblings are public associated parsers, so `N` leaves may be `N` overlapping suffixes of one
/// `B`-byte buffer — `B(B+1)/2` interned bytes from `B`, which crosses `u32::MAX` at
/// `B ≈ 92 682`. [`RawShape`] carries the measurement. The ceilings above turn both rows into
/// [`SchemaErrorKind::TooManyInternedBytes`].
///
/// The quadratic growth itself is not bounded here and is not a duplicate of it: every retained
/// byte is a *distinct* spelling — a name the finished [`Schema`] hands back through
/// `Schema::name`, or a literal the coercion table reads — so the dedup below is already the
/// strongest bound of its kind and there is no copy left to remove. The checked conversion is what
/// makes the growth harmless.
#[derive(Debug, Default)]
struct Interner {
  strings: Vec<u8>,
  spans: Vec<(u32, u32)>,
  map: BTreeMap<Box<[u8]>, u32>,
  /// The symbol a refused intern answers with, minted at the first refusal.
  ///
  /// `Some` is the arena saying it stopped growing, and [`SchemaBuilder::finish`] reads it as the
  /// build's refusal. Holding the placeholder here rather than a bare flag is what makes one
  /// refusal cost one span however many spellings follow it.
  refused: Option<Sym>,
}

impl Interner {
  fn intern(&mut self, bytes: &[u8]) -> Sym {
    self.intern_within(bytes, MAX_ARENA_BYTES, MAX_ARENA_SYMBOLS)
  }

  /// [`Interner::intern`] with the two ceilings supplied.
  ///
  /// The ceilings are parameters rather than constants read inside so that a test can stand at the
  /// boundary: the real one is four gigabytes of interned bytes, which no suite allocates. This is
  /// the whole mechanism — [`Interner::intern`] adds only the two numbers — so a cell driven to an
  /// injected ceiling exercises the same guards a document past the real one would.
  fn intern_within(&mut self, bytes: &[u8], max_bytes: u32, max_symbols: u32) -> Sym {
    if let Some(sym) = self.map.get(bytes) {
      return Sym::new(*sym);
    }
    // Each of the three was a `len() as u32`, and each is asked before anything grows: a refusal
    // that has already pushed the bytes is a refusal with a wrapped arena behind it.
    let (Ok(start), Ok(width), Ok(sym)) = (
      u32::try_from(self.strings.len()),
      u32::try_from(bytes.len()),
      u32::try_from(self.spans.len()),
    ) else {
      return self.refuse();
    };
    let Some(end) = start.checked_add(width).filter(|end| *end <= max_bytes) else {
      return self.refuse();
    };
    if sym >= max_symbols {
      return self.refuse();
    }
    self.strings.extend_from_slice(bytes);
    self.spans.push((start, end));
    self.map.insert(bytes.to_owned().into_boxed_slice(), sym);
    Sym::new(sym)
  }

  /// The symbol a refused intern answers with: an empty spelling, minted once.
  ///
  /// A real in-range symbol rather than a sentinel, because a [`Sym`] is a **dense index** in this
  /// builder — [`Positions`] addresses a table by it, `set_type_index` resizes one to it — so a
  /// symbol outside the arena is an out-of-bounds read or a four-billion-slot `resize`, which is a
  /// worse failure than the one being repaired. The empty range is in bounds for every arena,
  /// including one that holds nothing.
  ///
  /// The build is over by the time anything reads it: [`SchemaBuilder::finish`] turns a refusal
  /// into [`SchemaErrorKind::TooManyInternedBytes`] before the §3 passes run, so the collisions a
  /// shared placeholder would make are never reported.
  fn refuse(&mut self) -> Sym {
    if let Some(sym) = self.refused {
      return sym;
    }
    let sym = match u32::try_from(self.spans.len()) {
      // One slot short of the width, so that pushing the placeholder cannot be the thing that
      // overflows `len`.
      Ok(sym) if sym < u32::MAX => {
        self.spans.push((0, 0));
        sym
      }
      // The symbol space is full as well. Symbol zero exists, because filling it took `2^32`
      // accepted interns.
      _ => 0,
    };
    let sym = Sym::new(sym);
    self.refused = Some(sym);
    sym
  }

  fn lookup(&self, bytes: &[u8]) -> Option<Sym> {
    self.map.get(bytes).copied().map(Sym::new)
  }

  /// The spelling `sym` was interned with.
  ///
  /// Indexed rather than probed: `intern_within` records a span only after establishing
  /// `start <= end <= strings.len()`, and `sym` is the index it returned, so both reads are in
  /// bounds by construction. A `get` here would answer a future break with the empty spelling
  /// instead of saying so.
  fn bytes(&self, sym: Sym) -> &[u8] {
    let (start, end) = self.spans[sym.get() as usize];
    &self.strings[start as usize..end as usize]
  }

  fn text(&self, sym: Sym) -> &str {
    // ASCII by construction; every name is admitted through `is_name` first.
    core::str::from_utf8(self.bytes(sym)).unwrap_or("")
  }

  fn len(&self) -> u32 {
    // `intern_within` refuses past `MAX_ARENA_SYMBOLS`, so this is the count rather than a wrap of
    // it. Saturating rather than `as` is what makes that a claim a reader can check: a count past
    // the width answers `u32::MAX`, which `flatten` refuses as `SchemaErrorKind::TooManyNames`
    // instead of returning a schema addressed by the low bits of it.
    u32::try_from(self.spans.len()).unwrap_or(u32::MAX)
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
/// and once per nesting level.
///
/// That second reader is **gone**, and not because the index was slow. Asking a map node once per
/// declared field is the shape whose count is a product, and no index makes a product linear:
/// [`SchemaBuilder::validate_input_object_default_cycles`] now reads a literal the way
/// [`SchemaBuilder::check_const_value`] reads one — entry first, resolved against the declaration
/// through [`DeclaredNames`]. What is left here is the required-field pass, which asks one
/// literal `Θ(required)` questions and is a value per list for the reason above. al8n/smear#198.
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

/// The first position of each name one **type declares** — an input object's fields, an enum's
/// values — addressed by name, built at most once per type and shared by every literal offered to
/// it.
///
/// # One slot per type, and a type declares one of these lists
///
/// [`RawType`] carries a list for every kind, but its `kind` is fixed before any of this runs —
/// `apply_extensions` is finished by the time `validate` starts — and
/// [`SchemaBuilder::check_const_value`] dispatches on that `kind`. So the two lists one slot may be
/// asked about are never both asked about, and one `asks` count and one sort per type index cover
/// both without a second table to resize and without a kind tag to keep them apart.
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
/// the exponent over 16 k–64 k and 851 ms at 64 k, and is 1.03 and 20 ms.
///
/// And `enum_values.iter().any(..)`, restarted for every member an enum literal writes — the same
/// mechanism a fourth time, in a spelling none of the three sweeps before it went looking for.
/// `ok(p: [E] = [Vlast …M times])` in front of a `D`-value `enum E` is `Θ(M + D)` of SDL and was
/// `Θ(M × D)` of build, on a default `Schema::build` **accepts**. The literal writes the LAST value
/// because a scan reaching its answer at the first position hides the whole product.
///
/// **Swept on `M` and `D` independently, the product is invisible in the exponent**: with the other
/// axis fixed at 4 000 the scan is 1.0 in the exponent over 1 k–128 k on both, because a fixed
/// second factor is a constant. What it is worth is the constant — 314 ms at `M` = 128 k against
/// 21 ms, 346 ms at `D` = 128 k against 54 ms — and the count, which is where a product shows:
/// `M × D` declared slots examined, 512 000 000 at either end, against 1 843 232 and 10 562 912.
/// Swept together at 1 k/4 k/16 k/64 k/128 k the exponent is the product's: **1.89** over the
/// ladder and 1.98–2.15 over the top step, **10.1 s** and 16 384 000 000 slots examined at 128 k,
/// against **1.05**, **78 ms** and 12 670 912. al8n/smear#198.
#[derive(Debug, Default)]
struct DeclaredNames {
  /// One slot per type index, filled on demand.
  of_type: Vec<Declaration>,
}

/// What one type's declared names have cost so far, and the index once one is paid for.
#[derive(Debug, Default)]
struct Declaration {
  /// Asks this declaration has answered by scanning, across the whole build.
  asks: u32,
  /// `(name, position)` sorted once, so a lookup is a binary search.
  order: Option<Vec<(Sym, u32)>>,
}

impl DeclaredNames {
  /// The first position of `wanted` in `types[base]`'s declared input fields, or `None`.
  fn first(&mut self, types: &[RawType], base: usize, wanted: Sym) -> Option<usize> {
    let declared = &types[base].input_fields;
    self.position(types.len(), base, declared, |field| field.name.sym, wanted)
  }

  /// Whether `types[base]` declares `wanted` among its enum values.
  ///
  /// The membership half of the same question, asked of the other list one slot may hold — see the
  /// header for why one slot holds both.
  fn has_enum_value(&mut self, types: &[RawType], base: usize, wanted: Sym) -> bool {
    let declared = &types[base].enum_values;
    self
      .position(types.len(), base, declared, |value| value.name.sym, wanted)
      .is_some()
  }

  /// The first position of `wanted` in `declared`, `types` being how many slots the table needs and
  /// `name` how a position of `declared` is read.
  ///
  /// Sorted pairs order equal names by ascending position, so the head of a run is the first
  /// occurrence and not merely one of them — the answer the scan it replaces gave, and the one a
  /// type that declares a name twice needs, because moving it would move a blessed diagnostic
  /// while looking like a cost change.
  fn position<T>(
    &mut self,
    types: usize,
    base: usize,
    declared: &[T],
    name: impl Fn(&T) -> Sym,
    wanted: Sym,
  ) -> Option<usize> {
    if self.of_type.len() < types {
      self.of_type.resize_with(types, Declaration::default);
    }
    let slot = &mut self.of_type[base];
    if slot.order.is_none() {
      if slot.asks < NARROW_LIST as u32 {
        slot.asks += 1;
        return declared.iter().position(|entry| name(entry) == wanted);
      }
      let mut sorted: Vec<(Sym, u32)> = declared
        .iter()
        .enumerate()
        .map(|(at, entry)| (name(entry), at as u32))
        .collect();
      sorted.sort_unstable();
      slot.order = Some(sorted);
    }
    let order = slot.order.as_ref().expect("the index was just filled");
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

/// How many nested list and input-object literals one constant value may open.
///
/// # Why a ceiling exists here at all
///
/// `SchemaBuilder::const_value` folds a constant literal with an explicit stack — one frame per
/// open container — and every one of those frames, plus the output vector it carries, is reached
/// through an **infallible** allocation. [`Schema::build`] and [`SchemaBuilder::document`] take a
/// `&TypeSystemDocument<S>`, and every carrier on the route to a constant literal has a public
/// constructor, so the literal's nesting is the *caller's* and not a parser's. Without a ceiling a
/// hand-built literal is a process abort — `memory allocation of N bytes failed` — where the whole
/// point of the door is that it answers [`SchemaErrors`].
///
/// This is the difference between here and al8n/smear#199, which met the same growth inside a
/// `Drop`: a `Drop` has no return value and no caller to tell, so stating the bound was the honest
/// answer there. `Schema::build` returns a `Result`. **Refusing is available, so refusing is what
/// happens** — and it happens before the frame is pushed, not after the allocator has been asked.
///
/// # Why 1024 — the interval, then the pick inside it
///
/// | bound | from | value |
/// |---|---|---|
/// | lower: the deepest literal smear's own doors produce | the lossless door at `HARD_MAX`, measured | **255** |
/// | upper: the deepest literal any tree a projection descends can carry | `MAX_GREEN_DEPTH`, one green level per container at least | **1024** |
///
/// The lower row is measured rather than reasoned. `parse_type_system_document_with_limits` at
/// `LosslessLimits::with_max_nesting_depth(HARD_MAX)` over `scalar Foo @x(a: [[…1…]])`, projected
/// with `project_type_system_document`: 255 brackets parses clean and yields **255 open containers**
/// (256 levels counting the innermost leaf); 256 brackets does not parse clean at all, because the
/// `(` of the argument list spends one of the same budget. So 255 is what the door at the widest
/// ceiling smear itself installs can hand this function, and a ceiling below it would make the
/// builder refuse a document this workspace's own parser had just accepted — the window
/// al8n/smear#198 closed on the projection side.
///
/// The upper row is the point past which a higher ceiling buys a caller nothing. A literal
/// container costs at least one green level, so a tree that `smear_parser::lossless::project`'s
/// walks will descend at all — they refuse past `MAX_GREEN_DEPTH`, 1024 — carries at most 1024 of
/// them; the shape that comes closest is the list value at 1.020 green levels a bracket, which
/// reaches 1003. Above the row, the only literals admitted are ones no smear door can deliver,
/// which is precisely the population this ceiling exists to bound.
///
/// **The pick is the top of the interval, and the asymmetry runs the other way from `HARD_MAX`'s.**
/// A ceiling that is too low refuses a literal a door could have produced. A ceiling that is too
/// high does *not* re-open the abort — the storage is still bounded, by a bigger constant — so
/// nothing here trades a diagnostic against a crash, and the only cost of the top of the interval
/// is the number in the next section.
///
/// # What it costs, measured
///
/// Peak live bytes across the whole `Schema::build`, `aarch64-apple-darwin`, unoptimised, the
/// document built before the instrument is armed, against a build over `type Query { ok: Int }`
/// that peaks at **67 940** bytes with no literal in it at all:
///
/// | literal | peak | over the floor |
/// |---|---|---|
/// | 1024 nested lists — exactly the ceiling, not refused | **139 637** | 2.06x |
/// | 1024 nested one-field objects — the same | **164 189** | 2.42x |
/// | any list chain past the ceiling: 1 025, 2 001, 20 001, 200 001 containers | **140 007** | 2.06x |
/// | any object chain past it | **164 583** | 2.42x |
///
/// The last two rows are one number each and not a range: the peak, and the allocation count with
/// it, stop moving with the literal's depth altogether — 200 001 containers cost what 1 025 do. So
/// the ceiling turns an unbounded, caller-chosen amplification into about 160 KiB. See
/// `SchemaBuilder::const_value` for the per-level bands those two rows sit at the end of, and for
/// what the ceiling does *not* cover: a literal's **width** costs one reduced value an entry, which
/// has no ceiling and is measured there against the band an ordinary literal-free document opens.
///
/// # What a caller past it sees
///
/// [`SchemaErrorKind::ConstantValueTooDeep`], once for the literal, at the span of the first
/// container past the ceiling and naming the argument or input value the literal was written for.
/// The refused container is replaced by a marker that stands for the skipped subtree and answers
/// no question about it — not by an empty container, which is a claim about content nothing read —
/// so the rest of the document is still checked and every other defect in it still reported. And
/// the build refuses, because a non-empty error list is what `SchemaBuilder::finish` reads.
pub const MAX_CONST_VALUE_DEPTH: usize = 1024;

/// A constant literal, reduced to what a type check reads, with the position to blame.
#[derive(Debug, Clone)]
struct RawValue {
  span: SimpleSpan,
  shape: RawShape,
}

/// A container of children a release has taken over but not drained.
enum Spent {
  /// A list literal's entries, drained where they were allocated.
  Values(vec::IntoIter<RawValue>),
  /// An object literal's fields, drained where they were allocated.
  Fields(vec::IntoIter<RawObjectField>),
}

/// Hands `shape`'s children over, leaving it a leaf.
///
/// `mem::take` rather than a read: the container is moved out whole, so no element is ever copied
/// from one buffer to another and the caller can release what is left in a single frame.
fn spend(shape: &mut RawShape, sources: &mut Vec<Spent>) {
  match shape {
    RawShape::List(values) if !values.is_empty() => {
      sources.push(Spent::Values(core::mem::take(values).into_iter()));
    }
    RawShape::Object(fields) if !fields.is_empty() => {
      sources.push(Spent::Fields(core::mem::take(fields).into_iter()));
    }
    _ => {}
  }
}

impl Drop for RawValue {
  /// Releases a literal without a native frame per level.
  ///
  /// # Why this exists at all
  ///
  /// [`SchemaBuilder::const_value`] builds one of these for every constant literal in the document,
  /// mirroring the literal's own shape, and the literal's depth is the caller's:
  /// [`Schema::build`] takes a `&TypeSystemDocument<S>` and every carrier on the route to a const
  /// directive argument has a public constructor. Making that reduction a loop moved the abort
  /// rather than removing it — measured on `aarch64-apple-darwin`, unoptimised, with the document
  /// built on another thread, `Schema::build` went from dying at **1 545** levels of list literal on
  /// a 2 MiB thread to dying at **7 737**, and the second number is this glue at 271 bytes a level
  /// where the reduction cost 1 357. A release is not a call anyone makes and there is no
  /// diagnostic to return, so it goes the same way the parser's value tree went in al8n/smear#199.
  ///
  /// # The invariant
  ///
  /// **Anything not handed over is released here, so anything not handed over must be a leaf.** A
  /// child released inside this loop re-enters this `drop`, and that is exactly two frames deep
  /// because [`spend`] has already emptied its container: the re-entered call finds nothing to hand
  /// over, allocates nothing, and returns.
  ///
  /// A source is dropped the moment its last child is taken, so a chain of one-element lists costs
  /// one entry at any depth rather than one per level; a container is taken over whole, so a
  /// literal of a million scalars costs one entry too. What is left is one entry per ancestor with
  /// an unvisited child — the literal's *branching* nesting.
  ///
  /// `sources` grows through an infallible `push` like every other work list here, and here that is
  /// answerable rather than merely stated: the only thing that builds a [`RawValue`] is
  /// [`SchemaBuilder::const_value`], which refuses past [`MAX_CONST_VALUE_DEPTH`] open containers,
  /// so the branching nesting this walks is bounded by that ceiling before the value exists.
  ///
  /// # What this does not repair
  ///
  /// The derived `Debug` and `Clone` still descend one frame per level. Neither is reachable today:
  /// nothing in this crate formats or clones a [`RawValue`], and the type is private. What this
  /// removes is the one of the three that fires without a call being made.
  fn drop(&mut self) {
    let mut sources: Vec<Spent> = Vec::new();
    spend(&mut self.shape, &mut sources);
    loop {
      let mut value = match sources.last_mut() {
        None => return,
        Some(Spent::Values(rest)) => {
          let Some(value) = rest.next() else {
            sources.pop();
            continue;
          };
          if rest.as_slice().is_empty() {
            sources.pop();
          }
          value
        }
        Some(Spent::Fields(rest)) => {
          let Some(field) = rest.next() else {
            sources.pop();
            continue;
          };
          if rest.as_slice().is_empty() {
            sources.pop();
          }
          // The name is a `Located`, which owns nothing this loop can reach.
          field.value
        }
      };
      spend(&mut value.shape, &mut sources);
      // Released here with an empty container: one re-entry, no descent.
    }
  }
}

/// The literal itself.
///
/// Only the two numeric arms keep their spelling — that is what the range checks read — and only
/// the enum arm keeps its name. Everything else is decided by shape alone, so a `String`'s bytes
/// are dropped rather than copied into the builder.
///
/// # The three that keep one keep a symbol, not the bytes
///
/// Each of those three arms held a `Box<[u8]>` and filled it with `source().as_ref().into()` —
/// **an allocation and a copy per occurrence**. That is the one place in this reduction where the
/// output is not injective into the tree it reads, and the amplification is unbounded from a
/// bounded input: `ConstInputValue` is public and `Clone`, a clone of a leaf copies the `S` and not
/// the bytes behind it, so `N` clones of one `B`-byte literal are `O(B + N)` live in the caller's
/// hand and were `O(N × B)` retained here. Measured on `aarch64-apple-darwin`, unoptimised, as the
/// band between the peak live bytes of the caller's own tree and the peak across `Schema::build`
/// over it, `N` list entries each a clone of one parsed `B`-byte leaf:
///
/// | | `B` = 8 | 64 | 256 | 1 024 | 4 096 |
/// |---|---|---|---|---|---|
/// | `N` = 1 000 | 78 710 | 134 710 | 326 710 | 1 094 710 | 4 166 710 |
/// | 8 000 | 470 710 | 918 710 | 2 454 710 | 8 598 710 | **33 174 710** |
///
/// The fit is exact and it has two terms: `∂/∂N` is `B + 48` and `∂/∂B` is `N`. The 48 is the
/// [`RawValue`] the reduction is entitled to — one per `ConstInputValue`, which is what makes the
/// rest of it injective — and the `N × B` beside it is this copy. The caller's own tree over the
/// same grid is `88N` and does not move with `B` at all.
///
/// **[`MAX_CONST_VALUE_DEPTH`] does not see it**: the shape above is one list one level deep, so
/// the ceiling is read once with no frame open. A width ceiling would not have been the answer
/// either — the population that reaches this is ordinary valid documents, and a constant that
/// admitted them would have to admit the amplification too.
///
/// So the three arms hold a [`Sym`] into [`SchemaBuilder`]'s literal interner, a second
/// [`Interner`] beside the name arena, and the retained bytes are **one copy per distinct
/// spelling**. Cloning, which costs the caller nothing, now costs this reduction four bytes.
///
/// # Dedup bounds repetition and nothing else, and here is the measurement
///
/// An earlier reading of this had the three leaf constructors — `IntValue::new` and its two
/// siblings — being `pub(crate)`, therefore a caller's only route to one of these arms being a
/// parse or a clone, therefore a *distinct* spelling costing its own bytes in some source. The
/// first two steps hold; the third does not follow, because a parse is not injective into the
/// bytes it reads. `IntValue::graphql` is a **public** associated parser — `graphql_slice_api!`
/// generates one for each of the three — so a caller may run it over every suffix of one buffer
/// and hold `B` leaves that borrow the same `B` bytes. Measured through `Schema::build`'s own
/// door, as the literal arena after one assembled document whose single list holds those leaves:
///
/// | `B` | 10 | 30 | 100 | 300 |
/// |---|---|---|---|---|
/// | literal-arena bytes | 55 | 465 | 5 050 | 45 150 |
/// | over the buffer | 5.5x | 15.5x | 50.5x | 150.5x |
///
/// The fit is `B(B+1)/2` exactly and the ratio is `(B+1)/2`, so there is no size at which it
/// flattens. The name arena has the same shape through `Name::new`, which is public and needs no
/// parse at all.
///
/// What bounds it is [`MAX_ARENA_BYTES`] rather than this reduction: past four gigabytes the arena
/// refuses and the build answers [`SchemaErrorKind::TooManyInternedBytes`], which arrives at
/// `B ≈ 92 682` — a 92 KB input. Bounding the *growth* is a separate design and this is not it:
/// every retained byte is a distinct spelling the coercion table reads, so there is no copy left
/// to remove. What changed is which sentence is load-bearing — the ceiling, not a `pub(crate)` on
/// a constructor.
#[derive(Debug, Clone)]
enum RawShape {
  Null,
  Boolean,
  Int(Sym),
  Float(Sym),
  String,
  Enum(Sym),
  List(Vec<RawValue>),
  Object(Vec<RawObjectField>),
  /// A container past [`MAX_CONST_VALUE_DEPTH`], standing for the whole subtree it opened.
  ///
  /// **Not a shape any caller wrote.** It is the reduction saying it stopped here, and the
  /// [`SchemaErrorKind::ConstantValueTooDeep`] naming the literal is already in the error list by
  /// the time anything reads it. Every walk over a [`RawValue`] treats it as terminal and asks it
  /// nothing, because there is nothing to ask: the content it stands for was never built.
  ///
  /// What stood here was an empty container of the refused kind, and that is a statement about
  /// the skipped content rather than an absence of one — a statement no reader can tell from the
  /// caller's own `[]` or `{}`. [`SchemaBuilder::check_const_value`] read it as an object with no
  /// fields and reported every required field of the target missing from a subtree that may
  /// supply all of them; [`map_nodes`] offered it to the cycle rule as a map node supplying
  /// nothing, which is what makes that rule descend into a field's *own* default. A validator
  /// that invents defects is worse than one that stops, because the stop is visible.
  ///
  /// **What dropping the kind costs, measured.** The empty container carried one TRUE fact beside
  /// the false one — whether the refused node was a list or a map — and the coercion table could
  /// answer from it: `input B { s: Int, a: B }` with a chain whose innermost `s` is refused
  /// reported `ConstantValueTooDeep` *and* the coercion failure at that node, and now reports only
  /// the first. That verdict is about the refused node itself, on a document the ceiling is
  /// already refusing, so it is redundant rather than lost — and keeping the kind would put
  /// "which container is this" back in front of every walk, which is the question the sentinel
  /// made unsafe to ask. A coercion failure ELSEWHERE in the same literal is unaffected: the root
  /// of a literal is never refused, because the ceiling is read with no frame open.
  Refused,
}

impl RawShape {
  /// The shape, as the coercion table names it — `None` for a refused subtree, which has no
  /// content for the table to read and no answer to give it.
  const fn shape(&self) -> Option<LiteralShape> {
    match self {
      Self::Null => Some(LiteralShape::Null),
      Self::Boolean => Some(LiteralShape::Boolean),
      Self::Int(_) => Some(LiteralShape::Int),
      Self::Float(_) => Some(LiteralShape::Float),
      Self::String => Some(LiteralShape::String),
      Self::Enum(_) => Some(LiteralShape::Enum),
      Self::List(_) => Some(LiteralShape::List),
      Self::Object(_) => Some(LiteralShape::Object),
      Self::Refused => None,
    }
  }

  /// The retained spelling the numeric ranges read, `None` for every other shape.
  ///
  /// A symbol rather than the bytes, resolved through [`Model::spelling`]: the bytes live once per
  /// distinct spelling in the literal interner, and handing them out from here would put the
  /// interner's lifetime on this method.
  const fn spelling(&self) -> Option<Sym> {
    match self {
      Self::Int(sym) | Self::Float(sym) => Some(*sym),
      _ => None,
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
  // `Vec` by name, because this module is `mod`-scoped and a prelude is not: the crate root aliases
  // `alloc` to `std` under `no_std`, so the outer file's `use std::vec::Vec` is the only thing that
  // resolves the name and an inner module does not inherit it. Without this, `smear-schema` with
  // `build` and without `std` — a cell `cargo hack --each-feature` builds — is three `E0425`s, and
  // it is what has kept both `build` and both `clippy` jobs red since this module landed.
  use std::vec::Vec;

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
  /// Whether `@deprecated` is written on this field, decided once where the list is built.
  ///
  /// Draft `IsValidImplementation` 2.6 asks it of BOTH sides of every pair the conformance pass
  /// forms, and [`is_deprecated`] answers by scanning a written directive list and comparing each
  /// name's *text*. The pair count is the rule's own extent — an implementor's field against an
  /// interface's, once each — but the scan is a third factor multiplying it, and nothing bounds a
  /// field's directive list. `K` interfaces declaring one field, in front of one implementor whose
  /// field carries `D` directives, asks the implementor's own list `K` times: `Θ(K + D)` of SDL,
  /// `Θ(K × D)` of build — 1.77 in the exponent over 500–8 000 with `K = D`, 1.97 over the top
  /// step and **299.5 ms** at 8 000, on a schema `Schema::build` **accepts**, against 0.78, 1.05
  /// and **10.3 ms**.
  ///
  /// A bit on the field rather than a table beside the pass, because the answer is a property of
  /// the declaration and not of the pair: an extension appends new fields and never edits one that
  /// is already here, so what is decided at ingest is still true at conformance. al8n/smear#198.
  deprecated: bool,
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
  /// The resolved type index of each of [`members`](RawType::members), sorted, filled in during
  /// validation beside the closure.
  ///
  /// Draft §3.6.1's covariance check asks "is this object a member of that union" once per field an
  /// interface declares, and [`Model::is_sub_type`] answered it by scanning `members` and resolving
  /// each entry's name. `Θ(interface fields × union members)`, with a ceiling on neither: a
  /// `K`-member union behind `F` interface fields whose implementor names the LAST member is
  /// `Θ(F + K)` of SDL and was `Θ(F × K)` of build — with `F = K`, 1.48 in the exponent over
  /// 1 k–16 k, 1.81 over the top step and **156.6 ms** at 16 000, on a schema `Schema::build`
  /// **accepts**, against 0.95 and **28.2 ms**. Sorted once per union, the answer is a binary
  /// search.
  ///
  /// Empty for every other kind, and empty for a member that names nothing — which is the
  /// `UndefinedUnionMember` refusal, and an answer of `false` either way.
  sorted_members: Vec<u32>,
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
      sorted_members: Vec::new(),
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
  /// The literal spellings, deduplicated. See [`SchemaBuilder::literals`].
  literals: &'a Interner,
}

impl<'a> Model<'a> {
  fn text(&self, sym: Sym) -> &'a str {
    self.interner.text(sym)
  }

  /// The bytes behind a literal's retained spelling, empty where the shape retains none.
  ///
  /// Empty for `None` rather than refusing it, because that is what the two shapes with no
  /// spelling to keep mean to [`BuiltInScalar::accepts`]: the range arms are the only readers and
  /// they are reached only from the two numeric shapes.
  fn spelling(&self, sym: Option<Sym>) -> &'a [u8] {
    match sym {
      Some(sym) => self.literals.bytes(sym),
      None => &[],
    }
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
  ///
  /// # Both arms are searched, not scanned
  ///
  /// This runs once per field an interface declares, per type implementing it, and both
  /// populations it consults are the document's. `members.iter().any(..)` and
  /// `closure.contains(..)` were therefore two products, one per arm — see
  /// [`RawType::sorted_members`] for the union half's numbers, and the closure half is the same
  /// sentence about an object's interface list: `Θ(interface fields × closure)`, answered on the
  /// first entry only when the interface asked about happens to sort first. `F` fields typed as the
  /// LAST of a `K`-interface closure, `F = K`, is 1.18 in the exponent over 1 k–16 k, 1.34 over the
  /// top step and 38.5 ms at 16 000, against 1.07, 1.14 and 26.6 ms.
  ///
  /// Both lists are already sorted where they are built — [`SchemaBuilder::compute_closures`] sorts
  /// the closure, [`SchemaBuilder::index_union_members`] the members — so neither arm needed an
  /// index built here, only a search instead of a walk.
  fn is_sub_type(&self, candidate: TypeId, abstract_type: TypeId) -> bool {
    if candidate == UNRESOLVED || abstract_type == UNRESOLVED {
      return false;
    }
    let target = &self.types[abstract_type.get() as usize];
    match target.kind {
      TypeKind::Union => target
        .sorted_members
        .binary_search(&candidate.get())
        .is_ok(),
      TypeKind::Interface => self.types[candidate.get() as usize]
        .closure
        .binary_search(&abstract_type.get())
        .is_ok(),
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
  /// The spellings a literal's three retaining arms keep, one copy per **distinct** spelling.
  ///
  /// A second arena rather than a second use of `interner`, for two reasons that both bite. The
  /// name arena's symbol space is dense-indexed — `type_of_sym`, `directive_of_sym` and
  /// `Positions`'s table are all `Θ(symbols)` — so interning `42` there would widen every one of
  /// them for a spelling no name lookup can ever hit; and the finished [`Schema`] keeps that arena,
  /// whose ASCII-name invariant is what makes `Schema::name` infallible, while a literal's spelling
  /// is a caller's bytes and answers `is_name` for none of the three arms. This one is dropped with
  /// the builder: [`SchemaBuilder::flatten`] destructures the name arena out and lets the rest go.
  ///
  /// See [`RawShape`] for what it is bounding and for the two-dimensional measurement of what it
  /// replaced.
  literals: Interner,
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
    // Asked twice, around the only step after ingest that interns: ingest reads the document's
    // own names and literals, injection reads the built-ins', and applying an extension only
    // resolves symbols already in. The first call is what keeps injection from running over a
    // refused arena at all — every built-in name would come back as the same placeholder, and
    // `type_definition` would file each one as a redefinition of the last.
    if let Some(refusal) = self.arena_refusal() {
      return Err(refusal);
    }
    self.inject_built_ins();
    self.apply_extensions();
    if let Some(refusal) = self.arena_refusal() {
      return Err(refusal);
    }

    self.validate();

    if !self.errors.is_empty() {
      return Err(SchemaErrors::new(self.errors));
    }
    self.flatten()
  }

  /// The one diagnostic a build whose arena stopped growing can honestly make.
  ///
  /// Exactly one, and everything accumulated before it is dropped rather than reported beside it.
  /// A refused arena answers every later spelling with the same placeholder, so what the rules see
  /// is one name defined over and over: padding an arena to `u32::MAX` under a document holding a
  /// single type produced **fifteen** duplicate-definition diagnostics, none of which is about
  /// anything the caller wrote. A refusal ends the document, the way a nesting refusal does —
  /// al8n/smear#179.
  ///
  /// The span is the document's default rather than a position, because there is no position: the
  /// arena is full, not the spelling that found it full, and the next spelling of any length would
  /// have been refused just the same.
  fn arena_refusal(&self) -> Option<SchemaErrors> {
    (self.interner.refused.is_some() || self.literals.refused.is_some()).then(|| {
      SchemaErrors::new(vec![SchemaError::new(
        SchemaErrorKind::TooManyInternedBytes,
        "schema",
        SimpleSpan::default(),
      )])
    })
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
        literals: &self.literals,
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
          // Interned from the borrowed slice: `to_owned()` stood here, which is an allocation and
          // a copy of a caller-sized name per interface written, and the interner copies what it
          // keeps anyway. The borrow is the argument's and not `self`'s, so nothing needed it.
          let span = *name.as_span();
          let document = self.document;
          let sym = self.interner.intern(name.source().as_ref());
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
          // The same removal as [`SchemaBuilder::implements`]'s, for the same reason.
          let span = *name.as_span();
          let document = self.document;
          let sym = self.interner.intern(name.source().as_ref());
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
              value: self.const_value(argument.name(), argument.value()),
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
  /// # Why this is a loop
  ///
  /// It recursed, on the argument that the AST handed in was built by a recursive-descent parser
  /// and would be dropped recursively, so a literal deep enough to overflow here could not have
  /// been parsed in the first place. **Neither half of that argument holds.**
  ///
  /// The parser is not the only way in. [`Schema::build`] and [`SchemaBuilder::document`] take a
  /// `&TypeSystemDocument<S>`, and the carriers on the route to a constant literal — a scalar
  /// definition, a const directive, a const argument, and the value enum itself — each have a
  /// public constructor, so `scalar Foo @x(a: [[[…]]])` is safe code no parse ever saw. And
  /// al8n/smear#199 gave the value tree an iterative release, so the drop that was supposed to give
  /// out first no longer does.
  ///
  /// Measured on `aarch64-apple-darwin`, unoptimised, one child process per depth with the document
  /// built on another thread: `Schema::build` over exactly that document aborted at **1 545** levels
  /// of list literal on a 2 MiB thread — libtest's, tokio's and `std::thread::spawn`'s — 389 on
  /// 512 KiB and 196 on 256 KiB. An object literal is worse by about a tenth.
  ///
  /// # What the loop holds
  ///
  /// One frame per **open container**, holding the output vector that container is being built
  /// into, and the *borrowed* iterator over its remaining children. Children are never copied into
  /// a work list, so the peak follows the literal's nesting and not its width.
  ///
  /// **It is not free, and it is not smaller than what it builds.** Measured on the same host, as
  /// peak live bytes across the whole `Schema::build` with the document built before the instrument
  /// is armed: a flat literal of N empty lists peaks at **48 bytes an entry**, which is the
  /// [`RawValue`] tree and nothing else — one frame serves the whole width, and the allocation count
  /// does not move with N at all (215 either side of an eightfold widening). A chain of N
  /// one-element lists peaks between **138 and 224 bytes a container**, an object chain between
  /// **162 and 248**, because there is a frame per container and the `Vec` holding them doubles: the
  /// low end of each band is a count just under a power of two and the high end is a count just over
  /// one. So on the adversarial shape the frames are two to four times the tree they are producing.
  ///
  /// An earlier revision of this paragraph recorded the chain band as *199 to 223*. The high end is
  /// right and the low end was a sample rather than a minimum: at 1 001 containers the reading is
  /// 138.4 bytes each and at 8 001 it is 138.1, because the frame vector's capacity is then almost
  /// exactly the count. The band is the doubling, so both of its ends have to be taken from where
  /// the doubling puts them.
  ///
  /// # Why a loop still needed a ceiling
  ///
  /// Making the walk iterative moved the growth from the native stack to the heap, and that is the
  /// point — but `frames.push` and `Vec::with_capacity` are **infallible** allocation paths, so a
  /// caller-built literal deep enough still ends the process rather than the build. That is the
  /// abort al8n/smear#199 had no answer for, because it was inside a `Drop`; here there is one.
  /// [`Schema::build`] returns `Result<_, SchemaErrors>`, so the ceiling is a refusal a caller can
  /// read: past [`MAX_CONST_VALUE_DEPTH`] open containers the literal is refused with
  /// [`SchemaErrorKind::ConstantValueTooDeep`], **before** the frame that would have carried it is
  /// pushed and before its output vector is reserved.
  ///
  /// The refusal is per literal and it substitutes [`RawShape::Refused`], so the walk finishes and
  /// every other defect in the document is still reported. It substitutes a MARKER and not an empty
  /// container of the refused kind, which is what stood here: an empty container is a claim about
  /// the skipped content — "this object supplies no fields" — and it is a claim no reader can tell
  /// from the caller's own `{}`. [`SchemaBuilder::check_const_value`] believed it and reported
  /// every required field of the target missing from a subtree that may supply all of them. Only
  /// the first refused container is reported: the levels under it are never read, so there is
  /// nothing further to say about them.
  ///
  /// **What the ceiling does and does not cover.** It bounds the growth that follows the literal's
  /// *nesting* — the frame stack and the one output vector per level — which is the term with no
  /// bound in the input. It does not bound the growth that follows the literal's *width*: a list of
  /// a million entries still reserves a million [`RawValue`]s, 48 bytes each, and an object's
  /// fields 72.
  ///
  /// An earlier revision justified leaving that uncovered with "the width-driven request is
  /// proportional to a tree the allocator has already satisfied once", and **that is not an
  /// argument**: the caller's tree is still LIVE while the reduction runs, so having fitted the
  /// tree says nothing about fitting the tree *and* its reduction. What stands here instead is a
  /// measurement of the band those two footprints leave between them — the allocator limits that
  /// admit the caller's document and abort the build. Peak live bytes, `aarch64-apple-darwin`,
  /// unoptimised, one process per row, N = 8 000:
  ///
  /// | document | building it peaks at | `Schema::build` peaks at | band |
  /// |---|---|---|---|
  /// | hand-built list literal, N entries | 709 289 | 1 162 639 | **453 350** — 0.64x |
  /// | hand-built object literal, N fields | 1 157 291 | 1 803 383 | **646 092** — 0.56x |
  /// | the same list literal, PARSED from SDL | 1 104 538 | 1 207 552 | **103 014** — 0.09x |
  /// | N one-field object types, **no literal at all** | 12 277 717 | 33 292 363 | **21 014 646** — 1.71x |
  ///
  /// The band is real and it is this reduction's: 48N of the first row's is the output vector, a
  /// limit of 900 000 bytes on that row aborts at the 384 000-byte reservation, and the width term
  /// is 48.0 bytes an entry fitted over 1 000–8 000 (72.0 for the object row).
  ///
  /// **And a width ceiling still could not buy anything, which is what the last row is for.** That
  /// row is ordinary SDL `Schema::build` accepts, no ceiling refuses, and no literal appears in it
  /// — every allocation it makes is sized by the input just as this one is — and its band is 46
  /// times wider absolutely and 2.7 times wider against its own document. A caller whose allocator
  /// limit sits in this reduction's band has one sitting in that document's band too, so refusing
  /// wide literals would leave the abort exactly where it was and read as a guarantee it does not
  /// keep. The set `try_reserve` would have to reach to keep it is not "the sites whose size is
  /// chosen by the input" narrowed down — the last row is what happens when every such site is
  /// counted, and it is the whole builder.
  ///
  /// The parsed row says the same thing from the other side: at N = 8 000 the parse peaks 358 336
  /// bytes above the AST it leaves resident, so on the route a deployment actually takes the
  /// reduction fits almost entirely inside a band the parse has already opened — the band widens by
  /// 3.9 bytes an entry there rather than 48.
  ///
  /// What separates this from the depth term is not the size of the band but the shape of the
  /// growth. The reduction is injective into the tree it reads — one [`RawValue`] per
  /// `ConstInputValue` (48 against 88 bytes) and one [`RawObjectField`] per `ConstObjectField` (72
  /// against 144) — so widening the literal cannot make the output outgrow the input that named
  /// it. Past the ceiling above, the frames were two to four times the tree they produced and
  /// nothing in the input bounded how many of them there were.
  ///
  /// An object field's name is interned when the field is reached and not when it is queued, so the
  /// symbols are minted in the same order the recursion minted them; the name waits on its own
  /// frame while the value below it is reduced, which is what `pending` is for.
  fn const_value<S>(&mut self, subject: &Name<S>, value: &ConstInputValue<S>) -> RawValue
  where
    S: AsRef<[u8]>,
  {
    /// A container whose children are still being reduced.
    enum Frame<'a, S> {
      List {
        span: SimpleSpan,
        values: Vec<RawValue>,
        rest: core::slice::Iter<'a, ConstInputValue<S>>,
      },
      Object {
        span: SimpleSpan,
        fields: Vec<RawObjectField>,
        /// The name of the field whose value is being reduced right now.
        ///
        /// One at a time, because the walk is depth-first: a frame reaches its next field only
        /// once the previous field's whole subtree has been folded into `fields`.
        pending: Option<Located>,
        rest: core::slice::Iter<'a, ConstObjectField<S>>,
      },
    }

    let mut frames: Vec<Frame<'_, S>> = Vec::new();
    let mut answer: Option<RawValue> = None;
    let mut current: Option<&ConstInputValue<S>> = Some(value);
    // Where the first container past `MAX_CONST_VALUE_DEPTH` was written, once one has been.
    let mut refused: Option<SimpleSpan> = None;

    loop {
      let Some(value) = current.take() else {
        // Nothing in hand: take the next child of the innermost open container, or close it.
        match frames.last_mut() {
          None => break,
          Some(Frame::List { rest, .. }) => match rest.next() {
            Some(entry) => current = Some(entry),
            None => {
              let Some(Frame::List { span, values, .. }) = frames.pop() else {
                unreachable!("the frame just matched as a list")
              };
              emit(
                &mut frames,
                &mut answer,
                RawValue {
                  span,
                  shape: RawShape::List(values),
                },
              );
            }
          },
          Some(Frame::Object { rest, pending, .. }) => match rest.next() {
            Some(field) => {
              *pending = Some(self.located(field.name()));
              current = Some(field.value());
            }
            None => {
              let Some(Frame::Object { span, fields, .. }) = frames.pop() else {
                unreachable!("the frame just matched as an object")
              };
              emit(
                &mut frames,
                &mut answer,
                RawValue {
                  span,
                  shape: RawShape::Object(fields),
                },
              );
            }
          },
        }
        continue;
      };

      let span = *value.as_span();
      let shape = match value {
        ConstInputValue::Null(_) => RawShape::Null,
        ConstInputValue::Boolean(_) => RawShape::Boolean,
        ConstInputValue::String(_) => RawShape::String,
        // Interned rather than copied, and see [`RawShape`] for the measurement: `.into()` here was
        // an allocation and a copy of the caller's spelling PER OCCURRENCE, which is the one term
        // in this reduction that a bounded input does not bound.
        ConstInputValue::Int(int) => RawShape::Int(self.literals.intern(int.source().as_ref())),
        ConstInputValue::Float(float) => {
          RawShape::Float(self.literals.intern(float.source().as_ref()))
        }
        ConstInputValue::Enum(member) => {
          RawShape::Enum(self.literals.intern(member.source().as_ref()))
        }
        // The ceiling is read BEFORE either allocation the arm would make — the frame and the
        // output vector it reserves — so the refusal costs nothing the abort it replaces would
        // have spent. `RawShape::Refused` takes the subtree's place, which is what lets the walk
        // finish and the rest of the document still be checked WITHOUT any walk behind this one
        // reading a claim about what was skipped. The kind is not kept: nothing downstream asks a
        // refused subtree a question, so a list one and an object one would answer alike.
        ConstInputValue::List(list) => {
          if frames.len() < MAX_CONST_VALUE_DEPTH {
            let entries = list.values();
            frames.push(Frame::List {
              span,
              values: Vec::with_capacity(entries.len()),
              rest: entries.iter(),
            });
            continue;
          }
          refused.get_or_insert(span);
          RawShape::Refused
        }
        ConstInputValue::Object(object) => {
          if frames.len() < MAX_CONST_VALUE_DEPTH {
            let fields = object.fields();
            frames.push(Frame::Object {
              span,
              fields: Vec::with_capacity(fields.len()),
              pending: None,
              rest: fields.iter(),
            });
            continue;
          }
          refused.get_or_insert(span);
          RawShape::Refused
        }
      };
      emit(&mut frames, &mut answer, RawValue { span, shape });
    }

    if let Some(span) = refused {
      // Rendered lossily for the same reason `located` renders an invalid name that way: the
      // subject is a caller's `S`, and a refusal must not be the thing that decides it is UTF-8.
      let name = String::from_utf8_lossy(subject.source().as_ref()).into_owned();
      self.errors.push(
        SchemaError::new(SchemaErrorKind::ConstantValueTooDeep, &name, span)
          .in_document(self.document),
      );
    }

    /// Folds a finished value into the container it belongs to, or into the answer.
    fn emit<S>(frames: &mut [Frame<'_, S>], answer: &mut Option<RawValue>, value: RawValue) {
      match frames.last_mut() {
        None => *answer = Some(value),
        Some(Frame::List { values, .. }) => values.push(value),
        Some(Frame::Object {
          fields, pending, ..
        }) => fields.push(RawObjectField {
          name: pending
            .take()
            .expect("an object frame reached a value with no field name waiting on it"),
          value,
        }),
      }
    }

    answer.expect("the walk folds exactly one value into the answer")
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
        let deprecated = is_deprecated(&self.interner, &directives);
        RawField {
          name,
          ty,
          args: args.into(),
          directives,
          deprecated,
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
          .map(|default| self.const_value(value.name(), default.value()));
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
  ///
  /// # The chain is walked once and remembered in a fixed window
  ///
  /// This recursed, one native frame per `[`, on the argument [`SchemaBuilder::const_value`]'s
  /// header used to make for both of them: the AST came from a recursive-descent parser and would
  /// be dropped recursively anyway. **Neither half of that holds.** `Type: From<ListType<Self>>` is
  /// public, so the chain is built with a loop and no parser sees it; and al8n/smear#199 gave the
  /// enum an iterative release, so the drop that was supposed to give out first no longer does.
  /// Measured on `aarch64-apple-darwin`, unoptimised, one child process per depth with the document
  /// built on another thread: `Schema::build` over `type Q { f: [[…Int…]] }` aborted at **4 524**
  /// brackets on a 2 MiB thread and **570** on 256 KiB.
  ///
  /// The wrappers are applied innermost first and the chain is walked outermost first, so something
  /// has to be remembered across the turn. It is **not** the chain: [`PackedType`] holds
  /// [`MAX_WRAPPERS`] codes and saturates, and every list level costs at least one code, so no level
  /// outside the innermost `MAX_WRAPPERS` can change either the word or the `too_deep` flag. What is
  /// kept is a ring of that many `required` bits — a fixed 15 bytes, at any depth.
  fn type_ref<S>(&mut self, ty: &Type<Name<S>>) -> RawTypeRef
  where
    S: AsRef<[u8]>,
  {
    /// The innermost levels whose `required` bit can still reach the packed word.
    const WINDOW: usize = MAX_WRAPPERS as usize;

    // The outermost span is the reference's, and the walk below leaves it behind at the first step.
    let span = match ty {
      Type::Name(named) => *named.span(),
      Type::List(list) => *list.span(),
    };
    let mut window = [false; WINDOW];
    let mut levels = 0usize;
    let mut cursor = ty;
    let named = loop {
      match cursor {
        Type::Name(named) => break named,
        Type::List(list) => {
          window[levels % WINDOW] = list.required();
          levels += 1;
          cursor = list.ty();
        }
      }
    };

    let base = self.located(named.name());
    let mut packed = PackedType::named(base.sym, UNRESOLVED);
    // A level the window could not hold is a level whose `list` code the word had no room for:
    // `WINDOW` levels fill it on their own, so anything past that is refused before it is read.
    let mut too_deep = levels > WINDOW;
    let push =
      |packed: &mut PackedType, too_deep: &mut bool, code: fn(PackedType) -> Option<PackedType>| {
        match code(*packed) {
          Some(next) => *packed = next,
          None => *too_deep = true,
        }
      };
    if named.required() {
      push(&mut packed, &mut too_deep, PackedType::push_non_null);
    }
    for level in (levels.saturating_sub(WINDOW)..levels).rev() {
      push(&mut packed, &mut too_deep, PackedType::push_list);
      if window[level % WINDOW] {
        push(&mut packed, &mut too_deep, PackedType::push_non_null);
      }
    }

    RawTypeRef {
      base,
      span,
      packed,
      too_deep,
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
    self.index_union_members();
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
  ///
  /// # The membership mark, and why `contains` was the same product a fourth time
  ///
  /// The walk below is a depth-first traversal that must not visit a node twice, and "have I got
  /// this one already" was asked by scanning the closure built so far — `Θ(popped × closure)`,
  /// with a ceiling on neither. `type T implements I0 & … & I{K-1}` in front of `K` interfaces that
  /// implement nothing is `Θ(K)` of SDL, `Θ(K²)` of walk, and a schema `Schema::build` **accepts**:
  /// 2.15 in the exponent over 2 k–4 k and 16.0 ms at `K` = 4 000, of which the traversal is the
  /// term that grows.
  ///
  /// One `bool` per type index answers it in `O(1)`, and the marks are cleared by walking the
  /// closure this type just built rather than the table — `O(closure)`, not `O(types)`, which is
  /// what lets one table serve every type in the schema. It is the shape [`Positions`] takes for
  /// the same reason, minus the reentrancy question: this walk nests no second closure inside the
  /// one it is building. Lazily grown, so a schema whose types implement nothing never allocates
  /// it. The closure is pushed in the traversal order it was pushed in and sorted exactly where it
  /// was sorted, so what a later pass reads is unchanged. al8n/smear#198.
  fn compute_closures(&mut self) {
    let count = self.types.len();
    let mut member: Vec<bool> = Vec::new();
    for index in 0..count {
      if self.types[index].implements.is_empty() {
        continue;
      }
      if member.len() < count {
        member.resize(count, false);
      }
      let mut closure: Vec<u32> = Vec::new();
      let mut stack: Vec<u32> = Vec::new();
      for declared in &self.types[index].implements {
        if let Some(target) = self.type_index(declared.sym) {
          stack.push(target as u32);
        }
      }
      while let Some(next) = stack.pop() {
        if member[next as usize] {
          continue;
        }
        member[next as usize] = true;
        closure.push(next);
        for declared in &self.types[next as usize].implements {
          if let Some(target) = self.type_index(declared.sym) {
            stack.push(target as u32);
          }
        }
      }
      for &id in &closure {
        member[id as usize] = false;
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

  /// Fills [`RawType::sorted_members`], one pass over the members the whole schema declares.
  ///
  /// Here rather than at ingest because an `extend union` appends to `members` after the definition
  /// was read, and after [`SchemaBuilder::resolve_type_refs`] because it is the resolved index that
  /// is stored. Nothing it reads changes during validation.
  fn index_union_members(&mut self) {
    for index in 0..self.types.len() {
      if self.types[index].kind != TypeKind::Union {
        continue;
      }
      let mut sorted: Vec<u32> = self.types[index]
        .members
        .iter()
        .filter_map(|member| self.type_index(member.sym).map(|at| at as u32))
        .collect();
      sorted.sort_unstable();
      self.types[index].sorted_members = sorted;
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
    Self::check_directive_uses(
      model,
      errors,
      names,
      DirectivesOf::Schema,
      Coordinate::schema(),
    );

    for ty in 0..model.types.len() {
      let owner = Coordinate::named(model.types[ty].name.sym);
      Self::check_directive_uses(model, errors, names, DirectivesOf::Type { ty }, owner);

      for field in 0..model.types[ty].fields.len() {
        let path = owner.then(model.types[ty].fields[field].name.sym);
        Self::check_directive_uses(
          model,
          errors,
          names,
          DirectivesOf::Field { ty, field },
          path,
        );

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
        Self::check_directive_uses(
          model,
          errors,
          names,
          DirectivesOf::InputField { ty, field },
          path,
        );
      }

      for value in 0..model.types[ty].enum_values.len() {
        let path = owner.then(model.types[ty].enum_values[value].name.sym);
        Self::check_directive_uses(
          model,
          errors,
          names,
          DirectivesOf::EnumValue { ty, value },
          path,
        );
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
  ///
  /// # Why it is a loop
  ///
  /// It descended one native frame per level of literal, through two sites: a list entry offered to
  /// the item type, and an input-object field offered to its declared type. The first is bounded —
  /// [`PackedType`] holds [`MAX_WRAPPERS`] wrappers and a list position past that has no item type
  /// to strip — and **the second is not**: `input A { a: A }` is a legal nullable self-reference, so
  /// `{ a: { a: … } }` offered to `A` descends as far as the literal goes. Measured on
  /// `aarch64-apple-darwin`, unoptimised, one child process per depth with the document built on
  /// another thread: `Schema::build` over exactly that aborted at **1 327** levels on a 2 MiB
  /// thread, 334 on 512 KiB and 168 on 256 KiB — shallower than
  /// [`SchemaBuilder::const_value`]'s own boundary, so it is the first thing to give out once that
  /// one is a loop.
  ///
  /// What replaces the frames is one entry per **open literal**, holding the borrowed iterator over
  /// what is left of it. Nothing about which diagnostic is reported, in what order, or with which
  /// span changes: a level's fields are all offered before the level is closed, and the omitted-
  /// required pass runs at that close, exactly where the frame used to return.
  ///
  /// `levels` grows infallibly, and it is bounded by the literal rather than by the type graph: the
  /// walk descends only where the literal does, and the literal was built by
  /// [`SchemaBuilder::const_value`] under [`MAX_CONST_VALUE_DEPTH`]. `input A { a: A }` is still a
  /// legal self-reference and the type side is still unbounded; what ends the descent is the value,
  /// which now has an end.
  fn check_const_value(
    model: Model<'_>,
    errors: &mut Vec<SchemaError>,
    names: &mut DeclaredNames,
    value: &RawValue,
    expected: PackedType,
    blame: Blame,
  ) {
    /// A container the check has opened and not finished.
    ///
    /// One entry per **open literal**, holding the borrowed iterator over what is left of it — so
    /// the peak follows the literal's nesting and never its width, and no entry is ever a copy of a
    /// child. That is the same shape [`SchemaBuilder::const_value`] builds the literal with.
    enum Level<'a> {
      /// A list literal's entries, every one of them offered to the same item type.
      Entries {
        rest: core::slice::Iter<'a, RawValue>,
        item: PackedType,
      },
      /// An input-object literal's fields, plus what the omitted-required pass will need once they
      /// run out.
      Fields {
        rest: core::slice::Iter<'a, RawObjectField>,
        base: usize,
        written: &'a [RawObjectField],
        span: SimpleSpan,
      },
    }

    /// What the innermost open level had left, read without holding a borrow on the stack.
    enum Step<'a> {
      Entry(&'a RawValue, PackedType),
      Field(&'a RawObjectField, usize),
      Close,
    }

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

    let mut levels: Vec<Level<'_>> = Vec::new();
    let mut current: Option<(&RawValue, PackedType)> = Some((value, expected));

    loop {
      let Some((value, expected)) = current.take() else {
        let step = match levels.last_mut() {
          None => return,
          Some(Level::Entries { rest, item }) => match rest.next() {
            Some(entry) => Step::Entry(entry, *item),
            None => Step::Close,
          },
          Some(Level::Fields { rest, base, .. }) => match rest.next() {
            Some(field) => Step::Field(field, *base),
            None => Step::Close,
          },
        };
        match step {
          Step::Entry(entry, item) => current = Some((entry, item)),
          Step::Field(field, base) => {
            let object = Coordinate::named(model.types[base].name.sym);
            let Some(declared) = names
              .first(model.types, base, field.name.sym)
              .map(|at| &model.types[base].input_fields[at])
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
            // through to the value check below so that `{ x: null }` for a required `x` produces
            // the obligation once, and not also a non-null coercion failure.
            if declared.is_required() && matches!(field.value.shape, RawShape::Null) {
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
            current = Some((&field.value, declared.ty.packed));
          }
          Step::Close => {
            let Some(Level::Fields {
              base,
              written,
              span,
              ..
            }) = levels.pop()
            else {
              // A list level has nothing owed when its entries run out.
              continue;
            };
            // Draft 5.6.4's SDL twin, the omitted half. Nothing was written, so the literal itself
            // is what the omission is blamed on — the same choice `check_directive_arguments` makes
            // for an omitted required argument.
            //
            // Over the required positions rather than over the declaration, and the literal indexed
            // rather than rescanned: the two factors of the same product, one bounded by what this
            // reports and the other by [`Names`] — which stays a value per list here, because what
            // it indexes is the literal's own entries and no type keys those. It is built once this
            // level's fields are all done and dropped before the next step, so no second one is
            // ever live under it. Source order is the declaration's either way, because
            // `required_input_fields` is filled by ascending position.
            let object = Coordinate::named(model.types[base].name.sym);
            let required = &model.types[base].required_input_fields;
            let of_written = Names::over(written.len(), required.len(), |at| written[at].name.sym);
            for &position in required {
              let declared = &model.types[base].input_fields[position as usize];
              if of_written
                .first(written.len(), declared.name.sym, |at| written[at].name.sym)
                .is_some()
              {
                continue;
              }
              let name = model.text(declared.name.sym).to_owned();
              push_at(
                errors,
                SchemaErrorKind::MissingRequiredInputObjectField,
                &name,
                model.owner(object),
                span,
                blame.document,
              );
            }
          }
        }
        continue;
      };

      // A subtree `const_value` refused. Every question below is a question about the literal's
      // own content, and the content is not here — so none of them has an answer, and answering
      // anyway is how this pass fabricated a defect. Terminal: the walk resumes with the siblings,
      // which ARE here, and the build refuses on the `ConstantValueTooDeep` already in the list.
      if matches!(value.shape, RawShape::Refused) {
        continue;
      }

      if matches!(value.shape, RawShape::Null) {
        if expected.is_non_null() {
          reject(errors, value.span);
        }
        continue;
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
          continue;
        };
        levels.push(Level::Entries {
          rest: entries.iter(),
          item,
        });
        continue;
      }

      let base = expected.base_id();
      if base == UNRESOLVED {
        continue;
      }
      let base = base.get() as usize;

      match model.types[base].kind {
        TypeKind::InputObject => {
          let RawShape::Object(fields) = &value.shape else {
            reject(errors, value.span);
            continue;
          };
          // The literal is named by the input object it is being offered to, not by the argument
          // that carries it: `In.y` is what apollo's `UndefinedInputValue` says and what a nested
          // literal needs, because `Query.@v.p` cannot tell an offending field of the outer object
          // from one of the inner. The span still points at the field, so the usage is one lookup
          // away. The level below carries the coordinate implicitly, as `base`.
          //
          // One index per input OBJECT, held by the caller and shared by every literal offered to
          // this type — the nested entry the level below is about to offer to a second one
          // included. A value per literal is what this was, and it was live across that recursion
          // and built again for every sibling; [`DeclaredNames`] carries what that cost.
          levels.push(Level::Fields {
            rest: fields.iter(),
            base,
            written: fields,
            span: value.span,
          });
        }
        TypeKind::Enum => {
          // One index per ENUM, shared by every literal offered to it, for the reason the input
          // object's is shared: the declaration is a function of the type, and a list literal writes
          // as many members as it likes against it. `any` over the declared values, restarted per
          // written member, is [`DeclaredNames`]'s mechanism in a spelling the sweeps that found the
          // other three did not reach — see its header.
          //
          // The written member is resolved against the NAME arena here rather than where it was
          // reduced, and that ordering is load-bearing: a literal is reduced as its definition is
          // read, so an enum value declared further down the document — or in a later one — is not
          // a symbol yet. What the literal interner holds is the spelling; what says whether it
          // names a member is this lookup, and it runs once every declaration is in.
          let member = match &value.shape {
            RawShape::Enum(spelling) => model
              .interner
              .lookup(model.literals.bytes(*spelling))
              .is_some_and(|sym| names.has_enum_value(model.types, base, sym)),
            _ => false,
          };
          if !member {
            reject(errors, value.span);
          }
        }
        TypeKind::Scalar => {
          // A custom scalar accepts every literal, so only the five built-ins have anything to say.
          let name = model.text(model.types[base].name.sym);
          let accepted = match value.shape.shape() {
            // Terminal above, so this is unreachable rather than lenient — and it is written as
            // "nothing to reject" because a refused subtree has no shape the table could weigh.
            None => true,
            Some(shape) => BuiltInScalar::from_name(name.as_bytes())
              .is_none_or(|scalar| scalar.accepts(shape, model.spelling(value.shape.spelling()))),
          };
          if !accepted {
            reject(errors, value.span);
          }
        }
        // An object, interface or union declared as an argument type is already the
        // `DirectiveArgumentTypeNotInputType` refusal; there is no value that would fit it.
        TypeKind::Object | TypeKind::Interface | TypeKind::Union => {}
      }
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
    // One `bool` per type index, marking the interfaces THIS type declares, for the transitivity
    // rule below. Same table for the whole pass and cleared by the declaration that wrote it, so a
    // schema whose types implement nothing never allocates it. See the rule's own comment.
    let mut declared_here: Vec<bool> = Vec::new();
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
      if declared_here.len() < model.types.len() {
        declared_here.resize(model.types.len(), false);
      }
      for entry in declared {
        if let Some(at) = model.type_index(entry.sym) {
          declared_here[at] = true;
        }
      }

      for entry in declared {
        let Some(interface) = model.type_index(entry.sym) else {
          continue;
        };
        if model.types[interface].kind != TypeKind::Interface {
          continue;
        }

        // Transitivity: every interface the interface implements must also be declared here.
        //
        // Read off the mark rather than by rescanning `declared`. The scan was
        // `Θ(Σ closure × declared)` with a ceiling on neither factor, and it answered on the first
        // entry only when the missing one happened to be written first: `interface J{i} implements
        // I` for `K` values of `i`, and `type T implements J0 & … & J{K-1} & I`, is `Θ(K)` of SDL
        // and `Θ(K²)` here — 1.71 in the exponent over 1 k–16 k, 1.87 over the top step and
        // **187.3 ms** at `K` = 16 000, on a schema `Schema::build` **accepts**. Writing `I` first
        // instead hides the whole product, which is why the fixture writes it last.
        let required: &[u32] = &model.types[interface].closure;
        for &needed in required {
          if needed as usize == index {
            continue;
          }
          let is_declared = declared_here[needed as usize];
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
          if own.deprecated && !interface_field.deprecated {
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

      // Exactly the slots this declaration wrote, which is `O(declared)` and not `O(types)` — the
      // same accounting [`Positions::drop`] does one line above, for the same table.
      for entry in declared {
        if let Some(at) = model.type_index(entry.sym) {
          declared_here[at] = false;
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
  /// the whole declaration once to clear its per-field marks and once again to answer —
  /// *whatever the descent was about to do*, the descent that pushes no work at all included. Its complement is the
  /// subset that decides it: `coverable` holds, per object, the fields that are not inert, so
  /// "every declared field was asked or is inert" is "every coverable field was asked", and an
  /// object with none of them is canonical for nothing having been asked. That is what makes the
  /// no-map descent `O(1)` — a value unwrapping to no map node runs the draft's "for each field in
  /// `inputObject`" zero times, so it asks nothing, and it covers `target` exactly when `target`
  /// has nothing to cover. The marks are stamped with the descent that wrote them rather than
  /// cleared for it, so the other width-proportional pass goes too.
  ///
  /// # A frame's WORK is a second population, and narrowing it twice did not remove the product
  ///
  /// Two revisions tried. The first recognised a descent into a *single* empty map and built the
  /// work list over the whole declaration everywhere else. The second built it over `descendable`
  /// — `coverable` without the "carries a default" clause — and coalesced the empty map nodes,
  /// which between them made `[{} × M]` of a `D`-field object of nullable scalars cost one entry
  /// instead of `M × D`. Each was correct about the fixture in front of it, and each left the
  /// product standing one literal away, because **each narrowed a population that a literal
  /// writing something walks straight past**: `input Outer { w: [Wide] = [{f0: null} … ×M] }` in
  /// front of `input Wide { f0: Leaf = {} … fD: Leaf = {} }` over an acyclic `input Leaf { x: Int }`.
  ///
  /// No node is empty, so nothing coalesces; every one of `Wide`'s `D` fields names an input
  /// object and carries a default, so `descendable` and `coverable` are the whole declaration;
  /// and `f0` is supplied by every node, so `Wide` never settles. `Θ(M × D)` work entries on
  /// `O(M + D)` of SDL `finish` ACCEPTS, out of repeating one single-field map — and widening the
  /// predicate again would fix this fixture and leave the next one.
  ///
  /// So the loop is **inverted** rather than narrowed a third time. A frame's work is built from
  /// what the caller WROTE: one entry per map entry naming a descendable field — a total the
  /// document bounds, because every one of those entries was written once in the SDL — plus one
  /// entry per coverable field that some node left out, which is `Θ(coverable)` for the whole
  /// descent and not per node. A declared field is never visited once per map node, so there is
  /// no pair of nested loops left for a fixture to multiply, and that is a property a later
  /// reader checks by *looking at the loop* instead of by finding the literal that gets through
  /// it. The comment at the loop states it.
  ///
  /// The multiplicity goes with the nesting. `(field, None)` means "descend into this field's own
  /// default", which does not depend on WHICH node omitted it, so `M` omissions were `M` byte-
  /// identical entries and are now one — the argument the empty-node coalescing made, made where
  /// it belongs. The coalescing itself is gone: a node with no entries now contributes nothing to
  /// enumerate, so there is nothing left for a special case to skip.
  ///
  /// **The settling rule's meaning is unchanged, and so is how it is decided.** It is still
  /// "every coverable field of `target` was asked with nothing supplied at least once", and
  /// asked-with-nothing-supplied is still exactly "some map node of this level omitted it". What
  /// changed is which side of that is counted. Scanning the declaration per node stamped the
  /// fields a node OMITTED; enumerating entries stamps the fields a node SUPPLIED, and a field is
  /// asked when the nodes that supplied it are short of the nodes there are. No node can supply a
  /// field twice — an entry repeated inside one node is one supply, which is what reading the
  /// *first* occurrence meant — so `supplied < maps.len()` and "some node omitted it" are the
  /// same statement, and `canonical` is that statement over `coverable`.
  ///
  /// Resolving a written name through [`DeclaredNames`] resolves it to the FIRST declaration
  /// carrying it, which is what [`SchemaBuilder::check_const_value`] does with the same literal.
  /// The direction that is gone gave every declaration of a repeated name the same written value;
  /// a type declaring one input field `D` times under `M` nodes is that product again, and such a
  /// type is a `DuplicateInputFieldName` refusal in every case, so no schema changes verdict over
  /// it.
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
      /// `(field index in `object`, the value the caller supplied for it)` — the draft's "for
      /// each field in inputObject", built from the two sides of that question separately rather
      /// than by crossing the declaration with the map nodes.
      ///
      /// One entry per map ENTRY naming a field that can descend, which the document bounds
      /// because each was written once in the SDL; and one entry per coverable field that some
      /// node left out, which is one per FIELD rather than one per (node, field), because
      /// descending into a field's own default does not depend on which node omitted it. Nothing
      /// here is a count of pairs. The header says what counting pairs cost.
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

    let (model, errors, names) = self.split_indexed();
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
    // every descent cleared and resized a per-field mark to the target's full declared width and
    // then scanned every declaration, EVEN WHEN `map_nodes` produced no map at all and the frame it
    // pushed had no work in it. `N` input types with a valid `w: [Wide] = []` in front of a
    // `D`-field `Wide` is `Θ(N + D)` of SDL `Schema::build` ACCEPTS: an empty list unwraps to no
    // map, so it can settle nothing and repeats both `D`-wide passes `N` times. 1.90 in the
    // exponent over 16 k–128 k and 34.9 s at 128 k. al8n/smear#198.
    //
    // It is also what a descent's WORK is built over, on the side a literal does not write: a
    // coverable field no node supplied is asked, and asked with nothing supplied is the descent
    // into that field's own default. The side a literal *does* write is enumerated from the
    // literal, so no second subset of the declaration is derived for it — a field whose named
    // type is not an input object is turned away by an `O(1)` test on the entry that named it,
    // not by being kept out of a list built in advance.
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
    // Which input field each map node SUPPLIED, addressed by the same dense id `on_path` uses.
    // `supplied_in` is the node that last wrote the field and `supplied_by` how many nodes of the
    // descent now running have, and the pair answers both questions one pass over the entries can
    // raise: an entry repeated inside one node is one supply, and a field every node wrote was
    // asked by none of them.
    //
    // STAMPED rather than cleared, for the reason the answer is read over `coverable` at all: that
    // subset may be empty while the declaration is wide, so a clear proportional to the
    // declaration would be a width-proportional pass in front of a descent that reads none of it.
    // Node stamps are global and strictly increasing, and a descent's nodes are the contiguous run
    // that begins where its map loop begins, so `supplied_in[id] >= since` is "this descent", with
    // no second counter to keep in step. `u64` because a stamp that wraps is a stamp that answers
    // for a node that did not write, and no walk can make 2^64 of them; zero is therefore never a
    // live stamp, which is what lets the whole array start there.
    let mut supplied_in: Vec<u64> = vec![0; total_fields];
    let mut supplied_by: Vec<usize> = vec![0; total_fields];
    let mut node: u64 = 0;

    for start in 0..count {
      if model.types[start].kind != TypeKind::InputObject || implicated[start] || settled[start] {
        continue;
      }
      // The draft's top-level call: `defaultValue` is an empty map, so every field is asked, and
      // none of them is supplied a value. `canonical` is therefore true by construction and does
      // not depend on the work list — which is why this frame carries only the entries that
      // descend. Asked with nothing supplied descends into the field's own default, so the ones
      // that do are exactly `coverable`, and the rest are the declaration's whole width popped and
      // `continue`d past. The descent below reaches the same list from the other side: an empty
      // map writes no entry, so every coverable field is one no node supplied.
      let mut stack = vec![Frame {
        pushed: None,
        work: coverable[coverable_base[start]..coverable_base[start + 1]]
          .iter()
          .map(|&field| (field as usize, None))
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
          // **What the two loops below enumerate is every entry these map nodes WROTE, once each,
          // and every coverable field of `target`, once for the whole descent — and never a
          // declared field once per map node.** Neither loop is inside the other, so there is no
          // pair for a literal to multiply: the header's three fixtures each got through a
          // narrower predicate, and a predicate is not what stops this one.
          let since = node + 1;
          for &map in &maps {
            node += 1;
            for entry in map {
              // Draft 5.6.2's direction, and [`SchemaBuilder::check_const_value`]'s: the written
              // name is resolved against the declaration, through the one index per type that
              // every literal offered to `target` shares. An entry naming nothing `target`
              // declares is invisible to this rule, exactly as it was when the declaration did
              // the asking.
              let Some(index) = names.first(model.types, target, entry.name.sym) else {
                continue;
              };
              // A field whose named type is not an input object can hold no cycle whatever this
              // entry says about it, so it is turned away here rather than kept out of a subset
              // built in advance — and turning it away costs one test on an entry that exists,
              // not one per declared field per node.
              let base = declared[index].ty.packed.base_id();
              if base == UNRESOLVED
                || model.types[base.get() as usize].kind != TypeKind::InputObject
              {
                continue;
              }
              let id = field_base[target] + index;
              // Reading the first occurrence, written as a mark rather than as a lookup: a node
              // that writes one name twice supplies it once, which is the answer the sorted pairs
              // gave and the one a repeated entry needs, because descending into the second copy
              // as well would explore a literal the draft's `value[field]` never selects.
              if supplied_in[id] == node {
                continue;
              }
              supplied_by[id] = if supplied_in[id] >= since {
                supplied_by[id] + 1
              } else {
                1
              };
              supplied_in[id] = node;
              // The caller's literal named this field, so the walk descends into that literal and
              // the field's own default is never consulted.
              work.push((index, Some(&entry.value)));
            }
          }
          // The condition stated above, read off the supply marks — and the descents it licenses,
          // in the same pass. A coverable field short of a supply from every node was omitted by
          // one of them, which is the ask that makes the walk descend into that field's OWN
          // default; a field every node wrote was asked by none of them, and is what stops this
          // frame from covering `InputObjectDefaultValueHasCycle(target, {})`.
          let mut canonical = true;
          for &at in coverable_here {
            let index = at as usize;
            let id = field_base[target] + index;
            let supplied = if supplied_in[id] >= since {
              supplied_by[id]
            } else {
              0
            };
            if supplied == maps.len() {
              canonical = false;
            } else {
              work.push((index, None));
            }
          }
          canonical
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

    // Every interface's implementors, from the closures that name them, in one pass over the
    // objects.
    //
    // Draft §3.7 states the set the other way round — an interface's possible objects are the
    // objects that declare it — and the loop below stated it that way too: for each interface,
    // every type in the schema, and for each object among them a scan of its closure. That is
    // `Θ(interfaces × types × closure)` with a ceiling on none of the three, and it runs in
    // `flatten`, which is reached only when the build has **accepted** the document. `N`
    // implementor-less interfaces beside `N` objects is `Θ(N)` of SDL and was `Θ(N²)` here: 1.87
    // in the exponent over 500–4 000 and **55.4 ms** at `N` = 4 000, against 1.00 and 3.6 ms.
    //
    // A counting sort rather than a `Vec` per interface, because one row per interface is one
    // allocation per interface where this is two for the schema, and because the rows are read
    // once each in the loop below. The membership question disappears with the direction: an
    // object's closure names the interfaces it implements, so walking it emits exactly the pairs
    // the scan was looking for and no pair it was not. Bit order within a word is not a
    // representation: `set_bit` is idempotent and the bitset is a set. al8n/smear#198.
    let mut implementor_base: Vec<u32> = vec![0; raw_types.len() + 1];
    for raw in &raw_types {
      if raw.kind == TypeKind::Object {
        for &interface in &raw.closure {
          implementor_base[interface as usize + 1] += 1;
        }
      }
    }
    for at in 0..raw_types.len() {
      implementor_base[at + 1] += implementor_base[at];
    }
    let mut implementors: Vec<u32> = vec![0; implementor_base[raw_types.len()] as usize];
    {
      let mut cursor = implementor_base.clone();
      for (index, raw) in raw_types.iter().enumerate() {
        if raw.kind == TypeKind::Object {
          for &interface in &raw.closure {
            implementors[cursor[interface as usize] as usize] = ordinal_of[index];
            cursor[interface as usize] += 1;
          }
        }
      }
    }

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
            let row = implementor_base[index] as usize..implementor_base[index + 1] as usize;
            for &ordinal in &implementors[row] {
              set_bit(&mut possible, start, ordinal);
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
      refused: _,
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
/// It recursed, on the line [`SchemaBuilder::const_value`] used to draw — over a *literal*, whose
/// depth the parser has already bounded, rather than over the type graph, whose depth it has not.
/// **The parser is not the only thing that builds a literal**, and `const_value`'s own header
/// derives the public route that builds one at any depth. The list nesting is walked with an
/// explicit stack of borrowed iterators instead: one entry per open list, never one per item, and
/// document order preserved because each level is drained before the level under it is resumed.
/// That stack is bounded the same way the other two literal walks are — by
/// [`MAX_CONST_VALUE_DEPTH`], applied where the literal is built.
fn map_nodes<'a>(value: &'a RawValue, out: &mut Vec<&'a [RawObjectField]>) {
  let mut rest: Vec<core::slice::Iter<'a, RawValue>> = Vec::new();
  let mut current = Some(value);
  loop {
    let Some(value) = current.take() else {
      match rest.last_mut() {
        None => return,
        Some(items) => match items.next() {
          Some(item) => current = Some(item),
          None => {
            rest.pop();
          }
        },
      }
      continue;
    };
    // Exhaustive rather than `_`, so that a shape added later has to be decided here instead of
    // defaulting to "contributes nothing" — which is the right answer for every scalar and the
    // wrong one for anything that can hold a field.
    match &value.shape {
      RawShape::List(items) => rest.push(items.iter()),
      RawShape::Object(fields) => out.push(fields),
      // A refused subtree contributes NO map node, and an empty one is not the same answer: a map
      // node is the claim "these are the fields this node supplies", so an empty one says every
      // field was omitted — which is exactly the ask that makes the caller descend into a field's
      // own `defaultValue` and mark it on the path. Contributing none says nothing at all about
      // content that was never read.
      RawShape::Refused => {}
      RawShape::Null
      | RawShape::Boolean
      | RawShape::Int(_)
      | RawShape::Float(_)
      | RawShape::String
      | RawShape::Enum(_) => {}
    }
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

#[cfg(test)]
mod tests;

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
