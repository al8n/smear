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

#[derive(Debug, Clone)]
struct RawField {
  name: Located,
  ty: RawTypeRef,
  args: Vec<RawInput>,
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
    }
  }
}

#[derive(Debug, Clone)]
struct RawDirectiveDef {
  name: Located,
  args: Vec<RawInput>,
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

  fn directive_index(&self, sym: Sym) -> Option<usize> {
    match self.directive_of_sym.get(sym.get() as usize) {
      Some(&index) if index != u32::MAX => Some(index as usize),
      _ => None,
    }
  }

  fn arguments(&self, at: ArgumentsOf) -> &'a [RawInput] {
    match at {
      ArgumentsOf::Field { ty, field } => &self.types[ty].fields[field].args,
      ArgumentsOf::Directive { index } => &self.directives[index].args,
    }
  }

  fn directive_uses(&self, at: DirectivesOf) -> &'a [RawDirectiveUse] {
    match at {
      DirectivesOf::Schema => self.schema_directives,
      DirectivesOf::Type { ty } => &self.types[ty].directives,
      DirectivesOf::Field { ty, field } => &self.types[ty].fields[field].directives,
      DirectivesOf::FieldArgument { ty, field, arg } => {
        &self.types[ty].fields[field].args[arg].directives
      }
      DirectivesOf::InputField { ty, field } => &self.types[ty].input_fields[field].directives,
      DirectivesOf::EnumValue { ty, value } => &self.types[ty].enum_values[value].directives,
      DirectivesOf::DirectiveArgument { directive, arg } => {
        &self.directives[directive].args[arg].directives
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
    (
      Model {
        types: &self.types,
        directives: &self.directives,
        directive_of_sym: &self.directive_of_sym,
        schema_directives: &self.schema_directives,
        interner: &self.interner,
      },
      &mut self.errors,
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
    match self.type_of_sym.get(sym.get() as usize) {
      Some(&index) if index != u32::MAX => Some(index as usize),
      _ => None,
    }
  }

  fn set_type_index(&mut self, sym: Sym, index: u32) {
    let slot = sym.get() as usize;
    if self.type_of_sym.len() <= slot {
      self.type_of_sym.resize(slot + 1, u32::MAX);
    }
    self.type_of_sym[slot] = index;
  }

  fn directive_index(&self, sym: Sym) -> Option<usize> {
    match self.directive_of_sym.get(sym.get() as usize) {
      Some(&index) if index != u32::MAX => Some(index as usize),
      _ => None,
    }
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
      args,
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
          args,
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
        for arg in 0..self.types[index].fields[field].args.len() {
          let path = path.then(self.types[index].fields[field].args[arg].name.sym);
          Self::resolve_one(
            &self.type_of_sym,
            &mut self.types[index].fields[field].args[arg].ty,
            path,
            &mut unresolved,
            &mut too_deep,
          );
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
      for arg in 0..self.directives[index].args.len() {
        let path = owner.then(self.directives[index].args[arg].name.sym);
        Self::resolve_one(
          &self.type_of_sym,
          &mut self.directives[index].args[arg].ty,
          path,
          &mut unresolved,
          &mut too_deep,
        );
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

  /// Whether `@deprecated` is applied in this directive list.
  fn is_deprecated(&self, directives: &[RawDirectiveUse]) -> bool {
    directives
      .iter()
      .any(|used| self.text(used.name.sym) == DEPRECATED)
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
      // Before the argument walk below, and INSTEAD of it. Ordering alone was the first shape of
      // this check and it settled only the diagnostics: a field a thousand arguments wide would
      // otherwise report a thousand argument diagnostics ahead of the one that says why the field
      // itself is refused. But the refused field still paid `validate_arguments`, whose duplicate
      // scan compares each argument against every argument before it — `Θ(declared²)` — so the
      // ceiling stood in front of the work it names without gating it, and an oversized list
      // bought the whole scan on its way to being refused. A bound whose own refusal path is
      // unbounded bounds nothing an adversary has to respect. Not performing the argument checks
      // is the same argument the ordering was already making, carried to its conclusion: the
      // arguments are not why the field is refused, and a field that will not build has no
      // argument diagnostics worth the walk. al8n/smear#198.
      let declared = self.types[index].fields[field].args.len();
      if declared > MAX_FIELD_ARGUMENTS as usize {
        let name = self.text(at.sym).to_owned();
        let owner = self.owner(owner);
        self.push_owned(SchemaErrorKind::TooManyFieldArguments, &name, owner, at);
        continue;
      }

      let built_in = self.types[index].built_in;
      self.validate_arguments(
        ArgumentsOf::Field { ty: index, field },
        path,
        built_in,
        SchemaErrorKind::DuplicateArgumentName,
        SchemaErrorKind::ReservedArgumentName,
        SchemaErrorKind::ArgumentTypeNotInputType,
      );
    }
  }

  /// One input-value list, addressed rather than borrowed.
  ///
  /// `&self.types[…].args` cannot be held across a `self.push*`, and the list is walked far more
  /// often than it is reported on — every argument of every field of every type, built-ins
  /// included — so it is re-addressed per item rather than copied once per list. See [`Model`].
  fn arguments(&self, at: ArgumentsOf) -> &[RawInput] {
    match at {
      ArgumentsOf::Field { ty, field } => &self.types[ty].fields[field].args,
      ArgumentsOf::Directive { index } => &self.directives[index].args,
    }
  }

  fn validate_arguments(
    &mut self,
    args: ArgumentsOf,
    owner: Coordinate,
    built_in: bool,
    duplicate: SchemaErrorKind,
    reserved: SchemaErrorKind,
    not_input: SchemaErrorKind,
  ) {
    // Both callers refuse past their ceiling before reaching here, so this list is never wider
    // than sixty-four and `Duplicates` is always the scan the previous round decided to keep.
    // Written through the shared type all the same: a copy of the shape kept because a ceiling
    // happens to hold this one list today is the copy the next ceiling change re-opens.
    let mut seen = Duplicates::over(self.arguments(args).len(), |at| {
      self.arguments(args)[at].name.sym
    });
    for index in 0..self.arguments(args).len() {
      let arg = &self.arguments(args)[index];
      let at = arg.name;
      let ty = arg.ty;
      let required = arg.is_required();

      if let Some(earlier) = seen.first(index, at.sym) {
        let first = self.arguments(args)[earlier].name.span;
        let name = self.text(at.sym).to_owned();
        let owner = self.owner(owner);
        self.push_related(duplicate, &name, Some(owner), at, first);
      }

      if !built_in && is_reserved(self.text(at.sym).as_bytes()) {
        let name = self.text(at.sym).to_owned();
        let owner = self.owner(owner);
        self.push_owned(reserved, &name, owner, at);
      }

      let base = ty.packed.base_id();
      if base != UNRESOLVED && !self.types[base.get() as usize].kind.is_input() {
        let subject = self
          .text(self.types[base.get() as usize].name.sym)
          .to_owned();
        let path = self.owner(owner.then(at.sym));
        let mut where_ = at;
        where_.span = ty.span;
        self.push_owned(not_input, &subject, path, where_);
      }

      // Draft §3.6.1(2.4.4.1): "if argument type is Non-Null and a default value is not defined,
      // the `@deprecated` directive must not be applied to this argument" — which is exactly
      // `is_required`.
      if required && self.is_deprecated(&self.arguments(args)[index].directives) {
        let name = self.text(at.sym).to_owned();
        let owner = self.owner(owner);
        self.push_owned(
          SchemaErrorKind::DeprecatedRequiredArgument,
          &name,
          owner,
          at,
        );
      }

      // Draft §3.6.1(2.4.5): "if the argument has a default value it must be compatible with
      // `argumentType` as per the coercion rules for that type" — the same coercion procedure a
      // directive argument's *supplied* value goes through, so the two cannot answer differently.
      if self.arguments(args)[index].default_value.is_some() {
        let (model, errors) = self.split();
        let default = model.arguments(args)[index]
          .default_value
          .as_ref()
          .expect("just observed to be present");
        Self::check_const_value(
          model,
          errors,
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
      if required && self.is_deprecated(&self.types[index].input_fields[position].directives) {
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
        let (model, errors) = self.split();
        let default = model.types[index].input_fields[position]
          .default_value
          .as_ref()
          .expect("just observed to be present");
        Self::check_const_value(
          model,
          errors,
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

      // The second population of the same list, refused the same way and before the same walk.
      // A directive definition's arguments go through `validate_arguments` exactly as a field's
      // do, so they carry the same `Θ(declared²)` duplicate scan; and the width is read again at
      // every usage a document writes, which is the product `MAX_DIRECTIVE_ARGUMENTS` states.
      // The field ceiling did not reach here — one constant enforced at one site — so bounding
      // only the field path would have left the identical walk open on a list written by the
      // same party for the same kind of reason. al8n/smear#198.
      let declared = self.directives[index].args.len();
      if declared > MAX_DIRECTIVE_ARGUMENTS as usize {
        let name = self.text(at.sym).to_owned();
        self.push(SchemaErrorKind::TooManyDirectiveArguments, &name, at);
        continue;
      }

      self.validate_arguments(
        ArgumentsOf::Directive { index },
        Coordinate::named(at.sym),
        built_in,
        SchemaErrorKind::DuplicateDirectiveArgumentName,
        SchemaErrorKind::ReservedDirectiveArgumentName,
        SchemaErrorKind::DirectiveArgumentTypeNotInputType,
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
    let (model, errors) = self.split();
    Self::check_directive_uses(model, errors, DirectivesOf::Schema, Coordinate::schema());

    for ty in 0..model.types.len() {
      let owner = Coordinate::named(model.types[ty].name.sym);
      Self::check_directive_uses(model, errors, DirectivesOf::Type { ty }, owner);

      for field in 0..model.types[ty].fields.len() {
        let path = owner.then(model.types[ty].fields[field].name.sym);
        Self::check_directive_uses(model, errors, DirectivesOf::Field { ty, field }, path);

        for arg in 0..model.types[ty].fields[field].args.len() {
          let path = path.then(model.types[ty].fields[field].args[arg].name.sym);
          Self::check_directive_uses(
            model,
            errors,
            DirectivesOf::FieldArgument { ty, field, arg },
            path,
          );
        }
      }

      for field in 0..model.types[ty].input_fields.len() {
        let path = owner.then(model.types[ty].input_fields[field].name.sym);
        Self::check_directive_uses(model, errors, DirectivesOf::InputField { ty, field }, path);
      }

      for value in 0..model.types[ty].enum_values.len() {
        let path = owner.then(model.types[ty].enum_values[value].name.sym);
        Self::check_directive_uses(model, errors, DirectivesOf::EnumValue { ty, value }, path);
      }
    }

    for directive in 0..model.directives.len() {
      let owner = Coordinate::named(model.directives[directive].name.sym);
      for arg in 0..model.directives[directive].args.len() {
        let path = owner.then(model.directives[directive].args[arg].name.sym);
        Self::check_directive_uses(
          model,
          errors,
          DirectivesOf::DirectiveArgument { directive, arg },
          path,
        );
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

      Self::check_directive_arguments(model, errors, at, index, definition, owner);
    }
  }

  /// One usage's arguments, against the ones its definition declares.
  fn check_directive_arguments(
    model: Model<'_>,
    errors: &mut Vec<SchemaError>,
    at: DirectivesOf,
    index: usize,
    definition: usize,
    owner: Coordinate,
  ) {
    let used = &model.directive_uses(at)[index];
    // Rendered only where one is reported. `check_directive_arguments` runs for every well-formed
    // usage too, and a supergraph has thousands of them.
    let coordinate = owner.at_directive(used.name.sym);
    let declared = &model.directives[definition].args;

    // Draft 5.4.2's SDL twin. Unlike the repeatability rule above, this one needs nothing from the
    // definition — an argument written twice is a mistake whether or not the directive declares it
    // — so it is checked for every written argument, including one that is about to be reported
    // as undefined.
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
        Self::check_const_value(model, errors, entry, item, blame);
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
        for field in fields {
          let Some(expected) = declared.iter().find(|d| d.name.sym == field.name.sym) else {
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
          Self::check_const_value(model, errors, &field.value, expected.ty.packed, blame);
        }

        // Draft 5.6.4's SDL twin, the omitted half. Nothing was written, so the literal itself is
        // what the omission is blamed on — the same choice `check_directive_arguments` makes for
        // an omitted required argument.
        for expected in declared {
          if !expected.is_required() {
            continue;
          }
          if fields
            .iter()
            .any(|field| field.name.sym == expected.name.sym)
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
  fn validate_interface_implementations(&mut self) {
    for index in 0..self.types.len() {
      if !matches!(
        self.types[index].kind,
        TypeKind::Object | TypeKind::Interface
      ) {
        continue;
      }
      // Nothing below has anything to say about a type that implements nothing, and every
      // introspection type is one — so the owner name and the copy of the list are built after
      // the question is asked rather than before it.
      if self.types[index].implements.is_empty() {
        continue;
      }
      let owner = self.text(self.types[index].name.sym).to_owned();
      let declared: Vec<Located> = self.types[index].implements.clone();

      for entry in &declared {
        let Some(interface) = self.type_index(entry.sym) else {
          continue;
        };
        if self.types[interface].kind != TypeKind::Interface {
          continue;
        }

        // Transitivity: every interface the interface implements must also be declared here.
        let required: Vec<u32> = self.types[interface].closure.clone();
        for needed in required {
          if needed as usize == index {
            continue;
          }
          let is_declared = declared
            .iter()
            .any(|d| self.type_index(d.sym) == Some(needed as usize));
          if !is_declared {
            let subject = self.text(self.types[needed as usize].name.sym).to_owned();
            self.push_owned(
              SchemaErrorKind::MissingTransitiveInterface,
              &subject,
              owner.clone(),
              *entry,
            );
          }
        }

        // Field coverage, covariance, and argument invariance.
        let interface_fields = self.types[interface].fields.clone();
        for interface_field in &interface_fields {
          let field_name = self.text(interface_field.name.sym).to_owned();
          let Some(position) = self.types[index]
            .fields
            .iter()
            .position(|f| f.name.sym == interface_field.name.sym)
          else {
            self.push_owned(
              SchemaErrorKind::MissingInterfaceField,
              &field_name,
              owner.clone(),
              *entry,
            );
            continue;
          };
          let own = self.types[index].fields[position].clone();

          if !self.is_valid_implementation_type(own.ty.packed, interface_field.ty.packed) {
            let path = owner_path(&[&owner, &field_name]);
            let expected = self.render_type(interface_field.ty.packed);
            self.push_owned(
              SchemaErrorKind::InvalidInterfaceFieldType,
              &expected,
              path,
              own.name,
            );
          }

          // `IsValidImplementation` 2.6: "if `field` is deprecated then `implementedField` must
          // also be deprecated". The span is the implementing field, which is where the edit goes;
          // `related` points at the interface field, which is the other half of the obligation.
          if self.is_deprecated(&own.directives) && !self.is_deprecated(&interface_field.directives)
          {
            self.push_related(
              SchemaErrorKind::InterfaceFieldNotDeprecated,
              &field_name,
              Some(owner.clone()),
              own.name,
              interface_field.name.span,
            );
          }

          for interface_arg in &interface_field.args {
            let arg_name = self.text(interface_arg.name.sym).to_owned();
            match own
              .args
              .iter()
              .find(|a| a.name.sym == interface_arg.name.sym)
            {
              None => {
                let path = owner_path(&[&owner, &field_name]);
                self.push_owned(
                  SchemaErrorKind::MissingInterfaceFieldArgument,
                  &arg_name,
                  path,
                  own.name,
                );
              }
              Some(own_arg) => {
                if own_arg.ty.packed != interface_arg.ty.packed {
                  let path = owner_path(&[&owner, &field_name, &arg_name]);
                  let expected = self.render_type(interface_arg.ty.packed);
                  self.push_owned(
                    SchemaErrorKind::InvalidInterfaceFieldArgumentType,
                    &expected,
                    path,
                    own_arg.name,
                  );
                }
              }
            }
          }

          for own_arg in &own.args {
            let declared_by_interface = interface_field
              .args
              .iter()
              .any(|a| a.name.sym == own_arg.name.sym);
            if declared_by_interface {
              continue;
            }
            if own_arg.ty.packed.is_non_null() && !own_arg.default.is_present() {
              let arg_name = self.text(own_arg.name.sym).to_owned();
              let path = owner_path(&[&owner, &field_name]);
              self.push_owned(
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
  /// # Settling, and why it is sound
  ///
  /// A frame retires only when its whole sub-exploration finished, and nothing prunes that
  /// exploration except the cycle test itself — which does not prune, it `break`s the entire walk.
  /// So a retired frame's object has had every path below it followed to the end with no repeat,
  /// which is to say **no cycle is reachable from it at all**, whatever `visited` it was reached
  /// with. Starting a fresh walk there would re-derive that at the cost of the whole subtree over
  /// again, so it is skipped. Without it a chain of `N` defaulted input objects costs `O(N²)`:
  /// `a_deep_defaulted_input_chain_does_not_recurse` is twenty thousand links long and takes
  /// **41.5 s** with the skip removed against **under one second** with it — measured, not
  /// estimated. It is the standing guard on this and on the iterative shape both.
  fn validate_input_object_default_cycles(&mut self) {
    /// One level of `InputObjectDefaultValueHasCycle`: an input object, and the work its
    /// `defaultValue` produced.
    struct Frame {
      /// The field of the enclosing object whose *own* default this frame descended into, if that
      /// is why it exists. Popped off the path when the frame retires.
      pushed: Option<usize>,
      /// `(field index in `object`, the value the caller supplied for it)`, one entry per
      /// (map node, field) pair — which is exactly the draft's "for each field in inputObject",
      /// run once per map node the level's value unwraps to.
      work: Vec<(usize, Option<RawValue>)>,
      cursor: usize,
      /// The object whose fields `work` indexes, so a frame can name the field it blames.
      object: usize,
    }

    let count = self.types.len();
    // A dense id per input field, so path membership is a bit rather than a search.
    let mut field_base: Vec<usize> = vec![0; count + 1];
    for index in 0..count {
      field_base[index + 1] = field_base[index] + self.types[index].input_fields.len();
    }
    let total_fields = field_base[count];

    let mut implicated = vec![false; count];
    // Objects a completed walk proved clean; see the header for why that is transitive.
    let mut settled = vec![false; count];
    let mut on_path = vec![false; total_fields];

    for start in 0..count {
      if self.types[start].kind != TypeKind::InputObject || implicated[start] || settled[start] {
        continue;
      }
      // The draft's top-level call: `defaultValue` is an empty map, so every field is asked, and
      // none of them is supplied a value.
      let mut stack = vec![Frame {
        pushed: None,
        work: (0..self.types[start].input_fields.len())
          .map(|field| (field, None))
          .collect(),
        cursor: 0,
        object: start,
      }];

      let mut found: Option<(usize, usize)> = None;
      while let Some(frame) = stack.last_mut() {
        let Some((field_index, supplied)) = frame.work.get(frame.cursor).cloned() else {
          if let Some(id) = frame.pushed {
            on_path[id] = false;
          }
          settled[frame.object] = true;
          stack.pop();
          continue;
        };
        frame.cursor += 1;
        let object = frame.object;

        // `InputFieldDefaultValueHasCycle`. A field whose named type is not an input object can
        // hold no cycle, whatever its default says.
        let field = &self.types[object].input_fields[field_index];
        let base = field.ty.packed.base_id();
        if base == UNRESOLVED {
          continue;
        }
        let target = base.get() as usize;
        if self.types[target].kind != TypeKind::InputObject {
          continue;
        }

        let (descend_into, pushed) = match supplied {
          // The caller's literal named this field, so the field's own default is never consulted
          // and `visited` does not grow.
          Some(value) => (value, None),
          None => {
            let Some(default) = field.default_value.clone() else {
              continue;
            };
            let id = field_base[object] + field_index;
            if on_path[id] {
              found = Some((object, field_index));
              break;
            }
            on_path[id] = true;
            (default, Some(id))
          }
        };

        let declared = &self.types[target].input_fields;
        let mut maps: Vec<&[RawObjectField]> = Vec::new();
        map_nodes(&descend_into, &mut maps);
        let mut work = Vec::new();
        for map in &maps {
          for (index, declared_field) in declared.iter().enumerate() {
            let supplied = map
              .iter()
              .find(|entry| entry.name.sym == declared_field.name.sym)
              .map(|entry| entry.value.clone());
            work.push((index, supplied));
          }
        }
        stack.push(Frame {
          pushed,
          work,
          cursor: 0,
          object: target,
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
      let owner = self.text(self.types[object].name.sym).to_owned();
      let at = self.types[object].input_fields[field_index].name;
      let name = self.text(at.sym).to_owned();
      self.push_owned(
        SchemaErrorKind::InputObjectDefaultValueCycle,
        &name,
        owner,
        at,
      );
    }
  }

  /// Draft §3.13.1: a directive definition must not refer to itself, directly or indirectly.
  ///
  /// One reachability walk per directive, over two interleaved worklists — directives to expand
  /// and types to expand — with a visited bit each. Iterative on purpose: the type graph's depth
  /// is bounded by nothing but the document, and a recursive walk would put an SDL's input-object
  /// chain on the call stack.
  ///
  /// The cycle is only reported on the directive the walk *started* from. A directive `@b` that
  /// merely uses a self-referential `@a` is not itself self-referential, and blaming it would put
  /// the diagnostic on the wrong definition — the same refinement apollo-compiler makes.
  fn validate_directive_cycles(&mut self) {
    let directives = self.directives.len();
    if directives == 0 {
      return;
    }
    let types = self.types.len();
    let mut cyclic: Vec<usize> = Vec::new();

    for start in 0..directives {
      let mut seen_directives = vec![false; directives];
      let mut seen_types = vec![false; types];
      let mut pending_directives: Vec<usize> = Vec::new();
      let mut pending_types: Vec<usize> = Vec::new();

      // Seed with what the definition itself names: the directives applied to its arguments, and
      // the types those arguments accept.
      for arg in &self.directives[start].args {
        self.push_directive_uses(&arg.directives, &mut pending_directives);
        self.push_type(arg.ty.packed.base_id(), &mut pending_types);
      }

      let mut found = false;
      while !found {
        if let Some(next) = pending_directives.pop() {
          if next == start {
            found = true;
            break;
          }
          if seen_directives[next] {
            continue;
          }
          seen_directives[next] = true;
          for arg in &self.directives[next].args {
            self.push_directive_uses(&arg.directives, &mut pending_directives);
            self.push_type(arg.ty.packed.base_id(), &mut pending_types);
          }
          continue;
        }
        let Some(next) = pending_types.pop() else {
          break;
        };
        if seen_types[next] {
          continue;
        }
        seen_types[next] = true;
        let raw = &self.types[next];
        self.push_directive_uses(&raw.directives, &mut pending_directives);
        for field in &raw.input_fields {
          self.push_directive_uses(&field.directives, &mut pending_directives);
          self.push_type(field.ty.packed.base_id(), &mut pending_types);
        }
        for value in &raw.enum_values {
          self.push_directive_uses(&value.directives, &mut pending_directives);
        }
      }

      if found {
        cyclic.push(start);
      }
    }

    for index in cyclic {
      let at = self.directives[index].name;
      let name = self.text(at.sym).to_owned();
      self.push(SchemaErrorKind::SelfReferentialDirective, &name, at);
    }
  }

  fn push_directive_uses(&self, uses: &[RawDirectiveUse], pending: &mut Vec<usize>) {
    for used in uses {
      if let Some(index) = self.directive_index(used.name.sym) {
        pending.push(index);
      }
    }
  }

  fn push_type(&self, base: TypeId, pending: &mut Vec<usize>) {
    if base != UNRESOLVED {
      pending.push(base.get() as usize);
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
            let args_start = inputs.len() as u32;
            let mut args: Vec<InputValueDef> = field
              .args
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
      let args_start = inputs.len() as u32;
      let mut args: Vec<InputValueDef> = raw
        .args
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
