//! Coercion: the phase after instantiation, which makes the model's values of the values lowering
//! read, each by the type expected where it stands.
//!
//! It reads no document: it reads the trees lowering answered, the rows written so far, through
//! the builder, and the sources' texts, and it is the one writer of the model's values. Its
//! built-ins are found once, each by its name in its package's root module and never by a spelling
//! a program writes: `core`'s `NonZero`, `TypeRef`, `@oneOf`, `@set` and `@map`, and `alloc`'s
//! `string`, `id` and `bytes`; a primitive is told by its definition, which `core` materialises.
//!
//! # The walk
//!
//! A value is appended when the model's walk reaches it, and never before, so a value's id is its
//! place in that walk: each definition in id order, its applications, every contribution's in
//! order, then each of its direct members in id order, right after each its arguments; a member's
//! default, then its applications; and last the schema's applications. Within a value, a value
//! comes before what it holds, a list before its elements, left to right. An application's
//! arguments are appended all at once, in the order of its directive's arguments.
//!
//! # What a value becomes
//!
//! Each node of a tree is coerced against the type expected where it stands, a member's type for
//! its default and an argument's for its value, with an explicit stack: one row for each node, at
//! the node, typed with that type, its `!` included. In this order:
//!
//! 1. A node whose type can take no value is kept as written, a `Literal`, and nothing is said: a
//!    type that still names a type parameter, inside a generic definition or under an application
//!    with no complete type arguments, which only an instantiation can check; the unresolved
//!    type, an object, an interface, a union or a directive; `NonZero` of what is no nullable
//!    integer primitive at least one bit wide; and a collection of another count of type arguments
//!    than its collection has, or one its own package makes both a set and a map. The type is
//!    another rule's or phase's to refuse, and that is asked before `null` is.
//! 2. `null` is `null` where its type is nullable, and does not fit where it is not.
//! 3. By the type, without its `!`: a list takes a list, each element coerced against its
//!    element, and any other node is a lone value, which stands for the one-element list that
//!    holds it, a list for each level of list the type has more than the node, each level one row
//!    at the node; a set, a scalar its own package makes one, an extern one among them, a set
//!    literal, each element coerced against the type's one argument, and a map a map literal, each
//!    key coerced against the type's first argument and each value against its second, a key or
//!    an element non-null there however it is written; `bool` a boolean; an integer primitive an
//!    integer in its width's range, read at any length; `f32` and `f64` a float, or an integer,
//!    whose value, the literal rounded once to that type, is finite, an `f32` kept widened to an
//!    `f64`, which is exact; `string` a string, cooked; `id` a string, or an integer in `i32`'s
//!    range, kept an integer; `bytes` a string, its UTF-8 encoding, or a list of integers from 0
//!    to 255, one value of its bytes either way; `NonZero` of an integer primitive an integer in
//!    that primitive's range that is not zero; an enum a path that names one of its values;
//!    `core`'s `TypeRef` a path that names a definition, and an input object an object literal,
//!    each read as below. Any other scalar takes any literal, kept as written, a set or a map
//!    literal among them. A collection given any other literal but `null`, and a set or a map
//!    literal given to what is no collection of its shape, does not fit, with
//!    `collection-literal`.
//!
//! A set literal that holds one element twice, and a map literal that holds one key twice, is
//! `collection-literal`, where it is written, at the later occurrence, with a label at the first;
//! it keeps every element and every entry. Two are one when their coerced values are: a boolean
//! by its value; an integer by its sign and its magnitude, `-0` being `0`; a float by the bits of
//! its value in its type, `-0.0` being `0.0`, so that an `f32` is told by its literal rounded once
//! to `f32` and never by its `f64` rounded again; a string by its cooked text; an `id` by its
//! identifier, an integer's being its decimal digits, so that `"4"` and `4` are one; a byte string
//! by its bytes; an enum value by its member. What has no value of its own, a custom scalar's
//! literal, a list, a set, a map, an input object, and what did not fit, is compared with nothing,
//! and so are a map's values. Each identity is rendered once, and the duplicates are found by the
//! shared kernel. A copy compares nothing: its default was judged where it is written.
//!
//! An enum value's name is looked up among the values of the enum expected, every enum's values
//! being kept by the enum and the name, the first of each name. A bare name is looked up there
//! and nothing else, no scope and no parameter. A qualified one's segments but the last are
//! resolved once, as a type, in the module and the source the value is written in, the generic
//! parameter its first segment names, if any, in scope: they must name the enum expected, through
//! any path or alias, and another enum, a parameter, `NonZero` or a module does not fit. A prefix
//! the resolver refuses is the resolver's to report, at the path, whoever asks first; the value is
//! then unresolved, and nothing more is said. A name that is none of the enum's values does not
//! fit, and its help names the nearest of them, under the analysis's one near-miss budget.
//!
//! A `TypeRef` value's whole path is resolved once, as a type, as a field's type is, in the module
//! and the source the value is written in, the generic parameter its first segment names, if any,
//! in scope; the answer is kept with the path. A definition of any kind is a type value, a generic
//! one named bare; a type parameter, an alias such as `NonZeroU32`, which names an instantiation,
//! and a module do not fit. A path the resolver refuses is the resolver's to report, GraphQL's
//! `undefined-type` for one segment with no root, whoever asks first; the value is then
//! unresolved, and nothing more is said.
//!
//! An object literal against an input object is read once and kept, by the literal and the input
//! object: a field whose name is the name of one of the input object's fields, the first of that
//! name, sets that field; the fields it sets are sorted by the field, one written twice kept twice
//! in written order; and how many fields it writes is kept, unknown ones counted. Its row is an
//! `Object`, whose entries are the fields it sets in the input object's order, each named where its
//! name is written, its value coerced against the field's type as written. Where it is written, a
//! field the input object does not declare is `undefined-input-object-field`, at its name, with the
//! nearest of the input object's fields in its help, and is not kept: its value is no row. A
//! required field, one written non-null with no default, given `null` is
//! `missing-required-input-object-field`, at the `null`, its value one unresolved row; one the
//! literal leaves out is the same code, at the literal, with a label at the field. A later field of
//! a name an earlier one has is never asked for, since no literal can set it. An input object its
//! own package applies `core`'s `@oneOf` to, in its definition or in an extension of its own,
//! takes a literal of exactly one field, unknown ones counted, whose value is not `null`: another
//! literal is refused with the site's code, at the literal, or at the `null`, which is one
//! unresolved row, and is an `Object` all the same, its fields coerced. A field the literal leaves
//! out that has a default is an entry with no name, a copy of the default, where the field stands
//! in the input object's order, but for a OneOf input object, whose fields left out are never
//! filled; a field neither set, defaulted nor required is absent. Where the literal is written, its
//! required fields left out are reported before any default is filled.
//!
//! A node that does not fit is one row, unresolved, typed with its type, and is not descended: an
//! element of a list does not fit on its own, and the list keeps the others. It is reported where
//! it is written, with GraphQL's code for a default or for a directive's argument that does not
//! fit, but for a number out of range of any width but the 32-bit signed one, or out of `f32`'s,
//! which is `width-overflow`, and a zero where `NonZero` is expected, which is `nonzero-zero`. A
//! required argument, one written non-null with no default, given `null` is
//! `missing-required-directive-argument`, at its name, once. A path whose root lowering does not
//! know was reported there, and does not fit silently. A row made of a written path keeps its
//! path, whatever it becomes.
//!
//! What is read of a literal is read once and kept outside the tree, in a row the node's one slot
//! indexes: its text, kept in the model's text arena; a string's cooked text; an integer's count
//! of significant digits, its magnitude once its digits could fit the width asked, and its row; a
//! float's verdict and its row for each of `f64` and `f32`, whose values differ; a literal's row;
//! a path's segments, what its prefix names, and what the whole of it names as a type. A byte
//! string, which only a `bytes` value reads, is kept in a table of its own, by node. An object
//! literal's fields are kept by the literal and the input object.
//!
//! # An application's arguments
//!
//! Before the walk, each directive's arguments that an application leaving them out does something
//! about are listed once, in member order: one with a default, with its default's tree; and one
//! required, written non-null with no default. A later argument of a name an earlier one has is not
//! listed, since no application can write it. An application merges its written arguments, which
//! lowering kept in its directive's order, with that list, by the argument each sets. A written
//! argument is coerced as written, and a required one given `null` is
//! `missing-required-directive-argument`, at its name, and nothing more. A listed argument left
//! out takes a copy of its default: the default's tree coerced again against the argument's type,
//! silently, each row not written and located at the node it copies, what was read of the literal
//! shared; a node that does not fit is unresolved, and the default's own diagnostic, where it is
//! written, says why once. A required one left out is `missing-required-directive-argument`, at the
//! application's path, with a label at the argument. An argument neither written, defaulted nor
//! required is absent.
//!
//! # Under type arguments
//!
//! An application whose type is its directive's instantiation, with its complete type arguments,
//! reads each argument's type under them: a type that names the directive's parameters is rebuilt
//! with the application's type arguments in their place, through the type services' rebuild, and
//! one written closed is the type as written. A rebuild is remembered by the written type and the
//! instantiation's nullable form, what stopped it included, so both nullnesses and every
//! application of one instantiation share it. "Required" is read there: non-null under the
//! arguments, with no default. An argument written as a bare nullable type parameter of its
//! directive, with no default, may be required: left out of an application with complete type
//! arguments, it is required where its type under them is non-null, `@d<Int!>` and not `@d<Int>`.
//! Under an application inside a generic definition, a type that names that definition's
//! parameters is still open, and its value a `Literal`. An application with no complete type
//! arguments is read as written.
//!
//! An object literal against an instantiation of a generic input object, `In<i32>`, reads each
//! field's type under that type's arguments, in both modes and in the count alike: a field it sets,
//! a default it leaves out, and its required fields, a field written as a bare nullable type
//! parameter of its input object with no default required where its type under them is non-null,
//! a step each time a written literal leaves it out. A field is found by its definition and its
//! name, whatever the arguments.
//!
//! A type that cannot be built under the arguments, past `Limits::max_generic_depth` or
//! `Limits::max_instantiations`, or whose rebuild is past `Limits::max_value_steps`, makes a
//! written value one unresolved row, typed with the type the argument is written with, with
//! `resource-limit` at it; an application that leaves such an argument out is starved, and
//! `resource-limit` names the limit.
//!
//! A copy made against a type a rebuild made is judged: its count pays a step for each node, and
//! judges whether the copy fits, through the classifier and the reading of literals the written
//! mode uses, with no row written: what the written mode would refuse, `null` where its type is
//! non-null, a leaf its type does not take, an object literal's unknown field, a required field it
//! leaves out, a OneOf literal of other than one field or of `null` for its one, a set or a map
//! literal that holds an element or a key twice, and a judged count it reads that does not fit. A
//! count that is not judged fits: its default was judged where it is written. The copy is made all
//! the same, silently, and who reports it is told by the written node that asked for it:
//!
//! - an application whose type arguments rebuilt the type of an argument it leaves out reports
//!   the first such copy that does not fit, in its directive's order: one `invalid-default-value`,
//!   at its path, labelled at the default;
//! - a written object literal whose type a rebuild made, a field's or an argument's type rebuilt,
//!   or an element of one, reports the first copy of a default it leaves out that does not fit, in
//!   its input object's order: one `invalid-default-value`, at the literal, labelled at the
//!   default;
//! - a written object literal whose type a written occurrence gave is silent: that occurrence's
//!   instantiation is checked where it is written.
//!
//! A copy reports nothing: whether it fits reaches whoever asked for its outermost count.
//!
//! # The count of a copy
//!
//! A copy's rows are counted before its first row is made, and the count is what is charged. A
//! default is counted the first time a copy of it is asked for, by an application that leaves its
//! argument out, by a written object literal that leaves its field out, or by another count, and
//! never before; and the count is kept by the default's tree and the type it is counted against.
//! A count goes through the same classifier and the same reading of object literals as the copy:
//! its written nodes, a lone value counted from its type's levels of list, and, for each default
//! an object literal of it leaves out, a read of that default's count. A read is a step, paid
//! before it is read, and spent whether or not the copy it was counted for is made. So is asking
//! again for a count made already, for a later copy of the same default: the asker reads the kept
//! count, a step paid before the count is used, besides the rows the copy then pays.
//!
//! A count under a type its default is not written against is judged, as above: each of its nodes
//! is a step too, paid before it is counted, and what does not fit is kept beside the count. A
//! count still meets itself by its key, the tree and the type: a tree read again under another
//! type is a count of its own.
//!
//! A count is exact, past the budget, or poisoned. A count that reads a count still being made
//! has met itself: the copy would hold a copy of itself, and the count is poisoned, with every
//! count on the way to it, and so is every count that reads a poisoned one. A count whose rows
//! pass `Limits::max_value_steps`, or one of whose reads cannot be paid for, is past it, with every
//! count on the way, and is never added up further. An exact count's rows are added with overflow
//! checked, so it is exact whatever the limit; a default whose count is exact reaches no default
//! twice on one chain. Counting uses explicit stacks, a frame for each default being counted.
//!
//! Who asks for a count does what it says, once it has paid for reading a count made already:
//! exact, the count is charged, all of it or none, and the copy is made; past the budget, the
//! asker is starved; poisoned, one step is charged, and nothing is made: the argument, or the
//! field, is absent, and the cycle is another rule's to report. A read the budget cannot pay for
//! starves the asker too. A copy is whole or absent, and the rows a copy makes are its count's,
//! which the phase asserts when it ends.
//!
//! # What bounds it
//!
//! A written row is a written literal, which `Limits::max_source_bytes` bounds. What no literal
//! writes is a step of `Limits::max_value_steps`: each list a written lone value stands for; each
//! row of a copy, its whole count charged before its first row; each read of a count while
//! counting, and each count made already that is asked for again; each default asked for that
//! needs itself; each required argument, and each required field of an object literal, found
//! left out; each node of a judged count; each node a rebuild of a member's type under type
//! arguments looks at, its root included, once for each type and instantiation; and each argument
//! that may be required left out of an application with complete type arguments. So is the one
//! reading that is more than linear: a decimal integer of more than 38
//! significant digits, the most `u128` arithmetic reads whatever they spell, is converted nine
//! digits at a time over a vector of limbs, and that is charged first, an upper bound of its limb
//! updates; one the budget cannot pay for is `resource-limit` where it is written, and
//! unresolved. A shorter one is read in `u128` arithmetic, and a hexadecimal, octal or binary one
//! in one pass, each for nothing. Past it a lone value is unresolved, and
//! `resource-limit` where it is written; an application is `resource-limit` once, at its path, its
//! written arguments still coerced, a copy made whole or not at all, and the rest of its
//! directive's list not read; and an object literal likewise, once, at the literal, its written
//! fields still coerced and no more of its defaults read. The built-in library's own values are
//! charged nothing, and neither is a copy made for an application it writes, whose count reads
//! nothing and is never stopped at the budget; a copy of one of its defaults made for an
//! application a supplied package writes is charged.
//!
//! # The arenas
//!
//! The model addresses its names and its texts with `u32` offsets, and this phase is the last that
//! writes either. When it ends, a spelling the name arena refused, or a text arena longer than a
//! `u32` addresses, is reported once, as `resource-limit`, at the whole of the root package's
//! package source: otherwise the model would answer a wrong spelling with no diagnostic.

use std::{
  collections::{BTreeMap, BTreeSet},
  format,
  string::String,
  vec::Vec,
};

use smear_lexer::decode_string_value;
use smear_semantics::duplicates::Duplicates;
use tokora::{
  SimpleSpan,
  diagnostic::{Code, Location},
};

use super::{
  builtin::{ALLOC, CORE, DIRECTIVE, TYPE},
  code::{DiagnosticKind, schema},
  declare::Declarations,
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{ApplicationId, DefId, MemberId, SourceId, Sym, TyId, TypeExprId, ValueId, dense},
  instantiate::{applies, binding, own_package},
  model::{
    Bytes, Collection, DefKind, LiteralKind, ModelBuilder, Primitive, Resolution, Shape, Text,
  },
  raw::{Radix, RawArgument, RawKind, RawNode, RawPath, RawTree, RawValues, UNREAD},
  resolve::{Position, Request, Resolved, Root, Scopes, Segment, Wanted},
  set::ROOT,
  source::SourceMap,
  types::{Counter, Limit, Stop, Types},
};

mod number;

use number::{Float, Scanned};

/// The width an integer is read in where a float is expected: an integer longer than one of this
/// many bits is no finite float.
const FLOAT_BITS: u32 = 1024;

/// Makes the model's values of `raw`, the values lowering read, in `builder`, whose declarations
/// are `declarations`, whose scopes are `scopes` and whose sources `sources` numbers, with the type
/// services and limits of `types`, reporting into `reporter`: each member's default and each
/// application's arguments, in the model's walk; then the check of the model's arenas.
pub(crate) fn coerce(
  raw: RawValues<'_>,
  declarations: &Declarations<'_, '_>,
  scopes: &Scopes<'_>,
  sources: &SourceMap,
  types: &mut Types,
  builder: &mut ModelBuilder,
  reporter: &mut Reporter,
) {
  let builtins = Builtins {
    non_zero: binding(declarations, builder, CORE, TYPE, b"NonZero"),
    string: binding(declarations, builder, ALLOC, TYPE, b"string"),
    id: binding(declarations, builder, ALLOC, TYPE, b"id"),
    bytes: binding(declarations, builder, ALLOC, TYPE, b"bytes"),
    type_ref: binding(declarations, builder, CORE, TYPE, b"TypeRef"),
  };
  let one_of_directive = binding(declarations, builder, CORE, DIRECTIVE, b"oneOf");
  let set_directive = binding(declarations, builder, CORE, DIRECTIVE, b"set");
  let map_directive = binding(declarations, builder, CORE, DIRECTIVE, b"map");
  // Every enum's values and every input object's fields, the first of each name: what an enum
  // value's name, and an object literal's field, is looked up by.
  let mut members: BTreeMap<(DefId, Sym), MemberId> = BTreeMap::new();
  // The input objects their own package applies `@oneOf` to, in id order.
  let mut one_of: Vec<DefId> = Vec::new();
  // The collections their own package applies both `@set` and `@map` to, in id order, by the test
  // instantiation and the collection arity rule read: each takes no value.
  let mut both: Vec<DefId> = Vec::new();
  for index in 0..builder.def_count() {
    let def = DefId::new(index);
    let kind = builder.def_kind(def);
    if kind == Some(DefKind::Scalar)
      && builder.def_collection(def).is_some()
      && let (Some(set), Some(map)) = (set_directive, map_directive)
      && let Some(own) = own_package(declarations, def)
      && applies(builder, sources, own, def, set)
      && applies(builder, sources, own, def, map)
    {
      both.push(def);
    }
    if !matches!(kind, Some(DefKind::Enum | DefKind::InputObject)) {
      continue;
    }
    for member in builder.def_members(def) {
      if let Some(name) = builder.member_name(member) {
        members.entry((def, name)).or_insert(member);
      }
    }
    if kind == Some(DefKind::InputObject)
      && let Some(directive) = one_of_directive
      && let Some(own) = own_package(declarations, def)
      && applies(builder, sources, own, def, directive)
    {
      one_of.push(def);
    }
  }
  let max_steps = types.limits().max_value_steps();
  let mut coercion = Coercion {
    builder,
    reporter,
    types,
    scopes,
    raw,
    primitives: &declarations.primitives,
    builtins,
    members,
    one_of,
    both,
    defaulted: Vec::new(),
    required: Vec::new(),
    inputs: Vec::new(),
    set_fields: Vec::new(),
    objects: BTreeMap::new(),
    entries: Vec::new(),
    supplied: sources.package_sources(ROOT).start(),
    values: Counter::new(max_steps),
    rebuilds: BTreeMap::new(),
    kept: Vec::new(),
    magnitudes: Vec::new(),
    limbs: Vec::new(),
    scratch: String::new(),
    octets: Vec::new(),
    byte_strings: BTreeMap::new(),
    stack: Vec::new(),
    items: Vec::new(),
    idents: Vec::new(),
    pairs: Vec::new(),
    key_bytes: Vec::new(),
    keyed: Vec::new(),
    twice: Vec::new(),
    children: Vec::new(),
    syms: Vec::new(),
    listed: Vec::new(),
    runs: Vec::new(),
    counts: BTreeMap::new(),
    unfit: BTreeSet::new(),
    counting: Vec::new(),
    frames: Vec::new(),
    held: Vec::new(),
    paid: 0,
    copied: 0,
    levels: BTreeMap::new(),
    chain: Vec::new(),
  };
  coercion.list_arguments();
  coercion.list_fields();
  for index in 0..coercion.builder.def_count() {
    let def = DefId::new(index);
    let count = coercion.builder.def_applications(def).len();
    for at in 0..count {
      if let Some(&application) = coercion.builder.def_applications(def).get(at) {
        coercion.application(application);
      }
    }
    for member in coercion.builder.def_members(def) {
      coercion.member(member);
      for argument in coercion.builder.member_arguments(member) {
        coercion.member(argument);
      }
    }
  }
  let count = coercion.builder.schema_applications().len();
  for at in 0..count {
    if let Some(&application) = coercion.builder.schema_applications().get(at) {
      coercion.application(application);
    }
  }
  debug_assert_eq!(
    coercion.paid, coercion.copied,
    "the copies make exactly the rows they were counted to"
  );
  let root = sources.package_sources(ROOT);
  arenas(
    coercion.builder,
    coercion.reporter,
    Location::entire(root.start()),
  );
}

/// The built-in definitions coercion keys on, each found by its name in its package's root module.
#[derive(Clone, Copy)]
struct Builtins {
  non_zero: Option<DefId>,
  string: Option<DefId>,
  id: Option<DefId>,
  bytes: Option<DefId>,
  type_ref: Option<DefId>,
}

/// Where a value is written, which says the code it is refused with.
#[derive(Clone, Copy)]
enum Site {
  /// A member's default: GraphQL's code for an invalid default.
  Default,
  /// An application's argument: GraphQL's code for an invalid directive argument.
  Argument,
}

impl Site {
  /// The code of a value written here that does not fit, and its label.
  fn refusal(self) -> (Code, &'static str) {
    match self {
      Self::Default => (schema::INVALID_DEFAULT_VALUE, "invalid default value"),
      Self::Argument => (
        schema::INVALID_DIRECTIVE_ARGUMENT_VALUE,
        "invalid directive argument value",
      ),
    }
  }
}

/// How a tree is coerced.
#[derive(Clone, Copy)]
enum Mode {
  /// Where it is written, at this site: its rows are written, what does not fit is reported, and
  /// the lists a lone value stands for are charged.
  Written(Site),
  /// As a copy of a default, for an application that leaves its argument out or an object literal
  /// that leaves its field out: its rows are not written, and nothing is reported, charged or
  /// searched for, the copy's whole count having been charged before its first row.
  Copied,
}

impl Mode {
  /// Whether the rows made are written.
  fn written(self) -> bool {
    matches!(self, Self::Written(_))
  }
}

/// A directive's argument that an application leaving it out does something about.
#[derive(Clone, Copy)]
struct Listed {
  argument: MemberId,
  /// What leaving it out does.
  left: Left,
}

/// What leaving a listed argument out does.
#[derive(Clone, Copy)]
enum Left {
  /// A copy of its default, whose tree this is, fills it.
  Default(u32),
  /// It is required: written non-null, with no default.
  Required,
  /// It may be required: written as a bare nullable type parameter of its directive, with no
  /// default, it is required where the application's type arguments make its type non-null.
  Maybe,
}

/// An application whose arguments are being merged: its id, its directive's path, whether the
/// built-in library wrote it, and its type with its directive, the instantiation its arguments are
/// read under, when it has its directive's.
#[derive(Clone, Copy)]
struct Applying {
  application: ApplicationId,
  at: Location,
  library: bool,
  env: Option<(TyId, DefId)>,
}

/// The type a member's value takes, read under an instantiation's arguments.
#[derive(Clone, Copy)]
struct Expected {
  /// The type: as instantiation typed the member's occurrence, or rebuilt under the arguments.
  ty: Option<TyId>,
  /// Whether a rebuild made it.
  rebuilt: bool,
  /// Whether the member is non-null as the value takes it: the rebuilt type's `!`, or else the
  /// `!` the member's type is written with.
  non_null: bool,
}

/// An input object's fields that an object literal leaving them out does something about, each run
/// in member order: its run of the phase's defaulted fields, none for a OneOf input object, and
/// its run of the phase's required fields, each written non-null with no default.
#[derive(Clone, Copy)]
struct Input {
  def: DefId,
  defaults: (u32, u32),
  required: (u32, u32),
}

/// A default's count: how many rows a copy of it makes, or why no copy is made.
#[derive(Clone, Copy)]
enum Count {
  /// It is being counted: a count that reads it now has met itself.
  Counting,
  /// A copy of it makes this many rows, exactly.
  Rows(u32),
  /// Its rows pass `Limits::max_value_steps`, or a read it makes could not be paid for.
  Past,
  /// It reads its own count: its copy would hold a copy of itself.
  Poisoned,
  /// A type it reads under type arguments passes this limit, and cannot be built.
  Unbuilt(Limit),
}

/// One entry of a count's stack.
#[derive(Clone, Copy)]
enum Counted {
  /// A node of the tree being counted, against the type it is made against; in a judged count, an
  /// element or a key of a set or a map whose identity goes in the slot `slot` of the elements
  /// being searched.
  Node {
    node: u32,
    ty: Option<TyId>,
    slot: Option<u32>,
  },
  /// A read: the count of the default whose tree is `tree`, against `ty`, judged when `ty` is not
  /// the type the default is written against.
  Fill {
    tree: u32,
    ty: Option<TyId>,
    judged: bool,
  },
  /// In a judged count, a set or a map, as `held` says, whose elements or keys counted since have
  /// their identities from the slot `start` on: searched for one held twice.
  Held { start: u32, held: Held },
}

/// A default being counted: its tree and the type it is counted against, where its own entries of
/// the count's stack start, and its rows so far; whether it is judged, its type not the one its
/// default is written against, and, judged, whether its copy fits so far.
#[derive(Clone, Copy)]
struct Frame {
  tree: u32,
  ty: Option<TyId>,
  base: usize,
  rows: u32,
  judged: bool,
  fits: bool,
}

/// What asking for a default left out does, an application's argument or an object literal's
/// field.
#[derive(Clone, Copy)]
enum Asked {
  /// A copy of this many rows is made; under a type its default is not written against, `fits`
  /// says whether each of its nodes takes its type as a written one would.
  Copy { rows: u32, fits: bool },
  /// Nothing is made: the argument, or the field, is absent.
  Absent,
  /// The budget cannot pay: the asker is past it.
  Starved,
  /// A type its count reads under type arguments passes this limit: the asker is past it.
  Unbuilt(Limit),
}

/// Where a merge of an object literal's fields with its input object's defaults has got to: the
/// next field the literal sets and the next default, each with the end of its run.
#[derive(Clone, Copy)]
struct Merge {
  set: u32,
  set_end: u32,
  default: u32,
  default_end: u32,
}

/// What a merge gives next.
#[derive(Clone, Copy)]
enum Next {
  /// A field the literal sets.
  Set(SetField),
  /// The default of a field the literal leaves out: the field, and the default's tree.
  Default(MemberId, u32),
}

/// A field an object literal sets: the input object's field, where its name is written, in the
/// literal's source, and its value's node.
#[derive(Clone, Copy)]
struct SetField {
  field: MemberId,
  name: SimpleSpan,
  node: u32,
}

/// What is read of an object literal against an input object: its run of the phase's fields it
/// sets, sorted by the field, and how many fields it writes in all, unknown ones counted.
#[derive(Clone, Copy)]
struct Fields {
  start: u32,
  len: u32,
  written: u32,
}

/// The range of an integer type: the integer primitive `width` bits wide, signed when `signed`,
/// and never zero when `non_zero`.
#[derive(Clone, Copy)]
struct Range {
  signed: bool,
  width: u32,
  non_zero: bool,
}

/// `id`'s integers: `i32`'s.
const ID_RANGE: Range = Range {
  signed: true,
  width: 32,
  non_zero: false,
};

/// What a named type takes.
#[derive(Clone, Copy)]
enum Base {
  Bool,
  /// An integer primitive, or `NonZero` of one.
  Int(Range),
  /// `f32` when `single`, and `f64` otherwise.
  Float {
    single: bool,
  },
  String,
  Id,
  Bytes,
  /// This enum.
  Enum(DefId),
  /// This input object.
  Object(DefId),
  /// A set of this element.
  Set(TyId),
  /// A map from this key to this value.
  Map(TyId, TyId),
  /// `core`'s `TypeRef`: a path that names a definition.
  TypeRef,
  /// Any literal, kept as written: a custom scalar that is no collection.
  Kept,
}

/// What a node is made against a type, told by the node's syntactic kind and the type alone.
#[derive(Clone, Copy)]
enum Class {
  /// The type can take no value, or there is none: one `Literal` row.
  NoValue,
  /// `null` against a nullable type: one `Null` row.
  Null,
  /// `null` against a non-null type: one unresolved row.
  NotNull,
  /// A list against a list of this element: a `List` row, then each element against it.
  List(TyId),
  /// Any other node against a list of this element: a `List` row, then the same node against it.
  Lone(TyId),
  /// An object literal against the input object `def`, its fields' rows of the trees starting at
  /// `first`: an `Object` row, then each field it sets against the field's type.
  Object { def: DefId, first: u32 },
  /// A set literal against a set of this element: a `Set` row, then each element against it.
  Set(TyId),
  /// A map literal against a map from this key to this value: a `Map` row, then each key against
  /// the key and each value against the value.
  Map(TyId, TyId),
  /// Any other node against a named type: one row, its kind told by what the type takes.
  Leaf(Base),
}

/// Why a node does not fit.
#[derive(Clone, Copy)]
enum Why {
  /// `null` where a non-null value is expected.
  Null,
  /// A literal of another kind than the type takes.
  Kind,
  /// An integer out of this range.
  Range(Range),
  /// A float, or an integer where a float is expected, that is not finite as an `f64`.
  Infinite,
  /// A float, or an integer where `f32` is expected, out of `f32`'s range.
  Single,
  /// Zero where `NonZero` is expected.
  Zero,
  /// An element of a `bytes` list that is no integer.
  Byte,
  /// An element of a `bytes` list out of a byte's range.
  ByteRange,
  /// A path that names no value of the enum `def`: `name`, its last segment, is none of its
  /// values.
  NoValue { def: DefId, name: Sym },
  /// A decimal integer of `digits` significant digits whose conversion, `cost` steps, the budget
  /// could not pay for.
  Costly { digits: u64, cost: u32 },
  /// `null` for `field`, a required field of the input object `def`.
  Required { def: DefId, field: MemberId },
  /// A literal of the OneOf input object `def` that writes `written` fields, not one.
  OneOf { def: DefId, written: u32 },
  /// `null` for the one field a literal of the OneOf input object `def` writes.
  OneOfNull { def: DefId },
  /// Another literal where the collection `expected` is expected; or, with none expected, a
  /// collection literal where no collection of its shape is.
  Collection { expected: Option<Collection> },
  /// A type it takes under type arguments that cannot be built, as this says.
  Unbuilt(Stop),
}

/// The code a node that does not fit is reported with.
#[derive(Clone, Copy)]
enum Coded {
  /// The site's: GraphQL's code for a default, or for a directive's argument, that does not fit.
  Site,
  /// One of smear's own.
  Kind(DiagnosticKind),
  /// One of GraphQL's that is the same at every site, with its label.
  Schema(Code, &'static str),
}

/// A node that does not fit, and why: the node may be an element of the value being made.
#[derive(Clone, Copy)]
struct Refused {
  node: u32,
  why: Why,
}

/// What tells a value that fits from another of its type, for a set's elements and a map's keys:
/// two values of one type are one when their identities render alike. A value that is no leaf,
/// a custom scalar's, and one that did not fit have none.
#[derive(Clone, Copy)]
enum Ident {
  Bool(bool),
  /// An integer: its sign, kept for a magnitude that is not zero alone, so that `-0` is `0`, and
  /// its magnitude, a run of the phase's own.
  Int {
    negative: bool,
    start: u32,
    len: u32,
  },
  /// An `id`'s integer, read as its decimal digits, so that it is one with the string of them.
  Decimal {
    negative: bool,
    start: u32,
    len: u32,
  },
  /// A float's bits, of its value in its type: an `f64`'s, or an `f32`'s, the literal rounded once
  /// to `f32`; `-0.0` is `0.0`.
  Float(u64),
  /// A string's cooked text, or an `id`'s.
  Text(Text),
  Bytes(Bytes),
  /// An enum value.
  Member(MemberId),
}

/// An integer's magnitude once converted: its run of the phase's own magnitudes, its bit length,
/// and whether it is a power of two.
#[derive(Clone, Copy)]
struct Converted {
  start: u32,
  len: u32,
  bits: u64,
  power: bool,
}

/// What coercion has read of one node, each part read the first time it is asked for and kept, so
/// that no literal is read twice and no text kept twice.
#[derive(Clone, Copy, Default)]
struct Kept {
  /// Its text as written, in the model's text arena.
  text: Option<Text>,
  /// A string's cooked text, in the model's text arena; `None` inside for a spelling the decoder
  /// refuses.
  cooked: Option<Option<Text>>,
  /// An integer as scanned; `None` inside for a text that is no integer of its radix.
  scanned: Option<Option<Scanned>>,
  /// An integer's magnitude, once converted; `None` inside for a conversion the budget could not
  /// pay for.
  converted: Option<Option<Converted>>,
  /// An integer's row of the model's integer table.
  int: Option<u32>,
  /// What it reads as where a float is expected; `None` inside for a text no float reads.
  float: Option<Option<Float>>,
  /// Its row of the model's float table as an `f64`.
  float_row: Option<u32>,
  /// Its row of the model's float table as an `f32`, of the `f32`'s value.
  single_row: Option<u32>,
  /// Its row of the model's literal table.
  literal: Option<u32>,
}

// A row for each literal read, so its size is a cost per written literal: about 120 bytes, a few
// times the node's 32, since a node read for any kind has room for what every kind reads. What only
// a rare kind reads lives in a table of its own: a `bytes` value's bytes.
const _: () = assert!(core::mem::size_of::<Kept>() <= 128);

/// A node to make: the node, the type it is made against, the tree it is of, and how; and whether
/// a rebuild under type arguments made its type, or the type of a value it is held in, which says
/// who reports a copy of a default an object literal of it leaves out that does not fit.
#[derive(Clone, Copy)]
struct Making {
  node: u32,
  ty: Option<TyId>,
  tree: RawTree,
  mode: Mode,
  rebuilt: bool,
}

impl Making {
  /// `node` against `ty`, of the same tree and made the same way.
  fn child(self, node: u32, ty: Option<TyId>) -> Self {
    Self { node, ty, ..self }
  }
}

/// What a value that holds values is.
#[derive(Clone, Copy)]
enum Held {
  List,
  Set,
  Map,
}

/// One step of making a tree's rows.
#[derive(Clone, Copy)]
enum Task {
  /// Make a node's row, and push what it holds.
  Make(Making),
  /// Make a node's row one unresolved row, and report why, where it is written.
  Refuse(Making, Why),
  /// Give `value`, a list, a set or a map as `held` says, the rows made since `from` among the
  /// open values' rows: a map's in pairs, each key and then its value. A set or a map written in
  /// the source `check` is first searched for an element or a key held twice.
  Close {
    value: ValueId,
    from: usize,
    held: Held,
    check: Option<u32>,
  },
  /// Give the input object `object` its entries: the open entries from `entries` on, each with
  /// its value, the rows made since `from` among the open values' rows, in the same order.
  CloseObject {
    object: ValueId,
    from: usize,
    entries: usize,
  },
}

/// What coercion keeps while it writes.
struct Coercion<'c, 'p, 'x> {
  builder: &'c mut ModelBuilder,
  reporter: &'c mut Reporter,
  /// The type services: the rebuild that reads a member's type under an instantiation's
  /// arguments.
  types: &'c mut Types,
  /// Every module's scope: what a qualified enum value's prefix names.
  scopes: &'c Scopes<'x>,
  /// The trees, and each source's text.
  raw: RawValues<'p>,
  /// Each primitive `core` materialises, with its definition: the primitive at index `i` is the
  /// definition `i`.
  primitives: &'c [(Primitive, DefId)],
  builtins: Builtins,
  /// Every enum's values and every input object's fields, by the definition and the name, the
  /// first of each name.
  members: BTreeMap<(DefId, Sym), MemberId>,
  /// The input objects their own package applies `core`'s `@oneOf` to, in id order.
  one_of: Vec<DefId>,
  /// The collections their own package applies both `core`'s `@set` and its `@map` to, in id
  /// order: each takes no value.
  both: Vec<DefId>,
  /// Each input object's defaulted fields, each with its default's tree, one run each, in member
  /// order.
  defaulted: Vec<(MemberId, u32)>,
  /// Each input object's required fields, one run each, in member order: each field, and whether
  /// it may be required, a bare nullable type parameter of its input object, required where its
  /// type under the object's arguments is non-null.
  required: Vec<(MemberId, bool)>,
  /// Each input object that has a defaulted or a required field, with its runs, by `DefId`.
  inputs: Vec<Input>,
  /// The fields each object literal read so far sets, one run each, sorted by the field.
  set_fields: Vec<SetField>,
  /// What is read of each object literal read so far, by its node and the input object it was
  /// read against.
  objects: BTreeMap<(u32, DefId), Fields>,
  /// The entries of the input objects that are open, each its field, where its name is written,
  /// and its value once its row is made.
  entries: Vec<(MemberId, Option<Location>, ValueId)>,
  /// The first source of a supplied package: every source below it is the built-in library's.
  supplied: u32,
  /// `Limits::max_value_steps`, and the steps spent: completing values, and reading types and
  /// counting under type arguments.
  values: Counter,
  /// Each member's type read under an instantiation's arguments so far, by the type it is written
  /// with and the instantiation's nullable form: the type rebuilt, `None` for a type that is not
  /// rebuilt, or what stopped the rebuild.
  rebuilds: BTreeMap<(TyId, TyId), Result<Option<TyId>, Stop>>,
  /// What has been read of each node read so far: the rows a node's `kept` slot indexes.
  kept: Vec<Kept>,
  /// Every integer's magnitude converted so far.
  magnitudes: Vec<u8>,
  /// The limbs of the decimal integer being converted.
  limbs: Vec<u32>,
  /// The text being cooked.
  scratch: String,
  /// The bytes of the list being read as a `bytes` value.
  octets: Vec<u8>,
  /// Each node read as a `bytes` value, by its index: its bytes in the model's byte pool, or why
  /// it is none.
  byte_strings: BTreeMap<u32, Result<Bytes, Refused>>,
  /// What making the current tree has still to do.
  stack: Vec<Task>,
  /// The rows made of the values that are open, lists, sets, maps and input objects, each one's
  /// after it.
  items: Vec<ValueId>,
  /// Beside each of `items`, the row's identity, when it has one, and its node.
  idents: Vec<(Option<Ident>, u32)>,
  /// The entries of the map being closed, each a key and its value.
  pairs: Vec<(ValueId, ValueId)>,
  /// The identities of the set or the map being searched, rendered.
  key_bytes: Vec<u8>,
  /// Each element or key of the set or the map being searched that has an identity: its run of
  /// `key_bytes`, and its node.
  keyed: Vec<(u32, u32, u32)>,
  /// Each element or key held twice, and the first that holds it: their nodes.
  twice: Vec<(u32, u32)>,
  /// The children of the list being opened.
  children: Vec<u32>,
  /// The segments of the path being kept.
  syms: Vec<Sym>,
  /// Each directive's arguments that an application leaving them out does something about: one
  /// run per directive, in member order.
  listed: Vec<Listed>,
  /// Each directive that has such arguments, with its run of `listed`, by `DefId`: what an
  /// application looks its directive's run up by.
  runs: Vec<(DefId, u32, u32)>,
  /// Each default counted so far, by its tree and the type it is counted against: its count.
  counts: BTreeMap<(u32, Option<TyId>), Count>,
  /// The counts, by the same key, made under a type their default is not written against whose
  /// copy does not fit it.
  unfit: BTreeSet<(u32, Option<TyId>)>,
  /// What the count being made has still to count: nodes, and reads of other counts.
  counting: Vec<Counted>,
  /// The defaults the count being made is counting: the one asked for first, and each later one
  /// read by the one before it.
  frames: Vec<Frame>,
  /// The identities of the elements or the keys of the sets and the maps a judged count is
  /// searching, by slot.
  held: Vec<Option<Ident>>,
  /// The rows of every count a copy was made of, charged or not.
  paid: u64,
  /// The rows made of copies: what `paid` is when the phase ends, since a copy and its count are
  /// told by one classifier.
  copied: u64,
  /// How many levels of list each list type read so far is, and the type under them: what a lone
  /// value's count reads.
  levels: BTreeMap<TyId, (u32, TyId)>,
  /// The list types whose levels are being read.
  chain: Vec<TyId>,
}

impl<'p> Coercion<'_, 'p, '_> {
  /// The values of `member`: its default, then each of its applications' arguments.
  fn member(&mut self, member: MemberId) {
    if let Some(&tree) = self.raw.defaults.get(&member) {
      let ty = self.member_type(member);
      if let Some(value) = self.value(tree, ty, Mode::Written(Site::Default), false) {
        self.builder.set_member_default(member, value);
      }
    }
    let count = self.builder.member_applications(member).len();
    for at in 0..count {
      if let Some(&application) = self.builder.member_applications(member).get(at) {
        self.application(application);
      }
    }
  }

  /// The type `member` is written with, as instantiation typed it, and whether it is written
  /// non-null.
  fn member_occurrence(&self, member: MemberId) -> (Option<TyId>, bool) {
    let occurrence = self
      .builder
      .member_ty(member)
      .and_then(|ty: TypeExprId| self.builder.occurrence(ty));
    match occurrence {
      Some(occurrence) => (occurrence.ty, occurrence.non_null),
      None => (None, false),
    }
  }

  /// The type `member` is written with, as instantiation typed it.
  fn member_type(&self, member: MemberId) -> Option<TyId> {
    self.member_occurrence(member).0
  }

  /// The type `member`'s value takes under `env`, when it is given: a named type of `owner`, the
  /// member's own definition, whose arguments the member's type is read under, `library` when the
  /// built-in library writes what asks. A type written closed, or with no `env`, is the type as
  /// instantiation typed it, with the `!` it is written with; one that names `owner`'s parameters
  /// is rebuilt with `env`'s arguments in their place, a step of `Limits::max_value_steps` for its
  /// root and one for each argument or element of a node rebuilt, each new type completed as an
  /// occurrence's is, with its own `!`. A rebuild is remembered by the written type and `env`'s
  /// nullable form, whose arguments are its own, what stopped it included: a second costs nothing.
  fn under(
    &mut self,
    member: MemberId,
    env: Option<(TyId, DefId)>,
    library: bool,
  ) -> Result<Expected, Stop> {
    let (written, non_null) = self.member_occurrence(member);
    let unbuilt = Expected {
      ty: written,
      rebuilt: false,
      non_null,
    };
    let (Some(written), Some((env, owner))) = (written, env) else {
      return Ok(unbuilt);
    };
    if self.builder.ty_closed(written) {
      return Ok(unbuilt);
    }
    if !self.is_named(env, owner) {
      return Ok(unbuilt);
    }
    // A named type's nullable form is interned before its non-null one, and is found.
    let builder: &ModelBuilder = &*self.builder;
    let nullable = builder
      .find_named(owner, builder.ty_args(env), false)
      .unwrap_or(env);
    let made = match self.rebuilds.get(&(written, nullable)).copied() {
      Some(made) => made,
      None => {
        let made = self.types.derived.rebuild(
          self.builder,
          &mut self.values,
          library,
          written,
          nullable,
          owner,
        );
        self.rebuilds.insert((written, nullable), made);
        made
      }
    };
    Ok(match made? {
      Some(ty) => Expected {
        ty: Some(ty),
        rebuilt: true,
        non_null: self.builder.ty_non_null(ty),
      },
      None => unbuilt,
    })
  }

  /// Whether `ty` is a named type of `def`.
  fn is_named(&self, ty: TyId, def: DefId) -> bool {
    matches!(self.builder.ty_shape(ty), Some(Shape::Named(named)) if named == def)
  }

  /// Whether the type `member`, of `owner`, is written with is a bare nullable type parameter of
  /// `owner`: a member that may be required, as its type under an instantiation's arguments says.
  fn bare_param(&self, member: MemberId, owner: DefId) -> bool {
    let (ty, non_null) = self.member_occurrence(member);
    !non_null
      && matches!(
        ty.and_then(|ty: TyId| self.builder.ty_shape(ty)),
        Some(Shape::Param { def, .. }) if def == owner
      )
  }

  /// The arguments of `application`, in its directive's order: its written run, found by one binary
  /// search, merged with its directive's listed run. A written argument is coerced as written. A
  /// listed one it leaves out is filled with a copy of its default, or, required, reported left
  /// out, each charged first, but on an application the built-in library writes. Once a charge
  /// fails the application is reported once, at its path, and its written run alone is read on. An
  /// application whose directive did not resolve has no argument.
  fn application(&mut self, application: ApplicationId) {
    let mut written = self
      .raw
      .arguments
      .partition_point(|argument: &RawArgument| argument.application < application);
    let (directive, at) = match self.builder.applied(application) {
      Some((Resolution::Resolved(def), _, at)) => (Some(def), at),
      _ => (None, Location::entire(0)),
    };
    let (mut listed, end) = directive
      .and_then(|def: DefId| {
        self
          .runs
          .binary_search_by_key(&def, |&(owner, _, _): &(DefId, u32, u32)| owner)
          .ok()
      })
      .and_then(|row: usize| self.runs.get(row).copied())
      .map_or((0, 0), |(_, start, end): (DefId, u32, u32)| {
        (start as usize, end as usize)
      });
    let library = at.source() < self.supplied;
    // Its directive's instantiation, when it has one: its arguments are read under it.
    let env = match (directive, self.builder.application_ty(application)) {
      (Some(def), Some(ty)) if self.is_named(ty, def) => Some((ty, def)),
      _ => None,
    };
    let applying = Applying {
      application,
      at,
      library,
      env,
    };
    let mut starved = false;
    // Whether a copy that does not fit under its type arguments is reported: once for it.
    let mut reported = false;
    loop {
      let next_written = self
        .raw
        .arguments
        .get(written)
        .copied()
        .filter(|argument: &RawArgument| argument.application == application);
      let next_listed = if starved || listed >= end {
        None
      } else {
        self.listed.get(listed).copied()
      };
      match (next_written, next_listed) {
        (None, None) => break,
        (Some(argument), None) => {
          self.written_argument(applying, argument);
          written += 1;
        }
        (Some(argument), Some(left)) if argument.argument <= left.argument => {
          if argument.argument == left.argument {
            listed += 1;
          }
          self.written_argument(applying, argument);
          written += 1;
        }
        (_, Some(left)) => {
          listed += 1;
          if !self.left_out(applying, left, &mut reported) {
            starved = true;
          }
        }
      }
    }
  }

  /// Does what `left`, an argument the application `applying` says leaves out, asks, its type read
  /// under the application's type arguments: a copy of its default, as [`ask`](Self::ask) says;
  /// required, a report that it is left out; and one that may be required, a step, then that
  /// report when its type under the arguments is non-null, passed over with no step when the
  /// application has no complete type arguments. Each is charged first, but on an application the
  /// built-in library writes. A copy under the arguments that does not fit them is reported at the
  /// application, the first of its arguments' copies alone, as `reported` keeps. Answers `false`,
  /// and reports the application once, when the budget cannot pay, or when a type the argument
  /// takes cannot be built under the arguments.
  fn left_out(&mut self, applying: Applying, left: Listed, reported: &mut bool) -> bool {
    let Applying {
      application,
      at,
      library,
      env,
    } = applying;
    let tree = match left.left {
      Left::Default(tree) => tree,
      Left::Required => {
        if !library && !self.charge(1) {
          self.starved(at, Stop::Steps);
          return false;
        }
        self.missing(application, left.argument, at);
        return true;
      }
      Left::Maybe => {
        if env.is_none() {
          return true;
        }
        if !library && !self.charge(1) {
          self.starved(at, Stop::Steps);
          return false;
        }
        return match self.under(left.argument, env, library) {
          Ok(expected) => {
            if expected.non_null {
              self.missing(application, left.argument, at);
            }
            true
          }
          Err(stop) => {
            self.starved(at, stop);
            false
          }
        };
      }
    };
    let expected = match self.under(left.argument, env, library) {
      Ok(expected) => expected,
      Err(stop) => {
        self.starved(at, stop);
        return false;
      }
    };
    let ty = expected.ty;
    let rows = match self.ask(tree, ty, library, expected.rebuilt) {
      Asked::Copy { rows, fits } => {
        if !fits && !*reported {
          *reported = true;
          self.unfit_default(at, left.argument, tree);
        }
        rows
      }
      Asked::Absent => return true,
      Asked::Starved => {
        self.starved(at, Stop::Steps);
        return false;
      }
      Asked::Unbuilt(limit) => {
        self.starved(at, Stop::Built(limit));
        return false;
      }
    };
    let before = self.builder.value_count();
    if let Some(value) = self.value(tree, ty, Mode::Copied, false) {
      debug_assert_eq!(
        self.builder.value_count().saturating_sub(before),
        rows,
        "a copy makes exactly the rows it was counted to"
      );
      self
        .builder
        .push_application_argument(application, left.argument, None, value);
    }
    true
  }

  /// Lists each directive's arguments that an application leaving them out does something about:
  /// one with a default, with its default's tree; one required, written non-null with no default;
  /// and one that may be required, written as a bare nullable type parameter of the directive with
  /// no default. A later argument of a name an earlier one has is not listed: no application can
  /// write it.
  fn list_arguments(&mut self) {
    let mut names: BTreeSet<Sym> = BTreeSet::new();
    for index in 0..self.builder.def_count() {
      let def = DefId::new(index);
      if self.builder.def_kind(def) != Some(DefKind::Directive) {
        continue;
      }
      let start = dense(self.listed.len());
      names.clear();
      for argument in self.builder.def_members(def) {
        let Some(name) = self.builder.member_name(argument) else {
          continue;
        };
        if !names.insert(name) {
          continue;
        }
        let (_, non_null) = self.member_occurrence(argument);
        let left = match self.raw.defaults.get(&argument).copied() {
          Some(tree) => Left::Default(tree),
          None if non_null => Left::Required,
          None if self.bare_param(argument, def) => Left::Maybe,
          None => continue,
        };
        self.listed.push(Listed { argument, left });
      }
      let end = dense(self.listed.len());
      if end > start {
        self.runs.push((def, start, end));
      }
    }
  }

  /// Lists each input object's fields that an object literal leaving them out does something
  /// about, in member order: each field with a default, with its default's tree, but for a OneOf
  /// input object, whose fields left out are never filled; and each required field, written
  /// non-null with no default, and each that may be required, written as a bare nullable type
  /// parameter of its input object with no default, marked. A later field of a name an earlier one
  /// has is not listed: no literal can set it.
  fn list_fields(&mut self) {
    let mut names: BTreeSet<Sym> = BTreeSet::new();
    for index in 0..self.builder.def_count() {
      let def = DefId::new(index);
      if self.builder.def_kind(def) != Some(DefKind::InputObject) {
        continue;
      }
      let one_of = self.one_of.binary_search(&def).is_ok();
      let defaults = dense(self.defaulted.len());
      let required = dense(self.required.len());
      names.clear();
      for field in self.builder.def_members(def) {
        let Some(name) = self.builder.member_name(field) else {
          continue;
        };
        if !names.insert(name) {
          continue;
        }
        let (_, non_null) = self.member_occurrence(field);
        match self.raw.defaults.get(&field).copied() {
          Some(tree) if !one_of => self.defaulted.push((field, tree)),
          Some(_) => {}
          None if non_null => self.required.push((field, false)),
          None if self.bare_param(field, def) => self.required.push((field, true)),
          None => {}
        }
      }
      let defaults_end = dense(self.defaulted.len());
      let required_end = dense(self.required.len());
      if defaults_end > defaults || required_end > required {
        self.inputs.push(Input {
          def,
          defaults: (defaults, defaults_end),
          required: (required, required_end),
        });
      }
    }
  }

  /// The runs of `def`, an input object, when it has any.
  fn input(&self, def: DefId) -> Option<Input> {
    let at = self
      .inputs
      .binary_search_by_key(&def, |input: &Input| input.def)
      .ok()?;
    self.inputs.get(at).copied()
  }

  /// What asking for the default whose tree is `tree`, of the type `ty`, where it is left out
  /// does, the asker the built-in library's when `library`: its count is asked for, and charged
  /// unless `library`. A count made already is read first, a step, as a count's own reads are,
  /// paid before the count is used and apart from the rows a copy then pays; one that cannot be
  /// paid starves the asker. Exact, its rows are charged, all or none, and a copy is made; past
  /// the budget, the asker is starved; a default that needs itself costs one step and is absent.
  /// Steps its count's reads paid stay spent whatever the copy. `judged` when `ty` is not the type
  /// the default is written against: the count then judges whether the copy fits it.
  fn ask(&mut self, tree: u32, ty: Option<TyId>, library: bool, judged: bool) -> Asked {
    if !library && self.counts.contains_key(&(tree, ty)) && !self.charge(1) {
      return Asked::Starved;
    }
    match self.count(tree, ty, !library, judged) {
      Count::Rows(rows) => {
        if !library && !self.charge(rows) {
          return Asked::Starved;
        }
        self.paid = self.paid.saturating_add(u64::from(rows));
        Asked::Copy {
          rows,
          fits: !self.unfit.contains(&(tree, ty)),
        }
      }
      Count::Past => Asked::Starved,
      Count::Unbuilt(limit) => Asked::Unbuilt(limit),
      // A count asked for is never one being made: a count makes no copy and asks for none.
      Count::Poisoned | Count::Counting => {
        if !library && !self.charge(1) {
          return Asked::Starved;
        }
        Asked::Absent
      }
    }
  }

  /// The count of the default whose tree is `tree`, against `ty`: kept once made, by the two, and
  /// answered as kept when asked again, its asker having paid for that read
  /// ([`ask`](Self::ask)); and made with two stacks of its own, a frame for each default being
  /// counted. A frame counts its tree's written nodes through [`classify`](Self::classify), as a
  /// copy makes them: one row each, a list's elements, a lone value's levels of list from its type
  /// and then the value against the type under them, and the fields an object literal sets; and
  /// for each default an object literal leaves out, a read of that default's count, a step charged
  /// before it is pushed when `charged`. A read of a kept count adds its rows; of one not counted
  /// yet, opens its frame.
  ///
  /// A frame with nothing left is exact, kept, and its rows added to the frame that read it. A
  /// verdict is every open frame's, each reading the one above it: a read of a count being made,
  /// or of a poisoned one, poisons; a read of one past the budget, a read that cannot be paid for,
  /// and rows that pass `u32::MAX`, or `Limits::max_value_steps` when `charged`, are past it. So a
  /// count past the budget is never added up further, and an exact one is exact whatever the
  /// limit.
  ///
  /// A count `judged`, against a type its default is not written against, the asker's `ty` a
  /// rebuild made, pays a step for each node before it is counted, and judges whether the copy
  /// fits: whether each of its nodes takes its type as a written one would, and each judged count
  /// it reads fits. A count that is not judged fits: its default was judged where it is written.
  /// What does not fit is kept beside the count.
  fn count(&mut self, tree: u32, ty: Option<TyId>, charged: bool, judged: bool) -> Count {
    if let Some(&kept) = self.counts.get(&(tree, ty)) {
      return kept;
    }
    self.counting.clear();
    self.frames.clear();
    self.held.clear();
    self.open(tree, ty, judged);
    loop {
      let Some(frame) = self.frames.last().copied() else {
        return Count::Past;
      };
      if self.counting.len() <= frame.base {
        self.frames.pop();
        let done = Count::Rows(frame.rows);
        self.counts.insert((frame.tree, frame.ty), done);
        if !frame.fits {
          self.unfit.insert((frame.tree, frame.ty));
        }
        if self.frames.is_empty() {
          return done;
        }
        // A copy that holds one that does not fit does not fit.
        if !frame.fits {
          self.misfit();
        }
        if let Err(verdict) = self.add_rows(frame.rows, charged) {
          return self.verdict(verdict);
        }
        continue;
      }
      let Some(entry) = self.counting.pop() else {
        return self.verdict(Count::Past);
      };
      let counted = match entry {
        Counted::Node { node, ty, slot } => self.count_node(node, ty, slot, charged),
        Counted::Fill { tree, ty, judged } => match self.counts.get(&(tree, ty)).copied() {
          Some(Count::Rows(rows)) => {
            if self.unfit.contains(&(tree, ty)) {
              self.misfit();
            }
            self.add_rows(rows, charged)
          }
          Some(Count::Counting | Count::Poisoned) => Err(Count::Poisoned),
          Some(verdict @ (Count::Past | Count::Unbuilt(_))) => Err(verdict),
          None => {
            self.open(tree, ty, judged);
            Ok(())
          }
        },
        Counted::Held { start, held } => {
          if !self.held_once(start, held) {
            self.misfit();
          }
          Ok(())
        }
      };
      if let Err(verdict) = counted {
        return self.verdict(verdict);
      }
    }
  }

  /// Opens a frame for the default whose tree is `tree`, against `ty`, judged when `judged`: it is
  /// being counted from now on, and its tree's root is the next node counted.
  fn open(&mut self, tree: u32, ty: Option<TyId>, judged: bool) {
    self.counts.insert((tree, ty), Count::Counting);
    let base = self.counting.len();
    self.frames.push(Frame {
      tree,
      ty,
      base,
      rows: 0,
      judged,
      fits: true,
    });
    if let Some(root) = self
      .raw
      .trees
      .get(tree as usize)
      .map(|tree: &RawTree| tree.root)
    {
      self.counting.push(Counted::Node {
        node: root,
        ty,
        slot: None,
      });
    }
  }

  /// Keeps `verdict` as the count of every default being counted, and ends the count: its answer.
  fn verdict(&mut self, verdict: Count) -> Count {
    while let Some(frame) = self.frames.pop() {
      self.counts.insert((frame.tree, frame.ty), verdict);
    }
    self.counting.clear();
    self.held.clear();
    verdict
  }

  /// The copy of the default counted on top does not fit, when it is judged: one that is not
  /// judged fits, its default judged where it is written.
  fn misfit(&mut self) {
    if let Some(frame) = self.frames.last_mut()
      && frame.judged
    {
      frame.fits = false;
    }
  }

  /// Adds `rows` to the default being counted on top: a sum past `u32::MAX`, or, when `charged`,
  /// past `Limits::max_value_steps`, is past the budget.
  fn add_rows(&mut self, rows: u32, charged: bool) -> Result<(), Count> {
    let max = self.values.max();
    let Some(frame) = self.frames.last_mut() else {
      return Ok(());
    };
    match frame.rows.checked_add(rows) {
      Some(sum) if !charged || sum <= max => {
        frame.rows = sum;
        Ok(())
      }
      _ => Err(Count::Past),
    }
  }

  /// Counts `node` against `ty` into the default being counted on top, as [`count`](Self::count)
  /// says, or fails with the count's verdict; in a judged count, an element or a key of a set or a
  /// map being searched whose identity goes in `slot`.
  fn count_node(
    &mut self,
    node: u32,
    ty: Option<TyId>,
    slot: Option<u32>,
    charged: bool,
  ) -> Result<(), Count> {
    let Some(raw) = self.raw.nodes.get(node as usize).copied() else {
      return Ok(());
    };
    let judged = self.frames.last().is_some_and(|frame: &Frame| frame.judged);
    // Under a type its default is not written against, a node is a step, paid before it is
    // counted: its source paid for it under that type alone.
    if judged && charged && !self.charge(1) {
      return Err(Count::Past);
    }
    self.add_rows(1, charged)?;
    match self.classify(raw.kind, ty) {
      Class::NoValue | Class::Null => {}
      Class::NotNull => self.misfit(),
      Class::Leaf(base) => {
        if judged {
          self.judge_leaf(node, base, slot)?;
        }
      }
      Class::List(element) => {
        self.count_items(node, raw, (element, element), None);
      }
      Class::Set(element) => {
        self.count_items(node, raw, (element, element), judged.then_some(Held::Set));
      }
      Class::Map(key, value) => {
        // A last key with no value is dropped, as the copy drops it.
        if self.count_items(node, raw, (key, value), judged.then_some(Held::Map)) % 2 == 1 {
          let _ = self.counting.pop();
        }
      }
      Class::Lone(element) => {
        let (levels, under) = self.list_levels(element);
        self.add_rows(levels, charged)?;
        if self.is_list(under) {
          // A list type whose element was not interned before it, which interning never makes:
          // the value is one row more, and nothing is under it.
          self.add_rows(1, charged)?;
        } else {
          self.counting.push(Counted::Node {
            node,
            ty: Some(under),
            slot: None,
          });
        }
      }
      Class::Object { def, first } => {
        let fields = self.fields_of(node, def, first);
        // Each field's type is read under the object's type arguments, as the copy reads it.
        let env = ty.map(|ty: TyId| (ty, def));
        if judged && !self.object_fits(def, fields, charged, env)? {
          self.misfit();
        }
        let mut merge = self.merge(def, fields);
        while let Some(next) = self.next(&mut merge) {
          match next {
            Next::Set(set) => {
              let expected = self
                .under(set.field, env, !charged)
                .map_err(unbuilt_count)?;
              self.counting.push(Counted::Node {
                node: set.node,
                ty: expected.ty,
                slot: None,
              });
            }
            Next::Default(field, tree) => {
              // A read is paid before it is pushed: no charge covers work pushed before it.
              if charged && !self.charge(1) {
                return Err(Count::Past);
              }
              let expected = self.under(field, env, !charged).map_err(unbuilt_count)?;
              self.counting.push(Counted::Fill {
                tree,
                ty: expected.ty,
                judged: expected.rebuilt,
              });
            }
          }
        }
      }
    }
    Ok(())
  }

  /// Judges `node`, a leaf of the default counted on top, against `base`, as the written mode reads
  /// it and writing no row: one it refuses does not fit, but for a path whose root lowering does
  /// not know, which lowering reported. A conversion the budget cannot pay for is past it. Its
  /// identity, an element's or a key's, goes in `slot`.
  fn judge_leaf(&mut self, node: u32, base: Base, slot: Option<u32>) -> Result<(), Count> {
    let tree = self
      .frames
      .last()
      .and_then(|frame: &Frame| self.raw.trees.get(frame.tree as usize).copied());
    let Some(tree) = tree else {
      return Ok(());
    };
    match self.leaf(None, node, tree, base) {
      Ok(ident) => {
        if let Some(slot) = slot
          && let Some(held) = self.held.get_mut(slot as usize)
        {
          *held = ident;
        }
      }
      Err(Refused {
        why: Why::Costly { .. },
        ..
      }) => return Err(Count::Past),
      Err(refused) => {
        if !self.unknown_root(refused.node) {
          self.misfit();
        }
      }
    }
    Ok(())
  }

  /// Whether the object literal of the input object `def` that sets `fields` is refused nothing
  /// where it is written, as a judged count reads it, its fields' types under `env`: no field `def`
  /// does not declare; for a OneOf input object, one field, not `null`; and every required field
  /// set, each passed left out a step, one that may be required read under `env`, the first that is
  /// missing ending the walk.
  fn object_fits(
    &mut self,
    def: DefId,
    fields: Fields,
    charged: bool,
    env: Option<(TyId, DefId)>,
  ) -> Result<bool, Count> {
    if fields.written > fields.len {
      return Ok(false);
    }
    if self.one_of.binary_search(&def).is_ok()
      && (fields.written != 1
        || self
          .set_fields
          .get(fields.start as usize)
          .is_some_and(|set: &SetField| matches!(self.kind(set.node), RawKind::Null)))
    {
      return Ok(false);
    }
    let Some(input) = self.input(def) else {
      return Ok(true);
    };
    let (start, end) = input.required;
    let set_end = fields.start.saturating_add(fields.len);
    let mut set = fields.start;
    for row in start..end {
      let Some(&(field, maybe)) = self.required.get(row as usize) else {
        break;
      };
      while set < set_end
        && self
          .set_field_at(set)
          .is_some_and(|kept: MemberId| kept < field)
      {
        set = set.saturating_add(1);
      }
      if set < set_end && self.set_field_at(set) == Some(field) {
        continue;
      }
      if charged && !self.charge(1) {
        return Err(Count::Past);
      }
      if maybe
        && !self
          .under(field, env, !charged)
          .map_err(unbuilt_count)?
          .non_null
      {
        continue;
      }
      return Ok(false);
    }
    Ok(true)
  }

  /// Whether the elements of a set, or the keys of a map, as `held` says, whose identities are the
  /// slots from `start` on, hold none twice: each identity rendered once, the shared kernel finding
  /// the first of each. The slots are let go.
  fn held_once(&mut self, start: u32, held: Held) -> bool {
    self.key_bytes.clear();
    self.keyed.clear();
    let step = match held {
      Held::Map => 2,
      Held::List | Held::Set => 1,
    };
    let mut at = start as usize;
    while let Some(&ident) = self.held.get(at) {
      at = at.saturating_add(step);
      let Some(ident) = ident else {
        continue;
      };
      let from = dense(self.key_bytes.len());
      self.render(ident);
      self.keyed.push((from, dense(self.key_bytes.len()), 0));
    }
    let keys: &[u8] = &self.key_bytes;
    let keyed: &[(u32, u32, u32)] = &self.keyed;
    let mut duplicates: Duplicates<&[u8]> =
      Duplicates::over(keyed.len(), |position: usize| key_at(keys, keyed, position));
    let twice = (0..keyed.len()).any(|position: usize| {
      duplicates
        .first(position, key_at(keys, keyed, position))
        .is_some()
    });
    self.held.truncate(start as usize);
    !twice
  }

  /// Whether `node` is a path whose root lowering does not know, which lowering reported.
  fn unknown_root(&self, node: u32) -> bool {
    match self.kind(node) {
      RawKind::Path(row) => self
        .raw
        .paths
        .get(row as usize)
        .is_some_and(|path: &RawPath| path.written.is_none()),
      _ => false,
    }
  }

  /// Pushes each child of `node`, whose node is `raw`, to be counted: those at even places against
  /// `types.0`, and those at odd places against `types.1`; when `held` says the node is a set or a
  /// map a judged count searches, with a slot for each one's identity, under the entry that
  /// searches them. Answers how many it pushed.
  fn count_items(
    &mut self,
    node: u32,
    raw: RawNode,
    types: (TyId, TyId),
    held: Option<Held>,
  ) -> u32 {
    if let Some(held) = held {
      let start = dense(self.held.len());
      self.counting.push(Counted::Held { start, held });
    }
    let mut pushed: u32 = 0;
    let mut child = node.saturating_add(1);
    while child < raw.after {
      let ty = if pushed % 2 == 1 { types.1 } else { types.0 };
      let slot = held.map(|_: Held| {
        let slot = dense(self.held.len());
        self.held.push(None);
        slot
      });
      self.counting.push(Counted::Node {
        node: child,
        ty: Some(ty),
        slot,
      });
      pushed = pushed.saturating_add(1);
      let next = self
        .raw
        .nodes
        .get(child as usize)
        .map_or(raw.after, |held: &RawNode| held.after);
      if next <= child {
        break;
      }
      child = next;
    }
    pushed
  }

  /// The merge of `fields`, what an object literal sets, with the defaults of the input object
  /// `def`.
  fn merge(&self, def: DefId, fields: Fields) -> Merge {
    let (default, default_end) = self
      .input(def)
      .map_or((0, 0), |input: Input| input.defaults);
    Merge {
      set: fields.start,
      set_end: fields.start.saturating_add(fields.len),
      default,
      default_end,
    }
  }

  /// What `merge` gives next, in the input object's order: of the next field the literal sets and
  /// the next default, the one of the lesser field, a field set before the default of the same
  /// field, which is then passed by.
  fn next(&self, merge: &mut Merge) -> Option<Next> {
    let set = if merge.set < merge.set_end {
      self.set_fields.get(merge.set as usize).copied()
    } else {
      None
    };
    let default = if merge.default < merge.default_end {
      self.defaulted.get(merge.default as usize).copied()
    } else {
      None
    };
    match (set, default) {
      (Some(set), Some((field, _))) if set.field <= field => {
        merge.set = merge.set.saturating_add(1);
        if set.field == field {
          merge.default = merge.default.saturating_add(1);
        }
        Some(Next::Set(set))
      }
      (Some(set), None) => {
        merge.set = merge.set.saturating_add(1);
        Some(Next::Set(set))
      }
      (_, Some((field, tree))) => {
        merge.default = merge.default.saturating_add(1);
        Some(Next::Default(field, tree))
      }
      (None, None) => None,
    }
  }

  /// How many levels of list `ty` is before its element is no list, and the type under them: 0
  /// and `ty` itself for a type that is no list. Each list type's answer is kept once read, so the
  /// types of one program are walked once in all.
  fn list_levels(&mut self, ty: TyId) -> (u32, TyId) {
    self.chain.clear();
    let mut at = ty;
    let (mut levels, under) = loop {
      if let Some(&kept) = self.levels.get(&at) {
        break kept;
      }
      match self.builder.ty_shape(at) {
        // An element is interned before its list, so the walk goes down and ends.
        Some(Shape::List(element)) if element < at => {
          self.chain.push(at);
          at = element;
        }
        _ => break (0, at),
      }
    };
    while let Some(list) = self.chain.pop() {
      levels = levels.saturating_add(1);
      self.levels.insert(list, (levels, under));
    }
    (levels, under)
  }

  /// Whether `ty` is a list type.
  fn is_list(&self, ty: TyId) -> bool {
    matches!(self.builder.ty_shape(ty), Some(Shape::List(_)))
  }

  /// Reports `argument`, required, as left out by `application`, whose path is at `at`, with a
  /// label at the argument's declaration.
  fn missing(&mut self, application: ApplicationId, argument: MemberId, at: Location) {
    let directive: &[u8] = match self.builder.applied(application) {
      Some((Resolution::Resolved(def), _, _)) => self.builder.def_name(def),
      _ => b"",
    };
    let name: &[u8] = match self.builder.member_name(argument) {
      Some(sym) => self.builder.spelling(sym),
      None => b"",
    };
    let message = format!(
      "`@{}` requires its argument `{}`, and this application leaves it out",
      Excerpt(directive),
      Excerpt(name)
    );
    let mut report = Diagnostic::builder(schema::MISSING_REQUIRED_DIRECTIVE_ARGUMENT, at)
      .message(message)
      .primary_label("missing required directive argument");
    if let Some(declared) = self.builder.member_location(argument)
      && !declared.is_entire()
    {
      report = report.label(declared, "required here");
    }
    self.reporter.report(report.build());
  }

  /// Reports that the arguments the application whose path is at `at` leaves out are past the
  /// budget, or, as `stop` says, take a type that cannot be built under its type arguments.
  fn starved(&mut self, at: Location, stop: Stop) {
    let (message, help) = match stop {
      Stop::Steps => (
        format!(
          "the arguments this application leaves out pass `Limits::max_value_steps`, {}, the \
           steps the analysis may take to complete values, and are not filled",
          self.values.max()
        ),
        "raise the limit with `Limits::with_max_value_steps`, or write the arguments",
      ),
      Stop::Built(_) => self.stopped(stop, "the arguments this application leaves out take"),
    };
    self.limit(at, message, help);
  }

  /// Reports `resource-limit` at `at`, saying `message`, with `help`.
  fn limit(&mut self, at: Location, message: String, help: &'static str) {
    let kind = DiagnosticKind::ResourceLimit;
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(message)
        .primary_label(kind.title())
        .help(help)
        .build(),
    );
  }

  /// What a report says, and its help, of a type `what` names, followed by its verb, that `stop`
  /// kept from being read under type arguments.
  fn stopped(&self, stop: Stop, what: &str) -> (String, &'static str) {
    let limits = self.types.limits();
    match stop {
      Stop::Steps => (
        format!(
          "{what} a type that could not be read under these type arguments within \
           `Limits::max_value_steps`, {}, the steps the analysis may take to complete values",
          self.values.max()
        ),
        "raise the limit with `Limits::with_max_value_steps`",
      ),
      Stop::Built(Limit::Depth) => (
        format!(
          "{what} a type that, under these type arguments, nests deeper than \
           `Limits::max_generic_depth`, {}, admits",
          limits.max_generic_depth()
        ),
        "raise the bound with `Limits::with_max_generic_depth`, or nest types less deeply",
      ),
      Stop::Built(Limit::Instantiations) => (
        format!(
          "{what} a type that, under these type arguments, is one more instantiation of a \
           generic definition than `Limits::max_instantiations`, {}, admits",
          limits.max_instantiations()
        ),
        "raise the bound with `Limits::with_max_instantiations`, or instantiate fewer types",
      ),
      // The match's arm alone: a rebuild fills nothing, and never draws on this limit.
      Stop::Built(Limit::Substitutions) => (
        format!(
          "{what} a type that could not be read under these type arguments within \
           `Limits::max_substitutions`, {}",
          limits.max_substitutions()
        ),
        "raise the bound with `Limits::with_max_substitutions`",
      ),
    }
  }

  /// Reports, at the object literal `at` that leaves `field` out, whose type a rebuild under type
  /// arguments made, that the copy of its default, whose tree is `tree`, does not fit the type
  /// those arguments give it, labelled at the default: the one diagnostic of that copy.
  fn unfit_field(&mut self, at: Location, field: MemberId, tree: u32) {
    let Some(RawTree { root, source, .. }) = self.raw.trees.get(tree as usize).copied() else {
      return;
    };
    let Some(node) = self.raw.nodes.get(root as usize).copied() else {
      return;
    };
    let name: &[u8] = match self.builder.member_name(field) {
      Some(sym) => self.builder.spelling(sym),
      None => b"",
    };
    let name = Excerpt(name);
    let message = format!(
      "this literal leaves out `{name}`, whose default `{}` does not fit the type `{name}` takes \
       under these type arguments",
      Excerpt(self.raw.text(source, node.span))
    );
    let (code, label) = Site::Default.refusal();
    self.reporter.report(
      Diagnostic::builder(code, at)
        .message(message)
        .primary_label(label)
        .label(Location::new(source, node.span), "this default")
        .build(),
    );
  }

  /// Reports, at the path `at` of an application that leaves `argument` out, that the copy of its
  /// default, whose tree is `tree`, does not fit the type the application's type arguments give
  /// it, labelled at the default: the one diagnostic of that copy.
  fn unfit_default(&mut self, at: Location, argument: MemberId, tree: u32) {
    let Some(RawTree { root, source, .. }) = self.raw.trees.get(tree as usize).copied() else {
      return;
    };
    let Some(node) = self.raw.nodes.get(root as usize).copied() else {
      return;
    };
    let name: &[u8] = match self.builder.member_name(argument) {
      Some(sym) => self.builder.spelling(sym),
      None => b"",
    };
    let name = Excerpt(name);
    let message = format!(
      "under this application's type arguments, the default `{}` of `{name}` does not fit the \
       type `{name}` takes",
      Excerpt(self.raw.text(source, node.span))
    );
    let (code, label) = Site::Default.refusal();
    self.reporter.report(
      Diagnostic::builder(code, at)
        .message(message)
        .primary_label(label)
        .label(Location::new(source, node.span), "this default")
        .build(),
    );
  }

  /// The argument `argument` of the application `applying` says, as written: its value coerced
  /// against the argument's type under the application's type arguments. A required argument,
  /// non-null there and with no default, given `null` is reported as left out, at its name, and
  /// its value is unresolved. A value whose type cannot be built under the arguments is one
  /// unresolved row, typed with the type the argument is written with, `resource-limit` at it.
  fn written_argument(&mut self, applying: Applying, argument: RawArgument) {
    let application = applying.application;
    let expected = match self.under(argument.argument, applying.env, applying.library) {
      Ok(expected) => expected,
      Err(stop) => {
        let written = self.member_type(argument.argument);
        if let Some(value) = self.unbuilt_value(argument.tree, written, stop) {
          self.builder.push_application_argument(
            application,
            argument.argument,
            Some(argument.name),
            value,
          );
        }
        return;
      }
    };
    let (ty, non_null) = (expected.ty, expected.non_null);
    let required = non_null && !self.raw.defaults.contains_key(&argument.argument);
    let null = self
      .raw
      .trees
      .get(argument.tree as usize)
      .and_then(|tree: &RawTree| self.raw.nodes.get(tree.root as usize))
      .is_some_and(|node: &RawNode| matches!(node.kind, RawKind::Null));
    let value = if required && null {
      self.null_for_required(application, argument, ty)
    } else {
      self.value(
        argument.tree,
        ty,
        Mode::Written(Site::Argument),
        expected.rebuilt,
      )
    };
    if let Some(value) = value {
      self.builder.push_application_argument(
        application,
        argument.argument,
        Some(argument.name),
        value,
      );
    }
  }

  /// The value of the tree `tree`, written where it takes a type that `stop` kept from being read
  /// under type arguments: one unresolved row at its root, typed `ty`, the type it is written
  /// against, and `resource-limit` at it.
  fn unbuilt_value(&mut self, tree: u32, ty: Option<TyId>, stop: Stop) -> Option<ValueId> {
    let RawTree { root, source, .. } = self.raw.trees.get(tree as usize).copied()?;
    let node = self.raw.nodes.get(root as usize).copied()?;
    let at = Location::new(source, node.span);
    let id = self.builder.push_value(at, true);
    if let Some(ty) = ty {
      self.builder.set_value_ty(id, ty);
    }
    let (message, help) = self.stopped(stop, "this value takes");
    self.limit(at, message, help);
    Some(id)
  }

  /// The value of `argument`, a required argument of `application` given `null`, of type `ty`:
  /// one unresolved row, and one diagnostic, at the argument's name.
  fn null_for_required(
    &mut self,
    application: ApplicationId,
    argument: RawArgument,
    ty: Option<TyId>,
  ) -> Option<ValueId> {
    let RawTree { root, source, .. } = self.raw.trees.get(argument.tree as usize).copied()?;
    let node = self.raw.nodes.get(root as usize).copied()?;
    let id = self
      .builder
      .push_value(Location::new(source, node.span), true);
    if let Some(ty) = ty {
      self.builder.set_value_ty(id, ty);
    }
    let directive: &[u8] = match self.builder.applied(application) {
      Some((Resolution::Resolved(def), _, _)) => self.builder.def_name(def),
      _ => b"",
    };
    let name = argument.name;
    let written: &[u8] = match name.span() {
      Some(span) => self.raw.text(name.source(), span),
      None => b"",
    };
    let message = format!(
      "the argument `{}` of `@{}` is required, and `null` is given",
      Excerpt(written),
      Excerpt(directive)
    );
    self.reporter.report(
      Diagnostic::builder(schema::MISSING_REQUIRED_DIRECTIVE_ARGUMENT, name)
        .message(message)
        .primary_label("missing required directive argument")
        .build(),
    );
    Some(id)
  }

  /// Makes the rows of the tree `tree` against `ty`, as `mode` says, `rebuilt` when a rebuild under
  /// type arguments made `ty`: the value, then what it holds, in pre-order. Answers the value's id,
  /// `None` for a tree lowering did not make.
  fn value(&mut self, tree: u32, ty: Option<TyId>, mode: Mode, rebuilt: bool) -> Option<ValueId> {
    let tree = self.raw.trees.get(tree as usize).copied()?;
    let base = self.items.len();
    self.stack.push(Task::Make(Making {
      node: tree.root,
      ty,
      tree,
      mode,
      rebuilt,
    }));
    while let Some(task) = self.stack.pop() {
      match task {
        Task::Make(making) => self.make(making),
        Task::Refuse(making, why) => self.unfit(making, why),
        Task::Close {
          value,
          from,
          held,
          check,
        } => self.close(value, from, held, check),
        Task::CloseObject {
          object,
          from,
          entries,
        } => self.close_object(object, from, entries),
      }
    }
    let value = self.items.get(base).copied();
    self.items.truncate(base);
    self.idents.truncate(base);
    value
  }

  /// Makes the row of `making`'s node, as it says, and pushes what it holds: the row's id goes to
  /// the open values' rows.
  fn make(&mut self, making: Making) {
    let Making {
      node,
      ty,
      tree,
      mode,
      ..
    } = making;
    let Some(raw) = self.raw.nodes.get(node as usize).copied() else {
      return;
    };
    let source = tree.source;
    let at = Location::new(source, raw.span);
    let id = self.push(at, mode, node);
    if let Some(ty) = ty {
      self.builder.set_value_ty(id, ty);
    }
    match self.classify(raw.kind, ty) {
      Class::NoValue => {
        self.literal(id, node, source);
        self.path(id, raw.kind);
      }
      Class::Null => self.builder.set_null(id),
      Class::NotNull => self.refuse(
        Refused {
          node,
          why: Why::Null,
        },
        source,
        ty,
        mode,
      ),
      Class::List(element) => self.open_items(id, making, Held::List, (element, element)),
      Class::Set(element) => self.open_items(id, making, Held::Set, (element, element)),
      Class::Map(key, value) => self.open_items(id, making, Held::Map, (key, value)),
      Class::Lone(element) => {
        if mode.written() && source >= self.supplied && !self.charge(1) {
          self.path(id, raw.kind);
          self.lone_limit(at);
        } else {
          let from = self.items.len();
          self.stack.push(Task::Close {
            value: id,
            from,
            held: Held::List,
            check: None,
          });
          self
            .stack
            .push(Task::Make(making.child(node, Some(element))));
        }
      }
      Class::Object { def, first } => self.object(id, making, def, first),
      Class::Leaf(base) => {
        self.path(id, raw.kind);
        match self.leaf(Some(id), node, tree, base) {
          Ok(ident) => {
            if let Some(last) = self.idents.last_mut() {
              last.0 = ident;
            }
          }
          Err(refused) => self.refuse(refused, source, ty, mode),
        }
      }
    }
  }

  /// Pushes the task that closes `id`, the row of `making`'s node, a list, a set or a map as `held`
  /// says, then a task for each of the node's children over it, so that the first is made first:
  /// each against `types.0`; in a map, each key against `types.0` and each value against
  /// `types.1`, a last key with no value dropped.
  fn open_items(&mut self, id: ValueId, making: Making, held: Held, types: (TyId, TyId)) {
    let Some(raw) = self.raw.nodes.get(making.node as usize).copied() else {
      return;
    };
    let from = self.items.len();
    let check = match held {
      Held::Set | Held::Map if making.mode.written() => Some(making.tree.source),
      Held::List | Held::Set | Held::Map => None,
    };
    self.stack.push(Task::Close {
      value: id,
      from,
      held,
      check,
    });
    self.children.clear();
    let mut child = making.node.saturating_add(1);
    while child < raw.after {
      self.children.push(child);
      let next = self
        .raw
        .nodes
        .get(child as usize)
        .map_or(raw.after, |node: &RawNode| node.after);
      if next <= child {
        break;
      }
      child = next;
    }
    let (first, second) = types;
    let made = match held {
      Held::Map => self.children.len() - self.children.len() % 2,
      Held::List | Held::Set => self.children.len(),
    };
    for (index, &child) in self.children.iter().enumerate().take(made).rev() {
      let ty = if matches!(held, Held::Map) && index % 2 == 1 {
        second
      } else {
        first
      };
      self.stack.push(Task::Make(making.child(child, Some(ty))));
    }
  }

  /// Gives `value`, a list, a set or a map as `held` says, the rows made since `from` among the
  /// open values' rows: a map's in pairs, each key and then its value. A set or a map written in
  /// the source `check` is first searched for an element, or a key, held twice.
  fn close(&mut self, value: ValueId, from: usize, held: Held, check: Option<u32>) {
    if let Some(source) = check {
      self.held_twice(from, held, source);
    }
    let made = self.items.get(from..).unwrap_or_default();
    match held {
      Held::List => self.builder.set_list(value, made),
      Held::Set => self.builder.set_set(value, made),
      Held::Map => {
        self.pairs.clear();
        let mut rest = made;
        while let [key, item, tail @ ..] = rest {
          self.pairs.push((*key, *item));
          rest = tail;
        }
        self.builder.set_map(value, &self.pairs);
      }
    }
    self.items.truncate(from);
    self.idents.truncate(from);
  }

  /// Reports each element of the set, or each key of the map, as `held` says, whose rows are those
  /// made since `from` among the open values' rows, written in the source `source`, that an
  /// earlier one holds already: at the later, with a label at the first. Each identity is rendered
  /// once into one buffer, and the shared kernel finds the first of each key, by a scan up to its
  /// narrow width and by one sort above it. An element or a key with no identity is compared with
  /// nothing.
  fn held_twice(&mut self, from: usize, held: Held, source: u32) {
    self.key_bytes.clear();
    self.keyed.clear();
    self.twice.clear();
    let step = match held {
      Held::Map => 2,
      Held::List | Held::Set => 1,
    };
    let mut at = from;
    while let Some(&(ident, node)) = self.idents.get(at) {
      at = at.saturating_add(step);
      let Some(ident) = ident else {
        continue;
      };
      let start = dense(self.key_bytes.len());
      self.render(ident);
      self.keyed.push((start, dense(self.key_bytes.len()), node));
    }
    {
      // The keys borrow the buffer: the pairs are gathered first, and reported after.
      let keys: &[u8] = &self.key_bytes;
      let keyed: &[(u32, u32, u32)] = &self.keyed;
      let mut duplicates: Duplicates<&[u8]> =
        Duplicates::over(keyed.len(), |position: usize| key_at(keys, keyed, position));
      for (position, &(_, _, node)) in keyed.iter().enumerate() {
        if let Some(first) = duplicates.first(position, key_at(keys, keyed, position))
          && let Some(&(_, _, held_first)) = keyed.get(first)
        {
          self.twice.push((node, held_first));
        }
      }
    }
    for index in 0..self.twice.len() {
      if let Some(&(later, first)) = self.twice.get(index) {
        self.held_again(later, first, held, source);
      }
    }
  }

  /// Appends `ident`'s canonical bytes to the buffer of keys: a boolean as one byte; an integer as
  /// its sign and its magnitude; an `id`'s integer as its decimal digits, `-` first below zero;
  /// a float as its bits; a text or a byte string as its bytes; an enum value as its member.
  fn render(&mut self, ident: Ident) {
    match ident {
      Ident::Bool(flag) => self.key_bytes.push(u8::from(flag)),
      Ident::Int {
        negative,
        start,
        len,
      } => {
        self.key_bytes.push(u8::from(negative));
        let magnitude = run_of(&self.magnitudes, start, len);
        self.key_bytes.extend_from_slice(magnitude);
      }
      Ident::Decimal {
        negative,
        start,
        len,
      } => {
        // An `id`'s integer is in `i32`'s range: at most four bytes, eleven characters.
        let mut value: u64 = 0;
        for (place, byte) in run_of(&self.magnitudes, start, len)
          .iter()
          .enumerate()
          .take(8)
        {
          value |= u64::from(*byte) << (8 * place);
        }
        if negative {
          self.key_bytes.push(b'-');
        }
        let digits = self.key_bytes.len();
        loop {
          self
            .key_bytes
            .push(b'0'.saturating_add(u8::try_from(value % 10).unwrap_or_default()));
          value /= 10;
          if value == 0 {
            break;
          }
        }
        if let Some(written) = self.key_bytes.get_mut(digits..) {
          written.reverse();
        }
      }
      Ident::Float(bits) => self.key_bytes.extend_from_slice(&bits.to_le_bytes()),
      Ident::Text(text) => self
        .key_bytes
        .extend_from_slice(self.builder.kept_text(text).as_bytes()),
      Ident::Bytes(bytes) => self
        .key_bytes
        .extend_from_slice(self.builder.kept_bytes(bytes)),
      Ident::Member(member) => self
        .key_bytes
        .extend_from_slice(&member.get().to_le_bytes()),
    }
  }

  /// Reports the element or the key of node `later`, of a set or a map as `held` says, written in
  /// the source `source`, that the one of node `first` holds already.
  fn held_again(&mut self, later: u32, first: u32, held: Held, source: u32) {
    let (Some(at), Some(before)) = (
      self.raw.nodes.get(later as usize).copied(),
      self.raw.nodes.get(first as usize).copied(),
    ) else {
      return;
    };
    let literal = Excerpt(self.raw.text(source, at.span));
    let message = match held {
      Held::Map => format!("this map holds the key `{literal}` twice"),
      Held::List | Held::Set => format!("this set holds `{literal}` twice"),
    };
    let kind = DiagnosticKind::CollectionLiteral;
    self.reporter.report(
      Diagnostic::builder(kind.code(), Location::new(source, at.span))
        .severity(kind.severity())
        .message(message)
        .primary_label(kind.title())
        .label(Location::new(source, before.span), "first held here")
        .help("a set holds each element once, and a map each key once")
        .build(),
    );
  }

  /// Makes `id`, the row of `making`'s node, an object literal against the input object `def`,
  /// whose fields' rows of the trees start at `first`, and pushes its entries in the input
  /// object's order, then the task that closes it: each field it sets, and each default it leaves
  /// out, a copy. Where it is written, its unknown fields are reported, a OneOf input object's
  /// count of fields, and the required fields it leaves out, before any default is filled; a
  /// default is asked for as [`ask`](Self::ask) says, and once the budget cannot pay, the literal
  /// is reported once and its written fields alone are read on. In a copy each default it leaves
  /// out is copied with nothing asked and no count read: the count the copy was charged for read
  /// each of their counts and found it exact, so their rows are among those it paid for.
  fn object(&mut self, id: ValueId, making: Making, def: DefId, first: u32) {
    let Making {
      node, tree, mode, ..
    } = making;
    let Some(raw) = self.raw.nodes.get(node as usize).copied() else {
      return;
    };
    let source = tree.source;
    let at = Location::new(source, raw.span);
    let library = source < self.supplied;
    let fields = self.fields_of(node, def, first);
    let one_of = self.one_of.binary_search(&def).is_ok();
    let mut merge = self.merge(def, fields);
    // Each field's type is read under the object's type arguments.
    let env = making.ty.map(|ty: TyId| (ty, def));
    if mode.written() {
      self.unknown_fields(node, def, first, source);
      if one_of && fields.written != 1 {
        let why = Why::OneOf {
          def,
          written: fields.written,
        };
        self.refuse(Refused { node, why }, source, making.ty, mode);
      }
      if !self.required_left_out(def, fields, at, library, env) {
        merge.default = merge.default_end;
      }
    }
    let from = self.items.len();
    let entries = self.entries.len();
    self.stack.push(Task::CloseObject {
      object: id,
      from,
      entries,
    });
    // The entries' tasks are pushed in order, then reversed, so that the first is made first.
    let tasks = self.stack.len();
    let alone = one_of && fields.written == 1;
    // Whether a copy that does not fit under the object's type arguments is reported: once for it.
    let mut reported = false;
    while let Some(next) = self.next(&mut merge) {
      let (field, default) = match next {
        Next::Set(set) => {
          let task = self.entry_task(making, def, set, alone, env);
          self.stack.push(task);
          continue;
        }
        Next::Default(field, default) => (field, default),
      };
      let Some(default_tree) = self.raw.trees.get(default as usize).copied() else {
        continue;
      };
      // In a copy the type is the one its count read, and read already: the count would have
      // stopped at a type that cannot be built, and no copy would be made.
      let expected = match self.under(field, env, library) {
        Ok(expected) => expected,
        Err(stop) => {
          if mode.written() {
            self.object_starved(at, stop);
            merge.default = merge.default_end;
          }
          continue;
        }
      };
      let ty = expected.ty;
      // In a copy nothing is asked and no count is read: the count the copy was charged for read
      // this default's count, through this same reading of the literal, and it was exact, or no
      // copy would be made. So its rows are among those already paid for.
      if mode.written() {
        match self.ask(default, ty, library, expected.rebuilt) {
          Asked::Copy { fits, .. } => {
            // A literal whose type a rebuild made reports its copy that does not fit; one whose
            // type a written occurrence gave leaves it to that occurrence's instantiation.
            if !fits && making.rebuilt && !reported {
              reported = true;
              self.unfit_field(at, field, default);
            }
          }
          Asked::Absent => continue,
          Asked::Starved => {
            self.object_starved(at, Stop::Steps);
            merge.default = merge.default_end;
            continue;
          }
          Asked::Unbuilt(limit) => {
            self.object_starved(at, Stop::Built(limit));
            merge.default = merge.default_end;
            continue;
          }
        }
      }
      self.entries.push((field, None, ValueId::new(0)));
      self.stack.push(Task::Make(Making {
        node: default_tree.root,
        ty,
        tree: default_tree,
        mode: Mode::Copied,
        rebuilt: false,
      }));
    }
    if let Some(tail) = self.stack.get_mut(tasks..) {
      tail.reverse();
    }
  }

  /// The task that makes the value of `set`, a field an object literal of the input object `def`,
  /// made as `making` says, sets, and its entry, now open: the field's value against its type under
  /// `env`; or, for `null`, one unresolved row when the field is required there, or when it is the
  /// one field, `alone`, of a OneOf input object's literal. A type that cannot be built under `env`
  /// makes the value one unresolved row, typed with the type the field is written with.
  fn entry_task(
    &mut self,
    making: Making,
    def: DefId,
    set: SetField,
    alone: bool,
    env: Option<(TyId, DefId)>,
  ) -> Task {
    let name = Location::new(making.tree.source, set.name);
    self.entries.push((set.field, Some(name), ValueId::new(0)));
    let library = making.tree.source < self.supplied;
    let expected = match self.under(set.field, env, library) {
      Ok(expected) => expected,
      Err(stop) => {
        let written = self.member_type(set.field);
        let value = Making {
          rebuilt: false,
          ..making.child(set.node, written)
        };
        return Task::Refuse(value, Why::Unbuilt(stop));
      }
    };
    let value = Making {
      rebuilt: expected.rebuilt,
      ..making.child(set.node, expected.ty)
    };
    if !matches!(self.kind(set.node), RawKind::Null) {
      return Task::Make(value);
    }
    if expected.non_null && !self.raw.defaults.contains_key(&set.field) {
      return Task::Refuse(
        value,
        Why::Required {
          def,
          field: set.field,
        },
      );
    }
    if alone {
      return Task::Refuse(value, Why::OneOfNull { def });
    }
    Task::Make(value)
  }

  /// What the object literal `node` sets of the input object `def`, its fields' rows of the trees
  /// starting at `first`: read the first time it is asked for, and kept by the node and `def`. A
  /// field whose name is a symbol that names one of `def`'s fields sets the first field of that
  /// name, and the fields it sets are sorted by the field, one set twice kept in written order.
  fn fields_of(&mut self, node: u32, def: DefId, first: u32) -> Fields {
    if let Some(&kept) = self.objects.get(&(node, def)) {
      return kept;
    }
    let start = self.set_fields.len();
    let after = self
      .raw
      .nodes
      .get(node as usize)
      .map_or(node, |raw: &RawNode| raw.after);
    let mut written: u32 = 0;
    let mut child = node.saturating_add(1);
    let mut row = first;
    while child < after {
      written = written.saturating_add(1);
      if let Some(field) = self.raw.fields.get(row as usize).copied()
        && let Some(name) = field.name
        && let Some(&member) = self.members.get(&(def, name))
      {
        self.set_fields.push(SetField {
          field: member,
          name: field.at,
          node: child,
        });
      }
      let next = self
        .raw
        .nodes
        .get(child as usize)
        .map_or(after, |held: &RawNode| held.after);
      if next <= child {
        break;
      }
      child = next;
      row = row.saturating_add(1);
    }
    // A stable sort: two of one field keep their written order.
    if let Some(tail) = self.set_fields.get_mut(start..) {
      tail.sort_by_key(|set: &SetField| set.field);
    }
    let kept = Fields {
      start: dense(start),
      len: dense(self.set_fields.len().saturating_sub(start)),
      written,
    };
    self.objects.insert((node, def), kept);
    kept
  }

  /// Reports each field the object literal `node`, written in the source `source`, writes that
  /// `def` does not declare, its fields' rows of the trees starting at `first`: at its name.
  fn unknown_fields(&mut self, node: u32, def: DefId, first: u32, source: u32) {
    let after = self
      .raw
      .nodes
      .get(node as usize)
      .map_or(node, |raw: &RawNode| raw.after);
    let mut child = node.saturating_add(1);
    let mut row = first;
    while child < after {
      if let Some(field) = self.raw.fields.get(row as usize).copied() {
        let known = field
          .name
          .is_some_and(|name: Sym| self.members.contains_key(&(def, name)));
        if !known {
          self.unknown_field(def, field.at, source);
        }
      }
      let next = self
        .raw
        .nodes
        .get(child as usize)
        .map_or(after, |held: &RawNode| held.after);
      if next <= child {
        break;
      }
      child = next;
      row = row.saturating_add(1);
    }
  }

  /// Reports the field whose name is written at `at`, in the source `source`, that the input
  /// object `def` does not declare: with the nearest of `def`'s fields in the help, when one is
  /// within reach and the analysis's budget for near misses lasts.
  fn unknown_field(&mut self, def: DefId, at: SimpleSpan, source: u32) {
    let builder: &ModelBuilder = &*self.builder;
    let written: &[u8] = self.raw.text(source, at);
    let first = (def, Sym::new(0));
    let last = (def, Sym::new(u32::MAX));
    let mut candidates = self
      .members
      .range(first..=last)
      .map(|(&(_, sym), _): (&(DefId, Sym), &MemberId)| builder.spelling(sym));
    let near: Option<String> = self
      .scopes
      .nearest(written, &mut candidates)
      .map(|spelled: &[u8]| format!("did you mean `{}`?", Excerpt(spelled)));
    let message = format!(
      "the input object `{}` declares no field named `{}`",
      Excerpt(builder.def_name(def)),
      Excerpt(written)
    );
    let mut report = Diagnostic::builder(
      schema::UNDEFINED_INPUT_OBJECT_FIELD,
      Location::new(source, at),
    )
    .message(message)
    .primary_label("undefined input object field");
    if let Some(near) = near {
      report = report.help(near);
    }
    self.reporter.report(report.build());
  }

  /// Reports each required field of the input object `def` that the object literal at `at`
  /// leaves out, its fields set being `fields`, each a step charged first, but where the built-in
  /// library writes it, `library`; one that may be required is reported when its type under `env`
  /// is non-null. Answers `false`, once it has reported the literal past the budget, when the
  /// budget cannot pay, or a type cannot be built under `env`: the rest is not read.
  fn required_left_out(
    &mut self,
    def: DefId,
    fields: Fields,
    at: Location,
    library: bool,
    env: Option<(TyId, DefId)>,
  ) -> bool {
    let Some(input) = self.input(def) else {
      return true;
    };
    let (start, end) = input.required;
    let set_end = fields.start.saturating_add(fields.len);
    let mut set = fields.start;
    for row in start..end {
      let Some(&(field, maybe)) = self.required.get(row as usize) else {
        break;
      };
      // The fields it sets are in member order too: pass those before this one.
      while set < set_end
        && self
          .set_field_at(set)
          .is_some_and(|kept: MemberId| kept < field)
      {
        set = set.saturating_add(1);
      }
      if set < set_end && self.set_field_at(set) == Some(field) {
        continue;
      }
      if !library && !self.charge(1) {
        self.object_starved(at, Stop::Steps);
        return false;
      }
      if maybe {
        match self.under(field, env, library) {
          Ok(expected) if !expected.non_null => continue,
          Ok(_) => {}
          Err(stop) => {
            self.object_starved(at, stop);
            return false;
          }
        }
      }
      self.missing_field(def, field, at);
    }
    true
  }

  /// The field the kept row `row` of the fields object literals set sets.
  fn set_field_at(&self, row: u32) -> Option<MemberId> {
    self
      .set_fields
      .get(row as usize)
      .map(|set: &SetField| set.field)
  }

  /// Reports `field`, a required field of the input object `def`, as left out by the object
  /// literal at `at`, with a label at the field's declaration.
  fn missing_field(&mut self, def: DefId, field: MemberId, at: Location) {
    let name: &[u8] = match self.builder.member_name(field) {
      Some(sym) => self.builder.spelling(sym),
      None => b"",
    };
    let message = format!(
      "`{}` requires its field `{}`, and this literal leaves it out",
      Excerpt(self.builder.def_name(def)),
      Excerpt(name)
    );
    let mut report = Diagnostic::builder(schema::MISSING_REQUIRED_INPUT_OBJECT_FIELD, at)
      .message(message)
      .primary_label("missing required input object field");
    if let Some(declared) = self.builder.member_location(field)
      && !declared.is_entire()
    {
      report = report.label(declared, "required here");
    }
    self.reporter.report(report.build());
  }

  /// Reports that the fields the object literal at `at` leaves out are past the budget, or, as
  /// `stop` says, take a type that cannot be built under its input object's type arguments.
  fn object_starved(&mut self, at: Location, stop: Stop) {
    let (message, help) = match stop {
      Stop::Steps => (
        format!(
          "the fields this literal leaves out pass `Limits::max_value_steps`, {}, the steps the \
           analysis may take to complete values, and are not filled",
          self.values.max()
        ),
        "raise the limit with `Limits::with_max_value_steps`, or write the fields",
      ),
      Stop::Built(_) => self.stopped(stop, "the fields this literal leaves out take"),
    };
    self.limit(at, message, help);
  }

  /// Makes the row of `making`'s node one unresolved row, typed with its type, and reports why,
  /// `why`, where it is written: it holds nothing.
  fn unfit(&mut self, making: Making, why: Why) {
    let Making {
      node,
      ty,
      tree,
      mode,
      ..
    } = making;
    let Some(raw) = self.raw.nodes.get(node as usize).copied() else {
      return;
    };
    let id = self.push(Location::new(tree.source, raw.span), mode, node);
    if let Some(ty) = ty {
      self.builder.set_value_ty(id, ty);
    }
    self.refuse(Refused { node, why }, tree.source, ty, mode);
  }

  /// Appends the row of `node`, located at `at`, made as `mode` says, to the model's values and to
  /// the open values' rows, with no identity yet: its id. A copy's rows are tallied.
  fn push(&mut self, at: Location, mode: Mode, node: u32) -> ValueId {
    let id = self.builder.push_value(at, mode.written());
    self.items.push(id);
    self.idents.push((None, node));
    if !mode.written() {
      self.copied = self.copied.saturating_add(1);
    }
    id
  }

  /// Gives the input object `object` its entries: the open entries from `entries` on, each with
  /// its value, the rows made since `from` among the open values' rows.
  fn close_object(&mut self, object: ValueId, from: usize, entries: usize) {
    let made = self.items.get(from..).unwrap_or_default();
    let open = self.entries.get_mut(entries..).unwrap_or_default();
    let filled = open.len().min(made.len());
    for (entry, &value) in open.iter_mut().zip(made) {
      entry.2 = value;
    }
    self.builder.set_object(
      object,
      self
        .entries
        .get(entries..entries.saturating_add(filled))
        .unwrap_or_default(),
    );
    self.items.truncate(from);
    self.idents.truncate(from);
    self.entries.truncate(entries);
  }

  /// What `kind` is made against `ty`, told by the two alone: no literal is read.
  fn classify(&self, kind: RawKind, ty: Option<TyId>) -> Class {
    let Some(ty) = ty else {
      return Class::NoValue;
    };
    let builder: &ModelBuilder = &*self.builder;
    if !builder.ty_closed(ty) {
      return Class::NoValue;
    }
    let class = match builder.ty_shape(ty) {
      Some(Shape::Named(def)) => match (self.base(def, ty), kind) {
        (Some(Base::Object(def)), RawKind::Object(first)) => Class::Object { def, first },
        (Some(Base::Set(element)), RawKind::Set) => Class::Set(element),
        (Some(Base::Map(key, value)), RawKind::Map) => Class::Map(key, value),
        (Some(base), _) => Class::Leaf(base),
        (None, _) => return Class::NoValue,
      },
      Some(Shape::List(element)) => match kind {
        RawKind::List => Class::List(element),
        _ => Class::Lone(element),
      },
      Some(Shape::Param { .. } | Shape::Unresolved) | None => return Class::NoValue,
    };
    if matches!(kind, RawKind::Null) {
      return if builder.ty_non_null(ty) {
        Class::NotNull
      } else {
        Class::Null
      };
    }
    class
  }

  /// What the named type `ty`, of `def`, takes: `None` when it can take no value. A collection is
  /// asked first: a set of its one argument, or a map from its first to its second, the type's
  /// first argument non-null there however it is written; one of another count of arguments, or
  /// one its own package makes both, takes no value, the collection's arity being another rule's.
  fn base(&self, def: DefId, ty: TyId) -> Option<Base> {
    if let Some(collection) = self.builder.def_collection(def) {
      if self.both.binary_search(&def).is_ok() {
        return None;
      }
      return match (collection, self.builder.ty_args(ty)) {
        (Collection::Set, &[element]) => Some(Base::Set(element)),
        (Collection::Map, &[key, value]) => Some(Base::Map(key, value)),
        _ => None,
      };
    }
    if let Some(primitive) = self.primitive(def) {
      return Some(match primitive {
        Primitive::Bool => Base::Bool,
        Primitive::F32 => Base::Float { single: true },
        Primitive::F64 => Base::Float { single: false },
        Primitive::Int { signed, width } => Base::Int(Range {
          signed,
          width,
          non_zero: false,
        }),
      });
    }
    let builtins = self.builtins;
    if Some(def) == builtins.string {
      return Some(Base::String);
    }
    if Some(def) == builtins.id {
      return Some(Base::Id);
    }
    if Some(def) == builtins.bytes {
      return Some(Base::Bytes);
    }
    if Some(def) == builtins.non_zero {
      return self.non_zero(ty).map(Base::Int);
    }
    if Some(def) == builtins.type_ref {
      return Some(Base::TypeRef);
    }
    match self.builder.def_kind(def)? {
      DefKind::Enum => Some(Base::Enum(def)),
      DefKind::InputObject => Some(Base::Object(def)),
      DefKind::Scalar => Some(Base::Kept),
      DefKind::Object | DefKind::Interface | DefKind::Union | DefKind::Directive => None,
    }
  }

  /// The range `ty`, a named type of `NonZero`, takes: its one argument's, never zero, when that is
  /// a nullable named type, with no argument of its own, of an integer primitive at least one bit
  /// wide. `None` otherwise: the type takes no value, and `nonzero-argument` refuses it.
  fn non_zero(&self, ty: TyId) -> Option<Range> {
    let builder: &ModelBuilder = &*self.builder;
    let &[argument] = builder.ty_args(ty) else {
      return None;
    };
    if builder.ty_non_null(argument) || !builder.ty_args(argument).is_empty() {
      return None;
    }
    let Some(Shape::Named(def)) = builder.ty_shape(argument) else {
      return None;
    };
    match self.primitive(def)? {
      Primitive::Int { signed, width } if width >= 1 => Some(Range {
        signed,
        width,
        non_zero: true,
      }),
      _ => None,
    }
  }

  /// The primitive `def` is, when `core` materialised it.
  fn primitive(&self, def: DefId) -> Option<Primitive> {
    self
      .primitives
      .get(def.get() as usize)
      .filter(|&&(_, at): &&(Primitive, DefId)| at == def)
      .map(|&(primitive, _): &(Primitive, DefId)| primitive)
  }

  /// Makes `id`, the row of `node`, of `tree`, what `base` takes of it. Answers its identity, when
  /// it has one. With no `id` it reads the node exactly so, through the same kept reads, and writes
  /// no row: what does not fit is told by what tells it where the node is written.
  fn leaf(
    &mut self,
    id: Option<ValueId>,
    node: u32,
    tree: RawTree,
    base: Base,
  ) -> Result<Option<Ident>, Refused> {
    let source = tree.source;
    let kind = self.kind(node);
    let refused = move |why: Why| Refused { node, why };
    match (base, kind) {
      (Base::Kept, _) => {
        if let Some(id) = id {
          self.literal(id, node, source);
        }
        Ok(None)
      }
      (Base::Enum(def), RawKind::Path(row)) => self.enum_value(id, node, row, def, tree),
      (Base::TypeRef, RawKind::Path(row)) => self.type_value(id, node, row, tree),
      (Base::Bool, RawKind::Boolean(flag)) => {
        if let Some(id) = id {
          self.builder.set_bool(id, flag);
        }
        Ok(Some(Ident::Bool(flag)))
      }
      (Base::String | Base::Id, RawKind::String) => {
        let text = self.cooked(node, source).ok_or(refused(Why::Kind))?;
        if let Some(id) = id {
          self.builder.give_string(id, text);
        }
        Ok(Some(Ident::Text(text)))
      }
      (Base::Int(range), RawKind::Int(radix)) => {
        let (negative, converted) = self.int(id, node, source, radix, range)?;
        Ok(Some(Ident::Int {
          negative: negative && converted.len != 0,
          start: converted.start,
          len: converted.len,
        }))
      }
      (Base::Id, RawKind::Int(radix)) => {
        let (negative, converted) = self.int(id, node, source, radix, ID_RANGE)?;
        Ok(Some(Ident::Decimal {
          negative: negative && converted.len != 0,
          start: converted.start,
          len: converted.len,
        }))
      }
      (Base::Float { single }, RawKind::Float { .. } | RawKind::Int(_)) => {
        let bits = self.float(id, node, source, single)?;
        Ok(Some(Ident::Float(bits)))
      }
      (Base::Bytes, RawKind::String | RawKind::List) => {
        let bytes = self.bytes(node, source)?;
        if let Some(id) = id {
          self.builder.give_bytes(id, bytes);
        }
        Ok(Some(Ident::Bytes(bytes)))
      }
      // Every literal a base does not take: a collection takes its own literal alone, and a
      // collection literal is a value of its collection alone.
      _ => {
        let expected = match base {
          Base::Set(_) => Some(Collection::Set),
          Base::Map(..) => Some(Collection::Map),
          _ => None,
        };
        if expected.is_some() || matches!(kind, RawKind::Set | RawKind::Map) {
          Err(refused(Why::Collection { expected }))
        } else {
          Err(refused(Why::Kind))
        }
      }
    }
  }

  /// Makes `id` the integer `node` is, of `radix`, when it is in `range`, and no row with no `id`.
  /// Its digits are counted first, and a count past what the width holds refuses it unconverted.
  /// Answers whether it is written below zero, and its magnitude.
  fn int(
    &mut self,
    id: Option<ValueId>,
    node: u32,
    source: u32,
    radix: Radix,
    range: Range,
  ) -> Result<(bool, Converted), Refused> {
    let refused = move |why: Why| Refused { node, why };
    let scanned = self
      .scanned(node, source, radix)
      .ok_or(refused(Why::Kind))?;
    if range.non_zero && scanned.significant == 0 {
      return Err(refused(Why::Zero));
    }
    let out = Why::Range(range);
    if scanned.significant > number::max_digits(radix, range.width) {
      return Err(refused(out));
    }
    let Some(converted) = self.converted(node, source, radix, scanned) else {
      return Err(refused(Why::Costly {
        digits: scanned.significant,
        cost: number::decimal_cost(scanned.significant),
      }));
    };
    if !number::fits(
      scanned.negative,
      converted.bits,
      converted.power,
      range.signed,
      range.width,
    ) {
      return Err(refused(out));
    }
    if let Some(id) = id {
      let row = self.int_row(node, source, scanned.negative, converted);
      self.builder.give_int(id, row);
    }
    Ok((scanned.negative, converted))
  }

  /// Makes `id` the float `node`, a float or an integer, reads as in `f32` when `single` and in
  /// `f64` otherwise, when it is finite there, and no row with no `id`: an `f32` is the literal
  /// rounded once to `f32`, never its `f64` rounded again, and is kept widened to an `f64`, which
  /// is exact. Answers its identity: the bits of its value in its type, `-0.0` made `0.0`.
  fn float(
    &mut self,
    id: Option<ValueId>,
    node: u32,
    source: u32,
    single: bool,
  ) -> Result<u64, Refused> {
    let refused = move |why: Why| Refused { node, why };
    let read = self.float_read(node, source).ok_or(refused(Why::Kind))?;
    // Adding `0.0` makes `-0.0` `0.0`, and leaves every other value as it is.
    if single {
      let narrow = read.single.ok_or(refused(Why::Single))?;
      if let Some(id) = id {
        let row = self.float_row(node, source, f64::from(narrow), true);
        self.builder.give_float(id, row);
      }
      return Ok(u64::from((narrow + 0.0).to_bits()));
    }
    let value = read.value.ok_or(refused(Why::Infinite))?;
    if let Some(id) = id {
      let row = self.float_row(node, source, value, false);
      self.builder.give_float(id, row);
    }
    Ok((value + 0.0).to_bits())
  }

  /// The bytes `node`, a string or a list, is as a `bytes` value, kept: a string's cooked text's
  /// UTF-8 encoding, or a list's elements, each an integer from 0 to 255.
  fn bytes(&mut self, node: u32, source: u32) -> Result<Bytes, Refused> {
    if let Some(&bytes) = self.byte_strings.get(&node) {
      return bytes;
    }
    let bytes = self.read_bytes(node, source);
    self.byte_strings.insert(node, bytes);
    bytes
  }

  /// Reads the bytes of `node`, a string or a list, as [`bytes`](Self::bytes) keeps them.
  fn read_bytes(&mut self, node: u32, source: u32) -> Result<Bytes, Refused> {
    let refused = move |why: Why| Refused { node, why };
    let Some(raw) = self.raw.nodes.get(node as usize).copied() else {
      return Err(refused(Why::Kind));
    };
    match raw.kind {
      RawKind::String => {
        self.scratch.clear();
        if decode_string_value(self.raw.text(source, raw.span), &mut self.scratch).is_err() {
          return Err(refused(Why::Kind));
        }
        Ok(self.builder.keep_bytes(self.scratch.as_bytes()))
      }
      RawKind::List => {
        self.octets.clear();
        let mut child = node.saturating_add(1);
        while child < raw.after {
          let Some(element) = self.raw.nodes.get(child as usize).copied() else {
            break;
          };
          let RawKind::Int(radix) = element.kind else {
            return Err(Refused {
              node: child,
              why: Why::Byte,
            });
          };
          let byte = self
            .byte(child, source, radix)
            .map_err(|why: Why| Refused { node: child, why })?;
          self.octets.push(byte);
          if element.after <= child {
            break;
          }
          child = element.after;
        }
        Ok(self.builder.keep_bytes(&self.octets))
      }
      _ => Err(refused(Why::Kind)),
    }
  }

  /// The byte `node`, an integer of `radix`, is: `u8`'s range.
  fn byte(&mut self, node: u32, source: u32, radix: Radix) -> Result<u8, Why> {
    let scanned = self.scanned(node, source, radix).ok_or(Why::Byte)?;
    if scanned.significant > number::max_digits(radix, 8) {
      return Err(Why::ByteRange);
    }
    // At most four digits, which cost nothing to convert.
    let Some(converted) = self.converted(node, source, radix, scanned) else {
      return Err(Why::ByteRange);
    };
    if !number::fits(scanned.negative, converted.bits, converted.power, false, 8) {
      return Err(Why::ByteRange);
    }
    Ok(self.magnitude(converted).first().copied().unwrap_or(0))
  }

  /// Makes `id` the value of the enum `def` that `node`, the path at `row` of `tree`, names. A
  /// bare name is looked up under `def` and nothing else. A qualified one's segments but the last
  /// are resolved once, as a type, where the value is written, and must name `def`: another enum,
  /// a parameter, `NonZero` or a module does not fit, and a prefix the resolver refused leaves the
  /// value unresolved and says nothing more, the resolver having said why. Answers the value's
  /// identity, its member, when it names one. With no `id` it makes no row.
  fn enum_value(
    &mut self,
    id: Option<ValueId>,
    node: u32,
    row: u32,
    def: DefId,
    tree: RawTree,
  ) -> Result<Option<Ident>, Refused> {
    let refused = move |why: Why| Refused { node, why };
    let Some(path) = self.raw.paths.get(row as usize).copied() else {
      return Err(refused(Why::Kind));
    };
    let segments = self.raw.path_segments(path);
    let (count, last) = match segments.last() {
      Some(&(last, _)) => (segments.len(), last),
      None => return Err(refused(Why::Kind)),
    };
    if path.written.is_none() {
      return Err(refused(Why::Kind));
    }
    if !(matches!(path.root, Root::Relative) && count == 1) {
      // A root and one segment, `~::RED`, has no prefix to name the enum.
      if count < 2 {
        return Err(refused(Why::Kind));
      }
      match self.prefix(row, path, tree) {
        Resolved::Def(found) if found == def => {}
        Resolved::Unresolved => return Ok(None),
        Resolved::Def(_) | Resolved::NonZero { .. } | Resolved::Module(_) | Resolved::Param(_) => {
          return Err(refused(Why::Kind));
        }
      }
    }
    match self.members.get(&(def, last)) {
      Some(&member) => {
        if let Some(id) = id {
          self.builder.set_enum(id, def, member);
        }
        Ok(Some(Ident::Member(member)))
      }
      None => Err(refused(Why::NoValue { def, name: last })),
    }
  }

  /// What the segments but the last of `path`, the path at `row` of `tree`, name, resolved the
  /// first time it is asked and kept, as [`resolve_type`](Self::resolve_type) resolves them.
  fn prefix(&mut self, row: u32, path: RawPath, tree: RawTree) -> Resolved {
    if let Some(resolved) = path.prefix {
      return resolved;
    }
    let resolved = self.resolve_type(path, tree, false);
    if let Some(kept) = self.raw.paths.get_mut(row as usize) {
      kept.prefix = Some(resolved);
    }
    resolved
  }

  /// What the whole of `path`, the path at `row` of `tree`, names, resolved the first time it is
  /// asked and kept, as [`resolve_type`](Self::resolve_type) resolves it.
  fn named(&mut self, row: u32, path: RawPath, tree: RawTree) -> Resolved {
    if let Some(resolved) = path.named {
      return resolved;
    }
    let resolved = self.resolve_type(path, tree, true);
    if let Some(kept) = self.raw.paths.get_mut(row as usize) {
      kept.named = Some(resolved);
    }
    resolved
  }

  /// What `path`, of `tree`, names as a type, the whole of it when `whole` and its segments but
  /// the last otherwise: resolved in the module and the source of `tree`, with the one generic
  /// parameter in scope that the path's first segment names, if it names one. The resolver
  /// reports what does not resolve, at the path where it is written, with GraphQL's
  /// `undefined-type` for a whole path of one segment and no root.
  fn resolve_type(&mut self, path: RawPath, tree: RawTree, whole: bool) -> Resolved {
    let builder: &ModelBuilder = &*self.builder;
    let all = self.raw.path_segments(path);
    let written = if whole {
      all
    } else {
      all.get(..all.len().saturating_sub(1)).unwrap_or_default()
    };
    let segments: Vec<Segment<'_>> = written
      .iter()
      .map(|&(sym, span): &(Sym, SimpleSpan)| Segment {
        name: builder.spelling(sym),
        at: Location::new(tree.source, span),
      })
      .collect();
    let one: [(&[u8], u32); 1];
    let params: &[(&[u8], u32)] = match (path.param, written.first()) {
      (Some(index), Some(&(first, _))) => {
        one = [(builder.spelling(first), index)];
        &one
      }
      _ => &[],
    };
    let request = Request {
      module: tree.module,
      source: SourceId::new(tree.source),
      wanted: Wanted::Type,
      position: if whole {
        Position::Type
      } else {
        Position::Other
      },
      params,
    };
    self
      .scopes
      .resolve_segments(builder, self.reporter, &request, path.root, &segments)
  }

  /// Makes `id` the type value that `node`, the path at `row` of `tree`, names: its whole path
  /// resolved once, as a type, where the value is written. A definition of any kind is a type
  /// value; a type parameter, an alias such as `NonZeroU32`, which names an instantiation, and a
  /// module do not fit; a path the resolver refused leaves the value unresolved and says nothing
  /// more, the resolver having said why, whoever asked first. A type value has no identity. With
  /// no `id` it makes no row.
  fn type_value(
    &mut self,
    id: Option<ValueId>,
    node: u32,
    row: u32,
    tree: RawTree,
  ) -> Result<Option<Ident>, Refused> {
    let refused = move |why: Why| Refused { node, why };
    let Some(path) = self.raw.paths.get(row as usize).copied() else {
      return Err(refused(Why::Kind));
    };
    if path.written.is_none() {
      return Err(refused(Why::Kind));
    }
    match self.named(row, path, tree) {
      Resolved::Def(def) => {
        if let Some(id) = id {
          self.builder.set_type_value(id, def);
        }
        Ok(None)
      }
      Resolved::Unresolved => Ok(None),
      // A module is never answered where a type is wanted: the resolver refuses it, and reports it.
      Resolved::Param(_) | Resolved::NonZero { .. } | Resolved::Module(_) => {
        Err(refused(Why::Kind))
      }
    }
  }

  /// The syntactic kind of `node`: `null` for a node lowering did not make.
  fn kind(&self, node: u32) -> RawKind {
    self
      .raw
      .nodes
      .get(node as usize)
      .map_or(RawKind::Null, |raw: &RawNode| raw.kind)
  }

  /// The text `node` is written as, in the source `source`.
  fn written(&self, node: u32, source: u32) -> &'p [u8] {
    match self.raw.nodes.get(node as usize) {
      Some(raw) => self.raw.text(source, raw.span),
      None => &[],
    }
  }

  /// The slot of what has been read of `node`, appended the first time something is.
  fn slot(&mut self, node: u32) -> usize {
    if let Some(raw) = self.raw.nodes.get(node as usize)
      && raw.kept != UNREAD
    {
      return raw.kept as usize;
    }
    let slot = self.kept.len();
    self.kept.push(Kept::default());
    if let Some(raw) = self.raw.nodes.get_mut(node as usize) {
      raw.kept = dense(slot);
    }
    slot
  }

  /// What has been read at `slot`.
  fn read(&self, slot: usize) -> Kept {
    self.kept.get(slot).copied().unwrap_or_default()
  }

  /// Writes what `write` reads into `slot`.
  fn update(&mut self, slot: usize, write: impl FnOnce(&mut Kept)) {
    if let Some(kept) = self.kept.get_mut(slot) {
      write(kept);
    }
  }

  /// `node`'s text as written, kept in the model's text arena.
  fn text(&mut self, node: u32, source: u32) -> Text {
    let slot = self.slot(node);
    if let Some(text) = self.read(slot).text {
      return text;
    }
    let written = self.written(node, source);
    let text = self.builder.keep_text(&String::from_utf8_lossy(written));
    self.update(slot, |kept: &mut Kept| kept.text = Some(text));
    text
  }

  /// Makes `id` the literal `node` is, its syntactic kind and its text as written.
  fn literal(&mut self, id: ValueId, node: u32, source: u32) {
    let slot = self.slot(node);
    let row = match self.read(slot).literal {
      Some(row) => row,
      None => {
        let kind = match self.kind(node) {
          RawKind::Null => LiteralKind::Null,
          RawKind::Boolean(_) => LiteralKind::Boolean,
          RawKind::Int(_) => LiteralKind::Int,
          RawKind::Float { .. } => LiteralKind::Float,
          RawKind::String => LiteralKind::String,
          RawKind::Path(_) => LiteralKind::Enum,
          RawKind::List => LiteralKind::List,
          RawKind::Set => LiteralKind::Set,
          RawKind::Map => LiteralKind::Map,
          RawKind::Object(_) => LiteralKind::Object,
        };
        let text = self.text(node, source);
        let row = self.builder.push_literal(kind, text);
        self.update(slot, |kept: &mut Kept| kept.literal = Some(row));
        row
      }
    };
    self.builder.give_literal(id, row);
  }

  /// Gives `id` the path its node is written as, when `kind` is a path whose root lowering knows.
  fn path(&mut self, id: ValueId, kind: RawKind) {
    let RawKind::Path(row) = kind else {
      return;
    };
    let Some(path) = self.raw.paths.get(row as usize).copied() else {
      return;
    };
    let Some(root) = path.written else {
      return;
    };
    let segments = match path.kept {
      Some(segments) => segments,
      None => {
        self.syms.clear();
        self.syms.extend(
          self
            .raw
            .path_segments(path)
            .iter()
            .map(|&(sym, _): &(Sym, SimpleSpan)| sym),
        );
        let segments = self.builder.keep_segments(&self.syms);
        if let Some(path) = self.raw.paths.get_mut(row as usize) {
          path.kept = Some(segments);
        }
        segments
      }
    };
    self.builder.give_path(id, root, segments);
  }

  /// `node`'s cooked text, a string's, kept: `None` for a spelling the decoder refuses, which the
  /// lexer never makes.
  fn cooked(&mut self, node: u32, source: u32) -> Option<Text> {
    let slot = self.slot(node);
    if let Some(cooked) = self.read(slot).cooked {
      return cooked;
    }
    self.scratch.clear();
    let cooked = if decode_string_value(self.written(node, source), &mut self.scratch).is_ok() {
      Some(self.builder.keep_text(&self.scratch))
    } else {
      None
    };
    self.update(slot, |kept: &mut Kept| kept.cooked = Some(cooked));
    cooked
  }

  /// `node`, an integer of `radix`, as scanned, kept.
  fn scanned(&mut self, node: u32, source: u32, radix: Radix) -> Option<Scanned> {
    let slot = self.slot(node);
    if let Some(scanned) = self.read(slot).scanned {
      return scanned;
    }
    let scanned = number::scan(self.written(node, source), radix);
    self.update(slot, |kept: &mut Kept| kept.scanned = Some(scanned));
    scanned
  }

  /// `node`'s magnitude, an integer of `radix` scanned as `scanned`, converted once and kept.
  fn converted(
    &mut self,
    node: u32,
    source: u32,
    radix: Radix,
    scanned: Scanned,
  ) -> Option<Converted> {
    let slot = self.slot(node);
    if let Some(converted) = self.read(slot).converted {
      return converted;
    }
    // A long decimal integer's conversion is quadratic in its digits: it is paid for first, unless
    // the built-in library writes it.
    let cost = match radix {
      Radix::Decimal => number::decimal_cost(scanned.significant),
      Radix::Hex | Radix::Octal | Radix::Binary => 0,
    };
    if cost > 0 && source >= self.supplied && !self.charge(cost) {
      self.update(slot, |kept: &mut Kept| kept.converted = Some(None));
      return None;
    }
    let start = self.magnitudes.len();
    number::magnitude(
      self.written(node, source),
      radix,
      scanned.significant,
      &mut self.magnitudes,
      &mut self.limbs,
    );
    let magnitude = self.magnitudes.get(start..).unwrap_or_default();
    let converted = Converted {
      start: dense(start),
      len: dense(magnitude.len()),
      bits: number::bit_length(magnitude),
      power: number::is_power_of_two(magnitude),
    };
    self.update(slot, |kept: &mut Kept| {
      kept.converted = Some(Some(converted))
    });
    Some(converted)
  }

  /// The magnitude `converted` holds.
  fn magnitude(&self, converted: Converted) -> &[u8] {
    let start = converted.start as usize;
    self
      .magnitudes
      .get(start..start.saturating_add(converted.len as usize))
      .unwrap_or_default()
  }

  /// `node`'s row of the model's integer table, negative when `negative`, of the magnitude
  /// `converted`: made the first time a type it fits asks, and kept.
  fn int_row(&mut self, node: u32, source: u32, negative: bool, converted: Converted) -> u32 {
    let slot = self.slot(node);
    if let Some(row) = self.read(slot).int {
      return row;
    }
    let text = self.text(node, source);
    let start = converted.start as usize;
    let magnitude = self
      .magnitudes
      .get(start..start.saturating_add(converted.len as usize))
      .unwrap_or_default();
    let row = self.builder.push_int(negative, magnitude, text);
    self.update(slot, |kept: &mut Kept| kept.int = Some(row));
    row
  }

  /// What `node`, a float or an integer, reads as where a float is expected, kept: a decimal one
  /// by the standard parser, its exponent read first, a hexadecimal float by its top 64 bits, and
  /// a hexadecimal, octal or binary integer by its magnitude, when it is no longer than
  /// `FLOAT_BITS` bits could be.
  fn float_read(&mut self, node: u32, source: u32) -> Option<Float> {
    let slot = self.slot(node);
    if let Some(read) = self.read(slot).float {
      return read;
    }
    let read = match self.kind(node) {
      RawKind::Float { hex: false } | RawKind::Int(Radix::Decimal) => {
        number::decimal_float(self.written(node, source))
      }
      RawKind::Float { hex: true } => number::hex_float(self.written(node, source)),
      RawKind::Int(radix) => match self.scanned(node, source, radix) {
        // A hexadecimal, octal or binary integer, which costs nothing to convert.
        Some(scanned) if scanned.significant <= number::max_digits(radix, FLOAT_BITS) => self
          .converted(node, source, radix, scanned)
          .map(|converted: Converted| {
            number::magnitude_float(scanned.negative, self.magnitude(converted))
          }),
        Some(_) => Some(Float {
          value: None,
          single: None,
        }),
        None => None,
      },
      _ => None,
    };
    self.update(slot, |kept: &mut Kept| kept.float = Some(read));
    read
  }

  /// `node`'s row of the model's float table, of `value`, its value in `f32` when `single` and in
  /// `f64` otherwise: made the first time a type of that width it fits asks, and kept, one row for
  /// each width.
  fn float_row(&mut self, node: u32, source: u32, value: f64, single: bool) -> u32 {
    let slot = self.slot(node);
    let kept = self.read(slot);
    let made = if single {
      kept.single_row
    } else {
      kept.float_row
    };
    if let Some(row) = made {
      return row;
    }
    let text = self.text(node, source);
    let row = self.builder.push_float(value, text);
    self.update(slot, |kept: &mut Kept| {
      if single {
        kept.single_row = Some(row);
      } else {
        kept.float_row = Some(row);
      }
    });
    row
  }

  /// Spends `steps` of `Limits::max_value_steps`, or spends none and answers that the budget
  /// cannot pay them.
  fn charge(&mut self, steps: u32) -> bool {
    self.values.charge(steps)
  }

  /// Reports `refused`, a node written in the source `source`, that does not fit, the value it
  /// stands in being expected of `ty`, when `mode` is written; a copy says nothing and searches for
  /// no near miss. A path whose root lowering does not know says nothing either: lowering reported
  /// it.
  fn refuse(&mut self, refused: Refused, source: u32, ty: Option<TyId>, mode: Mode) {
    let Mode::Written(site) = mode else {
      return;
    };
    let Some(raw) = self.raw.nodes.get(refused.node as usize).copied() else {
      return;
    };
    if self.unknown_root(refused.node) {
      return;
    }
    let at = Location::new(source, raw.span);
    let literal = Excerpt(self.raw.text(source, raw.span));
    let name: &[u8] = match ty.and_then(|ty: TyId| self.builder.ty_shape(ty)) {
      Some(Shape::Named(def)) => self.builder.def_name(def),
      _ => b"",
    };
    let name = Excerpt(name);
    let width = Coded::Kind(DiagnosticKind::WidthOverflow);
    let (coded, message, help): (Coded, String, Option<String>) = match refused.why {
      Why::Null => (
        Coded::Site,
        String::from("`null` is given where a non-null value is expected"),
        None,
      ),
      Why::Kind => (
        Coded::Site,
        format!("`{literal}` is no value of `{name}`"),
        None,
      ),
      Why::Range(range) => (
        if range.signed && range.width == 32 {
          Coded::Site
        } else {
          width
        },
        format!("`{literal}` is out of the range of `{name}`"),
        Some(range_help(range)),
      ),
      Why::Infinite => (
        Coded::Site,
        format!("`{literal}` is not finite as a 64-bit float, which `{name}` is"),
        None,
      ),
      Why::Single => (
        width,
        format!("`{literal}` is out of the range of `{name}`, a 32-bit float"),
        Some(String::from(
          "a 32-bit float holds the values below 2^128 - 2^103 in magnitude; one of that \
           magnitude or more rounds to infinity",
        )),
      ),
      Why::Zero => (
        Coded::Kind(DiagnosticKind::NonZeroZero),
        format!("`{literal}` is zero, and `{name}` is never zero"),
        None,
      ),
      Why::Byte => (
        Coded::Site,
        format!("each element of a list given for `{name}` is a byte, and `{literal}` is none"),
        Some(String::from("a byte is an integer from 0 to 2^8 - 1")),
      ),
      Why::ByteRange => (
        width,
        format!(
          "`{literal}` is out of the range of a byte, which each element of a list given for \
           `{name}` is"
        ),
        Some(String::from("a byte is an integer from 0 to 2^8 - 1")),
      ),
      Why::NoValue { def, name: wanted } => {
        let builder: &ModelBuilder = &*self.builder;
        let first = (def, Sym::new(0));
        let last = (def, Sym::new(u32::MAX));
        let mut candidates = self
          .members
          .range(first..=last)
          .map(|(&(_, sym), _): (&(DefId, Sym), &MemberId)| builder.spelling(sym));
        let near: Option<String> = self
          .scopes
          .nearest(builder.spelling(wanted), &mut candidates)
          .map(|spelled: &[u8]| format!("did you mean `{}`?", Excerpt(spelled)));
        (
          Coded::Site,
          format!("`{literal}` names no value of the enum `{name}`"),
          near,
        )
      }
      Why::Costly { digits, cost } => {
        let max = self.values.max();
        (
          Coded::Kind(DiagnosticKind::ResourceLimit),
          format!(
            "converting this decimal integer of {digits} digits takes {cost} steps, more than \
             is left of `Limits::max_value_steps`, {max}, the steps the analysis may take to \
             complete values"
          ),
          Some(String::from(
            "raise the limit with `Limits::with_max_value_steps`, or write the integer in \
             hexadecimal, which costs nothing to convert",
          )),
        )
      }
      Why::Required { def, field } => {
        let builder: &ModelBuilder = &*self.builder;
        let field: &[u8] = match builder.member_name(field) {
          Some(sym) => builder.spelling(sym),
          None => b"",
        };
        (
          Coded::Schema(
            schema::MISSING_REQUIRED_INPUT_OBJECT_FIELD,
            "missing required input object field",
          ),
          format!(
            "the field `{}` of `{}` is required, and `null` is given",
            Excerpt(field),
            Excerpt(builder.def_name(def))
          ),
          None,
        )
      }
      Why::OneOf { def, written } => (
        Coded::Site,
        format!(
          "`{}` is a OneOf input object, which takes exactly one field, and this literal writes \
           {written}",
          Excerpt(self.builder.def_name(def))
        ),
        None,
      ),
      Why::OneOfNull { def } => (
        Coded::Site,
        format!(
          "`{}` is a OneOf input object, and the one field this literal writes is `null`",
          Excerpt(self.builder.def_name(def))
        ),
        None,
      ),
      Why::Collection { expected } => {
        let (message, help) = match expected {
          Some(Collection::Set) => (
            format!("`{literal}` is no value of `{name}`, a set, which takes a set literal"),
            Some(String::from("a set is written `set!{ … }`")),
          ),
          Some(Collection::Map) => (
            format!("`{literal}` is no value of `{name}`, a map, which takes a map literal"),
            Some(String::from("a map is written `map!{ key => value }`")),
          ),
          None if matches!(raw.kind, RawKind::Set) => (
            format!("`{literal}` is a set literal, and `{name}` is no set"),
            None,
          ),
          None => (
            format!("`{literal}` is a map literal, and `{name}` is no map"),
            None,
          ),
        };
        (
          Coded::Kind(DiagnosticKind::CollectionLiteral),
          message,
          help,
        )
      }
      Why::Unbuilt(stop) => {
        let (message, help) = self.stopped(stop, "this value takes");
        (
          Coded::Kind(DiagnosticKind::ResourceLimit),
          message,
          Some(String::from(help)),
        )
      }
    };
    let mut report = match coded {
      Coded::Kind(kind) => Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .primary_label(kind.title()),
      Coded::Site => {
        let (code, label) = site.refusal();
        Diagnostic::builder(code, at).primary_label(label)
      }
      Coded::Schema(code, label) => Diagnostic::builder(code, at).primary_label(label),
    }
    .message(message);
    if let Some(help) = help {
      report = report.help(help);
    }
    self.reporter.report(report.build());
  }

  /// Reports that the lists a value written at `at` stands for are past the budget.
  fn lone_limit(&mut self, at: Location) {
    let kind = DiagnosticKind::ResourceLimit;
    let max = self.values.max();
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(format!(
          "the lists this value stands for, written where a list is expected, pass \
           `Limits::max_value_steps`, {max}, the steps the analysis may take to complete values"
        ))
        .primary_label(kind.title())
        .help("raise the limit with `Limits::with_max_value_steps`, or write the lists")
        .build(),
    );
  }
}

/// The verdict of a count that reads a type `stop` kept from being built under type arguments.
fn unbuilt_count(stop: Stop) -> Count {
  match stop {
    Stop::Steps => Count::Past,
    Stop::Built(limit) => Count::Unbuilt(limit),
  }
}

/// The key at `position` of `keyed`, its run of `keys`: none past either.
fn key_at<'k>(keys: &'k [u8], keyed: &[(u32, u32, u32)], position: usize) -> &'k [u8] {
  keyed
    .get(position)
    .and_then(|&(start, end, _): &(u32, u32, u32)| keys.get(start as usize..end as usize))
    .unwrap_or_default()
}

/// The run of `pool` from `start`, `len` bytes long: none past it.
fn run_of(pool: &[u8], start: u32, len: u32) -> &[u8] {
  let start = start as usize;
  pool
    .get(start..start.saturating_add(len as usize))
    .unwrap_or_default()
}

/// What `range` holds, as a formula: a width is the one number it writes.
fn range_help(range: Range) -> String {
  let Range {
    signed,
    width,
    non_zero,
  } = range;
  let except = if non_zero { ", but zero" } else { "" };
  if width == 0 {
    String::from("it holds 0 alone")
  } else if signed {
    let high = width - 1;
    format!("it holds the integers from -2^{high} to 2^{high} - 1{except}")
  } else {
    format!("it holds the integers from 0 to 2^{width} - 1{except}")
  }
}

/// Reports, once and at `at`, a spelling the model's arenas could not keep.
fn arenas(builder: &ModelBuilder, reporter: &mut Reporter, at: Location) {
  if !builder.arenas_overflowed() {
    return;
  }
  let kind = DiagnosticKind::ResourceLimit;
  reporter.report(
    Diagnostic::builder(kind.code(), at)
      .severity(kind.severity())
      .message(
        "the names and texts the model keeps pass the four gigabytes its `u32` offsets address, \
         so some of the spellings it answers are not the ones written",
      )
      .primary_label(kind.title())
      .help("lower `Limits::max_source_bytes`, or supply less source")
      .build(),
  );
}
