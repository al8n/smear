//! Coercion: the phase after instantiation, which makes the model's values of the values lowering
//! read, each by the type expected where it stands.
//!
//! It reads no document: it reads the trees lowering answered, the rows written so far, through
//! the builder, and the sources' texts, and it is the one writer of the model's values. Its
//! built-ins are found once, each by its name in its package's root module and never by a spelling
//! a program writes: `core`'s `NonZero`, and `alloc`'s `string`, `id` and `bytes`; a primitive is
//! told by its definition, which `core` materialises.
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
//!    type that names a type parameter, which only an instantiation can check; the unresolved
//!    type, an object, an interface, a union or a directive; and `NonZero` of what is no nullable
//!    integer primitive at least one bit wide. Whoever refused the type said so, and that is asked
//!    before `null` is.
//! 2. `null` is `null` where its type is nullable, and does not fit where it is not.
//! 3. By the type, without its `!`: a list takes a list, each element coerced against its
//!    element, and any other node is a lone value, which stands for the one-element list that
//!    holds it, a list for each level of list the type has more than the node, each level one row
//!    at the node; `bool` a boolean; an integer primitive an integer in its width's range, read at
//!    any length; `f32` and `f64` a float, or an integer, whose value is finite in that type;
//!    `string` a string, cooked; `id` a string, or an integer in `i32`'s range, kept an integer;
//!    `bytes` a string, its UTF-8 encoding, or a list of integers from 0 to 255, one value of its
//!    bytes either way; `NonZero` of an integer primitive an integer in that primitive's range
//!    that is not zero; an enum a path that names one of its values. Any other scalar takes any
//!    literal, kept as written, and so does an input object and a collection in this version.
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
//! float's verdict and its row; a byte string; a literal's row; a path's segments, and what its
//! prefix names.
//!
//! # An application's arguments
//!
//! Before the walk, each directive's arguments that an application leaving them out does something
//! about are listed once, in member order: one with a default, with the count of rows a copy of it
//! makes, counted once through the same classifier that makes the rows, its written nodes walked
//! and a lone value counted from its type's levels of list, and saturating at
//! `Limits::max_value_steps` plus one; and one required, written non-null with no default. A later argument of a name an earlier one has is not listed, since no
//! application can write it. An application merges its written arguments, which lowering kept in
//! its directive's order, with that list, by the argument each sets. A written argument is coerced
//! as written, and a required one given `null` is `missing-required-directive-argument`, at its
//! name, and nothing more. A listed argument left out takes a copy of its default: the default's
//! tree coerced again against the argument's type, silently, each row not written and located at
//! the node it copies, what was read of the literal shared; a node that does not fit is
//! unresolved, and the default's own diagnostic, where it is written, says why once. A required one
//! left out is `missing-required-directive-argument`, at the application's path, with a label at
//! the argument. An argument neither written, defaulted nor required is absent.
//!
//! # What bounds it
//!
//! A written row is a written literal, which `Limits::max_source_bytes` bounds. What no literal
//! writes is a step of `Limits::max_value_steps`: each list a written lone value stands for; each
//! row of a copy, its whole count charged before its first row; and each required argument found
//! left out. Past it a lone value is unresolved, and `resource-limit` where it is written; and an
//! application is `resource-limit` once, at its path, its written arguments still coerced, a copy
//! made whole or not at all, and the rest of its directive's list not read. The built-in library's
//! own values are charged nothing, and neither is a copy made for an application it writes; a copy
//! of one of its defaults made for an application a supplied package writes is charged.
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
use tokora::{
  SimpleSpan,
  diagnostic::{Code, Location},
};

use super::{
  builtin::{ALLOC, CORE, TYPE},
  code::{DiagnosticKind, schema},
  declare::Declarations,
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{ApplicationId, DefId, MemberId, SourceId, Sym, TyId, TypeExprId, ValueId, dense},
  instantiate::binding,
  limits::Limits,
  model::{Bytes, DefKind, LiteralKind, ModelBuilder, Primitive, Resolution, Shape, Text},
  raw::{Radix, RawArgument, RawKind, RawNode, RawPath, RawTree, RawValues, UNREAD},
  resolve::{Position, Request, Resolved, Root, Scopes, Segment, Wanted},
  set::ROOT,
  source::SourceMap,
};

mod number;

use number::{Float, Scanned};

/// The width an integer is read in where a float is expected: an integer longer than one of this
/// many bits is no finite float.
const FLOAT_BITS: u32 = 1024;

/// Makes the model's values of `raw`, the values lowering read, in `builder`, whose declarations
/// are `declarations`, whose scopes are `scopes` and whose sources `sources` numbers, within
/// `limits`, reporting into `reporter`: each member's default and each application's arguments, in
/// the model's walk; then the check of the model's arenas.
pub(crate) fn coerce(
  raw: RawValues<'_>,
  declarations: &Declarations<'_, '_>,
  scopes: &Scopes<'_>,
  sources: &SourceMap,
  limits: &Limits,
  builder: &mut ModelBuilder,
  reporter: &mut Reporter,
) {
  let builtins = Builtins {
    non_zero: binding(declarations, builder, CORE, TYPE, b"NonZero"),
    string: binding(declarations, builder, ALLOC, TYPE, b"string"),
    id: binding(declarations, builder, ALLOC, TYPE, b"id"),
    bytes: binding(declarations, builder, ALLOC, TYPE, b"bytes"),
  };
  // Every enum's values, the first of each name: what an enum value's name is looked up by.
  let mut enums: BTreeMap<(DefId, Sym), MemberId> = BTreeMap::new();
  for index in 0..builder.def_count() {
    let def = DefId::new(index);
    if builder.def_kind(def) != Some(DefKind::Enum) {
      continue;
    }
    for member in builder.def_members(def) {
      if let Some(name) = builder.member_name(member) {
        enums.entry((def, name)).or_insert(member);
      }
    }
  }
  let mut coercion = Coercion {
    builder,
    reporter,
    scopes,
    raw,
    primitives: &declarations.primitives,
    builtins,
    enums,
    supplied: sources.package_sources(ROOT).start(),
    max_steps: limits.max_value_steps(),
    spent: 0,
    kept: Vec::new(),
    magnitudes: Vec::new(),
    limbs: Vec::new(),
    scratch: String::new(),
    octets: Vec::new(),
    stack: Vec::new(),
    items: Vec::new(),
    children: Vec::new(),
    syms: Vec::new(),
    listed: Vec::new(),
    runs: Vec::new(),
    counting: Vec::new(),
    levels: BTreeMap::new(),
    chain: Vec::new(),
  };
  coercion.list_arguments();
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
  /// As a copy of a default, for an application that leaves its argument out: its rows are not
  /// written, and nothing is reported, charged or searched for, the copy's whole count having been
  /// charged before its first row.
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
  /// Its default's tree, and how many rows a copy of it makes; `None` for a required argument with
  /// no default.
  default: Option<(u32, u32)>,
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
  /// Any literal, kept as written: a custom scalar, a collection, `TypeRef` and an input object.
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
  /// A node against a named type: one row, its kind told by what the type takes.
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
}

/// A node that does not fit, and why: the node may be an element of the value being made.
#[derive(Clone, Copy)]
struct Refused {
  node: u32,
  why: Why,
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
  /// An integer's magnitude, once converted.
  converted: Option<Converted>,
  /// An integer's row of the model's integer table.
  int: Option<u32>,
  /// What it reads as where a float is expected; `None` inside for a text no float reads.
  float: Option<Option<Float>>,
  /// Its row of the model's float table.
  float_row: Option<u32>,
  /// Its row of the model's literal table.
  literal: Option<u32>,
  /// Its bytes as a `bytes` value, in the model's byte pool, or why it is none.
  bytes: Option<Result<Bytes, Refused>>,
}

/// One step of making a tree's rows.
#[derive(Clone, Copy)]
enum Task {
  /// Make the row of `node` against `ty`.
  Make { node: u32, ty: Option<TyId> },
  /// Give the list `list` the rows made since `from` among the open lists' rows.
  Close { list: ValueId, from: usize },
}

/// What coercion keeps while it writes.
struct Coercion<'c, 'p, 'x> {
  builder: &'c mut ModelBuilder,
  reporter: &'c mut Reporter,
  /// Every module's scope: what a qualified enum value's prefix names.
  scopes: &'c Scopes<'x>,
  /// The trees, and each source's text.
  raw: RawValues<'p>,
  /// Each primitive `core` materialises, with its definition: the primitive at index `i` is the
  /// definition `i`.
  primitives: &'c [(Primitive, DefId)],
  builtins: Builtins,
  /// Every enum's values, by the enum and the name, the first of each name.
  enums: BTreeMap<(DefId, Sym), MemberId>,
  /// The first source of a supplied package: every source below it is the built-in library's.
  supplied: u32,
  /// `Limits::max_value_steps`.
  max_steps: u32,
  /// The steps spent.
  spent: u32,
  /// What has been read of each node read so far: the rows a node's `kept` slot indexes.
  kept: Vec<Kept>,
  /// Every integer's magnitude converted so far.
  magnitudes: Vec<u8>,
  /// The limbs of the decimal integer being converted.
  limbs: Vec<u32>,
  /// The text being cooked or read.
  scratch: String,
  /// The bytes of the list being read as a `bytes` value.
  octets: Vec<u8>,
  /// What making the current tree has still to do.
  stack: Vec<Task>,
  /// The rows made of the lists that are open, each list's after it.
  items: Vec<ValueId>,
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
  /// The nodes being counted, each with the type it is counted against.
  counting: Vec<(u32, Option<TyId>)>,
  /// How many levels of list each list type read so far is: what a lone value's count reads.
  levels: BTreeMap<TyId, u32>,
  /// The list types whose levels are being read.
  chain: Vec<TyId>,
}

impl<'p> Coercion<'_, 'p, '_> {
  /// The values of `member`: its default, then each of its applications' arguments.
  fn member(&mut self, member: MemberId) {
    if let Some(&tree) = self.raw.defaults.get(&member) {
      let ty = self.member_type(member);
      if let Some(value) = self.value(tree, ty, Mode::Written(Site::Default)) {
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
    let mut starved = false;
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
          self.written_argument(application, argument);
          written += 1;
        }
        (Some(argument), Some(left)) if argument.argument <= left.argument => {
          if argument.argument == left.argument {
            listed += 1;
          }
          self.written_argument(application, argument);
          written += 1;
        }
        (_, Some(left)) => {
          listed += 1;
          if !self.left_out(application, left, at, library) {
            starved = true;
          }
        }
      }
    }
  }

  /// Does what `left`, an argument `application` leaves out, asks, the application's path at `at`
  /// and written by the built-in library when `library`: a copy of its default, its count of rows
  /// charged first, or, required, a report that it is left out, a step charged first. Answers
  /// `false`, and reports the application once, when the budget cannot pay.
  fn left_out(
    &mut self,
    application: ApplicationId,
    left: Listed,
    at: Location,
    library: bool,
  ) -> bool {
    let cost = left.default.map_or(1, |(_, count): (u32, u32)| count);
    if !library && !self.charge(cost) {
      self.starved(at);
      return false;
    }
    match left.default {
      Some((tree, count)) => {
        let before = self.builder.value_count();
        let ty = self.member_type(left.argument);
        if let Some(value) = self.value(tree, ty, Mode::Copied) {
          // A count past the budget is saturated, and only a copy the library's application asks
          // for, which is not charged, is made then.
          debug_assert!(
            count > self.max_steps || self.builder.value_count().saturating_sub(before) == count,
            "a copy makes the rows it was counted to"
          );
          self
            .builder
            .push_application_argument(application, left.argument, None, value);
        }
      }
      None => self.missing(application, left.argument, at),
    }
    true
  }

  /// Lists each directive's arguments that an application leaving them out does something about,
  /// each with its default's count of rows: one with a default, and one required, written non-null
  /// with no default. A later argument of a name an earlier one has is not listed: no application
  /// can write it.
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
        let (ty, non_null) = self.member_occurrence(argument);
        match self.raw.defaults.get(&argument).copied() {
          Some(tree) => {
            let count = self.count(tree, ty);
            self.listed.push(Listed {
              argument,
              default: Some((tree, count)),
            });
          }
          None if non_null => self.listed.push(Listed {
            argument,
            default: None,
          }),
          None => {}
        }
      }
      let end = dense(self.listed.len());
      if end > start {
        self.runs.push((def, start, end));
      }
    }
  }

  /// How many rows coercing the tree `tree` against `ty` makes, as a copy makes them, told by
  /// [`classify`](Self::classify) alone: one for each written node it reaches, the elements of a
  /// list against a list type; and for a lone value, one for each level of list its type has
  /// left and one for the value, counted from the type and not row by row. The walk visits written
  /// nodes alone, and stops once the count passes `Limits::max_value_steps`, which no charge can
  /// pay: the count then saturates at that limit plus one.
  fn count(&mut self, tree: u32, ty: Option<TyId>) -> u32 {
    let Some(tree) = self.raw.trees.get(tree as usize).copied() else {
      return 0;
    };
    let cap = self.max_steps.saturating_add(1);
    let mut rows: u32 = 0;
    self.counting.clear();
    self.counting.push((tree.root, ty));
    while let Some((node, ty)) = self.counting.pop() {
      if rows >= cap {
        self.counting.clear();
        return cap;
      }
      let Some(raw) = self.raw.nodes.get(node as usize).copied() else {
        continue;
      };
      rows = rows.saturating_add(1);
      match self.classify(raw.kind, ty) {
        Class::List(element) => {
          let mut child = node.saturating_add(1);
          while child < raw.after {
            self.counting.push((child, Some(element)));
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
        }
        Class::Lone(element) => {
          let levels = self.list_levels(element);
          rows = rows.saturating_add(levels).saturating_add(1);
        }
        Class::NoValue | Class::Null | Class::NotNull | Class::Leaf(_) => {}
      }
    }
    rows.min(cap)
  }

  /// How many levels of list `ty` is before its element is no list: 0 for a type that is no list.
  /// Each list type's count is kept once read, so the types of one program are walked once in all.
  fn list_levels(&mut self, ty: TyId) -> u32 {
    self.chain.clear();
    let mut at = ty;
    let mut levels = loop {
      if let Some(&levels) = self.levels.get(&at) {
        break levels;
      }
      match self.builder.ty_shape(at) {
        // An element is interned before its list, so the walk goes down and ends.
        Some(Shape::List(element)) if element < at => {
          self.chain.push(at);
          at = element;
        }
        _ => break 0,
      }
    };
    while let Some(list) = self.chain.pop() {
      levels = levels.saturating_add(1);
      self.levels.insert(list, levels);
    }
    levels
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
  /// budget.
  fn starved(&mut self, at: Location) {
    let kind = DiagnosticKind::ResourceLimit;
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(
          "the arguments this application leaves out pass `Limits::max_value_steps`, the steps \
           the analysis may take to complete values, and are not filled",
        )
        .primary_label(kind.title())
        .help("raise the limit with `Limits::with_max_value_steps`, or write the arguments")
        .build(),
    );
  }

  /// The argument `argument` of `application`, as written: its value coerced against the
  /// argument's type. A required argument, written non-null and with no default, given `null` is
  /// reported as left out, at its name, and its value is unresolved.
  fn written_argument(&mut self, application: ApplicationId, argument: RawArgument) {
    let (ty, non_null) = self.member_occurrence(argument.argument);
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
      self.value(argument.tree, ty, Mode::Written(Site::Argument))
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

  /// Makes the rows of the tree `tree` against `ty`, as `mode` says: the value, then what it
  /// holds, in pre-order. Answers the value's id, `None` for a tree lowering did not make.
  fn value(&mut self, tree: u32, ty: Option<TyId>, mode: Mode) -> Option<ValueId> {
    let tree = self.raw.trees.get(tree as usize).copied()?;
    let base = self.items.len();
    self.stack.push(Task::Make {
      node: tree.root,
      ty,
    });
    while let Some(task) = self.stack.pop() {
      match task {
        Task::Make { node, ty } => self.make(node, ty, tree, mode),
        Task::Close { list, from } => {
          self
            .builder
            .set_list(list, self.items.get(from..).unwrap_or_default());
          self.items.truncate(from);
        }
      }
    }
    let value = self.items.get(base).copied();
    self.items.truncate(base);
    value
  }

  /// Makes the row of `node`, of `tree`, against `ty`, as `mode` says, and pushes what it holds:
  /// the row's id goes to the open lists' rows.
  fn make(&mut self, node: u32, ty: Option<TyId>, tree: RawTree, mode: Mode) {
    let Some(raw) = self.raw.nodes.get(node as usize).copied() else {
      return;
    };
    let source = tree.source;
    let at = Location::new(source, raw.span);
    let id = self.builder.push_value(at, mode.written());
    self.items.push(id);
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
      Class::List(element) => {
        let from = self.items.len();
        self.stack.push(Task::Close { list: id, from });
        self.children.clear();
        let mut child = node.saturating_add(1);
        while child < raw.after {
          self.children.push(child);
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
        self
          .stack
          .extend(self.children.iter().rev().map(|&child: &u32| Task::Make {
            node: child,
            ty: Some(element),
          }));
      }
      Class::Lone(element) => {
        if mode.written() && source >= self.supplied && !self.charge(1) {
          self.path(id, raw.kind);
          self.lone_limit(at);
        } else {
          let from = self.items.len();
          self.stack.push(Task::Close { list: id, from });
          self.stack.push(Task::Make {
            node,
            ty: Some(element),
          });
        }
      }
      Class::Leaf(base) => {
        self.path(id, raw.kind);
        if let Err(refused) = self.leaf(id, node, tree, base) {
          self.refuse(refused, source, ty, mode);
        }
      }
    }
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
      Some(Shape::Named(def)) => match self.base(def, ty) {
        Some(base) => Class::Leaf(base),
        None => return Class::NoValue,
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

  /// What the named type `ty`, of `def`, takes: `None` when it can take no value.
  fn base(&self, def: DefId, ty: TyId) -> Option<Base> {
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
    match self.builder.def_kind(def)? {
      DefKind::Enum => Some(Base::Enum(def)),
      DefKind::Scalar | DefKind::InputObject => Some(Base::Kept),
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

  /// Makes `id`, the row of `node`, of `tree`, what `base` takes of it.
  fn leaf(&mut self, id: ValueId, node: u32, tree: RawTree, base: Base) -> Result<(), Refused> {
    let source = tree.source;
    let kind = self.kind(node);
    let refused = move |why: Why| Refused { node, why };
    match (base, kind) {
      (Base::Kept, _) => {
        self.literal(id, node, source);
        Ok(())
      }
      (Base::Enum(def), RawKind::Path(row)) => self.enum_value(id, node, row, def, tree),
      (Base::Bool, RawKind::Boolean(flag)) => {
        self.builder.set_bool(id, flag);
        Ok(())
      }
      (Base::String | Base::Id, RawKind::String) => {
        let text = self.cooked(node, source).ok_or(refused(Why::Kind))?;
        self.builder.give_string(id, text);
        Ok(())
      }
      (Base::Int(range), RawKind::Int(radix)) => self.int(id, node, source, radix, range),
      (Base::Id, RawKind::Int(radix)) => self.int(id, node, source, radix, ID_RANGE),
      (Base::Float { single }, RawKind::Float { .. } | RawKind::Int(_)) => {
        self.float(id, node, source, single)
      }
      (Base::Bytes, RawKind::String | RawKind::List) => {
        let bytes = self.bytes(node, source)?;
        self.builder.give_bytes(id, bytes);
        Ok(())
      }
      _ => Err(refused(Why::Kind)),
    }
  }

  /// Makes `id` the integer `node` is, of `radix`, when it is in `range`. Its digits are counted
  /// first, and a count past what the width holds refuses it unconverted.
  fn int(
    &mut self,
    id: ValueId,
    node: u32,
    source: u32,
    radix: Radix,
    range: Range,
  ) -> Result<(), Refused> {
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
    let converted = self.converted(node, source, radix, scanned);
    if !number::fits(
      scanned.negative,
      converted.bits,
      converted.power,
      range.signed,
      range.width,
    ) {
      return Err(refused(out));
    }
    let row = self.int_row(node, source, scanned.negative, converted);
    self.builder.give_int(id, row);
    Ok(())
  }

  /// Makes `id` the float `node`, a float or an integer, reads as, when it is finite in `f32`
  /// when `single` and in `f64` otherwise.
  fn float(&mut self, id: ValueId, node: u32, source: u32, single: bool) -> Result<(), Refused> {
    let refused = move |why: Why| Refused { node, why };
    let read = self.float_read(node, source).ok_or(refused(Why::Kind))?;
    let value = if single {
      if !read.single {
        return Err(refused(Why::Single));
      }
      read.value.ok_or(refused(Why::Single))?
    } else {
      read.value.ok_or(refused(Why::Infinite))?
    };
    let row = self.float_row(node, source, value);
    self.builder.give_float(id, row);
    Ok(())
  }

  /// The bytes `node`, a string or a list, is as a `bytes` value, kept: a string's cooked text's
  /// UTF-8 encoding, or a list's elements, each an integer from 0 to 255.
  fn bytes(&mut self, node: u32, source: u32) -> Result<Bytes, Refused> {
    let slot = self.slot(node);
    if let Some(bytes) = self.read(slot).bytes {
      return bytes;
    }
    let bytes = self.read_bytes(node, source);
    self.update(slot, |kept: &mut Kept| kept.bytes = Some(bytes));
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
    let converted = self.converted(node, source, radix, scanned);
    if !number::fits(scanned.negative, converted.bits, converted.power, false, 8) {
      return Err(Why::ByteRange);
    }
    Ok(self.magnitude(converted).first().copied().unwrap_or(0))
  }

  /// Makes `id` the value of the enum `def` that `node`, the path at `row` of `tree`, names. A
  /// bare name is looked up under `def` and nothing else. A qualified one's segments but the last
  /// are resolved once, as a type, where the value is written, and must name `def`: another enum,
  /// a parameter, `NonZero` or a module does not fit, and a prefix the resolver refused leaves the
  /// value unresolved and says nothing more, the resolver having said why.
  fn enum_value(
    &mut self,
    id: ValueId,
    node: u32,
    row: u32,
    def: DefId,
    tree: RawTree,
  ) -> Result<(), Refused> {
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
        Resolved::Unresolved => return Ok(()),
        Resolved::Def(_) | Resolved::NonZero { .. } | Resolved::Module(_) | Resolved::Param(_) => {
          return Err(refused(Why::Kind));
        }
      }
    }
    match self.enums.get(&(def, last)) {
      Some(&member) => {
        self.builder.set_enum(id, def, member);
        Ok(())
      }
      None => Err(refused(Why::NoValue { def, name: last })),
    }
  }

  /// What the segments but the last of `path`, the path at `row` of `tree`, name, resolved the
  /// first time it is asked and kept: as a type, in the module and the source of `tree`, with the
  /// one generic parameter in scope that the path's first segment names, if it names one. The
  /// resolver reports what does not resolve, at the path where it is written, whoever asks first.
  fn prefix(&mut self, row: u32, path: RawPath, tree: RawTree) -> Resolved {
    if let Some(resolved) = path.prefix {
      return resolved;
    }
    let builder: &ModelBuilder = &*self.builder;
    let all = self.raw.path_segments(path);
    let prefix = all.get(..all.len().saturating_sub(1)).unwrap_or_default();
    let segments: Vec<Segment<'_>> = prefix
      .iter()
      .map(|&(sym, span): &(Sym, SimpleSpan)| Segment {
        name: builder.spelling(sym),
        at: Location::new(tree.source, span),
      })
      .collect();
    let one: [(&[u8], u32); 1];
    let params: &[(&[u8], u32)] = match (path.param, prefix.first()) {
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
      position: Position::Other,
      params,
    };
    let resolved =
      self
        .scopes
        .resolve_segments(builder, self.reporter, &request, path.root, &segments);
    if let Some(kept) = self.raw.paths.get_mut(row as usize) {
      kept.prefix = Some(resolved);
    }
    resolved
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
          RawKind::Object => LiteralKind::Object,
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
  fn converted(&mut self, node: u32, source: u32, radix: Radix, scanned: Scanned) -> Converted {
    let slot = self.slot(node);
    if let Some(converted) = self.read(slot).converted {
      return converted;
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
    self.update(slot, |kept: &mut Kept| kept.converted = Some(converted));
    converted
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
  /// by the standard parser, a hexadecimal float by its top 64 bits, and a hexadecimal, octal or
  /// binary integer by its magnitude, when it is no longer than `FLOAT_BITS` bits could be.
  fn float_read(&mut self, node: u32, source: u32) -> Option<Float> {
    let slot = self.slot(node);
    if let Some(read) = self.read(slot).float {
      return read;
    }
    let read = match self.kind(node) {
      RawKind::Float { hex: false } | RawKind::Int(Radix::Decimal) => {
        number::decimal_float(self.written(node, source), &mut self.scratch)
      }
      RawKind::Float { hex: true } => number::hex_float(self.written(node, source)),
      RawKind::Int(radix) => match self.scanned(node, source, radix) {
        Some(scanned) if scanned.significant <= number::max_digits(radix, FLOAT_BITS) => {
          let converted = self.converted(node, source, radix, scanned);
          Some(number::magnitude_float(
            scanned.negative,
            self.magnitude(converted),
          ))
        }
        Some(_) => Some(Float {
          value: None,
          single: false,
        }),
        None => None,
      },
      _ => None,
    };
    self.update(slot, |kept: &mut Kept| kept.float = Some(read));
    read
  }

  /// `node`'s row of the model's float table, of `value`: made the first time a type it fits asks,
  /// and kept.
  fn float_row(&mut self, node: u32, source: u32, value: f64) -> u32 {
    let slot = self.slot(node);
    if let Some(row) = self.read(slot).float_row {
      return row;
    }
    let text = self.text(node, source);
    let row = self.builder.push_float(value, text);
    self.update(slot, |kept: &mut Kept| kept.float_row = Some(row));
    row
  }

  /// Spends `steps` of `Limits::max_value_steps`, or spends none and answers that the budget
  /// cannot pay them.
  fn charge(&mut self, steps: u32) -> bool {
    match self.spent.checked_add(steps) {
      Some(spent) if spent <= self.max_steps => {
        self.spent = spent;
        true
      }
      _ => false,
    }
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
    if let RawKind::Path(row) = raw.kind
      && self
        .raw
        .paths
        .get(row as usize)
        .is_some_and(|path: &RawPath| path.written.is_none())
    {
      return;
    }
    let at = Location::new(source, raw.span);
    let literal = Excerpt(self.raw.text(source, raw.span));
    let name: &[u8] = match ty.and_then(|ty: TyId| self.builder.ty_shape(ty)) {
      Some(Shape::Named(def)) => self.builder.def_name(def),
      _ => b"",
    };
    let name = Excerpt(name);
    let width = DiagnosticKind::WidthOverflow;
    let (kind, message, help): (Option<DiagnosticKind>, String, Option<String>) = match refused.why
    {
      Why::Null => (
        None,
        String::from("`null` is given where a non-null value is expected"),
        None,
      ),
      Why::Kind => (None, format!("`{literal}` is no value of `{name}`"), None),
      Why::Range(range) => (
        (!(range.signed && range.width == 32)).then_some(width),
        format!("`{literal}` is out of the range of `{name}`"),
        Some(range_help(range)),
      ),
      Why::Infinite => (
        None,
        format!("`{literal}` is not finite as a 64-bit float, which `{name}` is"),
        None,
      ),
      Why::Single => (
        Some(width),
        format!("`{literal}` is out of the range of `{name}`, a 32-bit float"),
        Some(String::from(
          "a 32-bit float holds the finite values below 2^128 in magnitude",
        )),
      ),
      Why::Zero => (
        Some(DiagnosticKind::NonZeroZero),
        format!("`{literal}` is zero, and `{name}` is never zero"),
        None,
      ),
      Why::Byte => (
        None,
        format!("each element of a list given for `{name}` is a byte, and `{literal}` is none"),
        Some(String::from("a byte is an integer from 0 to 2^8 - 1")),
      ),
      Why::ByteRange => (
        Some(width),
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
          .enums
          .range(first..=last)
          .map(|(&(_, sym), _): (&(DefId, Sym), &MemberId)| builder.spelling(sym));
        let near: Option<String> = self
          .scopes
          .nearest(builder.spelling(wanted), &mut candidates)
          .map(|spelled: &[u8]| format!("did you mean `{}`?", Excerpt(spelled)));
        (
          None,
          format!("`{literal}` names no value of the enum `{name}`"),
          near,
        )
      }
    };
    let mut report = match kind {
      Some(kind) => Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .primary_label(kind.title()),
      None => {
        let (code, label) = site.refusal();
        Diagnostic::builder(code, at).primary_label(label)
      }
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
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(
          "the lists this value stands for, written where a list is expected, pass \
           `Limits::max_value_steps`, the steps the analysis may take to complete values",
        )
        .primary_label(kind.title())
        .help("raise the limit with `Limits::with_max_value_steps`, or write the lists")
        .build(),
    );
  }
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
