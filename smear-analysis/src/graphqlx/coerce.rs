//! Coercion: the phase after instantiation, which makes the model's values of the values lowering
//! read.
//!
//! It reads no document: it reads the trees lowering answered, the rows written so far, through
//! the builder, and the sources' texts, and it is the one writer of the model's values.
//!
//! # The walk
//!
//! A value is appended when the model's walk reaches it, and never before, so a value's id is its
//! place in that walk: each definition in id order, its applications, every contribution's in
//! order, then each of its direct members in id order, right after each its arguments; a member's
//! default, then its applications; and last the schema's applications. An application's arguments
//! are appended all at once, in the order of its directive's arguments.
//!
//! A written value is one literal: its syntactic kind and its text as written, quotes, brackets
//! and all, each sequence that is not UTF-8 replaced by U+FFFD; a value that is a path keeps its
//! path too. Nothing it holds is a value of its own.
//!
//! # The arenas
//!
//! The model addresses its names and its texts with `u32` offsets, and this phase is the last that
//! writes either. When it ends, a spelling the name arena refused, or a text arena longer than a
//! `u32` addresses, is reported once, as `resource-limit`, at the whole of the root package's
//! package source: otherwise the model would answer a wrong spelling with no diagnostic.

use std::string::String;

use tokora::diagnostic::Location;

use super::{
  code::DiagnosticKind,
  diagnostic::{Diagnostic, Reporter},
  id::{ApplicationId, DefId, MemberId, ValueId},
  model::{LiteralKind, ModelBuilder},
  raw::{RawArgument, RawKind, RawTree, RawValues},
  set::ROOT,
  source::SourceMap,
};

/// Makes the model's values of `raw`, the values lowering read, in `builder`, whose sources
/// `sources` numbers, reporting into `reporter`: each member's default and each application's
/// arguments, in the model's walk; then the check of the model's arenas.
pub(crate) fn coerce(
  raw: RawValues<'_>,
  sources: &SourceMap,
  builder: &mut ModelBuilder,
  reporter: &mut Reporter,
) {
  let mut coercion = Coercion { builder, raw };
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
  arenas(coercion.builder, reporter, Location::entire(root.start()));
}

/// What coercion keeps while it writes: the builder it writes into and the values it reads.
struct Coercion<'c, 'p> {
  builder: &'c mut ModelBuilder,
  raw: RawValues<'p>,
}

impl Coercion<'_, '_> {
  /// The values of `member`: its default, then each of its applications' arguments.
  fn member(&mut self, member: MemberId) {
    if let Some(&tree) = self.raw.defaults.get(&member)
      && let Some(value) = self.value(tree)
    {
      self.builder.set_member_default(member, value);
    }
    let count = self.builder.member_applications(member).len();
    for at in 0..count {
      if let Some(&application) = self.builder.member_applications(member).get(at) {
        self.application(application);
      }
    }
  }

  /// The arguments `application` writes, in the order lowering kept them, which is its directive's:
  /// its run of the written arguments, found by one binary search. An application whose directive
  /// did not resolve has none.
  fn application(&mut self, application: ApplicationId) {
    let mut next = self
      .raw
      .arguments
      .partition_point(|argument: &RawArgument| argument.application < application);
    while let Some(&argument) = self.raw.arguments.get(next)
      && argument.application == application
    {
      if let Some(value) = self.value(argument.tree) {
        self.builder.push_application_argument(
          application,
          argument.argument,
          Some(argument.name),
          value,
        );
      }
      next += 1;
    }
  }

  /// The value of the tree `tree`, written as it is written: one literal, which keeps its path
  /// when it is one. `None` for a tree lowering did not make.
  fn value(&mut self, tree: u32) -> Option<ValueId> {
    let RawTree { root, source } = self.raw.trees.get(tree as usize).copied()?;
    let node = self.raw.nodes.get(root as usize).copied()?;
    let id = self
      .builder
      .push_value(Location::new(source, node.span), true);
    let kind = match node.kind {
      RawKind::Null => LiteralKind::Null,
      RawKind::Boolean => LiteralKind::Boolean,
      RawKind::Int => LiteralKind::Int,
      RawKind::Float => LiteralKind::Float,
      RawKind::String => LiteralKind::String,
      RawKind::Path(_) => LiteralKind::Enum,
      RawKind::List => LiteralKind::List,
      RawKind::Set => LiteralKind::Set,
      RawKind::Map => LiteralKind::Map,
      RawKind::Object => LiteralKind::Object,
    };
    let written = self.raw.text(source, node.span);
    self
      .builder
      .set_literal(id, kind, &String::from_utf8_lossy(written));
    if let RawKind::Path(row) = node.kind
      && let Some(path) = self.raw.paths.get(row as usize).copied()
      && let Some(root) = path.written
    {
      self
        .builder
        .set_value_path(id, root, self.raw.path_segments(path));
    }
    Some(id)
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
