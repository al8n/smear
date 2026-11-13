//! The order every table is appended in, checked when the model is finished.
//!
//! An id is the position its row was appended at, and the model is never renumbered, so a table's
//! order is the order its rows were appended in. [`check`] runs under `debug_assertions` and fails
//! naming the rule an append broke.
//!
//! # The walk
//!
//! Type occurrences, values and applications are numbered in the order the walk over every package
//! reaches them, nested occurrences in pre-order. This is that walk, as the builder's callers must
//! follow it:
//!
//! 1. each definition, in id order:
//!    1. each type parameter's default, then each bound's interface;
//!    2. its `implements` entries, then its union members, every contribution's in order;
//!    3. its applications, every contribution's in order;
//!    4. its direct members, in id order;
//! 2. each member: its type; its default; its applications; then, for a field, each argument the
//!    same way;
//! 3. each application: itself; its type arguments; then each argument's value, its arguments in
//!    the order of its directive's, one written twice twice;
//! 4. the schema, after every definition: each written root's type, query, mutation, then
//!    subscription; then its applications.
//!
//! A type occurrence is followed by its arguments, or by its element. A list or a set is followed
//! by its elements, a map by each key and then its value, and an object by each entry's value.

use core::cmp::Ordering;
use std::{vec, vec::Vec};

use tokora::{SimpleSpan, diagnostic::Location};

use crate::graphqlx::{
  id::{ApplicationId, MemberId, SourceId, Sym, TypeExprId, ValueId, dense},
  set::ROOT,
};

use super::{
  Model, PackageKind, Resolution,
  member::MemberKind,
  module::ModuleKind,
  schema::Root,
  table::{
    ApplicationRow, ContributionRow, DefRow, KIND_LIST, KIND_NAMED, KIND_UNRESOLVED, MemberRow,
    ModuleRow, PackageRow, Run, UNSET_MODULE, UNSET_TY, VALUE_LIST, VALUE_MAP, VALUE_OBJECT,
    VALUE_SET,
  },
};

/// Checks the order of every table of `model`, and that the walk reaches every occurrence in id
/// order.
///
/// # Panics
///
/// On the first rule the model breaks.
pub(super) fn check(model: &Model) {
  packages(model);
  modules(model);
  defs(model);
  contributions(model);
  members(model);
  lists(model);
  walk(model);
  types(model);
}

/// Packages: `core`, `alloc` and `std`, the root, then dependencies, each numbering its sources
/// after the previous one's, each with its root module.
fn packages(model: &Model) {
  let mut next: u32 = 0;
  for (index, row) in model.packages.iter().enumerate() {
    let expected = match index.cmp(&ROOT) {
      Ordering::Less => PackageKind::BuiltIn,
      Ordering::Equal => PackageKind::Root,
      Ordering::Greater => PackageKind::Dependency,
    };
    assert_eq!(
      row.kind, expected,
      "package {index} is of the kind its position gives"
    );
    assert_eq!(
      row.sources.start(),
      next,
      "package {index}'s sources follow the previous package's"
    );
    next = row.sources.end();
    assert_ne!(
      row.root_module, UNSET_MODULE,
      "package {index} has a root module"
    );
    let root = &model.modules[row.root_module.get() as usize];
    assert!(
      root.package.get() as usize == index && root.kind == ModuleKind::Root,
      "package {index}'s root module is its own, of the root kind"
    );
  }
  tiles(
    model
      .packages
      .iter()
      .map(|row: &PackageRow| row.dependencies),
    model.dependencies.len(),
    "package dependencies",
  );
}

/// Modules: by package, then by path, segment by segment, each segment bytewise; a root module
/// first in each package, and every other module inside an earlier one of its package. Only an
/// inline module has the location, extent and description of a declaration.
fn modules(model: &Model) {
  let mut previous: Option<&ModuleRow> = None;
  for (index, row) in model.modules.iter().enumerate() {
    if row.kind == ModuleKind::Inline {
      assert!(
        row.location.is_some() && row.extent.is_some(),
        "module {index}, an inline module, has its declaration's location and extent"
      );
    } else {
      assert!(
        row.location.is_none() && row.extent.is_none() && row.description.is_none(),
        "module {index}, which no `mod` declares, has no location, extent or description"
      );
    }
    match row.parent {
      None => {
        assert_eq!(
          row.kind,
          ModuleKind::Root,
          "module {index} without a parent is a root module"
        );
        assert_eq!(
          row.path.len, 0,
          "module {index}, a root module, has an empty path"
        );
      }
      Some(parent) => {
        assert_ne!(
          row.kind,
          ModuleKind::Root,
          "module {index} with a parent is no root module"
        );
        assert!(
          (parent.get() as usize) < index,
          "module {index}'s parent is appended before it"
        );
        let up = &model.modules[parent.get() as usize];
        assert_eq!(
          up.package, row.package,
          "module {index} is inside a module of its package"
        );
        assert_eq!(
          up.path.len + 1,
          row.path.len,
          "module {index}'s path is its parent's and one segment"
        );
      }
    }
    if let Some(previous) = previous {
      let order = previous
        .package
        .cmp(&row.package)
        .then_with(|| compare_paths(model, previous.path, row.path));
      assert_eq!(
        order,
        Ordering::Less,
        "module {index} is appended in module order: by package, then by path"
      );
    }
    previous = Some(row);
  }
}

/// Definitions: by module, then by source id and position within one, `core`'s materialised
/// primitives first, in their own order.
///
/// A GraphQL built-in the root package replaces keeps its built-in's place while its name is
/// written in the root package, so its position is not compared.
fn defs(model: &Model) {
  let mut previous: Option<&DefRow> = None;
  for (index, row) in model.defs.iter().enumerate() {
    assert!(
      (row.module.get() as usize) < model.modules.len(),
      "definition {index}'s module is one of the model's"
    );
    if let Some(previous) = previous {
      assert!(
        previous.module <= row.module,
        "definition {index} is appended in module order"
      );
      if previous.module == row.module && written_in(model, previous) && written_in(model, row) {
        let (before, after) = (key(previous.location), key(row.location));
        assert!(
          before < after || (before == after && row.location.is_entire()),
          "definition {index} is appended by source id, then position"
        );
      }
      if let (Some(before), Some(after)) = (previous.intrinsic, row.intrinsic)
        && previous.module == row.module
      {
        assert!(
          before.rank() < after.rank(),
          "definition {index}: the primitives are appended bool, f32, f64, then the signed and the \
           unsigned widths ascending"
        );
      }
    }
    previous = Some(row);
  }
}

/// Contributions: a definition's own first, then only extensions, and only an extension of a
/// definition keeps the type parameter list it writes; the schema's definitions, then only
/// extensions.
fn contributions(model: &Model) {
  for (index, def) in model.defs.iter().enumerate() {
    for (position, part) in def
      .contributions
      .of(&model.contributions)
      .iter()
      .enumerate()
    {
      assert_eq!(
        part.extension,
        position > 0,
        "definition {index}'s first contribution is its own, and every later one an extension"
      );
      assert!(
        part.extension || part.params.len == 0,
        "definition {index}'s own names are its type parameters, and its own contribution keeps no \
         list"
      );
    }
  }
  let mut extending = false;
  for part in model.schema.contributions.of(&model.contributions) {
    assert!(
      part.extension || !extending,
      "the schema's definitions are appended before its extensions"
    );
    assert_eq!(part.params.len, 0, "a schema item names no type parameter");
    extending = part.extension;
  }
}

/// Members: owner by owner; an owner's direct members first, which its contributions hold in
/// order; then each field's arguments, field by field.
fn members(model: &Model) {
  let mut previous: Option<&MemberRow> = None;
  for (index, row) in model.members.iter().enumerate() {
    let id = MemberId::new(dense(index));
    let owner = &model.defs[row.owner.get() as usize];
    match row.parent {
      None => {
        assert!(
          owner.members.contains(id),
          "member {index} is one of its owner's direct members"
        );
        assert_eq!(
          Some(row.kind),
          owner.kind.member_kind(),
          "member {index} is of the kind its owner's kind has"
        );
      }
      Some(field) => {
        let parent = &model.members[field.get() as usize];
        assert!(
          parent.kind == MemberKind::Field
            && parent.owner == row.owner
            && parent.arguments.contains(id),
          "member {index} is one of the arguments of a field of its owner"
        );
        assert_eq!(
          row.kind,
          MemberKind::Argument,
          "member {index} is an argument"
        );
      }
    }
    if let Some(previous) = previous {
      assert!(
        previous.owner <= row.owner,
        "member {index} is appended owner by owner"
      );
      if previous.owner == row.owner {
        match (previous.parent, row.parent) {
          (Some(_), None) => {
            panic!("member {index}: an owner's direct members come before every argument")
          }
          (Some(before), Some(after)) => assert!(
            before <= after,
            "member {index}: arguments are appended field by field"
          ),
          (None, _) => {}
        }
      }
    }
    previous = Some(row);
  }

  for (index, def) in model.defs.iter().enumerate() {
    let mut next = def.members.start();
    for part in def.contributions.of(&model.contributions) {
      if !part.members.is_empty() {
        assert_eq!(
          part.members.start(),
          next,
          "definition {index}'s contributions hold its direct members, in contribution order"
        );
        next = part.members.end();
      }
    }
    if !def.members.is_empty() {
      assert_eq!(
        next,
        def.members.end(),
        "definition {index}'s contributions hold every direct member it has"
      );
    }
  }
}

/// Lists that grow at the end of their pools: each owner's run follows the previous owner's, so
/// every pool is its owners' runs in owner order, and each contribution holds the next part of its
/// definition's lists.
fn lists(model: &Model) {
  let schema = &model.schema;
  tiles(
    model
      .defs
      .iter()
      .map(|row: &DefRow| row.contributions)
      .chain([schema.contributions]),
    model.contributions.len(),
    "contributions",
  );
  tiles(
    model.defs.iter().map(|row: &DefRow| row.type_params),
    model.type_params.len(),
    "type parameters",
  );
  tiles(
    model
      .contributions
      .iter()
      .map(|part: &ContributionRow| part.params),
    model.written_params.len(),
    "extensions' own type parameter names",
  );
  tiles(
    model.defs.iter().map(|row: &DefRow| row.bounds),
    model.bounds.len(),
    "bounds",
  );
  tiles(
    model.defs.iter().map(|row: &DefRow| row.implements),
    model.implements.len(),
    "implements entries",
  );
  tiles(
    model.defs.iter().map(|row: &DefRow| row.union_members),
    model.union_members.len(),
    "union members",
  );
  tiles(
    model
      .defs
      .iter()
      .map(|row: &DefRow| row.applications)
      .chain([schema.applications]),
    model.listed_applications.len(),
    "definition and schema applications",
  );
  tiles(
    model.defs.iter().map(|row: &DefRow| row.locations),
    model.locations.len(),
    "directive locations",
  );
  tiles(
    model.defs.iter().map(|row: &DefRow| row.externs),
    model.externs.len(),
    "externs",
  );
  tiles(
    model
      .applications
      .iter()
      .map(|row: &ApplicationRow| row.arguments),
    model.arguments.len(),
    "application arguments",
  );

  for def in &model.defs {
    let parts = def.contributions.of(&model.contributions);
    within(
      parts.iter().map(|part: &ContributionRow| part.implements),
      def.implements,
      "implements entries",
    );
    within(
      parts
        .iter()
        .map(|part: &ContributionRow| part.union_members),
      def.union_members,
      "union members",
    );
    within(
      parts.iter().map(|part: &ContributionRow| part.applications),
      def.applications,
      "applications",
    );
  }
  within(
    schema
      .contributions
      .of(&model.contributions)
      .iter()
      .map(|part: &ContributionRow| part.applications),
    schema.applications,
    "schema applications",
  );
}

/// The walk of this module's header, reaching every type occurrence, value, application and
/// member application exactly once and in id order.
fn walk(model: &Model) {
  let mut cursor = Cursor::default();
  for def in &model.defs {
    for param in def.type_params.of(&model.type_params) {
      if let Some(default) = param.default {
        cursor.type_expr(model, default);
      }
    }
    for bound in def.bounds.of(&model.bounds) {
      cursor.type_expr(model, bound.interface);
    }
    for entry in def.implements.of(&model.implements) {
      cursor.type_expr(model, *entry);
    }
    for entry in def.union_members.of(&model.union_members) {
      cursor.type_expr(model, *entry);
    }
    for application in def.applications.of(&model.listed_applications) {
      cursor.application(model, *application);
    }
    for member in def.members {
      cursor.member(model, member);
      let field = &model.members[member.get() as usize];
      for argument in field.arguments {
        cursor.member(model, argument);
      }
    }
  }
  for root in model.schema.roots.iter().flatten() {
    if let Root::Written(ty) = root {
      cursor.type_expr(model, *ty);
    }
  }
  for application in model.schema.applications.of(&model.listed_applications) {
    cursor.application(model, *application);
  }

  assert_eq!(
    cursor.type_exprs as usize,
    model.type_exprs.len(),
    "the walk reaches every type occurrence"
  );
  assert_eq!(
    cursor.values as usize,
    model.values.len(),
    "the walk reaches every value"
  );
  assert_eq!(
    cursor.applications as usize,
    model.applications.len(),
    "the walk reaches every application"
  );
  assert_eq!(
    cursor.member_applications as usize,
    model.member_applications.len(),
    "the walk reaches every member's applications"
  );
  for (index, row) in model.type_exprs.iter().enumerate() {
    assert!(
      (row.ty.get() as usize) < model.tys.len(),
      "type occurrence {index} has a type"
    );
  }
  for (index, row) in model.values.iter().enumerate() {
    assert!(
      (row.ty.get() as usize) < model.tys.len(),
      "value {index} has a type"
    );
  }
}

/// Types: a type's arguments and element are interned before it; a named type has one argument
/// for each of its definition's type parameters; and the unresolved type is no argument and no
/// element of any type, since it makes unresolved whatever holds it.
fn types(model: &Model) {
  for (index, row) in model.tys.iter().enumerate() {
    let id = dense(index);
    let args = row.args.of(&model.ty_args);
    for arg in args {
      assert!(
        arg.get() < id,
        "type {index}'s arguments are interned before it"
      );
      assert_ne!(
        model.tys[arg.get() as usize].shape,
        KIND_UNRESOLVED,
        "type {index} has no unresolved argument"
      );
    }
    if row.shape == KIND_LIST {
      assert!(
        row.element.get() < id,
        "type {index}'s element is interned before it"
      );
      assert_ne!(
        model.tys[row.element.get() as usize].shape,
        KIND_UNRESOLVED,
        "type {index}, a list, has no unresolved element"
      );
    }
    if row.shape == KIND_NAMED {
      assert_eq!(
        args.len(),
        model.defs[row.def.get() as usize].type_params.len as usize,
        "type {index} has one argument for each of its definition's type parameters"
      );
    }
  }
}

/// Where the walk has got to: the next id of each kind it expects.
#[derive(Default)]
struct Cursor {
  type_exprs: u32,
  values: u32,
  applications: u32,
  /// The next entry of the member-application pool.
  member_applications: u32,
}

impl Cursor {
  /// Reaches the type occurrence `root`, then every occurrence it nests, in pre-order.
  fn type_expr(&mut self, model: &Model, root: TypeExprId) {
    let mut stack: Vec<TypeExprId> = vec![root];
    while let Some(id) = stack.pop() {
      assert_eq!(
        id.get(),
        self.type_exprs,
        "type occurrences are appended in the walk's order"
      );
      self.type_exprs += 1;
      let row = &model.type_exprs[id.get() as usize];
      if row.shape == KIND_LIST {
        stack.push(row.element);
      }
      stack.extend(row.args.of(&model.type_expr_args).iter().rev().copied());
    }
  }

  /// Reaches the value `root`, then every value it holds, in pre-order.
  fn value(&mut self, model: &Model, root: ValueId) {
    let mut stack: Vec<ValueId> = vec![root];
    while let Some(id) = stack.pop() {
      assert_eq!(
        id.get(),
        self.values,
        "values are appended in the walk's order"
      );
      self.values += 1;
      let row = &model.values[id.get() as usize];
      match row.shape {
        VALUE_LIST | VALUE_SET => {
          stack.extend(row.run.of(&model.value_items).iter().rev().copied());
        }
        VALUE_MAP => {
          for &(key, item) in row.run.of(&model.map_entries).iter().rev() {
            stack.push(item);
            stack.push(key);
          }
        }
        VALUE_OBJECT => {
          for entry in row.run.of(&model.object_entries).iter().rev() {
            stack.push(entry.value);
          }
        }
        _ => {}
      }
    }
  }

  /// Reaches `application`, then its type arguments, then each argument's value, its arguments in
  /// the order of its directive's: the argument each sets never goes back. Its type, when it has
  /// one, is the unresolved type or a named type of the directive it applies.
  fn application(&mut self, model: &Model, application: ApplicationId) {
    let index = application.get();
    assert_eq!(
      index, self.applications,
      "applications are appended in the walk's order"
    );
    self.applications += 1;
    let row = &model.applications[index as usize];
    if row.ty != UNSET_TY {
      let ty = &model.tys[row.ty.get() as usize];
      let applied = match row.directive {
        Resolution::Resolved(directive) => ty.shape == KIND_NAMED && ty.def == directive,
        Resolution::Unresolved => false,
      };
      assert!(
        applied || ty.shape == KIND_UNRESOLVED,
        "application {index}'s type is the unresolved type or a named type of its directive"
      );
    }
    for arg in row.type_args.of(&model.application_type_args) {
      self.type_expr(model, *arg);
    }
    let mut previous: Option<MemberId> = None;
    for argument in row.arguments.of(&model.arguments) {
      assert!(
        previous.is_none_or(|before: MemberId| before <= argument.argument),
        "application {index}'s arguments are in the order of its directive's"
      );
      previous = Some(argument.argument);
      self.value(model, argument.value);
    }
  }

  /// Reaches `member`'s type, its default, then its applications, which are the next run of the
  /// member-application pool. Its arguments are the caller's to reach.
  fn member(&mut self, model: &Model, member: MemberId) {
    let row = &model.members[member.get() as usize];
    if let Some(ty) = row.ty {
      self.type_expr(model, ty);
    }
    if let Some(default) = row.default {
      self.value(model, default);
    }
    if row.applications.len != 0 {
      assert_eq!(
        row.applications.start, self.member_applications,
        "a member's applications are appended in the walk's order"
      );
      self.member_applications = row.applications.end();
    }
    for application in row.applications.of(&model.member_applications) {
      self.application(model, *application);
    }
  }
}

/// Asserts that `runs`, in order, are every entry of a pool `len` entries long, each non-empty run
/// starting where the one before ended.
fn tiles(runs: impl Iterator<Item = Run>, len: usize, what: &str) {
  let mut next: u32 = 0;
  for run in runs {
    if run.len != 0 {
      assert_eq!(
        run.start, next,
        "{what} are appended owner by owner, each list at the end of its pool"
      );
      next = run.end();
    }
  }
  assert_eq!(
    next as usize, len,
    "every entry of the {what} belongs to a list"
  );
}

/// Asserts that `parts`, in order, are the whole of `whole`.
fn within(parts: impl Iterator<Item = Run>, whole: Run, what: &str) {
  let mut next = whole.start;
  for part in parts {
    if part.len != 0 {
      assert_eq!(
        part.start, next,
        "each contribution holds the next part of its owner's {what}"
      );
      next = part.end();
    }
  }
  assert_eq!(
    next,
    whole.end(),
    "the contributions hold all of their owner's {what}"
  );
}

/// Two module paths, segment by segment, each segment bytewise, a path before every path it
/// begins.
///
/// A name is interned once, so two segments of one symbol are one spelling and two of different
/// symbols are different spellings. The paths are therefore compared symbol by symbol, and only
/// the first two segments whose symbols differ are compared bytewise: that pair decides, as it
/// does when every segment is compared bytewise, since every pair before it is equal either way.
/// Two neighbouring modules share their ancestors' segments, so comparing those by symbol is what
/// keeps a long name from being read again for every module under it.
fn compare_paths(model: &Model, a: Run, b: Run) -> Ordering {
  let left: &[Sym] = a.of(&model.paths);
  let right: &[Sym] = b.of(&model.paths);
  for (x, y) in left.iter().zip(right) {
    if x != y {
      return model.name(*x).as_bytes().cmp(model.name(*y).as_bytes());
    }
  }
  left.len().cmp(&right.len())
}

/// Whether `row`'s name is written in a source of its module's package. Only a GraphQL built-in
/// the root package replaces has it written elsewhere.
fn written_in(model: &Model, row: &DefRow) -> bool {
  let package = model.modules[row.module.get() as usize].package;
  model.packages[package.get() as usize]
    .sources
    .contains(SourceId::new(row.location.source()))
}

/// A location's place in the order of definitions: by source, a whole source before every span of
/// it, then by span.
fn key(at: Location) -> (u32, Option<(usize, usize)>) {
  (
    at.source(),
    at.span().map(|span: SimpleSpan| (span.start(), span.end())),
  )
}
