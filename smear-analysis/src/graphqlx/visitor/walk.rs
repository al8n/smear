//! The walk itself: the model's entities in id order, each nested occurrence reached from an
//! explicit stack rather than by recursion.

use std::vec::Vec;

use tokora::diagnostic::Location;

use crate::graphqlx::{
  diagnostic::Collector,
  id::{ApplicationId, DefId, MemberId, ModuleId, PackageId, TypeExprId, ValueId, dense},
  model::{
    Application, Contribution, Def, Member, Model, Root, RootOperation, Schema, TypeExpr,
    TypeExprKind, ValueKind,
  },
  source::SourceMap,
};

use super::{Cx, Packages, Site, Visitor};

/// Walks `model`, whose sources `sources` numbers, for `visitor` over what `packages` wrote,
/// reporting into `collector`, and answers the collector.
pub(super) fn drive<V: Visitor + ?Sized>(
  model: &Model,
  sources: &SourceMap,
  packages: Packages,
  collector: Collector,
  visitor: &mut V,
) -> Collector {
  let mut walk = Walk::new(model, sources, packages, collector);
  walk.run(visitor);
  walk.cx.collector
}

/// One walk: the context the hooks are handed, which packages' writing it covers, and the
/// occurrences still to reach under the current outermost one.
struct Walk<'a> {
  model: &'a Model,
  cx: Cx<'a>,
  /// Whether the walk covers each package's writing, in package order.
  covered: Vec<bool>,
  /// The end of each package's sources, in package order: which package a source is.
  ends: Vec<u32>,
  /// The type occurrences still to reach. Empty between two outermost occurrences.
  types: Vec<TypeExprId>,
  /// The values still to reach. Empty between two outermost values.
  values: Vec<ValueId>,
}

impl<'a> Walk<'a> {
  /// A walk of `model` that covers what `packages` wrote, at no entity yet.
  fn new(
    model: &'a Model,
    sources: &'a SourceMap,
    packages: Packages,
    collector: Collector,
  ) -> Self {
    let count = model.packages().len() as usize;
    let mut covered: Vec<bool> = Vec::with_capacity(count);
    let mut ends: Vec<u32> = Vec::with_capacity(count);
    for id in model.packages() {
      let package = model.package(id);
      covered.push(packages.holds(package.kind()));
      ends.push(package.sources().end());
    }
    Self {
      model,
      cx: Cx::new(model, sources, collector),
      covered,
      ends,
      types: Vec::new(),
      values: Vec::new(),
    }
  }

  /// Whether the walk covers `package`'s writing.
  fn covers(&self, package: PackageId) -> bool {
    self
      .covered
      .get(package.get() as usize)
      .copied()
      .unwrap_or(false)
  }

  /// Whether the walk covers `contribution`, of a definition of the package `home`: whether it
  /// covers the package of the source the contribution is written in. A location in no package's
  /// sources, which only a hand-built model can hold, is taken as written in `home`.
  fn writes(&self, contribution: &Contribution<'_>, home: PackageId) -> bool {
    self.covers(self.writer(contribution.location(), home))
  }

  /// The package whose sources hold `at`'s source, or `home` when none does.
  fn writer(&self, at: Location, home: PackageId) -> PackageId {
    let source = at.source();
    // The packages number their sources one after another from 0, so the first package whose
    // sources end past `source` is the one that holds it.
    let index = self.ends.partition_point(|end: &u32| *end <= source);
    if index < self.ends.len() {
      PackageId::new(dense(index))
    } else {
      home
    }
  }

  /// The whole walk: `begin`, the packages, the schema, `end`.
  fn run<V: Visitor + ?Sized>(&mut self, visitor: &mut V) {
    let model: &'a Model = self.model;
    visitor.begin(&mut self.cx);

    // A definition is entered when the walk covers one of its contributions.
    let mut entered: Vec<bool> = Vec::with_capacity(model.defs().len() as usize);
    for id in model.defs() {
      let def: Def<'a> = model.def(id);
      let home = model.module(def.module()).package();
      entered.push(
        def
          .contributions()
          .any(|part: Contribution<'a>| self.writes(&part, home)),
      );
    }
    let holds = |module: ModuleId| -> bool {
      model
        .module(module)
        .defs()
        .into_iter()
        .any(|def: DefId| entered[def.get() as usize])
    };

    // Modules are numbered package by package, so each package's modules are the next run of
    // module ids.
    let modules = model.modules().len();
    let mut next: u32 = 0;
    for package in model.packages() {
      let first = next;
      while next < modules && model.module(ModuleId::new(next)).package() == package {
        next += 1;
      }
      let covered = self.covers(package);
      let entering = covered || (first..next).any(|index: u32| holds(ModuleId::new(index)));
      if !entering {
        continue;
      }
      self.cx.package = Some(package);
      visitor.enter_package(&mut self.cx, package);
      for index in first..next {
        let module = ModuleId::new(index);
        if covered || holds(module) {
          self.walk_module(visitor, module, &entered);
        }
      }
      visitor.exit_package(&mut self.cx, package);
      self.cx.package = None;
    }

    if self.covers(model.root_package()) {
      self.walk_schema(visitor);
    }
    visitor.end(&mut self.cx);
  }

  /// One module: its hooks, around the definitions it holds itself that the walk enters.
  fn walk_module<V: Visitor + ?Sized>(
    &mut self,
    visitor: &mut V,
    module: ModuleId,
    entered: &[bool],
  ) {
    let model: &'a Model = self.model;
    self.cx.module = Some(module);
    visitor.enter_module(&mut self.cx, module);
    for def in model.module(module).defs() {
      if entered[def.get() as usize] {
        self.walk_def(visitor, def);
      }
    }
    visitor.exit_module(&mut self.cx, module);
    self.cx.module = None;
  }

  /// One definition: its own generics when the walk covers its own contribution, then the covered
  /// contributions' `implements` entries, union members, applications and members, each kind in
  /// contribution order.
  fn walk_def<V: Visitor + ?Sized>(&mut self, visitor: &mut V, id: DefId) {
    let model: &'a Model = self.model;
    let def: Def<'a> = model.def(id);
    let home = model.module(def.module()).package();
    self.cx.def = Some(id);
    visitor.enter_def(&mut self.cx, id);

    // A definition's first contribution is its own, which writes its type parameters, their
    // defaults and its bounds.
    if let Some(own) = def.contributions().next()
      && self.writes(&own, home)
    {
      self.cx.contribution = Some(own);
      for param in def.type_params() {
        if let Some(default) = param.default() {
          self.walk_types(visitor, default, Site::TypeParamDefault);
        }
      }
      for bound in def.bounds() {
        self.walk_types(visitor, bound.interface(), Site::Bound);
      }
    }
    for part in def.contributions() {
      if self.writes(&part, home) {
        self.cx.contribution = Some(part);
        for entry in part.implements() {
          self.walk_types(visitor, *entry, Site::Implements);
        }
      }
    }
    for part in def.contributions() {
      if self.writes(&part, home) {
        self.cx.contribution = Some(part);
        for entry in part.union_members() {
          self.walk_types(visitor, *entry, Site::UnionMember);
        }
      }
    }
    for part in def.contributions() {
      if self.writes(&part, home) {
        self.cx.contribution = Some(part);
        for application in part.applications() {
          self.walk_application(visitor, *application);
        }
      }
    }
    for part in def.contributions() {
      if self.writes(&part, home) {
        self.cx.contribution = Some(part);
        for member in part.members() {
          self.walk_member(visitor, member);
        }
      }
    }
    self.cx.contribution = None;

    visitor.exit_def(&mut self.cx, id);
    self.cx.def = None;
  }

  /// One direct member, with a field's arguments nested in it.
  fn walk_member<V: Visitor + ?Sized>(&mut self, visitor: &mut V, id: MemberId) {
    let model: &'a Model = self.model;
    let member: Member<'a> = model.member(id);
    self.cx.member = Some(id);
    visitor.enter_member(&mut self.cx, id);
    self.walk_parts(visitor, member);
    for argument in member.arguments() {
      self.cx.member = Some(argument);
      visitor.enter_member(&mut self.cx, argument);
      self.walk_parts(visitor, model.member(argument));
      visitor.exit_member(&mut self.cx, argument);
      self.cx.member = Some(id);
    }
    visitor.exit_member(&mut self.cx, id);
    self.cx.member = None;
  }

  /// A member's type, its default, then its applications.
  fn walk_parts<V: Visitor + ?Sized>(&mut self, visitor: &mut V, member: Member<'a>) {
    if let Some(ty) = member.ty() {
      self.walk_types(visitor, ty, Site::MemberType);
    }
    if let Some(default) = member.default() {
      self.walk_values(visitor, default, Site::Default);
    }
    for application in member.applications() {
      self.walk_application(visitor, *application);
    }
  }

  /// One application: itself, its type arguments, then each argument's value.
  fn walk_application<V: Visitor + ?Sized>(&mut self, visitor: &mut V, id: ApplicationId) {
    let model: &'a Model = self.model;
    let application: Application<'a> = model.application(id);
    self.cx.application = Some(id);
    visitor.application(&mut self.cx, id);
    for argument in application.type_args() {
      self.walk_types(visitor, *argument, Site::ApplicationTypeArgument);
    }
    for argument in application.arguments() {
      self.walk_values(visitor, argument.value(), Site::ApplicationArgument);
    }
    self.cx.application = None;
  }

  /// The type occurrence `root` and every occurrence nested in it, in pre-order: an occurrence,
  /// then its arguments, or its element.
  fn walk_types<V: Visitor + ?Sized>(&mut self, visitor: &mut V, root: TypeExprId, site: Site) {
    let model: &'a Model = self.model;
    self.cx.site = Some(site);
    self.types.push(root);
    while let Some(id) = self.types.pop() {
      visitor.type_expr(&mut self.cx, id);
      let occurrence: TypeExpr<'a> = model.type_expr(id);
      if let TypeExprKind::List { element } = occurrence.kind() {
        self.types.push(element);
      }
      // The arguments as written, whatever the occurrence resolved to: one that did not resolve
      // keeps the arguments it was written with, and they are occurrences too.
      self
        .types
        .extend(occurrence.written_args().iter().rev().copied());
    }
    self.cx.site = None;
  }

  /// The value `root` and every value nested in it, in pre-order: a value, then each list or set
  /// element, each map entry's key and then its value, or each object field's value.
  fn walk_values<V: Visitor + ?Sized>(&mut self, visitor: &mut V, root: ValueId, site: Site) {
    let model: &'a Model = self.model;
    self.cx.site = Some(site);
    self.values.push(root);
    while let Some(id) = self.values.pop() {
      visitor.value(&mut self.cx, id);
      match model.value(id).kind() {
        ValueKind::List(items) | ValueKind::Set(items) => {
          self.values.extend(items.iter().rev().copied());
        }
        ValueKind::Map(entries) => {
          for &(key, item) in entries.iter().rev() {
            self.values.push(item);
            self.values.push(key);
          }
        }
        ValueKind::Object(entries) => {
          for entry in entries.iter().rev() {
            self.values.push(entry.value());
          }
        }
        _ => {}
      }
    }
    self.cx.site = None;
  }

  /// The schema: its hook, each written root's type, then its applications, each under the schema
  /// contribution that wrote it. The schema is the root package's.
  fn walk_schema<V: Visitor + ?Sized>(&mut self, visitor: &mut V) {
    let model: &'a Model = self.model;
    let schema: Schema<'a> = model.schema();
    self.cx.package = Some(model.root_package());
    visitor.schema(&mut self.cx);
    for operation in [
      RootOperation::Query,
      RootOperation::Mutation,
      RootOperation::Subscription,
    ] {
      if let Some(Root::Written(root)) = schema.root(operation) {
        self.walk_types(visitor, root, Site::RootOperation);
      }
    }
    for part in schema.contributions() {
      self.cx.contribution = Some(part);
      for application in part.applications() {
        self.walk_application(visitor, *application);
      }
    }
    self.cx.contribution = None;
    self.cx.package = None;
  }
}
