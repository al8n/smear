//! What the phases after item collection write: what definitions hold beyond their identity,
//! members, type occurrences and types, values, applications, the schema and marks.
//!
//! GATED ON HAVING A CALLER. Lowering, resolution, instantiation, coercion and the tier phase are
//! this module's callers, and until they land the test-support door is its only one: a method that
//! no shipped build calls is dead code. A phase that lands moves the methods it calls out of this
//! module, as the module tree and item collection moved theirs.

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{ApplicationId, DefId, IdRange, MemberId, Sym, TyId, TypeExprId, ValueId, dense},
  model::{
    application::{ApplicationArgument, Resolution},
    def::{Collection, DirectiveLocation, TypeBound},
    member::MemberKind,
    schema::{Root, RootOperation},
    table::{
      ApplicationRow, ExternRow, FloatRow, IntRow, KIND_LIST, KIND_NAMED, KIND_PARAM,
      KIND_UNRESOLVED, LiteralRow, MemberRow, NO_DEF, PathRow, Run, TyRow, TypeExprRow,
      TypeParamRow, UNSET_TY, VALUE_BOOL, VALUE_ENUM, VALUE_FLOAT, VALUE_INT, VALUE_LIST,
      VALUE_LITERAL, VALUE_MAP, VALUE_NULL, VALUE_OBJECT, VALUE_SET, VALUE_STRING, VALUE_TYPE,
      VALUE_UNRESOLVED, ValueRow,
    },
    ty::PathRoot,
    value::{LiteralKind, ObjectEntry},
  },
  package::Tier,
};

use super::{ModelBuilder, append, contribution_row, extend, grow, push_text, unresolved_ty};

impl ModelBuilder {
  /// Appends a type parameter named `name`, written at `location`, to `def`. Answers its index.
  pub fn push_type_param(&mut self, def: DefId, name: Sym, location: Location) -> u32 {
    let row = &mut self.model.defs[def.get() as usize];
    let index = row.type_params.len;
    grow(
      &mut self.model.type_params,
      &mut row.type_params,
      TypeParamRow {
        name,
        location,
        default: None,
      },
    );
    index
  }

  /// Gives `def`'s type parameter `index` the default `default`.
  pub fn set_type_param_default(&mut self, def: DefId, index: u32, default: TypeExprId) {
    let at = slot(self.model.defs[def.get() as usize].type_params, index);
    self.model.type_params[at].default = Some(default);
  }

  /// Adds a bound to `def`, after those it has: its type parameter `parameter` must satisfy
  /// `interface`, and the bound is written at `location`.
  pub fn push_bound(
    &mut self,
    def: DefId,
    parameter: u32,
    interface: TypeExprId,
    location: Location,
  ) {
    let row = &mut self.model.defs[def.get() as usize];
    debug_assert!(
      parameter < row.type_params.len,
      "a bound bounds one of its definition's type parameters"
    );
    grow(
      &mut self.model.bounds,
      &mut row.bounds,
      TypeBound {
        parameter,
        interface,
        location,
      },
    );
  }

  /// Adds `interface` to the `implements` entries of `def`'s contribution `contribution`, after
  /// those it has.
  pub fn push_implements(&mut self, def: DefId, contribution: u32, interface: TypeExprId) {
    let index = dense(self.model.implements.len());
    let row = &mut self.model.defs[def.get() as usize];
    extend(&mut row.implements, index);
    let part = slot(row.contributions, contribution);
    extend(&mut self.model.contributions[part].implements, index);
    self.model.implements.push(interface);
  }

  /// Adds `member` to the union members of `def`'s contribution `contribution`, after those it
  /// has.
  pub fn push_union_member(&mut self, def: DefId, contribution: u32, member: TypeExprId) {
    let index = dense(self.model.union_members.len());
    let row = &mut self.model.defs[def.get() as usize];
    extend(&mut row.union_members, index);
    let part = slot(row.contributions, contribution);
    extend(&mut self.model.contributions[part].union_members, index);
    self.model.union_members.push(member);
  }

  /// Adds `application` to the applications of `def`'s contribution `contribution`, after those it
  /// has.
  pub fn push_def_application(
    &mut self,
    def: DefId,
    contribution: u32,
    application: ApplicationId,
  ) {
    let index = dense(self.model.listed_applications.len());
    let row = &mut self.model.defs[def.get() as usize];
    extend(&mut row.applications, index);
    let part = slot(row.contributions, contribution);
    extend(&mut self.model.contributions[part].applications, index);
    self.model.listed_applications.push(application);
  }

  /// Adds `location` to the locations of the directive definition `def`, after those it has.
  pub fn push_directive_location(&mut self, def: DefId, location: DirectiveLocation) {
    let row = &mut self.model.defs[def.get() as usize];
    grow(&mut self.model.locations, &mut row.locations, location);
  }

  /// Makes the directive definition `def` repeatable.
  pub fn set_repeatable(&mut self, def: DefId) {
    self.model.defs[def.get() as usize].repeatable = true;
  }

  /// Makes `def` a marker.
  pub fn set_marker(&mut self, def: DefId) {
    self.model.defs[def.get() as usize].marker = true;
  }

  /// Makes `def` stand for `collection`.
  pub fn set_collection(&mut self, def: DefId, collection: Collection) {
    self.model.defs[def.get() as usize].collection = Some(collection);
  }

  /// Adds an `@extern` to `def`, after those it has: its `lang` and `type`, and its effective
  /// tier.
  pub fn push_extern(&mut self, def: DefId, lang: &str, ty: &str, tier: Tier) {
    let lang = push_text(&mut self.model.text, lang);
    let ty = push_text(&mut self.model.text, ty);
    let row = &mut self.model.defs[def.get() as usize];
    grow(
      &mut self.model.externs,
      &mut row.externs,
      ExternRow { lang, ty, tier },
    );
  }

  /// Records `ty` as a closed instantiation of `def`. Instantiations are added in any order, and
  /// each is kept once.
  pub fn push_instantiation(&mut self, def: DefId, ty: TyId) {
    self.instantiations.push((def, ty));
  }

  /// Records that `ty` satisfies `interface`, declared or intrinsic. Satisfactions are added in
  /// any order, and each is kept once.
  pub fn push_satisfaction(&mut self, ty: TyId, interface: DefId) {
    self.model.satisfactions.push((ty, interface));
  }

  /// Appends a direct member of `owner`, contributed by its contribution `contribution`: a `kind`
  /// named `name`, its name written at `location`, spanning `extent`.
  ///
  /// Members are appended in id order: owner by owner; an owner's direct members first, by
  /// contribution and then as written; then each field's arguments, field by field, through
  /// [`push_argument`](Self::push_argument).
  pub fn push_member(
    &mut self,
    owner: DefId,
    contribution: u32,
    kind: MemberKind,
    name: Sym,
    location: Location,
    extent: Location,
  ) -> MemberId {
    let index = dense(self.model.members.len());
    let row = &mut self.model.defs[owner.get() as usize];
    debug_assert_eq!(
      row.kind.member_kind(),
      Some(kind),
      "a definition's direct members are of the one kind its own kind has"
    );
    row.members = extend_range(row.members, index);
    let part = slot(row.contributions, contribution);
    let part = &mut self.model.contributions[part];
    part.members = extend_range(part.members, index);
    self
      .model
      .members
      .push(member_row(kind, name, owner, None, location, extent));
    MemberId::new(index)
  }

  /// Appends an argument of `field`, named `name`, its name written at `location`, spanning
  /// `extent`.
  pub fn push_argument(
    &mut self,
    field: MemberId,
    name: Sym,
    location: Location,
    extent: Location,
  ) -> MemberId {
    let index = dense(self.model.members.len());
    let parent = &mut self.model.members[field.get() as usize];
    debug_assert_eq!(parent.kind, MemberKind::Field, "only a field has arguments");
    parent.arguments = extend_range(parent.arguments, index);
    let owner = parent.owner;
    self.model.members.push(member_row(
      MemberKind::Argument,
      name,
      owner,
      Some(field),
      location,
      extent,
    ));
    MemberId::new(index)
  }

  /// Gives `member` the description `text`, cooked, written at `location`.
  pub fn set_member_description(&mut self, member: MemberId, text: &str, location: Location) {
    let description = self.description(text, location);
    self.model.members[member.get() as usize].description = Some(description);
  }

  /// Gives `member` its type, as written.
  pub fn set_member_type(&mut self, member: MemberId, ty: TypeExprId) {
    self.model.members[member.get() as usize].ty = Some(ty);
  }

  /// Gives `member` its default value.
  pub fn set_member_default(&mut self, member: MemberId, value: ValueId) {
    self.model.members[member.get() as usize].default = Some(value);
  }

  /// Adds `application` to `member`'s applications, after those it has.
  pub fn push_member_application(&mut self, member: MemberId, application: ApplicationId) {
    let row = &mut self.model.members[member.get() as usize];
    grow(
      &mut self.model.member_applications,
      &mut row.applications,
      application,
    );
  }

  /// Appends a type occurrence written at `location`, non-null when `non_null`.
  ///
  /// Type occurrences are appended in the walk's order, an occurrence before its arguments and
  /// its element. One starts unresolved, with no path, no arguments and no type; the methods
  /// below give it them.
  pub fn push_type_expr(&mut self, location: Location, non_null: bool) -> TypeExprId {
    let id = TypeExprId::new(dense(self.model.type_exprs.len()));
    self.model.type_exprs.push(TypeExprRow {
      shape: KIND_UNRESOLVED,
      def: NO_DEF,
      index: 0,
      element: TypeExprId::new(0),
      args: Run::default(),
      non_null,
      location,
      path: None,
      ty: UNSET_TY,
    });
    id
  }

  /// Records the path `type_expr` writes: from `root`, through `segments`, written at `location`.
  pub fn set_type_expr_path(
    &mut self,
    type_expr: TypeExprId,
    root: PathRoot,
    segments: &[Sym],
    location: Location,
  ) {
    let segments = append(&mut self.model.paths, segments);
    self.model.type_exprs[type_expr.get() as usize].path = Some(PathRow {
      root,
      segments,
      location,
    });
  }

  /// Records the arguments `type_expr` writes, in order, each appended after it.
  pub fn set_type_expr_args(&mut self, type_expr: TypeExprId, args: &[TypeExprId]) {
    debug_assert!(
      args.iter().all(|arg: &TypeExprId| *arg > type_expr),
      "an occurrence's arguments are appended after it"
    );
    let args = append(&mut self.model.type_expr_args, args);
    self.model.type_exprs[type_expr.get() as usize].args = args;
  }

  /// Makes `type_expr` the list of `element`, appended after it.
  pub fn set_type_expr_list(&mut self, type_expr: TypeExprId, element: TypeExprId) {
    debug_assert!(element > type_expr, "a list's element is appended after it");
    let row = &mut self.model.type_exprs[type_expr.get() as usize];
    row.shape = KIND_LIST;
    row.element = element;
  }

  /// Resolves `type_expr` to the definition `def`.
  pub fn resolve_named(&mut self, type_expr: TypeExprId, def: DefId) {
    let row = &mut self.model.type_exprs[type_expr.get() as usize];
    row.shape = KIND_NAMED;
    row.def = def;
  }

  /// Resolves `type_expr` to `def`'s type parameter `index`.
  pub fn resolve_param(&mut self, type_expr: TypeExprId, def: DefId, index: u32) {
    let row = &mut self.model.type_exprs[type_expr.get() as usize];
    row.shape = KIND_PARAM;
    row.def = def;
    row.index = index;
  }

  /// Gives `type_expr` its type.
  pub fn set_type_expr_ty(&mut self, type_expr: TypeExprId, ty: TyId) {
    debug_assert!(
      (ty.get() as usize) < self.model.tys.len(),
      "a type is interned before an occurrence is given it"
    );
    self.model.type_exprs[type_expr.get() as usize].ty = ty;
  }

  /// Interns the named type of `def` with `args`, every argument of it, each interned before it.
  pub fn intern_named(&mut self, def: DefId, args: &[TyId], non_null: bool) -> TyId {
    let closed = args
      .iter()
      .all(|arg: &TyId| self.model.tys[arg.get() as usize].closed);
    let row = TyRow {
      shape: KIND_NAMED,
      def,
      index: 0,
      element: TyId::new(0),
      args: Run::default(),
      non_null,
      closed,
      tier: Tier::Core,
    };
    self.intern_type(row, args)
  }

  /// Interns `def`'s type parameter `index`.
  pub fn intern_param(&mut self, def: DefId, index: u32, non_null: bool) -> TyId {
    let row = TyRow {
      shape: KIND_PARAM,
      def,
      index,
      element: TyId::new(0),
      args: Run::default(),
      non_null,
      closed: false,
      tier: Tier::Core,
    };
    self.intern_type(row, &[])
  }

  /// Interns the list of `element`, interned before it.
  pub fn intern_list(&mut self, element: TyId, non_null: bool) -> TyId {
    let closed = self.model.tys[element.get() as usize].closed;
    let row = TyRow {
      shape: KIND_LIST,
      def: NO_DEF,
      index: 0,
      element,
      args: Run::default(),
      non_null,
      closed,
      tier: Tier::Core,
    };
    self.intern_type(row, &[])
  }

  /// Interns the one type every unresolved occurrence shares.
  pub fn intern_unresolved(&mut self) -> TyId {
    self.intern_type(unresolved_ty(), &[])
  }

  /// Gives `ty` the tier it requires.
  pub fn set_ty_tier(&mut self, ty: TyId, tier: Tier) {
    self.model.tys[ty.get() as usize].tier = tier;
  }

  /// Appends a value written at `location`, or filled from a default when `written` is false.
  ///
  /// Values are appended in the walk's order, a list, set, map or object before what it holds.
  /// One starts unresolved, with no type; the methods below give it its shape and its type.
  pub fn push_value(&mut self, location: Location, written: bool) -> ValueId {
    let id = ValueId::new(dense(self.model.values.len()));
    self.model.values.push(ValueRow {
      shape: VALUE_UNRESOLVED,
      def: NO_DEF,
      member: MemberId::new(0),
      payload: 0,
      run: Run::default(),
      location,
      ty: UNSET_TY,
      written,
      path: None,
    });
    id
  }

  /// Makes `value` `null`.
  pub fn set_null(&mut self, value: ValueId) {
    self.value_row(value).shape = VALUE_NULL;
  }

  /// Makes `value` the boolean `flag`.
  pub fn set_bool(&mut self, value: ValueId, flag: bool) {
    let row = self.value_row(value);
    row.shape = VALUE_BOOL;
    row.payload = u32::from(flag);
  }

  /// Makes `value` an integer: negative when `negative`, of absolute value `magnitude`,
  /// little-endian, written as `text`. Trailing zero bytes of `magnitude` are not kept, so zero is
  /// no bytes, and zero is never negative.
  pub fn set_int(&mut self, value: ValueId, negative: bool, magnitude: &[u8], text: &str) {
    let len = magnitude
      .iter()
      .rposition(|byte: &u8| *byte != 0)
      .map_or(0, |last: usize| last + 1);
    let kept = &magnitude[..len];
    let int = IntRow {
      negative: negative && !kept.is_empty(),
      magnitude: append(&mut self.model.magnitudes, kept),
      text: push_text(&mut self.model.text, text),
    };
    let payload = dense(self.model.ints.len());
    self.model.ints.push(int);
    let row = self.value_row(value);
    row.shape = VALUE_INT;
    row.payload = payload;
  }

  /// Makes `value` the float `float`, written as `text`.
  pub fn set_float(&mut self, value: ValueId, float: f64, text: &str) {
    let text = push_text(&mut self.model.text, text);
    let payload = dense(self.model.floats.len());
    self.model.floats.push(FloatRow { value: float, text });
    let row = self.value_row(value);
    row.shape = VALUE_FLOAT;
    row.payload = payload;
  }

  /// Makes `value` the string `text`, cooked.
  pub fn set_string(&mut self, value: ValueId, text: &str) {
    let run = push_text(&mut self.model.text, text);
    let row = self.value_row(value);
    row.shape = VALUE_STRING;
    row.run = run;
  }

  /// Makes `value` the enum value `member` of the enum `def`.
  pub fn set_enum(&mut self, value: ValueId, def: DefId, member: MemberId) {
    let row = self.value_row(value);
    row.shape = VALUE_ENUM;
    row.def = def;
    row.member = member;
  }

  /// Makes `value` the type value naming `def`.
  pub fn set_type_value(&mut self, value: ValueId, def: DefId) {
    let row = self.value_row(value);
    row.shape = VALUE_TYPE;
    row.def = def;
  }

  /// Makes `value` the list of `elements`, each appended after it.
  pub fn set_list(&mut self, value: ValueId, elements: &[ValueId]) {
    self.set_items(value, VALUE_LIST, elements);
  }

  /// Makes `value` the set of `elements`, each appended after it.
  pub fn set_set(&mut self, value: ValueId, elements: &[ValueId]) {
    self.set_items(value, VALUE_SET, elements);
  }

  /// Makes `value` the map of `entries`, each a key and a value, each appended after it.
  pub fn set_map(&mut self, value: ValueId, entries: &[(ValueId, ValueId)]) {
    debug_assert!(
      entries
        .iter()
        .all(|&(key, item): &(ValueId, ValueId)| key > value && item > value),
      "what a value holds is appended after it"
    );
    let run = append(&mut self.model.map_entries, entries);
    let row = self.value_row(value);
    row.shape = VALUE_MAP;
    row.run = run;
  }

  /// Makes `value` the input object of `entries`: each the input field it sets, where the field's
  /// name is written (`None` for a field a default filled), and the field's value, appended after
  /// `value`.
  pub fn set_object(&mut self, value: ValueId, entries: &[(MemberId, Option<Location>, ValueId)]) {
    let start = dense(self.model.object_entries.len());
    for &(field, name, item) in entries {
      debug_assert!(item > value, "what a value holds is appended after it");
      self.model.object_entries.push(ObjectEntry {
        field,
        name,
        value: item,
      });
    }
    let row = self.value_row(value);
    row.shape = VALUE_OBJECT;
    row.run = Run {
      start,
      len: dense(entries.len()),
    };
  }

  /// Makes `value` a literal of `kind`, kept as written: `text`.
  pub fn set_literal(&mut self, value: ValueId, kind: LiteralKind, text: &str) {
    let text = push_text(&mut self.model.text, text);
    let payload = dense(self.model.literals.len());
    self.model.literals.push(LiteralRow { kind, text });
    let row = self.value_row(value);
    row.shape = VALUE_LITERAL;
    row.payload = payload;
  }

  /// Records the path an enum or type value writes: from `root`, through `segments`.
  pub fn set_value_path(&mut self, value: ValueId, root: PathRoot, segments: &[Sym]) {
    let segments = append(&mut self.model.paths, segments);
    let row = self.value_row(value);
    row.path = Some(PathRow {
      root,
      segments,
      location: row.location,
    });
  }

  /// Gives `value` the type it was coerced against.
  pub fn set_value_ty(&mut self, value: ValueId, ty: TyId) {
    debug_assert!(
      (ty.get() as usize) < self.model.tys.len(),
      "a type is interned before a value is given it"
    );
    self.value_row(value).ty = ty;
  }

  /// Appends an application written at `location`, its directive's path written from `root`
  /// through `segments` at `path_location`.
  ///
  /// Applications are appended in the walk's order. One starts unresolved, with no type argument
  /// and no argument; the methods below give it them.
  pub fn push_application(
    &mut self,
    location: Location,
    root: PathRoot,
    segments: &[Sym],
    path_location: Location,
  ) -> ApplicationId {
    let id = ApplicationId::new(dense(self.model.applications.len()));
    let segments = append(&mut self.model.paths, segments);
    self.model.applications.push(ApplicationRow {
      directive: Resolution::Unresolved,
      type_args: Run::default(),
      location,
      path: PathRow {
        root,
        segments,
        location: path_location,
      },
      arguments: Run::default(),
    });
    id
  }

  /// Resolves `application`'s directive to `directive`.
  pub fn resolve_application(&mut self, application: ApplicationId, directive: DefId) {
    self.model.applications[application.get() as usize].directive = Resolution::Resolved(directive);
  }

  /// Records the type arguments `application` writes, in order.
  pub fn set_application_type_args(&mut self, application: ApplicationId, args: &[TypeExprId]) {
    let args = append(&mut self.model.application_type_args, args);
    self.model.applications[application.get() as usize].type_args = args;
  }

  /// Adds an argument to `application`, after those it has: the directive definition's
  /// `argument`, where its name is written (`None` when a default filled it), and its value.
  pub fn push_application_argument(
    &mut self,
    application: ApplicationId,
    argument: MemberId,
    name: Option<Location>,
    value: ValueId,
  ) {
    let row = &mut self.model.applications[application.get() as usize];
    grow(
      &mut self.model.arguments,
      &mut row.arguments,
      ApplicationArgument {
        argument,
        name,
        value,
      },
    );
  }

  /// Appends a contribution to the schema: a schema definition or extension at `location`,
  /// spanning `extent`. Answers its index among the schema's contributions.
  ///
  /// The schema's contributions are appended after every definition's.
  pub fn push_schema_contribution(&mut self, location: Location, extent: Location) -> u32 {
    let index = self.model.schema.contributions.len;
    grow(
      &mut self.model.contributions,
      &mut self.model.schema.contributions,
      contribution_row(location, extent),
    );
    index
  }

  /// Gives the schema the description `text`, cooked, written at `location`.
  pub fn set_schema_description(&mut self, text: &str, location: Location) {
    let description = self.description(text, location);
    self.model.schema.description = Some(description);
  }

  /// Gives the schema `root` as its root for `operation`.
  pub fn set_schema_root(&mut self, operation: RootOperation, root: Root) {
    self.model.schema.roots[operation.index()] = Some(root);
  }

  /// Adds `application` to the applications of the schema's contribution `contribution`, after
  /// those it has.
  pub fn push_schema_application(&mut self, contribution: u32, application: ApplicationId) {
    let index = dense(self.model.listed_applications.len());
    extend(&mut self.model.schema.applications, index);
    let part = slot(self.model.schema.contributions, contribution);
    extend(&mut self.model.contributions[part].applications, index);
    self.model.listed_applications.push(application);
  }

  /// Marks, as an error reported at `at` does, every contribution and member whose extent holds
  /// `at`. Marks are applied when the model is finished.
  pub fn mark(&mut self, at: Location) {
    self.marks.push(at);
  }

  /// The row of `value`.
  fn value_row(&mut self, value: ValueId) -> &mut ValueRow {
    &mut self.model.values[value.get() as usize]
  }

  /// Makes `value` a list or a set, `shape`, of `elements`, each appended after it.
  fn set_items(&mut self, value: ValueId, shape: u8, elements: &[ValueId]) {
    debug_assert!(
      elements.iter().all(|item: &ValueId| *item > value),
      "what a value holds is appended after it"
    );
    let run = append(&mut self.model.value_items, elements);
    let row = self.value_row(value);
    row.shape = shape;
    row.run = run;
  }
}

/// The position in its pool of the entry `index` of `run`.
fn slot(run: Run, index: u32) -> usize {
  debug_assert!(index < run.len, "the entry is one of the list's");
  run.start as usize + index as usize
}

/// `range` with the member `index` added at its end, which must be where it ends: an owner's
/// direct members, and a field's arguments, are each one run of the member table.
fn extend_range<Id>(range: IdRange<Id>, index: u32) -> IdRange<Id> {
  let next = index.saturating_add(1);
  if range.is_empty() {
    return IdRange::new(index, next);
  }
  debug_assert_eq!(
    range.end(),
    index,
    "a run of members grows only at the end of the member table"
  );
  IdRange::new(range.start(), next)
}

/// A member with no description, type, default, argument or application yet.
fn member_row(
  kind: MemberKind,
  name: Sym,
  owner: DefId,
  parent: Option<MemberId>,
  location: Location,
  extent: Location,
) -> MemberRow {
  MemberRow {
    kind,
    name,
    owner,
    parent,
    location,
    extent,
    description: None,
    ty: None,
    default: None,
    arguments: IdRange::new(0, 0),
    applications: Run::default(),
    marked: false,
  }
}
