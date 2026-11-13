//! smear's own rules: what `analyze` runs, after its phases, through the one driver.
//!
//! A rule is one type in one file, implementing the public `Visitor` and nothing else, and the set
//! is a named struct of them that `rule_set!` declares. The rule types are private. The macro also
//! emits the set's table of each rule and the codes it reports, which the liveness test reads:
//! every code a rule lists must fire on at least one fixture.
//!
//! A rule reads the model the phases wrote, through its public views, and reports through the
//! context. Which package wrote a contribution or an application is read off where it is written,
//! by `SourceMap::written_by`, the one answer instantiation reads too. A rule says nothing about a
//! contribution a phase already refused something in, whose `has_errors()` is set: a diagnostic of
//! its own within that contribution would repeat the phase's. It sees the phases' marks alone,
//! since the rules' are applied once the walk has ended. `Silence` says it for an occurrence, and
//! answers for a written root, which no contribution holds, from one look at the schema's
//! contributions for the whole walk.
//!
//! Where an occurrence stands in its tree is told from the order the walk reaches occurrences in,
//! by `Nesting`, which reads one crate-private accessor, `TypeExpr::written_args`: the walk
//! descends the written arguments of an occurrence that names a type parameter, or nothing, and
//! the public kind does not answer them. `DuplicateTypeParameter` reads another,
//! `Def::extension_params`: the names an extension's own list writes, which no public view holds.

mod collection_arity;
mod duplicate_schema_definition;
mod duplicate_type_parameter;
mod generic_enum;
mod marker_misuse;
mod non_zero_argument;
mod orphan_extension;
mod schema_outside_root;
mod type_ref_misuse;

use self::{
  collection_arity::CollectionArity, duplicate_schema_definition::DuplicateSchemaDefinition,
  duplicate_type_parameter::DuplicateTypeParameter, generic_enum::GenericEnum,
  marker_misuse::MarkerMisuse, non_zero_argument::NonZeroArgument,
  orphan_extension::OrphanExtension, schema_outside_root::SchemaOutsideRoot,
  type_ref_misuse::TypeRefMisuse,
};
use super::{Contribution, Cx, DefId, Model, PackageId, TypeExprId, TypeExprKind, id::dense};

/// The definition `def`'s own contribution, its first, which is no extension, and the package that
/// wrote it, read off where it is written: the package that wrote the definition, whose
/// applications alone give it a meaning. That is the root package for a GraphQL built-in it
/// replaces, though the definition is the built-in's, and the built-in's package for every other
/// built-in. `None` when the first contribution is an extension, so that the answer never rests
/// on the order lowering appends contributions in.
fn own_contribution<'a>(cx: &Cx<'a>, def: DefId) -> Option<(Contribution<'a>, PackageId)> {
  let own = cx
    .model()
    .def(def)
    .contributions()
    .next()
    .filter(|part: &Contribution<'a>| !part.is_extension())?;
  let writer = cx.sources().written_by(own.location())?;
  Some((own, writer))
}

/// Whether a rule says nothing at the occurrence the walk has reached in `type_expr`: a phase
/// reported an error within the contribution that wrote it, or, for a written root, which no
/// contribution holds, within one of the schema's contributions.
///
/// The second is one answer for the whole walk, since the phases' marks are all applied before it
/// and the rules' after it. A rule that keeps one makes it in `begin`, so that it looks at the
/// schema's contributions once, and at no occurrence.
#[derive(Default)]
struct Silence {
  /// Whether a phase reported an error within one of the schema's contributions.
  schema: bool,
}

impl Silence {
  /// The silence of the walk `cx` begins.
  fn of(cx: &Cx<'_>) -> Self {
    Self {
      schema: cx
        .model()
        .schema()
        .contributions()
        .any(|part: Contribution<'_>| part.has_errors()),
    }
  }

  /// Whether a rule says nothing at the occurrence the walk has reached, `cx` saying where that
  /// is: one look at the contribution that holds it.
  fn silenced(&self, cx: &Cx<'_>) -> bool {
    match cx.contribution() {
      Some(part) => part.has_errors(),
      None => self.schema,
    }
  }
}

/// Where each occurrence the walk reaches stands in its tree, told from the order the walk reaches
/// them in: each outermost occurrence first, then everything it nests, in pre-order.
///
/// A rule that keeps one resets it in `begin`, and hands it every occurrence the walk reaches,
/// before it decides to say nothing about one. It keeps a count and a flag.
#[derive(Default)]
struct Nesting {
  /// The occurrences of the current tree still to come.
  pending: u32,
  /// Whether every occurrence of the current tree reached so far is a list.
  lists: bool,
}

/// Where an occurrence stands in its tree.
#[derive(Clone, Copy)]
struct Place {
  /// Whether it is the root of its tree, the outermost occurrence.
  root: bool,
  /// Whether every occurrence above it in its tree is a list: `TypeRef`, `[TypeRef!]` and
  /// `[[TypeRef]]` are, and an argument of any named occurrence is not.
  lists: bool,
}

impl Nesting {
  /// Takes `ty`, the occurrence of `model` the walk reached next, and answers where it stands.
  ///
  /// It is the root when no occurrence of the tree before it is still to come. A list has one
  /// child, so the lists above an occurrence are exactly the occurrences of its tree reached
  /// before it while each of them is a list.
  fn reach(&mut self, model: &Model, ty: TypeExprId) -> Place {
    let root = self.pending == 0;
    if root {
      self.lists = true;
    } else {
      self.pending = self.pending.saturating_sub(1);
    }
    let place = Place {
      root,
      lists: self.lists,
    };
    let occurrence = model.type_expr(ty);
    // What the walk reaches under it: its written arguments, whatever it resolved to, or a list's
    // element.
    let list = matches!(occurrence.kind(), TypeExprKind::List { .. });
    let children = dense(occurrence.written_args().len()).saturating_add(u32::from(list));
    self.pending = self.pending.saturating_add(children);
    self.lists &= list;
    place
  }
}

/// Declares a rule set: a struct with one field per rule, its `Visitor` impl, whose every hook
/// calls each rule's in field order, and its table of each rule with the codes it reports.
///
/// ```text
/// rule_set! {
///   /// What the set is.
///   pub(crate) struct Rules {
///     field: Rule => [CODE, …],
///   }
/// }
/// ```
///
/// Each rule implements `Visitor` and `Default`, and resets its state in `begin`; the set derives
/// `Default`, so `Rules::default()` is the set ready to run. The table, `TABLE`, lists each rule by
/// its type's name, in field order, and is compiled where something reads it: this crate's unit
/// tests and the test-support door.
macro_rules! rule_set {
  (
    $(#[$meta:meta])*
    $vis:vis struct $name:ident {
      $( $field:ident: $rule:ident => [$($code:expr),* $(,)?] ),+ $(,)?
    }
  ) => {
    $(#[$meta])*
    #[derive(Default)]
    $vis struct $name {
      $( $field: $rule, )+
    }

    #[cfg(any(test, feature = "test-support"))]
    impl $name {
      /// The rules of the set, in field order, each with the codes it reports.
      $vis const TABLE: &'static [(&'static str, &'static [::tokora::diagnostic::Code])] = &[
        $( (stringify!($rule), &[$($code),*]), )+
      ];
    }

    /// Each hook calls every rule's, in field order.
    impl $crate::graphqlx::Visitor for $name {
      fn begin(&mut self, cx: &mut $crate::graphqlx::Cx<'_>) {
        $( <$rule as $crate::graphqlx::Visitor>::begin(&mut self.$field, cx); )+
      }

      fn enter_package(
        &mut self,
        cx: &mut $crate::graphqlx::Cx<'_>,
        package: $crate::graphqlx::PackageId,
      ) {
        $( <$rule as $crate::graphqlx::Visitor>::enter_package(&mut self.$field, cx, package); )+
      }

      fn exit_package(
        &mut self,
        cx: &mut $crate::graphqlx::Cx<'_>,
        package: $crate::graphqlx::PackageId,
      ) {
        $( <$rule as $crate::graphqlx::Visitor>::exit_package(&mut self.$field, cx, package); )+
      }

      fn enter_module(
        &mut self,
        cx: &mut $crate::graphqlx::Cx<'_>,
        module: $crate::graphqlx::ModuleId,
      ) {
        $( <$rule as $crate::graphqlx::Visitor>::enter_module(&mut self.$field, cx, module); )+
      }

      fn exit_module(
        &mut self,
        cx: &mut $crate::graphqlx::Cx<'_>,
        module: $crate::graphqlx::ModuleId,
      ) {
        $( <$rule as $crate::graphqlx::Visitor>::exit_module(&mut self.$field, cx, module); )+
      }

      fn enter_def(&mut self, cx: &mut $crate::graphqlx::Cx<'_>, def: $crate::graphqlx::DefId) {
        $( <$rule as $crate::graphqlx::Visitor>::enter_def(&mut self.$field, cx, def); )+
      }

      fn exit_def(&mut self, cx: &mut $crate::graphqlx::Cx<'_>, def: $crate::graphqlx::DefId) {
        $( <$rule as $crate::graphqlx::Visitor>::exit_def(&mut self.$field, cx, def); )+
      }

      fn enter_member(
        &mut self,
        cx: &mut $crate::graphqlx::Cx<'_>,
        member: $crate::graphqlx::MemberId,
      ) {
        $( <$rule as $crate::graphqlx::Visitor>::enter_member(&mut self.$field, cx, member); )+
      }

      fn exit_member(
        &mut self,
        cx: &mut $crate::graphqlx::Cx<'_>,
        member: $crate::graphqlx::MemberId,
      ) {
        $( <$rule as $crate::graphqlx::Visitor>::exit_member(&mut self.$field, cx, member); )+
      }

      fn type_expr(
        &mut self,
        cx: &mut $crate::graphqlx::Cx<'_>,
        ty: $crate::graphqlx::TypeExprId,
      ) {
        $( <$rule as $crate::graphqlx::Visitor>::type_expr(&mut self.$field, cx, ty); )+
      }

      fn application(
        &mut self,
        cx: &mut $crate::graphqlx::Cx<'_>,
        application: $crate::graphqlx::ApplicationId,
      ) {
        $( <$rule as $crate::graphqlx::Visitor>::application(&mut self.$field, cx, application); )+
      }

      fn value(&mut self, cx: &mut $crate::graphqlx::Cx<'_>, value: $crate::graphqlx::ValueId) {
        $( <$rule as $crate::graphqlx::Visitor>::value(&mut self.$field, cx, value); )+
      }

      fn schema(&mut self, cx: &mut $crate::graphqlx::Cx<'_>) {
        $( <$rule as $crate::graphqlx::Visitor>::schema(&mut self.$field, cx); )+
      }

      fn end(&mut self, cx: &mut $crate::graphqlx::Cx<'_>) {
        $( <$rule as $crate::graphqlx::Visitor>::end(&mut self.$field, cx); )+
      }
    }
  };
}

rule_set! {
  /// smear's own rules, which `analyze` runs over every supplied package once its phases have
  /// written the model: a program defines its schema at most once; only the root package defines
  /// or extends the schema; a package extends its own definitions alone, but for the directives
  /// the root package adds to a built-in, none of which may give it a built-in meaning; an enum
  /// takes no type parameter; a marker has no field and is named only in `implements` and in
  /// bounds, and a scalar, an enum, an input object or a union implements markers alone; a set has
  /// one type parameter and a map two; `TypeRef` stands only in a directive's arguments;
  /// `NonZero`'s argument is an integer primitive at least one bit wide, written without `!`; and
  /// a definition's type parameters have distinct names, and so do the names an extension's own
  /// list gives them.
  pub(crate) struct Rules {
    duplicate_schema_definition: DuplicateSchemaDefinition
      => [super::code::schema::DUPLICATE_SCHEMA_DEFINITION],
    schema_outside_root: SchemaOutsideRoot => [super::DiagnosticKind::SchemaOutsideRoot.code()],
    orphan_extension: OrphanExtension => [super::DiagnosticKind::OrphanExtension.code()],
    generic_enum: GenericEnum => [super::DiagnosticKind::GenericEnum.code()],
    marker_misuse: MarkerMisuse => [super::DiagnosticKind::MarkerMisuse.code()],
    collection_arity: CollectionArity => [super::DiagnosticKind::CollectionArity.code()],
    type_ref_misuse: TypeRefMisuse => [super::DiagnosticKind::TypeRefMisuse.code()],
    non_zero_argument: NonZeroArgument => [super::DiagnosticKind::NonZeroArgument.code()],
    duplicate_type_parameter: DuplicateTypeParameter
      => [super::DiagnosticKind::DuplicateTypeParameter.code()],
  }
}

#[cfg(test)]
mod tests;
