//! smear's own rules: what `analyze` runs, after its phases, through the one driver.
//!
//! A rule is one type in one file, implementing the public `Visitor` and nothing else, and the set
//! is a named struct of them that `rule_set!` declares. The rule types are private. The macro also
//! emits the set's table of each rule and the codes it reports, which the liveness test reads:
//! every code a rule lists must fire on at least one fixture.
//!
//! A rule reads the model the phases wrote, through its public views, and reports through the
//! context. Which package wrote a contribution is read off where the contribution is written
//! (`written_by`). A rule says nothing about a contribution a phase already refused something in,
//! whose `has_errors()` is set: a diagnostic of its own within that contribution would repeat the
//! phase's. It sees the phases' marks alone, since the rules' are applied once the walk has ended.

mod duplicate_schema_definition;
mod orphan_extension;
mod schema_outside_root;

use tokora::diagnostic::Location;

use self::{
  duplicate_schema_definition::DuplicateSchemaDefinition, orphan_extension::OrphanExtension,
  schema_outside_root::SchemaOutsideRoot,
};
use super::{
  id::{PackageId, SourceId},
  source::SourceMap,
};

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
/// `Default`, so `Rules::default()` is the set ready to run. A set may hold no rule, and its every
/// hook then does nothing. The table, `TABLE`, lists each rule by its type's name, in field order,
/// and is compiled where something reads it: this crate's unit tests and the test-support door.
macro_rules! rule_set {
  (
    $(#[$meta:meta])*
    $vis:vis struct $name:ident {}
  ) => {
    $(#[$meta])*
    #[derive(Default)]
    $vis struct $name {}

    #[cfg(any(test, feature = "test-support"))]
    impl $name {
      /// The rules of the set, each with the codes it reports: none.
      $vis const TABLE: &'static [(&'static str, &'static [::tokora::diagnostic::Code])] = &[];
    }

    /// The set holds no rule, so every hook does nothing.
    impl $crate::graphqlx::Visitor for $name {}
  };
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
  /// or extends the schema; and a package extends its own definitions alone, but for the
  /// directives the root package adds to a built-in.
  pub(crate) struct Rules {
    duplicate_schema_definition: DuplicateSchemaDefinition
      => [super::code::schema::DUPLICATE_SCHEMA_DEFINITION],
    schema_outside_root: SchemaOutsideRoot => [super::DiagnosticKind::SchemaOutsideRoot.code()],
    orphan_extension: OrphanExtension => [super::DiagnosticKind::OrphanExtension.code()],
  }
}

/// The package that wrote what is written at `at`: the package of the source `at` is in. For a
/// contribution's location that is the package that wrote the contribution, which for an
/// extension, and for a definition that replaces a built-in, need not be its definition's.
///
/// `None` when `at`'s source is none of `sources`', as a location of a model built by hand may be:
/// a rule then reports nothing about what is written there. A package this answers is one of the
/// model's, since an analysis's source map numbers the sources of its model's packages.
fn written_by(sources: &SourceMap, at: Location) -> Option<PackageId> {
  let source = SourceId::new(at.source());
  if sources.sources().contains(source) {
    Some(sources.source(source).package())
  } else {
    None
  }
}

#[cfg(test)]
mod tests;
