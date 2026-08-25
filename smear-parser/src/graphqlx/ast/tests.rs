use std::vec::Vec;

use tokora::{
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{
  Argument, BooleanValue, DefinitionName, DefinitionTypeGenerics, DefinitionTypeParam,
  ExecutableDefinitionName, ExecutableDefinitionTypeGenerics, ExtensionName, ExtensionTypeGenerics,
  ExtensionTypeParam, Field, ImportMember, InputValue, Name, NamedSpecifier, OperationType, Path,
  Selection, TypePath, WhereClause, WherePredicate,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct CustomSpan(u8);

fn name(span: u8, source: &'static str) -> Name<&'static str, CustomSpan> {
  Name::new(CustomSpan(span), source)
}

fn path(span: u8, source: &'static str) -> Path<&'static str, CustomSpan> {
  Path::new(CustomSpan(span), Vec::from([name(span, source)]), false)
}

#[test]
fn generic_definition_and_where_aliases_preserve_custom_spans() {
  let parameter: DefinitionTypeParam<&str, CustomSpan> =
    DefinitionTypeParam::new(CustomSpan(1), name(1, "T"), None);
  let generics: DefinitionTypeGenerics<&str, CustomSpan> =
    DefinitionTypeGenerics::new(CustomSpan(2), Vec::from([parameter]));
  let definition: DefinitionName<&str, CustomSpan> =
    DefinitionName::new(CustomSpan(3), name(3, "Result"), Some(generics));
  assert_eq!(definition.as_span(), &CustomSpan(3));
  assert_eq!(definition.name().source(), &"Result");
  assert_eq!(
    definition.generics().unwrap().params()[0].name().source(),
    &"T"
  );

  let extension_parameter: ExtensionTypeParam<&str, CustomSpan> =
    ExtensionTypeParam::new(CustomSpan(4), name(4, "String"));
  let extension_generics: ExtensionTypeGenerics<&str, CustomSpan> =
    ExtensionTypeGenerics::new(CustomSpan(5), Vec::from([extension_parameter]));
  let extension: ExtensionName<&str, CustomSpan> =
    ExtensionName::new(CustomSpan(6), path(6, "Box"), Some(extension_generics));
  assert_eq!(extension.path().segments()[0].source(), &"Box");
  assert_eq!(
    extension.generics().unwrap().params()[0].name().source(),
    &"String"
  );

  let bounded: TypePath<&str, CustomSpan> = TypePath::new(CustomSpan(7), path(7, "T"), None);
  let bound: TypePath<&str, CustomSpan> = TypePath::new(CustomSpan(8), path(8, "Node"), None);
  let predicate: WherePredicate<&str, CustomSpan> =
    WherePredicate::new(CustomSpan(9), bounded, Vec::from([bound]));
  let clause: WhereClause<&str, CustomSpan> =
    WhereClause::new(CustomSpan(10), Vec::from([predicate]));
  assert_eq!(clause.as_span(), &CustomSpan(10));
  assert_eq!(
    clause.predicates()[0].bounded_type().path().segments()[0].source(),
    &"T"
  );
}

#[test]
fn executable_selection_and_import_nodes_keep_their_span_type() {
  let value: InputValue<&str, CustomSpan> =
    InputValue::Boolean(BooleanValue::new(CustomSpan(1), true));
  let argument = Argument::new(CustomSpan(2), name(2, "enabled"), value);
  assert_eq!(argument.as_span(), &CustomSpan(2));
  assert_eq!(argument.value().as_span(), &CustomSpan(1));

  let field = Field::new(CustomSpan(3), None, name(3, "viewer"), None, None, None);
  let selection: Selection<&str, CustomSpan> = Selection::Field(field);
  assert_eq!(selection.into_span(), CustomSpan(3));

  let executable_generics: ExecutableDefinitionTypeGenerics<&str, CustomSpan> =
    ExecutableDefinitionTypeGenerics::new(CustomSpan(4), Vec::from([name(4, "T")]));
  let executable_name: ExecutableDefinitionName<&str, CustomSpan> = ExecutableDefinitionName::new(
    CustomSpan(5),
    name(5, "UserFields"),
    Some(executable_generics),
  );
  assert_eq!(
    executable_name.generics().unwrap().params()[0].source(),
    &"T"
  );

  let specifier = NamedSpecifier::new(CustomSpan(6), name(6, "User"), None);
  let import = ImportMember::Named(specifier.clone());
  assert_eq!(import.as_span(), &CustomSpan(6));
  assert_eq!(
    specifier.into_components(),
    (CustomSpan(6), name(6, "User"), None)
  );

  let operation = OperationType::Query(CustomSpan(7));
  assert_eq!(operation.as_str(), "query");
  assert_eq!(operation.into_span(), CustomSpan(7));
}

/// How deep the two release gates below build, which is an order of magnitude past where the
/// generated glue died on [`RELEASE_STACK`].
///
/// Measured at `9f584d6` on `aarch64-apple-darwin`, release, on a 256 KiB thread: a type chain
/// aborted at 8 602 levels. The stack is sized here rather than inherited because the boundary is
/// a property of it — the same binary on an 8 MiB main thread reached 250 937, so a fixture sized
/// against libtest's thread would stop being a probe the day the runner's stack changed.
#[cfg(feature = "std")]
const RELEASE_DEPTH: usize = 100_000;

/// The stack the two release gates below are built and released on.
#[cfg(feature = "std")]
const RELEASE_STACK: usize = 256 * 1024;

/// Builds and releases a fixture on a stack this file sizes.
///
/// A regression aborts the process, which no assertion can soften; the thread is what makes the
/// abort reachable at a depth these fixtures can afford, not what contains it.
#[cfg(feature = "std")]
fn on_a_small_stack(fixture: impl FnOnce() + Send + 'static) {
  ::std::thread::Builder::new()
    .stack_size(RELEASE_STACK)
    .spawn(fixture)
    .expect("a fixture thread")
    .join()
    .expect("the fixture thread returned");
}

/// Releasing a set type 100 000 deep, which the generated `Drop` glue aborted the process over.
///
/// This gate and the map one below live here rather than in `smear/tests/ast_release.rs` — where
/// the rest of the family's are — because [`SetType`](crate::ty::SetType) and
/// [`MapType`](crate::ty::MapType) are re-exported by neither dialect, so these two arms cannot be
/// built from outside this crate at all. The list arm and the generic-argument arm can be, and are
/// pinned there.
#[cfg(feature = "std")]
#[test]
fn a_deep_set_type_is_released() {
  on_a_small_stack(|| {
    let leaf = || {
      super::Type::Path(super::DefinitionTypePath::new(
        CustomSpan(0),
        path(0, "Leaf"),
        None,
        false,
      ))
    };
    let mut ty = leaf();
    for _ in 0..RELEASE_DEPTH {
      ty = super::Type::Set(super::Nest::new(crate::ty::SetType::new(
        CustomSpan(0),
        ty,
        false,
      )));
    }
    drop(ty);
  });
}

/// A map nests through **two** slots, so this one is a tree rather than a chain: each level's key
/// is a leaf and its value carries the rest. The worklist is what makes the difference invisible.
#[cfg(feature = "std")]
#[test]
fn a_deep_map_type_is_released() {
  on_a_small_stack(|| {
    let leaf = || {
      super::Type::Path(super::DefinitionTypePath::new(
        CustomSpan(0),
        path(0, "Leaf"),
        None,
        false,
      ))
    };
    let mut ty = leaf();
    for _ in 0..RELEASE_DEPTH {
      ty = super::Type::Map(super::Nest::new(crate::ty::MapType::new(
        CustomSpan(0),
        leaf(),
        ty,
        false,
      )));
    }
    drop(ty);
  });
}
