use tokora::{
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{
  Constrained, DefinitionName, DefinitionTypeGenerics, DefinitionTypeParam,
  ExecutableDefinitionHeader, ExecutableDefinitionName, ExecutableDefinitionTypeGenerics,
  ExtensionName, ExtensionTypeGenerics, ExtensionTypeParam, TypePath, WhereClause, WherePredicate,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct CustomSpan(u8);

#[test]
fn generic_declarations_preserve_custom_spans_and_components() {
  let parameter = DefinitionTypeParam::new(CustomSpan(1), "T", Some("String"));
  assert_eq!(parameter.as_span(), &CustomSpan(1));
  assert_eq!(parameter.name(), &"T");
  assert_eq!(parameter.name(), &"T");
  assert_eq!(parameter.default(), Some(&"String"));

  let generics = DefinitionTypeGenerics::new(CustomSpan(2), [parameter.clone()]);
  assert_eq!(generics.params(), std::slice::from_ref(&parameter));
  assert_eq!(generics.params(), std::slice::from_ref(&parameter));
  assert_eq!(generics.as_span(), &CustomSpan(2));
  assert_eq!(generics.into_components(), (CustomSpan(2), [parameter]));

  let extension_parameter = ExtensionTypeParam::new(CustomSpan(3), "T");
  let extension_generics = ExtensionTypeGenerics::new(CustomSpan(4), [extension_parameter.clone()]);
  let extension = ExtensionName::new(CustomSpan(5), "Box", Some(extension_generics));
  assert_eq!(extension.path(), &"Box");
  assert_eq!(
    extension.generics().unwrap().params(),
    &[extension_parameter]
  );

  let executable_generics =
    ExecutableDefinitionTypeGenerics::<&str, CustomSpan, _>::new(CustomSpan(6), ["T"]);
  let executable_name =
    ExecutableDefinitionName::new(CustomSpan(7), "ItemFields", Some(executable_generics));
  let header = ExecutableDefinitionHeader::new(
    CustomSpan(8),
    Some(ExecutableDefinitionTypeGenerics::<&str, CustomSpan, _>::new(CustomSpan(9), ["T"])),
    executable_name,
  );
  assert_eq!(header.implementation_generics().unwrap().params(), &["T"]);
  assert_eq!(header.name().name(), &"ItemFields");

  let definition = DefinitionName::new(CustomSpan(10), "Result", None::<()>);
  assert_eq!(definition.into_span(), CustomSpan(10));
}

#[test]
fn type_paths_where_clauses_and_constraints_are_composable() {
  type Path = TypePath<&'static str, &'static str, CustomSpan>;

  let bounded = Path::new(CustomSpan(1), "T", None);
  let node = Path::new(CustomSpan(2), "Node", Some("<Id>"));
  let serializable = Path::new(CustomSpan(3), "Serializable", None);
  let predicate = WherePredicate::new(CustomSpan(4), bounded, [node, serializable]);
  assert_eq!(predicate.bounded_type().path(), &"T");
  assert_eq!(predicate.bounds().len(), 2);
  assert_eq!(predicate.bounds()[1].path(), &"Serializable");

  let clause = WhereClause::new(CustomSpan(5), [predicate.clone()]);
  assert_eq!(clause.predicates().len(), 1);
  assert_eq!(clause.as_span(), &CustomSpan(5));

  let constrained = Constrained::new(CustomSpan(6), Some(clause), "{ value }");
  assert_eq!(constrained.where_clause().unwrap().predicates().len(), 1);
  assert_eq!(constrained.target(), &"{ value }");
  assert_eq!(
    constrained.into_components(),
    (
      CustomSpan(6),
      Some(WhereClause::new(CustomSpan(5), [predicate])),
      "{ value }"
    )
  );
}
