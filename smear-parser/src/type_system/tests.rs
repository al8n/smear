use tokora::{
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{
  ArgumentsDefinition, DirectiveDefinition, DirectiveExtension, DirectiveLocations,
  ExecutableDirectiveLocation, FieldDefinition, InputValueDefinition, Location,
  ObjectTypeDefinition, TypeSystemDirectiveLocation, TypeSystemExtension,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CustomSpan(u8);

struct ArrayBacked<T, const N: usize>([T; N]);

impl<T, const N: usize> AsRef<[T]> for ArrayBacked<T, N> {
  fn as_ref(&self) -> &[T] {
    &self.0
  }
}

#[test]
fn carriers_support_custom_spans_and_components() {
  let input = InputValueDefinition::<_, _, _, _, CustomSpan>::new(
    CustomSpan(1),
    "limit",
    "Int",
    Some("10"),
    Some("@range"),
  );
  assert_eq!(input.as_span(), &CustomSpan(1));
  assert_eq!(input.name(), &"limit");
  assert_eq!(input.ty(), &"Int");
  assert_eq!(input.default_value(), Some(&"10"));
  assert_eq!(input.directives(), Some(&"@range"));
  assert_eq!(
    input.into_components(),
    (CustomSpan(1), "limit", "Int", Some("10"), Some("@range"))
  );

  let field = FieldDefinition::<_, _, _, _, CustomSpan>::new(
    CustomSpan(2),
    "users",
    Some("(limit: Int)"),
    "[User!]!",
    None::<&str>,
  );
  assert_eq!(field.arguments_definition(), Some(&"(limit: Int)"));
  assert_eq!(field.directives(), None);
  assert_eq!(field.into_span(), CustomSpan(2));

  let object = ObjectTypeDefinition::<_, _, _, _, CustomSpan>::new(
    CustomSpan(3),
    "Query",
    None::<&str>,
    Some("@root"),
    Some("{ users: [User!]! }"),
  );
  assert_eq!(object.implements(), None);
  assert_eq!(object.directives(), Some(&"@root"));
  assert_eq!(object.fields_definition(), Some(&"{ users: [User!]! }"));

  let directive = DirectiveDefinition::<_, _, _, _, CustomSpan>::new(
    CustomSpan(4),
    "deprecated",
    None::<&str>,
    Some("@experimental"),
    true,
    "FIELD_DEFINITION",
  );
  assert_eq!(directive.arguments_definition(), None);
  assert_eq!(directive.directives(), Some(&"@experimental"));
  assert!(directive.repeatable());
  assert_eq!(directive.locations(), &"FIELD_DEFINITION");
  assert_eq!(
    directive.into_components(),
    (
      CustomSpan(4),
      "deprecated",
      None,
      Some("@experimental"),
      true,
      "FIELD_DEFINITION"
    )
  );

  let extension =
    DirectiveExtension::<_, _, CustomSpan>::new(CustomSpan(5), "deprecated", "@experimental");
  assert_eq!(extension.as_span(), &CustomSpan(5));
  assert_eq!(extension.name(), &"deprecated");
  assert_eq!(extension.directives(), &"@experimental");
  assert_eq!(
    extension.into_components(),
    (CustomSpan(5), "deprecated", "@experimental")
  );

  type Extension = DirectiveExtension<&'static str, &'static str, CustomSpan>;
  let extension = TypeSystemExtension::<Extension, Extension, Extension>::Directive(
    Extension::new(CustomSpan(6), "d", "@k"),
  );
  assert!(extension.is_directive());
  assert_eq!(extension.span::<CustomSpan>(), &CustomSpan(6));
  let span: CustomSpan = extension.into_span();
  assert_eq!(span, CustomSpan(6));
}

#[test]
fn collections_project_array_backed_containers_to_slices() {
  let arguments =
    ArgumentsDefinition::<u8, _, CustomSpan>::new(CustomSpan(1), ArrayBacked([1_u8, 2]));
  let projected: &[u8] = arguments.input_value_definitions();
  assert_eq!(projected, &[1, 2]);
  assert_eq!(arguments.into_input_value_definitions().0, [1, 2]);

  let locations =
    DirectiveLocations::<u8, _, CustomSpan>::new(CustomSpan(2), ArrayBacked([3_u8, 4]));
  let projected: &[u8] = locations.locations();
  assert_eq!(projected, &[3, 4]);
  assert_eq!(locations.into_locations().0, [3, 4]);
}

#[test]
fn directive_locations_preserve_their_kind_and_span() {
  let executable = ExecutableDirectiveLocation::field(CustomSpan(1));
  assert_eq!(executable.as_str(), "FIELD");
  assert_eq!(executable.into_components(), (CustomSpan(1), "FIELD"));

  let location: Location<CustomSpan> =
    TypeSystemDirectiveLocation::input_field_definition(CustomSpan(2)).into();
  assert_eq!(location.as_span(), &CustomSpan(2));
  assert_eq!(location.as_str(), "INPUT_FIELD_DEFINITION");
}

#[test]
fn directive_location_enums_keep_variant_and_textual_ergonomics() {
  let query = ExecutableDirectiveLocation::query(CustomSpan(1));
  assert!(query.is_query());
  assert_eq!(query.unwrap_query_ref(), &CustomSpan(1));
  assert_eq!(query.try_unwrap_query_ref(), Ok(&CustomSpan(1)));
  assert_eq!(AsRef::<str>::as_ref(&query), "QUERY");

  let input_field = TypeSystemDirectiveLocation::input_field_definition(CustomSpan(2));
  assert!(input_field.is_input_field_definition());
  assert_eq!(
    input_field.unwrap_input_field_definition_ref(),
    &CustomSpan(2)
  );
  assert_eq!(AsRef::<str>::as_ref(&input_field), "INPUT_FIELD_DEFINITION");

  let directive = TypeSystemDirectiveLocation::directive_definition(CustomSpan(3));
  assert!(directive.is_directive_definition());
  assert_eq!(directive.unwrap_directive_definition_ref(), &CustomSpan(3));
  assert_eq!(AsRef::<str>::as_ref(&directive), "DIRECTIVE_DEFINITION");
  assert_eq!(
    directive.into_components(),
    (CustomSpan(3), "DIRECTIVE_DEFINITION")
  );

  let location: Location<CustomSpan> = query.into();
  assert!(location.is_executable());
  assert_eq!(location.unwrap_executable_ref().as_str(), "QUERY");
  assert!(location.try_unwrap_type_system_ref().is_err());
  assert_eq!(AsRef::<str>::as_ref(&location), "QUERY");
}
