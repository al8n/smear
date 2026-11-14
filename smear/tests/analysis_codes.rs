//! The `smear::schema` codes the GraphQLx analyzer shares with GraphQL's own validator, the
//! registry `graphqlx::schema`: each public constant is `smear-schema`'s code of the kind it names,
//! and `schema::ALL` lists each once, in declaration order, as a kebab-case code of the family;
//! every shared code an analysis of the schema corpus reports, and every one a rule of smear's
//! lists, has its constant; and every constant is reported on a fixture of that corpus or listed by
//! a rule. Together, every shared code the analysis reports is public and in `ALL`, and nothing
//! else is.
//!
//! The registry golden holds no row for a constant: each constant IS a `SchemaErrorKind`'s code,
//! whose row the golden has, and two rows with one code are what `diagnostic_codes.rs` refuses.
#![cfg(all(
  any(feature = "std", feature = "alloc"),
  feature = "analysis",
  feature = "validator"
))]

use smear::{
  analysis::graphqlx::{
    Analysis, Layout, Limits, Package, SourceFile, Tier, analyze, schema, test_support::RULES,
  },
  diagnostic::{Code, Diagnose},
  validator::SchemaErrorKind,
};

// The draft §3 corpus: an SDL for each kind of `smear-schema`'s that a parsed document reaches.
// The module also carries what this file does not read, which would be a `dead_code` denial under
// CI's `-Dwarnings`; allowed at the include, as its other readers do.
#[allow(dead_code)]
#[path = "support/validator_corpus.rs"]
mod corpus;

use corpus::SCHEMA_FIXTURES;

/// The analysis of the SDL `sdl`, a fixture of the draft §3 corpus, as the one file of a `Flat`
/// root package.
fn analyzed(sdl: &str) -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", sdl)]);
  analyze(&root, &[], &Limits::new())
}

/// Each shared code, with the kind of `smear-schema`'s it is, in the module's declaration order.
const SHARED: &[(Code, SchemaErrorKind)] = &[
  (
    schema::DUPLICATE_TYPE_NAME,
    SchemaErrorKind::DuplicateTypeName,
  ),
  (
    schema::DUPLICATE_DIRECTIVE_DEFINITION,
    SchemaErrorKind::DuplicateDirectiveDefinition,
  ),
  (
    schema::RESERVED_TYPE_NAME,
    SchemaErrorKind::ReservedTypeName,
  ),
  (
    schema::REDEFINED_BUILT_IN_TYPE,
    SchemaErrorKind::RedefinedBuiltInType,
  ),
  (
    schema::RESERVED_DIRECTIVE_NAME,
    SchemaErrorKind::ReservedDirectiveName,
  ),
  (schema::UNDEFINED_TYPE, SchemaErrorKind::UndefinedType),
  (
    schema::UNDEFINED_DIRECTIVE,
    SchemaErrorKind::UndefinedDirective,
  ),
  (
    schema::UNDEFINED_EXTENSION_TARGET,
    SchemaErrorKind::UndefinedExtensionTarget,
  ),
  (
    schema::UNDEFINED_IMPLEMENTS_INTERFACE,
    SchemaErrorKind::UndefinedImplementsInterface,
  ),
  (
    schema::UNDEFINED_UNION_MEMBER,
    SchemaErrorKind::UndefinedUnionMember,
  ),
  (
    schema::UNDEFINED_ROOT_OPERATION_TYPE,
    SchemaErrorKind::UndefinedRootOperationType,
  ),
  (
    schema::EXTENSION_KIND_MISMATCH,
    SchemaErrorKind::ExtensionKindMismatch,
  ),
  (
    schema::UNDEFINED_DIRECTIVE_ARGUMENT,
    SchemaErrorKind::UndefinedDirectiveArgument,
  ),
  (
    schema::DUPLICATE_ROOT_OPERATION_TYPE,
    SchemaErrorKind::DuplicateRootOperationType,
  ),
  (
    schema::DUPLICATE_SCHEMA_DEFINITION,
    SchemaErrorKind::DuplicateSchemaDefinition,
  ),
  (
    schema::INTROSPECTION_TYPE_EXTENSION,
    SchemaErrorKind::IntrospectionTypeExtension,
  ),
  (
    schema::RESERVED_FIELD_NAME,
    SchemaErrorKind::ReservedFieldName,
  ),
  (
    schema::RESERVED_ARGUMENT_NAME,
    SchemaErrorKind::ReservedArgumentName,
  ),
  (
    schema::RESERVED_ENUM_VALUE_NAME,
    SchemaErrorKind::ReservedEnumValueName,
  ),
  (
    schema::RESERVED_INPUT_FIELD_NAME,
    SchemaErrorKind::ReservedInputFieldName,
  ),
  (
    schema::RESERVED_DIRECTIVE_ARGUMENT_NAME,
    SchemaErrorKind::ReservedDirectiveArgumentName,
  ),
  (
    schema::DUPLICATE_FIELD_NAME,
    SchemaErrorKind::DuplicateFieldName,
  ),
  (
    schema::DUPLICATE_ARGUMENT_NAME,
    SchemaErrorKind::DuplicateArgumentName,
  ),
  (
    schema::DUPLICATE_ENUM_VALUE,
    SchemaErrorKind::DuplicateEnumValue,
  ),
  (
    schema::DUPLICATE_INPUT_FIELD_NAME,
    SchemaErrorKind::DuplicateInputFieldName,
  ),
  (
    schema::DUPLICATE_DIRECTIVE_ARGUMENT_NAME,
    SchemaErrorKind::DuplicateDirectiveArgumentName,
  ),
  (
    schema::EMPTY_FIELDS_DEFINITION,
    SchemaErrorKind::EmptyFieldsDefinition,
  ),
  (
    schema::EMPTY_UNION_MEMBERS,
    SchemaErrorKind::EmptyUnionMembers,
  ),
  (schema::EMPTY_ENUM_VALUES, SchemaErrorKind::EmptyEnumValues),
  (
    schema::EMPTY_INPUT_FIELDS,
    SchemaErrorKind::EmptyInputFields,
  ),
  (
    schema::FIELD_TYPE_NOT_OUTPUT_TYPE,
    SchemaErrorKind::FieldTypeNotOutputType,
  ),
  (
    schema::ARGUMENT_TYPE_NOT_INPUT_TYPE,
    SchemaErrorKind::ArgumentTypeNotInputType,
  ),
  (
    schema::INPUT_FIELD_TYPE_NOT_INPUT_TYPE,
    SchemaErrorKind::InputFieldTypeNotInputType,
  ),
  (
    schema::DIRECTIVE_ARGUMENT_TYPE_NOT_INPUT_TYPE,
    SchemaErrorKind::DirectiveArgumentTypeNotInputType,
  ),
  (
    schema::IMPLEMENTS_NON_INTERFACE,
    SchemaErrorKind::ImplementsNonInterface,
  ),
  (
    schema::SELF_IMPLEMENTING_INTERFACE,
    SchemaErrorKind::SelfImplementingInterface,
  ),
  (
    schema::UNION_MEMBER_NOT_OBJECT,
    SchemaErrorKind::UnionMemberNotObject,
  ),
  (
    schema::DEPRECATED_REQUIRED_ARGUMENT,
    SchemaErrorKind::DeprecatedRequiredArgument,
  ),
  (
    schema::DEPRECATED_REQUIRED_INPUT_FIELD,
    SchemaErrorKind::DeprecatedRequiredInputField,
  ),
  (
    schema::ONE_OF_FIELD_NOT_NULLABLE,
    SchemaErrorKind::OneOfFieldNotNullable,
  ),
  (
    schema::ONE_OF_FIELD_HAS_DEFAULT,
    SchemaErrorKind::OneOfFieldHasDefault,
  ),
  (
    schema::ONE_OF_ON_INPUT_OBJECT_EXTENSION,
    SchemaErrorKind::OneOfOnInputObjectExtension,
  ),
  (
    schema::UNSUPPORTED_DIRECTIVE_LOCATION,
    SchemaErrorKind::UnsupportedDirectiveLocation,
  ),
  (
    schema::DUPLICATE_DIRECTIVE_USE,
    SchemaErrorKind::DuplicateDirectiveUse,
  ),
  (
    schema::DUPLICATE_DIRECTIVE_ARGUMENT_USE,
    SchemaErrorKind::DuplicateDirectiveArgumentUse,
  ),
  (
    schema::SELF_REFERENTIAL_DIRECTIVE,
    SchemaErrorKind::SelfReferentialDirective,
  ),
  (
    schema::SPECIFIED_BY_ON_BUILT_IN_SCALAR,
    SchemaErrorKind::SpecifiedByOnBuiltInScalar,
  ),
  (
    schema::ROOT_OPERATION_TYPE_NOT_OBJECT,
    SchemaErrorKind::RootOperationTypeNotObject,
  ),
  (
    schema::SHARED_ROOT_OPERATION_TYPE,
    SchemaErrorKind::SharedRootOperationType,
  ),
  (
    schema::SCHEMA_EXTENSION_WITHOUT_SCHEMA,
    SchemaErrorKind::SchemaExtensionWithoutSchema,
  ),
  (
    schema::MISSING_QUERY_ROOT_OPERATION_TYPE,
    SchemaErrorKind::MissingQueryRootOperationType,
  ),
  (
    schema::INVALID_DEFAULT_VALUE,
    SchemaErrorKind::InvalidDefaultValue,
  ),
  (
    schema::INVALID_DIRECTIVE_ARGUMENT_VALUE,
    SchemaErrorKind::InvalidDirectiveArgumentValue,
  ),
  (
    schema::MISSING_REQUIRED_DIRECTIVE_ARGUMENT,
    SchemaErrorKind::MissingRequiredDirectiveArgument,
  ),
  (
    schema::UNDEFINED_INPUT_OBJECT_FIELD,
    SchemaErrorKind::UndefinedInputObjectField,
  ),
  (
    schema::MISSING_REQUIRED_INPUT_OBJECT_FIELD,
    SchemaErrorKind::MissingRequiredInputObjectField,
  ),
  (
    schema::DUPLICATE_IMPLEMENTS_INTERFACE,
    SchemaErrorKind::DuplicateImplementsInterface,
  ),
  (
    schema::MISSING_TRANSITIVE_INTERFACE,
    SchemaErrorKind::MissingTransitiveInterface,
  ),
  (
    schema::MISSING_INTERFACE_FIELD,
    SchemaErrorKind::MissingInterfaceField,
  ),
  (
    schema::INVALID_INTERFACE_FIELD_TYPE,
    SchemaErrorKind::InvalidInterfaceFieldType,
  ),
  (
    schema::MISSING_INTERFACE_FIELD_ARGUMENT,
    SchemaErrorKind::MissingInterfaceFieldArgument,
  ),
  (
    schema::INVALID_INTERFACE_FIELD_ARGUMENT_TYPE,
    SchemaErrorKind::InvalidInterfaceFieldArgumentType,
  ),
  (
    schema::UNEXPECTED_REQUIRED_ARGUMENT,
    SchemaErrorKind::UnexpectedRequiredArgument,
  ),
  (
    schema::INTERFACE_FIELD_NOT_DEPRECATED,
    SchemaErrorKind::InterfaceFieldNotDeprecated,
  ),
  (
    schema::DUPLICATE_UNION_MEMBER,
    SchemaErrorKind::DuplicateUnionMember,
  ),
];

/// Each constant is the code `smear-schema` reports for the kind it names, and `schema::ALL` holds
/// them all, in the module's order: a constant whose string drifted, and an `ALL` that leaves one
/// out or reorders them, each fail here.
#[test]
fn each_shared_code_is_smear_schemas() {
  assert_eq!(
    schema::ALL.len(),
    SHARED.len(),
    "`schema::ALL` holds {} codes, and the module {}",
    schema::ALL.len(),
    SHARED.len()
  );
  for ((constant, kind), listed) in SHARED.iter().zip(schema::ALL) {
    assert_eq!(
      listed, constant,
      "`schema::ALL` lists `{listed}` where the module declares `{constant}`"
    );
    assert_eq!(
      *constant,
      kind.code(),
      "the constant for `{kind:?}` is not `smear-schema`'s code"
    );
  }
}

/// No shared code is listed twice, and each is `smear::schema::` and a kebab-case rule.
#[test]
fn each_shared_code_is_listed_once_and_well_formed() {
  let mut codes: Vec<&str> = schema::ALL
    .iter()
    .map(|code: &Code| code.as_str())
    .collect();
  for code in &codes {
    let rule = code
      .strip_prefix("smear::schema::")
      .unwrap_or_else(|| panic!("{code} is not in the `smear::schema` family"));
    assert!(
      !rule.is_empty()
        && !rule.starts_with('-')
        && !rule.ends_with('-')
        && !rule.contains("--")
        && rule
          .bytes()
          .all(|byte: u8| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'-'),
      "{code} is not kebab-case"
    );
  }
  let count = codes.len();
  codes.sort_unstable();
  codes.dedup();
  assert_eq!(codes.len(), count, "`schema::ALL` lists a code twice");
}

/// Each SDL of the draft §3 corpus, analysed as the one file of a `Flat` root package: every
/// `smear::schema` code the analysis reports is in `schema::ALL`, so a shared code reported under a
/// string no constant names fails here. At least one row reports one, so that the test is not
/// empty.
#[test]
fn every_shared_code_an_analysis_reports_has_its_constant() {
  let mut shared = 0usize;
  for (kind, sdl, _) in SCHEMA_FIXTURES {
    let analysis = analyzed(sdl);
    for diagnostic in analysis.diagnostics() {
      let code = diagnostic.code();
      if !code.as_str().starts_with("smear::schema::") {
        continue;
      }
      shared += 1;
      assert!(
        schema::ALL.contains(&code),
        "the fixture of `{kind:?}` reports `{code}`, which no constant of `graphqlx::schema` names"
      );
    }
  }
  assert!(
    shared > 0,
    "no fixture of the corpus made the analysis report a shared code"
  );
}

/// Each `smear::schema` code a rule of smear's lists is in `schema::ALL`: a rule's constant left out
/// of the registry fails here, whatever the fixtures fire.
#[test]
fn every_shared_code_a_rule_lists_is_registered() {
  let mut shared = 0usize;
  for (rule, codes) in RULES {
    for code in *codes {
      if !code.as_str().starts_with("smear::schema::") {
        continue;
      }
      shared += 1;
      assert!(
        schema::ALL.contains(code),
        "`{rule}` lists `{code}`, which `schema::ALL` does not hold"
      );
    }
  }
  assert!(shared > 0, "no rule of smear's lists a shared code");
}

/// Each code of `schema::ALL` is reported by the analysis of a fixture of the draft §3 corpus, or
/// listed by a rule of smear's: a constant that names a code nothing reports fails here. Each
/// constant's kind has its fixture there, so each is reported on its own, and a rule's list is the
/// fallback.
#[test]
fn every_registered_code_is_reported_or_listed() {
  let mut seen: Vec<Code> = Vec::new();
  for (_, codes) in RULES {
    seen.extend_from_slice(codes);
  }
  for (_, sdl, _) in SCHEMA_FIXTURES {
    for diagnostic in analyzed(sdl).diagnostics() {
      seen.push(diagnostic.code());
    }
  }
  for code in schema::ALL {
    assert!(
      seen.contains(code),
      "`schema::ALL` holds `{code}`, which no fixture's analysis reports and no rule lists"
    );
  }
}
