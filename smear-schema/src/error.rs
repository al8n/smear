//! What [`Schema::build`] refuses, and how it says so.
//!
//! Draft §3 "Type Validation" runs inside the build, so a server rejects a bad SDL exactly once
//! rather than rediscovering it per request. This module is the vocabulary of that refusal:
//! [`SchemaErrorKind`] enumerates every class the build rejects, and [`SchemaError`] names the
//! artifact it rejected and points at it.
//!
//! The build path is explicitly not performance-critical — it runs once — so these errors own
//! their strings. That is what keeps [`Schema`](super::Schema)'s promise honest in the failure
//! direction too: a `SchemaErrors` outlives the SDL it came from and can cross a thread or an FFI
//! boundary with nothing borrowed.

use std::{boxed::Box, string::String, vec::Vec};

use tokora::SimpleSpan;

use crate::diagnostic::{Code, Diagnose, Label, Location, PathSegment, Severity};

/// Which of draft §3's rules a [`SchemaError`] reports.
///
/// Each variant carries the section it comes from in its documentation. The set is closed by the
/// specification, not by taste: `SchemaErrorKind::ALL` enumerates it, and the build fixtures pair
/// every variant with a schema that makes it fire.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum SchemaErrorKind {
  // -- §3.3 The schema, and document-level uniqueness ---------------------------------------
  /// Two definitions give the same name to a type (draft §3.3).
  DuplicateTypeName,
  /// Two definitions give the same name to a directive (draft §3.3).
  DuplicateDirectiveDefinition,
  /// A document defines `schema` more than once (draft §3.3).
  DuplicateSchemaDefinition,
  /// A `schema` block names the same root operation twice (draft §3.3).
  DuplicateRootOperationType,
  /// A root operation names a type the document does not define (draft §3.3).
  UndefinedRootOperationType,
  /// A root operation names a type that is not an object type (draft §3.3).
  ///
  /// All three roots are held to it, not only `query`: the draft says the `mutation` and
  /// `subscription` roots "must be an Object type" in the same breath.
  RootOperationTypeNotObject,
  /// Two root operations name the same type (draft §3.3).
  ///
  /// "The `query`, `mutation`, and `subscription` root types must all be different types if
  /// provided." A shared root is not a naming slip — the three roots are the entry points of three
  /// different operation kinds, and a type that is two of them has no consistent field set.
  SharedRootOperationType,
  /// The schema provides no query root operation type (draft §3.3).
  MissingQueryRootOperationType,
  /// `extend` names a type the document does not define (draft §3.3).
  UndefinedExtensionTarget,
  /// `extend` names a type of a different kind than the extension's keyword (draft §3.3).
  ExtensionKindMismatch,
  /// A definition redefines an introspection type (draft §3.3).
  ///
  /// The five built-in scalars and the five specified directives are *not* in this class: a
  /// document that spells one out replaces it, which is what makes a printed schema re-readable.
  /// The introspection types are `__`-prefixed and injected unconditionally, so a definition of
  /// one is a genuine collision.
  RedefinedBuiltInType,
  /// A user-defined type name begins with `__` (draft §3.3, "Reserved Names").
  ReservedTypeName,
  /// A type reference names a type the document does not define (draft §3.4).
  UndefinedType,
  /// A type reference nests more list and non-null wrappers than the representation admits.
  ///
  /// Not a specification rule: [`MAX_WRAPPERS`](super::MAX_WRAPPERS) is this implementation's
  /// packing limit, and refusing is the only alternative to silently truncating a type.
  TypeReferenceTooDeep,
  /// A name in the document is not spelled `/[_A-Za-z][_0-9A-Za-z]*/`.
  ///
  /// Unreachable from parsed input — the lexer's identifier rule is the same grammar — and
  /// checked anyway, because the AST types are public and a caller may assemble one by hand.
  InvalidName,
  /// The document interns more distinct names than the index can address.
  ///
  /// A capacity limit of this implementation ([`MAX_SYMBOLS`](super::MAX_SYMBOLS)), not a
  /// specification rule.
  TooManyNames,

  // -- §3.6 Objects, §3.7 Interfaces --------------------------------------------------------
  /// An object or interface type defines no fields (draft §3.6.1, §3.7.1).
  EmptyFieldsDefinition,
  /// A type defines the same field name twice (draft §3.6.1, §3.7.1).
  DuplicateFieldName,
  /// A field name begins with `__` (draft §3.6.1, §3.7.1).
  ReservedFieldName,
  /// A field's result type is an input type (draft §3.6.1, §3.7.1).
  FieldTypeNotOutputType,
  /// A field declares the same argument name twice (draft §3.6.1, §3.7.1).
  DuplicateArgumentName,
  /// An argument name begins with `__` (draft §3.6.1, §3.7.1).
  ReservedArgumentName,
  /// An argument's type is an output type (draft §3.6.1, §3.7.1).
  ArgumentTypeNotInputType,
  /// A field declares more arguments than
  /// [`MAX_FIELD_ARGUMENTS`](super::MAX_FIELD_ARGUMENTS).
  ///
  /// A capacity limit of this implementation, not a specification rule — and unlike
  /// [`TooManyNames`](Self::TooManyNames) or
  /// [`TypeReferenceTooDeep`](Self::TypeReferenceTooDeep) it is not about what the representation
  /// can address. Draft §6.4.1 `CoerceArgumentValues` iterates every declared argument at every
  /// runtime position of the field, and an executor can meter the positions but cannot charge the
  /// declared count without billing a request for the service's own design-time width. The
  /// constant's own documentation has the measurement and the rewrite a wider field takes: one
  /// argument of an input object type, whose fields no executor iterates per position.
  ///
  /// Raised for interface fields as well as object fields. Draft §3.7 makes an interface field's
  /// argument list a lower bound on every implementing field's, so an interface field past the
  /// ceiling is one no object type could implement without being refused itself.
  TooManyFieldArguments,
  /// `@deprecated` is applied to a required argument (draft §3.6.1 2.4.4.1, §3.7.1).
  ///
  /// Required means non-null with no default, and §3.13's own text for the directive says why:
  /// "to deprecate a required argument … it must first be made optional". A caller has no way to
  /// stop passing an argument it is obliged to pass, so the deprecation would be advice nobody can
  /// act on.
  ///
  /// A directive definition's arguments are held to it as well. §3.13.1's numbered list does not
  /// repeat the clause, but the `@deprecated` directive's own prose is not scoped to fields — it
  /// says "required (non-null without a default) arguments" — and the position is the same
  /// `InputValueDefinition`.
  DeprecatedRequiredArgument,
  /// An input value's default does not fit the type declared for it (draft §3.6.1 2.4.5, §3.7.1).
  ///
  /// The draft numbers the rule for a *field argument*: "if the argument has a default value it
  /// must be compatible with `argumentType` as per the coercion rules for that type". The same
  /// coercion is applied at the two other positions an `InputValueDefinition` appears — an input
  /// object's field and a directive definition's argument — because a default that cannot be
  /// coerced is unusable wherever it is written, and one procedure answering differently by
  /// position would be the defect this shares its coercion table to avoid.
  InvalidDefaultValue,
  /// `implements` names a type that is not an interface (draft §3.6.1, §3.7.1).
  ImplementsNonInterface,
  /// `implements` names a type the document does not define (draft §3.6.1, §3.7.1).
  UndefinedImplementsInterface,
  /// `implements` names the same interface twice (draft §3.6.1, §3.7.1).
  DuplicateImplementsInterface,
  /// An interface implements itself (draft §3.7.1).
  SelfImplementingInterface,
  /// A type implements an interface without also declaring what that interface implements
  /// (draft §3.6.1, §3.7.1).
  MissingTransitiveInterface,
  /// A type implements an interface but omits one of its fields (draft §3.6.1, §3.7.1).
  MissingInterfaceField,
  /// A field's type is not a valid implementation of the interface field's type — the covariance
  /// rule (draft §3.6.1, §3.7.1).
  InvalidInterfaceFieldType,
  /// A field omits an argument the interface field declares (draft §3.6.1, §3.7.1).
  MissingInterfaceFieldArgument,
  /// A field's argument type differs from the interface field's — arguments are invariant, not
  /// contravariant (draft §3.6.1, §3.7.1).
  InvalidInterfaceFieldArgumentType,
  /// A field adds a required argument the interface field does not declare (draft §3.6.1,
  /// §3.7.1).
  UnexpectedRequiredArgument,
  /// A field is deprecated and the interface field it implements is not
  /// (`IsValidImplementation` 2.6).
  ///
  /// The obligation runs one way only. An interface may deprecate a field its implementors have
  /// not, because a client reading the interface is told the field is going away everywhere; the
  /// reverse would tell a client reading the interface that a field is supported when one
  /// implementation has already withdrawn it.
  InterfaceFieldNotDeprecated,

  // -- §3.8 Unions --------------------------------------------------------------------------
  /// A union type declares no members (draft §3.8.1).
  EmptyUnionMembers,
  /// A union member is not an object type (draft §3.8.1).
  UnionMemberNotObject,
  /// A union names a type the document does not define (draft §3.8.1).
  UndefinedUnionMember,
  /// A union names the same member twice (draft §3.8.1).
  DuplicateUnionMember,

  // -- §3.9 Enums ---------------------------------------------------------------------------
  /// An enum type defines no values (draft §3.9.1).
  EmptyEnumValues,
  /// An enum defines the same value twice (draft §3.9.1).
  DuplicateEnumValue,
  /// An enum value name begins with `__` (draft §3.3, "Reserved Names").
  ReservedEnumValueName,

  // -- §3.10 Input objects ------------------------------------------------------------------
  /// An input object type defines no fields (draft §3.10.1).
  EmptyInputFields,
  /// An input object defines the same field name twice (draft §3.10.1).
  DuplicateInputFieldName,
  /// An input field name begins with `__` (draft §3.10.1).
  ReservedInputFieldName,
  /// An input field's type is an output type (draft §3.10.1).
  InputFieldTypeNotInputType,
  /// `@deprecated` is applied to a required input field (draft §3.10.1 2.4.1).
  ///
  /// The input-field half of [`DeprecatedRequiredArgument`](Self::DeprecatedRequiredArgument),
  /// with the same reasoning and its own kind because the two name different artifacts: `T.f.a` is
  /// an argument and `In.a` is an input field.
  DeprecatedRequiredInputField,
  /// A field of a `@oneOf` input object is non-null (draft §3.10.1).
  OneOfFieldNotNullable,
  /// A field of a `@oneOf` input object declares a default value (draft §3.10.1).
  OneOfFieldHasDefault,
  /// Input objects form a cycle in which every link is non-null, so no finite value satisfies it
  /// (draft §3.10.1).
  CircularNonNullInputField,
  /// An input object's default values refer to one another in a cycle, so coercing the outer
  /// default would never terminate (draft §3.10.1(4), `InputObjectDefaultValueHasCycle`).
  ///
  /// Distinct from [`CircularNonNullInputField`](Self::CircularNonNullInputField), which is about
  /// the *types*: `input A { b: B = {} } input B { a: A = {} }` has a perfectly finite type graph —
  /// every link is nullable — and it is the *defaults* that never bottom out, because coercing
  /// `{}` for `A` needs `A.b`'s default, which needs `B.a`'s, which needs `A.b`'s.
  InputObjectDefaultValueCycle,
  /// `@oneOf` is provided by an input object type *extension* (draft §3.10.3(5)).
  ///
  /// Being a OneOf Input Object changes what every *existing* field of the type means — each one
  /// becomes one of the alternatives, and every one of them has to be nullable and undefaulted. An
  /// extension may not impose that retroactively, so the directive belongs on the definition or
  /// nowhere.
  ///
  /// The type is still treated as OneOf for the rest of the build, so the fields an extension
  /// added are checked against the constraint too (§3.10.3(6)) rather than going unexamined behind
  /// this refusal.
  OneOfOnInputObjectExtension,

  // -- §3.13 Directives ---------------------------------------------------------------------
  /// A directive name begins with `__` (draft §3.13.1).
  ReservedDirectiveName,
  /// A directive declares the same argument name twice (draft §3.13.1).
  DuplicateDirectiveArgumentName,
  /// A directive argument name begins with `__` (draft §3.13.1).
  ReservedDirectiveArgumentName,
  /// A directive argument's type is an output type (draft §3.13.1).
  DirectiveArgumentTypeNotInputType,
  /// A directive definition declares more arguments than
  /// [`MAX_DIRECTIVE_ARGUMENTS`](super::MAX_DIRECTIVE_ARGUMENTS).
  ///
  /// A capacity limit of this implementation, and the directive twin of
  /// [`TooManyFieldArguments`](Self::TooManyFieldArguments) rather than the same rule read twice.
  /// The field ceiling bounds draft §6.4.1's coercion, which no directive definition is put
  /// through; this one bounds draft 5.4.3's presence scan, which walks every argument the named
  /// definition declares at every usage a document writes. Both are `count × declared` with the
  /// count the client's and the width the deployment's, and both are answered where the
  /// deployment writes the width. The constant's own documentation has the measurement, the
  /// reason it is a separate number, and the rewrite a wider directive takes.
  TooManyDirectiveArguments,
  /// A directive definition refers to itself, directly or through the types it names
  /// (draft §3.13.1).
  SelfReferentialDirective,

  // -- §3.13 Directives, at a use site --------------------------------------------------------
  //
  // The six below are the *usage* rules, and they are the SDL half of checks the executable side
  // already runs per request as draft 5.7.1, 5.7.2, 5.7.3, 5.4.1, 5.4.3 and 5.6.1. A type-system
  // element is not a request, so nothing would otherwise ever ask these questions of an SDL: the
  // build is the only place a malformed schema is refused.
  /// A directive names no definition in the schema (draft §3.13).
  UndefinedDirective,
  /// A directive is used somewhere its definition does not list (draft §3.13).
  UnsupportedDirectiveLocation,
  /// A directive that is not `repeatable` is used twice in the same location (draft §3.13).
  ///
  /// A type and its extensions are one location: `type T @d` plus `extend type T @d` is a repeat,
  /// which is what merging extensions before the check makes fall out.
  DuplicateDirectiveUse,
  /// A directive is given an argument its definition does not declare (draft §3.13).
  UndefinedDirectiveArgument,
  /// A directive omits a required argument, or passes it `null` (draft §3.13).
  ///
  /// Required means non-null with no default. An explicit `null` is reported here rather than as
  /// [`InvalidDirectiveArgumentValue`](Self::InvalidDirectiveArgumentValue) so that one mistake
  /// produces one diagnostic, and it is the one that names the obligation.
  MissingRequiredDirectiveArgument,
  /// A directive argument's value does not fit the type the definition declares for it
  /// (draft §3.13, by way of §3.4's input coercion).
  InvalidDirectiveArgumentValue,
  /// A directive usage passes the same argument name twice (draft §3.13, draft 5.4.2's SDL twin).
  ///
  /// Distinct from [`DuplicateDirectiveArgumentName`](Self::DuplicateDirectiveArgumentName), which
  /// is a *definition* declaring one argument twice. Two mistakes, two coordinates: `d.a` names
  /// the declaration, `Query.@d.a` names the application.
  DuplicateDirectiveArgumentUse,

  // -- §3.13 use sites, continued: inside a constant input-object literal ---------------------
  //
  // Draft §3.13 admits an input object as a directive argument type, so an SDL constant position
  // can hold an input-object literal — and the two rules that govern one are draft 5.6.2 and
  // 5.6.4, which the executable side already runs per request as `smear-compiler`'s
  // `Rule::InputObjectFieldNames` and `Rule::InputObjectRequiredFields`. Named rather than
  // linked: `Rule` is in a crate above this one, so the link would point upward and rustdoc
  // could not resolve it. The
  // use-site pass descends into such a literal to type-check the fields the input object
  // *declares*; these two are what it used to say nothing about.
  /// An input-object literal names a field the input object type does not declare (draft §3.10.1
  /// at a constant position, draft 5.6.2's SDL twin).
  UndefinedInputObjectField,
  /// An input-object literal omits a required field, or passes it `null` (draft §3.10.1 at a
  /// constant position, draft 5.6.4's SDL twin).
  ///
  /// Required means non-null with no default. An explicit `null` is reported here rather than as
  /// [`InvalidDirectiveArgumentValue`](Self::InvalidDirectiveArgumentValue), for the same reason
  /// [`MissingRequiredDirectiveArgument`](Self::MissingRequiredDirectiveArgument) claims an
  /// explicitly `null` argument: one mistake produces one diagnostic, and it is the one that names
  /// the obligation.
  MissingRequiredInputObjectField,
}

impl SchemaErrorKind {
  /// Every kind the build can report.
  ///
  /// The build fixtures iterate this rather than a hand-kept list, so a kind added without a
  /// schema that makes it fire fails a test instead of shipping unexercised.
  pub const ALL: &'static [Self] = &[
    Self::DuplicateTypeName,
    Self::DuplicateDirectiveDefinition,
    Self::DuplicateSchemaDefinition,
    Self::DuplicateRootOperationType,
    Self::UndefinedRootOperationType,
    Self::RootOperationTypeNotObject,
    Self::SharedRootOperationType,
    Self::MissingQueryRootOperationType,
    Self::UndefinedExtensionTarget,
    Self::ExtensionKindMismatch,
    Self::RedefinedBuiltInType,
    Self::ReservedTypeName,
    Self::UndefinedType,
    Self::TypeReferenceTooDeep,
    Self::InvalidName,
    Self::TooManyNames,
    Self::EmptyFieldsDefinition,
    Self::DuplicateFieldName,
    Self::ReservedFieldName,
    Self::FieldTypeNotOutputType,
    Self::DuplicateArgumentName,
    Self::ReservedArgumentName,
    Self::ArgumentTypeNotInputType,
    Self::TooManyFieldArguments,
    Self::DeprecatedRequiredArgument,
    Self::InvalidDefaultValue,
    Self::ImplementsNonInterface,
    Self::UndefinedImplementsInterface,
    Self::DuplicateImplementsInterface,
    Self::SelfImplementingInterface,
    Self::MissingTransitiveInterface,
    Self::MissingInterfaceField,
    Self::InvalidInterfaceFieldType,
    Self::MissingInterfaceFieldArgument,
    Self::InvalidInterfaceFieldArgumentType,
    Self::UnexpectedRequiredArgument,
    Self::InterfaceFieldNotDeprecated,
    Self::EmptyUnionMembers,
    Self::UnionMemberNotObject,
    Self::UndefinedUnionMember,
    Self::DuplicateUnionMember,
    Self::EmptyEnumValues,
    Self::DuplicateEnumValue,
    Self::ReservedEnumValueName,
    Self::EmptyInputFields,
    Self::DuplicateInputFieldName,
    Self::ReservedInputFieldName,
    Self::InputFieldTypeNotInputType,
    Self::DeprecatedRequiredInputField,
    Self::OneOfFieldNotNullable,
    Self::OneOfFieldHasDefault,
    Self::CircularNonNullInputField,
    Self::InputObjectDefaultValueCycle,
    Self::OneOfOnInputObjectExtension,
    Self::ReservedDirectiveName,
    Self::DuplicateDirectiveArgumentName,
    Self::ReservedDirectiveArgumentName,
    Self::DirectiveArgumentTypeNotInputType,
    Self::TooManyDirectiveArguments,
    Self::SelfReferentialDirective,
    Self::UndefinedDirective,
    Self::UnsupportedDirectiveLocation,
    Self::DuplicateDirectiveUse,
    Self::UndefinedDirectiveArgument,
    Self::MissingRequiredDirectiveArgument,
    Self::InvalidDirectiveArgumentValue,
    Self::DuplicateDirectiveArgumentUse,
    Self::UndefinedInputObjectField,
    Self::MissingRequiredInputObjectField,
  ];

  /// Returns the phrase this kind renders as, with no subject attached.
  pub const fn message(&self) -> &'static str {
    match self {
      Self::DuplicateTypeName => "duplicate type definition",
      Self::DuplicateDirectiveDefinition => "duplicate directive definition",
      Self::DuplicateSchemaDefinition => "duplicate schema definition",
      Self::DuplicateRootOperationType => "duplicate root operation type",
      Self::UndefinedRootOperationType => "undefined root operation type",
      Self::RootOperationTypeNotObject => "root operation type is not an object type",
      Self::SharedRootOperationType => "two root operations name the same type",
      Self::MissingQueryRootOperationType => "no query root operation type",
      Self::UndefinedExtensionTarget => "extension target is not defined",
      Self::ExtensionKindMismatch => "extension does not match the kind of the type it extends",
      Self::RedefinedBuiltInType => "redefinition of an introspection type",
      Self::ReservedTypeName => "type name is reserved for introspection",
      Self::UndefinedType => "undefined type",
      Self::TypeReferenceTooDeep => "type reference nests too deeply",
      Self::InvalidName => "not a GraphQL name",
      Self::TooManyNames => "too many distinct names",
      Self::EmptyFieldsDefinition => "type defines no fields",
      Self::DuplicateFieldName => "duplicate field",
      Self::ReservedFieldName => "field name is reserved for introspection",
      Self::FieldTypeNotOutputType => "field type is not an output type",
      Self::DuplicateArgumentName => "duplicate argument",
      Self::ReservedArgumentName => "argument name is reserved for introspection",
      Self::ArgumentTypeNotInputType => "argument type is not an input type",
      Self::TooManyFieldArguments => "field declares too many arguments",
      Self::DeprecatedRequiredArgument => "required argument is deprecated",
      Self::InvalidDefaultValue => "default value does not fit its declared type",
      Self::ImplementsNonInterface => "implemented type is not an interface",
      Self::UndefinedImplementsInterface => "implemented interface is not defined",
      Self::DuplicateImplementsInterface => "duplicate implemented interface",
      Self::SelfImplementingInterface => "interface implements itself",
      Self::MissingTransitiveInterface => "transitively implemented interface is not declared",
      Self::MissingInterfaceField => "interface field is missing",
      Self::InvalidInterfaceFieldType => {
        "field type is not a valid implementation of the \
                                          interface field type"
      }
      Self::MissingInterfaceFieldArgument => "interface field argument is missing",
      Self::InvalidInterfaceFieldArgumentType => {
        "argument type differs from the interface \
                                                  field's"
      }
      Self::UnexpectedRequiredArgument => {
        "required argument is not declared by the interface \
                                            field"
      }
      Self::InterfaceFieldNotDeprecated => {
        "field is deprecated but the interface field it \
                                             implements is not"
      }
      Self::EmptyUnionMembers => "union declares no member types",
      Self::UnionMemberNotObject => "union member is not an object type",
      Self::UndefinedUnionMember => "union member is not defined",
      Self::DuplicateUnionMember => "duplicate union member",
      Self::EmptyEnumValues => "enum defines no values",
      Self::DuplicateEnumValue => "duplicate enum value",
      Self::ReservedEnumValueName => "enum value name is reserved for introspection",
      Self::EmptyInputFields => "input object defines no fields",
      Self::DuplicateInputFieldName => "duplicate input field",
      Self::ReservedInputFieldName => "input field name is reserved for introspection",
      Self::InputFieldTypeNotInputType => "input field type is not an input type",
      Self::DeprecatedRequiredInputField => "required input field is deprecated",
      Self::OneOfFieldNotNullable => "field of a @oneOf input object is non-null",
      Self::OneOfFieldHasDefault => "field of a @oneOf input object has a default value",
      Self::CircularNonNullInputField => "input object cycle has no nullable or list link",
      Self::InputObjectDefaultValueCycle => "input object default values form a cycle",
      Self::OneOfOnInputObjectExtension => "@oneOf is provided by an input object type extension",
      Self::ReservedDirectiveName => "directive name is reserved for introspection",
      Self::DuplicateDirectiveArgumentName => "duplicate directive argument",
      Self::ReservedDirectiveArgumentName => {
        "directive argument name is reserved for introspection"
      }
      Self::DirectiveArgumentTypeNotInputType => "directive argument type is not an input type",
      Self::TooManyDirectiveArguments => "directive declares too many arguments",
      Self::SelfReferentialDirective => "directive definition refers to itself",
      Self::UndefinedDirective => "undefined directive",
      Self::UnsupportedDirectiveLocation => "directive is not allowed here",
      Self::DuplicateDirectiveUse => "directive is not repeatable",
      Self::UndefinedDirectiveArgument => "undefined directive argument",
      Self::MissingRequiredDirectiveArgument => "required directive argument is missing",
      Self::InvalidDirectiveArgumentValue => {
        "directive argument value does not fit its declared type"
      }
      Self::DuplicateDirectiveArgumentUse => "directive argument is passed twice",
      Self::UndefinedInputObjectField => "undefined input object field",
      Self::MissingRequiredInputObjectField => "required input object field is missing",
    }
  }

  /// Returns the stable identifier for this kind.
  ///
  /// It is the [`Code`] every [`SchemaError`] of this kind answers, and it outlives every
  /// rewording of [`message`](Self::message) — which is the whole reason a consumer keys off
  /// this rather than off the text.
  #[inline]
  pub const fn code(&self) -> Code {
    match self {
      Self::DuplicateTypeName => Code::new("smear::schema::duplicate-type-name"),
      Self::DuplicateDirectiveDefinition => {
        Code::new("smear::schema::duplicate-directive-definition")
      }
      Self::DuplicateSchemaDefinition => Code::new("smear::schema::duplicate-schema-definition"),
      Self::DuplicateRootOperationType => Code::new("smear::schema::duplicate-root-operation-type"),
      Self::UndefinedRootOperationType => Code::new("smear::schema::undefined-root-operation-type"),
      Self::RootOperationTypeNotObject => {
        Code::new("smear::schema::root-operation-type-not-object")
      }
      Self::SharedRootOperationType => Code::new("smear::schema::shared-root-operation-type"),
      Self::MissingQueryRootOperationType => {
        Code::new("smear::schema::missing-query-root-operation-type")
      }
      Self::UndefinedExtensionTarget => Code::new("smear::schema::undefined-extension-target"),
      Self::ExtensionKindMismatch => Code::new("smear::schema::extension-kind-mismatch"),
      Self::RedefinedBuiltInType => Code::new("smear::schema::redefined-built-in-type"),
      Self::ReservedTypeName => Code::new("smear::schema::reserved-type-name"),
      Self::UndefinedType => Code::new("smear::schema::undefined-type"),
      Self::TypeReferenceTooDeep => Code::new("smear::schema::type-reference-too-deep"),
      Self::InvalidName => Code::new("smear::schema::invalid-name"),
      Self::TooManyNames => Code::new("smear::schema::too-many-names"),
      Self::EmptyFieldsDefinition => Code::new("smear::schema::empty-fields-definition"),
      Self::DuplicateFieldName => Code::new("smear::schema::duplicate-field-name"),
      Self::ReservedFieldName => Code::new("smear::schema::reserved-field-name"),
      Self::FieldTypeNotOutputType => Code::new("smear::schema::field-type-not-output-type"),
      Self::DuplicateArgumentName => Code::new("smear::schema::duplicate-argument-name"),
      Self::ReservedArgumentName => Code::new("smear::schema::reserved-argument-name"),
      Self::ArgumentTypeNotInputType => Code::new("smear::schema::argument-type-not-input-type"),
      Self::TooManyFieldArguments => Code::new("smear::schema::too-many-field-arguments"),
      Self::DeprecatedRequiredArgument => Code::new("smear::schema::deprecated-required-argument"),
      Self::InvalidDefaultValue => Code::new("smear::schema::invalid-default-value"),
      Self::ImplementsNonInterface => Code::new("smear::schema::implements-non-interface"),
      Self::UndefinedImplementsInterface => {
        Code::new("smear::schema::undefined-implements-interface")
      }
      Self::DuplicateImplementsInterface => {
        Code::new("smear::schema::duplicate-implements-interface")
      }
      Self::SelfImplementingInterface => Code::new("smear::schema::self-implementing-interface"),
      Self::MissingTransitiveInterface => Code::new("smear::schema::missing-transitive-interface"),
      Self::MissingInterfaceField => Code::new("smear::schema::missing-interface-field"),
      Self::InvalidInterfaceFieldType => Code::new("smear::schema::invalid-interface-field-type"),
      Self::MissingInterfaceFieldArgument => {
        Code::new("smear::schema::missing-interface-field-argument")
      }
      Self::InvalidInterfaceFieldArgumentType => {
        Code::new("smear::schema::invalid-interface-field-argument-type")
      }
      Self::UnexpectedRequiredArgument => Code::new("smear::schema::unexpected-required-argument"),
      Self::InterfaceFieldNotDeprecated => {
        Code::new("smear::schema::interface-field-not-deprecated")
      }
      Self::EmptyUnionMembers => Code::new("smear::schema::empty-union-members"),
      Self::UnionMemberNotObject => Code::new("smear::schema::union-member-not-object"),
      Self::UndefinedUnionMember => Code::new("smear::schema::undefined-union-member"),
      Self::DuplicateUnionMember => Code::new("smear::schema::duplicate-union-member"),
      Self::EmptyEnumValues => Code::new("smear::schema::empty-enum-values"),
      Self::DuplicateEnumValue => Code::new("smear::schema::duplicate-enum-value"),
      Self::ReservedEnumValueName => Code::new("smear::schema::reserved-enum-value-name"),
      Self::EmptyInputFields => Code::new("smear::schema::empty-input-fields"),
      Self::DuplicateInputFieldName => Code::new("smear::schema::duplicate-input-field-name"),
      Self::ReservedInputFieldName => Code::new("smear::schema::reserved-input-field-name"),
      Self::InputFieldTypeNotInputType => {
        Code::new("smear::schema::input-field-type-not-input-type")
      }
      Self::DeprecatedRequiredInputField => {
        Code::new("smear::schema::deprecated-required-input-field")
      }
      Self::OneOfFieldNotNullable => Code::new("smear::schema::one-of-field-not-nullable"),
      Self::OneOfFieldHasDefault => Code::new("smear::schema::one-of-field-has-default"),
      Self::CircularNonNullInputField => Code::new("smear::schema::circular-non-null-input-field"),
      Self::InputObjectDefaultValueCycle => {
        Code::new("smear::schema::input-object-default-value-cycle")
      }
      Self::OneOfOnInputObjectExtension => {
        Code::new("smear::schema::one-of-on-input-object-extension")
      }
      Self::ReservedDirectiveName => Code::new("smear::schema::reserved-directive-name"),
      Self::DuplicateDirectiveArgumentName => {
        Code::new("smear::schema::duplicate-directive-argument-name")
      }
      Self::ReservedDirectiveArgumentName => {
        Code::new("smear::schema::reserved-directive-argument-name")
      }
      Self::DirectiveArgumentTypeNotInputType => {
        Code::new("smear::schema::directive-argument-type-not-input-type")
      }
      Self::TooManyDirectiveArguments => Code::new("smear::schema::too-many-directive-arguments"),
      Self::SelfReferentialDirective => Code::new("smear::schema::self-referential-directive"),
      Self::UndefinedDirective => Code::new("smear::schema::undefined-directive"),
      Self::UnsupportedDirectiveLocation => {
        Code::new("smear::schema::unsupported-directive-location")
      }
      Self::DuplicateDirectiveUse => Code::new("smear::schema::duplicate-directive-use"),
      Self::UndefinedDirectiveArgument => Code::new("smear::schema::undefined-directive-argument"),
      Self::MissingRequiredDirectiveArgument => {
        Code::new("smear::schema::missing-required-directive-argument")
      }
      Self::InvalidDirectiveArgumentValue => {
        Code::new("smear::schema::invalid-directive-argument-value")
      }
      Self::DuplicateDirectiveArgumentUse => {
        Code::new("smear::schema::duplicate-directive-argument-use")
      }
      Self::UndefinedInputObjectField => Code::new("smear::schema::undefined-input-object-field"),
      Self::MissingRequiredInputObjectField => {
        Code::new("smear::schema::missing-required-input-object-field")
      }
    }
  }

  /// Returns how much this kind asks of its reader.
  ///
  /// Every draft §3 refusal is an [`Severity::Error`]: the build returns `Err` and there is no
  /// schema. The arm is spelled out variant by variant rather than as a wildcard so that a kind
  /// added for the deprecation-lint class has to say so here.
  #[inline]
  pub const fn severity(&self) -> Severity {
    match self {
      Self::DuplicateTypeName
      | Self::DuplicateDirectiveDefinition
      | Self::DuplicateSchemaDefinition
      | Self::DuplicateRootOperationType
      | Self::UndefinedRootOperationType
      | Self::RootOperationTypeNotObject
      | Self::SharedRootOperationType
      | Self::MissingQueryRootOperationType
      | Self::UndefinedExtensionTarget
      | Self::ExtensionKindMismatch
      | Self::RedefinedBuiltInType
      | Self::ReservedTypeName
      | Self::UndefinedType
      | Self::TypeReferenceTooDeep
      | Self::InvalidName
      | Self::TooManyNames
      | Self::EmptyFieldsDefinition
      | Self::DuplicateFieldName
      | Self::ReservedFieldName
      | Self::FieldTypeNotOutputType
      | Self::DuplicateArgumentName
      | Self::ReservedArgumentName
      | Self::ArgumentTypeNotInputType
      | Self::TooManyFieldArguments
      | Self::DeprecatedRequiredArgument
      | Self::InvalidDefaultValue
      | Self::ImplementsNonInterface
      | Self::UndefinedImplementsInterface
      | Self::DuplicateImplementsInterface
      | Self::SelfImplementingInterface
      | Self::MissingTransitiveInterface
      | Self::MissingInterfaceField
      | Self::InvalidInterfaceFieldType
      | Self::MissingInterfaceFieldArgument
      | Self::InvalidInterfaceFieldArgumentType
      | Self::UnexpectedRequiredArgument
      | Self::InterfaceFieldNotDeprecated
      | Self::EmptyUnionMembers
      | Self::UnionMemberNotObject
      | Self::UndefinedUnionMember
      | Self::DuplicateUnionMember
      | Self::EmptyEnumValues
      | Self::DuplicateEnumValue
      | Self::ReservedEnumValueName
      | Self::EmptyInputFields
      | Self::DuplicateInputFieldName
      | Self::ReservedInputFieldName
      | Self::InputFieldTypeNotInputType
      | Self::DeprecatedRequiredInputField
      | Self::OneOfFieldNotNullable
      | Self::OneOfFieldHasDefault
      | Self::CircularNonNullInputField
      | Self::InputObjectDefaultValueCycle
      | Self::OneOfOnInputObjectExtension
      | Self::ReservedDirectiveName
      | Self::DuplicateDirectiveArgumentName
      | Self::ReservedDirectiveArgumentName
      | Self::DirectiveArgumentTypeNotInputType
      | Self::TooManyDirectiveArguments
      | Self::SelfReferentialDirective
      | Self::UndefinedDirective
      | Self::UnsupportedDirectiveLocation
      | Self::DuplicateDirectiveUse
      | Self::UndefinedDirectiveArgument
      | Self::MissingRequiredDirectiveArgument
      | Self::InvalidDirectiveArgumentValue
      | Self::DuplicateDirectiveArgumentUse
      | Self::UndefinedInputObjectField
      | Self::MissingRequiredInputObjectField => Severity::Error,
    }
  }

  /// Returns what the SDL author can do about it, where the rule has something actionable to
  /// say beyond [`message`](Self::message).
  ///
  /// `None` is a real answer, not a gap: for a duplicate the two spans say everything, and a
  /// help line repeating the message would be noise in every renderer that shows both.
  #[inline]
  pub const fn help(&self) -> Option<&'static str> {
    match self {
      Self::DuplicateTypeName => {
        Some("rename one of the definitions, or make the second an `extend` of the first.")
      }
      Self::DuplicateDirectiveDefinition => Some(
        "`repeatable` governs repeated *use*, not repeated definition; a directive is defined once.",
      ),
      Self::DuplicateSchemaDefinition => {
        Some("a document holds at most one `schema` block; put the extra roots in `extend schema`.")
      }
      Self::RootOperationTypeNotObject => Some(
        "a root operation type must be an object type: an interface or union has no concrete field set to resolve.",
      ),
      Self::SharedRootOperationType => Some(
        "the three roots are the entry points of three different operation kinds, so each needs its own object type.",
      ),
      Self::MissingQueryRootOperationType => {
        Some("define `type Query`, or name an existing object type with `schema { query: … }`.")
      }
      Self::UndefinedExtensionTarget => {
        Some("`extend` adds to a type the document defines; define it, or drop the `extend`.")
      }
      Self::ExtensionKindMismatch => {
        Some("the `extend` keyword has to match the kind of the definition it extends.")
      }
      Self::RedefinedBuiltInType => Some(
        "the `__`-prefixed introspection types are injected by every build and cannot be replaced.",
      ),
      Self::ReservedTypeName => {
        Some("a name beginning with `__` is reserved for introspection; rename it.")
      }
      Self::TypeReferenceTooDeep => Some(
        "this implementation packs a bounded number of list and non-null wrappers into one type reference; flatten the type.",
      ),
      Self::InvalidName => Some("a GraphQL name is spelled `/[_A-Za-z][_0-9A-Za-z]*/`."),
      Self::EmptyFieldsDefinition => {
        Some("an object or interface type must define at least one field.")
      }
      Self::ReservedFieldName => {
        Some("a name beginning with `__` is reserved for introspection; rename it.")
      }
      Self::FieldTypeNotOutputType => Some(
        "a field's type must be a scalar, object, interface, union or enum; an input object appears only in argument and input-field positions.",
      ),
      Self::ReservedArgumentName => {
        Some("a name beginning with `__` is reserved for introspection; rename it.")
      }
      Self::ArgumentTypeNotInputType => {
        Some("an argument's type must be a scalar, enum or input object.")
      }
      Self::TooManyFieldArguments => Some(
        "argument coercion runs once per runtime position of the field, so this list is bounded at          the schema; gather the arguments into one input object.",
      ),
      Self::DeprecatedRequiredArgument => Some(
        "to deprecate a required argument, first make it optional — give it a default, or drop the `!`.",
      ),
      Self::MissingTransitiveInterface => Some(
        "a type that implements an interface must also declare everything that interface implements.",
      ),
      Self::MissingInterfaceField => {
        Some("add the field, with a type valid for the interface field's.")
      }
      Self::InvalidInterfaceFieldType => Some(
        "an implementing field may add `!` and narrow to a member or implementor; it may never widen.",
      ),
      Self::MissingInterfaceFieldArgument => {
        Some("add the argument, with the interface field's exact type.")
      }
      Self::InvalidInterfaceFieldArgumentType => Some(
        "argument types are invariant, so the implementing field's argument must match the interface field's exactly.",
      ),
      Self::UnexpectedRequiredArgument => Some(
        "an argument the interface field does not declare must be optional — give it a default, or drop the `!`.",
      ),
      Self::InterfaceFieldNotDeprecated => Some(
        "deprecate the interface field too, or undeprecate this one; the obligation runs outwards from the interface.",
      ),
      Self::UnionMemberNotObject => Some(
        "a union's members must be object types: a union of interfaces or of unions has no concrete member set.",
      ),
      Self::ReservedEnumValueName => {
        Some("a name beginning with `__` is reserved for introspection; rename it.")
      }
      Self::ReservedInputFieldName => {
        Some("a name beginning with `__` is reserved for introspection; rename it.")
      }
      Self::InputFieldTypeNotInputType => {
        Some("an input field's type must be a scalar, enum or input object.")
      }
      Self::DeprecatedRequiredInputField => Some(
        "to deprecate a required input field, first make it optional — give it a default, or drop the `!`.",
      ),
      Self::OneOfFieldNotNullable => Some(
        "every field of a `@oneOf` input object is one of the alternatives, so each one has to be nullable.",
      ),
      Self::OneOfFieldHasDefault => {
        Some("a defaulted field would always be present, which is the one thing `@oneOf` forbids.")
      }
      Self::CircularNonNullInputField => Some(
        "break the cycle with a nullable link or a list; as written, no finite value satisfies it.",
      ),
      Self::InputObjectDefaultValueCycle => {
        Some("drop one of the defaults, or give it a value that does not refer back.")
      }
      Self::OneOfOnInputObjectExtension => Some(
        "`@oneOf` changes what every existing field of the type means, so it belongs on the definition.",
      ),
      Self::ReservedDirectiveName => {
        Some("a name beginning with `__` is reserved for introspection; rename it.")
      }
      Self::ReservedDirectiveArgumentName => {
        Some("a name beginning with `__` is reserved for introspection; rename it.")
      }
      Self::DirectiveArgumentTypeNotInputType => {
        Some("a directive argument's type must be a scalar, enum or input object.")
      }
      Self::TooManyDirectiveArguments => Some(
        "every usage rescans the whole declared list, so it is bounded at the schema; gather the arguments into one input object.",
      ),
      Self::SelfReferentialDirective => Some(
        "a directive may not appear on its own definition, nor on any type that definition names.",
      ),
      Self::UnsupportedDirectiveLocation => {
        Some("add the location to the directive's definition, or move the usage.")
      }
      Self::DuplicateDirectiveUse => Some(
        "declare the directive `repeatable`, or apply it once; a type and its extensions are one location.",
      ),
      Self::MissingRequiredDirectiveArgument => Some(
        "a required argument is non-null with no default: pass it, or give the definition a default.",
      ),
      Self::MissingRequiredInputObjectField => Some(
        "a required field is non-null with no default: pass it, or give the definition a default.",
      ),
      Self::DuplicateRootOperationType
      | Self::UndefinedRootOperationType
      | Self::UndefinedType
      | Self::TooManyNames
      | Self::DuplicateFieldName
      | Self::DuplicateArgumentName
      | Self::InvalidDefaultValue
      | Self::ImplementsNonInterface
      | Self::UndefinedImplementsInterface
      | Self::DuplicateImplementsInterface
      | Self::SelfImplementingInterface
      | Self::EmptyUnionMembers
      | Self::UndefinedUnionMember
      | Self::DuplicateUnionMember
      | Self::EmptyEnumValues
      | Self::DuplicateEnumValue
      | Self::EmptyInputFields
      | Self::DuplicateInputFieldName
      | Self::DuplicateDirectiveArgumentName
      | Self::UndefinedDirective
      | Self::UndefinedDirectiveArgument
      | Self::InvalidDirectiveArgumentValue
      | Self::DuplicateDirectiveArgumentUse
      | Self::UndefinedInputObjectField => None,
    }
  }

  /// Returns the phrase for the second span a [`SchemaError`] of this kind points at.
  ///
  /// `Some` exactly for the kinds that are about a *relationship* between two places — a
  /// duplicate and the definition it repeats, an implementing field and the interface field it
  /// answers to. A kind that never attaches a related span answers `None`, and
  /// `tests/diagnostic_codes.rs` pins the implication in the direction that matters: an error
  /// carrying a related span whose kind has no phrase for it would drop the span silently.
  #[inline]
  pub const fn related_label(&self) -> Option<&'static str> {
    match self {
      Self::DuplicateTypeName => Some("first defined here"),
      Self::DuplicateDirectiveDefinition => Some("first defined here"),
      Self::DuplicateSchemaDefinition => Some("first defined here"),
      Self::DuplicateRootOperationType => Some("first named here"),
      Self::SharedRootOperationType => Some("already a root operation type here"),
      Self::ExtensionKindMismatch => Some("the definition it extends"),
      Self::DuplicateFieldName => Some("first defined here"),
      Self::DuplicateArgumentName => Some("first declared here"),
      Self::DuplicateImplementsInterface => Some("first implemented here"),
      Self::InterfaceFieldNotDeprecated => Some("the interface field, which is not deprecated"),
      Self::DuplicateUnionMember => Some("first named here"),
      Self::DuplicateEnumValue => Some("first defined here"),
      Self::DuplicateInputFieldName => Some("first defined here"),
      Self::DuplicateDirectiveArgumentName => Some("first declared here"),
      Self::DuplicateDirectiveUse => Some("first applied here"),
      Self::DuplicateDirectiveArgumentUse => Some("first passed here"),
      Self::UndefinedRootOperationType
      | Self::RootOperationTypeNotObject
      | Self::MissingQueryRootOperationType
      | Self::UndefinedExtensionTarget
      | Self::RedefinedBuiltInType
      | Self::ReservedTypeName
      | Self::UndefinedType
      | Self::TypeReferenceTooDeep
      | Self::InvalidName
      | Self::TooManyNames
      | Self::EmptyFieldsDefinition
      | Self::ReservedFieldName
      | Self::FieldTypeNotOutputType
      | Self::ReservedArgumentName
      | Self::ArgumentTypeNotInputType
      | Self::TooManyFieldArguments
      | Self::DeprecatedRequiredArgument
      | Self::InvalidDefaultValue
      | Self::ImplementsNonInterface
      | Self::UndefinedImplementsInterface
      | Self::SelfImplementingInterface
      | Self::MissingTransitiveInterface
      | Self::MissingInterfaceField
      | Self::InvalidInterfaceFieldType
      | Self::MissingInterfaceFieldArgument
      | Self::InvalidInterfaceFieldArgumentType
      | Self::UnexpectedRequiredArgument
      | Self::EmptyUnionMembers
      | Self::UnionMemberNotObject
      | Self::UndefinedUnionMember
      | Self::EmptyEnumValues
      | Self::ReservedEnumValueName
      | Self::EmptyInputFields
      | Self::ReservedInputFieldName
      | Self::InputFieldTypeNotInputType
      | Self::DeprecatedRequiredInputField
      | Self::OneOfFieldNotNullable
      | Self::OneOfFieldHasDefault
      | Self::CircularNonNullInputField
      | Self::InputObjectDefaultValueCycle
      | Self::OneOfOnInputObjectExtension
      | Self::ReservedDirectiveName
      | Self::ReservedDirectiveArgumentName
      | Self::DirectiveArgumentTypeNotInputType
      | Self::TooManyDirectiveArguments
      | Self::SelfReferentialDirective
      | Self::UndefinedDirective
      | Self::UnsupportedDirectiveLocation
      | Self::UndefinedDirectiveArgument
      | Self::MissingRequiredDirectiveArgument
      | Self::InvalidDirectiveArgumentValue
      | Self::UndefinedInputObjectField
      | Self::MissingRequiredInputObjectField => None,
    }
  }
}

impl core::fmt::Display for SchemaErrorKind {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.message())
  }
}

/// One reason [`Schema::build`](super::Schema::build) refused a document.
///
/// Every error names its subject — the artifact that was rejected, qualified by its owner where
/// it has one, so `User.pet` reads as the field and `User.pet.first` as its argument — and points
/// at it with a span into the document it came from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemaError {
  kind: SchemaErrorKind,
  subject: Box<str>,
  owner: Option<Box<str>>,
  span: SimpleSpan,
  related: Option<SimpleSpan>,
  document: u32,
}

impl SchemaError {
  /// Assembles an error naming a top-level artifact.
  pub(crate) fn new(kind: SchemaErrorKind, subject: &str, span: SimpleSpan) -> Self {
    Self {
      kind,
      subject: subject.into(),
      owner: None,
      span,
      related: None,
      document: 0,
    }
  }

  /// Qualifies the subject with the artifact that owns it.
  pub(crate) fn owned_by(mut self, owner: impl Into<String>) -> Self {
    self.owner = Some(owner.into().into_boxed_str());
    self
  }

  /// Points at a second, related position — the first of two duplicates, say.
  pub(crate) fn related_to(mut self, span: SimpleSpan) -> Self {
    self.related = Some(span);
    self
  }

  /// Records which of the builder's documents the spans belong to.
  pub(crate) fn in_document(mut self, index: u32) -> Self {
    self.document = index;
    self
  }

  /// Returns which rule refused.
  #[inline]
  pub const fn kind(&self) -> SchemaErrorKind {
    self.kind
  }

  /// Returns the name of the rejected artifact, unqualified.
  #[inline]
  pub fn subject(&self) -> &str {
    &self.subject
  }

  /// Returns the name of the artifact that owns the subject, when it has one.
  #[inline]
  pub fn owner(&self) -> Option<&str> {
    self.owner.as_deref()
  }

  /// Returns the subject's position in its document.
  #[inline]
  pub const fn span(&self) -> SimpleSpan {
    self.span
  }

  /// Returns a second position the error refers to, when there is one.
  #[inline]
  pub const fn related(&self) -> Option<SimpleSpan> {
    self.related
  }

  /// Returns the index of the document the spans belong to, in the order the builder was given
  /// them.
  #[inline]
  pub const fn document(&self) -> u32 {
    self.document
  }

  /// The related span paired with the phrase its kind gives it.
  ///
  /// Both halves are needed for a label to exist, and the pair is computed once here so that
  /// [`Diagnose::labels`] and [`Diagnose::label`] cannot disagree about how many there are.
  #[inline]
  const fn secondary(&self) -> Option<Label> {
    match (self.related, self.kind.related_label()) {
      (Some(span), Some(text)) => Some(Label::new(Location::new(self.document, span), text)),
      _ => None,
    }
  }
}

/// A draft §3 refusal, read as structure rather than as a sentence.
///
/// The primary position is the artifact the build rejected, in the document it came from — the
/// builder accepts several and numbers them, which is what [`SchemaError::document`] carries and
/// why the location is not a bare span.
///
/// There is at most one secondary label, and it is the kind's own phrase over
/// [`SchemaError::related`]: a duplicate points back at the definition it repeats, an implementing
/// field at the interface field it answers to.
///
/// **No path segments, ever.** A schema refusal is about SDL, and draft §7.1.2's `path` is a
/// coordinate in a *response*. The dotted owner path this error renders — `User.pet.first` — is a
/// schema coordinate that merely looks similar, and publishing it as a response path would put a
/// `path` on the wire for an error that never had one.
impl Diagnose for SchemaError {
  #[inline]
  fn code(&self) -> Code {
    self.kind.code()
  }

  #[inline]
  fn severity(&self) -> Severity {
    self.kind.severity()
  }

  #[inline]
  fn primary(&self) -> Location {
    Location::new(self.document, self.span)
  }

  #[inline]
  fn primary_label(&self) -> Option<&'static str> {
    Some(self.kind.message())
  }

  #[inline]
  fn labels(&self) -> usize {
    usize::from(self.secondary().is_some())
  }

  #[inline]
  fn label(&self, index: usize) -> Option<Label> {
    match index {
      0 => self.secondary(),
      _ => None,
    }
  }

  #[inline]
  fn path_segments(&self) -> usize {
    0
  }

  #[inline]
  fn path_segment(&self, _: usize) -> Option<PathSegment<'_>> {
    None
  }

  #[inline]
  fn help(&self) -> Option<&'static str> {
    self.kind.help()
  }
}

impl core::fmt::Display for SchemaError {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}: `", self.kind)?;
    if let Some(owner) = &self.owner {
      write!(f, "{owner}.")?;
    }
    write!(f, "{}`", self.subject)
  }
}

impl core::error::Error for SchemaError {}

/// Every reason a build failed, in the order the builder found them.
///
/// The build does not stop at the first refusal: an SDL author wants the whole list, and the path
/// runs once.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemaErrors {
  errors: Vec<SchemaError>,
}

impl SchemaErrors {
  /// Wraps a nonempty error list.
  pub(crate) fn new(errors: Vec<SchemaError>) -> Self {
    Self { errors }
  }

  /// Returns the errors.
  #[inline]
  pub fn errors(&self) -> &[SchemaError] {
    &self.errors
  }

  /// Returns how many errors the build reported.
  #[inline]
  pub fn len(&self) -> usize {
    self.errors.len()
  }

  /// Returns whether the list is empty. It never is for a returned `Err`.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.errors.is_empty()
  }

  /// Returns whether any error has the given kind.
  #[inline]
  pub fn contains_kind(&self, kind: SchemaErrorKind) -> bool {
    self.errors.iter().any(|error| error.kind() == kind)
  }

  /// Returns every kind reported, deduplicated and sorted.
  pub fn kinds(&self) -> Vec<SchemaErrorKind> {
    let mut kinds: Vec<_> = self.errors.iter().map(SchemaError::kind).collect();
    kinds.sort_unstable();
    kinds.dedup();
    kinds
  }
}

impl AsRef<[SchemaError]> for SchemaErrors {
  #[inline]
  fn as_ref(&self) -> &[SchemaError] {
    &self.errors
  }
}

impl core::ops::Deref for SchemaErrors {
  type Target = [SchemaError];

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.errors
  }
}

impl IntoIterator for SchemaErrors {
  type Item = SchemaError;
  type IntoIter = std::vec::IntoIter<SchemaError>;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.errors.into_iter()
  }
}

impl core::fmt::Display for SchemaErrors {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let plural = if self.errors.len() == 1 { "" } else { "s" };
    write!(f, "{} schema error{plural}", self.errors.len())?;
    for error in &self.errors {
      write!(f, "\n  {error}")?;
    }
    Ok(())
  }
}

impl core::error::Error for SchemaErrors {}

/// Convenience for the builder: an owner path built from up to three name segments.
pub(crate) fn owner_path(segments: &[&str]) -> String {
  let mut path = String::new();
  for (index, segment) in segments.iter().enumerate() {
    if index > 0 {
      path.push('.');
    }
    path.push_str(segment);
  }
  path
}

/// The owner path of a directive *usage*: the element it sits on, then `@name`.
///
/// The `@` is what keeps `Query.@deprecated.reason` from reading as a field path, which matters
/// because an argument of a directive on a field and an argument of the field itself would
/// otherwise render identically.
pub(crate) fn directive_coordinate(owner: &str, directive: &str) -> String {
  let mut path = String::with_capacity(owner.len() + directive.len() + 2);
  path.push_str(owner);
  path.push('.');
  path.push('@');
  path.push_str(directive);
  path
}
