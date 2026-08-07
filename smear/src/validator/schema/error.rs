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
  RootOperationTypeNotObject,
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
  /// A field of a `@oneOf` input object is non-null (draft §3.10.1).
  OneOfFieldNotNullable,
  /// A field of a `@oneOf` input object declares a default value (draft §3.10.1).
  OneOfFieldHasDefault,
  /// Input objects form a cycle in which every link is non-null, so no finite value satisfies it
  /// (draft §3.10.1).
  CircularNonNullInputField,

  // -- §3.13 Directives ---------------------------------------------------------------------
  /// A directive name begins with `__` (draft §3.13.1).
  ReservedDirectiveName,
  /// A directive declares the same argument name twice (draft §3.13.1).
  DuplicateDirectiveArgumentName,
  /// A directive argument name begins with `__` (draft §3.13.1).
  ReservedDirectiveArgumentName,
  /// A directive argument's type is an output type (draft §3.13.1).
  DirectiveArgumentTypeNotInputType,
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
  // 5.6.4, which the executable side already runs per request as
  // [`Rule::InputObjectFieldNames`](crate::validator::Rule::InputObjectFieldNames) and
  // [`Rule::InputObjectRequiredFields`](crate::validator::Rule::InputObjectRequiredFields). The
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
    Self::OneOfFieldNotNullable,
    Self::OneOfFieldHasDefault,
    Self::CircularNonNullInputField,
    Self::ReservedDirectiveName,
    Self::DuplicateDirectiveArgumentName,
    Self::ReservedDirectiveArgumentName,
    Self::DirectiveArgumentTypeNotInputType,
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
      Self::OneOfFieldNotNullable => "field of a @oneOf input object is non-null",
      Self::OneOfFieldHasDefault => "field of a @oneOf input object has a default value",
      Self::CircularNonNullInputField => "input object cycle has no nullable or list link",
      Self::ReservedDirectiveName => "directive name is reserved for introspection",
      Self::DuplicateDirectiveArgumentName => "duplicate directive argument",
      Self::ReservedDirectiveArgumentName => {
        "directive argument name is reserved for introspection"
      }
      Self::DirectiveArgumentTypeNotInputType => "directive argument type is not an input type",
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
