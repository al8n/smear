//! Turning one or more `TypeSystemDocument`s into a [`Schema`], and refusing the ones that are
//! not schemas.
//!
//! # Everything happens here, once
//!
//! The builder interns, merges extensions, injects the built-in and introspection definitions,
//! resolves every type reference, runs draft §3 "Type Validation", and flattens the result into
//! the tables validation reads. A server pays for all of it at startup and nothing per request —
//! which is the whole reason the representation looks the way it does.
//!
//! # The source type stops at the door
//!
//! [`SchemaBuilder::document`] is generic over the document's source slice `S`, bounded by
//! `AsRef<[u8]>` — the bound the entire `tokora` source lattice satisfies, from `&str` through
//! `bytes::Bytes` to `HipStr`. Names are copied into the schema's own arena as they are interned,
//! so the finished [`Schema`] borrows nothing and a single builder may be fed documents with
//! *different* `S` types. The generic parameter never appears on `Schema`.

use std::{
  borrow::ToOwned,
  boxed::Box,
  collections::BTreeMap,
  string::{String, ToString},
  vec,
  vec::Vec,
};

use tokora::{Parse as _, Parser, SimpleSpan, span::AsSpan};

use crate::parser::graphql::{
  GraphQL,
  ast::{
    ConstDirectives, ConstInputValue, DirectiveDefinition, EnumTypeDefinition,
    EnumValuesDefinition, FieldsDefinition, ImplementInterfaces, InputFieldsDefinition,
    InputObjectTypeDefinition, InputValueDefinition, InterfaceTypeDefinition, Location, Name,
    ObjectTypeDefinition, OperationType, ScalarTypeDefinition, SchemaDefinition, Type,
    TypeDefinition, TypeExtension, TypeSystemDefinition, TypeSystemDefinitionOrExtension,
    TypeSystemDocument, TypeSystemExtension, UnionMemberTypes, UnionTypeDefinition,
  },
  error::GraphqlErrors,
  syntactic::{GraphqlLexer, type_system_document},
};

use super::{
  builtin,
  error::{SchemaError, SchemaErrorKind, SchemaErrors, owner_path},
  repr::{
    DefaultKind, DirectiveDef, DirectiveLocation, DirectiveLocations, FieldDef, InputValueDef,
    MAX_SYMBOLS, NameIndex, PackedType, Range32, RootOperation, Schema, Sym, TypeDef, TypeFlags,
    TypeId, TypeKind, is_name, is_reserved,
  },
};

/// The placeholder a type reference carries until its base name is resolved.
const UNRESOLVED: TypeId = TypeId::new(u32::MAX);

// ---------------------------------------------------------------------------------------------
// interning
// ---------------------------------------------------------------------------------------------

/// The growable half of the name arena.
///
/// The finished [`Schema`] keeps `strings` and `spans` and a probe-only [`NameIndex`]; this map
/// exists only while building, which is why the schema has no hash map in it at all.
#[derive(Debug, Default)]
struct Interner {
  strings: Vec<u8>,
  spans: Vec<(u32, u32)>,
  map: BTreeMap<Box<[u8]>, u32>,
}

impl Interner {
  fn intern(&mut self, bytes: &[u8]) -> Sym {
    if let Some(sym) = self.map.get(bytes) {
      return Sym::new(*sym);
    }
    let start = self.strings.len() as u32;
    self.strings.extend_from_slice(bytes);
    let end = self.strings.len() as u32;
    let sym = self.spans.len() as u32;
    self.spans.push((start, end));
    self.map.insert(bytes.to_owned().into_boxed_slice(), sym);
    Sym::new(sym)
  }

  fn lookup(&self, bytes: &[u8]) -> Option<Sym> {
    self.map.get(bytes).copied().map(Sym::new)
  }

  fn bytes(&self, sym: Sym) -> &[u8] {
    let (start, end) = self.spans[sym.get() as usize];
    &self.strings[start as usize..end as usize]
  }

  fn text(&self, sym: Sym) -> &str {
    // ASCII by construction; every name is admitted through `is_name` first.
    core::str::from_utf8(self.bytes(sym)).unwrap_or("")
  }

  fn len(&self) -> u32 {
    self.spans.len() as u32
  }
}

// ---------------------------------------------------------------------------------------------
// the owned intermediate model
// ---------------------------------------------------------------------------------------------

/// An interned name with the position and document it was read from.
#[derive(Debug, Clone, Copy)]
struct Located {
  sym: Sym,
  span: SimpleSpan,
  document: u32,
}

/// A type reference whose base has been interned but not yet resolved.
#[derive(Debug, Clone, Copy)]
struct RawTypeRef {
  base: Located,
  span: SimpleSpan,
  packed: PackedType,
  too_deep: bool,
}

#[derive(Debug, Clone)]
struct RawInput {
  name: Located,
  ty: RawTypeRef,
  default: DefaultKind,
  directives: Vec<Located>,
}

#[derive(Debug, Clone)]
struct RawField {
  name: Located,
  ty: RawTypeRef,
  args: Vec<RawInput>,
}

#[derive(Debug, Clone)]
struct RawEnumValue {
  name: Located,
  directives: Vec<Located>,
}

#[derive(Debug, Clone)]
struct RawType {
  name: Located,
  kind: TypeKind,
  built_in: bool,
  /// The definition collides with an introspection type, already reported as
  /// `RedefinedBuiltInType`; the reserved-name rule stays quiet so one defect reports once.
  collides_with_built_in: bool,
  implements: Vec<Located>,
  fields: Vec<RawField>,
  input_fields: Vec<RawInput>,
  members: Vec<Located>,
  enum_values: Vec<RawEnumValue>,
  directives: Vec<Located>,
  /// The interface closure, filled in during validation.
  closure: Vec<u32>,
}

impl RawType {
  fn new(name: Located, kind: TypeKind, built_in: bool) -> Self {
    Self {
      name,
      kind,
      built_in,
      collides_with_built_in: false,
      implements: Vec::new(),
      fields: Vec::new(),
      input_fields: Vec::new(),
      members: Vec::new(),
      enum_values: Vec::new(),
      directives: Vec::new(),
      closure: Vec::new(),
    }
  }
}

#[derive(Debug, Clone)]
struct RawDirectiveDef {
  name: Located,
  args: Vec<RawInput>,
  locations: DirectiveLocations,
  repeatable: bool,
  built_in: bool,
}

/// A type extension, converted to owned form and applied once every document has been read.
#[derive(Debug, Clone, Default)]
struct RawExtension {
  target: Option<Located>,
  kind: Option<TypeKind>,
  implements: Vec<Located>,
  fields: Vec<RawField>,
  input_fields: Vec<RawInput>,
  members: Vec<Located>,
  enum_values: Vec<RawEnumValue>,
  directives: Vec<Located>,
  /// Root operations, for a `extend schema` rather than a type extension.
  roots: Vec<(RootOperation, Located)>,
}

// ---------------------------------------------------------------------------------------------
// the builder
// ---------------------------------------------------------------------------------------------

/// Accumulates type-system documents and produces a [`Schema`].
///
/// Documents are read in the order they are given; extensions are applied only once every
/// document has been read, so an `extend type Foo` may precede `type Foo` and may live in a
/// different document.
#[derive(Debug, Default)]
pub struct SchemaBuilder {
  interner: Interner,
  types: Vec<RawType>,
  /// `Sym` to an index into `types`, dense over the symbol space; `u32::MAX` for "not a type".
  type_of_sym: Vec<u32>,
  directives: Vec<RawDirectiveDef>,
  /// `Sym` to an index into `directives`; `u32::MAX` for "not a directive".
  directive_of_sym: Vec<u32>,
  roots: [Option<Located>; 3],
  schema_definition: Option<SimpleSpan>,
  extensions: Vec<RawExtension>,
  errors: Vec<SchemaError>,
  document: u32,
}

impl SchemaBuilder {
  /// Creates an empty builder.
  #[inline]
  pub fn new() -> Self {
    Self::default()
  }

  /// Reads one type-system document.
  ///
  /// Definitions are recorded immediately; extensions are deferred to [`SchemaBuilder::finish`].
  /// Errors are accumulated rather than returned, so a caller sees every problem in every
  /// document at the end rather than one at a time.
  pub fn document<S>(&mut self, doc: &TypeSystemDocument<S>) -> &mut Self
  where
    S: AsRef<[u8]>,
  {
    for entry in doc.definitions() {
      match entry {
        TypeSystemDefinitionOrExtension::Definition(described) => {
          self.definition(described.node(), false);
        }
        TypeSystemDefinitionOrExtension::Extension(extension) => {
          self.extension(extension);
        }
      }
    }
    self.document += 1;
    self
  }

  /// Consumes the builder and produces a schema, or every reason it is not one.
  pub fn finish(mut self) -> Result<Schema, SchemaErrors> {
    // Injection precedes extension so that `extend scalar Int @tag(...)` — extending something
    // the specification provided rather than something the document defined — is not reported as
    // an undefined target. Whether a built-in is injected at all is decided by the *definitions*,
    // all of which are already in, so the order does not change what gets replaced.
    self.inject_built_ins();
    self.apply_extensions();
    self.validate();

    if !self.errors.is_empty() {
      return Err(SchemaErrors::new(self.errors));
    }
    self.flatten()
  }

  // -- error helpers --------------------------------------------------------------------------

  fn push(&mut self, kind: SchemaErrorKind, subject: &str, at: Located) {
    self
      .errors
      .push(SchemaError::new(kind, subject, at.span).in_document(at.document));
  }

  fn push_owned(&mut self, kind: SchemaErrorKind, subject: &str, owner: String, at: Located) {
    self.errors.push(
      SchemaError::new(kind, subject, at.span)
        .owned_by(owner)
        .in_document(at.document),
    );
  }

  fn push_related(
    &mut self,
    kind: SchemaErrorKind,
    subject: &str,
    owner: Option<String>,
    at: Located,
    related: SimpleSpan,
  ) {
    let mut error = SchemaError::new(kind, subject, at.span)
      .in_document(at.document)
      .related_to(related);
    if let Some(owner) = owner {
      error = error.owned_by(owner);
    }
    self.errors.push(error);
  }

  // -- interning ------------------------------------------------------------------------------

  fn located<S>(&mut self, name: &Name<S>) -> Located
  where
    S: AsRef<[u8]>,
  {
    let bytes = name.source().as_ref();
    let span = *name.as_span();
    let document = self.document;
    if !is_name(bytes) {
      // Unreachable from parsed input; reachable from a hand-assembled AST, and the arena's
      // ASCII invariant is what makes `Schema::name` infallible, so it is checked.
      let rendered = String::from_utf8_lossy(bytes).to_string();
      self.errors.push(
        SchemaError::new(SchemaErrorKind::InvalidName, &rendered, span).in_document(document),
      );
      let sym = self.interner.intern(b"__invalid");
      return Located {
        sym,
        span,
        document,
      };
    }
    let sym = self.interner.intern(bytes);
    Located {
      sym,
      span,
      document,
    }
  }

  fn text(&self, sym: Sym) -> &str {
    self.interner.text(sym)
  }

  // -- ingest ---------------------------------------------------------------------------------

  fn definition<S>(&mut self, definition: &TypeSystemDefinition<S>, built_in: bool)
  where
    S: AsRef<[u8]>,
  {
    match definition {
      TypeSystemDefinition::Type(ty) => self.type_definition(ty, built_in),
      TypeSystemDefinition::Directive(directive) => self.directive_definition(directive, built_in),
      TypeSystemDefinition::Schema(schema) => self.schema_definition(schema),
    }
  }

  fn type_definition<S>(&mut self, definition: &TypeDefinition<S>, built_in: bool)
  where
    S: AsRef<[u8]>,
  {
    let raw = match definition {
      TypeDefinition::Scalar(def) => self.scalar(def, built_in),
      TypeDefinition::Object(def) => self.object(def, built_in),
      TypeDefinition::Interface(def) => self.interface(def, built_in),
      TypeDefinition::Union(def) => self.union(def, built_in),
      TypeDefinition::Enum(def) => self.enumeration(def, built_in),
      TypeDefinition::InputObject(def) => self.input_object(def, built_in),
    };
    self.record_type(raw);
  }

  fn record_type(&mut self, raw: RawType) {
    let sym = raw.name.sym;
    if let Some(previous) = self.type_index(sym) {
      let related = self.types[previous].name.span;
      let subject = self.text(sym).to_owned();
      self.push_related(
        SchemaErrorKind::DuplicateTypeName,
        &subject,
        None,
        raw.name,
        related,
      );
      return;
    }
    let index = self.types.len() as u32;
    self.set_type_index(sym, index);
    self.types.push(raw);
  }

  fn type_index(&self, sym: Sym) -> Option<usize> {
    match self.type_of_sym.get(sym.get() as usize) {
      Some(&index) if index != u32::MAX => Some(index as usize),
      _ => None,
    }
  }

  fn set_type_index(&mut self, sym: Sym, index: u32) {
    let slot = sym.get() as usize;
    if self.type_of_sym.len() <= slot {
      self.type_of_sym.resize(slot + 1, u32::MAX);
    }
    self.type_of_sym[slot] = index;
  }

  fn directive_index(&self, sym: Sym) -> Option<usize> {
    match self.directive_of_sym.get(sym.get() as usize) {
      Some(&index) if index != u32::MAX => Some(index as usize),
      _ => None,
    }
  }

  fn set_directive_index(&mut self, sym: Sym, index: u32) {
    let slot = sym.get() as usize;
    if self.directive_of_sym.len() <= slot {
      self.directive_of_sym.resize(slot + 1, u32::MAX);
    }
    self.directive_of_sym[slot] = index;
  }

  fn scalar<S>(&mut self, def: &ScalarTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::Scalar, built_in);
    raw.directives = self.directive_uses(def.directives());
    raw
  }

  fn object<S>(&mut self, def: &ObjectTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::Object, built_in);
    raw.implements = self.implements(def.implements());
    raw.directives = self.directive_uses(def.directives());
    raw.fields = self.fields(def.fields_definition());
    raw
  }

  fn interface<S>(&mut self, def: &InterfaceTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::Interface, built_in);
    raw.implements = self.implements(def.implements());
    raw.directives = self.directive_uses(def.directives());
    raw.fields = self.fields(def.fields_definition());
    raw
  }

  fn union<S>(&mut self, def: &UnionTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::Union, built_in);
    raw.directives = self.directive_uses(def.directives());
    raw.members = self.members(def.member_types());
    raw
  }

  fn enumeration<S>(&mut self, def: &EnumTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::Enum, built_in);
    raw.directives = self.directive_uses(def.directives());
    raw.enum_values = self.enum_values(def.enum_values_definition());
    raw
  }

  fn input_object<S>(&mut self, def: &InputObjectTypeDefinition<S>, built_in: bool) -> RawType
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let mut raw = RawType::new(name, TypeKind::InputObject, built_in);
    raw.directives = self.directive_uses(def.directives());
    raw.input_fields = self.input_fields(def.fields_definition());
    raw
  }

  fn directive_definition<S>(&mut self, def: &DirectiveDefinition<S>, built_in: bool)
  where
    S: AsRef<[u8]>,
  {
    let name = self.located(def.name());
    let args = match def.arguments_definition() {
      Some(arguments) => self.input_values(arguments.input_value_definitions()),
      None => Vec::new(),
    };
    let mut locations = DirectiveLocations::EMPTY;
    for location in def.locations().locations() {
      if let Some(mapped) = map_location(location) {
        locations.insert(mapped);
      }
    }
    let repeatable = def.repeatable();

    if let Some(previous) = self.directive_index(name.sym) {
      let related = self.directives[previous].name.span;
      let subject = self.text(name.sym).to_owned();
      self.push_related(
        SchemaErrorKind::DuplicateDirectiveDefinition,
        &subject,
        None,
        name,
        related,
      );
      return;
    }
    let index = self.directives.len() as u32;
    self.set_directive_index(name.sym, index);
    self.directives.push(RawDirectiveDef {
      name,
      args,
      locations,
      repeatable,
      built_in,
    });
  }

  fn schema_definition<S>(&mut self, def: &SchemaDefinition<S>)
  where
    S: AsRef<[u8]>,
  {
    let span = *def.span();
    if let Some(previous) = self.schema_definition {
      self.errors.push(
        SchemaError::new(SchemaErrorKind::DuplicateSchemaDefinition, "schema", span)
          .in_document(self.document)
          .related_to(previous),
      );
      return;
    }
    self.schema_definition = Some(span);
    for root in def
      .root_operation_types_definition()
      .root_operation_type_definitions()
    {
      let operation = map_operation(root.operation_type());
      let target = self.located(root.name());
      self.set_root(operation, target);
    }
  }

  fn set_root(&mut self, operation: RootOperation, target: Located) {
    if let Some(previous) = self.roots[operation.index()] {
      let subject = operation.as_str();
      self.push_related(
        SchemaErrorKind::DuplicateRootOperationType,
        subject,
        None,
        target,
        previous.span,
      );
      return;
    }
    self.roots[operation.index()] = Some(target);
  }

  fn implements<S, C>(
    &mut self,
    implements: Option<&ImplementInterfaces<Name<S>, C>>,
  ) -> Vec<Located>
  where
    S: AsRef<[u8]>,
    C: AsRef<[Name<S>]>,
  {
    match implements {
      None => Vec::new(),
      Some(list) => list
        .interfaces()
        .iter()
        .map(|name| {
          let bytes = name.source().as_ref().to_owned();
          let span = *name.as_span();
          let document = self.document;
          let sym = self.interner.intern(&bytes);
          Located {
            sym,
            span,
            document,
          }
        })
        .collect(),
    }
  }

  fn members<S, C>(&mut self, members: Option<&UnionMemberTypes<Name<S>, C>>) -> Vec<Located>
  where
    S: AsRef<[u8]>,
    C: AsRef<[Name<S>]>,
  {
    match members {
      None => Vec::new(),
      Some(list) => list
        .members()
        .iter()
        .map(|name| {
          let bytes = name.source().as_ref().to_owned();
          let span = *name.as_span();
          let document = self.document;
          let sym = self.interner.intern(&bytes);
          Located {
            sym,
            span,
            document,
          }
        })
        .collect(),
    }
  }

  fn directive_uses<S>(&mut self, directives: Option<&ConstDirectives<S>>) -> Vec<Located>
  where
    S: AsRef<[u8]>,
  {
    match directives {
      None => Vec::new(),
      Some(list) => list
        .directives()
        .iter()
        .map(|directive| self.located(directive.name()))
        .collect(),
    }
  }

  fn fields<S>(&mut self, fields: Option<&FieldsDefinition<S>>) -> Vec<RawField>
  where
    S: AsRef<[u8]>,
  {
    let Some(fields) = fields else {
      return Vec::new();
    };
    fields
      .field_definitions()
      .iter()
      .map(|field| {
        let name = self.located(field.name());
        let ty = self.type_ref(field.ty());
        let args = match field.arguments_definition() {
          Some(arguments) => self.input_values(arguments.input_value_definitions()),
          None => Vec::new(),
        };
        RawField { name, ty, args }
      })
      .collect()
  }

  fn input_fields<S>(&mut self, fields: Option<&InputFieldsDefinition<S>>) -> Vec<RawInput>
  where
    S: AsRef<[u8]>,
  {
    match fields {
      None => Vec::new(),
      Some(fields) => self.input_values(fields.input_value_definitions()),
    }
  }

  fn input_values<S>(&mut self, values: &[InputValueDefinition<S>]) -> Vec<RawInput>
  where
    S: AsRef<[u8]>,
  {
    values
      .iter()
      .map(|value| {
        let name = self.located(value.name());
        let ty = self.type_ref(value.ty());
        let default = match value.default_value() {
          None => DefaultKind::Absent,
          Some(default) => match default.value() {
            ConstInputValue::Null(_) => DefaultKind::Null,
            _ => DefaultKind::NonNull,
          },
        };
        let directives = self.directive_uses(value.directives());
        RawInput {
          name,
          ty,
          default,
          directives,
        }
      })
      .collect()
  }

  fn enum_values<S>(&mut self, values: Option<&EnumValuesDefinition<S>>) -> Vec<RawEnumValue>
  where
    S: AsRef<[u8]>,
  {
    let Some(values) = values else {
      return Vec::new();
    };
    values
      .enum_value_definitions()
      .iter()
      .map(|value| {
        let name = self.located(value.value());
        let directives = self.directive_uses(value.directives());
        RawEnumValue { name, directives }
      })
      .collect()
  }

  /// Flattens a type reference into a base name plus a wrapper word.
  fn type_ref<S>(&mut self, ty: &Type<Name<S>>) -> RawTypeRef
  where
    S: AsRef<[u8]>,
  {
    match ty {
      Type::Name(named) => {
        let base = self.located(named.name());
        let packed = PackedType::named(base.sym, UNRESOLVED);
        let (packed, too_deep) = if named.required() {
          match packed.push_non_null() {
            Some(packed) => (packed, false),
            None => (packed, true),
          }
        } else {
          (packed, false)
        };
        RawTypeRef {
          base,
          span: *named.span(),
          packed,
          too_deep,
        }
      }
      Type::List(list) => {
        let inner = self.type_ref(list.ty());
        let mut too_deep = inner.too_deep;
        let mut packed = inner.packed;
        match packed.push_list() {
          Some(next) => packed = next,
          None => too_deep = true,
        }
        if list.required() {
          match packed.push_non_null() {
            Some(next) => packed = next,
            None => too_deep = true,
          }
        }
        RawTypeRef {
          base: inner.base,
          span: *list.span(),
          packed,
          too_deep,
        }
      }
    }
  }

  fn extension<S>(&mut self, extension: &TypeSystemExtension<S>)
  where
    S: AsRef<[u8]>,
  {
    let mut raw = RawExtension::default();
    match extension {
      TypeSystemExtension::Schema(schema) => {
        raw.directives = self.directive_uses(schema.directives());
        if let Some(roots) = schema.root_operation_types_definition() {
          for root in roots.root_operation_type_definitions() {
            let operation = map_operation(root.operation_type());
            let target = self.located(root.name());
            raw.roots.push((operation, target));
          }
        }
      }
      TypeSystemExtension::Type(ty) => match ty {
        TypeExtension::Scalar(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::Scalar);
          raw.directives = self.directive_uses(Some(def.directives()));
        }
        TypeExtension::Object(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::Object);
          raw.implements = self.implements(def.implements());
          raw.directives = self.directive_uses(def.directives());
          raw.fields = self.fields(def.fields_definition());
        }
        TypeExtension::Interface(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::Interface);
          raw.implements = self.implements(def.implements());
          raw.directives = self.directive_uses(def.directives());
          raw.fields = self.fields(def.fields_definition());
        }
        TypeExtension::Union(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::Union);
          raw.directives = self.directive_uses(def.directives());
          raw.members = self.members(def.member_types());
        }
        TypeExtension::Enum(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::Enum);
          raw.directives = self.directive_uses(def.directives());
          raw.enum_values = self.enum_values(def.enum_values_definition());
        }
        TypeExtension::InputObject(def) => {
          raw.target = Some(self.located(def.name()));
          raw.kind = Some(TypeKind::InputObject);
          raw.directives = self.directive_uses(def.directives());
          raw.input_fields = self.input_fields(def.fields_definition());
        }
      },
    }
    self.extensions.push(raw);
  }

  fn apply_extensions(&mut self) {
    let extensions = core::mem::take(&mut self.extensions);
    for extension in extensions {
      let Some(target) = extension.target else {
        // `extend schema`.
        for (operation, located) in extension.roots {
          self.set_root(operation, located);
        }
        continue;
      };
      let Some(index) = self.type_index(target.sym) else {
        let subject = self.text(target.sym).to_owned();
        self.push(SchemaErrorKind::UndefinedExtensionTarget, &subject, target);
        continue;
      };
      if self.types[index].kind != extension.kind.unwrap_or(self.types[index].kind) {
        let subject = self.text(target.sym).to_owned();
        let related = self.types[index].name.span;
        self.push_related(
          SchemaErrorKind::ExtensionKindMismatch,
          &subject,
          None,
          target,
          related,
        );
        continue;
      }
      let raw = &mut self.types[index];
      raw.implements.extend(extension.implements);
      raw.fields.extend(extension.fields);
      raw.input_fields.extend(extension.input_fields);
      raw.members.extend(extension.members);
      raw.enum_values.extend(extension.enum_values);
      raw.directives.extend(extension.directives);
    }
  }

  // -- built-ins ------------------------------------------------------------------------------

  /// Parses one of the crate's own SDL constants.
  ///
  /// # Panics
  ///
  /// If a constant in [`builtin`] stops parsing, which is a defect in this crate rather than in
  /// any input. `builtin_sdl_parses` in the build test suite is the standing guard.
  fn parse_builtin(sdl: &'static str) -> TypeSystemDocument<&'static str> {
    Parser::with_parser::<
      GraphqlLexer<'static, str>,
      TypeSystemDocument<&'static str>,
      GraphqlErrors<&'static str>,
      _,
      GraphQL,
    >(type_system_document)
    .parse_str(sdl)
    .expect("smear's own built-in SDL must parse; this is a defect in smear, not in the input")
  }

  fn inject_built_ins(&mut self) {
    // Anything injected here belongs to no document the caller supplied; `self.document` is
    // already one past the last real index, which is what an injected definition reports under.

    // The introspection meta-schema is unconditional: an introspection query must validate
    // against every schema. A user definition of one of its types collides.
    let introspection = Self::parse_builtin(builtin::INTROSPECTION_SDL);
    for entry in introspection.definitions() {
      let TypeSystemDefinitionOrExtension::Definition(described) = entry else {
        continue;
      };
      let TypeSystemDefinition::Type(ty) = described.node() else {
        continue;
      };
      let name = type_definition_name(ty);
      match self.interner.lookup(name.as_bytes()).and_then(|sym| {
        self
          .type_index(sym)
          .map(|index| (sym, self.types[index].name))
      }) {
        Some((sym, at)) => {
          let subject = self.text(sym).to_owned();
          self.push(SchemaErrorKind::RedefinedBuiltInType, &subject, at);
          if let Some(index) = self.type_index(sym) {
            self.types[index].collides_with_built_in = true;
          }
        }
        None => self.type_definition(ty, true),
      }
    }

    // Built-in scalars and directives are replaceable: a document that spells one out keeps its
    // own, which is what makes a printed schema re-readable.
    let scalars = Self::parse_builtin(builtin::BUILT_IN_SCALARS_SDL);
    for entry in scalars.definitions() {
      let TypeSystemDefinitionOrExtension::Definition(described) = entry else {
        continue;
      };
      let TypeSystemDefinition::Type(ty) = described.node() else {
        continue;
      };
      let name = type_definition_name(ty);
      let taken = self
        .interner
        .lookup(name.as_bytes())
        .is_some_and(|sym| self.type_index(sym).is_some());
      if !taken {
        self.type_definition(ty, true);
      }
    }

    let directives = Self::parse_builtin(builtin::BUILT_IN_DIRECTIVES_SDL);
    for entry in directives.definitions() {
      let TypeSystemDefinitionOrExtension::Definition(described) = entry else {
        continue;
      };
      let TypeSystemDefinition::Directive(def) = described.node() else {
        continue;
      };
      let taken = self
        .interner
        .lookup(def.name().source().as_bytes())
        .is_some_and(|sym| self.directive_index(sym).is_some());
      if !taken {
        self.directive_definition(def, true);
      }
    }

    // The three meta-field names, interned now so flattening never has to grow the arena.
    self.interner.intern(builtin::TYPENAME_FIELD.as_bytes());
    self.interner.intern(builtin::SCHEMA_FIELD.as_bytes());
    self.interner.intern(builtin::TYPE_FIELD.as_bytes());
    self
      .interner
      .intern(builtin::TYPE_FIELD_ARGUMENT.as_bytes());
  }

  // -- validation (draft §3) --------------------------------------------------------------------

  fn validate(&mut self) {
    self.resolve_roots();
    self.resolve_type_refs();
    self.compute_closures();
    self.validate_types();
    self.validate_directive_definitions();
    self.validate_interface_implementations();
    self.validate_input_object_cycles();
    self.validate_directive_cycles();
  }

  fn resolve_roots(&mut self) {
    // With no `schema` definition, the roots default to the conventionally named object types.
    for operation in RootOperation::ALL {
      if self.roots[operation.index()].is_some() {
        continue;
      }
      let name = operation.default_type_name();
      if let Some(sym) = self.interner.lookup(name.as_bytes())
        && let Some(index) = self.type_index(sym)
        && self.types[index].kind == TypeKind::Object
      {
        self.roots[operation.index()] = Some(self.types[index].name);
      }
    }

    for operation in RootOperation::ALL {
      let Some(root) = self.roots[operation.index()] else {
        if operation == RootOperation::Query {
          self.errors.push(
            SchemaError::new(
              SchemaErrorKind::MissingQueryRootOperationType,
              "query",
              self.schema_definition.unwrap_or_default(),
            )
            .in_document(0),
          );
        }
        continue;
      };
      match self.type_index(root.sym) {
        None => {
          let subject = self.text(root.sym).to_owned();
          self.push(SchemaErrorKind::UndefinedRootOperationType, &subject, root);
          self.roots[operation.index()] = None;
        }
        Some(index) if self.types[index].kind != TypeKind::Object => {
          let subject = self.text(root.sym).to_owned();
          self.push(SchemaErrorKind::RootOperationTypeNotObject, &subject, root);
          self.roots[operation.index()] = None;
        }
        Some(_) => {}
      }
    }
  }

  /// Resolves every type reference's base to a type index, reporting the ones that name nothing.
  fn resolve_type_refs(&mut self) {
    let mut unresolved: Vec<(Located, String, SimpleSpan)> = Vec::new();
    let mut too_deep: Vec<(Located, String, SimpleSpan)> = Vec::new();

    for index in 0..self.types.len() {
      let owner = self.types[index].name.sym;
      let owner_name = self.text(owner).to_owned();

      for field in 0..self.types[index].fields.len() {
        let field_name = self
          .text(self.types[index].fields[field].name.sym)
          .to_owned();
        let path = owner_path(&[&owner_name, &field_name]);
        Self::resolve_one(
          &self.type_of_sym,
          &mut self.types[index].fields[field].ty,
          &path,
          &mut unresolved,
          &mut too_deep,
        );
        for arg in 0..self.types[index].fields[field].args.len() {
          let arg_name = self
            .text(self.types[index].fields[field].args[arg].name.sym)
            .to_owned();
          let path = owner_path(&[&owner_name, &field_name, &arg_name]);
          Self::resolve_one(
            &self.type_of_sym,
            &mut self.types[index].fields[field].args[arg].ty,
            &path,
            &mut unresolved,
            &mut too_deep,
          );
        }
      }

      for field in 0..self.types[index].input_fields.len() {
        let field_name = self
          .text(self.types[index].input_fields[field].name.sym)
          .to_owned();
        let path = owner_path(&[&owner_name, &field_name]);
        Self::resolve_one(
          &self.type_of_sym,
          &mut self.types[index].input_fields[field].ty,
          &path,
          &mut unresolved,
          &mut too_deep,
        );
      }
    }

    for index in 0..self.directives.len() {
      let owner_name = self.text(self.directives[index].name.sym).to_owned();
      for arg in 0..self.directives[index].args.len() {
        let arg_name = self
          .text(self.directives[index].args[arg].name.sym)
          .to_owned();
        let path = owner_path(&[&owner_name, &arg_name]);
        Self::resolve_one(
          &self.type_of_sym,
          &mut self.directives[index].args[arg].ty,
          &path,
          &mut unresolved,
          &mut too_deep,
        );
      }
    }

    for (at, path, _) in unresolved {
      let subject = self.text(at.sym).to_owned();
      self.push_owned(SchemaErrorKind::UndefinedType, &subject, path, at);
    }
    for (at, path, span) in too_deep {
      let subject = self.text(at.sym).to_owned();
      let mut at = at;
      at.span = span;
      self.push_owned(SchemaErrorKind::TypeReferenceTooDeep, &subject, path, at);
    }
  }

  fn resolve_one(
    type_of_sym: &[u32],
    reference: &mut RawTypeRef,
    path: &str,
    unresolved: &mut Vec<(Located, String, SimpleSpan)>,
    too_deep: &mut Vec<(Located, String, SimpleSpan)>,
  ) {
    if reference.too_deep {
      too_deep.push((reference.base, path.to_owned(), reference.span));
    }
    let slot = reference.base.sym.get() as usize;
    match type_of_sym.get(slot) {
      Some(&index) if index != u32::MAX => {
        reference.packed = PackedType::from_parts(
          reference.base.sym,
          TypeId::new(index),
          reference.packed.wrappers(),
        );
      }
      _ => unresolved.push((reference.base, path.to_owned(), reference.span)),
    }
  }

  /// Fills every type's interface closure, reporting interfaces that implement themselves.
  fn compute_closures(&mut self) {
    let count = self.types.len();
    for index in 0..count {
      if self.types[index].implements.is_empty() {
        continue;
      }
      let mut closure: Vec<u32> = Vec::new();
      let mut stack: Vec<u32> = Vec::new();
      for declared in &self.types[index].implements {
        if let Some(target) = self.type_index(declared.sym) {
          stack.push(target as u32);
        }
      }
      while let Some(next) = stack.pop() {
        if closure.contains(&next) {
          continue;
        }
        closure.push(next);
        for declared in &self.types[next as usize].implements {
          if let Some(target) = self.type_index(declared.sym) {
            stack.push(target as u32);
          }
        }
      }
      closure.sort_unstable();
      self.types[index].closure = closure;
    }

    for index in 0..count {
      if self.types[index].kind != TypeKind::Interface {
        continue;
      }
      if self.types[index].closure.contains(&(index as u32)) {
        let at = self.types[index].name;
        let subject = self.text(at.sym).to_owned();
        self.push(SchemaErrorKind::SelfImplementingInterface, &subject, at);
        // Break the cycle so nothing downstream walks it.
        self.types[index].closure.retain(|id| *id != index as u32);
      }
    }
  }

  fn validate_types(&mut self) {
    for index in 0..self.types.len() {
      let kind = self.types[index].kind;
      let built_in = self.types[index].built_in;
      let at = self.types[index].name;
      let name = self.text(at.sym).to_owned();

      if !built_in && !self.types[index].collides_with_built_in && is_reserved(name.as_bytes()) {
        self.push(SchemaErrorKind::ReservedTypeName, &name, at);
      }

      // `@oneOf` is read here rather than at ingest so an extension that adds it is seen.
      let one_of = self.has_directive(index, "oneOf");

      match kind {
        TypeKind::Scalar => {}
        TypeKind::Object | TypeKind::Interface => {
          self.validate_fields(index, &name);
          self.validate_implements(index, &name);
        }
        TypeKind::Union => self.validate_union(index, &name),
        TypeKind::Enum => self.validate_enum(index, &name),
        TypeKind::InputObject => self.validate_input_object(index, &name, one_of),
      }
    }
  }

  fn has_directive(&self, index: usize, name: &str) -> bool {
    self.types[index]
      .directives
      .iter()
      .any(|used| self.text(used.sym) == name)
  }

  fn validate_fields(&mut self, index: usize, owner: &str) {
    if self.types[index].fields.is_empty() {
      let at = self.types[index].name;
      self.push(SchemaErrorKind::EmptyFieldsDefinition, owner, at);
      return;
    }

    let mut seen: Vec<(Sym, SimpleSpan)> = Vec::new();
    for field in 0..self.types[index].fields.len() {
      let at = self.types[index].fields[field].name;
      let name = self.text(at.sym).to_owned();

      if let Some((_, first)) = seen.iter().find(|(sym, _)| *sym == at.sym).copied() {
        self.push_related(
          SchemaErrorKind::DuplicateFieldName,
          &name,
          Some(owner.to_owned()),
          at,
          first,
        );
      } else {
        seen.push((at.sym, at.span));
      }

      if !self.types[index].built_in && is_reserved(name.as_bytes()) {
        self.push_owned(
          SchemaErrorKind::ReservedFieldName,
          &name,
          owner.to_owned(),
          at,
        );
      }

      let base = self.types[index].fields[field].ty.packed.base_id();
      if base != UNRESOLVED && !self.types[base.get() as usize].kind.is_output() {
        let subject = self
          .text(self.types[base.get() as usize].name.sym)
          .to_owned();
        let path = owner_path(&[owner, &name]);
        let mut where_ = at;
        where_.span = self.types[index].fields[field].ty.span;
        self.push_owned(
          SchemaErrorKind::FieldTypeNotOutputType,
          &subject,
          path,
          where_,
        );
      }

      let path = owner_path(&[owner, &name]);
      let args = self.types[index].fields[field].args.clone();
      let built_in = self.types[index].built_in;
      self.validate_arguments(
        args,
        &path,
        built_in,
        SchemaErrorKind::DuplicateArgumentName,
        SchemaErrorKind::ReservedArgumentName,
        SchemaErrorKind::ArgumentTypeNotInputType,
      );
    }
  }

  fn validate_arguments(
    &mut self,
    args: Vec<RawInput>,
    owner: &str,
    built_in: bool,
    duplicate: SchemaErrorKind,
    reserved: SchemaErrorKind,
    not_input: SchemaErrorKind,
  ) {
    let mut seen: Vec<(Sym, SimpleSpan)> = Vec::new();
    for arg in &args {
      let at = arg.name;
      let name = self.text(at.sym).to_owned();

      if let Some((_, first)) = seen.iter().find(|(sym, _)| *sym == at.sym).copied() {
        self.push_related(duplicate, &name, Some(owner.to_owned()), at, first);
      } else {
        seen.push((at.sym, at.span));
      }

      if !built_in && is_reserved(name.as_bytes()) {
        self.push_owned(reserved, &name, owner.to_owned(), at);
      }

      let base = arg.ty.packed.base_id();
      if base != UNRESOLVED && !self.types[base.get() as usize].kind.is_input() {
        let subject = self
          .text(self.types[base.get() as usize].name.sym)
          .to_owned();
        let path = owner_path(&[owner, &name]);
        let mut where_ = at;
        where_.span = arg.ty.span;
        self.push_owned(not_input, &subject, path, where_);
      }
    }
  }

  fn validate_implements(&mut self, index: usize, owner: &str) {
    let implements = self.types[index].implements.clone();
    let mut seen: Vec<(Sym, SimpleSpan)> = Vec::new();
    for declared in implements {
      let name = self.text(declared.sym).to_owned();
      if let Some((_, first)) = seen.iter().find(|(sym, _)| *sym == declared.sym).copied() {
        self.push_related(
          SchemaErrorKind::DuplicateImplementsInterface,
          &name,
          Some(owner.to_owned()),
          declared,
          first,
        );
        continue;
      }
      seen.push((declared.sym, declared.span));

      match self.type_index(declared.sym) {
        None => self.push_owned(
          SchemaErrorKind::UndefinedImplementsInterface,
          &name,
          owner.to_owned(),
          declared,
        ),
        Some(target) if self.types[target].kind != TypeKind::Interface => self.push_owned(
          SchemaErrorKind::ImplementsNonInterface,
          &name,
          owner.to_owned(),
          declared,
        ),
        Some(_) => {}
      }
    }
  }

  fn validate_union(&mut self, index: usize, owner: &str) {
    if self.types[index].members.is_empty() {
      let at = self.types[index].name;
      self.push(SchemaErrorKind::EmptyUnionMembers, owner, at);
      return;
    }
    let members = self.types[index].members.clone();
    let mut seen: Vec<(Sym, SimpleSpan)> = Vec::new();
    for member in members {
      let name = self.text(member.sym).to_owned();
      if let Some((_, first)) = seen.iter().find(|(sym, _)| *sym == member.sym).copied() {
        self.push_related(
          SchemaErrorKind::DuplicateUnionMember,
          &name,
          Some(owner.to_owned()),
          member,
          first,
        );
        continue;
      }
      seen.push((member.sym, member.span));

      match self.type_index(member.sym) {
        None => self.push_owned(
          SchemaErrorKind::UndefinedUnionMember,
          &name,
          owner.to_owned(),
          member,
        ),
        Some(target) if self.types[target].kind != TypeKind::Object => self.push_owned(
          SchemaErrorKind::UnionMemberNotObject,
          &name,
          owner.to_owned(),
          member,
        ),
        Some(_) => {}
      }
    }
  }

  fn validate_enum(&mut self, index: usize, owner: &str) {
    if self.types[index].enum_values.is_empty() {
      let at = self.types[index].name;
      self.push(SchemaErrorKind::EmptyEnumValues, owner, at);
      return;
    }
    let built_in = self.types[index].built_in;
    let values: Vec<Located> = self.types[index]
      .enum_values
      .iter()
      .map(|value| value.name)
      .collect();
    let mut seen: Vec<(Sym, SimpleSpan)> = Vec::new();
    for value in values {
      let name = self.text(value.sym).to_owned();
      if let Some((_, first)) = seen.iter().find(|(sym, _)| *sym == value.sym).copied() {
        self.push_related(
          SchemaErrorKind::DuplicateEnumValue,
          &name,
          Some(owner.to_owned()),
          value,
          first,
        );
      } else {
        seen.push((value.sym, value.span));
      }
      if !built_in && is_reserved(name.as_bytes()) {
        self.push_owned(
          SchemaErrorKind::ReservedEnumValueName,
          &name,
          owner.to_owned(),
          value,
        );
      }
    }
  }

  fn validate_input_object(&mut self, index: usize, owner: &str, one_of: bool) {
    if self.types[index].input_fields.is_empty() {
      let at = self.types[index].name;
      self.push(SchemaErrorKind::EmptyInputFields, owner, at);
      return;
    }
    let built_in = self.types[index].built_in;
    let fields = self.types[index].input_fields.clone();
    let mut seen: Vec<(Sym, SimpleSpan)> = Vec::new();
    for field in &fields {
      let at = field.name;
      let name = self.text(at.sym).to_owned();

      if let Some((_, first)) = seen.iter().find(|(sym, _)| *sym == at.sym).copied() {
        self.push_related(
          SchemaErrorKind::DuplicateInputFieldName,
          &name,
          Some(owner.to_owned()),
          at,
          first,
        );
      } else {
        seen.push((at.sym, at.span));
      }

      if !built_in && is_reserved(name.as_bytes()) {
        self.push_owned(
          SchemaErrorKind::ReservedInputFieldName,
          &name,
          owner.to_owned(),
          at,
        );
      }

      let base = field.ty.packed.base_id();
      if base != UNRESOLVED && !self.types[base.get() as usize].kind.is_input() {
        let subject = self
          .text(self.types[base.get() as usize].name.sym)
          .to_owned();
        let path = owner_path(&[owner, &name]);
        let mut where_ = at;
        where_.span = field.ty.span;
        self.push_owned(
          SchemaErrorKind::InputFieldTypeNotInputType,
          &subject,
          path,
          where_,
        );
      }

      if one_of {
        if field.ty.packed.is_non_null() {
          self.push_owned(
            SchemaErrorKind::OneOfFieldNotNullable,
            &name,
            owner.to_owned(),
            at,
          );
        }
        if field.default.is_present() {
          self.push_owned(
            SchemaErrorKind::OneOfFieldHasDefault,
            &name,
            owner.to_owned(),
            at,
          );
        }
      }
    }
  }

  fn validate_directive_definitions(&mut self) {
    for index in 0..self.directives.len() {
      let at = self.directives[index].name;
      let name = self.text(at.sym).to_owned();
      let built_in = self.directives[index].built_in;

      if !built_in && is_reserved(name.as_bytes()) {
        self.push(SchemaErrorKind::ReservedDirectiveName, &name, at);
      }

      let args = self.directives[index].args.clone();
      self.validate_arguments(
        args,
        &name,
        built_in,
        SchemaErrorKind::DuplicateDirectiveArgumentName,
        SchemaErrorKind::ReservedDirectiveArgumentName,
        SchemaErrorKind::DirectiveArgumentTypeNotInputType,
      );
    }
  }

  /// Draft §3.6.1/§3.7.1: transitivity, covariance of field types, invariance of argument types.
  fn validate_interface_implementations(&mut self) {
    for index in 0..self.types.len() {
      if !matches!(
        self.types[index].kind,
        TypeKind::Object | TypeKind::Interface
      ) {
        continue;
      }
      let owner = self.text(self.types[index].name.sym).to_owned();
      let declared: Vec<Located> = self.types[index].implements.clone();

      for entry in &declared {
        let Some(interface) = self.type_index(entry.sym) else {
          continue;
        };
        if self.types[interface].kind != TypeKind::Interface {
          continue;
        }

        // Transitivity: every interface the interface implements must also be declared here.
        let required: Vec<u32> = self.types[interface].closure.clone();
        for needed in required {
          if needed as usize == index {
            continue;
          }
          let is_declared = declared
            .iter()
            .any(|d| self.type_index(d.sym) == Some(needed as usize));
          if !is_declared {
            let subject = self.text(self.types[needed as usize].name.sym).to_owned();
            self.push_owned(
              SchemaErrorKind::MissingTransitiveInterface,
              &subject,
              owner.clone(),
              *entry,
            );
          }
        }

        // Field coverage, covariance, and argument invariance.
        let interface_fields = self.types[interface].fields.clone();
        for interface_field in &interface_fields {
          let field_name = self.text(interface_field.name.sym).to_owned();
          let Some(position) = self.types[index]
            .fields
            .iter()
            .position(|f| f.name.sym == interface_field.name.sym)
          else {
            self.push_owned(
              SchemaErrorKind::MissingInterfaceField,
              &field_name,
              owner.clone(),
              *entry,
            );
            continue;
          };
          let own = self.types[index].fields[position].clone();

          if !self.is_valid_implementation_type(own.ty.packed, interface_field.ty.packed) {
            let path = owner_path(&[&owner, &field_name]);
            let expected = self.render_type(interface_field.ty.packed);
            self.push_owned(
              SchemaErrorKind::InvalidInterfaceFieldType,
              &expected,
              path,
              own.name,
            );
          }

          for interface_arg in &interface_field.args {
            let arg_name = self.text(interface_arg.name.sym).to_owned();
            match own
              .args
              .iter()
              .find(|a| a.name.sym == interface_arg.name.sym)
            {
              None => {
                let path = owner_path(&[&owner, &field_name]);
                self.push_owned(
                  SchemaErrorKind::MissingInterfaceFieldArgument,
                  &arg_name,
                  path,
                  own.name,
                );
              }
              Some(own_arg) => {
                if own_arg.ty.packed != interface_arg.ty.packed {
                  let path = owner_path(&[&owner, &field_name, &arg_name]);
                  let expected = self.render_type(interface_arg.ty.packed);
                  self.push_owned(
                    SchemaErrorKind::InvalidInterfaceFieldArgumentType,
                    &expected,
                    path,
                    own_arg.name,
                  );
                }
              }
            }
          }

          for own_arg in &own.args {
            let declared_by_interface = interface_field
              .args
              .iter()
              .any(|a| a.name.sym == own_arg.name.sym);
            if declared_by_interface {
              continue;
            }
            if own_arg.ty.packed.is_non_null() && !own_arg.default.is_present() {
              let arg_name = self.text(own_arg.name.sym).to_owned();
              let path = owner_path(&[&owner, &field_name]);
              self.push_owned(
                SchemaErrorKind::UnexpectedRequiredArgument,
                &arg_name,
                path,
                own_arg.name,
              );
            }
          }
        }
      }
    }
  }

  /// Draft's `IsValidImplementationFieldType`.
  fn is_valid_implementation_type(&self, field: PackedType, implemented: PackedType) -> bool {
    if field.is_non_null() {
      return self.is_valid_implementation_type(field.nullable(), implemented.nullable());
    }
    if let (Some(item), Some(implemented_item)) = (field.list_item(), implemented.list_item()) {
      return self.is_valid_implementation_type(item, implemented_item);
    }
    if field == implemented {
      return true;
    }
    if field.wrappers() != implemented.wrappers() {
      return false;
    }
    self.is_sub_type(field.base_id(), implemented.base_id())
  }

  /// Whether `candidate` is one of `abstract_type`'s possible types, in the draft's wider sense:
  /// union membership, or the interface closure — which, unlike the runtime possible-object
  /// bitsets, includes interfaces implementing interfaces.
  fn is_sub_type(&self, candidate: TypeId, abstract_type: TypeId) -> bool {
    if candidate == UNRESOLVED || abstract_type == UNRESOLVED {
      return false;
    }
    let target = &self.types[abstract_type.get() as usize];
    match target.kind {
      TypeKind::Union => target
        .members
        .iter()
        .any(|member| self.type_index(member.sym) == Some(candidate.get() as usize)),
      TypeKind::Interface => self.types[candidate.get() as usize]
        .closure
        .contains(&abstract_type.get()),
      _ => false,
    }
  }

  fn render_type(&self, packed: PackedType) -> String {
    struct Rendered<'a>(PackedType, &'a str);
    impl core::fmt::Display for Rendered<'_> {
      fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.write(f, self.1)
      }
    }
    Rendered(packed, self.text(packed.base())).to_string()
  }

  /// Draft §3.10.1: an input-object cycle must have at least one nullable or list link.
  fn validate_input_object_cycles(&mut self) {
    let count = self.types.len();
    // 0 = unvisited, 1 = on the current path, 2 = settled.
    let mut colour = vec![0u8; count];
    let mut reported = vec![false; count];

    for start in 0..count {
      if self.types[start].kind != TypeKind::InputObject || colour[start] != 0 {
        continue;
      }
      // Explicit stack: (type, next field to examine).
      let mut stack: Vec<(usize, usize)> = vec![(start, 0)];
      colour[start] = 1;
      while let Some((current, cursor)) = stack.pop() {
        if cursor >= self.types[current].input_fields.len() {
          colour[current] = 2;
          continue;
        }
        stack.push((current, cursor + 1));
        let field = &self.types[current].input_fields[cursor];
        let packed = field.ty.packed;
        // The only link that cannot be broken by a `null` or an empty list is a bare `X!`.
        if packed.depth() != 1 || !packed.is_non_null() {
          continue;
        }
        let target = packed.base_id();
        if target == UNRESOLVED {
          continue;
        }
        let target = target.get() as usize;
        if self.types[target].kind != TypeKind::InputObject {
          continue;
        }
        if colour[target] == 1 {
          if !reported[current] {
            reported[current] = true;
            let owner = self.text(self.types[current].name.sym).to_owned();
            let name = self.text(field.name.sym).to_owned();
            let at = field.name;
            self.push_owned(SchemaErrorKind::CircularNonNullInputField, &name, owner, at);
          }
          continue;
        }
        if colour[target] == 0 {
          colour[target] = 1;
          stack.push((target, 0));
        }
      }
    }
  }

  /// Draft §3.13.1: a directive definition must not refer to itself, directly or indirectly.
  ///
  /// One reachability walk per directive, over two interleaved worklists — directives to expand
  /// and types to expand — with a visited bit each. Iterative on purpose: the type graph's depth
  /// is bounded by nothing but the document, and a recursive walk would put an SDL's input-object
  /// chain on the call stack.
  ///
  /// The cycle is only reported on the directive the walk *started* from. A directive `@b` that
  /// merely uses a self-referential `@a` is not itself self-referential, and blaming it would put
  /// the diagnostic on the wrong definition — the same refinement apollo-compiler makes.
  fn validate_directive_cycles(&mut self) {
    let directives = self.directives.len();
    if directives == 0 {
      return;
    }
    let types = self.types.len();
    let mut cyclic: Vec<usize> = Vec::new();

    for start in 0..directives {
      let mut seen_directives = vec![false; directives];
      let mut seen_types = vec![false; types];
      let mut pending_directives: Vec<usize> = Vec::new();
      let mut pending_types: Vec<usize> = Vec::new();

      // Seed with what the definition itself names: the directives applied to its arguments, and
      // the types those arguments accept.
      for arg in &self.directives[start].args {
        self.push_directive_uses(&arg.directives, &mut pending_directives);
        self.push_type(arg.ty.packed.base_id(), &mut pending_types);
      }

      let mut found = false;
      while !found {
        if let Some(next) = pending_directives.pop() {
          if next == start {
            found = true;
            break;
          }
          if seen_directives[next] {
            continue;
          }
          seen_directives[next] = true;
          for arg in &self.directives[next].args {
            self.push_directive_uses(&arg.directives, &mut pending_directives);
            self.push_type(arg.ty.packed.base_id(), &mut pending_types);
          }
          continue;
        }
        let Some(next) = pending_types.pop() else {
          break;
        };
        if seen_types[next] {
          continue;
        }
        seen_types[next] = true;
        let raw = &self.types[next];
        self.push_directive_uses(&raw.directives, &mut pending_directives);
        for field in &raw.input_fields {
          self.push_directive_uses(&field.directives, &mut pending_directives);
          self.push_type(field.ty.packed.base_id(), &mut pending_types);
        }
        for value in &raw.enum_values {
          self.push_directive_uses(&value.directives, &mut pending_directives);
        }
      }

      if found {
        cyclic.push(start);
      }
    }

    for index in cyclic {
      let at = self.directives[index].name;
      let name = self.text(at.sym).to_owned();
      self.push(SchemaErrorKind::SelfReferentialDirective, &name, at);
    }
  }

  fn push_directive_uses(&self, uses: &[Located], pending: &mut Vec<usize>) {
    for used in uses {
      if let Some(index) = self.directive_index(used.sym) {
        pending.push(index);
      }
    }
  }

  fn push_type(&self, base: TypeId, pending: &mut Vec<usize>) {
    if base != UNRESOLVED {
      pending.push(base.get() as usize);
    }
  }

  // -- flattening -------------------------------------------------------------------------------

  fn flatten(self) -> Result<Schema, SchemaErrors> {
    let Self {
      interner,
      types: raw_types,
      type_of_sym: raw_type_of_sym,
      directives: raw_directives,
      roots: raw_roots,
      ..
    } = self;

    let symbol_count = interner.len();
    // `raw_type_of_sym` is the builder's own symbol-to-index map; reusing it keeps flattening
    // linear instead of rescanning the type table for every member and root.
    let id_of = |sym: Sym| -> Option<TypeId> {
      match raw_type_of_sym.get(sym.get() as usize) {
        Some(&index) if index != u32::MAX => Some(TypeId::new(index)),
        _ => None,
      }
    };
    if symbol_count > MAX_SYMBOLS {
      return Err(SchemaErrors::new(vec![SchemaError::new(
        SchemaErrorKind::TooManyNames,
        "schema",
        SimpleSpan::default(),
      )]));
    }

    // Object ordinals, in type-id order.
    let mut objects: Vec<TypeId> = Vec::new();
    let mut ordinal_of: Vec<u32> = vec![TypeDef::NONE; raw_types.len()];
    for (index, raw) in raw_types.iter().enumerate() {
      if raw.kind == TypeKind::Object {
        ordinal_of[index] = objects.len() as u32;
        objects.push(TypeId::new(index as u32));
      }
    }
    let possible_words = objects.len().div_ceil(64).max(1) as u32;

    let string_id = interner
      .lookup(b"String")
      .and_then(id_of)
      .unwrap_or(UNRESOLVED);
    let schema_type_id = interner
      .lookup(b"__Schema")
      .and_then(id_of)
      .unwrap_or(UNRESOLVED);
    let type_type_id = interner
      .lookup(b"__Type")
      .and_then(id_of)
      .unwrap_or(UNRESOLVED);
    let typename_sym = interner.lookup(builtin::TYPENAME_FIELD.as_bytes());
    let schema_field_sym = interner.lookup(builtin::SCHEMA_FIELD.as_bytes());
    let type_field_sym = interner.lookup(builtin::TYPE_FIELD.as_bytes());
    let type_arg_sym = interner.lookup(builtin::TYPE_FIELD_ARGUMENT.as_bytes());

    let query_root = raw_roots[RootOperation::Query.index()]
      .and_then(|root| id_of(root.sym))
      .unwrap_or(UNRESOLVED);

    let mut types: Vec<TypeDef> = Vec::with_capacity(raw_types.len());
    let mut fields: Vec<FieldDef> = Vec::new();
    let mut inputs: Vec<InputValueDef> = Vec::new();
    let mut interfaces: Vec<TypeId> = Vec::new();
    let mut members: Vec<TypeId> = Vec::new();
    let mut enum_values: Vec<Sym> = Vec::new();
    let mut possible: Vec<u64> = Vec::new();

    for (index, raw) in raw_types.iter().enumerate() {
      let id = TypeId::new(index as u32);

      let interfaces_range = {
        let start = interfaces.len() as u32;
        interfaces.extend(raw.closure.iter().map(|i| TypeId::new(*i)));
        Range32::new(start, interfaces.len() as u32)
      };

      let members_range = {
        let start = members.len() as u32;
        for member in &raw.members {
          if let Some(target) = id_of(member.sym) {
            members.push(target);
          }
        }
        Range32::new(start, members.len() as u32)
      };

      let enum_range = {
        let start = enum_values.len() as u32;
        let mut values: Vec<Sym> = raw.enum_values.iter().map(|value| value.name.sym).collect();
        values.sort_unstable();
        enum_values.extend(values);
        Range32::new(start, enum_values.len() as u32)
      };

      let fields_range = match raw.kind {
        TypeKind::Object | TypeKind::Interface | TypeKind::Union => {
          let mut rows: Vec<FieldDef> = Vec::with_capacity(raw.fields.len() + 3);
          for field in &raw.fields {
            let args_start = inputs.len() as u32;
            let mut args: Vec<InputValueDef> = field
              .args
              .iter()
              .map(|arg| InputValueDef::new(arg.name.sym, arg.ty.packed, arg.default))
              .collect();
            args.sort_unstable_by_key(|arg| arg.name().get());
            inputs.extend(args);
            let args_range = Range32::new(args_start, inputs.len() as u32);
            rows.push(FieldDef::new(field.name.sym, field.ty.packed, args_range));
          }

          // The meta-fields, on the types draft §4.4 puts them on.
          if let (Some(typename), true) = (typename_sym, string_id != UNRESOLVED)
            && !rows.iter().any(|row| row.name() == typename)
          {
            let ty = PackedType::named(raw_types[string_id.get() as usize].name.sym, string_id);
            let ty = ty.push_non_null().unwrap_or(ty);
            rows.push(FieldDef::new(typename, ty, Range32::EMPTY));
          }
          if id == query_root {
            if let (Some(name), true) = (schema_field_sym, schema_type_id != UNRESOLVED)
              && !rows.iter().any(|row| row.name() == name)
            {
              let ty = PackedType::named(
                raw_types[schema_type_id.get() as usize].name.sym,
                schema_type_id,
              );
              let ty = ty.push_non_null().unwrap_or(ty);
              rows.push(FieldDef::new(name, ty, Range32::EMPTY));
            }
            if let (Some(name), Some(arg), true) = (
              type_field_sym,
              type_arg_sym,
              type_type_id != UNRESOLVED && string_id != UNRESOLVED,
            ) && !rows.iter().any(|row| row.name() == name)
            {
              let args_start = inputs.len() as u32;
              let arg_ty =
                PackedType::named(raw_types[string_id.get() as usize].name.sym, string_id);
              let arg_ty = arg_ty.push_non_null().unwrap_or(arg_ty);
              inputs.push(InputValueDef::new(arg, arg_ty, DefaultKind::Absent));
              let args_range = Range32::new(args_start, inputs.len() as u32);
              let ty = PackedType::named(
                raw_types[type_type_id.get() as usize].name.sym,
                type_type_id,
              );
              rows.push(FieldDef::new(name, ty, args_range));
            }
          }

          rows.sort_unstable_by_key(|row| row.name().get());
          let start = fields.len() as u32;
          fields.extend(rows);
          Range32::new(start, fields.len() as u32)
        }
        TypeKind::InputObject => {
          let mut rows: Vec<InputValueDef> = raw
            .input_fields
            .iter()
            .map(|field| InputValueDef::new(field.name.sym, field.ty.packed, field.default))
            .collect();
          rows.sort_unstable_by_key(|row| row.name().get());
          let start = inputs.len() as u32;
          inputs.extend(rows);
          Range32::new(start, inputs.len() as u32)
        }
        _ => Range32::EMPTY,
      };

      let possible_start = if raw.kind.is_composite() {
        let start = possible.len() as u32;
        possible.resize(possible.len() + possible_words as usize, 0);
        match raw.kind {
          TypeKind::Object => {
            set_bit(&mut possible, start, ordinal_of[index]);
          }
          TypeKind::Union => {
            for member in &raw.members {
              if let Some(target) = id_of(member.sym) {
                let ordinal = ordinal_of[target.get() as usize];
                set_bit(&mut possible, start, ordinal);
              }
            }
          }
          TypeKind::Interface => {
            for (candidate, other) in raw_types.iter().enumerate() {
              if other.kind == TypeKind::Object && other.closure.contains(&(index as u32)) {
                set_bit(&mut possible, start, ordinal_of[candidate]);
              }
            }
          }
          _ => {}
        }
        start
      } else {
        TypeDef::NONE
      };

      let mut flags = TypeFlags::EMPTY;
      if raw.built_in {
        flags = flags.union(TypeFlags::BUILT_IN);
      }
      if raw
        .directives
        .iter()
        .any(|used| interner.text(used.sym) == "oneOf")
      {
        flags = flags.union(TypeFlags::ONE_OF);
      }

      types.push(TypeDef::new(
        raw.name.sym,
        raw.kind,
        flags,
        fields_range,
        interfaces_range,
        members_range,
        enum_range,
        possible_start,
        ordinal_of[index],
      ));
    }

    let mut directives: Vec<DirectiveDef> = Vec::with_capacity(raw_directives.len());
    for raw in &raw_directives {
      let args_start = inputs.len() as u32;
      let mut args: Vec<InputValueDef> = raw
        .args
        .iter()
        .map(|arg| InputValueDef::new(arg.name.sym, arg.ty.packed, arg.default))
        .collect();
      args.sort_unstable_by_key(|arg| arg.name().get());
      inputs.extend(args);
      let args_range = Range32::new(args_start, inputs.len() as u32);
      directives.push(DirectiveDef::new(
        raw.name.sym,
        raw.locations,
        args_range,
        raw.repeatable,
        raw.built_in,
      ));
    }
    directives.sort_unstable_by_key(|directive| directive.name().get());

    let mut type_of_sym = vec![u32::MAX; symbol_count as usize];
    for (index, raw) in raw_types.iter().enumerate() {
      type_of_sym[raw.name.sym.get() as usize] = index as u32;
    }

    let Interner {
      strings,
      spans,
      map: _,
    } = interner;
    let strings = strings.into_boxed_slice();
    let spans = spans.into_boxed_slice();
    let index = {
      let spans_ref = &spans;
      let strings_ref = &strings;
      NameIndex::build(symbol_count, |sym| {
        let (start, end) = spans_ref[sym as usize];
        &strings_ref[start as usize..end as usize]
      })
    };
    let Some(index) = index else {
      return Err(SchemaErrors::new(vec![SchemaError::new(
        SchemaErrorKind::TooManyNames,
        "schema",
        SimpleSpan::default(),
      )]));
    };

    let roots = [
      raw_roots[0].and_then(|root| id_of(root.sym)),
      raw_roots[1].and_then(|root| id_of(root.sym)),
      raw_roots[2].and_then(|root| id_of(root.sym)),
    ];

    Ok(Schema {
      strings,
      spans,
      index,
      types: types.into_boxed_slice(),
      type_of_sym: type_of_sym.into_boxed_slice(),
      fields: fields.into_boxed_slice(),
      inputs: inputs.into_boxed_slice(),
      interfaces: interfaces.into_boxed_slice(),
      members: members.into_boxed_slice(),
      enum_values: enum_values.into_boxed_slice(),
      possible: possible.into_boxed_slice(),
      possible_words,
      objects: objects.into_boxed_slice(),
      directives: directives.into_boxed_slice(),
      roots,
    })
  }
}

fn set_bit(words: &mut [u64], start: u32, ordinal: u32) {
  if ordinal == TypeDef::NONE {
    return;
  }
  let word = start as usize + (ordinal / 64) as usize;
  if let Some(slot) = words.get_mut(word) {
    *slot |= 1u64 << (ordinal % 64);
  }
}

fn map_operation(operation: &OperationType) -> RootOperation {
  match operation {
    OperationType::Query(_) => RootOperation::Query,
    OperationType::Mutation(_) => RootOperation::Mutation,
    _ => RootOperation::Subscription,
  }
}

fn map_location(location: &Location) -> Option<DirectiveLocation> {
  DirectiveLocation::from_name(location.as_str())
}

/// The name of a type definition, as a `&str` over the built-in SDL's `&'static str` source.
///
/// Used for both the introspection types and the built-in scalars; the two differ in what the
/// caller does when the name is already taken, not in how it is read.
fn type_definition_name(definition: &TypeDefinition<&'static str>) -> &'static str {
  match definition {
    TypeDefinition::Scalar(def) => def.name().source(),
    TypeDefinition::Object(def) => def.name().source(),
    TypeDefinition::Interface(def) => def.name().source(),
    TypeDefinition::Union(def) => def.name().source(),
    TypeDefinition::Enum(def) => def.name().source(),
    TypeDefinition::InputObject(def) => def.name().source(),
  }
}

impl Schema {
  /// Builds a schema from one type-system document.
  ///
  /// Draft §3 "Type Validation" runs here: a document that is not a valid schema comes back as
  /// [`SchemaErrors`] rather than as a `Schema` nobody checked.
  ///
  /// Use [`SchemaBuilder`] directly when the schema spans several documents.
  #[inline]
  pub fn build<S>(document: &TypeSystemDocument<S>) -> Result<Self, SchemaErrors>
  where
    S: AsRef<[u8]>,
  {
    let mut builder = SchemaBuilder::new();
    builder.document(document);
    builder.finish()
  }
}
