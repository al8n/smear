//! The written widths: a syntactic pre-pass that finds every intrinsic primitive a program writes,
//! so that `core` materialises those and no other.
//!
//! A primitive's definition comes first in `core`'s root module, before any other definition has
//! an id, so which primitives exist is decided before any name is resolved: by spelling alone.
//! The last name of a path in a type position, of a value path, and of the item a `use` binds is
//! read as [`written`](super::builtin::written) reads it, so `i32`, `Int` and `NonZeroI32` each
//! write `i32`. A spelling that a declaration of the program shadows still writes its primitive:
//! the pass runs before any name means anything, and one unused definition in `core` is cheaper
//! than a second rule for ordering ids. A width the writing package may not have is not written
//! by it: name resolution reports such a spelling where it is used.
//!
//! Each primitive a package writes first is also noted with where it is written, in the order
//! written, so that the walk counts its definition against `Limits::max_definitions` when it is
//! made, and takes it back when the limit refuses it.
//!
//! Every walk here is written once, generic over a document's slice type, and nested types,
//! values and `use` groups are walked with explicit stacks.

use std::{
  collections::{BTreeMap, btree_map::Entry},
  vec,
  vec::Vec,
};

use smear_parser::graphqlx::ast::{
  ConstDirectives, ConstInputValue, DefinitionName, FieldDefinition, ImplementInterfaces,
  InputValueDefinition, Name, Path, Type, TypeDefinition, TypeExtension, TypePath,
  TypeSystemDefinition, TypeSystemExtension, UseDefinition, UsePrefix, UseTree, UseTreeKind,
  WhereClause,
};
use tokora::{diagnostic::Location, span::AsSpan};

use super::{
  builtin::{available, written},
  model::Primitive,
  package::IntegerProvider,
};

/// What the pre-pass has found so far, and the package and the source it is reading.
pub(crate) struct Widths<'p> {
  /// The integer provider of the package being read.
  integers: Option<&'p IntegerProvider>,
  /// The source being read, where what it writes is written.
  source: u32,
  /// Every primitive written where it is available, keyed by its place among `core`'s
  /// materialised definitions: the key is the order the ids take, and two primitives share it
  /// only when they are one.
  written: BTreeMap<(u8, u32), Primitive>,
  /// The primitives written first since the walk last took them, in the order written, each with
  /// where it is written.
  fresh: Vec<(Primitive, Location)>,
  /// How many nodes of a kind it does not know the pass met.
  unknown: u32,
}

impl<'p> Widths<'p> {
  /// A pass that has found nothing.
  pub(crate) fn new() -> Self {
    Self {
      integers: None,
      source: 0,
      written: BTreeMap::new(),
      fresh: Vec::new(),
      unknown: 0,
    }
  }

  /// Reads the next items as the package whose integer provider is `integers` writes them.
  pub(crate) fn package(&mut self, integers: Option<&'p IntegerProvider>) {
    self.integers = integers;
  }

  /// Reads the next items as the source `source` writes them.
  pub(crate) fn source(&mut self, source: u32) {
    self.source = source;
  }

  /// The primitives written first since the last call, in the order written, each with where it is
  /// written: the definitions `core` is to make for them.
  pub(crate) fn fresh(&mut self) -> Vec<(Primitive, Location)> {
    core::mem::take(&mut self.fresh)
  }

  /// Takes back `primitive`, written first where the model may hold no more definitions: `core`
  /// makes no definition for it.
  pub(crate) fn unwrite(&mut self, primitive: Primitive) {
    self.written.remove(&primitive.rank());
  }

  /// Whether `primitive` is written, or made, already.
  pub(crate) fn wrote(&self, primitive: Primitive) -> bool {
    self.written.contains_key(&primitive.rank())
  }

  /// Makes `primitive`, which a root package's definition replaces whether or not a package writes
  /// it.
  pub(crate) fn make(&mut self, primitive: Primitive) {
    self.written.insert(primitive.rank(), primitive);
  }

  /// How many nodes of a kind it does not know the pass has met so far.
  pub(crate) fn unknown(&self) -> u32 {
    self.unknown
  }

  /// Every primitive written, keyed by its place among `core`'s materialised definitions, and how
  /// many nodes of an unknown kind the pass met.
  pub(crate) fn finish(self) -> (BTreeMap<(u8, u32), Primitive>, u32) {
    (self.written, self.unknown)
  }

  /// Reads a type-system definition: its generic defaults, `implements`, `where` clause,
  /// directives, members and root operations.
  pub(crate) fn definition<S: AsRef<[u8]>>(&mut self, definition: &TypeSystemDefinition<S>) {
    match definition {
      TypeSystemDefinition::Type(ty) => match ty {
        TypeDefinition::Scalar(scalar) => {
          self.name(scalar.name());
          self.implements(scalar.implements());
          self.directives(scalar.directives());
          self.clause(scalar.where_clause());
        }
        TypeDefinition::Object(object) => {
          self.name(object.name());
          self.implements(object.implements());
          self.directives(object.directives());
          if let Some(fields) = object.fields_definition() {
            self.clause(fields.where_clause());
            self.fields(fields.target().field_definitions());
          }
        }
        TypeDefinition::Interface(interface) => {
          self.name(interface.name());
          self.implements(interface.implements());
          self.directives(interface.directives());
          if let Some(fields) = interface.fields_definition() {
            self.clause(fields.where_clause());
            self.fields(fields.target().field_definitions());
          }
        }
        TypeDefinition::Union(union) => {
          self.name(union.name());
          self.implements(union.implements());
          self.directives(union.directives());
          if let Some(members) = union.member_types() {
            self.clause(members.where_clause());
            for member in members.target().members() {
              self.type_path(member);
            }
          }
        }
        TypeDefinition::Enum(enumeration) => {
          self.name(enumeration.name());
          self.implements(enumeration.implements());
          self.directives(enumeration.directives());
          if let Some(values) = enumeration.enum_values_definition() {
            for value in values.enum_value_definitions() {
              self.directives(value.directives());
            }
          }
        }
        TypeDefinition::InputObject(input) => {
          self.name(input.name());
          self.implements(input.implements());
          self.directives(input.directives());
          if let Some(fields) = input.fields_definition() {
            self.clause(fields.where_clause());
            self.input_values(fields.target().input_value_definitions());
          }
        }
        // `TypeDefinition` is `#[non_exhaustive]`: a kind this pass does not know may write a
        // width it cannot see. It is counted, and the walk refuses the count in a debug build.
        _ => self.unknown = self.unknown.saturating_add(1),
      },
      TypeSystemDefinition::Directive(directive) => {
        self.name(directive.name());
        if let Some(arguments) = directive.arguments_definition() {
          self.input_values(arguments.input_value_definitions());
        }
        self.directives(directive.directives());
        self.clause(directive.locations().where_clause());
      }
      TypeSystemDefinition::Schema(schema) => {
        self.directives(schema.directives());
        for root in schema
          .root_operation_types_definition()
          .root_operation_type_definitions()
        {
          self.type_path(root.name());
        }
      }
    }
  }

  /// Reads a type-system extension: its target's path, and what it adds.
  pub(crate) fn extension<S: AsRef<[u8]>>(&mut self, extension: &TypeSystemExtension<S>) {
    match extension {
      TypeSystemExtension::Type(ty) => match ty {
        TypeExtension::Scalar(scalar) => {
          self.path(scalar.name().path());
          self.implements(scalar.implements());
          self.directives(scalar.directives());
        }
        TypeExtension::Object(object) => {
          self.path(object.name().path());
          self.implements(object.implements());
          self.directives(object.directives());
          if let Some(fields) = object.fields_definition() {
            self.clause(fields.where_clause());
            self.fields(fields.target().field_definitions());
          }
        }
        TypeExtension::Interface(interface) => {
          self.path(interface.name().path());
          self.implements(interface.implements());
          self.directives(interface.directives());
          if let Some(fields) = interface.fields_definition() {
            self.clause(fields.where_clause());
            self.fields(fields.target().field_definitions());
          }
        }
        TypeExtension::Union(union) => {
          self.path(union.name().path());
          self.implements(union.implements());
          self.directives(union.directives());
          if let Some(members) = union.member_types() {
            self.clause(members.where_clause());
            for member in members.target().members() {
              self.type_path(member);
            }
          }
        }
        TypeExtension::Enum(enumeration) => {
          self.path(enumeration.name().path());
          self.implements(enumeration.implements());
          self.directives(enumeration.directives());
          if let Some(values) = enumeration.enum_values_definition() {
            for value in values.enum_value_definitions() {
              self.directives(value.directives());
            }
          }
        }
        TypeExtension::InputObject(input) => {
          self.path(input.name().path());
          self.implements(input.implements());
          self.directives(input.directives());
          if let Some(fields) = input.fields_definition() {
            self.clause(fields.where_clause());
            self.input_values(fields.target().input_value_definitions());
          }
        }
      },
      TypeSystemExtension::Directive(directive) => {
        self.directives(Some(directive.directives()));
      }
      TypeSystemExtension::Schema(schema) => {
        self.directives(schema.directives());
        if let Some(roots) = schema.root_operation_types_definition() {
          for root in roots.root_operation_type_definitions() {
            self.type_path(root.name());
          }
        }
      }
    }
  }

  /// Reads a `use` item: the last name of every path it binds an item by.
  pub(crate) fn use_item<S: AsRef<[u8]>>(&mut self, item: &UseDefinition<S>) {
    let mut stack: Vec<&UseTree<S>> = vec![item.tree()];
    while let Some(tree) = stack.pop() {
      match tree.kind() {
        UseTreeKind::Simple { .. } => match tree.prefix() {
          UsePrefix::Path(path) => self.path(path),
          UsePrefix::Source { segments, .. } => {
            if let Some(last) = segments.last() {
              self.spelling(last);
            }
          }
          // A bare root binds no item of its own.
          UsePrefix::Root(_) => {}
          // `UsePrefix` is `#[non_exhaustive]`: counted, as an unknown definition is.
          _ => self.unknown = self.unknown.saturating_add(1),
        },
        UseTreeKind::Group(trees) => stack.extend(trees.as_slice()),
        // A glob binds the names its module has, each written where it is used.
        UseTreeKind::Glob => {}
        // `UseTreeKind` is `#[non_exhaustive]`: counted, as an unknown definition is.
        _ => self.unknown = self.unknown.saturating_add(1),
      }
    }
  }

  /// Notes the primitive `name` writes, when the package being read may have it; and, when no item
  /// wrote it before, where it is written first.
  fn spelling<S: AsRef<[u8]>>(&mut self, name: &Name<S>) {
    if let Some(primitive) = written(name.source().as_ref())
      && available(primitive, self.integers)
      && let Entry::Vacant(slot) = self.written.entry(primitive.rank())
    {
      slot.insert(primitive);
      let at = Location::new(self.source, *name.as_span());
      self.fresh.push((primitive, at));
    }
  }

  /// Reads a path: its last name is the item it names.
  fn path<S: AsRef<[u8]>>(&mut self, path: &Path<S>) {
    if let Some(last) = path.segments().last() {
      self.spelling(last);
    }
  }

  /// Reads a path with type arguments: an `implements` entry, a bound, a union member, a root
  /// operation, or a directive's name.
  fn type_path<S: AsRef<[u8]>>(&mut self, path: &TypePath<S>) {
    self.path(path.path());
    if let Some(arguments) = path.type_generics() {
      for argument in arguments.params() {
        self.ty(argument);
      }
    }
  }

  /// Reads a type and every type it nests.
  fn ty<S: AsRef<[u8]>>(&mut self, ty: &Type<S>) {
    let mut stack: Vec<&Type<S>> = vec![ty];
    while let Some(ty) = stack.pop() {
      match ty {
        Type::Path(path) => {
          self.path(path.path());
          if let Some(arguments) = path.type_generics() {
            stack.extend(arguments.params());
          }
        }
        Type::List(list) => stack.push(list.ty()),
      }
    }
  }

  /// Reads a constant value and every value it holds: an enum value's path may name a type, as a
  /// `TypeRef` argument's does.
  fn value<S: AsRef<[u8]>>(&mut self, value: &ConstInputValue<S>) {
    let mut stack: Vec<&ConstInputValue<S>> = vec![value];
    while let Some(value) = stack.pop() {
      match value {
        ConstInputValue::Enum(path) => self.path(path.source()),
        ConstInputValue::List(list) => stack.extend(list.values()),
        ConstInputValue::Set(set) => stack.extend(set.values()),
        ConstInputValue::Map(map) => {
          for entry in map.entries() {
            stack.push(entry.key());
            stack.push(entry.value());
          }
        }
        ConstInputValue::Object(object) => {
          for field in object.fields() {
            stack.push(field.value());
          }
        }
        ConstInputValue::Boolean(_)
        | ConstInputValue::String(_)
        | ConstInputValue::Float(_)
        | ConstInputValue::Int(_)
        | ConstInputValue::Null(_) => {}
      }
    }
  }

  /// Reads a definition's name: its type parameters' defaults.
  fn name<S: AsRef<[u8]>>(&mut self, name: &DefinitionName<S>) {
    if let Some(generics) = name.generics() {
      for parameter in generics.params() {
        if let Some(default) = parameter.default() {
          self.ty(default);
        }
      }
    }
  }

  /// Reads an `implements` clause.
  fn implements<S: AsRef<[u8]>>(&mut self, implements: Option<&ImplementInterfaces<S>>) {
    if let Some(implements) = implements {
      for interface in implements.interfaces() {
        self.type_path(interface);
      }
    }
  }

  /// Reads a `where` clause: each bounded parameter and each bound.
  fn clause<S: AsRef<[u8]>>(&mut self, clause: Option<&WhereClause<S>>) {
    if let Some(clause) = clause {
      for predicate in clause.predicates() {
        self.type_path(predicate.bounded_type());
        for bound in predicate.bounds() {
          self.type_path(bound);
        }
      }
    }
  }

  /// Reads directive applications: each one's type arguments and argument values.
  fn directives<S: AsRef<[u8]>>(&mut self, directives: Option<&ConstDirectives<S>>) {
    if let Some(directives) = directives {
      for directive in directives.directives() {
        self.type_path(directive.name());
        if let Some(arguments) = directive.arguments() {
          for argument in arguments.arguments() {
            self.value(argument.value());
          }
        }
      }
    }
  }

  /// Reads fields: each one's arguments, type and directives.
  fn fields<S: AsRef<[u8]>>(&mut self, fields: &[FieldDefinition<S>]) {
    for field in fields {
      if let Some(arguments) = field.arguments_definition() {
        self.input_values(arguments.input_value_definitions());
      }
      self.ty(field.ty());
      self.directives(field.directives());
    }
  }

  /// Reads input values, an input object's fields or the arguments of a field or a directive:
  /// each one's type, default and directives.
  fn input_values<S: AsRef<[u8]>>(&mut self, values: &[InputValueDefinition<S>]) {
    for value in values {
      self.ty(value.ty());
      if let Some(default) = value.default_value() {
        self.value(default.value());
      }
      self.directives(value.directives());
    }
  }
}
