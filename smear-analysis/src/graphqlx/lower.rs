//! Lowering: what each declaration holds beyond its identity, written into the model.
//!
//! # Extensions
//!
//! Before the definitions, each extension finds its target: its path, resolved in the module the
//! extension is written in, in the type namespace, or in the directive namespace for `extend
//! directive`, with no generic parameter in scope. A target that names nothing is reported by name
//! resolution, where it is written. A target that names a definition of another kind than the
//! extension's keyword is reported as `extension-kind-mismatch`, at the target, with a label at the
//! definition. Either way the extension contributes nothing and nothing inside it is lowered: one
//! mistake, one diagnostic. Every other extension is a contribution of its target, after the
//! definition's own, by source id and position wherever each is written: named where its target
//! path is written, and spanning from its `extend` to its last token. The parameter names an
//! extension writes name its target's parameters by position, and are no rows of their own.
//!
//! # The definitions
//!
//! Lowering walks the definitions in `DefId` order and appends, for each, what holds no type
//! occurrence, value or application:
//!
//! 1. its contributions: its own, from where its declaration is written, named where its name is
//!    and spanning from its description, or its first keyword, to its last token; then each of its
//!    extensions. A primitive `core` materialises has the whole of `core`'s package source for its
//!    own, which writes nothing else;
//! 2. its type parameters, in written order, each named where its name is written;
//! 3. a directive definition's locations, in written order, and whether it is `repeatable`;
//! 4. its direct members, contribution by contribution and as written within each: an object's or
//!    an interface's fields, an input object's fields, an enum's values, a directive definition's
//!    arguments;
//! 5. then the arguments of its fields, field by field, each field's as written.
//!
//! So a definition's direct members come before every argument of its fields, which is the
//! `MemberId` order. A member is named where its name is written, and spans from its description,
//! or its first token, to its last token. A description is cooked as a definition's is: absent
//! when none is written, and empty when an empty one is. A name that is not UTF-8, which only a
//! document of bytes could hold, has each such sequence replaced by U+FFFD, as a definition's name
//! has. A directive definition's arguments are also kept by their directive and their names, the
//! first of each name, for the applications that name them.
//!
//! A kind of definition this lowering does not know, which only a parser newer than the analyzer
//! makes, is reported as `unknown-item`, once per file, and nothing of it is lowered.
//!
//! # The schema
//!
//! After every definition's contributions come the schema's: every schema definition any package
//! writes, by source id and position, then every schema extension, likewise, each marked as what
//! it is. A schema item's contribution is named where its first keyword is written, `schema` or
//! `extend`: the six bytes the item's node starts with, as the parser spans a schema definition
//! from its `schema` and a schema extension from its `extend`. It spans the whole item, a
//! description included. The schema is the root package's, and the first schema definition the
//! root package writes gives it its description; every other schema item writes nothing into it
//! and is a contribution alone, which is what the rules that refuse a second schema definition, or
//! a dependency's schema item, read.
//!
//! # The arenas
//!
//! The model addresses its names and its texts with `u32` offsets. When lowering ends, a spelling
//! the name arena refused, or a text arena longer than a `u32` addresses, is reported once, as
//! `resource-limit`, at the whole of the root package's package source: otherwise the model would
//! answer a wrong spelling with no diagnostic.
//!
//! # Generic once
//!
//! This is the last step that sees a parsed document, and so the last that is generic: a document
//! is `TypeSystemDocument<&str>`, or `TypeSystemDocument<&[u8]>` for a byte text that is not UTF-8,
//! never the caller's own text type. Each walk is written once, generic over the slice type, and
//! each item is dispatched on its document's parser where it is walked. Lowering copies every
//! spelling the model keeps, so the model it builds borrows nothing, and everything after it is
//! compiled once.

use std::{
  collections::{BTreeMap, BTreeSet},
  format,
  string::String,
  vec::Vec,
};

use smear_lexer::decode_string_value;
use smear_parser::graphqlx::ast::{
  self, DefinitionName, DescribedTypeSystemDefinition, EnumValueDefinition,
  ExecutableDirectiveLocation, FieldDefinition, InputValueDefinition, Name, Path, StringValue,
  TypeDefinition, TypeExtension, TypeSystemDefinition, TypeSystemDirectiveLocation,
  TypeSystemExtension, TypeSystemItem,
};
use tokora::{SimpleSpan, diagnostic::Location, span::AsSpan};

use super::{
  code::{DiagnosticKind, schema},
  collect::{Items, unknown_item},
  declare::{Declaration, Declarations, Entry, Item},
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{DefId, IdRange, MemberId, SourceId, Sym, dense},
  model::{DefKind, DirectiveLocation, MemberKind, ModelBuilder},
  resolve::{Position, Request, Resolved, Scopes, Wanted},
  set::ROOT,
  source::SourceMap,
};

/// The length of a schema item's first keyword, `schema` or `extend`.
const KEYWORD: usize = 6;

/// Lowers what `declarations` holds into `builder`, which holds every package, module and
/// definition already, resolving each path through `scopes` and reporting into `reporter`;
/// `sources` numbers the packages' sources.
pub(crate) fn lower(
  declarations: &Declarations<'_, '_>,
  scopes: &Scopes<'_>,
  sources: &SourceMap,
  builder: &mut ModelBuilder,
  reporter: &mut Reporter,
) {
  let mut lowering = Lowering {
    builder,
    reporter,
    scopes,
    text: String::new(),
    field_ids: Vec::new(),
    next_field: 0,
    arguments: BTreeMap::new(),
    reported_unknown: BTreeSet::new(),
  };
  let attached = lowering.targets(declarations);
  // `attached` is sorted by target, so each definition's extensions are the next run of it.
  let mut next = 0;
  for (index, declaration) in declarations.defs.iter().enumerate() {
    let def = DefId::new(dense(index));
    let end = next
      + attached[next..]
        .iter()
        .take_while(|extension: &&Attached| extension.target == def)
        .count();
    lowering.definition(declarations, def, declaration, &attached[next..end]);
    next = end;
  }
  let root = sources.package_sources(ROOT);
  lowering.schema(declarations, root);
  lowering.arenas(Location::entire(root.start()));
}

/// What lowering keeps while it writes: the builder and the reporter it writes into, the scopes it
/// resolves in, and what it reuses or gathers on the way.
struct Lowering<'l, 'x> {
  builder: &'l mut ModelBuilder,
  reporter: &'l mut Reporter,
  /// Every module's scope: what a path means where it is written.
  scopes: &'l Scopes<'x>,
  /// The description being cooked: one buffer for every description.
  text: String,
  /// The fields of the definition being lowered, in id order, whose arguments follow every one of
  /// its direct members: one buffer for every definition.
  field_ids: Vec<MemberId>,
  /// How many of `field_ids` have had their arguments appended.
  next_field: usize,
  /// Each directive definition's arguments, by the directive and the argument's name, the first of
  /// each name kept: the key is what a written argument of an application is looked up by.
  // Read by the lowering of directive applications, which needs name resolution; until then
  // nothing reads what it holds.
  #[allow(dead_code)]
  arguments: BTreeMap<(DefId, Sym), MemberId>,
  /// The sources an item of a kind lowering does not know was reported in, each once.
  reported_unknown: BTreeSet<u32>,
}

/// Where the members being lowered are written: the definition they belong to, the contribution
/// that writes them, and the source it is written in.
#[derive(Clone, Copy)]
struct Writer {
  def: DefId,
  contribution: u32,
  source: u32,
}

/// An extension that joins its target, as the definitions' walk takes it.
#[derive(Clone, Copy)]
struct Attached {
  /// The definition it extends.
  target: DefId,
  /// Where it is written.
  entry: Entry,
  /// Its target path.
  location: Location,
  /// The whole extension, from its `extend` to its last token.
  extent: Location,
}

/// What a walk of one contribution's item appends.
#[derive(Clone, Copy)]
enum Step {
  /// The definition's own type parameters, a directive definition's locations, and the item's
  /// direct members.
  Members,
  /// The arguments of the item's fields, which follow every contribution's direct members.
  Arguments,
}

/// A schema definition or extension, as the schema's contributions take it.
struct SchemaPart<'d> {
  /// Where it is.
  entry: Entry,
  /// Whether it is an extension: the schema's definitions come first.
  extension: bool,
  /// Its first keyword, `schema` or `extend`.
  location: Location,
  /// The whole item, a description included.
  extent: Location,
  /// A schema definition's description, as written, and where it is written.
  description: Option<(&'d [u8], Location)>,
}

impl Lowering<'_, '_> {
  /// Every extension that joins its target, by target, then source id, then position. Each
  /// target is resolved where its extension is written, in the hand-over's order; an extension
  /// whose target names nothing, which name resolution reports, or names a definition of another
  /// kind, which is reported here, joins nothing.
  fn targets(&mut self, declarations: &Declarations<'_, '_>) -> Vec<Attached> {
    let mut attached: Vec<Attached> = Vec::with_capacity(declarations.extensions.len());
    for entry in &declarations.extensions {
      let joined = match declarations.items.get(entry.item.source.get() as usize) {
        Some(Some(Items::Text(items))) => self.target(declarations, items, *entry),
        Some(Some(Items::Bytes(items))) => self.target(declarations, items, *entry),
        // An extension is an item of a source the walk read.
        Some(None) | None => None,
      };
      if let Some(joined) = joined {
        attached.push(joined);
      }
    }
    // A source's items are flattened in source order, so an item's index is its position.
    attached.sort_unstable_by_key(|extension: &Attached| {
      (
        extension.target,
        extension.entry.item.source,
        extension.entry.item.index,
      )
    });
    attached
  }

  /// The extension `entry` addresses among `items`, the items of its source, as it joins its
  /// target; `None` when its target names nothing or names a definition of another kind.
  fn target<S: AsRef<[u8]>>(
    &mut self,
    declarations: &Declarations<'_, '_>,
    items: &[&TypeSystemItem<S>],
    entry: Entry,
  ) -> Option<Attached> {
    let &item = items.get(entry.item.index as usize)?;
    let source = entry.item.source;
    let (keyword, name) = match item {
      TypeSystemItem::Extension(extension) => match extension {
        TypeSystemExtension::Type(ty) => match ty {
          TypeExtension::Scalar(scalar) => (DefKind::Scalar, scalar.name()),
          TypeExtension::Object(object) => (DefKind::Object, object.name()),
          TypeExtension::Interface(interface) => (DefKind::Interface, interface.name()),
          TypeExtension::Union(union) => (DefKind::Union, union.name()),
          TypeExtension::Enum(enumeration) => (DefKind::Enum, enumeration.name()),
          TypeExtension::InputObject(input) => (DefKind::InputObject, input.name()),
        },
        TypeSystemExtension::Directive(directive) => (DefKind::Directive, directive.name()),
        // A schema extension is the schema's.
        TypeSystemExtension::Schema(_) => return None,
      },
      // An entry of the extensions is an extension.
      TypeSystemItem::Definition(_) | TypeSystemItem::Use(_) | TypeSystemItem::Mod(_) => {
        return None;
      }
      // `TypeSystemItem` is `#[non_exhaustive]`: an item of a kind lowering does not know is
      // reported, and nothing of it is lowered.
      _ => {
        self.unknown(source.get(), *item.span());
        return None;
      }
    };
    let path = name.path();
    let wanted = if keyword == DefKind::Directive {
      Wanted::Directive
    } else {
      Wanted::Type
    };
    let request = Request {
      module: entry.module,
      source,
      wanted,
      position: Position::ExtensionTarget,
      params: &[],
    };
    let target = match self
      .scopes
      .resolve(&*self.builder, self.reporter, &request, path)
    {
      Resolved::Def(def) => def,
      // `NonZeroU32` and its kin name `core`'s `NonZero`.
      Resolved::NonZero { non_zero, .. } => non_zero,
      // Name resolution has reported a target that names nothing. A type or a directive is
      // wanted, which no module is, and no parameter is in scope.
      Resolved::Module(_) | Resolved::Param(_) | Resolved::Unresolved => return None,
    };
    let location = Location::new(source.get(), *path.span());
    let found = declarations.defs.get(target.get() as usize)?.kind;
    if found != keyword {
      let definition = self.builder.def_location(target);
      self.mismatch(keyword, found, path, location, definition);
      return None;
    }
    Some(Attached {
      target,
      entry,
      location,
      extent: Location::new(source.get(), *item.span()),
    })
  }

  /// Reports an extension whose keyword extends a definition of the kind `keyword`, while its
  /// target `path`, written at `at`, names a definition of the kind `found`, named at
  /// `definition`.
  fn mismatch<S: AsRef<[u8]>>(
    &mut self,
    keyword: DefKind,
    found: DefKind,
    path: &Path<S>,
    at: Location,
    definition: Location,
  ) {
    let name: &[u8] = match path.segments().last() {
      Some(last) => last.source().as_ref(),
      None => &[],
    };
    self.reporter.report(
      Diagnostic::builder(schema::EXTENSION_KIND_MISMATCH, at)
        .message(format!(
          "`extend {}` names `{}`, which is {}",
          keyword_of(keyword),
          Excerpt(name),
          noun_of(found)
        ))
        .primary_label("extension does not match the kind of the type it extends")
        .label(definition, "the definition it extends")
        .help("the `extend` keyword has to match the kind of the definition it extends.")
        .build(),
    );
  }

  /// The definition `def`, declared as `declaration`, which the extensions `attached` extend:
  /// its contributions, its own and then each extension's, and what each writes, every direct
  /// member before any field's arguments.
  fn definition(
    &mut self,
    declarations: &Declarations<'_, '_>,
    def: DefId,
    declaration: &Declaration,
    attached: &[Attached],
  ) {
    let own = self
      .builder
      .push_contribution(def, declaration.location, declaration.extent);
    self.field_ids.clear();
    self.next_field = 0;
    // A primitive `core` materialises writes nothing of its own: its extensions may.
    if let Some(item) = declaration.item {
      let at = Writer {
        def,
        contribution: own,
        source: item.source.get(),
      };
      self.walk(declarations, item, at, Step::Members);
    }
    for extension in attached {
      let contribution = self
        .builder
        .push_extension(def, extension.location, extension.extent);
      let at = Writer {
        def,
        contribution,
        source: extension.entry.item.source.get(),
      };
      self.walk(declarations, extension.entry.item, at, Step::Members);
    }
    if let Some(item) = declaration.item {
      let at = Writer {
        def,
        contribution: own,
        source: item.source.get(),
      };
      self.walk(declarations, item, at, Step::Arguments);
    }
    // Each extension is the contribution after the one before it.
    for (contribution, extension) in (own.saturating_add(1)..).zip(attached) {
      let at = Writer {
        def,
        contribution,
        source: extension.entry.item.source.get(),
      };
      self.walk(declarations, extension.entry.item, at, Step::Arguments);
    }
  }

  /// Walks `item`, which `at` writes, for `step`, as the parser that made its document read it.
  fn walk(&mut self, declarations: &Declarations<'_, '_>, item: Item, at: Writer, step: Step) {
    match declarations.items.get(item.source.get() as usize) {
      Some(Some(Items::Text(items))) => self.written(items, item.index, at, step),
      Some(Some(Items::Bytes(items))) => self.written(items, item.index, at, step),
      // A declaration's item is one of a source the walk read.
      Some(None) | None => {}
    }
  }

  /// Walks the item at `index` of `items`, the items of the source `at` writes in, for `step`.
  fn written<S: AsRef<[u8]>>(
    &mut self,
    items: &[&TypeSystemItem<S>],
    index: u32,
    at: Writer,
    step: Step,
  ) {
    let Some(&item) = items.get(index as usize) else {
      return;
    };
    match item {
      TypeSystemItem::Definition(described) => match step {
        Step::Members => self.members(at, described),
        Step::Arguments => self.arguments(at, described),
      },
      TypeSystemItem::Extension(extension) => match step {
        Step::Members => self.extension_members(at, extension),
        Step::Arguments => self.extension_arguments(at, extension),
      },
      // A contribution is written by a definition or an extension, and by no other kind of item.
      TypeSystemItem::Use(_) | TypeSystemItem::Mod(_) => {}
      // `TypeSystemItem` is `#[non_exhaustive]`: an item of a kind lowering does not know is
      // reported, and nothing of it is lowered.
      _ => self.unknown(at.source, *item.span()),
    }
  }

  /// What the definition `described`, which `at` writes, holds beyond its occurrences: its type
  /// parameters, a directive definition's locations and whether it is `repeatable`, and its direct
  /// members, in written order.
  fn members<S: AsRef<[u8]>>(&mut self, at: Writer, described: &DescribedTypeSystemDefinition<S>) {
    match described.node() {
      TypeSystemDefinition::Type(definition) => match definition {
        TypeDefinition::Scalar(scalar) => self.type_params(at, scalar.name()),
        TypeDefinition::Object(object) => {
          self.type_params(at, object.name());
          if let Some(fields) = object.fields_definition() {
            self.fields(at, fields.target().field_definitions());
          }
        }
        TypeDefinition::Interface(interface) => {
          self.type_params(at, interface.name());
          if let Some(fields) = interface.fields_definition() {
            self.fields(at, fields.target().field_definitions());
          }
        }
        TypeDefinition::Union(union) => self.type_params(at, union.name()),
        TypeDefinition::Enum(enumeration) => {
          self.type_params(at, enumeration.name());
          if let Some(values) = enumeration.enum_values_definition() {
            self.enum_values(at, values.enum_value_definitions());
          }
        }
        TypeDefinition::InputObject(input) => {
          self.type_params(at, input.name());
          if let Some(fields) = input.fields_definition() {
            self.input_values(
              at,
              MemberKind::InputField,
              fields.target().input_value_definitions(),
            );
          }
        }
        // `TypeDefinition` is `#[non_exhaustive]`: a kind of definition lowering does not know is
        // reported, and nothing of it is lowered.
        _ => self.unknown(at.source, *described.span()),
      },
      TypeSystemDefinition::Directive(directive) => {
        self.type_params(at, directive.name());
        for location in directive.locations().target().locations() {
          self
            .builder
            .push_directive_location(at.def, directive_location(location));
        }
        if directive.repeatable() {
          self.builder.set_repeatable(at.def);
        }
        if let Some(arguments) = directive.arguments_definition() {
          self.input_values(
            at,
            MemberKind::Argument,
            arguments.input_value_definitions(),
          );
        }
      }
      // A schema definition declares no definition.
      TypeSystemDefinition::Schema(_) => {}
    }
  }

  /// The direct members the extension `extension`, which `at` writes, adds, as written.
  fn extension_members<S: AsRef<[u8]>>(&mut self, at: Writer, extension: &TypeSystemExtension<S>) {
    // A directive extension adds applications alone, and a schema extension is the schema's.
    let TypeSystemExtension::Type(ty) = extension else {
      return;
    };
    match ty {
      TypeExtension::Object(object) => {
        if let Some(fields) = object.fields_definition() {
          self.fields(at, fields.target().field_definitions());
        }
      }
      TypeExtension::Interface(interface) => {
        if let Some(fields) = interface.fields_definition() {
          self.fields(at, fields.target().field_definitions());
        }
      }
      TypeExtension::Enum(enumeration) => {
        if let Some(values) = enumeration.enum_values_definition() {
          self.enum_values(at, values.enum_value_definitions());
        }
      }
      TypeExtension::InputObject(input) => {
        if let Some(fields) = input.fields_definition() {
          self.input_values(
            at,
            MemberKind::InputField,
            fields.target().input_value_definitions(),
          );
        }
      }
      TypeExtension::Scalar(_) | TypeExtension::Union(_) => {}
    }
  }

  /// The arguments of the fields the extension `extension`, which `at` writes, adds.
  fn extension_arguments<S: AsRef<[u8]>>(
    &mut self,
    at: Writer,
    extension: &TypeSystemExtension<S>,
  ) {
    let fields = match extension {
      TypeSystemExtension::Type(ty) => match ty {
        TypeExtension::Object(object) => object.fields_definition(),
        TypeExtension::Interface(interface) => interface.fields_definition(),
        TypeExtension::Scalar(_)
        | TypeExtension::Union(_)
        | TypeExtension::Enum(_)
        | TypeExtension::InputObject(_) => None,
      },
      TypeSystemExtension::Directive(_) | TypeSystemExtension::Schema(_) => None,
    };
    if let Some(fields) = fields {
      self.field_arguments(at, fields.target().field_definitions());
    }
  }

  /// The arguments of the fields of the definition `described`, which `at` writes.
  fn arguments<S: AsRef<[u8]>>(
    &mut self,
    at: Writer,
    described: &DescribedTypeSystemDefinition<S>,
  ) {
    let fields = match described.node() {
      TypeSystemDefinition::Type(definition) => match definition {
        TypeDefinition::Object(object) => object.fields_definition(),
        TypeDefinition::Interface(interface) => interface.fields_definition(),
        TypeDefinition::Scalar(_)
        | TypeDefinition::Union(_)
        | TypeDefinition::Enum(_)
        | TypeDefinition::InputObject(_) => None,
        // `TypeDefinition` is `#[non_exhaustive]`: reported as its members were, once per file.
        _ => {
          self.unknown(at.source, *described.span());
          None
        }
      },
      TypeSystemDefinition::Directive(_) | TypeSystemDefinition::Schema(_) => None,
    };
    if let Some(fields) = fields {
      self.field_arguments(at, fields.target().field_definitions());
    }
  }

  /// The arguments of `fields`, which `at` writes, field by field and each field's as written:
  /// each field is the next of the ids its definition's direct members noted.
  fn field_arguments<S: AsRef<[u8]>>(&mut self, at: Writer, fields: &[FieldDefinition<S>]) {
    for field in fields {
      let Some(&id) = self.field_ids.get(self.next_field) else {
        return;
      };
      self.next_field += 1;
      let Some(arguments) = field.arguments_definition() else {
        continue;
      };
      for argument in arguments.input_value_definitions() {
        let name = argument.name();
        let sym = self.intern(name);
        let member = self.builder.push_argument(
          id,
          sym,
          Location::new(at.source, *name.as_span()),
          Location::new(at.source, *argument.span()),
        );
        self.describe(member, at.source, argument.description());
      }
    }
  }

  /// The type parameters `name` declares, which `at` writes, in written order.
  fn type_params<S: AsRef<[u8]>>(&mut self, at: Writer, name: &DefinitionName<S>) {
    let Some(generics) = name.generics() else {
      return;
    };
    for parameter in generics.params() {
      let written = parameter.name();
      let sym = self.intern(written);
      self
        .builder
        .push_type_param(at.def, sym, Location::new(at.source, *written.as_span()));
    }
  }

  /// The fields `fields`, which `at` writes, as direct members, each id noted for its arguments.
  fn fields<S: AsRef<[u8]>>(&mut self, at: Writer, fields: &[FieldDefinition<S>]) {
    for field in fields {
      let id = self.member(
        at,
        MemberKind::Field,
        field.name(),
        *field.span(),
        field.description(),
      );
      self.field_ids.push(id);
    }
  }

  /// The enum values `values`, which `at` writes, as direct members.
  fn enum_values<S: AsRef<[u8]>>(&mut self, at: Writer, values: &[EnumValueDefinition<S>]) {
    for value in values {
      self.member(
        at,
        MemberKind::EnumValue,
        value.value(),
        *value.span(),
        value.description(),
      );
    }
  }

  /// The input values `values`, which `at` writes, as direct members of `kind`: an input object's
  /// fields, or a directive definition's arguments.
  fn input_values<S: AsRef<[u8]>>(
    &mut self,
    at: Writer,
    kind: MemberKind,
    values: &[InputValueDefinition<S>],
  ) {
    for value in values {
      self.member(at, kind, value.name(), *value.span(), value.description());
    }
  }

  /// Appends a direct member of `kind`, which `at` writes, named `name`, spanning `extent`, with
  /// its description. A directive definition's argument is also kept by its directive and its
  /// name, the first of each name.
  fn member<S: AsRef<[u8]>>(
    &mut self,
    at: Writer,
    kind: MemberKind,
    name: &Name<S>,
    extent: SimpleSpan,
    description: Option<&StringValue<S>>,
  ) -> MemberId {
    let sym = self.intern(name);
    let member = self.builder.push_member(
      at.def,
      at.contribution,
      kind,
      sym,
      Location::new(at.source, *name.as_span()),
      Location::new(at.source, extent),
    );
    if kind == MemberKind::Argument {
      self.arguments.entry((at.def, sym)).or_insert(member);
    }
    self.describe(member, at.source, description);
    member
  }

  /// Gives `member` its description, written in the source `source`, cooked: none when none is
  /// written, and an empty one when an empty one is.
  fn describe<S: AsRef<[u8]>>(
    &mut self,
    member: MemberId,
    source: u32,
    description: Option<&StringValue<S>>,
  ) {
    let Some(written) = description else {
      return;
    };
    self.text.clear();
    // A description the lexer read always decodes; one that did not is left absent.
    if decode_string_value(written.source().as_ref(), &mut self.text).is_ok() {
      self.builder.set_member_description(
        member,
        self.text.as_str(),
        Location::new(source, *written.span()),
      );
    }
  }

  /// The symbol of `name`'s spelling, each sequence of it that is not UTF-8 replaced by U+FFFD.
  fn intern<S: AsRef<[u8]>>(&mut self, name: &Name<S>) -> Sym {
    self
      .builder
      .intern(&String::from_utf8_lossy(name.source().as_ref()))
  }

  /// Reports an item, or a part of one, of a kind lowering does not know, written in the source
  /// `source` at `span`: once per source.
  fn unknown(&mut self, source: u32, span: SimpleSpan) {
    if self.reported_unknown.insert(source) {
      unknown_item(self.reporter, Location::new(source, span));
    }
  }

  /// The schema's contributions, after every definition's: each schema definition by source id and
  /// position, then each schema extension likewise; then its description, the first schema
  /// definition's of the root package, whose sources are `root`.
  fn schema(&mut self, declarations: &Declarations<'_, '_>, root: IdRange<SourceId>) {
    let mut parts: Vec<SchemaPart<'_>> = Vec::with_capacity(declarations.schemas.len());
    for entry in &declarations.schemas {
      let part = match declarations.items.get(entry.item.source.get() as usize) {
        Some(Some(Items::Text(items))) => self.schema_part(items, *entry),
        Some(Some(Items::Bytes(items))) => self.schema_part(items, *entry),
        // A schema item is one of a source the walk read.
        Some(None) | None => None,
      };
      if let Some(part) = part {
        parts.push(part);
      }
    }
    // A source's items are flattened in source order, so an item's index is its position.
    parts.sort_by_key(|part: &SchemaPart<'_>| {
      (
        part.extension,
        part.entry.item.source,
        part.entry.item.index,
      )
    });
    for part in &parts {
      if part.extension {
        self
          .builder
          .push_schema_extension(part.location, part.extent);
      } else {
        self
          .builder
          .push_schema_contribution(part.location, part.extent);
      }
    }

    let first = parts
      .iter()
      .find(|part: &&SchemaPart<'_>| !part.extension && root.contains(part.entry.item.source));
    if let Some(part) = first
      && let Some((spelling, location)) = part.description
    {
      self.text.clear();
      // A description the lexer read always decodes; one that did not is left absent.
      if decode_string_value(spelling, &mut self.text).is_ok() {
        self
          .builder
          .set_schema_description(self.text.as_str(), location);
      }
    }
  }

  /// The schema item `entry` addresses among `items`, the items of its source, as the schema's
  /// contributions take it; `None` for an item of a kind lowering does not know, which is
  /// reported.
  fn schema_part<'d, S: AsRef<[u8]>>(
    &mut self,
    items: &[&'d TypeSystemItem<S>],
    entry: Entry,
  ) -> Option<SchemaPart<'d>> {
    let &item = items.get(entry.item.index as usize)?;
    let source = entry.item.source.get();
    let extent = Location::new(source, *item.span());
    match item {
      TypeSystemItem::Definition(described) => match described.node() {
        TypeSystemDefinition::Schema(schema) => Some(SchemaPart {
          entry,
          extension: false,
          location: keyword(source, *schema.span()),
          extent,
          description: described.description().map(
            |text: &'d StringValue<S>| -> (&'d [u8], Location) {
              (text.source().as_ref(), Location::new(source, *text.span()))
            },
          ),
        }),
        // An entry of the schema's is a schema definition or extension.
        TypeSystemDefinition::Type(_) | TypeSystemDefinition::Directive(_) => None,
      },
      TypeSystemItem::Extension(extension) => match extension {
        TypeSystemExtension::Schema(schema) => Some(SchemaPart {
          entry,
          extension: true,
          location: keyword(source, *schema.span()),
          extent,
          description: None,
        }),
        TypeSystemExtension::Type(_) | TypeSystemExtension::Directive(_) => None,
      },
      TypeSystemItem::Use(_) | TypeSystemItem::Mod(_) => None,
      // `TypeSystemItem` is `#[non_exhaustive]`: an item of a kind lowering does not know is
      // reported, and nothing of it is lowered.
      _ => {
        self.unknown(source, *item.span());
        None
      }
    }
  }

  /// Reports, once and at `at`, a spelling the model's arenas could not keep.
  fn arenas(&mut self, at: Location) {
    if !self.builder.arenas_overflowed() {
      return;
    }
    let kind = DiagnosticKind::ResourceLimit;
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(
          "the names and texts the model keeps pass the four gigabytes its `u32` offsets \
           address, so some of the spellings it answers are not the ones written",
        )
        .primary_label(kind.title())
        .help("lower `Limits::max_source_bytes`, or supply less source")
        .build(),
    );
  }
}

/// The keyword an extension of a definition of `kind` is written with.
fn keyword_of(kind: DefKind) -> &'static str {
  match kind {
    DefKind::Scalar => "scalar",
    DefKind::Object => "type",
    DefKind::Interface => "interface",
    DefKind::Union => "union",
    DefKind::Enum => "enum",
    DefKind::InputObject => "input",
    DefKind::Directive => "directive",
  }
}

/// A definition of `kind`, as a message names it.
fn noun_of(kind: DefKind) -> &'static str {
  match kind {
    DefKind::Scalar => "a scalar",
    DefKind::Object => "an object type",
    DefKind::Interface => "an interface",
    DefKind::Union => "a union",
    DefKind::Enum => "an enum",
    DefKind::InputObject => "an input object",
    DefKind::Directive => "a directive",
  }
}

/// Where the schema item whose node spans `node`, in the source `source`, writes its first keyword:
/// the parser starts a schema definition's node at its `schema` and a schema extension's at its
/// `extend`, and each is six bytes.
fn keyword(source: u32, node: SimpleSpan) -> Location {
  let start = node.start();
  Location::new(
    source,
    SimpleSpan::new(start, start.saturating_add(KEYWORD).min(node.end())),
  )
}

/// The model's directive location for `location`, as a directive definition writes it.
fn directive_location(location: &ast::Location) -> DirectiveLocation {
  match location {
    ast::Location::Executable(executable) => match executable {
      ExecutableDirectiveLocation::Query(_) => DirectiveLocation::Query,
      ExecutableDirectiveLocation::Mutation(_) => DirectiveLocation::Mutation,
      ExecutableDirectiveLocation::Subscription(_) => DirectiveLocation::Subscription,
      ExecutableDirectiveLocation::Field(_) => DirectiveLocation::Field,
      ExecutableDirectiveLocation::FragmentDefinition(_) => DirectiveLocation::FragmentDefinition,
      ExecutableDirectiveLocation::FragmentSpread(_) => DirectiveLocation::FragmentSpread,
      ExecutableDirectiveLocation::InlineFragment(_) => DirectiveLocation::InlineFragment,
      ExecutableDirectiveLocation::VariableDefinition(_) => DirectiveLocation::VariableDefinition,
    },
    ast::Location::TypeSystem(system) => match system {
      TypeSystemDirectiveLocation::Schema(_) => DirectiveLocation::Schema,
      TypeSystemDirectiveLocation::Scalar(_) => DirectiveLocation::Scalar,
      TypeSystemDirectiveLocation::Object(_) => DirectiveLocation::Object,
      TypeSystemDirectiveLocation::FieldDefinition(_) => DirectiveLocation::FieldDefinition,
      TypeSystemDirectiveLocation::ArgumentDefinition(_) => DirectiveLocation::ArgumentDefinition,
      TypeSystemDirectiveLocation::Interface(_) => DirectiveLocation::Interface,
      TypeSystemDirectiveLocation::Union(_) => DirectiveLocation::Union,
      TypeSystemDirectiveLocation::Enum(_) => DirectiveLocation::Enum,
      TypeSystemDirectiveLocation::EnumValue(_) => DirectiveLocation::EnumValue,
      TypeSystemDirectiveLocation::InputObject(_) => DirectiveLocation::InputObject,
      TypeSystemDirectiveLocation::InputFieldDefinition(_) => {
        DirectiveLocation::InputFieldDefinition
      }
      TypeSystemDirectiveLocation::DirectiveDefinition(_) => DirectiveLocation::DirectiveDefinition,
    },
  }
}
