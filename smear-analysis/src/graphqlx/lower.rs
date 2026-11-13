//! Lowering: what each declaration holds beyond its identity, written into the model.
//!
//! # Extensions
//!
//! Before the definitions, each extension finds its target: its path, resolved in the module the
//! extension is written in, in the type namespace, or in the directive namespace for `extend
//! directive`, with no generic parameter in scope. A target that names nothing is reported by name
//! resolution, where it is written. A target that names a definition of another kind than the
//! extension's keyword is reported as `extension-kind-mismatch`, at the target, with a label at the
//! definition. A target that is an alias, `NonZeroU32` and its kin, spells an instantiation of
//! `core`'s `NonZero` and is no definition of its own: it is reported at the target, with GraphQL's
//! `undefined-extension-target` when it is one segment with no root and `unresolved-path`
//! otherwise. Each way the extension contributes nothing and nothing inside it is lowered: one
//! mistake, one diagnostic. Every other extension is a contribution of its target, after the
//! definition's own, by source id and position wherever each is written: named where its target
//! path is written, and spanning from its `extend` to its last token. The parameter names an
//! extension writes name its target's parameters by position, and are no type parameters of their
//! own: each is kept with its extension, for the rule that judges them distinct. A list of them
//! names every one of its target's parameters: a list of another length is `generic-arity`, at the
//! list, with a label at the definition's name, and the extension still contributes what it writes.
//! An extension that writes no list names none of its target's parameters, whatever their count,
//! and a name in it resolves as any other name of its module does.
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
//! 2. its type parameters, in written order, each named where its name is written. A definition
//!    that replaces a GraphQL built-in keeps the built-in's identity, which has no type parameter:
//!    a list it writes is `generic-arity`, at the list, and nothing of it is lowered, neither a
//!    parameter, nor a default, nor its `where` clause. Its names stay in scope, so a path inside
//!    it that names one names a position no parameter has;
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
//! # The occurrences
//!
//! A second walk over the definitions, in `DefId` order, appends every type occurrence and
//! directive application in the order the model's walk reaches it, and reads every written value:
//! each type parameter's default, then each bound; the `implements` entries, then the union
//! members, then the applications, each over every contribution in turn; then each direct member's
//! type, default and applications, and right after a field's, each of its arguments' the same way.
//! The first walk has appended every member by then, the arguments of every directive definition
//! among them, so each one's id is known, whichever definition comes first.
//!
//! A written type is one occurrence, appended before what it nests: its arguments in order, each
//! with everything it nests before the next, or its element, walked with an explicit stack. It
//! keeps its path as written, `Int` staying `Int` and an extension's own name for a parameter
//! staying its own, and its written arguments, however many its definition has parameters. Its
//! path is resolved once, where its contribution is written: in the module the extension is
//! written in for an extension, and in the root package's root module for a built-in that package
//! replaces. In scope are the contribution's generic parameters, built into one table for each
//! contribution: a definition's own, in every one of its parameters' defaults too, and an
//! extension's own names, each naming its target's parameter at the same position. A path that
//! names nothing leaves its occurrence unresolved, with its path and its arguments, once name
//! resolution has reported it with the code of its place. An alias such as `NonZeroU32` names
//! `core`'s `NonZero`, with its path and its arguments as written, none unless it is written with
//! some, as a `NonZero` a `use` item renames does too; the primitive the alias spells is noted
//! beside the row, whatever is written with it, for instantiation, which alone tells the two apart
//! and refuses an alias's arguments. No type is interned: an occurrence's type is instantiation's.
//!
//! A bound is its definition's own: a `where` predicate whose left side is one of the definition's
//! type parameters, written with no argument, gives each interface it names as a bound of that
//! parameter, located at the predicate. Any other left side, and any `where` clause on an
//! extension, is `invalid-bound`, and nothing of it is lowered. A right side is an interface, a
//! marker among them, with or without type arguments: once its occurrence is appended, one that
//! names a definition of another kind, an alias such as `NonZeroU32` or a type parameter is
//! `invalid-bound` too, at the right side, and the bound is still recorded, as written.
//!
//! An application keeps its directive's path as written, and its directive is resolved where it
//! is written, as a type is. One whose directive names nothing stays, unresolved, with no type
//! argument and no argument: nothing inside it is lowered. Otherwise its type arguments follow,
//! each an occurrence, then its arguments: each written argument is looked up by its name among
//! the directive definition's, one lookup each; one the definition does not declare is
//! `undefined-directive-argument`, with the nearest name it does declare in its help, and is not
//! kept; the rest are kept in the definition's order, an argument written twice twice, and their
//! values are read in that order.
//!
//! Lowering writes no value. A written value, a member's default or an argument's, is read into a
//! tree, kept with the member whose default it is or with the application, the argument it sets
//! and where its name is written: one node for each literal it writes, the value first and then
//! what it holds, in pre-order, walked with an explicit stack. A node keeps its syntactic kind and
//! where it is written, a number its radix, a boolean its value, and a path its root and its
//! segments, interned as written. No path in it is resolved: coercion, which runs after
//! instantiation and knows the type expected, makes the model's values of the trees.
//!
//! # The schema
//!
//! After every definition's contributions come the schema's, appended once the second walk over the
//! definitions has ended, which appends no contribution: every schema definition any package
//! writes, by source id and position, then every schema extension, likewise, each marked as what
//! it is. A schema item's contribution is named where its first keyword is written, `schema` or
//! `extend`: the six bytes the item's node starts with, as the parser spans a schema definition
//! from its `schema` and a schema extension from its `extend`. It spans the whole item, a
//! description included. The schema is the root package's: the first schema definition the root
//! package writes gives it its description, its roots and its applications, and each schema
//! extension the root package writes gives it its roots and its applications. Every other schema
//! item writes nothing into it, and nothing in it is lowered: it is a contribution alone, which is
//! what the rules that refuse a second schema definition, or a dependency's schema item, read.
//!
//! The roots are read item by item, in contribution order, and each item's as written: the first
//! for an operation is the schema's, and each later one is `duplicate-root-operation-type`, with a
//! label at the first, and is not lowered. With no schema definition of the root package's, an
//! object type its root module itself declares named `Query`, `Mutation` or `Subscription` is that
//! operation's root, presumed before any extension's is read; a name a `use` item or the prelude
//! gives is none. The roots written are then lowered, query, mutation and then subscription,
//! however they were written, each a type occurrence resolved where its item is written, with no
//! generic parameter in scope; then the schema's applications, item by item.
//!
//! # Generic once
//!
//! This is the last step that sees a parsed document, and so the last that is generic: a document
//! is `TypeSystemDocument<&str>`, or `TypeSystemDocument<&[u8]>` for a byte text that is not UTF-8,
//! never the caller's own text type. Each walk is written once, generic over the slice type, and
//! each item is dispatched on its document's parser where it is walked. Lowering copies every
//! spelling the model keeps, so the model it builds borrows nothing; the trees it answers borrow
//! the sources' texts and no document, and everything after it is compiled once.

use core::num::NonZeroU32;
use std::{
  collections::{BTreeMap, BTreeSet},
  format,
  string::String,
  vec,
  vec::Vec,
};

use smear_lexer::{
  decode_string_value,
  graphqlx::{LitFloat, LitInt},
};
use smear_parser::graphqlx::ast::{
  self, ConstArgument, ConstDirective, ConstDirectives, ConstInputValue, DefinitionName,
  DescribedTypeSystemDefinition, EnumValueDefinition, ExecutableDirectiveLocation, ExtensionName,
  FieldDefinition, ImplementInterfaces, InputValueDefinition, Name, Path,
  RootOperationTypeDefinition, StringValue, Type, TypeDefinition, TypeExtension, TypePath,
  TypeSystemDefinition, TypeSystemDirectiveLocation, TypeSystemExtension, TypeSystemItem,
  WhereClause,
};
use tokora::{SimpleSpan, diagnostic::Location, span::AsSpan};

use super::{
  builtin::{TYPE, spelled},
  code::{DiagnosticKind, schema},
  collect::{Items, unknown_item},
  declare::{Binding, Declaration, Declarations, Entry, Item},
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{ApplicationId, DefId, IdRange, MemberId, ModuleId, SourceId, Sym, TypeExprId, dense},
  model::{
    DefKind, DirectiveLocation, MemberKind, ModelBuilder, PathRoot, Root, RootOperation, Written,
  },
  raw::{Radix, RawArgument, RawKind, RawValues},
  resolve::{self, Position, Request, Resolved, Scopes, Wanted},
  set::{PackageView, ROOT},
  source::SourceMap,
};

/// The length of a schema item's first keyword, `schema` or `extend`.
const KEYWORD: usize = 6;

/// Lowers what `declarations` holds into `builder`, which holds every package, module and
/// definition already, resolving each path through `scopes` and reporting into `reporter`;
/// `sources` numbers the sources of `packages`. Answers every written value, read from the texts
/// of `packages`, for coercion.
pub(crate) fn lower<'p>(
  packages: &[PackageView<'p>],
  declarations: &Declarations<'_, '_>,
  scopes: &Scopes<'_>,
  sources: &SourceMap,
  builder: &mut ModelBuilder,
  reporter: &mut Reporter,
) -> RawValues<'p> {
  // Each source's text, by `SourceId`, numbered as `sources` numbers them: a package source has
  // none.
  let mut texts: Vec<&'p [u8]> = Vec::with_capacity(sources.len() as usize);
  for package in packages {
    texts.push(&[]);
    for file in &package.files {
      texts.push(file.text.bytes());
    }
  }
  let mut lowering = Lowering {
    builder,
    reporter,
    scopes,
    values: RawValues::new(texts),
    text: String::new(),
    field_ids: Vec::new(),
    next_field: 0,
    arguments: BTreeMap::new(),
    reported_unknown: BTreeSet::new(),
    firsts: Vec::with_capacity(declarations.defs.len()),
    next_member: 0,
    direct: 0,
    argument: 0,
    segments: Vec::new(),
    type_args: Vec::new(),
  };
  let attached = lowering.targets(declarations);
  // `attached` is sorted by target, so each definition's extensions are the next run of it. The
  // first walk appends what holds no occurrence; the second, every occurrence.
  let mut next = 0;
  for (index, declaration) in declarations.defs.iter().enumerate() {
    let def = DefId::new(dense(index));
    let end = run_end(&attached, next, def);
    lowering.definition(declarations, def, declaration, &attached[next..end]);
    next = end;
  }
  let mut next = 0;
  for (index, declaration) in declarations.defs.iter().enumerate() {
    let def = DefId::new(dense(index));
    let end = run_end(&attached, next, def);
    lowering.occurrences(declarations, def, declaration, &attached[next..end]);
    next = end;
  }
  lowering.schema(declarations, sources.package_sources(ROOT));
  lowering.values
}

/// What lowering keeps while it writes: the builder and the reporter it writes into, the scopes it
/// resolves in, the values it reads, and what it reuses or gathers on the way.
struct Lowering<'l, 'x, 'p> {
  builder: &'l mut ModelBuilder,
  reporter: &'l mut Reporter,
  /// Every module's scope: what a path means where it is written.
  scopes: &'l Scopes<'x>,
  /// Every value read so far, and each source's text.
  values: RawValues<'p>,
  /// The description being cooked: one buffer for every description.
  text: String,
  /// The fields of the definition being lowered, in id order, whose arguments follow every one of
  /// its direct members: one buffer for every definition.
  field_ids: Vec<MemberId>,
  /// How many of `field_ids` have had their arguments appended.
  next_field: usize,
  /// Each directive definition's arguments, by the directive and the argument's name, the first of
  /// each name kept: the key is what a written argument of an application is looked up by.
  arguments: BTreeMap<(DefId, Sym), MemberId>,
  /// The sources an item of a kind lowering does not know was reported in, each once.
  reported_unknown: BTreeSet<u32>,
  /// Where each definition's members start, by `DefId`, as the first walk appended them.
  firsts: Vec<Firsts>,
  /// The id of the next member the first walk appends.
  next_member: u32,
  /// The id of the next direct member of the definition the second walk is in.
  direct: u32,
  /// The id of the next argument of a field of the definition the second walk is in.
  argument: u32,
  /// The segments of the path being recorded: one buffer for every path.
  segments: Vec<Sym>,
  /// The arguments of the written types being lowered, gathered until each one's are all
  /// appended: one buffer for every written type.
  type_args: Vec<TypeExprId>,
}

/// Where one definition's members start: its first direct member's id, and its first argument's,
/// which follows its last direct member.
#[derive(Clone, Copy)]
struct Firsts {
  direct: u32,
  argument: u32,
}

/// Where the members being lowered are written: the definition they belong to, the contribution
/// that writes them, and the source it is written in.
#[derive(Clone, Copy)]
struct Writer {
  def: DefId,
  contribution: u32,
  source: u32,
  /// Whether the contribution is the definition the root package replaces a GraphQL built-in with,
  /// whose identity is the built-in's and has no type parameter.
  replaces: bool,
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

/// What a schema item gives the schema: its applications and its root operations.
struct SchemaShape<'d, S> {
  directives: Option<&'d ConstDirectives<S>>,
  roots: &'d [RootOperationTypeDefinition<S>],
}

/// The root of one operation, as the schema's roots are read: the first one each operation is
/// given.
#[derive(Clone, Copy)]
enum Kept {
  /// Written: the root operation `index` of the schema's contribution `part`, its type written at
  /// `at`.
  Written {
    part: usize,
    index: usize,
    at: Location,
  },
  /// Presumed, with no schema definition: the definition `def`, named at `at`.
  Presumed { def: DefId, at: Location },
}

impl Kept {
  /// Where the root's type is named.
  fn at(self) -> Location {
    match self {
      Self::Written { at, .. } | Self::Presumed { at, .. } => at,
    }
  }
}

/// What one walk of the second pass appends of each contribution, in the walk's order.
#[derive(Clone, Copy)]
enum Occurrences {
  /// The definition's type parameters' defaults, then its bounds; an extension's `where` clause is
  /// refused.
  Generics,
  /// The `implements` entries.
  Implements,
  /// A union's members.
  UnionMembers,
  /// The applications.
  Applications,
  /// What each member holds, each direct member's and then its arguments'.
  Members,
}

/// One contribution as the second walk takes it.
#[derive(Clone, Copy)]
struct Part<'p> {
  /// The definition it belongs to.
  def: DefId,
  /// The item that writes it.
  item: Item,
  /// Its index among the definition's contributions.
  contribution: u32,
  /// Whether it is the definition the root package replaces a GraphQL built-in with.
  replaces: bool,
  /// Where its paths resolve.
  site: Site<'p>,
}

/// Where the paths of one contribution resolve.
#[derive(Clone, Copy)]
struct Site<'p> {
  /// The definition whose parameters a parameter in scope is.
  owner: Option<DefId>,
  /// The module it is written in.
  module: ModuleId,
  /// The source it is written in.
  source: SourceId,
  /// The generic parameters in scope, by spelling, each with its position.
  params: &'p [(&'p [u8], u32)],
}

/// What the second walk reads of one item that writes a contribution: each part `None`, or
/// empty, where the item's kind has none.
struct Shape<'d, S> {
  /// A definition's name, with its type parameters and their defaults.
  name: Option<&'d DefinitionName<S>>,
  /// An extension's name, whose parameter names name its target's by position.
  extension: Option<&'d ExtensionName<S>>,
  /// Its `where` clause.
  clause: Option<&'d WhereClause<S>>,
  implements: Option<&'d ImplementInterfaces<S>>,
  /// A union's members.
  union_members: &'d [TypePath<S>],
  /// Its applications.
  directives: Option<&'d ConstDirectives<S>>,
  /// An object's or an interface's fields.
  fields: &'d [FieldDefinition<S>],
  /// An input object's fields, or a directive definition's arguments.
  input_values: &'d [InputValueDefinition<S>],
  /// An enum's values.
  enum_values: &'d [EnumValueDefinition<S>],
}

/// What the walk of one written value has still to do.
enum Read<'v, S> {
  /// Read this value into the next node.
  Enter(&'v ConstInputValue<S>),
  /// Close the node at this index: everything under it is read.
  Leave(u32),
}

/// What the walk of one written type has still to do.
enum Pending<'t, S> {
  /// Lower `ty`: the element of the list `list`, or, with no list, an argument of the occurrence
  /// whose arguments are being gathered.
  Type {
    ty: &'t Type<S>,
    list: Option<TypeExprId>,
  },
  /// Give `occurrence` the arguments gathered from `from` on.
  Args { occurrence: TypeExprId, from: usize },
}

/// A schema definition or extension, as the schema's contributions take it.
struct SchemaPart<'d> {
  /// Where it is.
  entry: Entry,
  /// Its index among the schema's contributions, once it is one.
  contribution: u32,
  /// Whether it is an extension: the schema's definitions come first.
  extension: bool,
  /// Its first keyword, `schema` or `extend`.
  location: Location,
  /// The whole item, a description included.
  extent: Location,
  /// A schema definition's description, as written, and where it is written.
  description: Option<(&'d [u8], Location)>,
}

impl Lowering<'_, '_, '_> {
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
    let location = Location::new(source.get(), *path.span());
    let target = match self
      .scopes
      .resolve(&*self.builder, self.reporter, &request, path)
    {
      Resolved::Def(def) => def,
      // `NonZeroU32` and its kin spell an instantiation of `core`'s `NonZero`, and are no
      // definitions of their own: there is nothing to extend.
      Resolved::NonZero { primitive, .. } => {
        self.alias(declarations, keyword, path, primitive, location);
        return None;
      }
      // Name resolution has reported a target that names nothing. A type or a directive is
      // wanted, which no module is, and no parameter is in scope.
      Resolved::Module(_) | Resolved::Param(_) | Resolved::Unresolved => return None,
    };
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
  /// target `path`, written at `at`, is an alias: a spelling of `core`'s `NonZero` of the
  /// primitive whose definition is `primitive`, which is no definition of its own. A name of one
  /// segment with no root keeps GraphQL's code for a target that names no definition.
  fn alias<S: AsRef<[u8]>>(
    &mut self,
    declarations: &Declarations<'_, '_>,
    keyword: DefKind,
    path: &Path<S>,
    primitive: DefId,
    at: Location,
  ) {
    let name: &[u8] = match path.segments().last() {
      Some(last) => last.source().as_ref(),
      None => &[],
    };
    // The primitives `core` materialises are the first definitions, each at its own index.
    let instance = match declarations.primitives.get(primitive.get() as usize) {
      Some(&(written, def)) if def == primitive => format!("NonZero<{}>", spelled(written)),
      _ => String::from("NonZero"),
    };
    let (code, title) = if path.root().is_relative() && path.segments().len() == 1 {
      (
        schema::UNDEFINED_EXTENSION_TARGET,
        "extension target is not defined",
      )
    } else {
      let kind = DiagnosticKind::UnresolvedPath;
      (kind.code(), kind.title())
    };
    self.reporter.report(
      Diagnostic::builder(code, at)
        .message(format!(
          "`extend {}` names `{}`, a spelling of `{}`, which is no definition of its own",
          keyword_of(keyword),
          Excerpt(name),
          Excerpt(instance.as_bytes())
        ))
        .primary_label(title)
        .help("an alias has nothing to extend: extend `NonZero` to reach every width.")
        .build(),
    );
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
    let direct = self.next_member;
    let replaces = replaces_built_in(&*self.builder, def, declaration);
    // A primitive `core` materialises writes nothing of its own: its extensions may.
    if let Some(item) = declaration.item {
      let at = Writer {
        def,
        contribution: own,
        source: item.source.get(),
        replaces,
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
        replaces: false,
      };
      self.walk(declarations, extension.entry.item, at, Step::Members);
    }
    let argument = self.next_member;
    if let Some(item) = declaration.item {
      let at = Writer {
        def,
        contribution: own,
        source: item.source.get(),
        replaces,
      };
      self.walk(declarations, item, at, Step::Arguments);
    }
    // Each extension is the contribution after the one before it.
    for (contribution, extension) in (own.saturating_add(1)..).zip(attached) {
      let at = Writer {
        def,
        contribution,
        source: extension.entry.item.source.get(),
        replaces: false,
      };
      self.walk(declarations, extension.entry.item, at, Step::Arguments);
    }
    self.firsts.push(Firsts { direct, argument });
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
        self.next_member = member.get().saturating_add(1);
        self.describe(member, at.source, argument.description());
      }
    }
  }

  /// The type parameters `name` declares, which `at` writes, in written order. A definition that
  /// replaces a GraphQL built-in declares none: a list it writes is reported, and none of it is a
  /// row.
  fn type_params<S: AsRef<[u8]>>(&mut self, at: Writer, name: &DefinitionName<S>) {
    let Some(generics) = name.generics() else {
      return;
    };
    if at.replaces {
      self.replaced_generics(Location::new(at.source, *generics.span()), at.def, name);
      return;
    }
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
    self.next_member = member.get().saturating_add(1);
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

  /// The occurrences of the definition `def`, declared as `declaration` and extended by
  /// `attached`, in the walk's order: its parameters' defaults and its bounds; its `implements`
  /// entries, then its union members, then its applications, each over every contribution in
  /// turn; then what its members hold.
  fn occurrences(
    &mut self,
    declarations: &Declarations<'_, '_>,
    def: DefId,
    declaration: &Declaration,
    attached: &[Attached],
  ) {
    if let Some(firsts) = self.firsts.get(def.get() as usize).copied() {
      self.direct = firsts.direct;
      self.argument = firsts.argument;
    }
    // Each contribution an item writes: the item, the module it is written in, and its index
    // among the definition's contributions, the definition's own being the first.
    let mut written: Vec<(Item, ModuleId, u32)> = Vec::with_capacity(attached.len() + 1);
    if let Some(item) = declaration.item {
      written.push((item, declaration.module, 0));
    }
    for (contribution, extension) in (1..).zip(attached) {
      written.push((extension.entry.item, extension.entry.module, contribution));
    }
    // The parameters in scope in each contribution: every one's names gathered first, and then
    // one table built for each, which every path the contribution writes is resolved with.
    let mut names: Vec<&[u8]> = Vec::new();
    let mut runs: Vec<(usize, usize)> = Vec::with_capacity(written.len());
    for &(item, _, _) in &written {
      let from = names.len();
      self.param_names(declarations, item, &mut names);
      runs.push((from, names.len()));
    }
    let mut tables: Vec<Vec<(&[u8], u32)>> = Vec::with_capacity(runs.len());
    for &(from, to) in &runs {
      tables.push(resolve::params(names.get(from..to).unwrap_or_default()));
    }
    let replaces = replaces_built_in(&*self.builder, def, declaration);
    for step in [
      Occurrences::Generics,
      Occurrences::Implements,
      Occurrences::UnionMembers,
      Occurrences::Applications,
      Occurrences::Members,
    ] {
      for (&(item, module, contribution), table) in written.iter().zip(&tables) {
        let part = Part {
          def,
          item,
          contribution,
          // The definition's own contribution is the first; the rest are extensions.
          replaces: replaces && contribution == 0,
          site: Site {
            owner: Some(def),
            module,
            source: item.source,
            params: table.as_slice(),
          },
        };
        self.visit(declarations, part, step);
      }
    }
  }

  /// Adds the spellings of the generic parameters `item` writes to `names`, in written order: a
  /// definition's own, or the names an extension gives its target's.
  fn param_names<'d>(
    &mut self,
    declarations: &'d Declarations<'_, '_>,
    item: Item,
    names: &mut Vec<&'d [u8]>,
  ) {
    match declarations.items.get(item.source.get() as usize) {
      Some(Some(Items::Text(items))) => self.names_in(items, item, names),
      Some(Some(Items::Bytes(items))) => self.names_in(items, item, names),
      // A contribution's item is one of a source the walk read.
      Some(None) | None => {}
    }
  }

  /// Adds the spellings of the generic parameters the item `item` addresses among `items`, the
  /// items of its source, writes to `names`.
  fn names_in<'d, S: AsRef<[u8]>>(
    &mut self,
    items: &[&'d TypeSystemItem<S>],
    item: Item,
    names: &mut Vec<&'d [u8]>,
  ) {
    let Some(&written) = items.get(item.index as usize) else {
      return;
    };
    let Some(shape) = self.shape(written, item.source.get()) else {
      return;
    };
    if let Some(name) = shape.name
      && let Some(generics) = name.generics()
    {
      for parameter in generics.params() {
        names.push(parameter.name().source().as_ref());
      }
    }
    if let Some(name) = shape.extension
      && let Some(generics) = name.generics()
    {
      for parameter in generics.params() {
        names.push(parameter.name().source().as_ref());
      }
    }
  }

  /// Walks the item `part` addresses for `step`, as the parser that made its document read it.
  fn visit(&mut self, declarations: &Declarations<'_, '_>, part: Part<'_>, step: Occurrences) {
    match declarations.items.get(part.item.source.get() as usize) {
      Some(Some(Items::Text(items))) => self.occurrences_in(items, part, step),
      Some(Some(Items::Bytes(items))) => self.occurrences_in(items, part, step),
      // A contribution's item is one of a source the walk read.
      Some(None) | None => {}
    }
  }

  /// Appends, for `step`, the occurrences the item `part` addresses among `items`, the items of
  /// its source, writes.
  fn occurrences_in<S: AsRef<[u8]>>(
    &mut self,
    items: &[&TypeSystemItem<S>],
    part: Part<'_>,
    step: Occurrences,
  ) {
    let Some(&item) = items.get(part.item.index as usize) else {
      return;
    };
    let Some(shape) = self.shape(item, part.site.source.get()) else {
      return;
    };
    match step {
      Occurrences::Generics => self.generics(part, &shape),
      Occurrences::Implements => {
        if let Some(implements) = shape.implements {
          for interface in implements.interfaces() {
            let entry = self.type_path(part.site, interface, Position::Implements);
            self
              .builder
              .push_implements(part.def, part.contribution, entry);
          }
        }
      }
      Occurrences::UnionMembers => {
        for member in shape.union_members {
          let entry = self.type_path(part.site, member, Position::UnionMember);
          self
            .builder
            .push_union_member(part.def, part.contribution, entry);
        }
      }
      Occurrences::Applications => {
        if let Some(directives) = shape.directives {
          for directive in directives.directives() {
            let application = self.application(part.site, directive);
            self
              .builder
              .push_def_application(part.def, part.contribution, application);
          }
        }
      }
      Occurrences::Members => self.member_occurrences(part.site, &shape),
    }
  }

  /// What the second walk reads of `item`, written in the source `source`: `None` for an item
  /// that writes no definition's contribution, and for one of a kind lowering does not know,
  /// which is reported.
  fn shape<'d, S: AsRef<[u8]>>(
    &mut self,
    item: &'d TypeSystemItem<S>,
    source: u32,
  ) -> Option<Shape<'d, S>> {
    match item {
      TypeSystemItem::Definition(described) => match described.node() {
        TypeSystemDefinition::Type(definition) => match definition {
          TypeDefinition::Scalar(scalar) => {
            let mut shape = shape_of(
              Some(scalar.name()),
              None,
              scalar.implements(),
              scalar.directives(),
            );
            shape.clause = scalar.where_clause();
            Some(shape)
          }
          TypeDefinition::Object(object) => {
            let mut shape = shape_of(
              Some(object.name()),
              None,
              object.implements(),
              object.directives(),
            );
            if let Some(fields) = object.fields_definition() {
              shape.clause = fields.where_clause();
              shape.fields = fields.target().field_definitions();
            }
            Some(shape)
          }
          TypeDefinition::Interface(interface) => {
            let mut shape = shape_of(
              Some(interface.name()),
              None,
              interface.implements(),
              interface.directives(),
            );
            if let Some(fields) = interface.fields_definition() {
              shape.clause = fields.where_clause();
              shape.fields = fields.target().field_definitions();
            }
            Some(shape)
          }
          TypeDefinition::Union(union) => {
            let mut shape = shape_of(
              Some(union.name()),
              None,
              union.implements(),
              union.directives(),
            );
            if let Some(members) = union.member_types() {
              shape.clause = members.where_clause();
              shape.union_members = members.target().members();
            }
            Some(shape)
          }
          TypeDefinition::Enum(enumeration) => {
            let mut shape = shape_of(
              Some(enumeration.name()),
              None,
              enumeration.implements(),
              enumeration.directives(),
            );
            if let Some(values) = enumeration.enum_values_definition() {
              shape.enum_values = values.enum_value_definitions();
            }
            Some(shape)
          }
          TypeDefinition::InputObject(input) => {
            let mut shape = shape_of(
              Some(input.name()),
              None,
              input.implements(),
              input.directives(),
            );
            if let Some(fields) = input.fields_definition() {
              shape.clause = fields.where_clause();
              shape.input_values = fields.target().input_value_definitions();
            }
            Some(shape)
          }
          // `TypeDefinition` is `#[non_exhaustive]`: a kind of definition lowering does not know is
          // reported, and nothing of it is lowered.
          _ => {
            self.unknown(source, *described.span());
            None
          }
        },
        TypeSystemDefinition::Directive(directive) => {
          let mut shape = shape_of(Some(directive.name()), None, None, directive.directives());
          shape.clause = directive.locations().where_clause();
          if let Some(arguments) = directive.arguments_definition() {
            shape.input_values = arguments.input_value_definitions();
          }
          Some(shape)
        }
        // A schema definition is the schema's.
        TypeSystemDefinition::Schema(_) => None,
      },
      TypeSystemItem::Extension(extension) => match extension {
        TypeSystemExtension::Type(ty) => match ty {
          TypeExtension::Scalar(scalar) => Some(shape_of(
            None,
            Some(scalar.name()),
            scalar.implements(),
            scalar.directives(),
          )),
          TypeExtension::Object(object) => {
            let mut shape = shape_of(
              None,
              Some(object.name()),
              object.implements(),
              object.directives(),
            );
            if let Some(fields) = object.fields_definition() {
              shape.clause = fields.where_clause();
              shape.fields = fields.target().field_definitions();
            }
            Some(shape)
          }
          TypeExtension::Interface(interface) => {
            let mut shape = shape_of(
              None,
              Some(interface.name()),
              interface.implements(),
              interface.directives(),
            );
            if let Some(fields) = interface.fields_definition() {
              shape.clause = fields.where_clause();
              shape.fields = fields.target().field_definitions();
            }
            Some(shape)
          }
          TypeExtension::Union(union) => {
            let mut shape = shape_of(
              None,
              Some(union.name()),
              union.implements(),
              union.directives(),
            );
            if let Some(members) = union.member_types() {
              shape.clause = members.where_clause();
              shape.union_members = members.target().members();
            }
            Some(shape)
          }
          TypeExtension::Enum(enumeration) => {
            let mut shape = shape_of(
              None,
              Some(enumeration.name()),
              enumeration.implements(),
              enumeration.directives(),
            );
            if let Some(values) = enumeration.enum_values_definition() {
              shape.enum_values = values.enum_value_definitions();
            }
            Some(shape)
          }
          TypeExtension::InputObject(input) => {
            let mut shape = shape_of(
              None,
              Some(input.name()),
              input.implements(),
              input.directives(),
            );
            if let Some(fields) = input.fields_definition() {
              shape.clause = fields.where_clause();
              shape.input_values = fields.target().input_value_definitions();
            }
            Some(shape)
          }
        },
        // A directive extension's applications are required.
        TypeSystemExtension::Directive(directive) => Some(shape_of(
          None,
          Some(directive.name()),
          None,
          Some(directive.directives()),
        )),
        // A schema extension is the schema's.
        TypeSystemExtension::Schema(_) => None,
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

  /// The contribution `part`'s generics, read as `shape`: for the definition's own, each type
  /// parameter's default, then each bound, predicate by predicate and bound by bound as written.
  /// An extension's parameter list names its target's parameters by position, one name each, and
  /// another count is refused; its names are kept with the extension, for the rule that judges
  /// them distinct. Its `where` clause is refused too: the model's bounds are the definition's. A
  /// definition that replaces a GraphQL built-in has none of its list lowered.
  fn generics<S: AsRef<[u8]>>(&mut self, part: Part<'_>, shape: &Shape<'_, S>) {
    let source = part.site.source.get();
    if let Some(extension) = shape.extension {
      // An extension that writes no list names none of its target's parameters, and is legal
      // whatever their count.
      if let Some(list) = extension.generics() {
        let written = dense(list.params().len());
        let count = self.builder.type_param_count(part.def);
        if written != count {
          self.extension_arity(
            part.def,
            Location::new(source, *list.span()),
            written,
            count,
          );
        }
        // The names the list gives, kept for the rule that judges them distinct.
        let mut names: Vec<(Sym, Location)> = Vec::with_capacity(list.params().len());
        for parameter in list.params() {
          let name = parameter.name();
          let sym = self.intern(name);
          names.push((sym, Location::new(source, *name.as_span())));
        }
        self
          .builder
          .set_extension_params(part.def, part.contribution, &names);
      }
      if let Some(clause) = shape.clause {
        self.invalid_bound(
          Location::new(source, *clause.span()),
          "a `where` clause on an extension bounds nothing: a definition's bounds are written on \
           the definition",
          "move the bounds to the definition's own `where` clause",
        );
      }
      return;
    }
    // The first walk reported the list, and made no parameter row a default or a bound could name.
    if part.replaces
      && shape
        .name
        .is_some_and(|name: &DefinitionName<S>| name.generics().is_some())
    {
      return;
    }
    if let Some(name) = shape.name
      && let Some(generics) = name.generics()
    {
      for (index, parameter) in generics.params().iter().enumerate() {
        if let Some(default) = parameter.default() {
          let ty = self.ty(part.site, default, Position::Type);
          self
            .builder
            .set_type_param_default(part.def, dense(index), ty);
        }
      }
    }
    let Some(clause) = shape.clause else {
      return;
    };
    for predicate in clause.predicates() {
      let bounded = predicate.bounded_type();
      let request = Request {
        module: part.site.module,
        source: part.site.source,
        wanted: Wanted::Type,
        position: Position::Type,
        params: part.site.params,
      };
      let parameter =
        match self
          .scopes
          .resolve(&*self.builder, self.reporter, &request, bounded.path())
        {
          Resolved::Param(index) if bounded.type_generics().is_none() => index,
          // Name resolution has reported a left side that names nothing.
          Resolved::Unresolved => continue,
          Resolved::Def(_)
          | Resolved::NonZero { .. }
          | Resolved::Module(_)
          | Resolved::Param(_) => {
            self.invalid_bound(
              Location::new(source, *bounded.span()),
              "a bound's left side is one of its definition's own type parameters, written with no \
             type argument",
              "bound a type parameter of this definition, as `where T: Interface` does",
            );
            continue;
          }
        };
      let location = Location::new(source, *predicate.span());
      for bound in predicate.bounds() {
        let interface = self.type_path(part.site, bound, Position::Type);
        self.bound_interface(interface);
        self
          .builder
          .push_bound(part.def, parameter, interface, location);
      }
    }
  }

  /// What the members `shape` writes hold, where `site` says, in `MemberId` order: each direct
  /// member's type, default and applications, and right after a field's, each of its arguments'.
  fn member_occurrences<S: AsRef<[u8]>>(&mut self, site: Site<'_>, shape: &Shape<'_, S>) {
    for field in shape.fields {
      let member = self.next_direct();
      let ty = self.ty(site, field.ty(), Position::Type);
      self.builder.set_member_type(member, ty);
      self.member_applications(site, member, field.directives());
      if let Some(arguments) = field.arguments_definition() {
        for argument in arguments.input_value_definitions() {
          let member = self.next_argument();
          self.input_value(site, member, argument);
        }
      }
    }
    for value in shape.input_values {
      let member = self.next_direct();
      self.input_value(site, member, value);
    }
    for value in shape.enum_values {
      let member = self.next_direct();
      self.member_applications(site, member, value.directives());
    }
  }

  /// What `member`, the input value `value` writes where `site` says, holds: its type, its default
  /// and its applications.
  fn input_value<S: AsRef<[u8]>>(
    &mut self,
    site: Site<'_>,
    member: MemberId,
    value: &InputValueDefinition<S>,
  ) {
    let ty = self.ty(site, value.ty(), Position::Type);
    self.builder.set_member_type(member, ty);
    if let Some(default) = value.default_value() {
      let tree = self.value(site, default.value());
      self.values.defaults.insert(member, tree);
    }
    self.member_applications(site, member, value.directives());
  }

  /// The applications `directives` of `member`, written where `site` says.
  fn member_applications<S: AsRef<[u8]>>(
    &mut self,
    site: Site<'_>,
    member: MemberId,
    directives: Option<&ConstDirectives<S>>,
  ) {
    let Some(directives) = directives else {
      return;
    };
    for directive in directives.directives() {
      let application = self.application(site, directive);
      self.builder.push_member_application(member, application);
    }
  }

  /// Lowers the application `directive`, written where `site` says: itself, and, once its
  /// directive resolves, its type arguments, then its arguments. One whose directive names nothing
  /// keeps its path, and nothing written inside it is lowered. Answers its id.
  fn application<S: AsRef<[u8]>>(
    &mut self,
    site: Site<'_>,
    directive: &ConstDirective<S>,
  ) -> ApplicationId {
    let source = site.source.get();
    let name = directive.name();
    let path = name.path();
    // A root of a kind lowering does not know is recorded as none: name resolution reports it,
    // as `unknown-item`, and leaves the application unresolved.
    let root = self.written_path(path).unwrap_or(PathRoot::Relative);
    let application = self.builder.push_application(
      Location::new(source, *directive.span()),
      root,
      &self.segments,
      Location::new(source, *path.span()),
    );
    let request = Request {
      module: site.module,
      source: site.source,
      wanted: Wanted::Directive,
      position: Position::Directive,
      params: site.params,
    };
    let def = match self
      .scopes
      .resolve(&*self.builder, self.reporter, &request, path)
    {
      Resolved::Def(def) => def,
      // Name resolution has reported a path that names nothing; a directive is wanted, which no
      // type, module or parameter is.
      Resolved::NonZero { .. }
      | Resolved::Module(_)
      | Resolved::Param(_)
      | Resolved::Unresolved => return application,
    };
    self.builder.resolve_application(application, def);
    if let Some(generics) = name.type_generics() {
      let from = self.type_args.len();
      for argument in generics.params() {
        let argument = self.ty(site, argument, Position::Type);
        self.type_args.push(argument);
      }
      self
        .builder
        .set_application_type_args(application, self.type_args.get(from..).unwrap_or_default());
      self.type_args.truncate(from);
    }
    if let Some(arguments) = directive.arguments() {
      self.application_arguments(site, application, def, path, arguments.arguments());
    }
    application
  }

  /// The arguments `written`, where `site` says, of `application`, whose directive is
  /// `directive`, named by `path`: each one the directive declares, found by its name, is kept, in
  /// the order of the directive's arguments, and its value read in that order; each one it does
  /// not declare is reported and dropped. One written twice is kept twice, in written order.
  fn application_arguments<S: AsRef<[u8]>>(
    &mut self,
    site: Site<'_>,
    application: ApplicationId,
    directive: DefId,
    path: &Path<S>,
    written: &[ConstArgument<S>],
  ) {
    let source = site.source.get();
    let mut kept: Vec<(MemberId, Location, &ConstInputValue<S>)> =
      Vec::with_capacity(written.len());
    for argument in written {
      let name = argument.name();
      let at = Location::new(source, *name.as_span());
      // An argument's name is interned as a member's is, so a name never interned is no
      // argument's.
      let spelled = String::from_utf8_lossy(name.source().as_ref());
      let declared = self
        .builder
        .find(spelled.as_bytes())
        .and_then(|sym: Sym| self.arguments.get(&(directive, sym)).copied());
      match declared {
        Some(member) => kept.push((member, at, argument.value())),
        None => self.unknown_argument(directive, path, spelled.as_bytes(), at),
      }
    }
    // A stable sort: two of one name keep their written order.
    kept.sort_by_key(|&(member, _, _): &(MemberId, Location, &ConstInputValue<S>)| member);
    for (member, at, value) in kept {
      let tree = self.value(site, value);
      self.values.arguments.push(RawArgument {
        application,
        argument: member,
        name: at,
        tree,
      });
    }
  }

  /// Reports the argument `written`, written at `at`, of an application of `directive`, named by
  /// `path`, which declares no argument of that name: with the nearest name it does declare in the
  /// help, when one is within reach and the analysis's budget for near misses lasts.
  fn unknown_argument<S: AsRef<[u8]>>(
    &mut self,
    directive: DefId,
    path: &Path<S>,
    written: &[u8],
    at: Location,
  ) {
    let builder: &ModelBuilder = &*self.builder;
    let first = (directive, Sym::new(0));
    let last = (directive, Sym::new(u32::MAX));
    let mut candidates = self
      .arguments
      .range(first..=last)
      .map(|(&(_, sym), _): (&(DefId, Sym), &MemberId)| builder.spelling(sym));
    let near: Option<String> = self
      .scopes
      .nearest(written, &mut candidates)
      .map(|spelled: &[u8]| format!("did you mean `{}`?", Excerpt(spelled)));
    let shown: &[u8] = match path.segments().last() {
      Some(last) => last.source().as_ref(),
      None => &[],
    };
    let mut report = Diagnostic::builder(schema::UNDEFINED_DIRECTIVE_ARGUMENT, at)
      .message(format!(
        "the directive `@{}` declares no argument named `{}`",
        Excerpt(shown),
        Excerpt(written)
      ))
      .primary_label("undefined directive argument");
    if let Some(near) = near {
      report = report.help(near);
    }
    self.reporter.report(report.build());
  }

  /// Reads the written value `value`, written where `site` says, into a tree: one node for each
  /// literal it writes, in pre-order, each with its syntactic kind and where it is written, a
  /// number with its radix, a boolean with its value, and a path with its root, as the model
  /// records it and as the resolver reads it, its segments, interned, each with where it is
  /// written, and the generic parameter in scope its first segment names when it has no root.
  /// Walked with an explicit stack. Nothing in it is resolved. Answers the tree.
  fn value<S: AsRef<[u8]>>(&mut self, site: Site<'_>, value: &ConstInputValue<S>) -> u32 {
    let source = site.source.get();
    let root = dense(self.values.nodes.len());
    let mut pending: Vec<Read<'_, S>> = vec![Read::Enter(value)];
    while let Some(next) = pending.pop() {
      let value = match next {
        Read::Enter(value) => value,
        Read::Leave(at) => {
          self.values.close_node(at);
          continue;
        }
      };
      let span = *value.as_span();
      let kind = match value {
        ConstInputValue::Boolean(boolean) => RawKind::Boolean(boolean.value()),
        ConstInputValue::String(_) => RawKind::String,
        ConstInputValue::Float(float) => RawKind::Float {
          hex: matches!(float.source(), LitFloat::Hex(_)),
        },
        ConstInputValue::Int(int) => RawKind::Int(match int.source() {
          LitInt::Decimal(_) => Radix::Decimal,
          LitInt::Hex(_) => Radix::Hex,
          LitInt::Octal(_) => Radix::Octal,
          LitInt::Binary(_) => Radix::Binary,
        }),
        ConstInputValue::Enum(path) => {
          let path = path.source();
          let written = self.written_path(path);
          // `PathRoot` is `#[non_exhaustive]`: a root of a kind lowering does not know is reported,
          // and the path is kept without it.
          if written.is_none() {
            self.unknown(source, span);
          }
          let start = resolve::root(path, site.source);
          let param = match (start, path.segments().first()) {
            (resolve::Root::Relative, Some(first)) => {
              resolve::param(site.params, first.source().as_ref())
            }
            _ => None,
          };
          let spans = path.segments().iter().map(|name: &Name<S>| *name.as_span());
          RawKind::Path(self.values.push_path(
            written,
            start,
            param,
            self.segments.iter().copied().zip(spans),
          ))
        }
        ConstInputValue::Null(_) => RawKind::Null,
        ConstInputValue::List(_) => RawKind::List,
        ConstInputValue::Set(_) => RawKind::Set,
        ConstInputValue::Map(_) => RawKind::Map,
        ConstInputValue::Object(_) => RawKind::Object,
      };
      let at = self.values.push_node(kind, span);
      // What a value holds is entered after it, in written order, so it is pushed in reverse over
      // the entry that closes it.
      match value {
        ConstInputValue::List(list) => {
          pending.push(Read::Leave(at));
          pending.extend(list.values().iter().rev().map(Read::Enter));
        }
        ConstInputValue::Set(set) => {
          pending.push(Read::Leave(at));
          pending.extend(set.values().iter().rev().map(Read::Enter));
        }
        ConstInputValue::Map(map) => {
          pending.push(Read::Leave(at));
          for entry in map.entries().iter().rev() {
            pending.push(Read::Enter(entry.value()));
            pending.push(Read::Enter(entry.key()));
          }
        }
        ConstInputValue::Object(object) => {
          pending.push(Read::Leave(at));
          for field in object.fields().iter().rev() {
            pending.push(Read::Enter(field.value()));
          }
        }
        ConstInputValue::Boolean(_)
        | ConstInputValue::String(_)
        | ConstInputValue::Float(_)
        | ConstInputValue::Int(_)
        | ConstInputValue::Enum(_)
        | ConstInputValue::Null(_) => {}
      }
    }
    self.values.push_tree(root, source, site.module)
  }

  /// The id of the next direct member of the definition the second walk is in.
  fn next_direct(&mut self) -> MemberId {
    let member = MemberId::new(self.direct);
    self.direct = self.direct.saturating_add(1);
    member
  }

  /// The id of the next argument of a field of the definition the second walk is in.
  fn next_argument(&mut self) -> MemberId {
    let member = MemberId::new(self.argument);
    self.argument = self.argument.saturating_add(1);
    member
  }

  /// Lowers the written type `ty`, standing at `position`, where `site` says: one occurrence for
  /// each type written, each before what it nests, its arguments in order and each with everything
  /// it nests before the next. Answers the outermost.
  fn ty<S: AsRef<[u8]>>(&mut self, site: Site<'_>, ty: &Type<S>, position: Position) -> TypeExprId {
    let root = self.occurrence(site, ty, position);
    let mut pending: Vec<Pending<'_, S>> = Vec::new();
    nested(ty, root, self.type_args.len(), &mut pending);
    while let Some(next) = pending.pop() {
      match next {
        Pending::Type { ty, list } => {
          let id = self.occurrence(site, ty, Position::Type);
          match list {
            Some(list) => self.builder.set_type_expr_list(list, id),
            None => self.type_args.push(id),
          }
          nested(ty, id, self.type_args.len(), &mut pending);
        }
        Pending::Args { occurrence, from } => {
          self
            .builder
            .set_type_expr_args(occurrence, self.type_args.get(from..).unwrap_or_default());
          self.type_args.truncate(from);
        }
      }
    }
    root
  }

  /// Appends the occurrence `ty`, standing at `position` where `site` says, and resolves its path:
  /// what it nests is the caller's.
  fn occurrence<S: AsRef<[u8]>>(
    &mut self,
    site: Site<'_>,
    ty: &Type<S>,
    position: Position,
  ) -> TypeExprId {
    let id = self
      .builder
      .push_type_expr(Location::new(site.source.get(), *ty.span()), ty.required());
    if let Type::Path(path) = ty {
      self.path(site, id, path.path(), position);
    }
    id
  }

  /// Lowers a written path with type arguments, which is never non-null: an `implements` entry, a
  /// union member, a bound or a root operation's type, standing at `position` where `site` says.
  fn type_path<S: AsRef<[u8]>>(
    &mut self,
    site: Site<'_>,
    path: &TypePath<S>,
    position: Position,
  ) -> TypeExprId {
    let id = self
      .builder
      .push_type_expr(Location::new(site.source.get(), *path.span()), false);
    self.path(site, id, path.path(), position);
    if let Some(generics) = path.type_generics() {
      let from = self.type_args.len();
      for argument in generics.params() {
        let argument = self.ty(site, argument, Position::Type);
        self.type_args.push(argument);
      }
      self
        .builder
        .set_type_expr_args(id, self.type_args.get(from..).unwrap_or_default());
      self.type_args.truncate(from);
    }
    id
  }

  /// Records `path`, which the occurrence `id` writes, as written, and resolves it once, standing
  /// at `position` where `site` says.
  fn path<S: AsRef<[u8]>>(
    &mut self,
    site: Site<'_>,
    id: TypeExprId,
    path: &Path<S>,
    position: Position,
  ) {
    // A root of a kind lowering does not know is recorded as none: name resolution reports it,
    // as `unknown-item`, and leaves the occurrence unresolved.
    let root = self.written_path(path).unwrap_or(PathRoot::Relative);
    self.builder.set_type_expr_path(
      id,
      root,
      &self.segments,
      Location::new(site.source.get(), *path.span()),
    );
    let request = Request {
      module: site.module,
      source: site.source,
      wanted: Wanted::Type,
      position,
      params: site.params,
    };
    match self
      .scopes
      .resolve(&*self.builder, self.reporter, &request, path)
    {
      Resolved::Def(def) => self.builder.resolve_named(id, def),
      // `NonZeroU32` and its kin name `core`'s `NonZero`, of the primitive they spell. The row is
      // a renamed `NonZero`'s too, so the primitive is noted beside it for instantiation, which
      // expands the alias.
      Resolved::NonZero {
        non_zero,
        primitive,
      } => {
        self.builder.resolve_named(id, non_zero);
        self.builder.note_non_zero(id, primitive);
      }
      Resolved::Param(index) => {
        if let Some(owner) = site.owner {
          self.builder.resolve_param(id, owner, index);
        }
      }
      // Name resolution has reported a path that names nothing; a type is wanted, which no module
      // is.
      Resolved::Module(_) | Resolved::Unresolved => {}
    }
  }

  /// Interns `path`'s segments into `segments`, as written, and answers its root as the model
  /// records it: `None` for a root of a kind lowering does not know, which the caller reports or
  /// leaves to name resolution.
  fn written_path<S: AsRef<[u8]>>(&mut self, path: &Path<S>) -> Option<PathRoot> {
    self.segments.clear();
    for segment in path.segments() {
      let sym = self.intern(segment);
      self.segments.push(sym);
    }
    match path.root() {
      ast::PathRoot::Relative => Some(PathRoot::Relative),
      ast::PathRoot::Package(_) => Some(PathRoot::Package),
      ast::PathRoot::Dependency(_) => Some(PathRoot::Dependency),
      ast::PathRoot::Parent { count, .. } => Some(PathRoot::Parent(
        NonZeroU32::try_from(*count).unwrap_or(NonZeroU32::MAX),
      )),
      // `PathRoot` is `#[non_exhaustive]`.
      _ => None,
    }
  }

  /// Refuses `interface`, the occurrence a bound's right side is, where it is written, unless it
  /// names an interface, a marker among them: a definition of another kind, an alias such as
  /// `NonZeroU32` and a type parameter are `invalid-bound`. One whose path names nothing was
  /// reported by name resolution. The bound is recorded either way.
  fn bound_interface(&mut self, interface: TypeExprId) {
    let Some(occurrence) = self.builder.occurrence(interface) else {
      return;
    };
    let at = occurrence.location;
    let refused = match occurrence.written {
      Written::Named(def) => self.builder.def_kind(def) != Some(DefKind::Interface),
      Written::Param { .. } | Written::List(_) => true,
      Written::Unresolved => false,
    };
    if refused {
      self.invalid_bound(
        at,
        "a bound's right side is an interface or a marker, which the type parameter must \
         implement",
        "name an interface, as `where T: Node` does, or an interface with type arguments, as \
         `where T: Comparable<T>` does",
      );
    }
  }

  /// Reports, at `at`, a bound the model cannot hold: `message` says why, and `help` what to write.
  fn invalid_bound(&mut self, at: Location, message: &'static str, help: &'static str) {
    let kind = DiagnosticKind::InvalidBound;
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(message)
        .primary_label(kind.title())
        .help(help)
        .build(),
    );
  }

  /// What a message writes before the name of `def`: `@` for a directive, as every message quotes
  /// one, and nothing for a type.
  fn sigil(&self, def: DefId) -> &'static str {
    if self.builder.def_kind(def) == Some(DefKind::Directive) {
      "@"
    } else {
      ""
    }
  }

  /// Reports the type parameter list written at `at` on `name`, the definition `def` that replaces
  /// a GraphQL built-in: the built-in keeps its identity, which has no type parameter. A directive
  /// is quoted with its `@`, as every message quotes one.
  fn replaced_generics<S: AsRef<[u8]>>(
    &mut self,
    at: Location,
    def: DefId,
    name: &DefinitionName<S>,
  ) {
    let kind = DiagnosticKind::GenericArity;
    let written: &[u8] = name.name().source().as_ref();
    let sigil = self.sigil(def);
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(format!(
          "`{sigil}{}` replaces a GraphQL built-in and keeps its identity, which has no type \
           parameter",
          Excerpt(written)
        ))
        .primary_label("a built-in has no type parameter")
        .help("remove the type parameter list: a definition that replaces a built-in declares none")
        .build(),
    );
  }

  /// Reports the parameter list written at `at` on an extension of `def`, which names `written`
  /// type parameters where `def` has `count`. A directive is quoted with its `@`.
  fn extension_arity(&mut self, def: DefId, at: Location, written: u32, count: u32) {
    let kind = DiagnosticKind::GenericArity;
    let definition = self.builder.def_location(def);
    let sigil = self.sigil(def);
    let mut report = Diagnostic::builder(kind.code(), at)
      .severity(kind.severity())
      .message(format!(
        "an extension's list names its target's type parameters by position, one name each: \
         `{sigil}{}` has {count}, and this list names {written}",
        Excerpt(self.builder.def_name(def))
      ))
      .primary_label("wrong number of type parameters named")
      .help("name every type parameter of the definition, in order, or write no list");
    // A primitive `core` materialises has no position of its own to point at.
    if !definition.is_entire() {
      report = report.label(definition, "the definition it extends");
    }
    self.reporter.report(report.build());
  }

  /// The schema: its contributions, after every definition's, each schema definition by source id
  /// and position, then each schema extension likewise; then what the root package's first schema
  /// definition and its schema extensions give it, the root package's sources being `root`: the
  /// description, the roots, query, mutation and then subscription, and the applications.
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
    for part in &mut parts {
      part.contribution = if part.extension {
        self
          .builder
          .push_schema_extension(part.location, part.extent)
      } else {
        self
          .builder
          .push_schema_contribution(part.location, part.extent)
      };
    }

    // The root package's first schema definition, which gives the schema its description.
    let first = parts
      .iter()
      .position(|part: &SchemaPart<'_>| !part.extension && root.contains(part.entry.item.source));
    if let Some(at) = first
      && let Some(part) = parts.get(at)
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

    // The roots: presumed, with no schema definition, and then as each item writes them.
    let mut kept: [Option<Kept>; 3] = [None; 3];
    if first.is_none() {
      self.presume(declarations, &mut kept);
    }
    for (index, part) in parts.iter().enumerate() {
      if gives(part, index, first, root) {
        self.schema_roots(declarations, index, part.entry, &mut kept);
      }
    }
    let operations = [
      RootOperation::Query,
      RootOperation::Mutation,
      RootOperation::Subscription,
    ];
    for (operation, slot) in operations.into_iter().zip(kept) {
      match slot {
        Some(Kept::Written { part, index, .. }) => {
          if let Some(written) = parts.get(part)
            && let Some(ty) = self.schema_root(declarations, written.entry, index)
          {
            self.builder.set_schema_root(operation, Root::Written(ty));
          }
        }
        Some(Kept::Presumed { def, .. }) => {
          self.builder.set_schema_root(operation, Root::Presumed(def));
        }
        None => {}
      }
    }

    for (index, part) in parts.iter().enumerate() {
      if gives(part, index, first, root) {
        self.schema_applications(declarations, part.entry, part.contribution);
      }
    }
  }

  /// Gives `kept` the roots GraphQL presumes with no schema definition: for each operation, the
  /// object type named as its default root among the root package's root module's own
  /// declarations, which a `use` item and the prelude give none of.
  fn presume(&mut self, declarations: &Declarations<'_, '_>, kept: &mut [Option<Kept>; 3]) {
    let Some(&module) = declarations.roots.get(ROOT) else {
      return;
    };
    let defaults: [&[u8]; 3] = [b"Query", b"Mutation", b"Subscription"];
    for (slot, name) in kept.iter_mut().zip(defaults) {
      // A name never interned is declared nowhere.
      let Some(sym) = self.builder.find(name) else {
        continue;
      };
      let key = (module, TYPE, sym);
      let Ok(at) = declarations.names.binary_search_by(|binding: &Binding| {
        (binding.module, binding.namespace, binding.name).cmp(&key)
      }) else {
        continue;
      };
      let Some(def) = declarations
        .names
        .get(at)
        .map(|binding: &Binding| binding.def)
      else {
        continue;
      };
      if declarations
        .defs
        .get(def.get() as usize)
        .is_some_and(|declared: &Declaration| declared.kind == DefKind::Object)
      {
        *slot = Some(Kept::Presumed {
          def,
          at: self.builder.def_location(def),
        });
      }
    }
  }

  /// Gives `kept` the roots the schema item `entry`, the schema's contribution `part`, writes, as
  /// written: the first for an operation is kept, and each later one reported.
  fn schema_roots(
    &mut self,
    declarations: &Declarations<'_, '_>,
    part: usize,
    entry: Entry,
    kept: &mut [Option<Kept>; 3],
  ) {
    match declarations.items.get(entry.item.source.get() as usize) {
      Some(Some(Items::Text(items))) => self.roots_in(items, part, entry, kept),
      Some(Some(Items::Bytes(items))) => self.roots_in(items, part, entry, kept),
      // A schema item is one of a source the walk read.
      Some(None) | None => {}
    }
  }

  /// Gives `kept` the roots the schema item `entry` addresses among `items`, the items of its
  /// source, writes: it is the schema's contribution `part`.
  fn roots_in<S: AsRef<[u8]>>(
    &mut self,
    items: &[&TypeSystemItem<S>],
    part: usize,
    entry: Entry,
    kept: &mut [Option<Kept>; 3],
  ) {
    let Some(&item) = items.get(entry.item.index as usize) else {
      return;
    };
    let source = entry.item.source.get();
    let Some(shape) = self.schema_shape(item, source) else {
      return;
    };
    for (index, root) in shape.roots.iter().enumerate() {
      let operation = root.operation_type();
      let slot = match operation {
        ast::OperationType::Query(_) => 0,
        ast::OperationType::Mutation(_) => 1,
        ast::OperationType::Subscription(_) => 2,
        // `OperationType` is `#[non_exhaustive]`: a root of an operation lowering does not know
        // is reported, and dropped.
        _ => {
          self.unknown(source, *root.span());
          continue;
        }
      };
      let at = Location::new(source, *root.name().span());
      let Some(held) = kept.get_mut(slot) else {
        continue;
      };
      match held {
        Some(first) => {
          let first = first.at();
          self.duplicate_root(operation.as_str(), at, first);
        }
        None => *held = Some(Kept::Written { part, index, at }),
      }
    }
  }

  /// Lowers the type of the root operation `index` the schema item `entry` writes.
  fn schema_root(
    &mut self,
    declarations: &Declarations<'_, '_>,
    entry: Entry,
    index: usize,
  ) -> Option<TypeExprId> {
    match declarations.items.get(entry.item.source.get() as usize) {
      Some(Some(Items::Text(items))) => self.root_in(items, entry, index),
      Some(Some(Items::Bytes(items))) => self.root_in(items, entry, index),
      // A schema item is one of a source the walk read.
      Some(None) | None => None,
    }
  }

  /// Lowers the type of the root operation `index` the schema item `entry` addresses among
  /// `items`, the items of its source, writes: resolved where the item is written, with no
  /// generic parameter in scope.
  fn root_in<S: AsRef<[u8]>>(
    &mut self,
    items: &[&TypeSystemItem<S>],
    entry: Entry,
    index: usize,
  ) -> Option<TypeExprId> {
    let &item = items.get(entry.item.index as usize)?;
    let shape = self.schema_shape(item, entry.item.source.get())?;
    let root = shape.roots.get(index)?;
    let site = Site {
      owner: None,
      module: entry.module,
      source: entry.item.source,
      params: &[],
    };
    Some(self.type_path(site, root.name(), Position::RootOperation))
  }

  /// Lowers the applications the schema item `entry`, the schema's contribution `contribution`,
  /// writes.
  fn schema_applications(
    &mut self,
    declarations: &Declarations<'_, '_>,
    entry: Entry,
    contribution: u32,
  ) {
    match declarations.items.get(entry.item.source.get() as usize) {
      Some(Some(Items::Text(items))) => self.applications_in(items, entry, contribution),
      Some(Some(Items::Bytes(items))) => self.applications_in(items, entry, contribution),
      // A schema item is one of a source the walk read.
      Some(None) | None => {}
    }
  }

  /// Lowers the applications the schema item `entry` addresses among `items`, the items of its
  /// source, writes, as the schema's contribution `contribution`'s.
  fn applications_in<S: AsRef<[u8]>>(
    &mut self,
    items: &[&TypeSystemItem<S>],
    entry: Entry,
    contribution: u32,
  ) {
    let Some(&item) = items.get(entry.item.index as usize) else {
      return;
    };
    let Some(shape) = self.schema_shape(item, entry.item.source.get()) else {
      return;
    };
    let Some(directives) = shape.directives else {
      return;
    };
    let site = Site {
      owner: None,
      module: entry.module,
      source: entry.item.source,
      params: &[],
    };
    for directive in directives.directives() {
      let application = self.application(site, directive);
      self
        .builder
        .push_schema_application(contribution, application);
    }
  }

  /// What the schema item `item`, written in the source `source`, gives the schema: `None` for an
  /// item that is no schema item, and for one of a kind lowering does not know, which is reported.
  fn schema_shape<'d, S: AsRef<[u8]>>(
    &mut self,
    item: &'d TypeSystemItem<S>,
    source: u32,
  ) -> Option<SchemaShape<'d, S>> {
    match item {
      TypeSystemItem::Definition(described) => match described.node() {
        TypeSystemDefinition::Schema(schema) => Some(SchemaShape {
          directives: schema.directives(),
          roots: schema
            .root_operation_types_definition()
            .root_operation_type_definitions(),
        }),
        TypeSystemDefinition::Type(_) | TypeSystemDefinition::Directive(_) => None,
      },
      TypeSystemItem::Extension(extension) => match extension {
        TypeSystemExtension::Schema(schema) => Some(SchemaShape {
          directives: schema.directives(),
          roots: match schema.root_operation_types_definition() {
            Some(roots) => roots.root_operation_type_definitions(),
            None => &[],
          },
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

  /// Reports a second root for `operation`, its type written at `at`, the first named at `first`.
  fn duplicate_root(&mut self, operation: &str, at: Location, first: Location) {
    self.reporter.report(
      Diagnostic::builder(schema::DUPLICATE_ROOT_OPERATION_TYPE, at)
        .message(format!(
          "the schema is given a second `{operation}` root operation type"
        ))
        .primary_label("duplicate root operation type")
        .label(first, "first named here")
        .build(),
    );
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
          contribution: 0,
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
          contribution: 0,
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
}

/// Whether the schema item `part`, the schema's contribution `index`, gives the schema anything: it
/// does when it is the root package's first schema definition, `first`, or a schema extension of
/// the root package, whose sources are `root`.
fn gives(
  part: &SchemaPart<'_>,
  index: usize,
  first: Option<usize>,
  root: IdRange<SourceId>,
) -> bool {
  if part.extension {
    root.contains(part.entry.item.source)
  } else {
    first == Some(index)
  }
}

/// Whether `def`, declared as `declaration`, is the definition the root package replaces a GraphQL
/// built-in with: written in the root package's root module, it belongs to the built-in's.
fn replaces_built_in(builder: &ModelBuilder, def: DefId, declaration: &Declaration) -> bool {
  builder.def_module(def) != Some(declaration.module)
}

/// Where the run of `attached`, sorted by target, that extends `def` and starts at `next` ends.
fn run_end(attached: &[Attached], next: usize, def: DefId) -> usize {
  let rest = attached.get(next..).unwrap_or_default();
  next
    + rest
      .iter()
      .take_while(|extension: &&Attached| extension.target == def)
      .count()
}

/// What the second walk reads of an item named `name`, as a definition, or `extension`, as an
/// extension, that writes `implements` and `directives`, and nothing else yet.
fn shape_of<'d, S>(
  name: Option<&'d DefinitionName<S>>,
  extension: Option<&'d ExtensionName<S>>,
  implements: Option<&'d ImplementInterfaces<S>>,
  directives: Option<&'d ConstDirectives<S>>,
) -> Shape<'d, S> {
  Shape {
    name,
    extension,
    clause: None,
    implements,
    union_members: &[],
    directives,
    fields: &[],
    input_values: &[],
    enum_values: &[],
  }
}

/// Queues what the written type `ty`, lowered as `id`, nests: its element, or its arguments, which
/// are gathered from `from` on.
fn nested<'t, S>(ty: &'t Type<S>, id: TypeExprId, from: usize, pending: &mut Vec<Pending<'t, S>>) {
  match ty {
    Type::List(list) => pending.push(Pending::Type {
      ty: list.ty(),
      list: Some(id),
    }),
    Type::Path(path) => {
      if let Some(generics) = path.type_generics() {
        pending.push(Pending::Args {
          occurrence: id,
          from,
        });
        // Popped first to last.
        for argument in generics.params().iter().rev() {
          pending.push(Pending::Type {
            ty: argument,
            list: None,
          });
        }
      }
    }
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
