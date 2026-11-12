//! The declarations: every module and every definition numbered, each module's names checked, and
//! what the phases after this one read of them handed on.
//!
//! # Ids
//!
//! Modules take their [`ModuleId`]s from the tree. Every definition that parsed then takes a
//! [`DefId`], in the order W gives: by package, then module path, which is the `ModuleId` order,
//! then source id, then position. `core`'s root module is the first module of all, and the
//! primitives the program writes come first in it, at `core`'s package source, in the order
//! `bool`, `f32`, `f64`, the signed widths ascending, then the unsigned widths ascending. A
//! definition's row holds its identity alone: its kind, name, module, location, origin, its
//! package's tier when it is a built-in, and its description. Where each one is declared is handed
//! on, and lowering appends its definition contribution from it.
//!
//! # Replaced built-ins
//!
//! In the root package's root module, the first scalar named `Int`, `Float`, `String`, `Boolean`
//! or `ID`, and the first directive named `@deprecated`, `@specifiedBy`, `@oneOf`, `@skip` or
//! `@include`, replaces the built-in of that name: `i32`, `f64`, `string`, `bool`, `id`, or
//! `core`'s directive. The built-in keeps its id, its module, its name and its origin; its
//! location, its description and its definition contribution are the root package's; what smear
//! embeds for it contributes nothing; and nothing is reported. A definition of one of those names
//! anywhere else, or of another kind, is an ordinary declaration. A primitive a root package
//! replaces is materialised whether or not the program writes it.
//!
//! # Names
//!
//! A module names types and directives in two namespaces. Two definitions of one name in one
//! namespace of one module, in one file or in two, keep their ids, and the first binds the name;
//! the second is reported with the code GraphQL's rule has in `smear-schema`, with a label at the
//! first. A supplied package's definition whose name begins with `__`, which introspection
//! reserves, is reported with `smear-schema`'s code too. A type that a module names as one of its
//! child modules is `smear::graphqlx::module-type-collision`.

use std::{collections::BTreeMap, format, string::String, vec, vec::Vec};

use smear_lexer::decode_string_value;
use smear_semantics::duplicates::Duplicates;
use tokora::diagnostic::Location;

use super::{
  builtin::{ALIASES, BUILT_INS, CORE, META_TYPES, Replaced, TYPE, Target, namespace, spelled},
  code::{DiagnosticKind, schema},
  collect::{Collected, Found, Items, Placed},
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{DefId, ModuleId, SourceId, Sym},
  model::{DefKind, ModelBuilder, ModuleKind, Origin, Primitive},
  set::ROOT,
  source::SourceMap,
  tree::{Declared, Node, Tree},
};

/// What item collection hands the phases after it: every item they read, already found and
/// placed, so that none of them walks a document's items again, and the module tree they resolve
/// names in.
///
/// Name resolution reads the modules, the names each module declares and the `use` items; lowering
/// reads the definitions, the extensions and the schema's items. An item is addressed by its
/// source and its index among the source's items, which `items` holds for each source, of
/// whichever parser made its document.
pub(crate) struct Declarations<'d, 'a> {
  /// Each source's items, in the order the walk read them; `None` for a source it did not read.
  pub(crate) items: Vec<Option<Items<'d, 'a>>>,
  /// Each definition's declaration, by `DefId`: where it is written, which lowering appends the
  /// definition's own contribution from and walks for what the definition holds.
  pub(crate) defs: Vec<Declaration>,
  /// The names each module declares, sorted by module, then namespace, then name: one binding per
  /// name, the first declaration's. A built-in package's second names, `Int` to `ID`, are bindings
  /// of its root module, each to the definition it names.
  pub(crate) names: Vec<Binding>,
  /// Every module, by `ModuleId`.
  pub(crate) modules: Vec<ModuleEntry>,
  /// Every module but a root module, sorted by its parent, then its name: what a path's later
  /// segments are looked up in.
  pub(crate) children: Vec<Child>,
  /// Each package's root module, by the package's index.
  pub(crate) roots: Vec<ModuleId>,
  /// Each primitive `core` materialises, with its definition, in `DefId` order: they are the first
  /// definitions of all, so the primitive at index `i` is the definition `i`.
  pub(crate) primitives: Vec<(Primitive, DefId)>,
  /// Every `use` item, by module, then source, then position.
  pub(crate) uses: Vec<Entry>,
  /// Every type and directive extension, by module, then source, then position.
  pub(crate) extensions: Vec<Entry>,
  /// Every schema definition and extension, by module, then source, then position.
  pub(crate) schemas: Vec<Entry>,
}

/// Where a definition is declared: what lowering appends the definition's own contribution from,
/// and the item it walks for what the definition holds.
#[derive(Clone, Copy)]
pub(crate) struct Declaration {
  /// The item that declares it; `None` for a primitive `core` materialises, which writes nothing.
  /// A built-in the root package replaces has the root package's item.
  pub(crate) item: Option<Item>,
  /// The module its declaration is written in, where the paths it writes resolve: its own module,
  /// but the root package's root module for a built-in the root package replaces, and `core`'s
  /// root module for a materialised primitive.
  // Read by lowering when it resolves the paths a contribution writes; until then nothing reads it.
  #[allow(dead_code)]
  pub(crate) module: ModuleId,
  /// Its kind, which an extension of it is checked against: `Scalar` for a materialised
  /// primitive.
  pub(crate) kind: DefKind,
  /// Where its name is written; the whole of `core`'s package source for a materialised
  /// primitive, which has no position of its own.
  pub(crate) location: Location,
  /// From its description, or its first keyword, to its last token; the whole of `core`'s
  /// package source for a materialised primitive.
  pub(crate) extent: Location,
}

/// A module, as name resolution reads it.
#[derive(Clone, Copy)]
pub(crate) struct ModuleEntry {
  /// The index of its package, in id order.
  pub(crate) package: usize,
  /// The module it is inside; `None` for a root module.
  pub(crate) parent: Option<ModuleId>,
  /// Its name, the last segment of its path; `None` for a root module.
  pub(crate) name: Option<Sym>,
  /// Whether it is a root module, a directory's, or one only an inline `mod` declares: a string
  /// source names only a directory's.
  pub(crate) kind: ModuleKind,
  /// Where the first inline `mod` that declares it names it; `None` for a root module and a
  /// directory module.
  pub(crate) declared: Option<Location>,
}

/// A module that is not a root module, found by its parent and its name.
#[derive(Clone, Copy)]
pub(crate) struct Child {
  pub(crate) parent: ModuleId,
  pub(crate) name: Sym,
  pub(crate) module: ModuleId,
}

/// An item a later phase reads: its source, and its index among that source's items.
#[derive(Clone, Copy)]
pub(crate) struct Item {
  pub(crate) source: SourceId,
  pub(crate) index: u32,
}

/// An item of a module: a `use` item, an extension, or a schema definition or extension.
#[derive(Clone, Copy)]
pub(crate) struct Entry {
  pub(crate) module: ModuleId,
  pub(crate) item: Item,
}

/// A name one module declares in one namespace, and the definition it binds.
#[derive(Clone, Copy)]
pub(crate) struct Binding {
  pub(crate) module: ModuleId,
  /// `TYPE` or `DIRECTIVE`.
  pub(crate) namespace: u8,
  pub(crate) name: Sym,
  pub(crate) def: DefId,
  /// Whether the declaration replaces a GraphQL built-in, whose id it binds: such a name shadows
  /// nothing, and is not reported as shadowing.
  pub(crate) replaces: bool,
}

/// Numbers every module of `collected`'s tree and every definition it found, appending them to
/// `builder`, which holds every package and nothing else yet, and checks each module's names,
/// reporting into `reporter`; answers what the later phases read.
pub(crate) fn declarations<'d, 'a>(
  collected: Collected<'d, 'a>,
  sources: &SourceMap,
  builder: &mut ModelBuilder,
  reporter: &mut Reporter,
) -> Declarations<'d, 'a> {
  let Collected {
    tree,
    items,
    mut definitions,
    uses,
    extensions,
    schemas,
    mut written,
  } = collected;
  let modules: Vec<ModuleId> = tree.push(builder);
  let (entries, children, roots) = module_tables(&tree, &modules, builder);
  definitions.sort_by_key(|found: &Found<'d>| order(&modules, found));

  // The built-in packages' root modules' definitions, by package, namespace and name, the first
  // kept: the definitions a root package can replace, and that a second name can name. The key is
  // what a replacement and a second name ask for.
  let mut built: BTreeMap<(usize, u8, &'d [u8]), usize> = BTreeMap::new();
  for (at, found) in definitions.iter().enumerate() {
    let node = tree.node(found.placed.module);
    if node.package < ROOT && node.parent.is_none() {
      built
        .entry((node.package, namespace(found.kind), found.name))
        .or_insert(at);
    }
  }

  // What the root package's root module replaces: for each, the definition that replaces it. The
  // walk found which definitions those are, the first of each name in the root module.
  let mut replacing: Vec<bool> = vec![false; definitions.len()];
  let mut primitives: BTreeMap<(u8, u32), usize> = BTreeMap::new();
  let mut named: BTreeMap<(usize, u8, &'d [u8]), usize> = BTreeMap::new();
  for (at, found) in definitions.iter().enumerate() {
    match found.replaces {
      Some(Replaced::Primitive(primitive)) => {
        written.insert(primitive.rank(), primitive);
        primitives.insert(primitive.rank(), at);
        replacing[at] = true;
      }
      Some(Replaced::Built(package, space, name))
        if built.contains_key(&(package, space, name)) =>
      {
        named.insert((package, space, name), at);
        replacing[at] = true;
      }
      // A built-in the library does not hold is replaced by nothing.
      Some(Replaced::Built(..)) | None => {}
    }
  }

  let mut defs: Vec<Declaration> = Vec::with_capacity(written.len() + definitions.len());
  let mut ids: Vec<Option<DefId>> = vec![None; definitions.len()];
  let mut names: Vec<Binding> = Vec::new();
  let mut text = String::new();

  // The primitives, first in `core`'s root module, which is the first module of all.
  let core = modules[tree.root_of(CORE) as usize];
  let whole = Location::entire(sources.package_sources(CORE).start());
  let mut materialised: BTreeMap<(u8, u32), DefId> = BTreeMap::new();
  let mut primitive_defs: Vec<(Primitive, DefId)> = Vec::with_capacity(written.len());
  for (&rank, &primitive) in &written {
    let name = builder.intern(&spelled(primitive));
    let def = match primitives.get(&rank) {
      Some(&at) => {
        let root_found = &definitions[at];
        let def = builder.push_def(
          core,
          DefKind::Scalar,
          name,
          root_found.location,
          Origin::BuiltIn,
        );
        describe(builder, def, root_found.description, &mut text);
        defs.push(declaration(&modules, root_found));
        ids[at] = Some(def);
        def
      }
      None => {
        let def = builder.push_def(core, DefKind::Scalar, name, whole, Origin::BuiltIn);
        defs.push(Declaration {
          item: None,
          module: core,
          kind: DefKind::Scalar,
          location: whole,
          extent: whole,
        });
        def
      }
    };
    builder.set_intrinsic(def, primitive);
    materialised.insert(rank, def);
    primitive_defs.push((primitive, def));
    names.push(Binding {
      module: core,
      namespace: TYPE,
      name,
      def,
      replaces: false,
    });
  }

  // Every definition found, in id order; a root package's replacing definition takes the id of
  // what it replaces, which comes before it.
  for (at, found) in definitions.iter().enumerate() {
    if replacing[at] {
      continue;
    }
    let module = modules[found.placed.module as usize];
    let package = tree.node(found.placed.module).package;
    let origin = if package >= ROOT {
      Origin::User
    } else if package == CORE
      && found.kind != DefKind::Directive
      && META_TYPES
        .iter()
        .any(|meta: &&str| meta.as_bytes() == found.name)
    {
      Origin::Compat
    } else {
      Origin::BuiltIn
    };
    let replacement = if package < ROOT {
      named
        .get(&(package, namespace(found.kind), found.name))
        .copied()
    } else {
      None
    };
    let declaring: &Found<'d> = match replacement {
      Some(by) => &definitions[by],
      None => found,
    };
    let name = builder.intern(&String::from_utf8_lossy(found.name));
    let def = builder.push_def(module, found.kind, name, declaring.location, origin);
    describe(builder, def, declaring.description, &mut text);
    if package < ROOT {
      builder.set_def_tier(def, BUILT_INS[package].tier);
    }
    defs.push(declaration(&modules, declaring));
    ids[at] = Some(def);
    if let Some(by) = replacement {
      ids[by] = Some(def);
    }
  }

  // The second names, each in its built-in package's root module.
  for (package, alias, target) in ALIASES {
    let def = match target {
      Target::Primitive(primitive) => materialised.get(&primitive.rank()).copied(),
      Target::Named(owner, name) => {
        let key: (usize, u8, &[u8]) = (owner, TYPE, name.as_bytes());
        built.get(&key).and_then(|at: &usize| ids[*at])
      }
    };
    if let Some(def) = def {
      let module = modules[tree.root_of(package) as usize];
      let name = builder.intern(alias);
      builder.push_alias(module, name, def);
      names.push(Binding {
        module,
        namespace: TYPE,
        name,
        def,
        replaces: false,
      });
    }
  }

  // Each module's names, module by module: the first of each name binds it, and each later one is
  // a duplicate.
  let mut types: BTreeMap<(ModuleId, &'d [u8]), usize> = BTreeMap::new();
  let mut start = 0;
  while start < definitions.len() {
    let module = modules[definitions[start].placed.module as usize];
    let len = definitions[start..]
      .iter()
      .take_while(|found: &&Found<'d>| modules[found.placed.module as usize] == module)
      .count();
    let run: &[Found<'d>] = &definitions[start..start + len];
    // What each definition is named by in its module: its namespace and its interned name. Two
    // definitions with one key are two declarations of one name, whatever their kinds; and one
    // name has one symbol, so comparing keys reads no spelling again.
    let keys: Vec<(u8, Sym)> = run
      .iter()
      .map(|found: &Found<'d>| {
        (
          namespace(found.kind),
          builder.intern(&String::from_utf8_lossy(found.name)),
        )
      })
      .collect();
    let mut duplicates = Duplicates::over(run.len(), |at: usize| keys[at]);
    for (offset, found) in run.iter().enumerate() {
      let (space, name) = keys[offset];
      if let Some(first) = duplicates.first(offset, (space, name)) {
        duplicate(reporter, found, &run[first]);
        continue;
      }
      let at = start + offset;
      if let Some(def) = ids[at] {
        names.push(Binding {
          module,
          namespace: space,
          name,
          def,
          replaces: replacing[at],
        });
      }
      if found.kind != DefKind::Directive {
        types.insert((module, found.name), at);
      }
      if tree.node(found.placed.module).package >= ROOT && found.name.starts_with(b"__") {
        reserved(reporter, found);
      }
    }
    start += len;
  }
  // The primitives were pushed first and the second names next, so a name one of them and a
  // definition both bind keeps the earlier binding.
  names.sort_by_key(|binding: &Binding| (binding.module, binding.namespace, binding.name));
  names.dedup_by_key(|binding: &mut Binding| (binding.module, binding.namespace, binding.name));

  // A type and a child module of one name, in one module's scope.
  for node in tree.nodes() {
    let Some(parent) = node.parent else {
      continue;
    };
    if let Some(&at) = types.get(&(modules[parent as usize], node.name)) {
      let whole = Location::entire(sources.package_sources(node.package).start());
      collision(reporter, &definitions[at], node, whole);
    }
  }

  Declarations {
    items,
    defs,
    names,
    modules: entries,
    children,
    roots,
    primitives: primitive_defs,
    uses: placed_entries(&modules, uses),
    extensions: placed_entries(&modules, extensions),
    schemas: placed_entries(&modules, schemas),
  }
}

/// Every module of `tree`, whose nodes `ids` numbers, by id, as name resolution reads it; every
/// module but a root module, sorted by its parent and its name; and each package's root module,
/// by the package's index.
///
/// A module's name was interned when it was numbered, so interning it again finds its symbol and
/// adds none.
fn module_tables(
  tree: &Tree<'_>,
  ids: &[ModuleId],
  builder: &mut ModelBuilder,
) -> (Vec<ModuleEntry>, Vec<Child>, Vec<ModuleId>) {
  let unset = ModuleEntry {
    package: 0,
    parent: None,
    name: None,
    kind: ModuleKind::Root,
    declared: None,
  };
  let mut entries: Vec<ModuleEntry> = vec![unset; ids.len()];
  let mut children: Vec<Child> = Vec::with_capacity(ids.len());
  let mut roots: Vec<ModuleId> = Vec::new();
  // The tree adds each package's root module before anything of the package, packages in id
  // order, so the root modules are met in package order.
  for (node, row) in tree.nodes().iter().enumerate() {
    let id = ids[node];
    let parent: Option<ModuleId> = row.parent.map(|up: u32| ids[up as usize]);
    let name: Option<Sym> =
      parent.map(|_: ModuleId| builder.intern(&String::from_utf8_lossy(row.name)));
    entries[id.get() as usize] = ModuleEntry {
      package: row.package,
      parent,
      name,
      kind: row.kind,
      declared: row.declared.map(|declared: Declared<'_>| declared.location),
    };
    match (parent, name) {
      (Some(parent), Some(name)) => children.push(Child {
        parent,
        name,
        module: id,
      }),
      _ => roots.push(id),
    }
  }
  children.sort_by_key(|child: &Child| (child.parent, child.name));
  (entries, children, roots)
}

/// A definition's place in the `DefId` order: its module, its source, then its position, which its
/// name's is.
fn order(modules: &[ModuleId], found: &Found<'_>) -> (ModuleId, SourceId, usize) {
  let position = found
    .location
    .span()
    .map_or(0, |span: tokora::SimpleSpan| span.start());
  (
    modules[found.placed.module as usize],
    found.placed.source,
    position,
  )
}

/// Gives `def` the description written as `description`, decoded into `text`.
fn describe(
  builder: &mut ModelBuilder,
  def: DefId,
  description: Option<(&[u8], Location)>,
  text: &mut String,
) {
  if let Some((spelling, location)) = description {
    text.clear();
    // A description the lexer read always decodes; one that did not is left absent.
    if decode_string_value(spelling, text).is_ok() {
      builder.set_def_description(def, text.as_str(), location);
    }
  }
}

/// Where `placed` is, as a later phase addresses it.
fn item(placed: Placed) -> Item {
  Item {
    source: placed.source,
    index: placed.item,
  }
}

/// Where `found` declares its definition, whose module ids `modules` holds by node.
fn declaration(modules: &[ModuleId], found: &Found<'_>) -> Declaration {
  Declaration {
    item: Some(item(found.placed)),
    module: modules[found.placed.module as usize],
    kind: found.kind,
    location: found.location,
    extent: found.extent,
  }
}

/// `placed`, each with its module's id, by module, then source, then position.
fn placed_entries(modules: &[ModuleId], placed: Vec<Placed>) -> Vec<Entry> {
  let mut entries: Vec<Entry> = Vec::with_capacity(placed.len());
  for placed in placed {
    entries.push(Entry {
      module: modules[placed.module as usize],
      item: item(placed),
    });
  }
  entries.sort_by_key(|entry: &Entry| (entry.module, entry.item.source, entry.item.index));
  entries
}

/// Reports `found`, a second definition of `first`'s name in `first`'s namespace and module.
fn duplicate(reporter: &mut Reporter, found: &Found<'_>, first: &Found<'_>) {
  let builder = if found.kind == DefKind::Directive {
    Diagnostic::builder(schema::DUPLICATE_DIRECTIVE_DEFINITION, found.location)
      .message(format!(
        "the directive `@{}` is defined twice in one module",
        Excerpt(found.name)
      ))
      .primary_label("duplicate directive definition")
      .help(
        "`repeatable` governs repeated *use*, not repeated definition; a directive is defined \
         once.",
      )
  } else {
    Diagnostic::builder(schema::DUPLICATE_TYPE_NAME, found.location)
      .message(format!(
        "the type `{}` is defined twice in one module",
        Excerpt(found.name)
      ))
      .primary_label("duplicate type definition")
      .help("rename one of the definitions, or make the second an `extend` of the first.")
  };
  reporter.report(builder.label(first.location, "first defined here").build());
}

/// Reports `found`, a supplied package's definition whose name begins with `__`.
fn reserved(reporter: &mut Reporter, found: &Found<'_>) {
  let builder = if found.kind == DefKind::Directive {
    Diagnostic::builder(schema::RESERVED_DIRECTIVE_NAME, found.location)
      .message(format!(
        "the directive `@{}` begins with `__`, which introspection reserves",
        Excerpt(found.name)
      ))
      .primary_label("directive name is reserved for introspection")
  } else {
    Diagnostic::builder(schema::RESERVED_TYPE_NAME, found.location)
      .message(format!(
        "the type `{}` begins with `__`, which introspection reserves",
        Excerpt(found.name)
      ))
      .primary_label("type name is reserved for introspection")
  };
  reporter.report(
    builder
      .help("a name beginning with `__` is reserved for introspection; rename it.")
      .build(),
  );
}

/// Reports `found`, a type its module names as it names its child module `module`, whose package's
/// package source is `whole`.
fn collision(reporter: &mut Reporter, found: &Found<'_>, module: &Node<'_>, whole: Location) {
  let kind = DiagnosticKind::ModuleTypeCollision;
  let mut builder = Diagnostic::builder(kind.code(), found.location)
    .severity(kind.severity())
    .message(format!(
      "the type `{}` has the name of a module beside it",
      Excerpt(found.name)
    ))
    .primary_label(kind.title());
  builder = match (module.declared, module.directory) {
    (Some(declared), _) => builder.label(declared.location, "the module is declared here"),
    (None, Some(directory)) => builder.label(
      whole,
      format!("the directory `{}` is this module", Excerpt(directory)),
    ),
    // A root module is no module's child.
    (None, None) => builder,
  };
  reporter.report(
    builder
      .help("a name in a module's scope names one thing; rename the type or the module")
      .build(),
  );
}
