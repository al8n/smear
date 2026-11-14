//! What the analyzer's layer and extent tests read analyses by: the GraphQLx corpus, the package
//! sets written by hand, each taken from a test that already holds it, a rendering of a model
//! through its public accessors, and [`differ`], which names the first place two analyses part.
//!
//! `analysis_layers.rs` and `analysis_extents.rs` include it with `#[path]`, with
//! `#[allow(dead_code)]` at the include, since each uses a part of it.

use std::path::PathBuf;

use smear::{
  analysis::graphqlx::{
    Analysis, Contribution, Diagnostic, Layout, MemberId, Model, Package, SourceFile, SourceId,
    SourceMap, Sym, Tier,
  },
  diagnostic::Location,
};

/// Every corpus entry whose name starts with `prefix`, as `(name, text)`, in name order.
pub fn corpus(prefix: &str) -> Vec<(String, String)> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpusx");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|error| panic!("the corpus at {} is unreadable: {error}", dir.display()))
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path: &PathBuf| path.extension().is_some_and(|ext| ext == "graphqlx"))
    .collect();
  files.sort();
  let mut entries = Vec::new();
  for path in files {
    let name = path
      .file_name()
      .expect("a corpus entry has a file name")
      .to_string_lossy()
      .to_string();
    if name.starts_with(prefix) {
      let text = std::fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("{name} is unreadable: {error}"));
      entries.push((name, text));
    }
  }
  entries
}

/// Whether `text` holds nothing but what the lexer skips: spaces, tabs, line terminators, commas,
/// byte order marks, and comments, each from its `#` to the end of its line. The analyzer reads
/// such a file as a document with no item, and hands it to no layer.
pub fn blank(text: &[u8]) -> bool {
  let mut rest = text;
  loop {
    match rest {
      [] => return true,
      [b' ' | b'\t' | b'\r' | b'\n' | b',', tail @ ..] => rest = tail,
      [0xEF, 0xBB, 0xBF, tail @ ..] => rest = tail,
      [b'#', tail @ ..] => {
        let end = tail
          .iter()
          .position(|byte: &u8| *byte == b'\n' || *byte == b'\r')
          .unwrap_or(tail.len());
        rest = &tail[end..];
      }
      _ => return false,
    }
  }
}

// ── The package sets written by hand ─────────────────────────────────────────────────────────

/// `analysis_lower.rs`'s root package: three files in two modules, with extensions of definitions
/// and of the schema in each.
pub const APP: [(&str, &str); 3] = [
  (
    "b.graphqlx",
    "\"The root.\"\ntype Query {\n  node(id: ID, depth: Int = 1): Node\n  \"Listed.\"\n  \
     nodes: [Node!]!\n}\ninterface Node {\n  id: ID\n}\n\"The schema.\"\nschema {\n  \
     query: Query\n}\n",
  ),
  (
    "a/inner.graphqlx",
    "enum Color {\n  RED\n  GREEN\n}\nextend enum Color {\n  BLUE\n}\n\
     input Filter<T> {\n  color: Color\n  value: T\n}\n\
     directive @mark(level: Int) repeatable on FIELD_DEFINITION | OBJECT\n\
     type Change @mark(level: 2) {\n  done: Int\n}\nextend schema {\n  mutation: Change\n}\n",
  ),
  (
    "z.graphqlx",
    "extend type Query @a::mark(level: 3) {\n  more(first: Int): Int @a::mark(level: 4)\n}\n\
     scalar Late\nextend schema {\n  subscription: ^::geo::Point\n}\n",
  ),
];

/// `analysis_lower.rs`'s dependency `geo`'s files.
pub const GEO: [(&str, &str); 2] = [
  ("p.graphqlx", "type Point {\n  x: Float\n  y: Float\n}\n"),
  (
    "l.graphqlx",
    "type Line {\n  head: Point\n  tail: Point\n}\n",
  ),
];

/// `analysis_lower.rs`'s dependency `astro`'s file.
pub const ASTRO: [(&str, &str); 1] = [(
  "s.graphqlx",
  "\"A star.\"\ntype Star {\n  name: String\n}\n",
)];

/// `analysis_modules_tree.rs`'s described `mod` holding another, in one file.
pub const NESTED: &str =
  "\"The outer module.\"\nmod outer {\n  mod inner {\n    scalar X\n  }\n}\n";

/// `analysis_modules_tree.rs`'s second file's `mod`.
pub const OTHER: &str = "mod other {\n  scalar Y\n}\n";

/// `analysis_modules_declarations.rs`'s two definitions of one name in each namespace.
pub const TWICE: &str = "scalar X\nscalar X\ndirective @d on FIELD\ndirective @d on FIELD\n\
                         scalar same\ndirective @same on FIELD\n";

/// A package named `name` of `layout` at tier `Std`, holding `files`, `(path, text)`.
pub fn package(
  name: &str,
  layout: Layout,
  files: &[(&'static str, &'static str)],
) -> Package<&'static str> {
  Package::new(name, layout, Tier::Std).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'static str, &'static str)| SourceFile::new(path, text)),
  )
}

/// A package set written by hand, taken from a test CI already runs.
pub struct Set {
  /// What a failure calls it.
  pub name: &'static str,
  pub root: Package<&'static str>,
  pub dependencies: Vec<Package<&'static str>>,
  /// The code of the rule this set is the fixture of in `analysis_rules.rs`, which its analysis
  /// reports; `None` for a set no rule fires on.
  pub rule: Option<&'static str>,
}

/// The package sets written by hand: four whose analyses no rule speaks on, and then the three
/// programs `analysis_rules.rs`'s liveness test makes each rule fire on.
///
/// 1. `analysis_lower.rs`'s program: a `Directory` root of three files in two modules beside two
///    dependencies, with extensions of definitions and of the schema in several files, and schema
///    items. It analyzes with no diagnostic.
/// 2. `analysis_modules_tree.rs`'s inline modules: a described `mod` holding another, in one file,
///    and a `mod` in a second. It analyzes with no diagnostic.
/// 3. `analysis_modules_declarations.rs`'s files that hold only what the lexer skips, an empty one
///    and a comment-only one among them, beside one definition. It analyzes with no diagnostic.
/// 4. Well-formed and refused by the phases: `analysis_lower.rs`'s extension of nothing and its
///    unresolved type, and `analysis_modules_declarations.rs`'s two definitions of one name in each
///    namespace, as three files of one `Flat` package.
/// 5. `analysis_rules.rs`'s root package that defines its schema twice.
/// 6. Its dependency that defines the schema.
/// 7. Its root package that extends a dependency's type.
pub fn sets() -> Vec<Set> {
  vec![
    Set {
      name: "a root of three files in two modules, beside two dependencies",
      root: package("app", Layout::Directory, &APP).with_dependencies(["geo", "astro"]),
      dependencies: vec![
        package("geo", Layout::Flat, &GEO),
        package("astro", Layout::Flat, &ASTRO),
      ],
      rule: None,
    },
    Set {
      name: "inline modules nested twice, one described",
      root: package(
        "app",
        Layout::Directory,
        &[("a.graphqlx", NESTED), ("b.graphqlx", OTHER)],
      ),
      dependencies: Vec::new(),
      rule: None,
    },
    Set {
      name: "files that hold only what the lexer skips",
      root: package(
        "app",
        Layout::Directory,
        &[
          ("empty.graphqlx", ""),
          ("notes.graphqlx", "# Only a comment.\n"),
          (
            "trivia.graphqlx",
            "\u{FEFF} \t\r\n,,\n# a comment, last, with no line end",
          ),
          (
            "docs/README.graphqlx",
            "# What the `docs` directory holds.\n",
          ),
          ("schema.graphqlx", "scalar Real\n"),
        ],
      ),
      dependencies: Vec::new(),
      rule: None,
    },
    Set {
      name: "well-formed and refused by the phases",
      root: package(
        "app",
        Layout::Flat,
        &[
          (
            "extension.graphqlx",
            "extend type Missing {\n  f: Undefined\n}\nscalar Kept\n",
          ),
          (
            "unresolved.graphqlx",
            "type T {\n  bad: Missing<Int>\n  good: Int\n}\n",
          ),
          ("twice.graphqlx", TWICE),
        ],
      ),
      dependencies: Vec::new(),
      rule: None,
    },
    Set {
      name: "a root package that defines its schema twice",
      root: package(
        "app",
        Layout::Flat,
        &[(
          "schema.graphqlx",
          "schema {\n  query: Query\n}\nschema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n",
        )],
      ),
      dependencies: Vec::new(),
      rule: Some("smear::schema::duplicate-schema-definition"),
    },
    Set {
      name: "a dependency that defines the schema",
      root: package(
        "app",
        Layout::Flat,
        &[("schema.graphqlx", "type Query {\n  ok: Int\n}\n")],
      )
      .with_dependencies(["geo"]),
      dependencies: vec![package(
        "geo",
        Layout::Flat,
        &[(
          "geo.graphqlx",
          "schema {\n  query: Point\n}\ntype Point {\n  x: Int\n}\n",
        )],
      )],
      rule: Some("smear::graphqlx::schema-outside-root"),
    },
    Set {
      name: "a root package that extends a dependency's type",
      root: package(
        "app",
        Layout::Flat,
        &[(
          "schema.graphqlx",
          "extend type ^::geo::Point {\n  label: String\n}\n",
        )],
      )
      .with_dependencies(["geo"]),
      dependencies: vec![package(
        "geo",
        Layout::Flat,
        &[("geo.graphqlx", "type Point {\n  x: Int\n}\n")],
      )],
      rule: Some("smear::graphqlx::orphan-extension"),
    },
  ]
}

// ── A model, read through its public accessors ───────────────────────────────────────────────

/// One line of a rendering: one entity of a model, keyed by its id, with its own facts and none of
/// its children's.
pub struct Line {
  /// Where the entity is written: a module's, a contribution's or a member's extent, and a
  /// definition's location. `None` for a module no `mod` declares.
  pub at: Option<Location>,
  /// The entity's key and its facts.
  pub text: String,
}

/// `model` read through its public accessors: a line for each module, definition, contribution
/// and member, each keyed by its id and holding that entity's kind, name, location and extent,
/// and none of its children's.
pub fn render(model: &Model) -> Vec<Line> {
  let mut lines: Vec<Line> = Vec::new();
  for id in model.modules() {
    let module = model.module(id);
    let path: Vec<&str> = module
      .path()
      .iter()
      .map(|segment: &Sym| model.name(*segment))
      .collect::<Vec<&str>>();
    lines.push(Line {
      at: module.extent(),
      text: format!(
        "module {} {:?} {path:?} {:?} {:?}",
        id.get(),
        module.kind(),
        module.location(),
        module.extent()
      ),
    });
  }
  for id in model.defs() {
    let def = model.def(id);
    lines.push(Line {
      at: Some(def.location()),
      text: format!(
        "def {} {:?} {} {} {:?}",
        id.get(),
        def.kind(),
        def.name(),
        def.module().get(),
        def.location()
      ),
    });
    for (index, part) in def.contributions().enumerate() {
      lines.push(contribution(
        &format!("contribution {}.{index}", id.get()),
        part,
      ));
    }
  }
  for (index, part) in model.schema().contributions().enumerate() {
    lines.push(contribution(&format!("schema contribution {index}"), part));
  }
  for id in model.defs() {
    let def = model.def(id);
    let direct: Vec<MemberId> = def
      .fields()
      .into_iter()
      .chain(def.input_fields())
      .chain(def.enum_values())
      .chain(def.arguments())
      .collect::<Vec<MemberId>>();
    for member in &direct {
      lines.push(member_line(model, *member));
    }
    for member in &direct {
      for argument in model.member(*member).arguments() {
        lines.push(member_line(model, argument));
      }
    }
  }
  lines
}

/// The line of the contribution `part`, keyed by `key`.
fn contribution(key: &str, part: Contribution<'_>) -> Line {
  Line {
    at: Some(part.extent()),
    text: format!(
      "{key} {:?} {:?} extension {}",
      part.location(),
      part.extent(),
      part.is_extension()
    ),
  }
}

/// The line of the member `id`.
fn member_line(model: &Model, id: MemberId) -> Line {
  let member = model.member(id);
  Line {
    at: Some(member.extent()),
    text: format!(
      "member {} {:?} {} {:?} {:?}",
      id.get(),
      member.kind(),
      member.name(),
      member.location(),
      member.extent()
    ),
  }
}

// ── Where two analyses part ──────────────────────────────────────────────────────────────────

/// The first place `a` and `b` part, for a failure message: their source maps, source by source;
/// then their diagnostics, at the first index where the two lists part; then their models, at the
/// first line where their renderings part.
pub fn differ(a: &Analysis, b: &Analysis) -> String {
  if a.sources() != b.sources() {
    return sources_part(a.sources(), b.sources());
  }
  let (left, right) = (a.diagnostics(), b.diagnostics());
  if left != right {
    let one: Vec<&Diagnostic> = left.iter().collect::<Vec<&Diagnostic>>();
    let two: Vec<&Diagnostic> = right.iter().collect::<Vec<&Diagnostic>>();
    for index in 0..one.len().max(two.len()) {
      let (x, y) = (one.get(index), two.get(index));
      if x != y {
        return format!("the diagnostics part at {index}: {x:#?} against {y:#?}");
      }
    }
    return format!(
      "the diagnostics part in what they count: {} errors, truncated {}, against {} errors, \
       truncated {}",
      left.error_count(),
      left.truncated(),
      right.error_count(),
      right.truncated()
    );
  }
  if a.model() != b.model() {
    let (one, two) = (render(a.model()), render(b.model()));
    for index in 0..one.len().max(two.len()) {
      let x: Option<&str> = one.get(index).map(|line: &Line| line.text.as_str());
      let y: Option<&str> = two.get(index).map(|line: &Line| line.text.as_str());
      if x != y {
        return format!("the models part at line {index}: {x:?} against {y:?}");
      }
    }
    return String::from("the models part in a table the rendering does not read");
  }
  String::from("they part where no accessor reads: in the diagnostic ceilings they were made under")
}

/// Where two source maps part: the first source whose view differs.
fn sources_part(left: &SourceMap, right: &SourceMap) -> String {
  let shown = |map: &SourceMap, index: u32| -> String {
    if index < map.len() {
      format!("{:?}", map.source(SourceId::new(index)))
    } else {
      String::from("no source")
    }
  };
  for index in 0..left.len().max(right.len()) {
    let (x, y) = (shown(left, index), shown(right, index));
    if x != y {
      return format!("the source maps part at source {index}: {x} against {y}");
    }
  }
  String::from("the source maps part where no view of a source reads")
}
