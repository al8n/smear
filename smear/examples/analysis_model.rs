//! What a lowering reads, as running code: two GraphQLx packages built in memory, analyzed, and
//! read back through the model; then a rule of the consumer's own, walked over the same model, and
//! one list of diagnostics for a renderer.
//!
//! It prints what it reads and asserts it, so a model that answers otherwise fails the run.
//!
//! ```text
//! cargo run -p smear --example analysis_model --features analysis
//! ```

use smear::{
  analysis::graphqlx::{
    Cx, DefId, DefKind, Diagnostic, Layout, Limits, MemberId, MemberKind, Model, Namespace,
    Package, PackageKind, Packages, Resolution, SourceFile, SourceId, Tier, TyId, TyKind,
    ValueKind, Visitor, analyze, visit,
  },
  diagnostic::{Code, Diagnose},
};

/// The dependency: an object type, and a scalar its own package binds to a host type.
const GEO: &str = r#"
"""A point on a map."""
type Point {
  x: Float!
  y: Float!
}

"""A coordinate, held in the host's own type."""
scalar Coord @extern(lang: "rust", type: "geo::Coord")
"#;

/// The root package: a generic type with a default, a directive with a default, and the query root
/// that instantiates the one and applies the other.
const APP: &str = r#"
"""A page of results, and where the next one starts."""
type Page<T, C = String> {
  items: [T!]!
  next: C
}

directive @cached(seconds: Int = 60) on FIELD_DEFINITION

type Query {
  points(first: Int = 10): Page<^::geo::Point> @cached
  legacy: Page<^::geo::Coord, Int> @cached(seconds: 5)
}
"#;

/// The code of the consumer's rule.
const RETIRED: Code = Code::new("app::retired-field");

/// A rule of the consumer's own: a field named `legacy` is retired.
struct Retired;

impl Visitor for Retired {
  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    // The model is the analysis's, so the view outlives the borrow of `cx` it is read through.
    let field = cx.model().member(member);
    if field.kind() == MemberKind::Field && field.name() == "legacy" {
      cx.report(
        Diagnostic::builder(RETIRED, field.location())
          .message("`legacy` is retired; read `points`")
          .build(),
      );
    }
  }
}

/// Spells the type `ty` as a code generator names it: a definition by its canonical name, with its
/// arguments in `<…>`; a list in `[…]`; a type parameter by its name; and `!` when it is non-null.
///
/// It recurses once for each level the type nests, and a type nests at most
/// `Limits::max_generic_depth` levels, 64 at the default, so the recursion is bounded.
fn spell(model: &Model, ty: TyId) -> String {
  let ty = model.ty(ty);
  let mut text = match ty.kind() {
    TyKind::Named { def, args, .. } => {
      let mut text = model.canonical_name(def).to_string();
      if !args.is_empty() {
        let args: Vec<String> = args.iter().map(|&arg: &TyId| spell(model, arg)).collect();
        text.push('<');
        text.push_str(&args.join(", "));
        text.push('>');
      }
      text
    }
    TyKind::List { element, .. } => format!("[{}]", spell(model, element)),
    TyKind::Param { def, index, .. } => model
      .def(def)
      .type_params()
      .nth(index as usize)
      .map_or_else(|| format!("#{index}"), |param| param.name().to_string()),
    _ => String::from("?"),
  };
  if ty.non_null() {
    text.push('!');
  }
  text
}

/// The `seconds` argument of the `@cached` application on `field`: its value, and whether the
/// author wrote it rather than the directive's default filling it.
fn seconds(model: &Model, field: MemberId, cached: DefId) -> (Option<i128>, bool) {
  for &id in model.member(field).applications() {
    let application = model.application(id);
    if application.directive() != Resolution::Resolved(cached) {
      continue;
    }
    for argument in application.arguments() {
      if model.member(argument.argument()).name() != "seconds" {
        continue;
      }
      let value = match model.value(argument.value()).kind() {
        ValueKind::Int(int) => int.to_i128(),
        _ => None,
      };
      return (value, argument.written());
    }
  }
  panic!("the field applies `@cached`, with its `seconds`");
}

fn main() {
  let geo =
    Package::new("geo", Layout::Flat, Tier::Std).with_files([SourceFile::new("geo.graphqlx", GEO)]);
  let app = Package::new("app", Layout::Flat, Tier::Std)
    .with_dependencies(["geo"])
    .with_files([SourceFile::new("schema.graphqlx", APP)]);
  // Kept, to pair the analysis with the same texts again below.
  let dependencies = [geo];
  let analysis = analyze(&app, &dependencies, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();

  // The definitions the two packages wrote, in id order: the built-in packages' are passed by.
  let written: Vec<DefId> = model
    .defs()
    .into_iter()
    .filter(|&def: &DefId| {
      let module = model.module(model.def(def).module());
      model.package(module.package()).kind() != PackageKind::BuiltIn
    })
    .collect();
  println!("definitions:");
  for &def in &written {
    println!("  {}", model.canonical_name(def));
  }
  assert_eq!(written.len(), 5);

  // Each field of each object they wrote, and the type it resolved to, defaults filled.
  println!("fields:");
  let (mut points, mut legacy) = (None, None);
  for &def in &written {
    let definition = model.def(def);
    if definition.kind() != DefKind::Object {
      continue;
    }
    for field in definition.fields() {
      let member = model.member(field);
      let occurrence = member.ty().expect("a field has a type");
      let ty = model.type_expr(occurrence).ty();
      let name = member.name();
      println!(
        "  {}.{name}: {}",
        model.canonical_name(def),
        spell(model, ty)
      );
      match name {
        "points" => points = Some((field, occurrence)),
        "legacy" => legacy = Some(field),
        _ => {}
      }
    }
  }
  let (points, occurrence) = points.expect("`Query` declares `points`");
  let legacy = legacy.expect("`Query` declares `legacy`");

  // `points` writes one argument of `Page`, and its type holds two: `C`'s default filled the
  // second, `String`, which is `alloc`'s `string`.
  let page = model
    .lookup(Namespace::Type, "^::app::Page")
    .expect("`app` declares `Page`");
  assert_eq!(model.type_expr(occurrence).written_args().len(), 1);
  let TyKind::Named { def, args, .. } = model.ty(model.type_expr(occurrence).ty()).kind() else {
    panic!("`points` is typed by a named type");
  };
  assert_eq!(def, page);
  assert_eq!(args.len(), 2);
  let TyKind::Named { def: filled, .. } = model.ty(args[1]).kind() else {
    panic!("the filled argument is a named type");
  };
  assert_eq!(filled, model.builtins().string());

  // The applications on those fields: each argument, and whether a default filled it.
  println!("applications:");
  for &def in &written {
    let definition = model.def(def);
    if definition.kind() != DefKind::Object {
      continue;
    }
    for field in definition.fields() {
      let member = model.member(field);
      for &id in member.applications() {
        let application = model.application(id);
        let Resolution::Resolved(directive) = application.directive() else {
          continue;
        };
        let mut arguments: Vec<String> = Vec::new();
        for argument in application.arguments() {
          let value = match model.value(argument.value()).kind() {
            ValueKind::Int(int) => int
              .to_i128()
              .map_or_else(|| String::from("?"), |int: i128| int.to_string()),
            _ => String::from("?"),
          };
          let note = if argument.written() { "" } else { " (default)" };
          let name = model.member(argument.argument()).name();
          arguments.push(format!("{name} = {value}{note}"));
        }
        println!(
          "  {}.{}: @{}({})",
          model.canonical_name(def),
          member.name(),
          model.def(directive).name(),
          arguments.join(", ")
        );
      }
    }
  }
  let cached = model
    .lookup(Namespace::Directive, "@^::app::cached")
    .expect("`app` declares `@cached`");
  assert_eq!(seconds(model, points, cached), (Some(60), false));
  assert_eq!(seconds(model, legacy, cached), (Some(5), true));

  // `Coord`'s host types: each `@extern` its own package writes on it.
  let coord = model
    .lookup(Namespace::Type, "^::geo::Coord")
    .expect("`geo` declares `Coord`");
  println!("externs:");
  let externs: Vec<_> = model.def(coord).externs().collect();
  for host in &externs {
    println!(
      "  {}: {} {} ({:?})",
      model.canonical_name(coord),
      host.lang(),
      host.r#type(),
      host.tier()
    );
  }
  assert_eq!(externs.len(), 1);
  assert_eq!(
    (externs[0].lang(), externs[0].r#type()),
    ("rust", "geo::Coord")
  );

  // The consumer's rule, walked over what the two packages wrote. Its verdict is the consumer's,
  // a conjunction with the analysis's.
  let mut mine = visit(&analysis, Packages::Supplied, &mut Retired);
  assert_eq!(mine.len(), 1);
  let ok = analysis.is_ok() && !mine.has_errors();
  assert!(!ok, "the consumer's rule refuses `legacy`");

  // One list for a renderer: smear's diagnostics and the rule's, in one order, each found again
  // in the caller's texts.
  mine.merge(analysis.diagnostics().clone());
  let texts = analysis.sources().bind(&app, &dependencies);
  println!("diagnostics:");
  let mut retired = 0;
  for diagnostic in &mine {
    let path = texts
      .path(SourceId::new(diagnostic.primary().source()))
      .unwrap_or("?");
    println!("  {path}: {diagnostic}");
    if diagnostic.code() == RETIRED {
      assert_eq!(path, "schema.graphqlx");
      retired += 1;
    }
  }
  assert_eq!(retired, 1);
}
