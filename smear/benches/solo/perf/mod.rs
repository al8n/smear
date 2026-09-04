//! The workloads both perf gates read, and the two sizes each of them declares.
//!
//! # Why two sizes, and why the ratio is the number that matters
//!
//! A single peak-bytes reading catches a regression that is *already large*. It cannot catch a
//! complexity change until the constant has grown past a threshold somebody picked, and by then
//! the shape has been in the tree for however long that took.
//!
//! So every workload here is a function of a size `n`, declares `lo` and `hi = 2 * lo`, and is
//! reported at both. `hi / lo` is then an empirical exponent base: **~2 is linear, ~4 is
//! quadratic**, and a workload that moves from one to the other has changed its law whatever the
//! absolute figure says. That reading is what settled every allocation investigation this
//! repository ran: `flatten`'s possible-set table was proved quadratic by 3.97 / 3.99 / 3.93 per
//! doubling and proved repaired by 1.98 / 2.07, at a point where the absolute number at the small
//! size looked unremarkable in both.
//!
//! The two readings are therefore not redundant. The absolute catches a **constant-factor**
//! regression — an `owner_path(…)` put back on a path that runs per item — with the ratio
//! unmoved. The ratio catches a **complexity** regression with the absolute still small. Neither
//! subsumes the other, and the gate fails on either.
//!
//! # What these measure that `benches/` did not
//!
//! The six criterion benches in this package are *parser* benches. Every allocation defect this
//! repository has found lives in [`Schema::build`] or in the rule engine behind it — the refused
//! `compute_closures` population, the field-coverage name rendering, the
//! `MissingInterfaceField` family, `flatten`'s possible-set table — and **not one of them is on a
//! path any of those six enters**. A gate built over the existing suite would have caught none of
//! them, which is the whole reason this corpus exists rather than a comparator over
//! `criterion`'s output.
//!
//! So the schema and validate families below are the subject and the parse family is the control,
//! in the sense `ci/perf/compare.py` reads them: `parse_type_system` and `schema_real` prepare the
//! *same* fixtures and differ only in whether [`Schema::build`] is inside the measured region, so
//! a delta on both is a parser or lexer change and a delta on one is the builder's.
//!
//! # Where the ceilings come in
//!
//! Two of the schema workloads are deliberately sized **past** a budget the builder enforces —
//! `MAX_INTERFACE_IMPLEMENTATION_MISMATCHES` and `MAX_MISSING_TRANSITIVE_INTERFACES`, both
//! 16 384. Their SDL is `Θ(n)` and the diagnostics they *owe* are `Θ(n²)`, so with the ceiling in
//! place the reported list is flat and the peak tracks the document: **ratio ~2**. Without it the
//! peak tracks the product: **ratio ~4**. Deleting or widening either ceiling is therefore not a
//! subtle regression here — it is a workload changing law, which is exactly the reading this file
//! is built to produce.
//!
//! # The measured region
//!
//! Every workload builds its inputs, then calls `region` with a closure holding the subject and
//! nothing else. Parsing is outside — that is the methodology `MAX_INTERFACE_IMPLEMENTATION_MISMATCHES`'s
//! own table was taken under ("with the document parsed before the instrument is armed"), and it
//! is what makes a schema row a reading about the builder rather than about the parser in front of
//! it. The closure must be re-runnable, because the wall-clock instrument calls it many times.
//!
//! [`Schema::build`]: smear::validator::Schema::build

#![allow(dead_code)]

use std::{fmt::Write as _, hint::black_box};

use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  validator::{Budget, Collect, Diagnostic, Schema, Scratch, validate_executable},
};

/// A closure the instrument wraps: it is handed the subject and decides what to read around it.
///
/// The peak-bytes instrument arms its counter and runs it once; the wall-clock instrument runs it
/// many times and keeps the fastest. Neither knows what is inside, and the workload does not know
/// which is calling.
pub type Region<'a> = dyn FnMut(&mut dyn FnMut()) + 'a;

/// What a delta in this workload can be attributed to, and what it rules out.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Family {
  /// Inside the region: `Schema::build`. Outside: the parse.
  Schema,
  /// Inside the region: `validate_executable`. Outside: the parse and the schema build.
  Validate,
  /// Inside the region: the parser alone. This is the control — see the module header.
  Parse,
}

impl Family {
  pub fn as_str(self) -> &'static str {
    match self {
      Self::Schema => "schema",
      Self::Validate => "validate",
      Self::Parse => "parse",
    }
  }
}

pub struct Workload {
  pub name: &'static str,
  pub family: Family,
  /// The small size. Reported on its own and as the denominator of the ratio.
  pub lo: usize,
  /// The large size. **Always `2 * lo`** — the ratio is read as "per doubling" and stops meaning
  /// that the moment the two stop being a doubling.
  pub hi: usize,
  pub run: fn(usize, &mut Region<'_>),
}

pub const WORKLOADS: &[Workload] = &[
  Workload {
    name: "schema_valid",
    family: Family::Schema,
    lo: 256,
    hi: 512,
    run: schema_valid,
  },
  Workload {
    name: "schema_iface_mismatch",
    family: Family::Schema,
    lo: 192,
    hi: 384,
    run: schema_iface_mismatch,
  },
  Workload {
    name: "schema_transitive",
    family: Family::Schema,
    lo: 160,
    hi: 320,
    run: schema_transitive,
  },
  Workload {
    name: "schema_real",
    family: Family::Schema,
    lo: 4,
    hi: 8,
    run: schema_real,
  },
  Workload {
    name: "validate_abstract",
    family: Family::Validate,
    lo: 64,
    hi: 128,
    run: validate_abstract,
  },
  Workload {
    name: "validate_merge",
    family: Family::Validate,
    lo: 64,
    hi: 128,
    run: validate_merge,
  },
  Workload {
    name: "parse_type_system",
    family: Family::Parse,
    lo: 4,
    hi: 8,
    run: parse_type_system,
  },
  Workload {
    name: "parse_executable",
    family: Family::Parse,
    lo: 64,
    hi: 128,
    run: parse_executable,
  },
  Workload {
    name: "parse_deep_values",
    family: Family::Parse,
    lo: 128,
    hi: 256,
    run: parse_deep_values,
  },
];

/// How deep each value in `parse_deep_values` nests.
///
/// **The syntactic door's nesting ceiling is a fixed 24** (`smear_lexer::limits::MAX_NESTING_DEPTH`)
/// and, unlike the lossless door's, it takes no `with_max_nesting_depth`. So depth is not a
/// scalable axis on this path and there is deliberately no workload here that doubles it: a
/// two-size reading over 8 and 16 levels would be a law about nothing. The axis that *is* scalable
/// is how many deep values one document holds, which is what this workload doubles, and it is the
/// axis the amplification defect was measured on — a value tree released recursively cost 3.18x
/// the caller's tree against 1.08x for the iterative release, at one depth, for every value.
const VALUE_DEPTH: usize = 16;

// ---------------------------------------------------------------------------------------------
// the real corpus
// ---------------------------------------------------------------------------------------------

/// Eight schemas taken from this package's own fixtures, in the order the two `_real` workloads
/// consume them.
///
/// The order is not size-sorted, and that is deliberate: `lo = 4` and `hi = 8` take a prefix, so
/// alternating a larger fixture with a smaller one keeps the first four close to half the total
/// and the ratio close to the linear 2 that a per-schema cost predicts.
///
/// The ratio's *absolute* value here is informational — it is a property of which eight files
/// these are — while its *movement* is the signal, because both sides of every comparison read the
/// same eight. A shape that made the builder's peak depend on how many schemas are alive at once,
/// rather than on the largest, moves it and nothing else here would.
const REAL_SCHEMAS: &[(&str, &str)] = &[
  (
    "axelar",
    include_str!("../../../tests/fixtures/schemas/axelar_schema.graphql"),
  ),
  (
    "aura-finance",
    include_str!("../../../tests/fixtures/schemas/aura-finance_schema.graphql"),
  ),
  (
    "cbridge",
    include_str!("../../../tests/fixtures/schemas/cbridge_schema.graphql"),
  ),
  (
    "arrakis-finance",
    include_str!("../../../tests/fixtures/schemas/arrakis-finance_schema.graphql"),
  ),
  (
    "compound-forks",
    include_str!("../../../tests/fixtures/schemas/compound-forks_schema.graphql"),
  ),
  (
    "badgerdao",
    include_str!("../../../tests/fixtures/schemas/badgerdao_schema.graphql"),
  ),
  (
    "abracadabra",
    include_str!("../../../tests/fixtures/schemas/abracadabra_schema.graphql"),
  ),
  (
    "beefy-finance",
    include_str!("../../../tests/fixtures/schemas/beefy-finance_schema.graphql"),
  ),
];

/// One realistic executable document, repeated with distinct operation names to reach a size.
const EXECUTABLE_UNIT: &str =
  include_str!("../../../tests/fixtures/executables/bench_10_huge_comprehensive.graphql");

// ---------------------------------------------------------------------------------------------
// parsing, outside every measured region
// ---------------------------------------------------------------------------------------------

pub fn parse_sdl(sdl: &str) -> TypeSystemDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .unwrap_or_else(|errors| panic!("a perf corpus SDL does not parse: {errors:?}"))
}

pub fn parse_executable_document(source: &str) -> ExecutableDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(source)
  .unwrap_or_else(|errors| panic!("a perf corpus document does not parse: {errors:?}"))
}

// ---------------------------------------------------------------------------------------------
// the schema family
// ---------------------------------------------------------------------------------------------

/// `n` object types that all resolve, so the reading is the accepting path's own cost.
///
/// This is the row an eager copy per merged item shows up on with no diagnostics anywhere near it:
/// the builder reports nothing, and only the allocator notices.
fn schema_valid(n: usize, region: &mut Region<'_>) {
  let mut sdl = String::with_capacity(n * 128);
  sdl.push_str("type Query { root: T0 }\n");
  for i in 0..n {
    let _ = writeln!(
      sdl,
      "type T{i} {{ a: Int b: String c: Boolean d: [ID!]! e: T{next} f(arg: Int = {i}): Float }}",
      next = (i + 1) % n
    );
  }
  let document = parse_sdl(&sdl);
  region(&mut || {
    let built = Schema::build(&document).expect("the generated schema is valid");
    black_box(&built);
  });
}

/// The `IsValidImplementation` shape `MAX_INTERFACE_IMPLEMENTATION_MISMATCHES` is derived over:
/// one interface of `n` fields, `n` types declaring it and covering none of them.
///
/// `Θ(n)` of SDL, `n²` mismatches owed. Both sizes sit above the 16 384 ceiling (`192² = 36 864`,
/// `384² = 147 456`), so with the ceiling in place the reported list is a constant and the peak
/// tracks the document — **ratio ~2**. Remove the ceiling and the peak tracks the product —
/// **ratio ~4** — which is the 6.03 GB reading that constant exists against.
fn schema_iface_mismatch(n: usize, region: &mut Region<'_>) {
  let mut sdl = String::with_capacity(n * 64);
  sdl.push_str("type Query { ok: Int }\ninterface I {\n");
  for i in 0..n {
    let _ = writeln!(sdl, "  f{i}: ID!");
  }
  sdl.push_str("}\n");
  for i in 0..n {
    let _ = writeln!(sdl, "type T{i} implements I {{ z: Int }}");
  }
  let document = parse_sdl(&sdl);
  region(&mut || {
    let refused = Schema::build(&document).expect_err("the mismatch corpus is refused");
    black_box(&refused);
  });
}

/// The transitivity shape `MAX_MISSING_TRANSITIVE_INTERFACES` is derived over: a hub interface
/// implementing `n` parents, and `n` types declaring the hub and none of the parents.
///
/// `Θ(n)` of SDL, `n²` missing-transitive pairs owed, and the same ceiling arithmetic as
/// `schema_iface_mismatch`. It is a separate row because the two lists have separate budgets by
/// design, so a change that merged them would move exactly one of these two.
fn schema_transitive(n: usize, region: &mut Region<'_>) {
  let mut sdl = String::with_capacity(n * 48);
  sdl.push_str("type Query { ok: Int }\n");
  for i in 0..n {
    let _ = writeln!(sdl, "interface P{i} {{ p: Int }}");
  }
  sdl.push_str("interface Hub implements ");
  for i in 0..n {
    if i > 0 {
      sdl.push_str(" & ");
    }
    let _ = write!(sdl, "P{i}");
  }
  sdl.push_str(" { p: Int }\n");
  for i in 0..n {
    let _ = writeln!(sdl, "type T{i} implements Hub {{ p: Int }}");
  }
  let document = parse_sdl(&sdl);
  region(&mut || {
    let refused = Schema::build(&document).expect_err("the transitivity corpus is refused");
    black_box(&refused);
  });
}

/// The first `n` of [`REAL_SCHEMAS`], built and **retained**, so the peak is what holding `n`
/// schemas costs rather than what building the largest one costs.
///
/// Retention is what makes this a law and not a maximum: without it the peak would be the biggest
/// single build and the ratio would be ~1, which says nothing about `n`.
fn schema_real(n: usize, region: &mut Region<'_>) {
  let documents: Vec<_> = REAL_SCHEMAS[..n]
    .iter()
    .map(|(_, sdl)| parse_sdl(sdl))
    .collect();
  region(&mut || {
    let mut built = Vec::with_capacity(n);
    for document in &documents {
      built.push(Schema::build(document));
    }
    black_box(&built);
  });
}

// ---------------------------------------------------------------------------------------------
// the validate family
// ---------------------------------------------------------------------------------------------

/// `n` object types behind one interface, and a document that names every one of them in an inline
/// fragment on that interface.
///
/// This is the possible-set path: each inline fragment's type condition has to be resolved against
/// the parent's possible objects, and the merge engine has to partition the response names over
/// them. `Θ(n)` of document against an `Θ(n)` possible set, so a table rebuilt per fragment is
/// `Θ(n²)` — the `flatten` shape, whose repair took 128 MB to 517 KB and whose signature at the
/// time was a ratio of ~4 per doubling.
fn validate_abstract(n: usize, region: &mut Region<'_>) {
  let mut sdl = String::with_capacity(n * 96);
  sdl.push_str("type Query { node: Node }\ninterface Node { id: ID! name: String }\n");
  for i in 0..n {
    let _ = writeln!(
      sdl,
      "type T{i} implements Node {{ id: ID! name: String only{i}: Int }}"
    );
  }
  let sdl_document = parse_sdl(&sdl);
  let schema = Schema::build(&sdl_document).expect("the abstract corpus is a schema");

  let mut query = String::with_capacity(n * 64);
  query.push_str("query Abstract { node { id\n");
  for i in 0..n {
    let _ = writeln!(query, "    ... on T{i} {{ id name only{i} }}");
  }
  query.push_str("  }\n}\n");
  let document = parse_executable_document(&query);

  let budget = Budget::default();
  let mut scratch = Scratch::new();
  let mut collected: Vec<Diagnostic<&str>> = Vec::new();
  region(&mut || {
    collected.clear();
    let mut sink = Collect::new(&mut collected);
    let outcome = validate_executable(&schema, &document, &mut scratch, &budget, &mut sink);
    black_box(&outcome);
  });
}

/// `n` fragment definitions on one interface, every one of them spread into the same selection
/// set and every one of them writing the same response names.
///
/// This is draft 5.3.2's engine, the only rule with a working set of its own: it has to group the
/// selections by response name, partition them over the parent's possible objects, and compare
/// each pair that lands in one group. `Theta(n)` of document against a rule whose naive form is
/// `Theta(n^2)` in the group size, so this row is where a merge engine that stopped short-circuiting
/// shows up as a ratio rather than as a number somebody has to recognise.
fn validate_merge(n: usize, region: &mut Region<'_>) {
  const SDL: &str = "type Query { node: Node }\n\
    interface Node { id: ID! name: String }\n\
    type A implements Node { id: ID! name: String }\n\
    type B implements Node { id: ID! name: String }\n\
    type C implements Node { id: ID! name: String }\n\
    type D implements Node { id: ID! name: String }\n";
  let sdl_document = parse_sdl(SDL);
  let schema = Schema::build(&sdl_document).expect("the merge corpus is a schema");

  let mut query = String::with_capacity(n * 64);
  query.push_str("query Merging { node { id\n");
  for i in 0..n {
    let _ = writeln!(query, "    ...f{i}");
  }
  query.push_str("  }\n}\n");
  for i in 0..n {
    let _ = writeln!(query, "fragment f{i} on Node {{ id name alias{i}: name }}");
  }
  let document = parse_executable_document(&query);

  let budget = Budget::default();
  let mut scratch = Scratch::new();
  let mut collected: Vec<Diagnostic<&str>> = Vec::new();
  region(&mut || {
    collected.clear();
    let mut sink = Collect::new(&mut collected);
    let outcome = validate_executable(&schema, &document, &mut scratch, &budget, &mut sink);
    black_box(&outcome);
  });
}

// ---------------------------------------------------------------------------------------------
// the parse family — the control
// ---------------------------------------------------------------------------------------------

/// The first `n` of [`REAL_SCHEMAS`], parsed and retained, and **not** built.
///
/// The deliberate twin of `schema_real`: same fixtures, same `n`, same retention, and the only
/// difference is whether `Schema::build` is inside the region. The pair is what tells a builder
/// regression from a parser one without anybody having to guess.
fn parse_type_system(n: usize, region: &mut Region<'_>) {
  let sources: Vec<&str> = REAL_SCHEMAS[..n].iter().map(|(_, sdl)| *sdl).collect();
  region(&mut || {
    let mut parsed = Vec::with_capacity(n);
    for source in &sources {
      parsed.push(parse_sdl(source));
    }
    black_box(&parsed);
  });
}

/// `n` copies of one realistic operation, renamed so the document is well formed, parsed and
/// retained.
///
/// The second control, over the executable grammar rather than the type-system one, because the
/// two do not share a parser entry point and a change can reach one without the other.
fn parse_executable(n: usize, region: &mut Region<'_>) {
  let mut source = String::with_capacity(EXECUTABLE_UNIT.len() * n + n * 32);
  for i in 0..n {
    let _ = write!(source, "query Copy{i} ");
    source.push_str(strip_leading_operation(EXECUTABLE_UNIT));
    source.push('\n');
  }
  region(&mut || {
    let parsed = parse_executable_document(&source);
    black_box(&parsed);
  });
}

/// Everything from the first `{` of the fixture onward, so a fresh operation name can be put in
/// front of it.
///
/// A fixture whose first definition is not an anonymous or named operation would make this a
/// silent corruption rather than a rename, so it is asserted here and not assumed: the copies must
/// parse, and `parse_executable_document` panics if they do not.
fn strip_leading_operation(unit: &str) -> &str {
  let start = unit
    .find('{')
    .expect("the executable fixture has a selection set");
  &unit[start..]
}

// ---------------------------------------------------------------------------------------------
// the reading, and how it is written down
// ---------------------------------------------------------------------------------------------

/// One workload's pair of readings, in whatever unit the instrument works in.
pub struct Reading {
  pub name: &'static str,
  pub family: Family,
  pub lo_size: usize,
  pub hi_size: usize,
  pub lo: f64,
  pub hi: f64,
}

impl Reading {
  /// `hi / lo`, the per-doubling ratio the gate reads as an exponent base.
  ///
  /// A zero `lo` has no ratio rather than an infinite one; the comparator prints it as `-` and
  /// declines to gate on it, because a workload that allocates nothing at the small size is a
  /// workload whose law this instrument cannot see.
  pub fn ratio(&self) -> Option<f64> {
    (self.lo > 0.0).then(|| self.hi / self.lo)
  }
}

/// Writes the readings as JSON, for `ci/perf/compare.py`.
///
/// Hand-written rather than serialised: this binary installs a counting allocator over its own
/// process, so every dependency it links is a dependency whose allocations the instrument has to
/// stay out of the way of. Sixteen numbers do not need a crate.
pub fn emit_json(instrument: &str, unit: &str, rounds: usize, readings: &[Reading]) -> String {
  let mut out = String::with_capacity(readings.len() * 160 + 128);
  let _ = writeln!(
    out,
    "{{\n  \"instrument\": \"{instrument}\",\n  \"unit\": \"{unit}\",\n  \"rounds\": {rounds},\n  \"workloads\": {{"
  );
  for (index, reading) in readings.iter().enumerate() {
    let comma = if index + 1 == readings.len() { "" } else { "," };
    let _ = writeln!(
      out,
      "    \"{name}\": {{ \"family\": \"{family}\", \"lo_size\": {lo_size}, \"hi_size\": {hi_size}, \"lo\": {lo:.6}, \"hi\": {hi:.6} }}{comma}",
      name = reading.name,
      family = reading.family.as_str(),
      lo_size = reading.lo_size,
      hi_size = reading.hi_size,
      lo = reading.lo,
      hi = reading.hi,
    );
  }
  out.push_str("  }\n}\n");
  out
}

/// Prints the same readings as a table, so a local run says something without a comparator.
pub fn print_table(unit: &str, readings: &[Reading]) {
  let width = readings.iter().map(|r| r.name.len()).max().unwrap_or(8);
  println!(
    "\n{:<width$}  {:>8}  {:>16}  {:>16}  {:>7}",
    "workload",
    "family",
    format!("lo ({unit})"),
    format!("hi ({unit})"),
    "hi/lo",
    width = width
  );
  println!("{}", "-".repeat(width + 56));
  for reading in readings {
    let ratio = match reading.ratio() {
      Some(value) => format!("{value:.3}"),
      None => "-".to_owned(),
    };
    println!(
      "{:<width$}  {:>8}  {:>16.0}  {:>16.0}  {:>7}",
      reading.name,
      reading.family.as_str(),
      reading.lo,
      reading.hi,
      ratio,
      width = width
    );
  }
  println!();
}

/// Where to write the JSON, read out of `--json <path>`.
///
/// `--bench` is accepted and ignored: `cargo bench` passes it to every `harness = false` target,
/// and a binary that rejected it would fail the moment somebody ran `cargo bench` rather than the
/// gate. Anything else unrecognised is an error, because a mistyped flag that is silently dropped
/// is a run that measured something other than what was asked for.
pub fn json_destination(args: &[String]) -> Option<String> {
  let mut destination = None;
  let mut index = 0;
  while index < args.len() {
    match args[index].as_str() {
      "--json" => {
        index += 1;
        destination = Some(
          args
            .get(index)
            .unwrap_or_else(|| panic!("--json needs a path"))
            .clone(),
        );
      }
      // Consumed by `rounds` below; its value is skipped here so it cannot be read as a
      // positional argument, which is what turned a valid `--rounds 5` into a panic once.
      "--rounds" => index += 1,
      "--bench" | "--nocapture" => {}
      other => panic!("unrecognised argument: {other}"),
    }
    index += 1;
  }
  destination
}

/// `--rounds <n>`, defaulting to `default`.
pub fn rounds(args: &[String], default: usize) -> usize {
  let mut index = 0;
  while index < args.len() {
    if args[index] == "--rounds" {
      return args
        .get(index + 1)
        .and_then(|value| value.parse().ok())
        .unwrap_or_else(|| panic!("--rounds needs a positive integer"));
    }
    index += 1;
  }
  default
}

/// Settles the once-per-process work no workload should be charged for.
///
/// `SchemaBuilder`'s built-in SDL is parsed once and cached in a `OnceLock` under `std`, so
/// without this the first schema workload to run would carry that parse and the rest would not —
/// a reading that depends on the order of the table.
pub fn warm_up() {
  let document = parse_sdl("type Query { ok: Int }");
  let schema = Schema::build(&document).expect("the warm-up SDL is a schema");
  black_box(&schema);
}

/// `n` arguments, each holding a list nested [`VALUE_DEPTH`] levels deep, parsed and retained.
///
/// The row for the amplification the iterative value rewrite exists against: what it costs to
/// *hold* a materialised value tree, relative to the source that describes it. A build or a
/// release that walks recursively, or that copies a level per level, moves this absolute without
/// moving its ratio — which is the pair of readings the two-size design is for.
fn parse_deep_values(n: usize, region: &mut Region<'_>) {
  let open = "[".repeat(VALUE_DEPTH);
  let close = "]".repeat(VALUE_DEPTH);
  let mut source = String::with_capacity(n * (VALUE_DEPTH * 2 + 24));
  source.push_str("query Values { f(");
  for i in 0..n {
    let _ = write!(source, "a{i}: {open}{i}{close} ");
  }
  source.push_str(") { x } }\n");
  region(&mut || {
    let parsed = parse_executable_document(&source);
    black_box(&parsed);
  });
}
