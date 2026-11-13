//! The GraphQLx analyzer's values: each written value coerced to the type expected where it
//! stands, each value nested in it one of its own; `null`, a list and the list a lone value stands
//! for; integers of every width and radix, read at any length; floats, `id`, `bytes`, `NonZero` and
//! strings; a custom scalar's literal and a value whose type can take none kept as written; the
//! codes a value that does not fit is refused with; and the steps that completing values takes.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationArgument, Cx, DefId, Diagnostic, IntegerProvider, Layout, Limits,
    MemberId, Model, Namespace, Package, PackageId, Packages, Path, PathRoot, SourceFile, SourceId,
    Sym, Tier, TyId, TyKind, ValueId, ValueKind, Visitor, analyze, visit,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// GraphQL's code for a default that does not fit.
const DEFAULT: &str = "smear::schema::invalid-default-value";
/// GraphQL's code for a directive argument's value that does not fit.
const ARGUMENT: &str = "smear::schema::invalid-directive-argument-value";
/// GraphQL's code for a required argument left out or given `null`.
const MISSING: &str = "smear::schema::missing-required-directive-argument";
/// A number out of the range of a width but the 32-bit signed one, or of `f32`.
const WIDTH: &str = "smear::graphqlx::width-overflow";
/// Zero where `NonZero` is expected.
const ZERO: &str = "smear::graphqlx::nonzero-zero";
/// A bound of the limits passed.
const LIMIT: &str = "smear::graphqlx::resource-limit";

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`.
fn analyze_text(text: &str, limits: &Limits) -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], limits)
}

/// The same, where the package's integer provider gives every width from 0 to 65536 bits.
fn analyze_wide(text: &str) -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)])
    .with_integers(IntegerProvider::numo());
  analyze(&root, &[], &Limits::new())
}

/// The root package's one file, `schema.graphqlx`.
fn schema_file(analysis: &Analysis) -> SourceId {
  let sources = analysis.sources();
  analysis
    .model()
    .package(PackageId::new(3))
    .sources()
    .into_iter()
    .find(|id: &SourceId| sources.source(*id).path() == "schema.graphqlx")
    .expect("the root package's file")
}

/// Where the `nth` occurrence, counting from 0, of `needle` is in `text`, the file `source`.
fn at(source: SourceId, text: &str, needle: &str, nth: usize) -> Location {
  let start = text
    .match_indices(needle)
    .nth(nth)
    .map(|(start, _): (usize, &str)| start)
    .unwrap_or_else(|| panic!("`{needle}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + needle.len()))
}

/// Where `literal` is written right after `before`, the first time, in `text`, the file `source`.
fn after(source: SourceId, text: &str, before: &str, literal: &str) -> Location {
  let written = format!("{before}{literal}");
  let start = text
    .find(written.as_str())
    .unwrap_or_else(|| panic!("`{written}` is not written"))
    + before.len();
  Location::new(source.get(), SimpleSpan::new(start, start + literal.len()))
}

/// Each diagnostic's code and primary location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect()
}

/// The definition the root package's `name` names in the type namespace.
fn type_def(model: &Model, name: &str) -> DefId {
  let canonical = format!("^::app::{name}");
  model
    .lookup(Namespace::Type, &canonical)
    .unwrap_or_else(|| panic!("no type `{canonical}`"))
}

/// The input field `name` of the root package's input object `owner`.
fn input_field(model: &Model, owner: &str, name: &str) -> MemberId {
  model
    .def(type_def(model, owner))
    .input_fields()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == name)
    .unwrap_or_else(|| panic!("no input field `{owner}.{name}`"))
}

/// The default of the input field `name` of `owner`.
fn default(model: &Model, owner: &str, name: &str) -> ValueId {
  model
    .member(input_field(model, owner, name))
    .default()
    .unwrap_or_else(|| panic!("`{owner}.{name}` has no default"))
}

/// The type `member` is written with.
fn member_ty(model: &Model, member: MemberId) -> TyId {
  model
    .type_expr(model.member(member).ty().expect("the member has a type"))
    .ty()
}

/// The arguments of the first application on the root package's type `owner`.
fn arguments_of(model: &Model, owner: &str) -> Vec<ApplicationArgument> {
  let application = model.def(type_def(model, owner)).applications()[0];
  model.application(application).arguments().to_vec()
}

/// `path` as written: its root and its segments.
fn spelled(model: &Model, path: Option<Path<'_>>) -> String {
  let Some(path) = path else {
    return String::new();
  };
  let root = match path.root() {
    PathRoot::Relative => String::new(),
    PathRoot::Package => String::from("~::"),
    PathRoot::Dependency => String::from("^::"),
    PathRoot::Parent(levels) => "..::".repeat(levels.get() as usize),
    _ => String::from("?::"),
  };
  let segments: Vec<&str> = path
    .segments()
    .iter()
    .map(|segment: &Sym| model.name(*segment))
    .collect();
  format!("{root}{}", segments.join("::"))
}

/// The value `id`, written out: a coerced value as its kind's name and its text, `Int 1`, `String
/// "q"` with its cooked text quoted, `Enum Color::RED` with its path as written and `List [Int 1]`;
/// a byte string as `Bytes` and its bytes; a literal kept as written as `Literal(Int) 1`; and an
/// unresolved value as `?`.
fn shown(model: &Model, id: ValueId) -> String {
  let value = model.value(id);
  match value.kind() {
    ValueKind::Null => String::from("Null null"),
    ValueKind::Bool(flag) => format!("Boolean {flag}"),
    ValueKind::Int(int) => format!("Int {}", int.text()),
    ValueKind::Float(float) => format!("Float {}", float.text()),
    ValueKind::String(text) => format!("String {text:?}"),
    ValueKind::Bytes(bytes) => format!("Bytes {bytes:?}"),
    ValueKind::Enum { .. } => format!("Enum {}", spelled(model, value.path())),
    ValueKind::List(items) => {
      let items: Vec<String> = items
        .iter()
        .map(|item: &ValueId| shown(model, *item))
        .collect();
      format!("List [{}]", items.join(", "))
    }
    ValueKind::Literal(literal) => format!("Literal({:?}) {}", literal.kind(), literal.text()),
    ValueKind::Unresolved => String::from("?"),
    other => format!("{other:?}"),
  }
}

/// The default of `owner.name`, written out.
fn shown_default(model: &Model, owner: &str, name: &str) -> String {
  shown(model, default(model, owner, name))
}

/// Every value a walk of some packages reaches.
#[derive(Default)]
struct Values {
  ids: Vec<ValueId>,
}

impl Visitor for Values {
  fn value(&mut self, _cx: &mut Cx<'_>, value: ValueId) {
    self.ids.push(value);
  }
}

/// Every value a walk of `packages` reaches, in its order.
fn values(analysis: &Analysis, packages: Packages) -> Vec<ValueId> {
  let mut reached = Values::default();
  let _ = visit(analysis, packages, &mut reached);
  reached.ids
}

const KINDS: &str = "directive @all(b: Boolean, i: Int, f: Float, s: String, d: ID, n: Int, \
                     l: [Int]) on OBJECT\n\
                     type T @all(b: true, i: -42, f: -1.5e3, s: \"two\\nlines\", d: \"x\", \
                     n: null, l: [1, 2]) {\n  f: Int\n}\n";

#[test]
fn each_kind_of_literal_coerces_to_its_type() {
  let analysis = analyze_text(KINDS, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let arguments = arguments_of(model, "T");
  let kinds: Vec<ValueKind<'_>> = arguments
    .iter()
    .map(|argument: &ApplicationArgument| model.value(argument.value()).kind())
    .collect();
  assert!(matches!(kinds[0], ValueKind::Bool(true)));
  let ValueKind::Int(int) = kinds[1] else {
    panic!("`-42` is no integer: {:?}", kinds[1]);
  };
  assert!(int.is_negative());
  assert_eq!(int.to_i128(), Some(-42));
  assert_eq!(int.text(), "-42");
  let ValueKind::Float(float) = kinds[2] else {
    panic!("`-1.5e3` is no float: {:?}", kinds[2]);
  };
  assert_eq!(float.to_f64().to_bits(), (-1500.0f64).to_bits());
  assert_eq!(float.text(), "-1.5e3");
  // The string is cooked: its escape is a real newline.
  assert_eq!(kinds[3], ValueKind::String("two\nlines"));
  assert_eq!(kinds[4], ValueKind::String("x"));
  assert_eq!(kinds[5], ValueKind::Null);
  assert_eq!(shown(model, arguments[6].value()), "List [Int 1, Int 2]");
  // Each is typed with its argument's type, written, and located at its literal.
  let literals = [
    "true",
    "-42",
    "-1.5e3",
    "\"two\\nlines\"",
    "\"x\"",
    "null",
    "[1, 2]",
  ];
  for (argument, literal) in arguments.iter().zip(literals) {
    let value = model.value(argument.value());
    assert_eq!(
      value.ty(),
      member_ty(model, argument.argument()),
      "{literal}"
    );
    assert!(value.written(), "{literal}");
    assert_eq!(value.location(), at(schema, KINDS, literal, 0), "{literal}");
  }
}

#[test]
fn a_values_type_is_the_type_it_was_coerced_against() {
  let text = "input I {\n  l: [Int!]! = [1]\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let list = default(model, "I", "l");
  let ty = member_ty(model, input_field(model, "I", "l"));
  assert_eq!(model.value(list).ty(), ty);
  assert!(model.ty(ty).non_null());
  let TyKind::List { element, .. } = model.ty(ty).kind() else {
    panic!("`[Int!]!` is no list");
  };
  let ValueKind::List(&[item]) = model.value(list).kind() else {
    panic!("`[1]` is no list of one");
  };
  assert_eq!(model.value(item).ty(), element);
  assert!(model.ty(element).non_null());
}

#[test]
fn null_fits_a_nullable_type_and_no_other() {
  let text = "input I {\n  a: Int = null\n  b: Int! = null\n  c: [Int!] = [null]\n  \
              d: [Int]! = null\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (DEFAULT, at(schema, text, "null", 1)),
      (DEFAULT, at(schema, text, "null", 2)),
      (DEFAULT, at(schema, text, "null", 3)),
    ]
  );
  let model = analysis.model();
  assert_eq!(shown_default(model, "I", "a"), "Null null");
  assert_eq!(shown_default(model, "I", "b"), "?");
  assert_eq!(shown_default(model, "I", "c"), "List [?]");
  assert_eq!(shown_default(model, "I", "d"), "?");
  // The error marks the member that holds it, and no sibling.
  assert!(model.member(input_field(model, "I", "b")).has_errors());
  assert!(!model.member(input_field(model, "I", "a")).has_errors());
}

#[test]
fn a_literal_of_the_wrong_kind_does_not_fit() {
  let defaults = "enum Color {\n  RED\n}\ninput I {\n  a: Int = \"1\"\n  b: Int = 1.5\n  \
                  c: Int = true\n  d: Int = [1, \"a\"]\n  e: Int = RED\n  f: Boolean = 7\n  \
                  g: String = 8\n  h: Float = \"2\"\n  i: ID = 2.5\n}\n";
  let arguments = "enum Color {\n  RED\n}\n\
                   directive @k(a: Int, b: Int, c: Int, d: Int, e: Int, f: Boolean, g: String, \
                   h: Float, i: ID) on OBJECT\n\
                   type T @k(a: \"1\", b: 1.5, c: true, d: [1, \"a\"], e: RED, f: 7, g: 8, \
                   h: \"2\", i: 2.5) {\n  f: Int\n}\n";
  let literals = [
    ("\"1\"", 0),
    ("1.5", 0),
    ("true", 0),
    ("[1, \"a\"]", 0),
    ("RED", 1),
    ("7", 0),
    ("8", 0),
    ("\"2\"", 0),
    ("2.5", 0),
  ];
  for (text, code) in [(defaults, DEFAULT), (arguments, ARGUMENT)] {
    let analysis = analyze_text(text, &Limits::new());
    let schema = schema_file(&analysis);
    let expected: Vec<(&str, Location)> = literals
      .iter()
      .map(|&(literal, nth): &(&str, usize)| (code, at(schema, text, literal, nth)))
      .collect();
    assert_eq!(found(&analysis), expected);
  }
}

#[test]
fn a_list_keeps_the_elements_that_fit() {
  let text = "input I {\n  l: [Int] = [1, \"two\", 3]\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(DEFAULT, at(schema, text, "\"two\"", 0))]
  );
  assert_eq!(
    shown_default(analysis.model(), "I", "l"),
    "List [Int 1, ?, Int 3]"
  );
}

#[test]
fn a_lone_value_stands_for_a_list_of_itself() {
  let text = "input I {\n  a: [Int] = 1\n  b: [[Int]] = 2\n  c: [[Int]] = [3]\n  \
              d: [Int] = null\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let schema = schema_file(&analysis);
  assert_eq!(shown_default(model, "I", "a"), "List [Int 1]");
  assert_eq!(shown_default(model, "I", "b"), "List [List [Int 2]]");
  assert_eq!(shown_default(model, "I", "c"), "List [List [Int 3]]");
  assert_eq!(shown_default(model, "I", "d"), "Null null");

  // Each level of `b` is a row at the literal, the outermost first, typed with its level's type.
  let outer = default(model, "I", "b");
  let ty = member_ty(model, input_field(model, "I", "b"));
  let ValueKind::List(&[inner]) = model.value(outer).kind() else {
    panic!("`b` is no list of one");
  };
  let ValueKind::List(&[two]) = model.value(inner).kind() else {
    panic!("`b`'s element is no list of one");
  };
  assert!(outer < inner && inner < two);
  let literal = at(schema, text, "2", 0);
  for id in [outer, inner, two] {
    assert_eq!(model.value(id).location(), literal);
    assert!(model.value(id).written());
  }
  let TyKind::List { element, .. } = model.ty(ty).kind() else {
    panic!("`[[Int]]` is no list");
  };
  let TyKind::List { element: int, .. } = model.ty(element).kind() else {
    panic!("`[Int]` is no list");
  };
  assert_eq!(model.value(outer).ty(), ty);
  assert_eq!(model.value(inner).ty(), element);
  assert_eq!(model.value(two).ty(), int);

  // In `c` the written list is the outer one, and the inner one stands for the lone `3`.
  let written = default(model, "I", "c");
  let ValueKind::List(&[lone]) = model.value(written).kind() else {
    panic!("`c` is no list of one");
  };
  assert_eq!(model.value(written).location(), at(schema, text, "[3]", 0));
  assert_eq!(model.value(lone).location(), at(schema, text, "3", 0));
  assert_eq!(
    model.value(default(model, "I", "d")).ty(),
    member_ty(model, input_field(model, "I", "d"))
  );
}

#[test]
fn a_custom_scalar_keeps_any_literal() {
  let text = "scalar Date\ninput I {\n  a: Date = null\n  b: Date! = null\n  \
              c: Date = [1, { x: RED }]\n  d: Date = \"x\"\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis), [(DEFAULT, at(schema, text, "null", 1))]);
  let model = analysis.model();
  assert_eq!(shown_default(model, "I", "a"), "Null null");
  assert_eq!(shown_default(model, "I", "b"), "?");
  assert_eq!(
    shown_default(model, "I", "c"),
    "Literal(List) [1, { x: RED }]"
  );
  // Nothing the literal holds is a value of its own: the next value is the next default.
  assert_eq!(
    default(model, "I", "d").get(),
    default(model, "I", "c").get() + 1
  );
  assert_eq!(shown_default(model, "I", "d"), "Literal(String) \"x\"");
}

#[test]
fn what_takes_no_value_keeps_its_literal_and_says_nothing() {
  // A type that names a type parameter: an application's argument and a generic input's fields.
  let text = "directive @d<T>(v: T) on OBJECT\ntype Q @d<Int>(v: \"x\") {\n  f: Int\n}\n\
              input I<T> {\n  x: T = 1\n  l: [T] = [1]\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let argument = arguments_of(model, "Q")[0];
  assert_eq!(shown(model, argument.value()), "Literal(String) \"x\"");
  assert_eq!(
    model.value(argument.value()).ty(),
    member_ty(model, argument.argument())
  );
  assert_eq!(shown_default(model, "I", "x"), "Literal(Int) 1");
  assert_eq!(shown_default(model, "I", "l"), "Literal(List) [1]");

  // A type that names nothing: its own error alone, `null` for its non-null form included.
  let text = "input J {\n  a: Missing = 1\n  b: Missing! = null\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        "smear::schema::undefined-type",
        at(schema, text, "Missing", 0)
      ),
      (
        "smear::schema::undefined-type",
        at(schema, text, "Missing", 1)
      ),
    ]
  );
  assert_eq!(shown_default(analysis.model(), "J", "a"), "Literal(Int) 1");
  assert_eq!(
    shown_default(analysis.model(), "J", "b"),
    "Literal(Null) null"
  );

  // An output type: nothing at all from this phase.
  let text = "type Obj {\n  f: Int\n}\ntype Q {\n  f(a: Obj = 1): Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn integers_are_range_checked_against_their_width() {
  let text = "input W {\n  a: i8 = 127\n  b: i8 = -128\n  c: i8 = 128\n  d: i8 = -129\n  \
              e: u8 = 255\n  f: u8 = 256\n  g: u8 = -1\n  h: u8 = -0\n  i: u0 = 0\n  j: u0 = 1\n  \
              k: i0 = 0\n  l: i0 = -1\n  m: i1 = -1\n  n: i1 = 0\n  o: i1 = 1\n  \
              p: u128 = 340282366920938463463374607431768211455\n  \
              q: u128 = 340282366920938463463374607431768211456\n  \
              r: i256 = 57896044618658097711785492504343953926634992332820282019728792003956564819967\n  \
              s: i256 = -57896044618658097711785492504343953926634992332820282019728792003956564819968\n  \
              t: i256 = 57896044618658097711785492504343953926634992332820282019728792003956564819968\n}\n";
  let analysis = analyze_wide(text);
  let codes: Vec<&str> = found(&analysis)
    .iter()
    .map(|&(code, _): &(&str, Location)| code)
    .collect();
  assert_eq!(codes, [WIDTH; 9]);
  let model = analysis.model();
  for refused in ["c", "d", "f", "g", "j", "l", "o", "q", "t"] {
    assert_eq!(shown_default(model, "W", refused), "?", "`{refused}`");
  }
  let int = |name: &str| match model.value(default(model, "W", name)).kind() {
    ValueKind::Int(int) => int,
    other => panic!("`{name}` is no integer: {other:?}"),
  };
  assert_eq!(int("a").to_i128(), Some(127));
  assert_eq!(int("b").to_i128(), Some(-128));
  assert_eq!(int("e").to_u128(), Some(255));
  // `-0` is zero: no magnitude byte, and not negative.
  assert!(!int("h").is_negative());
  assert!(int("h").magnitude().is_empty());
  assert_eq!(int("i").to_u128(), Some(0));
  assert_eq!(int("k").to_i128(), Some(0));
  assert_eq!(int("m").to_i128(), Some(-1));
  assert_eq!(int("n").to_i128(), Some(0));
  assert_eq!(int("p").to_u128(), Some(u128::MAX));
  let mut greatest = vec![0xff; 31];
  greatest.push(0x7f);
  assert_eq!(int("r").magnitude(), greatest.as_slice());
  let mut least = vec![0; 31];
  least.push(0x80);
  assert_eq!(int("s").magnitude(), least.as_slice());
  assert!(int("s").is_negative());
}

#[test]
fn int_keeps_graphqls_rule_and_code() {
  let text = "input I {\n  x: Int = 2147483648\n  w: i32 = 2147483648\n  y: u32 = 4294967296\n  \
              z: Int = -2147483648\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (DEFAULT, after(schema, text, "x: Int = ", "2147483648")),
      (DEFAULT, after(schema, text, "w: i32 = ", "2147483648")),
      (WIDTH, after(schema, text, "y: u32 = ", "4294967296")),
    ]
  );
  assert_eq!(shown_default(analysis.model(), "I", "z"), "Int -2147483648");
}

#[test]
fn every_radix_reads_the_same_integer() {
  let text = "input R {\n  a: Int = 255\n  b: Int = 0xff\n  c: Int = 0xFF\n  d: Int = 0o377\n  \
              e: Int = 0b1111_1111\n  f: Int = 2_5_5\n  g: Int = 0x_f_f\n  h: i8 = -0x80\n  \
              i: u16 = 0x1_0000\n  j: u8 = 0x000000000000000000ff\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(WIDTH, after(schema, text, "i: u16 = ", "0x1_0000"))]
  );
  let model = analysis.model();
  for (name, written) in [
    ("a", "255"),
    ("b", "0xff"),
    ("c", "0xFF"),
    ("d", "0o377"),
    ("e", "0b1111_1111"),
    ("f", "2_5_5"),
    ("g", "0x_f_f"),
    ("j", "0x000000000000000000ff"),
  ] {
    let ValueKind::Int(int) = model.value(default(model, "R", name)).kind() else {
      panic!("`{written}` is no integer");
    };
    assert_eq!(int.to_u128(), Some(255), "`{written}`");
    assert_eq!(int.magnitude(), [255], "`{written}`");
    assert_eq!(int.text(), written);
  }
  assert_eq!(shown_default(model, "R", "h"), "Int -0x80");
}

/// The decimal digits of `2^exponent`, by doubling a vector of digits.
fn power_of_two(exponent: u32) -> String {
  // Little-endian decimal digits.
  let mut digits: Vec<u8> = vec![1];
  for _ in 0..exponent {
    let mut carry = 0;
    for digit in &mut digits {
      let doubled = *digit * 2 + carry;
      *digit = doubled % 10;
      carry = doubled / 10;
    }
    if carry > 0 {
      digits.push(carry);
    }
  }
  digits
    .iter()
    .rev()
    .map(|digit: &u8| char::from(b'0' + digit))
    .collect()
}

#[test]
fn a_decimal_integer_of_any_length_is_converted_exactly() {
  let power = power_of_two(4095);
  // `2^4095` ends with an 8, so one less ends with a 7.
  let mut greatest = power.clone();
  assert_eq!(greatest.pop(), Some('8'));
  greatest.push('7');
  let text = format!(
    "input B {{\n  a: i4096 = {greatest}\n  b: i4096 = {power}\n  c: i4096 = -{power}\n}}\n"
  );
  let analysis = analyze_wide(&text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(WIDTH, after(schema, &text, "b: i4096 = ", &power))]
  );
  let model = analysis.model();
  let ValueKind::Int(a) = model.value(default(model, "B", "a")).kind() else {
    panic!("`2^4095 - 1` is no integer");
  };
  let mut magnitude = vec![0xff; 511];
  magnitude.push(0x7f);
  assert_eq!(a.magnitude(), magnitude.as_slice());
  assert!(!a.is_negative());
  let ValueKind::Int(c) = model.value(default(model, "B", "c")).kind() else {
    panic!("`-2^4095` is no integer");
  };
  let mut magnitude = vec![0; 511];
  magnitude.push(0x80);
  assert_eq!(c.magnitude(), magnitude.as_slice());
  assert!(c.is_negative());
}

#[test]
fn a_literal_too_long_for_its_width_is_refused_by_its_length() {
  let digits = "9".repeat(100_000);
  let text = format!("scalar Any\ninput L {{\n  a: i8 = {digits}\n  b: Any = {digits}\n}}\n");
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(WIDTH, after(schema, &text, "a: i8 = ", &digits))]
  );
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert!(diagnostic.to_string().len() < 1024);
  let model = analysis.model();
  let ValueKind::Literal(literal) = model.value(default(model, "L", "b")).kind() else {
    panic!("a custom scalar's value is no literal");
  };
  assert_eq!(literal.text(), digits);
}

#[test]
fn floats_are_checked_against_their_width() {
  let text = "input F {\n  a: f64 = 1.5\n  b: f64 = 1e308\n  c: f64 = 1e-400\n  d: f64 = 1e309\n  \
              e: f32 = 3.4028234e38\n  f: f32 = 3.5e38\n  \
              g: f32 = 340282356779733661637539395458142568447.0\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (DEFAULT, after(schema, text, "d: f64 = ", "1e309")),
      (WIDTH, after(schema, text, "f: f32 = ", "3.5e38")),
    ]
  );
  let model = analysis.model();
  for (name, written) in [
    ("a", "1.5"),
    ("b", "1e308"),
    ("c", "1e-400"),
    ("e", "3.4028234e38"),
    ("g", "340282356779733661637539395458142568447.0"),
  ] {
    let ValueKind::Float(float) = model.value(default(model, "F", name)).kind() else {
      panic!("`{written}` is no float");
    };
    let nearest: f64 = written.parse().expect("a float");
    assert_eq!(float.to_f64().to_bits(), nearest.to_bits(), "`{written}`");
    assert_eq!(float.text(), written);
  }
  let ValueKind::Float(zero) = model.value(default(model, "F", "c")).kind() else {
    panic!("`1e-400` is no float");
  };
  assert_eq!(zero.to_f64().to_bits(), 0);
}

#[test]
fn an_integer_literal_is_a_float() {
  let long = "1".repeat(400);
  let text = format!("input G {{\n  f: Float = 1\n  g: Float = 0x10\n  h: Float = {long}\n}}\n");
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(DEFAULT, after(schema, &text, "h: Float = ", &long))]
  );
  let model = analysis.model();
  for (name, written, value) in [("f", "1", 1.0f64), ("g", "0x10", 16.0)] {
    let ValueKind::Float(float) = model.value(default(model, "G", name)).kind() else {
      panic!("`{written}` is no float");
    };
    assert_eq!(float.to_f64().to_bits(), value.to_bits());
    assert_eq!(float.text(), written);
  }
}

#[test]
fn a_hexadecimal_float_has_a_decimal_exponent() {
  let text = "input H {\n  a: f64 = 0x1p10\n  b: f64 = 0x1.8p1\n  c: f64 = 0x10p-4\n  \
              d: f64 = 0x1p1f\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(DEFAULT, after(schema, text, "d: f64 = ", "0x1p1f"))]
  );
  let model = analysis.model();
  for (name, value) in [("a", 1024.0f64), ("b", 3.0), ("c", 1.0)] {
    let ValueKind::Float(float) = model.value(default(model, "H", name)).kind() else {
      panic!("`{name}` is no float");
    };
    assert_eq!(float.to_f64().to_bits(), value.to_bits(), "`{name}`");
  }
}

#[test]
fn id_takes_a_string_or_a_32_bit_integer() {
  let text = "input D {\n  a: ID = \"x\"\n  b: ID = 7\n  c: ID = 2147483648\n  d: ID = 1.5\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (DEFAULT, after(schema, text, "c: ID = ", "2147483648")),
      (DEFAULT, after(schema, text, "d: ID = ", "1.5")),
    ]
  );
  let model = analysis.model();
  assert_eq!(shown_default(model, "D", "a"), "String \"x\"");
  assert_eq!(shown_default(model, "D", "b"), "Int 7");
  for name in ["a", "b"] {
    assert_eq!(
      model.value(default(model, "D", name)).ty(),
      member_ty(model, input_field(model, "D", name))
    );
  }
}

#[test]
fn a_bytes_value_is_a_string_or_a_list_of_bytes() {
  let text = "input Y {\n  a: bytes = \"abc\"\n  b: bytes = \"é\"\n  \
              c: bytes = \"\"\"\n    x\n    y\n  \"\"\"\n  d: bytes = [0x89, 0x50, 0x4e, 0x47]\n  \
              e: bytes = []\n  f: bytes = [97, 98, 99]\n  g: bytes = [256]\n  \
              h: bytes = [1, \"a\"]\n  i: bytes = 1\n  j: bytes = null\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (WIDTH, after(schema, text, "g: bytes = [", "256")),
      (DEFAULT, after(schema, text, "h: bytes = [1, ", "\"a\"")),
      (DEFAULT, after(schema, text, "i: bytes = ", "1")),
    ]
  );
  let model = analysis.model();
  let bytes = |name: &str| model.value(default(model, "Y", name)).kind();
  assert_eq!(bytes("a"), ValueKind::Bytes(b"abc"));
  assert_eq!(bytes("b"), ValueKind::Bytes(&[0xc3, 0xa9]));
  assert_eq!(bytes("c"), ValueKind::Bytes(b"x\ny"));
  assert_eq!(bytes("d"), ValueKind::Bytes(&[0x89, 0x50, 0x4e, 0x47]));
  assert_eq!(bytes("e"), ValueKind::Bytes(&[]));
  // Both spellings of one sequence are one value.
  assert_eq!(bytes("f"), bytes("a"));
  assert_eq!(bytes("j"), ValueKind::Null);
}

#[test]
fn non_zero_refuses_zero() {
  let text = "directive @nz(a: NonZeroU32) on OBJECT\ninput Z {\n  a: NonZeroU32 = 0\n  \
              b: NonZeroU32 = 0x0\n  c: NonZeroU32 = -0\n  d: NonZero<u8> = 256\n  \
              e: NonZero<i32> = 2147483648\n  f: NonZeroU32 = 1\n}\n\
              input Float0 {\n  g: NonZero<Float> = 0\n}\n\
              type T @nz(a: 0) {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (ZERO, after(schema, text, "a: NonZeroU32 = ", "0")),
      (ZERO, after(schema, text, "b: NonZeroU32 = ", "0x0")),
      (ZERO, after(schema, text, "c: NonZeroU32 = ", "-0")),
      (WIDTH, after(schema, text, "d: NonZero<u8> = ", "256")),
      (
        DEFAULT,
        after(schema, text, "e: NonZero<i32> = ", "2147483648")
      ),
      (
        "smear::graphqlx::nonzero-argument",
        at(schema, text, "NonZero<Float>", 0)
      ),
      (ZERO, after(schema, text, "@nz(a: ", "0")),
    ]
  );
  let model = analysis.model();
  assert_eq!(shown_default(model, "Z", "f"), "Int 1");
  assert_eq!(shown_default(model, "Float0", "g"), "Literal(Int) 0");
}

#[test]
fn strings_are_cooked() {
  let text = "input S {\n  a: String = \"a\\tb\\u00e9\"\n  \
              b: String = \"\"\"\n      indented\n        more\n      text\n  \"\"\"\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let cooked = |name: &str| model.value(default(model, "S", name)).kind();
  assert_eq!(cooked("a"), ValueKind::String("a\tbé"));
  assert_eq!(cooked("b"), ValueKind::String("indented\n  more\ntext"));
}

#[test]
fn a_required_argument_is_reported_once() {
  let text = "directive @need(a: Int!, b: Int! = 1, c: Int) on OBJECT\n\
              type T @need(a: null) {\n  f: Int\n}\n\
              type U @need(a: 1, b: null) {\n  f: Int\n}\n\
              type V @need(a: 1) {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  // A required argument given `null` is left out, at its name, and its value is not refused too;
  // one with a default is not required, and `null` does not fit its type.
  assert_eq!(
    found(&analysis),
    [
      (MISSING, after(schema, text, "type T @need(", "a")),
      (ARGUMENT, after(schema, text, "b: ", "null")),
    ]
  );
  let model = analysis.model();
  assert_eq!(shown(model, arguments_of(model, "T")[0].value()), "?");
  assert_eq!(arguments_of(model, "V").len(), 1);
}

#[test]
fn deprecated_refuses_null() {
  let text = "type T {\n  f: Int @deprecated(reason: null)\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis), [(ARGUMENT, at(schema, text, "null", 0))]);
}

#[test]
fn a_lone_value_costs_a_step_a_level() {
  let text = "input I {\n  a: [[Int]] = 1\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(2));
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert_eq!(
    shown_default(analysis.model(), "I", "a"),
    "List [List [Int 1]]"
  );

  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(1));
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis), [(LIMIT, at(schema, text, "1", 0))]);
  assert_eq!(shown_default(analysis.model(), "I", "a"), "List [?]");
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert!(
    diagnostic
      .help()
      .is_some_and(|help: &str| help.contains("Limits::with_max_value_steps"))
  );

  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(0));
  assert_eq!(found(&analysis).len(), 1);
  assert_eq!(shown_default(analysis.model(), "I", "a"), "?");
}

#[test]
fn written_values_cost_no_step() {
  let items: Vec<String> = (0..1000).map(|item: u32| item.to_string()).collect();
  let text = format!(
    "input I {{\n  a: [Int] = [{}]\n  b: [[Int]] = [[1], [2]]\n}}\n",
    items.join(", ")
  );
  let analysis = analyze_text(&text, &Limits::new().with_max_value_steps(0));
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn the_built_in_library_is_completed_whatever_the_limit() {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  let analysis = analyze(&root, &[], &Limits::new().with_max_value_steps(0));
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn an_ok_analysis_has_no_unresolved_value() {
  let text = "scalar Date\nenum Color {\n  RED\n}\n\
              directive @all(b: Boolean, l: [[Int]], s: [String!], d: Date) repeatable on OBJECT\n\
              input I<T> {\n  a: Int = 1\n  b: [Int] = 2\n  c: f32 = 0x1p3\n  d: bytes = [1, 2]\n  \
              e: ID = 3\n  f: NonZeroI8 = -1\n  g: T = 1\n  h: Date = { x: [RED] }\n  \
              i: Color = RED\n  j: Float = 7\n  k: String = null\n  l: i16 = 0b11\n}\n\
              type Q @all(b: false, l: 1, s: \"x\", d: 1.5) @all(l: [[1], 2]) {\n  \
              f(a: [u8] = [255, 0]): Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let reached = values(&analysis, Packages::All);
  assert!(!reached.is_empty());
  for id in reached {
    assert!(
      !matches!(model.value(id).kind(), ValueKind::Unresolved),
      "value {} is unresolved",
      id.get()
    );
  }
}

#[test]
fn long_spellings_are_quoted_as_excerpts() {
  // A literal of four kilobytes where an integer is expected.
  let long = "x".repeat(4096);
  let text = format!("input I {{\n  a: Int = \"{long}\"\n}}\n");
  let analysis = analyze_text(&text, &Limits::new());
  assert_eq!(analysis.diagnostics().len(), 1);
  for diagnostic in analysis.diagnostics() {
    assert!(diagnostic.to_string().len() < 1024);
  }

  // A directive and an argument of four kilobytes each, the argument required and given `null`.
  let directive = "d".repeat(4096);
  let argument = "a".repeat(4096);
  let text = format!(
    "directive @{directive}({argument}: Int!) on OBJECT\n\
     type T @{directive}({argument}: null) {{\n  f: Int\n}}\n"
  );
  let analysis = analyze_text(&text, &Limits::new());
  let codes: Vec<&str> = found(&analysis)
    .iter()
    .map(|&(code, _): &(&str, Location)| code)
    .collect();
  assert_eq!(codes, [MISSING]);
  for diagnostic in analysis.diagnostics() {
    assert!(diagnostic.to_string().len() < 1024);
  }
}
