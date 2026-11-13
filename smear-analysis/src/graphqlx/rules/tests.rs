//! The rule-set macro, over the set smear ships and over a set of two rules that only these tests
//! declare.

use core::cell::RefCell;
use std::{rc::Rc, vec::Vec};

use tokora::diagnostic::{Code, Diagnose, Location};

use super::Rules;
use crate::graphqlx::{
  Cx, Diagnostic, Layout, Limits, ModuleId, Package, Packages, Tier, Visitor, analyze, visit,
};

/// The first rule's code.
const FIRST: Code = Code::new("smear::graphqlx::test-first");
/// The second rule's code.
const SECOND: Code = Code::new("smear::graphqlx::test-second");
/// A code the second rule lists and does not report.
const AGAIN: Code = Code::new("smear::graphqlx::test-again");

/// What the rules saw, in the order they saw it: which rule, and which hook.
type Log = Rc<RefCell<Vec<(&'static str, &'static str)>>>;

/// Writes down `begin`, `enter_module` and `end`, and reports its code at each module.
#[derive(Default)]
struct First {
  log: Log,
}

impl Visitor for First {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.log.borrow_mut().push(("First", "begin"));
  }

  fn enter_module(&mut self, cx: &mut Cx<'_>, _module: ModuleId) {
    self.log.borrow_mut().push(("First", "enter_module"));
    cx.report(Diagnostic::builder(FIRST, Location::entire(0)).build());
  }

  fn end(&mut self, _cx: &mut Cx<'_>) {
    self.log.borrow_mut().push(("First", "end"));
  }
}

/// Writes down `begin`, `enter_module` and `end`, and reports its code at each module.
#[derive(Default)]
struct Second {
  log: Log,
}

impl Visitor for Second {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.log.borrow_mut().push(("Second", "begin"));
  }

  fn enter_module(&mut self, cx: &mut Cx<'_>, _module: ModuleId) {
    self.log.borrow_mut().push(("Second", "enter_module"));
    cx.report(Diagnostic::builder(SECOND, Location::entire(0)).build());
  }

  fn end(&mut self, _cx: &mut Cx<'_>) {
    self.log.borrow_mut().push(("Second", "end"));
  }
}

rule_set! {
  /// Two rules, the first before the second.
  struct Pair {
    first: First => [FIRST],
    second: Second => [SECOND, AGAIN],
  }
}

/// The set smear ships lists each of its rules by its type's name, in field order, with the codes
/// it reports.
#[test]
fn the_set_smear_ships_lists_each_rule_with_its_codes() {
  let table: &[(&str, &[Code])] = Rules::TABLE;
  assert_eq!(table.len(), 2);
  assert_eq!(table[0].0, "DuplicateSchemaDefinition");
  assert_eq!(
    table[0].1,
    &[Code::new("smear::schema::duplicate-schema-definition")]
  );
  assert_eq!(table[1].0, "SchemaOutsideRoot");
  assert_eq!(
    table[1].1,
    &[Code::new("smear::graphqlx::schema-outside-root")]
  );
}

/// A set's table lists each rule by its type's name, in field order, with the codes it lists.
#[test]
fn a_set_lists_each_rule_with_its_codes_in_field_order() {
  let table: &[(&str, &[Code])] = Pair::TABLE;
  assert_eq!(table.len(), 2);
  assert_eq!(table[0].0, "First");
  assert_eq!(table[0].1, &[FIRST]);
  assert_eq!(table[1].0, "Second");
  assert_eq!(table[1].1, &[SECOND, AGAIN]);
}

/// A set's every hook calls each rule's, in field order, and what each reports is kept.
#[test]
fn a_set_calls_each_rule_in_field_order_and_keeps_what_each_reports() {
  let log: Log = Log::default();
  let mut pair = Pair {
    first: First {
      log: Rc::clone(&log),
    },
    second: Second {
      log: Rc::clone(&log),
    },
  };
  let root: Package<&str> = Package::new("app", Layout::Directory, Tier::Std);
  let analysis = analyze(&root, &[], &Limits::new());

  // Over the supplied packages, the walk enters the root package's root module and nothing else.
  let diagnostics = visit(&analysis, Packages::Supplied, &mut pair);
  assert_eq!(
    *log.borrow(),
    [
      ("First", "begin"),
      ("Second", "begin"),
      ("First", "enter_module"),
      ("Second", "enter_module"),
      ("First", "end"),
      ("Second", "end"),
    ]
  );
  let codes: Vec<Code> = diagnostics
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.code())
    .collect::<Vec<Code>>();
  assert_eq!(codes, [FIRST, SECOND]);
}
