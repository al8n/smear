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
  assert_eq!(table.len(), 33);
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
  assert_eq!(table[2].0, "OrphanExtension");
  assert_eq!(
    table[2].1,
    &[
      Code::new("smear::graphqlx::orphan-extension"),
      Code::new("smear::schema::introspection-type-extension"),
    ]
  );
  assert_eq!(table[3].0, "GenericEnum");
  assert_eq!(table[3].1, &[Code::new("smear::graphqlx::generic-enum")]);
  assert_eq!(table[4].0, "MarkerMisuse");
  assert_eq!(table[4].1, &[Code::new("smear::graphqlx::marker-misuse")]);
  assert_eq!(table[5].0, "CollectionArity");
  assert_eq!(
    table[5].1,
    &[Code::new("smear::graphqlx::collection-arity")]
  );
  assert_eq!(table[6].0, "TypeRefMisuse");
  assert_eq!(table[6].1, &[Code::new("smear::graphqlx::typeref-misuse")]);
  assert_eq!(table[7].0, "NonZeroArgument");
  assert_eq!(
    table[7].1,
    &[Code::new("smear::graphqlx::nonzero-argument")]
  );
  assert_eq!(table[8].0, "DuplicateTypeParameter");
  assert_eq!(
    table[8].1,
    &[Code::new("smear::graphqlx::duplicate-type-parameter")]
  );
  assert_eq!(table[9].0, "ReservedName");
  assert_eq!(
    table[9].1,
    &[
      Code::new("smear::schema::reserved-field-name"),
      Code::new("smear::schema::reserved-argument-name"),
      Code::new("smear::schema::reserved-enum-value-name"),
      Code::new("smear::schema::reserved-input-field-name"),
      Code::new("smear::schema::reserved-directive-argument-name"),
    ]
  );
  assert_eq!(table[10].0, "DuplicateMember");
  assert_eq!(
    table[10].1,
    &[
      Code::new("smear::schema::duplicate-field-name"),
      Code::new("smear::schema::duplicate-argument-name"),
      Code::new("smear::schema::duplicate-enum-value"),
      Code::new("smear::schema::duplicate-input-field-name"),
      Code::new("smear::schema::duplicate-directive-argument-name"),
    ]
  );
  assert_eq!(table[11].0, "EmptyDefinition");
  assert_eq!(
    table[11].1,
    &[
      Code::new("smear::schema::empty-fields-definition"),
      Code::new("smear::schema::empty-union-members"),
      Code::new("smear::schema::empty-enum-values"),
      Code::new("smear::schema::empty-input-fields"),
    ]
  );
  assert_eq!(table[12].0, "MemberTypeKind");
  assert_eq!(
    table[12].1,
    &[
      Code::new("smear::schema::field-type-not-output-type"),
      Code::new("smear::schema::argument-type-not-input-type"),
      Code::new("smear::schema::input-field-type-not-input-type"),
      Code::new("smear::schema::directive-argument-type-not-input-type"),
    ]
  );
  assert_eq!(table[13].0, "ImplementsNonInterface");
  assert_eq!(
    table[13].1,
    &[Code::new("smear::schema::implements-non-interface")]
  );
  assert_eq!(table[14].0, "SelfImplementingInterface");
  assert_eq!(
    table[14].1,
    &[Code::new("smear::schema::self-implementing-interface")]
  );
  assert_eq!(table[15].0, "UnionMemberNotObject");
  assert_eq!(
    table[15].1,
    &[Code::new("smear::schema::union-member-not-object")]
  );
  assert_eq!(table[16].0, "DeprecatedRequired");
  assert_eq!(
    table[16].1,
    &[
      Code::new("smear::schema::deprecated-required-argument"),
      Code::new("smear::schema::deprecated-required-input-field"),
    ]
  );
  assert_eq!(table[17].0, "OneOfFieldNotNullable");
  assert_eq!(
    table[17].1,
    &[Code::new("smear::schema::one-of-field-not-nullable")]
  );
  assert_eq!(table[18].0, "OneOfFieldHasDefault");
  assert_eq!(
    table[18].1,
    &[Code::new("smear::schema::one-of-field-has-default")]
  );
  assert_eq!(table[19].0, "OneOfOnInputObjectExtension");
  assert_eq!(
    table[19].1,
    &[Code::new("smear::schema::one-of-on-input-object-extension")]
  );
  assert_eq!(table[20].0, "UnsupportedDirectiveLocation");
  assert_eq!(
    table[20].1,
    &[Code::new("smear::schema::unsupported-directive-location")]
  );
  assert_eq!(table[21].0, "DuplicateDirectiveUse");
  assert_eq!(
    table[21].1,
    &[Code::new("smear::schema::duplicate-directive-use")]
  );
  assert_eq!(table[22].0, "DuplicateDirectiveArgumentUse");
  assert_eq!(
    table[22].1,
    &[Code::new("smear::schema::duplicate-directive-argument-use")]
  );
  assert_eq!(table[23].0, "SelfReferentialDirective");
  assert_eq!(
    table[23].1,
    &[Code::new("smear::schema::self-referential-directive")]
  );
  assert_eq!(table[24].0, "SpecifiedByOnBuiltInScalar");
  assert_eq!(
    table[24].1,
    &[Code::new("smear::schema::specified-by-on-built-in-scalar")]
  );
  assert_eq!(table[25].0, "RootOperationTypeNotObject");
  assert_eq!(
    table[25].1,
    &[Code::new("smear::schema::root-operation-type-not-object")]
  );
  assert_eq!(table[26].0, "SharedRootOperationType");
  assert_eq!(
    table[26].1,
    &[Code::new("smear::schema::shared-root-operation-type")]
  );
  assert_eq!(table[27].0, "SchemaExtensionWithoutSchema");
  assert_eq!(
    table[27].1,
    &[Code::new("smear::schema::schema-extension-without-schema")]
  );
  assert_eq!(table[28].0, "MissingQueryRootOperationType");
  assert_eq!(
    table[28].1,
    &[Code::new(
      "smear::schema::missing-query-root-operation-type"
    )]
  );
  assert_eq!(table[29].0, "ExternMisuse");
  assert_eq!(table[29].1, &[Code::new("smear::graphqlx::extern-misuse")]);
  assert_eq!(table[30].0, "ReprMisuse");
  assert_eq!(table[30].1, &[Code::new("smear::graphqlx::repr-misuse")]);
  assert_eq!(table[31].0, "TierViolation");
  assert_eq!(table[31].1, &[Code::new("smear::graphqlx::tier")]);
  assert_eq!(table[32].0, "CircularInputField");
  assert_eq!(
    table[32].1,
    &[
      Code::new("smear::schema::circular-non-null-input-field"),
      Code::new("smear::schema::circular-one-of-input-field"),
    ]
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
