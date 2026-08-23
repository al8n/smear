//! The rule vocabulary: one identity per draft §5 rule, and a set over them.
//!
//! [`Rule`] is a fieldless enum, so a diagnostic names the rule that produced it with one byte and
//! a consumer matches on it exhaustively. [`Rule::ALL`] enumerates the space, which is what lets
//! the liveness gate iterate the rules rather than a hand-kept list: a rule added without a
//! fixture that makes it fire is a failing test, not a silently unexercised branch.
//!
//! [`RuleSet`] is the selection door. Validation runs every rule by default; a consumer that wants
//! a subset — a linter checking only the fragment rules, say — hands one to
//! [`validate_executable_with`](super::validate_executable_with).

use smear_schema::diagnostic::{Code, Severity};

/// One draft §5 validation rule.
///
/// Every variant carries its specification section in its documentation, and the section numbers
/// are the **draft**'s (<https://spec.graphql.org/draft/>), which renumbered §5 after the October
/// 2021 edition: *Required Arguments* is 5.4.3 rather than 5.4.2.1, and *Operation Type Existence*
/// (5.2.1.1) did not exist in the 2021 text at all.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum Rule {
  // -- 5.2 Operations -------------------------------------------------------------------------
  /// The schema must define a root operation type of the operation's kind (draft 5.2.1.1).
  OperationTypeExistence,
  /// Two operations must not share a name (draft 5.2.2.1).
  OperationNameUniqueness,
  /// An anonymous operation must be the document's only operation (draft 5.2.3.1).
  LoneAnonymousOperation,
  /// A subscription's root selection set must collect exactly one field, which must not be an
  /// introspection field, and must not use `@skip` or `@include` (draft 5.2.4.1).
  SingleRootField,

  // -- 5.3 Fields -----------------------------------------------------------------------------
  /// A selected field must be defined on the type in scope (draft 5.3.1).
  ///
  /// A union defines no fields of its own, so the only field selectable directly on one is
  /// `__typename` — which falls out of this rule rather than needing one of its own, because the
  /// schema puts `__typename` in every composite type's field group.
  FieldSelections,
  /// Two selections that share a response name must be able to merge (draft 5.3.2).
  ///
  /// `FieldsInSetCanMerge` in the draft's own words: for every pair of selections a response name
  /// collects — through fragments, inline fragments and nesting — the two must have the same
  /// response shape, and, where they could both be encountered against the same object, must name
  /// the same field with the same arguments. [`Context::Merge`](super::Context::Merge) says which
  /// of the three failed.
  ///
  /// # Two selections carry "identical arguments" when their literals are written identically
  ///
  /// Argument sets are compared by name, order-independently, and each pair of values is compared
  /// structurally — object literals order-independently too. Scalar literals are compared by their
  /// **source spelling**: `1` and `1.0`, or `"a"` and `"""a"""`, are different literals here even
  /// where a coercion would give them the same value. graphql-js compares the printed form and so
  /// separates those same pairs; the one place this is stricter than graphql-js is a string
  /// written with different escapes for the same text, which is reported rather than merged.
  FieldSelectionMerging,
  /// A field whose type is a leaf must have no subselection, and a field whose type is composite
  /// must have one (draft 5.3.3).
  LeafFieldSelections,

  // -- 5.4 Arguments --------------------------------------------------------------------------
  /// An argument must be defined by the field or directive it is passed to (draft 5.4.1).
  ArgumentNames,
  /// An argument set must not name the same argument twice (draft 5.4.2).
  ArgumentUniqueness,
  /// A required argument — non-null type, no default — must be supplied and must not be the
  /// `null` literal (draft 5.4.3).
  RequiredArguments,

  // -- 5.5 Fragments --------------------------------------------------------------------------
  /// Two fragment definitions must not share a name (draft 5.5.1.1).
  FragmentNameUniqueness,
  /// A fragment's type condition must name a type the schema defines (draft 5.5.1.2).
  FragmentSpreadTypeExistence,
  /// A fragment's type condition must be an object, interface or union type (draft 5.5.1.3).
  FragmentsOnCompositeTypes,
  /// Every fragment definition must be spread by some operation, transitively (draft 5.5.1.4).
  FragmentsMustBeUsed,
  /// A fragment spread must name a fragment the document defines (draft 5.5.2.1).
  FragmentSpreadTargetDefined,
  /// Fragment spreads must not form a cycle (draft 5.5.2.2).
  ///
  /// Checked over the whole fragment graph, including fragments no operation reaches: a later
  /// rule that expands fragments must never meet an unestablished graph.
  FragmentSpreadsMustNotFormCycles,
  /// A spread's type condition must be able to apply within the type in scope (draft 5.5.2.3, all
  /// four subsections).
  ///
  /// One deliberate ecosystem deviation is adopted here: a spread whose condition *is* the scope
  /// is valid even where the possible-object set is empty — an interface with no implementors
  /// spread on itself. graphql-js, apollo-compiler and the graphql-spec#1109 discussion all land
  /// there, and a lone strict reading would only make a differential comparison noisy.
  FragmentSpreadIsPossible,

  // -- 5.6 Values -----------------------------------------------------------------------------
  /// A literal value must be coercible to the type expected in its position (draft 5.6.1).
  ///
  /// Includes the OneOf input object literal rules: exactly one field, and that field's value not
  /// `null`.
  ValuesOfCorrectType,
  /// An input object field must be defined by the input object type expected in its position
  /// (draft 5.6.2).
  InputObjectFieldNames,
  /// An input object literal must not name the same field twice (draft 5.6.3).
  ///
  /// apollo-compiler 1.32 does not implement this rule at all, so it is not a reference for what
  /// this one should do; the draft text is.
  InputObjectFieldUniqueness,
  /// A required input field — non-null type, no default — must be supplied and must not be the
  /// `null` literal (draft 5.6.4).
  InputObjectRequiredFields,

  // -- 5.7 Directives -------------------------------------------------------------------------
  /// A directive must be defined by the schema (draft 5.7.1).
  DirectivesAreDefined,
  /// A directive must be used at a location its definition declares (draft 5.7.2).
  DirectivesAreInValidLocations,
  /// A non-repeatable directive must appear at most once per location (draft 5.7.3).
  DirectivesAreUniquePerLocation,

  // -- 5.8 Variables --------------------------------------------------------------------------
  /// An operation must not define the same variable twice (draft 5.8.1).
  VariableUniqueness,
  /// A variable's declared type must be an input type (draft 5.8.2).
  VariablesAreInputTypes,
  /// Every variable used by an operation, or by a fragment it transitively spreads, must be
  /// defined by that operation (draft 5.8.3).
  AllVariableUsesDefined,
  /// Every variable an operation defines must be used by it, or by a fragment it transitively
  /// spreads (draft 5.8.4).
  AllVariablesUsed,
  /// A variable usage must be allowed in the position it appears in — `IsVariableUsageAllowed`,
  /// in every position, including inside list and object literals (draft 5.8.5).
  AllVariableUsagesAreAllowed,

  // -- resource bounds --------------------------------------------------------------------------
  /// The merge recursion nested deeper than [`Budget::merge_depth`](super::Budget::merge_depth).
  ///
  /// Not a specification rule: draft 5.3.2 is unbounded as written, and a validator on a request
  /// path may not be. A document that reaches this is **refused** — never passed unvalidated —
  /// and the verdict carries [`Invalid::budget_tripped`](super::Invalid::budget_tripped).
  ///
  /// Excluding it from a [`RuleSet`](super::RuleSet) removes the *diagnostic*, not the refusal:
  /// the engine still stops, and the document is still refused, but the verdict carries the
  /// refusal alone — `Err` with [`Invalid::emitted`](super::Invalid::emitted) at zero and
  /// [`Invalid::budget_tripped`](super::Invalid::budget_tripped) true. A caller who wants to be
  /// *told which bound* wants this rule on. al8n/smear#196.
  ///
  /// The alternative, letting the working set's capacity be the bound, was rejected in design: an
  /// allocation failure has no rule identity and no span, and cannot distinguish "this document is
  /// adversarial" from "this caller sized their buffers small". This rule has both.
  MergeDepthBudget,
  /// The merge engine reached [`Budget::merge_work`](super::Budget::merge_work).
  ///
  /// The companion to [`Rule::MergeDepthBudget`], and the one that actually caps the worst case:
  /// depth alone does not bound draft 5.3.2, breadth times fragment reuse does.
  MergeWorkBudget,
}

impl Rule {
  /// Every rule, in specification order.
  ///
  /// The liveness gate iterates this rather than a hand-kept list, so a rule added without a
  /// document that makes it fire fails a test instead of shipping unexercised.
  pub const ALL: &'static [Self] = &[
    Self::OperationTypeExistence,
    Self::OperationNameUniqueness,
    Self::LoneAnonymousOperation,
    Self::SingleRootField,
    Self::FieldSelections,
    Self::FieldSelectionMerging,
    Self::LeafFieldSelections,
    Self::ArgumentNames,
    Self::ArgumentUniqueness,
    Self::RequiredArguments,
    Self::FragmentNameUniqueness,
    Self::FragmentSpreadTypeExistence,
    Self::FragmentsOnCompositeTypes,
    Self::FragmentsMustBeUsed,
    Self::FragmentSpreadTargetDefined,
    Self::FragmentSpreadsMustNotFormCycles,
    Self::FragmentSpreadIsPossible,
    Self::ValuesOfCorrectType,
    Self::InputObjectFieldNames,
    Self::InputObjectFieldUniqueness,
    Self::InputObjectRequiredFields,
    Self::DirectivesAreDefined,
    Self::DirectivesAreInValidLocations,
    Self::DirectivesAreUniquePerLocation,
    Self::VariableUniqueness,
    Self::VariablesAreInputTypes,
    Self::AllVariableUsesDefined,
    Self::AllVariablesUsed,
    Self::AllVariableUsagesAreAllowed,
    Self::MergeDepthBudget,
    Self::MergeWorkBudget,
  ];

  /// Returns the rule's bit position in a [`RuleSet`].
  ///
  /// It is the rule's index in [`Rule::ALL`], which is why that list is the enumeration and this
  /// is derived from it rather than the other way round.
  #[inline]
  pub const fn bit(self) -> u32 {
    // A `const fn` cannot iterate a slice of `Self`, so the mapping is spelled out. The
    // `bits_match_all_order` test pins it against `ALL`.
    match self {
      Self::OperationTypeExistence => 0,
      Self::OperationNameUniqueness => 1,
      Self::LoneAnonymousOperation => 2,
      Self::SingleRootField => 3,
      Self::FieldSelections => 4,
      Self::FieldSelectionMerging => 5,
      Self::LeafFieldSelections => 6,
      Self::ArgumentNames => 7,
      Self::ArgumentUniqueness => 8,
      Self::RequiredArguments => 9,
      Self::FragmentNameUniqueness => 10,
      Self::FragmentSpreadTypeExistence => 11,
      Self::FragmentsOnCompositeTypes => 12,
      Self::FragmentsMustBeUsed => 13,
      Self::FragmentSpreadTargetDefined => 14,
      Self::FragmentSpreadsMustNotFormCycles => 15,
      Self::FragmentSpreadIsPossible => 16,
      Self::ValuesOfCorrectType => 17,
      Self::InputObjectFieldNames => 18,
      Self::InputObjectFieldUniqueness => 19,
      Self::InputObjectRequiredFields => 20,
      Self::DirectivesAreDefined => 21,
      Self::DirectivesAreInValidLocations => 22,
      Self::DirectivesAreUniquePerLocation => 23,
      Self::VariableUniqueness => 24,
      Self::VariablesAreInputTypes => 25,
      Self::AllVariableUsesDefined => 26,
      Self::AllVariablesUsed => 27,
      Self::AllVariableUsagesAreAllowed => 28,
      Self::MergeDepthBudget => 29,
      Self::MergeWorkBudget => 30,
    }
  }

  /// Returns the draft specification section the rule comes from.
  #[inline]
  pub const fn section(self) -> &'static str {
    match self {
      Self::OperationTypeExistence => "5.2.1.1",
      Self::OperationNameUniqueness => "5.2.2.1",
      Self::LoneAnonymousOperation => "5.2.3.1",
      Self::SingleRootField => "5.2.4.1",
      Self::FieldSelections => "5.3.1",
      Self::FieldSelectionMerging => "5.3.2",
      Self::LeafFieldSelections => "5.3.3",
      Self::ArgumentNames => "5.4.1",
      Self::ArgumentUniqueness => "5.4.2",
      Self::RequiredArguments => "5.4.3",
      Self::FragmentNameUniqueness => "5.5.1.1",
      Self::FragmentSpreadTypeExistence => "5.5.1.2",
      Self::FragmentsOnCompositeTypes => "5.5.1.3",
      Self::FragmentsMustBeUsed => "5.5.1.4",
      Self::FragmentSpreadTargetDefined => "5.5.2.1",
      Self::FragmentSpreadsMustNotFormCycles => "5.5.2.2",
      Self::FragmentSpreadIsPossible => "5.5.2.3",
      Self::ValuesOfCorrectType => "5.6.1",
      Self::InputObjectFieldNames => "5.6.2",
      Self::InputObjectFieldUniqueness => "5.6.3",
      Self::InputObjectRequiredFields => "5.6.4",
      Self::DirectivesAreDefined => "5.7.1",
      Self::DirectivesAreInValidLocations => "5.7.2",
      Self::DirectivesAreUniquePerLocation => "5.7.3",
      Self::VariableUniqueness => "5.8.1",
      Self::VariablesAreInputTypes => "5.8.2",
      Self::AllVariableUsesDefined => "5.8.3",
      Self::AllVariablesUsed => "5.8.4",
      Self::AllVariableUsagesAreAllowed => "5.8.5",
      // Not draft sections: these two are this crate's resource policy over draft 5.3.2, which
      // the specification leaves unbounded. They are spelled so that a reader lands in the right
      // neighbourhood and a grep for a real section number never finds them.
      Self::MergeDepthBudget => "5.3.2/depth",
      Self::MergeWorkBudget => "5.3.2/work",
    }
  }

  /// Returns the specification's own title for the rule.
  #[inline]
  pub const fn title(self) -> &'static str {
    match self {
      Self::OperationTypeExistence => "Operation Type Existence",
      Self::OperationNameUniqueness => "Operation Name Uniqueness",
      Self::LoneAnonymousOperation => "Lone Anonymous Operation",
      Self::SingleRootField => "Single Root Field",
      Self::FieldSelections => "Field Selections",
      Self::FieldSelectionMerging => "Field Selection Merging",
      Self::LeafFieldSelections => "Leaf Field Selections",
      Self::ArgumentNames => "Argument Names",
      Self::ArgumentUniqueness => "Argument Uniqueness",
      Self::RequiredArguments => "Required Arguments",
      Self::FragmentNameUniqueness => "Fragment Name Uniqueness",
      Self::FragmentSpreadTypeExistence => "Fragment Spread Type Existence",
      Self::FragmentsOnCompositeTypes => "Fragments on Object, Interface or Union Types",
      Self::FragmentsMustBeUsed => "Fragments Must Be Used",
      Self::FragmentSpreadTargetDefined => "Fragment Spread Target Defined",
      Self::FragmentSpreadsMustNotFormCycles => "Fragment Spreads Must Not Form Cycles",
      Self::FragmentSpreadIsPossible => "Fragment Spread Is Possible",
      Self::ValuesOfCorrectType => "Values of Correct Type",
      Self::InputObjectFieldNames => "Input Object Field Names",
      Self::InputObjectFieldUniqueness => "Input Object Field Uniqueness",
      Self::InputObjectRequiredFields => "Input Object Required Fields",
      Self::DirectivesAreDefined => "Directives Are Defined",
      Self::DirectivesAreInValidLocations => "Directives Are in Valid Locations",
      Self::DirectivesAreUniquePerLocation => "Directives Are Unique per Location",
      Self::VariableUniqueness => "Variable Uniqueness",
      Self::VariablesAreInputTypes => "Variables Are Input Types",
      Self::AllVariableUsesDefined => "All Variable Uses Defined",
      Self::AllVariablesUsed => "All Variables Used",
      Self::AllVariableUsagesAreAllowed => "All Variable Usages Are Allowed",
      Self::MergeDepthBudget => "Merge Depth Budget Exceeded",
      Self::MergeWorkBudget => "Merge Work Budget Exceeded",
    }
  }

  /// Returns the stable identifier for this rule.
  ///
  /// Not [`section`](Self::section), which looks like one and is not: it is unique only inside
  /// this enum, the two budget rules had to invent fake sections to keep it so, and a renderer
  /// holding schema, validation and execution diagnostics at once needs one flat namespace across
  /// all of them.
  #[inline]
  pub const fn code(&self) -> Code {
    match self {
      Self::OperationTypeExistence => Code::new("smear::validation::operation-type-existence"),
      Self::OperationNameUniqueness => Code::new("smear::validation::operation-name-uniqueness"),
      Self::LoneAnonymousOperation => Code::new("smear::validation::lone-anonymous-operation"),
      Self::SingleRootField => Code::new("smear::validation::single-root-field"),
      Self::FieldSelections => Code::new("smear::validation::field-selections"),
      Self::FieldSelectionMerging => Code::new("smear::validation::field-selection-merging"),
      Self::LeafFieldSelections => Code::new("smear::validation::leaf-field-selections"),
      Self::ArgumentNames => Code::new("smear::validation::argument-names"),
      Self::ArgumentUniqueness => Code::new("smear::validation::argument-uniqueness"),
      Self::RequiredArguments => Code::new("smear::validation::required-arguments"),
      Self::FragmentNameUniqueness => Code::new("smear::validation::fragment-name-uniqueness"),
      Self::FragmentSpreadTypeExistence => {
        Code::new("smear::validation::fragment-spread-type-existence")
      }
      Self::FragmentsOnCompositeTypes => {
        Code::new("smear::validation::fragments-on-composite-types")
      }
      Self::FragmentsMustBeUsed => Code::new("smear::validation::fragments-must-be-used"),
      Self::FragmentSpreadTargetDefined => {
        Code::new("smear::validation::fragment-spread-target-defined")
      }
      Self::FragmentSpreadsMustNotFormCycles => {
        Code::new("smear::validation::fragment-spreads-must-not-form-cycles")
      }
      Self::FragmentSpreadIsPossible => Code::new("smear::validation::fragment-spread-is-possible"),
      Self::ValuesOfCorrectType => Code::new("smear::validation::values-of-correct-type"),
      Self::InputObjectFieldNames => Code::new("smear::validation::input-object-field-names"),
      Self::InputObjectFieldUniqueness => {
        Code::new("smear::validation::input-object-field-uniqueness")
      }
      Self::InputObjectRequiredFields => {
        Code::new("smear::validation::input-object-required-fields")
      }
      Self::DirectivesAreDefined => Code::new("smear::validation::directives-are-defined"),
      Self::DirectivesAreInValidLocations => {
        Code::new("smear::validation::directives-are-in-valid-locations")
      }
      Self::DirectivesAreUniquePerLocation => {
        Code::new("smear::validation::directives-are-unique-per-location")
      }
      Self::VariableUniqueness => Code::new("smear::validation::variable-uniqueness"),
      Self::VariablesAreInputTypes => Code::new("smear::validation::variables-are-input-types"),
      Self::AllVariableUsesDefined => Code::new("smear::validation::all-variable-uses-defined"),
      Self::AllVariablesUsed => Code::new("smear::validation::all-variables-used"),
      Self::AllVariableUsagesAreAllowed => {
        Code::new("smear::validation::all-variable-usages-are-allowed")
      }
      Self::MergeDepthBudget => Code::new("smear::validation::merge-depth-budget"),
      Self::MergeWorkBudget => Code::new("smear::validation::merge-work-budget"),
    }
  }

  /// Returns how much this rule asks of its reader.
  ///
  /// Every draft §5 rule is an [`Severity::Error`]: a document that trips one is invalid and is
  /// not executed. Spelled out variant by variant rather than as a wildcard so that the first
  /// rule of the deprecation-lint class — a selected field that is `@deprecated`, which changes
  /// no verdict — has to declare itself here.
  #[inline]
  pub const fn severity(&self) -> Severity {
    match self {
      Self::OperationTypeExistence
      | Self::OperationNameUniqueness
      | Self::LoneAnonymousOperation
      | Self::SingleRootField
      | Self::FieldSelections
      | Self::FieldSelectionMerging
      | Self::LeafFieldSelections
      | Self::ArgumentNames
      | Self::ArgumentUniqueness
      | Self::RequiredArguments
      | Self::FragmentNameUniqueness
      | Self::FragmentSpreadTypeExistence
      | Self::FragmentsOnCompositeTypes
      | Self::FragmentsMustBeUsed
      | Self::FragmentSpreadTargetDefined
      | Self::FragmentSpreadsMustNotFormCycles
      | Self::FragmentSpreadIsPossible
      | Self::ValuesOfCorrectType
      | Self::InputObjectFieldNames
      | Self::InputObjectFieldUniqueness
      | Self::InputObjectRequiredFields
      | Self::DirectivesAreDefined
      | Self::DirectivesAreInValidLocations
      | Self::DirectivesAreUniquePerLocation
      | Self::VariableUniqueness
      | Self::VariablesAreInputTypes
      | Self::AllVariableUsesDefined
      | Self::AllVariablesUsed
      | Self::AllVariableUsagesAreAllowed
      | Self::MergeDepthBudget
      | Self::MergeWorkBudget => Severity::Error,
    }
  }

  /// Returns what the document's author can do about it, where the rule has something
  /// actionable to say beyond its title.
  ///
  /// `None` where the rule's own name is the instruction: nothing useful is added to "argument
  /// names" by a line saying to check the argument's name.
  #[inline]
  pub const fn help(&self) -> Option<&'static str> {
    match self {
      Self::OperationTypeExistence => Some(
        "the schema defines no root operation type of this kind; add one, or send a different operation.",
      ),
      Self::LoneAnonymousOperation => Some("name every operation, or send only one."),
      Self::SingleRootField => Some(
        "a subscription's root selection set must collect exactly one field, and that field must not be an introspection field nor carry `@skip` or `@include`.",
      ),
      Self::FieldSelections => Some(
        "check the type in scope: a union defines no fields of its own, so `__typename` is the only thing selectable directly on one.",
      ),
      Self::FieldSelectionMerging => {
        Some("give one of the two selections an alias, so the response key is no longer shared.")
      }
      Self::LeafFieldSelections => Some(
        "a scalar or enum field takes no subselection; an object, interface or union field requires one.",
      ),
      Self::RequiredArguments => Some(
        "a required argument is non-null with no default, and an explicit `null` does not satisfy one.",
      ),
      Self::FragmentsOnCompositeTypes => Some(
        "a type condition must name an object, interface or union type; a scalar or enum has no selection set to spread into.",
      ),
      Self::FragmentsMustBeUsed => Some("spread it from an operation, or delete it."),
      Self::FragmentSpreadsMustNotFormCycles => {
        Some("break the cycle; expanding it would never terminate.")
      }
      Self::FragmentSpreadIsPossible => Some(
        "the type condition and the type in scope have no object type in common, so the fragment could never apply.",
      ),
      Self::InputObjectRequiredFields => Some(
        "a required field is non-null with no default, and an explicit `null` does not satisfy one.",
      ),
      Self::DirectivesAreInValidLocations => Some(
        "the directive's definition lists where it may be used, and this is not one of those places.",
      ),
      Self::DirectivesAreUniquePerLocation => {
        Some("declare the directive `repeatable` in the schema, or apply it once here.")
      }
      Self::VariablesAreInputTypes => Some(
        "a variable's type must be a scalar, enum or input object; an object, interface or union cannot be sent as input.",
      ),
      Self::AllVariableUsesDefined => Some(
        "declare it on the operation — including when the use is inside a fragment the operation spreads.",
      ),
      Self::AllVariablesUsed => Some("use it, or drop the declaration."),
      Self::AllVariableUsagesAreAllowed => Some(
        "a nullable variable may stand in a non-null position only if it has a default; otherwise widen the position or narrow the variable.",
      ),
      Self::MergeDepthBudget => Some(
        "raise `Budget::merge_depth`, or refuse the document: the depth is this crate's bound on draft 5.3.2, which the specification leaves unbounded.",
      ),
      Self::MergeWorkBudget => Some(
        "raise `Budget::merge_work`, or refuse the document: breadth times fragment reuse is what actually bounds draft 5.3.2.",
      ),
      Self::OperationNameUniqueness
      | Self::ArgumentNames
      | Self::ArgumentUniqueness
      | Self::FragmentNameUniqueness
      | Self::FragmentSpreadTypeExistence
      | Self::FragmentSpreadTargetDefined
      | Self::ValuesOfCorrectType
      | Self::InputObjectFieldNames
      | Self::InputObjectFieldUniqueness
      | Self::DirectivesAreDefined
      | Self::VariableUniqueness => None,
    }
  }

  /// Returns the phrase for the second span a [`Diagnostic`](super::Diagnostic) of this rule
  /// points at.
  ///
  /// `Some` exactly for the rules that are about a *relationship* between two places: the eight
  /// uniqueness and cycle rules, plus draft 5.3.2, whose whole subject is a pair of selections.
  #[inline]
  pub const fn related_label(&self) -> Option<&'static str> {
    match self {
      Self::OperationNameUniqueness => Some("first defined here"),
      Self::FieldSelectionMerging => Some("the other selection"),
      Self::ArgumentUniqueness => Some("first passed here"),
      Self::FragmentNameUniqueness => Some("first defined here"),
      Self::FragmentSpreadsMustNotFormCycles => Some("the fragment the cycle returns to"),
      Self::InputObjectFieldUniqueness => Some("first given here"),
      Self::DirectivesAreUniquePerLocation => Some("first applied here"),
      Self::VariableUniqueness => Some("first declared here"),
      Self::OperationTypeExistence
      | Self::LoneAnonymousOperation
      | Self::SingleRootField
      | Self::FieldSelections
      | Self::LeafFieldSelections
      | Self::ArgumentNames
      | Self::RequiredArguments
      | Self::FragmentSpreadTypeExistence
      | Self::FragmentsOnCompositeTypes
      | Self::FragmentsMustBeUsed
      | Self::FragmentSpreadTargetDefined
      | Self::FragmentSpreadIsPossible
      | Self::ValuesOfCorrectType
      | Self::InputObjectFieldNames
      | Self::InputObjectRequiredFields
      | Self::DirectivesAreDefined
      | Self::DirectivesAreInValidLocations
      | Self::VariablesAreInputTypes
      | Self::AllVariableUsesDefined
      | Self::AllVariablesUsed
      | Self::AllVariableUsagesAreAllowed
      | Self::MergeDepthBudget
      | Self::MergeWorkBudget => None,
    }
  }
}

impl core::fmt::Display for Rule {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{} {}", self.section(), self.title())
  }
}

/// A set of [`Rule`]s, as a bitmask.
///
/// The default is [`RuleSet::ALL`] — validation checks everything unless a caller narrows it.
/// Narrowing does not merely filter diagnostics: a draft §5 rule that is off is not evaluated, so
/// a consumer that only wants, say, the fragment rules does not pay for value coercion.
///
/// **The resource bounds are not rules in that sense**, and reading the sentence above as though
/// they were is the one way to be wrong about this type. [`Rule::MergeDepthBudget`] and
/// [`Rule::MergeWorkBudget`] are each a rule *and* a bound, and a set reaches only the rule:
/// narrowing removes a bound's diagnostic, never the bound. A validator asked for
/// [`Rule::FieldSelectionMerging`] alone still stops when the merge engine reaches
/// [`Budget::merge_work`](super::Budget::merge_work), and still answers `Err` — with
/// [`Invalid::budget_tripped`](super::Invalid::budget_tripped) set and
/// [`Invalid::emitted`](super::Invalid::emitted) zero, because a validator that abandoned a pass
/// and then answered `Ok` would be spelling giving up the same way it spells finishing.
///
/// The knob is what switches a bound off: see [`Budget`](super::Budget). al8n/smear#196.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RuleSet(u64);

impl RuleSet {
  /// The empty set. Validation with it emits nothing and is always `Ok`.
  pub const EMPTY: Self = Self(0);

  /// Every rule.
  pub const ALL: Self = {
    // `Rule::ALL.len()` low bits set. Written as a shift so adding a rule cannot leave it stale.
    let count = Rule::ALL.len() as u32;
    Self(if count >= 64 {
      u64::MAX
    } else {
      (1u64 << count) - 1
    })
  };

  /// Returns the set containing exactly `rule`.
  #[inline]
  pub const fn only(rule: Rule) -> Self {
    Self(1u64 << rule.bit())
  }

  /// Returns the raw mask.
  #[inline]
  pub const fn bits(self) -> u64 {
    self.0
  }

  /// Returns the set with `rule` added.
  #[inline]
  pub const fn with(self, rule: Rule) -> Self {
    Self(self.0 | (1u64 << rule.bit()))
  }

  /// Returns the set with `rule` removed.
  #[inline]
  pub const fn without(self, rule: Rule) -> Self {
    Self(self.0 & !(1u64 << rule.bit()))
  }

  /// Returns the union of two sets.
  #[inline]
  pub const fn union(self, other: Self) -> Self {
    Self(self.0 | other.0)
  }

  /// Returns the intersection of two sets.
  #[inline]
  pub const fn intersection(self, other: Self) -> Self {
    Self(self.0 & other.0)
  }

  /// Returns whether the set contains `rule`.
  #[inline]
  pub const fn contains(self, rule: Rule) -> bool {
    self.0 & (1u64 << rule.bit()) != 0
  }

  /// Returns whether the set is empty.
  #[inline]
  pub const fn is_empty(self) -> bool {
    self.0 == 0
  }

  /// Returns how many rules the set contains.
  #[inline]
  pub const fn len(self) -> u32 {
    self.0.count_ones()
  }

  /// Iterates the set's rules in specification order.
  pub fn iter(self) -> impl Iterator<Item = Rule> {
    Rule::ALL.iter().copied().filter(move |r| self.contains(*r))
  }
}

impl Default for RuleSet {
  #[inline]
  fn default() -> Self {
    Self::ALL
  }
}

impl FromIterator<Rule> for RuleSet {
  fn from_iter<T: IntoIterator<Item = Rule>>(iter: T) -> Self {
    iter.into_iter().fold(Self::EMPTY, Self::with)
  }
}

#[cfg(test)]
mod tests {
  use super::{Rule, RuleSet};

  /// [`Rule::bit`] is spelled out because a `const fn` cannot search [`Rule::ALL`]. This is what
  /// keeps the two from drifting: a rule inserted in the middle of `ALL` without renumbering
  /// `bit` fails here rather than silently aliasing another rule's bit.
  #[test]
  fn bits_match_all_order() {
    for (index, rule) in Rule::ALL.iter().enumerate() {
      assert_eq!(
        rule.bit() as usize,
        index,
        "{rule:?} is at index {index} of Rule::ALL but reports bit {}",
        rule.bit()
      );
    }
    assert_eq!(
      RuleSet::ALL.len() as usize,
      Rule::ALL.len(),
      "RuleSet::ALL does not cover every rule"
    );
  }

  #[test]
  fn every_rule_is_addressable_alone() {
    for rule in Rule::ALL {
      let set = RuleSet::only(*rule);
      assert_eq!(set.len(), 1);
      assert!(set.contains(*rule));
      assert_eq!(set.iter().collect::<std::vec::Vec<_>>(), [*rule]);
      assert!(RuleSet::ALL.without(*rule).len() as usize == Rule::ALL.len() - 1);
    }
  }

  #[test]
  fn sections_and_titles_are_distinct() {
    for (i, a) in Rule::ALL.iter().enumerate() {
      for b in &Rule::ALL[i + 1..] {
        assert_ne!(a.section(), b.section(), "{a:?} and {b:?} share a section");
        assert_ne!(a.title(), b.title(), "{a:?} and {b:?} share a title");
      }
    }
  }
}
