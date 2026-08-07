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
      Self::LeafFieldSelections => 5,
      Self::ArgumentNames => 6,
      Self::ArgumentUniqueness => 7,
      Self::RequiredArguments => 8,
      Self::FragmentNameUniqueness => 9,
      Self::FragmentSpreadTypeExistence => 10,
      Self::FragmentsOnCompositeTypes => 11,
      Self::FragmentsMustBeUsed => 12,
      Self::FragmentSpreadTargetDefined => 13,
      Self::FragmentSpreadsMustNotFormCycles => 14,
      Self::FragmentSpreadIsPossible => 15,
      Self::ValuesOfCorrectType => 16,
      Self::InputObjectFieldNames => 17,
      Self::InputObjectFieldUniqueness => 18,
      Self::InputObjectRequiredFields => 19,
      Self::DirectivesAreDefined => 20,
      Self::DirectivesAreInValidLocations => 21,
      Self::DirectivesAreUniquePerLocation => 22,
      Self::VariableUniqueness => 23,
      Self::VariablesAreInputTypes => 24,
      Self::AllVariableUsesDefined => 25,
      Self::AllVariablesUsed => 26,
      Self::AllVariableUsagesAreAllowed => 27,
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
/// Narrowing does not merely filter diagnostics: a rule that is off is not evaluated, so a
/// consumer that only wants, say, the fragment rules does not pay for value coercion.
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
