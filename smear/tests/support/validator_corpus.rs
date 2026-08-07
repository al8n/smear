//! The draft §5 rule corpus: one entry per rule, shared by every gate that needs documents a
//! rule actually fires on.
//!
//! # Why it is a module and not a table in one test
//!
//! It was `validator_rules.rs`'s own `FIXTURES`, and it moved here unchanged when
//! `validator_lossless.rs` needed the same thirty documents. A second copy is the failure this
//! move exists to prevent: the two gates would drift, and the differential one would then be
//! comparing the doors over a corpus the liveness floor no longer covers. One table, two readers,
//! and `validator_rules.rs`'s [`liveness_floor`] still asserts that every rule in `Rule::ALL` has
//! an entry here.
//!
//! Nothing in this file runs. It is data plus the shape that holds it, so a reader that needs a
//! different harness — parsed twice, padded with trivia, validated through a different door —
//! writes its own and reads these.
//!
//! [`liveness_floor`]: ../validator_rules.rs

use smear::validator::{Budget, Rule};

/// The schema every fixture without an override validates against.
///
/// It is the specification's own example type system — the dog, cat, human and alien menagerie of
/// §5 — extended with the argument, input object and directive shapes the later rules need. Using
/// the specification's schema means the fixtures below are mostly its own counter-examples.
pub const SCHEMA: &str = r#"
type Query {
  dog: Dog
  human: Human
  pet: Pet
  catOrDog: CatOrDog
  empty: Empty
  arguments: Arguments
  findDog(searchBy: FindDogInput): Dog
  booleanList(booleanListArg: [Boolean!]): Boolean
  requiredInput(input: RequiredInput!): Boolean
  nest: Nest
  recursive(input: Recursive): Boolean
}

# Two self-referential shapes, one per tree, so the depth gate can nest as far as it likes.
type Nest {
  nest: Nest
  leaf: Int
}

input Recursive {
  rec: Recursive
  n: Int
}

type Mutation {
  addPet(pet: PetInput!): Pet
  addPets(pets: [PetInput!]!): [Pet]
}

type Subscription {
  newMessage: Message
  disallowedSecondRootField: Boolean
}

type Message {
  body: String
  sender: String
}

interface Sentient {
  name: String!
}

interface Pet {
  name: String!
}

# An interface with no implementors: the empty possible-object set draft 5.5.2.3's ecosystem
# exception exists for.
interface Empty {
  nothing: Int
}

type Alien implements Sentient {
  name: String!
  homePlanet: String
}

type Human implements Sentient {
  name: String!
  pets: [Pet]
}

enum DogCommand {
  SIT
  DOWN
  HEEL
}

type Dog implements Pet {
  name: String!
  nickname: String
  barkVolume: Int
  doesKnowCommand(dogCommand: DogCommand!): Boolean!
  isHouseTrained(atOtherHomes: Boolean): Boolean!
  owner: Human
}

enum CatCommand {
  JUMP
}

type Cat implements Pet {
  name: String!
  nickname: String
  doesKnowCommand(catCommand: CatCommand!): Boolean!
  meowVolume: Int
}

union CatOrDog = Cat | Dog
union HumanOrAlien = Human | Alien

scalar CustomScalar

type Arguments {
  multipleRequirements(x: Int!, y: Int!): Int!
  booleanArgField(booleanArg: Boolean): Boolean
  floatArgField(floatArg: Float): Float
  intArgField(intArg: Int): Int
  idArgField(idArg: ID): ID
  stringArgField(stringArg: String): String
  enumArgField(enumArg: DogCommand): Boolean
  customScalarArgField(customArg: CustomScalar): Boolean
  nonNullBooleanArgField(nonNullBooleanArg: Boolean!): Boolean!
  booleanListArgField(booleanListArg: [Boolean]!): [Boolean]
  optionalNonNullBooleanArgField(optionalBooleanArg: Boolean! = false): Boolean!
}

input FindDogInput {
  name: String
  owner: String
}

input RequiredInput {
  req: Int!
  opt: Int
}

input CatInput {
  name: String!
}

input DogInput {
  name: String!
}

input PetInput @oneOf {
  cat: CatInput
  dog: DogInput
}

directive @onQuery on QUERY
directive @repeatableField repeatable on FIELD
"#;

/// One rule's liveness evidence.
pub struct Fixture {
  /// The rule this entry exists for.
  pub rule: Rule,
  /// An SDL override, when the standard schema cannot express the failure.
  pub schema: Option<&'static str>,
  /// A [`Budget`] override, for the two rules that are a resource bound rather than a draft rule.
  ///
  /// Lowering the bound is how a bound is exercised without a hundred-kilobyte fixture; that the
  /// **default** bound holds against a genuinely hostile document is `validator_merge.rs`'s job,
  /// and it measures the wall clock while it is there.
  pub budget: Option<Budget>,
  /// A document that makes [`Fixture::rule`] fire.
  pub invalid: &'static str,
  /// The **complete** set of rules [`Fixture::invalid`] fires.
  pub fires: &'static [Rule],
  /// A document as close to it as possible that fires nothing at all.
  pub valid: &'static str,
}

/// A schema with no mutation root, so an operation can ask for one that is not there.
pub const QUERY_ONLY: &str = "type Query { ok: Int }";

pub const FIXTURES: &[Fixture] = &[
  // -- 5.2 operations -------------------------------------------------------------------------
  Fixture {
    rule: Rule::OperationTypeExistence,
    schema: Some(QUERY_ONLY),
    budget: None,
    invalid: "mutation goodbyeMutation { goodbye }",
    fires: &[Rule::OperationTypeExistence],
    valid: "query helloQuery { ok }",
  },
  Fixture {
    rule: Rule::OperationNameUniqueness,
    schema: None,
    budget: None,
    invalid: "query getName { dog { name } } query getName { dog { nickname } }",
    fires: &[Rule::OperationNameUniqueness],
    valid: "query getName { dog { name } } query getNickname { dog { nickname } }",
  },
  Fixture {
    rule: Rule::LoneAnonymousOperation,
    schema: None,
    budget: None,
    invalid: "{ dog { name } } query getName { dog { nickname } }",
    fires: &[Rule::LoneAnonymousOperation],
    valid: "{ dog { name } }",
  },
  Fixture {
    rule: Rule::SingleRootField,
    schema: None,
    budget: None,
    invalid: "subscription sub { newMessage { body } disallowedSecondRootField }",
    fires: &[Rule::SingleRootField],
    valid: "subscription sub { newMessage { body } }",
  },
  // -- 5.3 fields -----------------------------------------------------------------------------
  Fixture {
    rule: Rule::FieldSelections,
    schema: None,
    budget: None,
    invalid: "{ dog { meowVolume } }",
    fires: &[Rule::FieldSelections],
    valid: "{ dog { barkVolume } }",
  },
  Fixture {
    rule: Rule::FieldSelectionMerging,
    schema: None,
    budget: None,
    // The specification's own `conflictingBecauseAlias`. It fires twice, because the pair breaks
    // two of the three requirements at once: `nickname` is `String` where `name` is `String!`, and
    // the two are not the same field.
    invalid: "{ dog { name: nickname name } }",
    fires: &[Rule::FieldSelectionMerging],
    // `mergeIdenticalAliasesAndFields`, which is the same shape and merges.
    valid: "{ dog { otherName: name otherName: name } }",
  },
  Fixture {
    rule: Rule::LeafFieldSelections,
    schema: None,
    budget: None,
    invalid: "{ dog { barkVolume { sinceWhen } } }",
    fires: &[Rule::LeafFieldSelections],
    valid: "{ dog { barkVolume } }",
  },
  // -- 5.4 arguments --------------------------------------------------------------------------
  Fixture {
    rule: Rule::ArgumentNames,
    schema: None,
    budget: None,
    invalid: "{ dog { isHouseTrained(atOtherHomes: true, unless: false) } }",
    fires: &[Rule::ArgumentNames],
    valid: "{ dog { isHouseTrained(atOtherHomes: true) } }",
  },
  Fixture {
    rule: Rule::ArgumentUniqueness,
    schema: None,
    budget: None,
    invalid: "{ dog { isHouseTrained(atOtherHomes: true, atOtherHomes: false) } }",
    fires: &[Rule::ArgumentUniqueness],
    valid: "{ dog { isHouseTrained(atOtherHomes: true) } }",
  },
  Fixture {
    rule: Rule::RequiredArguments,
    schema: None,
    budget: None,
    invalid: "{ dog { doesKnowCommand } }",
    fires: &[Rule::RequiredArguments],
    valid: "{ dog { doesKnowCommand(dogCommand: SIT) } }",
  },
  // -- 5.5 fragments --------------------------------------------------------------------------
  Fixture {
    rule: Rule::FragmentNameUniqueness,
    schema: None,
    budget: None,
    invalid: "{ dog { ...part } } fragment part on Dog { name } fragment part on Dog { nickname }",
    fires: &[Rule::FragmentNameUniqueness],
    valid: "{ dog { ...part ...other } } fragment part on Dog { name } \
            fragment other on Dog { nickname }",
  },
  Fixture {
    rule: Rule::FragmentSpreadTypeExistence,
    schema: None,
    budget: None,
    invalid: "{ dog { ...part } } fragment part on NotInSchema { name }",
    fires: &[Rule::FragmentSpreadTypeExistence],
    valid: "{ dog { ...part } } fragment part on Dog { name }",
  },
  Fixture {
    rule: Rule::FragmentsOnCompositeTypes,
    schema: None,
    budget: None,
    invalid: "{ dog { ...part } } fragment part on Int { something }",
    fires: &[Rule::FragmentsOnCompositeTypes],
    valid: "{ dog { ...part } } fragment part on Dog { name }",
  },
  Fixture {
    rule: Rule::FragmentsMustBeUsed,
    schema: None,
    budget: None,
    invalid: "{ dog { name } } fragment nameFragment on Dog { name }",
    fires: &[Rule::FragmentsMustBeUsed],
    valid: "{ dog { ...nameFragment } } fragment nameFragment on Dog { name }",
  },
  Fixture {
    rule: Rule::FragmentSpreadTargetDefined,
    schema: None,
    budget: None,
    invalid: "{ dog { ...undefinedFragment } }",
    fires: &[Rule::FragmentSpreadTargetDefined],
    valid: "{ dog { ...definedFragment } } fragment definedFragment on Dog { name }",
  },
  Fixture {
    rule: Rule::FragmentSpreadsMustNotFormCycles,
    schema: None,
    budget: None,
    invalid: "{ dog { ...nameFragment } } fragment nameFragment on Dog { name ...barkFragment } \
              fragment barkFragment on Dog { barkVolume ...nameFragment }",
    fires: &[Rule::FragmentSpreadsMustNotFormCycles],
    valid: "{ dog { ...nameFragment } } fragment nameFragment on Dog { name ...barkFragment } \
            fragment barkFragment on Dog { barkVolume }",
  },
  Fixture {
    rule: Rule::FragmentSpreadIsPossible,
    schema: None,
    budget: None,
    invalid: "{ catOrDog { ... on Cat { ...dogFragment } } } \
              fragment dogFragment on Dog { barkVolume }",
    fires: &[Rule::FragmentSpreadIsPossible],
    valid: "{ catOrDog { ... on Dog { ...dogFragment } } } \
            fragment dogFragment on Dog { barkVolume }",
  },
  // -- 5.6 values -----------------------------------------------------------------------------
  Fixture {
    rule: Rule::ValuesOfCorrectType,
    schema: None,
    budget: None,
    invalid: r#"{ arguments { intArgField(intArg: "123") } }"#,
    fires: &[Rule::ValuesOfCorrectType],
    valid: "{ arguments { intArgField(intArg: 123) } }",
  },
  Fixture {
    rule: Rule::InputObjectFieldNames,
    schema: None,
    budget: None,
    invalid: r#"{ findDog(searchBy: { favoriteCookieFlavor: "Bacon" }) { name } }"#,
    fires: &[Rule::InputObjectFieldNames],
    valid: r#"{ findDog(searchBy: { name: "Fido" }) { name } }"#,
  },
  Fixture {
    rule: Rule::InputObjectFieldUniqueness,
    schema: None,
    budget: None,
    invalid: r#"{ findDog(searchBy: { name: "Fido", name: "Milou" }) { name } }"#,
    fires: &[Rule::InputObjectFieldUniqueness],
    valid: r#"{ findDog(searchBy: { name: "Fido" }) { name } }"#,
  },
  Fixture {
    rule: Rule::InputObjectRequiredFields,
    schema: None,
    budget: None,
    invalid: "{ requiredInput(input: { opt: 1 }) }",
    fires: &[Rule::InputObjectRequiredFields],
    valid: "{ requiredInput(input: { req: 1 }) }",
  },
  // -- 5.7 directives -------------------------------------------------------------------------
  Fixture {
    rule: Rule::DirectivesAreDefined,
    schema: None,
    budget: None,
    invalid: "{ dog { name @undefinedDirective } }",
    fires: &[Rule::DirectivesAreDefined],
    valid: "{ dog { name @skip(if: true) } }",
  },
  Fixture {
    rule: Rule::DirectivesAreInValidLocations,
    schema: None,
    budget: None,
    invalid: "query getName @skip(if: true) { dog { name } }",
    fires: &[Rule::DirectivesAreInValidLocations],
    valid: "query getName @onQuery { dog { name } }",
  },
  Fixture {
    rule: Rule::DirectivesAreUniquePerLocation,
    schema: None,
    budget: None,
    invalid: "{ dog { name @skip(if: true) @skip(if: false) } }",
    fires: &[Rule::DirectivesAreUniquePerLocation],
    valid: "{ dog { name @repeatableField @repeatableField } }",
  },
  // -- 5.8 variables --------------------------------------------------------------------------
  Fixture {
    rule: Rule::VariableUniqueness,
    schema: None,
    budget: None,
    invalid: "query houseTrained($atOtherHomes: Boolean, $atOtherHomes: Boolean) \
              { dog { isHouseTrained(atOtherHomes: $atOtherHomes) } }",
    fires: &[Rule::VariableUniqueness],
    valid: "query houseTrained($atOtherHomes: Boolean) \
            { dog { isHouseTrained(atOtherHomes: $atOtherHomes) } }",
  },
  Fixture {
    rule: Rule::VariablesAreInputTypes,
    schema: None,
    // The variable is also unused, and cannot be otherwise: no argument anywhere can have an
    // object type, so there is no position a `Cat` variable could legally be used in.
    budget: None,
    invalid: "query takesCat($cat: Cat) { dog { name } }",
    fires: &[Rule::VariablesAreInputTypes, Rule::AllVariablesUsed],
    valid: "query takesBoolean($atOtherHomes: Boolean) \
            { dog { isHouseTrained(atOtherHomes: $atOtherHomes) } }",
  },
  Fixture {
    rule: Rule::AllVariableUsesDefined,
    schema: None,
    budget: None,
    invalid: "query variableIsNotDefined { dog { isHouseTrained(atOtherHomes: $atOtherHomes) } }",
    fires: &[Rule::AllVariableUsesDefined],
    valid: "query variableIsDefined($atOtherHomes: Boolean) \
            { dog { isHouseTrained(atOtherHomes: $atOtherHomes) } }",
  },
  Fixture {
    rule: Rule::AllVariablesUsed,
    schema: None,
    budget: None,
    invalid: "query variableUnused($atOtherHomes: Boolean) { dog { name } }",
    fires: &[Rule::AllVariablesUsed],
    valid: "query variableUsed($atOtherHomes: Boolean) \
            { dog { isHouseTrained(atOtherHomes: $atOtherHomes) } }",
  },
  Fixture {
    rule: Rule::AllVariableUsagesAreAllowed,
    schema: None,
    budget: None,
    invalid: "query intCannotGoIntoBoolean($intArg: Int) \
              { arguments { booleanArgField(booleanArg: $intArg) } }",
    fires: &[Rule::AllVariableUsagesAreAllowed],
    valid: "query booleanIntoBoolean($booleanArg: Boolean) \
            { arguments { booleanArgField(booleanArg: $booleanArg) } }",
  },
  // -- resource bounds --------------------------------------------------------------------------
  Fixture {
    rule: Rule::MergeDepthBudget,
    schema: None,
    // `Nest` is self-referential, so the response shape can be as deep as the fixture likes and
    // nothing else about the document changes between the two halves.
    budget: Some(Budget::new(3, Budget::DEFAULT_MERGE_WORK)),
    invalid: "{ nest { nest { nest { leaf } } } }",
    fires: &[Rule::MergeDepthBudget],
    valid: "{ nest { nest { leaf } } }",
  },
  Fixture {
    rule: Rule::MergeWorkBudget,
    schema: None,
    budget: Some(Budget::new(Budget::DEFAULT_MERGE_DEPTH, 24)),
    invalid: "{ dog { name nickname barkVolume } }",
    fires: &[Rule::MergeWorkBudget],
    valid: "{ dog { name } }",
  },
];
