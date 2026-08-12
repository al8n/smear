use crate::graphql::ast::{
  ConstInputValue as SliceConstInputValue, InputValue as SliceInputValue,
  materialized::{
    ConstInputValue as MaterializedConstInputValue, InputValue as MaterializedInputValue,
  },
  materialized32::{
    ConstInputValue as Materialized32ConstInputValue, InputValue as Materialized32InputValue,
  },
};

/// Declares a tree's variant set once, and uses it for both a wildcard-free `match` and the name
/// list the parity assertion compares.
///
/// The `match` is what makes the list honest: a variant added to the enum and not here is
/// `E0004`, and a name listed here that the enum does not declare is `E0599`. So the list cannot
/// drift from the enum it describes, which is the only way comparing two lists says anything
/// about two enums.
macro_rules! variant_census {
  ($names:ident, $tag:ident, $tree:ident, $($variant:ident),+ $(,)?) => {
    fn $tag<S>(value: &$tree<S>) -> &'static str {
      match value {
        $($tree::$variant { .. } => stringify!($variant),)+
      }
    }

    const $names: &[&str] = &[$(stringify!($variant)),+];
  };
}

variant_census!(
  SLICE_VALUE,
  slice_value_tag,
  SliceInputValue,
  Variable,
  Boolean,
  String,
  Float,
  Int,
  Enum,
  Null,
  List,
  Object,
);

variant_census!(
  MATERIALIZED_VALUE,
  materialized_value_tag,
  MaterializedInputValue,
  Variable,
  Boolean,
  String,
  Float,
  Int,
  Enum,
  Null,
  List,
  Object,
);

variant_census!(
  SLICE_CONST,
  slice_const_tag,
  SliceConstInputValue,
  Boolean,
  String,
  Float,
  Int,
  Enum,
  Null,
  List,
  Object,
);

variant_census!(
  MATERIALIZED_CONST,
  materialized_const_tag,
  MaterializedConstInputValue,
  Boolean,
  String,
  Float,
  Int,
  Enum,
  Null,
  List,
  Object,
);

variant_census!(
  MATERIALIZED32_VALUE,
  materialized32_value_tag,
  Materialized32InputValue,
  Variable,
  Boolean,
  String,
  Float,
  Int,
  Enum,
  Null,
  List,
  Object,
);

variant_census!(
  MATERIALIZED32_CONST,
  materialized32_const_tag,
  Materialized32ConstInputValue,
  Boolean,
  String,
  Float,
  Int,
  Enum,
  Null,
  List,
  Object,
);

/// The cost of writing the variant lists once per tree, paid rather than commented.
///
/// Each materialised tree is its own `enum` because a type alias is not a module, and a second
/// *width* is a second pair of enums for the same reason a second payload was — see this module's
/// header and [`ast::materialized32`](crate::graphql::ast::materialized32)'s. What that buys in
/// compatibility it owes in drift, and this is the payment: a variant added to one tree and not
/// the others fails here, naming the tree that is missing it, instead of being discovered by a
/// consumer who can pattern-match one and not another.
///
/// **Every tree is compared against the slice tree, not against its neighbour.** A chain of
/// pairwise equalities would pass if two trees drifted together, and the two materialised trees
/// are exactly the pair most likely to be edited in one sitting.
#[test]
fn every_value_tree_declares_the_same_variants() {
  // Non-vacuity: two empty lists are equal, which is the failure mode a census has to rule out
  // before its equality means anything.
  assert_eq!(SLICE_VALUE.len(), 9, "the slice value tree lost a variant");
  assert_eq!(SLICE_CONST.len(), 8, "the slice const tree lost a variant");

  for (name, listed) in [
    ("materialized", MATERIALIZED_VALUE),
    ("materialized32", MATERIALIZED32_VALUE),
  ] {
    assert_eq!(
      SLICE_VALUE, listed,
      "the `{name}` value tree declares different variants from the slice tree",
    );
  }

  for (name, listed) in [
    ("materialized", MATERIALIZED_CONST),
    ("materialized32", MATERIALIZED32_CONST),
  ] {
    assert_eq!(
      SLICE_CONST, listed,
      "the `{name}` constant value tree declares different variants from the slice tree",
    );
  }

  // The `match` arms are what make the lists above describe the enums. Naming the functions is
  // what keeps them compiled.
  let _ = (
    slice_value_tag::<&str>,
    materialized_value_tag::<&str>,
    materialized32_value_tag::<&str>,
    slice_const_tag::<&str>,
    materialized_const_tag::<&str>,
    materialized32_const_tag::<&str>,
  );
}
