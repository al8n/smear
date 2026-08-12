use crate::graphql::ast::{
  ConstInputValue as SliceConstInputValue, InputValue as SliceInputValue,
  materialized::{
    ConstInputValue as MaterializedConstInputValue, InputValue as MaterializedInputValue,
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

/// The cost of writing the variant lists twice, paid rather than commented.
///
/// The materialised tree is a second `enum` because a type alias is not a module — see this
/// module's header. What that buys in compatibility it owes in drift, and this is the payment: a
/// variant added to one tree and not the other fails here, naming the tree that is missing it,
/// instead of being discovered by a consumer who can pattern-match one and not the other.
#[test]
fn the_two_value_trees_have_the_same_variants() {
  // Non-vacuity: two empty lists are equal, which is the failure mode a census has to rule out
  // before its equality means anything.
  assert_eq!(SLICE_VALUE.len(), 9, "the slice value tree lost a variant");
  assert_eq!(SLICE_CONST.len(), 8, "the slice const tree lost a variant");

  assert_eq!(
    SLICE_VALUE, MATERIALIZED_VALUE,
    "the two value trees declare different variants",
  );
  assert_eq!(
    SLICE_CONST, MATERIALIZED_CONST,
    "the two constant value trees declare different variants",
  );

  // The `match` arms are what make the lists above describe the enums. Naming the functions is
  // what keeps them compiled.
  let _ = (
    slice_value_tag::<&str>,
    materialized_value_tag::<&str>,
    slice_const_tag::<&str>,
    materialized_const_tag::<&str>,
  );
}
