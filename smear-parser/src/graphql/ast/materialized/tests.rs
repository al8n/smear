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
///
/// `$($param),*` is the tree's own parameter list, because the two trees no longer have the same
/// one: the slice tree is keyed by the source slice and the materialised tree by the source slice
/// *and* the integer payload. The tag function is generic over whatever the tree takes, so the
/// census reads a variant list and not an instantiation.
macro_rules! variant_census {
  ($names:ident, $tag:ident, $tree:ident<$($param:ident),+>, $($variant:ident),+ $(,)?) => {
    fn $tag<$($param),+>(value: &$tree<$($param),+>) -> &'static str {
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
  SliceInputValue<S>,
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
  MaterializedInputValue<S, I>,
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
  SliceConstInputValue<S>,
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
  MaterializedConstInputValue<S, I>,
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
/// The materialised tree is its own `enum` because a type alias is not a module — see this
/// module's header. What that buys in compatibility it owes in drift, and this is the payment: a
/// variant added to one tree and not the other fails here, naming the tree that is missing it,
/// instead of being discovered by a consumer who can pattern-match one and not another.
///
/// **The second *width* is not a second tree and so is not a second census row.** It used to be:
/// `materialized32::InputValue` was a third enum whose variant list was this one retyped, and the
/// only thing keeping the two in step was this test. `I` is a parameter now, so every width is the
/// same declaration and there is nothing left to compare — the property is held by the language
/// rather than by a row here. Two rows went with it, and their absence is the point.
#[test]
fn every_value_tree_declares_the_same_variants() {
  // Non-vacuity: two empty lists are equal, which is the failure mode a census has to rule out
  // before its equality means anything.
  assert_eq!(SLICE_VALUE.len(), 9, "the slice value tree lost a variant");
  assert_eq!(SLICE_CONST.len(), 8, "the slice const tree lost a variant");

  assert_eq!(
    SLICE_VALUE, MATERIALIZED_VALUE,
    "the materialised value tree declares different variants from the slice tree",
  );
  assert_eq!(
    SLICE_CONST, MATERIALIZED_CONST,
    "the materialised constant value tree declares different variants from the slice tree",
  );

  // The `match` arms are what make the lists above describe the enums. Naming the functions is
  // what keeps them compiled — at both widths, so a variant list that somehow depended on `I`
  // would have to be wrong at one of them.
  let _ = (
    slice_value_tag::<&str>,
    materialized_value_tag::<&str, i32>,
    materialized_value_tag::<&str, i64>,
    slice_const_tag::<&str>,
    materialized_const_tag::<&str, i32>,
    materialized_const_tag::<&str, i64>,
  );
}
