//! The resolver's own tables, asked directly: the generic parameters a request carries, found by
//! search, and the near miss of a name among the candidates lowering gives.

use core::cell::Cell;
use std::{collections::BTreeMap, format, string::String, vec::Vec};

use tokora::diagnostic::Location;

use super::{Position, Request, Resolved, Root, Scopes, Segment, Wanted, params};
use crate::graphqlx::{
  declare::ModuleEntry,
  diagnostic::Reporter,
  id::{ModuleId, SourceId, dense},
  model::{ModelBuilder, ModuleKind},
};

/// How many generic parameters the search test spells. Under Miri, which interprets every
/// comparison, a few dozen: enough for every branch of the search, not for its cost.
const PARAMS: usize = if cfg!(miri) { 64 } else { 1 << 17 };

/// A root module of the package at index 0, which is all the tests' scopes hold.
fn root_module() -> ModuleEntry {
  ModuleEntry {
    package: 0,
    parent: None,
    name: None,
    kind: ModuleKind::Root,
    declared: None,
  }
}

/// Scopes over `modules` and nothing else, with `budget` near-miss comparisons to spend.
fn bare(modules: &[ModuleEntry], budget: u32) -> Scopes<'_> {
  Scopes {
    bindings: &[],
    modules,
    children: &[],
    roots: &[],
    primitives: &[],
    packages: Vec::new(),
    non_zero: None,
    explicit: BTreeMap::new(),
    globs: BTreeMap::new(),
    directories: Vec::new(),
    files: Vec::new(),
    max_segments: 64,
    max_use_depth: 32,
    near: Cell::new(budget),
  }
}

#[test]
fn a_parameter_table_is_sorted_by_spelling_and_keeps_the_first_index_of_each() {
  let written: [&[u8]; 4] = [b"T", b"U", b"T", b"A"];
  assert_eq!(
    params(&written),
    [
      (b"A".as_slice(), 3),
      (b"T".as_slice(), 0),
      (b"U".as_slice(), 1)
    ]
  );
  assert!(params(&[]).is_empty());
}

/// Each of many generic parameters is found by a binary search of the table.
///
/// A path's first segment was compared with every generic parameter in scope, in turn, so a
/// definition with P parameters and N paths cost N × P comparisons, and no limit bounds P. Now
/// lowering tables the parameters once, by spelling, and the resolver searches the table.
///
/// Nothing here can count that work, so the sizes tell the two costs apart: 2^17 parameters, each
/// named by a path of its own. Compared in turn, the paths cost about 8.6 × 10^9 comparisons,
/// which is minutes; searched for, about 2 × 10^6.
#[test]
fn a_parameter_is_found_by_search_among_many() {
  let spellings: Vec<String> = (0..PARAMS)
    .map(|index: usize| format!("P{index}"))
    .collect();
  let written: Vec<&[u8]> = spellings
    .iter()
    .map(|spelling: &String| spelling.as_bytes())
    .collect();
  let table = params(&written);
  let modules = [root_module()];
  let scopes = bare(&modules, 0);
  let names = ModelBuilder::default();
  let mut reporter = Reporter::without_marks(0);
  let request = Request {
    module: ModuleId::new(0),
    source: SourceId::new(0),
    wanted: Wanted::Type,
    position: Position::Type,
    params: &table,
  };
  for (index, &spelling) in written.iter().enumerate() {
    let segment = Segment {
      name: spelling,
      at: Location::entire(0),
    };
    let resolved =
      scopes.resolve_segments(&names, &mut reporter, &request, Root::Relative, &[segment]);
    assert!(resolved == Resolved::Param(dense(index)), "{index}");
  }
}

/// The near miss among given candidates keeps every search's rules and spends the analysis's one
/// budget: each candidate looked at costs a comparison, compared or not, and a search the budget
/// runs out in suggests nothing.
#[test]
fn a_near_miss_among_given_candidates_spends_the_one_budget() {
  let modules = [root_module()];
  let scopes = bare(&modules, 5);

  // `colour` is looked at but not compared, `color` is one insertion away, `size` four.
  let mut arguments = [
    b"colour".as_slice(),
    b"color".as_slice(),
    b"size".as_slice(),
  ]
  .into_iter();
  assert_eq!(
    scopes.nearest(b"colr", &mut arguments),
    Some(b"color".as_slice())
  );

  // Two comparisons are left: `weight` is one swap away, but the budget runs out at `height`.
  let mut arguments = [
    b"weight".as_slice(),
    b"wieght".as_slice(),
    b"height".as_slice(),
  ]
  .into_iter();
  assert_eq!(scopes.nearest(b"weigth", &mut arguments), None);

  let mut arguments = [b"weight".as_slice()].into_iter();
  assert_eq!(scopes.nearest(b"weigth", &mut arguments), None);
}
