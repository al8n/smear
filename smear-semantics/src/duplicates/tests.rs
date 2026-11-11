//! Both paths against a plain scan, at the widths either side of [`NARROW_LIST`], and what each
//! path promises about reading a name and recording one.

use core::cell::Cell;

use super::*;

/// What a duplicate rule answers at every position of `names`, by the scan nobody gets wrong: the
/// earliest position before it that wrote the same name.
fn scanned(names: &[u32]) -> Vec<Option<usize>> {
  names
    .iter()
    .enumerate()
    .map(|(at, name)| names[..at].iter().position(|earlier| earlier == name))
    .collect()
}

/// What [`Duplicates`] answers at every position of `names`, asked once per position, in order.
fn answered(names: &[u32]) -> Vec<Option<usize>> {
  let mut seen = Duplicates::over(names.len(), |at| names[at]);
  names
    .iter()
    .enumerate()
    .map(|(at, &name)| seen.first(at, name))
    .collect()
}

/// `width` names with repeats among them: position `at` writes `7 × at` modulo a third of the
/// width, so every name past the first third repeats an earlier one, and seldom the one beside it.
fn repeating(width: usize) -> Vec<u32> {
  let distinct = width / 3 + 1;
  (0..width).map(|at| (at * 7 % distinct) as u32).collect()
}

/// Either path answers what the scan answers: [`NARROW_LIST`] is the widest list that scans, and
/// one name more is the narrowest that sorts.
#[test]
fn either_path_answers_what_a_plain_scan_answers() {
  for width in [0, 1, 2, NARROW_LIST - 1, NARROW_LIST, NARROW_LIST + 1, 200] {
    let names = repeating(width);
    assert!(
      width < 2 || scanned(&names).iter().any(Option::is_some),
      "a list of {width} names has no repeat to find"
    );
    assert_eq!(
      matches!(
        Duplicates::over(width, |at| names[at]),
        Duplicates::Index(_)
      ),
      width > NARROW_LIST,
      "a list of {width} names took the other path"
    );
    assert_eq!(answered(&names), scanned(&names), "a list of {width} names");
  }
}

/// On the wide path every repeat of a name answers the earliest position that wrote it, not the
/// latest one before it.
#[test]
fn a_wide_list_answers_a_repeat_with_the_earliest_position() {
  // Position `at` writes `at`, except that 10, 40 and 70 write 7 again: one name at four
  // positions, in a list too wide to scan.
  let mut names: Vec<u32> = (0..100).collect();
  for at in [10, 40, 70] {
    names[at] = 7;
  }
  assert!(names.len() > NARROW_LIST, "the list is wide");

  let answers = answered(&names);
  assert_eq!(answers[7], None, "the first occurrence is new");
  assert_eq!(
    [answers[10], answers[40], answers[70]],
    [Some(7); 3],
    "every repeat answers the first occurrence"
  );
}

/// A narrow list never reads a name through the accessor, and a wide one reads each position's
/// name exactly once.
#[test]
fn the_accessor_is_read_once_per_position_and_only_on_the_wide_path() {
  let reads = Cell::new(0u32);
  let narrow = Duplicates::over(NARROW_LIST, |at| {
    reads.set(reads.get() + 1);
    at as u32
  });
  assert!(matches!(narrow, Duplicates::Trail(_)));
  assert_eq!(reads.get(), 0, "the narrow path read a name");

  let width = NARROW_LIST + 1;
  let per_position = vec![Cell::new(0u32); width];
  let wide = Duplicates::over(width, |at| {
    per_position[at].set(per_position[at].get() + 1);
    at as u32
  });
  assert!(matches!(wide, Duplicates::Index(_)));
  assert!(
    per_position.iter().all(|count| count.get() == 1),
    "a position's name was read other than once"
  );
}

/// On the narrow path a position never passed to `first` is never recorded: a name skipped at
/// every occurrence, the way a repeatable directive is, leaves the other names answered as the scan
/// answers them and puts nothing of its own in the trail.
#[test]
fn a_name_skipped_at_every_occurrence_is_never_recorded() {
  // `2` is written at positions 1, 3 and 4, and skipped at all three.
  let names: [u32; 6] = [1, 2, 3, 2, 2, 1];
  let mut seen = Duplicates::over(names.len(), |at| names[at]);
  let answers: Vec<(usize, Option<usize>)> = names
    .iter()
    .enumerate()
    .filter(|&(_, &name)| name != 2)
    .map(|(at, &name)| (at, seen.first(at, name)))
    .collect();
  assert_eq!(answers, [(0, None), (2, None), (5, Some(0))]);

  let Duplicates::Trail(trail) = seen else {
    panic!("six names is the narrow path");
  };
  assert_eq!(
    trail,
    [(1, 0), (3, 2)],
    "the trail holds the first occurrences it was asked about, and nothing of the skipped name"
  );
}
