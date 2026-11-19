//! The order of paths: the segment-wise order, read once and no further than two paths agree.

use core::cmp::Ordering;
use std::{vec, vec::Vec};

use super::{compare_path_bytes, compare_paths};

/// The longest path the exhaustive tests build. Under Miri, which interprets every comparison,
/// the paths are shorter: the order is checked on every path of up to five bytes natively, and
/// Miri needs only every branch of the comparison.
const LONGEST: usize = if cfg!(miri) { 3 } else { 5 };

/// The segment-wise order, in the shape the analyzer first wrote it: each path split on `/`, the
/// segments compared in turn, each bytewise, and a path whose segments begin the other's first.
fn segment_wise(a: &[u8], b: &[u8]) -> Ordering {
  let mut left = a.split(|byte: &u8| *byte == b'/');
  let mut right = b.split(|byte: &u8| *byte == b'/');
  loop {
    match (left.next(), right.next()) {
      (Some(x), Some(y)) => match x.cmp(y) {
        Ordering::Equal => {}
        unequal => return unequal,
      },
      (None, Some(_)) => return Ordering::Less,
      (Some(_), None) => return Ordering::Greater,
      (None, None) => return Ordering::Equal,
    }
  }
}

/// Every path of at most `longest` bytes over `alphabet`, the empty path first.
fn paths(alphabet: &[u8], longest: usize) -> Vec<Vec<u8>> {
  let mut all: Vec<Vec<u8>> = Vec::from([Vec::new()]);
  let mut last: Vec<Vec<u8>> = Vec::from([Vec::new()]);
  for _ in 0..longest {
    let mut next: Vec<Vec<u8>> = Vec::with_capacity(last.len() * alphabet.len());
    for path in &last {
      for &byte in alphabet {
        let mut longer = path.clone();
        longer.push(byte);
        next.push(longer);
      }
    }
    all.extend_from_slice(&next);
    last = next;
  }
  all
}

/// Compares `a` and `b`, answering the order and how many bytes of each the comparison read.
fn counted(a: &[u8], b: &[u8]) -> (Ordering, usize, usize) {
  let (mut read_a, mut read_b) = (0, 0);
  let order = compare_path_bytes(
    a.iter().copied().inspect(|_: &u8| read_a += 1),
    b.iter().copied().inspect(|_: &u8| read_b += 1),
  );
  (order, read_a, read_b)
}

/// A comparison reads two paths no further than they agree, and one byte more of each: a long path
/// with no `/` costs one byte of each between short paths that differ from it in their first byte,
/// however long it is.
#[test]
fn a_comparison_reads_no_further_than_the_paths_agree() {
  let long: Vec<u8> = vec![b'm'; 1 << 16];
  assert_eq!(counted(b"a.graphqlx", &long), (Ordering::Less, 1, 1));
  assert_eq!(counted(&long, b"z.graphqlx"), (Ordering::Less, 1, 1));
  assert_eq!(counted(b"z.graphqlx", &long), (Ordering::Greater, 1, 1));

  // Paths that agree on their first `k` bytes, separators among them or not: `k + 1` of each.
  let agreeing: [(&[u8], &[u8], usize); 5] = [
    (b"src/a.graphqlx", b"src/b.graphqlx", 4),
    (b"src/a", b"src.a", 3),
    (b"a/b", b"a.b", 1),
    (b"x//y", b"x/y", 2),
    (b"media/vision.graphqlx", b"media.graphqlx", 5),
  ];
  for (a, b, k) in agreeing {
    assert_eq!(
      counted(a, b),
      (segment_wise(a, b), k + 1, k + 1),
      "{a:?} and {b:?}"
    );
  }

  // A path that begins the other: all of it, and one byte more of the other.
  assert_eq!(counted(b"src", b"src/x"), (Ordering::Less, 3, 4));
  assert_eq!(counted(b"src/x", b"src"), (Ordering::Greater, 4, 3));
  assert_eq!(counted(b"", b"a"), (Ordering::Less, 0, 1));

  // Equal paths: all of both.
  assert_eq!(
    counted(b"src/lib.graphqlx", b"src/lib.graphqlx"),
    (Ordering::Equal, 16, 16)
  );
}

/// On every pair of paths of at most `LONGEST` bytes over `/`, `.`, `a` and `b`, the order is the
/// segment-wise one. `.` sorts before `/` bytewise and after it here, so the alphabet holds every
/// way the two orders could part.
#[test]
fn the_order_is_the_segment_wise_order_on_every_short_path() {
  let all = paths(b"/.ab", LONGEST);
  for a in &all {
    for b in &all {
      assert_eq!(compare_paths(a, b), segment_wise(a, b), "{a:?} and {b:?}");
    }
  }
}

/// On bytes that are not UTF-8, and on a byte below `/`, the order is the segment-wise one too.
#[test]
fn the_order_is_the_segment_wise_order_on_bytes_that_are_not_utf8() {
  let all = paths(b"/\x00\x80\xff", LONGEST - 1);
  for a in &all {
    for b in &all {
      assert_eq!(compare_paths(a, b), segment_wise(a, b), "{a:?} and {b:?}");
    }
  }
}

/// The examples the analyzer documents keep their order: a directory's files before a sibling
/// that only shares their prefix, a path before every path it begins, and an empty segment before
/// every other.
#[test]
fn the_documented_examples_keep_their_order() {
  let ordered: [(&[u8], &[u8]); 8] = [
    (b"a/b", b"a.b"),
    (b"a/z", b"a.b"),
    (b"media/vision.graphqlx", b"media.graphqlx"),
    (b"a", b"a/b"),
    (b"a", b"ab"),
    (b"", b"a"),
    (b"a/", b"a/b"),
    (b"a//b", b"a/b"),
  ];
  for (before, after) in ordered {
    assert_eq!(compare_paths(before, after), Ordering::Less, "{before:?}");
    assert_eq!(compare_paths(after, before), Ordering::Greater, "{after:?}");
  }
}

/// On every path of at most `LONGEST` bytes over `/`, `.`, `a` and `b`, the order is total: it is
/// equal only for one path, swapping the paths reverses it, and sorted by it, every path comes
/// before every later one.
#[test]
fn the_order_is_total_on_every_short_path() {
  let mut all = paths(b"/.ab", LONGEST);
  for a in &all {
    for b in &all {
      let order = compare_paths(a, b);
      assert_eq!(order, compare_paths(b, a).reverse(), "{a:?} and {b:?}");
      assert_eq!(order == Ordering::Equal, a == b, "{a:?} and {b:?}");
    }
  }
  all.sort_by(|a: &Vec<u8>, b: &Vec<u8>| compare_paths(a, b));
  for (index, a) in all.iter().enumerate() {
    for b in &all[index + 1..] {
      assert_eq!(compare_paths(a, b), Ordering::Less, "{a:?} and {b:?}");
    }
  }
}
