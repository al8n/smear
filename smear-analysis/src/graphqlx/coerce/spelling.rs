//! A written value's spelling as the model's text arena keeps it, which holds UTF-8 alone.
//!
//! A spelling that is UTF-8 is copied as it is, and the byte at an offset of it is at that offset
//! of the copy. One that is not has each sequence that is not UTF-8 replaced by one U+FFFD, as
//! `String::from_utf8_lossy` writes it: a U+FFFD is three bytes and the sequence it replaces one to
//! three, so the copy grows there by up to two bytes, and an offset past the sequence moves by the
//! growth before it. A node's span starts and ends at a token's edge, never inside a sequence that
//! is not UTF-8, so its offsets move to a character's edge in the copy.

use std::{string::String, vec::Vec};

use crate::graphqlx::id::dense;

/// Appends `written` to `out`, each sequence of it that is not UTF-8 replaced by one U+FFFD, as
/// `String::from_utf8_lossy` writes it, and appends to `growths`, for each such sequence in order,
/// the offset in `written` just past it and how many bytes longer the copy is than `written` up to
/// there.
pub(super) fn copy(written: &[u8], out: &mut String, growths: &mut Vec<(u32, u32)>) {
  let replaced = dense(char::REPLACEMENT_CHARACTER.len_utf8());
  let mut at: usize = 0;
  let mut grown: u32 = 0;
  for chunk in written.utf8_chunks() {
    out.push_str(chunk.valid());
    let invalid = chunk.invalid();
    at = at
      .saturating_add(chunk.valid().len())
      .saturating_add(invalid.len());
    if !invalid.is_empty() {
      out.push(char::REPLACEMENT_CHARACTER);
      grown = grown.saturating_add(replaced.saturating_sub(dense(invalid.len())));
      growths.push((dense(at), grown));
    }
  }
}

/// Where the byte at `at` of a spelling is in its copy, `growths` being what [`copy`] recorded of
/// it: `at`, moved by how much the copy grew before it.
pub(super) fn offset(growths: &[(u32, u32)], at: u32) -> u32 {
  let before = growths.partition_point(|&(after, _): &(u32, u32)| after <= at);
  let grown = before
    .checked_sub(1)
    .and_then(|row: usize| growths.get(row))
    .map_or(0, |&(_, grown): &(u32, u32)| grown);
  at.saturating_add(grown)
}

#[cfg(test)]
mod tests;
