//! Types: each written occurrence, and each type's interned identity.

use core::num::NonZeroU32;

/// Where a written path starts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum PathRoot {
  /// No root: the path starts in the scope it is written in, as `Int` and `geo::Point` do.
  Relative,
  /// `~::`: the root module of the package it is written in.
  Package,
  /// `^::`: a package the one it is written in may reach, named by the path's first segment.
  Dependency,
  /// `..::`, as many times as it holds: that many modules up from the one it is written in.
  Parent(NonZeroU32),
}
