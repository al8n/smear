//! The package set's own rules: names, file paths, dependencies and the total source size. Each
//! violation is reported at the whole of the offending package's package source, since none of
//! them has a position in any text, and its message quotes each name and path as an `Excerpt`.

use std::{format, string::String, vec::Vec};

use tokora::diagnostic::Location;

use super::{
  builtin::is_built_in,
  code::DiagnosticKind,
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{PackageId, dense},
  limits::Limits,
  set::{PackageView, ROOT},
  source::SourceMap,
};

/// Reports every package-level violation of `packages`, which are in id order, and answers each
/// package's resolved dependencies, in id order.
pub(crate) fn package_set(
  packages: &[PackageView<'_>],
  sources: &SourceMap,
  reporter: &mut Reporter,
) -> Vec<Vec<PackageId>> {
  let mut resolved: Vec<Vec<PackageId>> = Vec::with_capacity(packages.len());
  for (index, package) in packages.iter().enumerate() {
    let at = Location::entire(sources.package_sources(index).start());
    // The built-ins are smear's own, and nothing about them is the caller's to get wrong.
    if index >= ROOT {
      check_name(packages, index, at, reporter);
      check_paths(package, at, reporter);
    }
    resolved.push(dependencies(packages, index, at, reporter));
  }
  resolved
}

/// Reports the supplied packages if their sources hold more bytes than `limits` admits, at the
/// root package; answers whether they are within it.
pub(crate) fn source_bytes(
  packages: &[PackageView<'_>],
  sources: &SourceMap,
  limits: &Limits,
  reporter: &mut Reporter,
) -> bool {
  let mut total: u64 = 0;
  for package in &packages[ROOT..] {
    for file in &package.files {
      total = total.saturating_add(file.text.bytes().len() as u64);
    }
  }
  let max = u64::from(limits.max_source_bytes());
  if total <= max {
    return true;
  }
  report(
    reporter,
    DiagnosticKind::ResourceLimit,
    Location::entire(sources.package_sources(ROOT).start()),
    format!("the supplied packages hold {total} bytes of source, over the limit of {max}"),
    "raise `Limits::max_source_bytes`, or supply less source",
  );
  false
}

/// Reports a supplied package's name: one that is a built-in's, one that is not a Name, and one
/// that an earlier supplied package already has.
fn check_name(packages: &[PackageView<'_>], index: usize, at: Location, reporter: &mut Reporter) {
  let name = packages[index].name;
  if is_built_in(name) {
    report(
      reporter,
      DiagnosticKind::ReservedPackageName,
      at,
      format!(
        "`{}` is a built-in package's name, so no supplied package can take it",
        Excerpt(name)
      ),
      "`core`, `alloc` and `std` are smear's own packages; choose another name",
    );
  } else if !is_name(name) {
    report(
      reporter,
      DiagnosticKind::InvalidPackageName,
      at,
      format!(
        "`{}` is not a GraphQLx name, so it cannot name a package",
        Excerpt(name)
      ),
      "a package name is a letter or `_`, then letters, digits or `_`",
    );
  }

  // The dependencies follow the root sorted by name, so an earlier package with this name is the
  // root or the dependency just before.
  let duplicate = index > ROOT
    && (packages[ROOT].name == name || (index > ROOT + 1 && packages[index - 1].name == name));
  if duplicate {
    report(
      reporter,
      DiagnosticKind::DuplicatePackageName,
      at,
      format!("two supplied packages are named `{}`", Excerpt(name)),
      "supply each package once",
    );
  }
}

/// Reports a package's file paths: each one that breaks the path rule, and each one that an
/// earlier file of the package already has.
fn check_paths(package: &PackageView<'_>, at: Location, reporter: &mut Reporter) {
  let mut previous: Option<&[u8]> = None;
  for file in &package.files {
    if let Some(problem) = path_problem(file.path) {
      report(
        reporter,
        DiagnosticKind::InvalidFilePath,
        at,
        format!(
          "package `{}` holds the file path `{}`, which {problem}",
          Excerpt(package.name),
          Excerpt(file.path)
        ),
        "a file path is relative to its package and `/`-separated, with no empty, `.` or `..` \
         segment",
      );
    }
    // The files are sorted by path, so files sharing a path sit side by side.
    if previous == Some(file.path) {
      report(
        reporter,
        DiagnosticKind::DuplicateFilePath,
        at,
        format!(
          "package `{}` holds two files at `{}`",
          Excerpt(package.name),
          Excerpt(file.path)
        ),
        "a package holds each path once; remove or rename one of the files",
      );
    }
    previous = Some(file.path);
  }
}

/// What is wrong with `path` as a file path, if anything. The module tree places no file whose
/// path this refuses.
pub(crate) fn path_problem(path: &[u8]) -> Option<&'static str> {
  if core::str::from_utf8(path).is_err() {
    return Some("is not UTF-8");
  }
  if path.contains(&b'\\') {
    return Some("separates with `\\` where a path is `/`-separated");
  }
  if path.first() == Some(&b'/') || has_drive(path) {
    return Some("is absolute");
  }
  for segment in path.split(|byte: &u8| *byte == b'/') {
    match segment {
      b"" => return Some("has an empty segment"),
      b"." => return Some("has a `.` segment"),
      b".." => return Some("has a `..` segment"),
      _ => {}
    }
  }
  None
}

/// Whether `path` begins with a drive, as `C:` does: absolute, or relative to a drive's own
/// working directory, and in neither case relative to the package.
fn has_drive(path: &[u8]) -> bool {
  matches!(path, [letter, b':', ..] if letter.is_ascii_alphabetic())
}

/// Whether `name` is a GraphQLx Name: a letter or `_`, then letters, digits or `_`. A package's
/// name, and a directory that is a module, must be one.
pub(crate) fn is_name(name: &[u8]) -> bool {
  match name.split_first() {
    Some((first, rest)) => {
      (first.is_ascii_alphabetic() || *first == b'_')
        && rest
          .iter()
          .all(|byte: &u8| byte.is_ascii_alphanumeric() || *byte == b'_')
    }
    None => false,
  }
}

/// Resolves the package at `index`'s declared dependencies, reporting each that is not supplied,
/// and answers the resolved ones in id order.
fn dependencies(
  packages: &[PackageView<'_>],
  index: usize,
  at: Location,
  reporter: &mut Reporter,
) -> Vec<PackageId> {
  let package = &packages[index];
  let mut resolved: Vec<PackageId> = Vec::with_capacity(package.dependencies.len());
  for &name in &package.dependencies {
    if let Some(found) = resolve(packages, index, name) {
      resolved.push(PackageId::new(dense(found)));
    } else if name == package.name {
      report(
        reporter,
        DiagnosticKind::MissingDependency,
        at,
        format!(
          "package `{}` declares itself as a dependency",
          Excerpt(name)
        ),
        "a package reaches its own items through `~::`; remove the declaration",
      );
    } else {
      report(
        reporter,
        DiagnosticKind::MissingDependency,
        at,
        format!(
          "package `{}` declares the dependency `{}`, which is not supplied",
          Excerpt(package.name),
          Excerpt(name)
        ),
        "supply the package, or remove the declaration",
      );
    }
  }
  resolved.sort_unstable();
  resolved.dedup();
  resolved
}

/// The index of the package a dependency named `name`, declared by the package at `from`, is:
/// the built-in with that name, or else the supplied package with that name that comes first in id
/// order, other than `from` itself, which a package never reaches as a dependency.
fn resolve(packages: &[PackageView<'_>], from: usize, name: &[u8]) -> Option<usize> {
  if let Some(index) = packages[..ROOT]
    .iter()
    .position(|package: &PackageView<'_>| package.name == name)
  {
    return Some(index);
  }
  if from != ROOT && packages[ROOT].name == name {
    return Some(ROOT);
  }
  let dependencies = &packages[ROOT + 1..];
  let first = dependencies.partition_point(|package: &PackageView<'_>| package.name < name);
  for (offset, package) in dependencies[first..].iter().enumerate() {
    if package.name != name {
      break;
    }
    let index = ROOT + 1 + first + offset;
    if index != from {
      return Some(index);
    }
  }
  None
}

/// Reports one package-level diagnostic of `kind`.
fn report(
  reporter: &mut Reporter,
  kind: DiagnosticKind,
  at: Location,
  message: String,
  help: &'static str,
) {
  reporter.report(
    Diagnostic::builder(kind.code(), at)
      .severity(kind.severity())
      .message(message)
      .primary_label(kind.title())
      .help(help)
      .build(),
  );
}
