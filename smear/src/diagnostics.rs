use codespan_reporting::diagnostic::Diagnostic as CodespanDiagnostic;

pub(crate) enum DiagnosticInner<FileId> {
  Single(CodespanDiagnostic<FileId>),
  Multiple(Vec<DiagnosticInner<FileId>>),
}

pub struct Diagnostic<FileId>(pub(crate) DiagnosticInner<FileId>);

impl<FileId> From<CodespanDiagnostic<FileId>> for Diagnostic<FileId> {
  fn from(value: CodespanDiagnostic<FileId>) -> Self {
    Self(DiagnosticInner::Single(value))
  }
}

impl<FileId> From<Vec<CodespanDiagnostic<FileId>>> for Diagnostic<FileId> {
  fn from(value: Vec<CodespanDiagnostic<FileId>>) -> Self {
    Self(DiagnosticInner::Multiple(
      value.into_iter().map(DiagnosticInner::Single).collect(),
    ))
  }
}

impl<FileId> From<Vec<Diagnostic<FileId>>> for Diagnostic<FileId> {
  fn from(value: Vec<Diagnostic<FileId>>) -> Self {
    Self(DiagnosticInner::Multiple(
      value.into_iter().map(|d| d.0).collect(),
    ))
  }
}

pub trait Reporter {
  fn report<'a, FileId>(&self, file_id: FileId) -> Diagnostic<FileId>
  where
    FileId: 'a + Copy + PartialEq;
}

pub trait Diagnosticable {
  type Error: Reporter;

  /// The syntax node that this directive is parsed from.
  type Node: apollo_parser::ast::AstNode;

  /// Parses from the given directive.
  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized;
}

pub trait NamedDiagnosticable: Diagnosticable {
  /// Returns the possible names of the node.
  fn possible_names() -> &'static [&'static str];

  /// Returns the short name of the node.
  fn short() -> Option<char>;

  /// Returns the long name of the node.
  fn long() -> &'static str;

  /// Returns the aliases of the node.
  fn aliases() -> &'static [&'static str];
}
