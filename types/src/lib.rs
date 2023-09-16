pub mod directive;
pub mod error;
pub mod utils;
pub mod value;

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

pub trait Encodable {
  type SDL: core::fmt::Display;

  fn encode(&self) -> Self::SDL;
}

pub trait Diagnosticable {
  type Error: Reporter;

  /// The syntax node that this directive is parsed from.
  type Node: apollo_parser::ast::AstNode;

  type Descriptor;

  /// Parses from the given directive.
  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized;

  fn descriptor() -> &'static Self::Descriptor;
}

#[viewit::viewit(vis_all = "pub(crate)", setters(skip), getters(style = "move"))]
#[derive(Copy, Clone, Default, Debug)]
pub struct Deprecated {
  version: Option<&'static str>,
  reason: Option<&'static str>,
  suggestion: Option<&'static str>,
}

impl Deprecated {
  pub fn new(
    version: Option<&'static str>,
    reason: Option<&'static str>,
    suggestion: Option<&'static str>,
  ) -> Self {
    Self {
      version,
      reason,
      suggestion,
    }
  }
}

impl core::fmt::Display for Deprecated {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Deprecated")?;
    if let Some(version) = self.version {
      write!(f, " since version {version}")?;
    }
    if let Some(reason) = self.reason {
      write!(f, " because {reason}")?;
    }
    if let Some(suggestion) = self.suggestion {
      write!(f, " , please see {suggestion}")?;
    }
    Ok(())
  }
}
