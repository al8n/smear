pub trait Reporter {
  fn report<'a, FileId>(
    &self,
    file_id: FileId,
  ) -> codespan_reporting::diagnostic::Diagnostic<FileId>
  where
    FileId: 'a + Copy + PartialEq;
}

pub trait Diagnosticable {
  type Error: Reporter;

  /// The syntax node that this directive is parsed from.
  type Node: apollo_parser::ast::AstNode;

  /// Parses from the given directive.
  fn parse(node: Self::Node) -> Result<Self, Self::Error>
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
