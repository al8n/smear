pub trait Reporter {
  fn report<'a, FileId>(
    &self,
    file_id: FileId,
  ) -> codespan_reporting::diagnostic::Diagnostic<FileId>
  where
    FileId: 'a + Copy + PartialEq;
}
