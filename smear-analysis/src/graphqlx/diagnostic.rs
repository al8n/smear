//! What an analysis says: one [`Diagnostic`] per finding, built with [`Diagnostic::builder`], and
//! [`Diagnostics`], the list of them in one total order.

use core::cmp::Ordering;
use std::{borrow::Cow, collections::BinaryHeap, vec::Vec};

use tokora::diagnostic::{Code, Diagnose, Label, Location, PathSegment, Severity};

/// One finding about the input: smear's, or a consumer's built with the same builder.
///
/// A diagnostic owns its message, labels and help, and holds only locations into the sources: it
/// renders against texts the caller keeps, through [`SourceMap::bind`](super::SourceMap::bind).
/// It answers tokora's [`Diagnose`], which a renderer such as painty reads, and it lends every text
/// it holds, so reading one allocates nothing.
///
/// Its [`Display`](core::fmt::Display) is the message alone; where it points is structure, read
/// through [`Diagnose::primary`] and the labels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
  code: Code,
  severity: Severity,
  primary: Location,
  message: Cow<'static, str>,
  primary_label: Option<Cow<'static, str>>,
  labels: Vec<Secondary>,
  help: Option<Cow<'static, str>>,
}

/// A secondary label: a second place, and what is there.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Secondary {
  location: Location,
  text: Cow<'static, str>,
}

impl Diagnostic {
  /// Starts a diagnostic with `code`, pointing at `at`: an error with an empty message, no labels
  /// and no help, until the builder says otherwise.
  ///
  /// `at`'s source may be any source id, a consumer's own included: ids at or past the analysis's
  /// source count are the consumer's, for inputs of its own.
  #[inline]
  pub fn builder(code: Code, at: Location) -> DiagnosticBuilder {
    DiagnosticBuilder {
      diagnostic: Diagnostic {
        code,
        severity: Severity::Error,
        primary: at,
        message: Cow::Borrowed(""),
        primary_label: None,
        labels: Vec::new(),
        help: None,
      },
    }
  }
}

/// A [`Diagnostic`] being built, from [`Diagnostic::builder`].
///
/// Each text is static or owned: a literal is kept borrowed, and a phrase composed from the input,
/// "did you mean `MainDb`?", is kept owned. Either way the finished diagnostic lends it.
#[derive(Debug, Clone)]
pub struct DiagnosticBuilder {
  diagnostic: Diagnostic,
}

impl DiagnosticBuilder {
  /// The same diagnostic at `severity`. The default is [`Severity::Error`].
  #[inline]
  #[must_use]
  pub fn severity(mut self, severity: Severity) -> Self {
    self.diagnostic.severity = severity;
    self
  }

  /// The same diagnostic with `text` as its message.
  #[inline]
  #[must_use]
  pub fn message(mut self, text: impl Into<Cow<'static, str>>) -> Self {
    self.diagnostic.message = text.into();
    self
  }

  /// The same diagnostic with `text` beside its primary location.
  #[inline]
  #[must_use]
  pub fn primary_label(mut self, text: impl Into<Cow<'static, str>>) -> Self {
    self.diagnostic.primary_label = Some(text.into());
    self
  }

  /// The same diagnostic with one more secondary label: `text` at `at`, after the labels it has.
  ///
  /// `at` carries its own source, which need not be the primary location's.
  #[inline]
  #[must_use]
  pub fn label(mut self, at: Location, text: impl Into<Cow<'static, str>>) -> Self {
    self.diagnostic.labels.push(Secondary {
      location: at,
      text: text.into(),
    });
    self
  }

  /// The same diagnostic with `text` as its help: what the reader can do about it.
  #[inline]
  #[must_use]
  pub fn help(mut self, text: impl Into<Cow<'static, str>>) -> Self {
    self.diagnostic.help = Some(text.into());
    self
  }

  /// The finished diagnostic.
  #[inline]
  pub fn build(self) -> Diagnostic {
    self.diagnostic
  }
}

/// How many bytes of a spelling an `Excerpt` keeps from each end.
const EXCERPT_END: usize = 64;

/// A spelling taken from the input, as one of smear's diagnostics quotes it.
///
/// It is the one way a message, a label or a help line quotes what the input spelled, so that no
/// diagnostic holds more than a fixed number of bytes of any one spelling, and composing one reads
/// no more than that, however long the spelling and however many diagnostics quote it.
///
/// Its `Display` cuts a spelling of more than `2 * EXCERPT_END` bytes after its first
/// `EXCERPT_END` bytes and before its last `EXCERPT_END`, and writes what lies before the first
/// cut, `…`, and what lies after the second. A cut that falls inside a character moves back to the
/// character's first byte, over three continuation bytes at most, since a character is at most
/// four bytes long; it never walks further, so a spelling of nothing but continuation bytes costs
/// the same. Moving back never takes the second cut before the first, but the two meet where one
/// character holds both, which only a spelling of at most `2 * EXCERPT_END + 3` bytes allows: then
/// nothing lies between them to elide. So the whole spelling is written when it is at most
/// `2 * EXCERPT_END` bytes long, and when the two cuts meet. Bytes that are not UTF-8 are written
/// as U+FFFD, as `String::from_utf8_lossy` writes them, and only the bytes written are decoded.
///
/// An excerpt therefore holds at most `2 * 64 + 3` of the spelling's bytes, 131: when elided, at
/// most 64 before the `…` and at most 67 after it, and when whole, at most 131. Each is written as
/// at most the three bytes of a U+FFFD, and the `…` is three more, so an elided spelling writes at
/// most 396 bytes and a whole one at most 393: an excerpt writes at most 396 bytes.
pub(crate) struct Excerpt<'a>(pub(crate) &'a [u8]);

impl core::fmt::Display for Excerpt<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let bytes = self.0;
    if bytes.len() <= 2 * EXCERPT_END {
      return write_lossy(f, bytes);
    }
    let head = boundary(bytes, EXCERPT_END);
    let tail = boundary(bytes, bytes.len() - EXCERPT_END);
    if head >= tail {
      // The two cuts met, so nothing lies between them to elide.
      return write_lossy(f, bytes);
    }
    write_lossy(f, &bytes[..head])?;
    f.write_str("…")?;
    write_lossy(f, &bytes[tail..])
  }
}

/// The offset `at` into `bytes`, moved back to the first byte of the character it falls inside:
/// over three continuation bytes at most, and never further.
fn boundary(bytes: &[u8], at: usize) -> usize {
  let mut cut = at;
  while at - cut < 3
    && cut > 0
    && bytes
      .get(cut)
      .is_some_and(|byte: &u8| is_continuation(*byte))
  {
    cut -= 1;
  }
  cut
}

/// Whether `byte` continues a character, as `10xxxxxx` does, rather than beginning one.
fn is_continuation(byte: u8) -> bool {
  (byte & 0b1100_0000) == 0b1000_0000
}

/// Writes `bytes` as `String::from_utf8_lossy` writes them: each sequence that is not UTF-8 as one
/// U+FFFD.
fn write_lossy(f: &mut core::fmt::Formatter<'_>, bytes: &[u8]) -> core::fmt::Result {
  for chunk in bytes.utf8_chunks() {
    f.write_str(chunk.valid())?;
    if !chunk.invalid().is_empty() {
      f.write_str("\u{FFFD}")?;
    }
  }
  Ok(())
}

/// A diagnostic read as structure: its code, severity and primary location by value, and its
/// primary label, labels and help lent from the diagnostic.
///
/// No path segments: an analysis is about source text, never about a response.
impl Diagnose for Diagnostic {
  #[inline]
  fn code(&self) -> Code {
    self.code
  }

  #[inline]
  fn severity(&self) -> Severity {
    self.severity
  }

  #[inline]
  fn primary(&self) -> Location {
    self.primary
  }

  #[inline]
  fn primary_label(&self) -> Option<&str> {
    self.primary_label.as_deref()
  }

  #[inline]
  fn labels(&self) -> usize {
    self.labels.len()
  }

  #[inline]
  fn label(&self, index: usize) -> Option<Label<'_>> {
    let label = self.labels.get(index)?;
    Some(Label::new(label.location, &label.text))
  }

  #[inline]
  fn path_segments(&self) -> usize {
    0
  }

  #[inline]
  fn path_segment(&self, _: usize) -> Option<PathSegment<'_>> {
    None
  }

  #[inline]
  fn help(&self) -> Option<&str> {
    self.help.as_deref()
  }
}

impl core::fmt::Display for Diagnostic {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(&self.message)
  }
}

impl core::error::Error for Diagnostic {}

/// The diagnostics of an analysis, or of a consumer's own run, in one total order.
///
/// # The order
///
/// By source id; then by span start, with a location covering its whole source before every span
/// of that source; then span end; then code; then severity, errors first, then warnings, then
/// advice; then message; then primary label, absent first; then help, absent first; then the
/// labels, label by label, each by its location and then its text, and a shorter list before a
/// longer one it begins. Two diagnostics equal in all of that are identical.
///
/// # The ceiling
///
/// An analysis keeps the least diagnostics in that order, up to its
/// [`Limits::max_diagnostics`](super::Limits::max_diagnostics), and drops the rest, so which are
/// kept depends on the diagnostics alone and never on the order they were reported in.
/// [`truncated`](Self::truncated) says whether any was dropped. The list is also truncated by a
/// syntax error a parser found and did not lend, which is counted and has no diagnostic.
///
/// The verdict is counted, never read off the list: [`error_count`](Self::error_count) counts
/// every error reported, kept or dropped, and [`has_errors`](Self::has_errors) reads that count.
///
/// Collecting diagnostics into a list, as `Diagnostics` implements [`FromIterator`], applies no
/// ceiling.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostics {
  list: Vec<Diagnostic>,
  errors: usize,
  truncated: bool,
}

impl Diagnostics {
  /// The diagnostics kept, in order.
  #[inline]
  pub fn iter(&self) -> core::slice::Iter<'_, Diagnostic> {
    self.list.iter()
  }

  /// How many diagnostics were kept.
  #[inline]
  pub fn len(&self) -> usize {
    self.list.len()
  }

  /// Whether none was kept. A truncated list is empty only when its ceiling was 0, or when every
  /// error it counts is one a parser did not lend.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.list.is_empty()
  }

  /// Whether the list holds fewer diagnostics than were found: the ceiling dropped one, or a
  /// parser found a syntax error it did not lend.
  #[inline]
  pub fn truncated(&self) -> bool {
    self.truncated
  }

  /// How many errors were found: every diagnostic of [`Severity::Error`] reported, kept or
  /// dropped, and every syntax error a parser found and did not lend.
  #[inline]
  pub fn error_count(&self) -> usize {
    self.errors
  }

  /// Whether any error was found: a diagnostic of [`Severity::Error`] reported, kept or dropped,
  /// or a syntax error a parser found and did not lend.
  #[inline]
  pub fn has_errors(&self) -> bool {
    self.errors != 0
  }

  /// Takes `other`'s diagnostics into this list, in the same order.
  ///
  /// Both lists are kept whole, with no ceiling; the error counts add; and the result is truncated
  /// if either list was. So smear's diagnostics and a consumer's become one list for a renderer.
  pub fn merge(&mut self, other: Diagnostics) {
    let Diagnostics {
      list,
      errors,
      truncated,
    } = other;
    self.list.extend(list);
    self.list.sort_by(compare);
    self.errors = self.errors.saturating_add(errors);
    self.truncated |= truncated;
  }
}

impl<'a> IntoIterator for &'a Diagnostics {
  type Item = &'a Diagnostic;
  type IntoIter = core::slice::Iter<'a, Diagnostic>;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.list.iter()
  }
}

impl IntoIterator for Diagnostics {
  type Item = Diagnostic;
  type IntoIter = std::vec::IntoIter<Diagnostic>;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.list.into_iter()
  }
}

/// Every diagnostic, sorted into the list's order, with no ceiling.
impl FromIterator<Diagnostic> for Diagnostics {
  fn from_iter<I: IntoIterator<Item = Diagnostic>>(diagnostics: I) -> Self {
    let mut collector = Collector::new(usize::MAX);
    for diagnostic in diagnostics {
      collector.report(diagnostic);
    }
    collector.finish()
  }
}

/// A list being reported into: the least diagnostics so far, up to a ceiling, and every error
/// counted.
pub(crate) struct Collector {
  ceiling: usize,
  /// A max-heap, so the greatest kept diagnostic is the one the ceiling drops next.
  kept: BinaryHeap<Ranked>,
  errors: usize,
  truncated: bool,
}

impl Collector {
  /// An empty list that keeps at most `ceiling` diagnostics.
  pub(crate) fn new(ceiling: usize) -> Self {
    Self {
      ceiling,
      kept: BinaryHeap::new(),
      errors: 0,
      truncated: false,
    }
  }

  /// Reports `diagnostic`: counted if it is an error, then kept unless `ceiling` lesser ones
  /// already are.
  ///
  /// Each report costs a logarithm of the ceiling, and the list never holds more than the ceiling
  /// and one, so a run reporting without bound runs in bounded memory.
  pub(crate) fn report(&mut self, diagnostic: Diagnostic) {
    if diagnostic.severity == Severity::Error {
      self.errors = self.errors.saturating_add(1);
    }
    self.kept.push(Ranked(diagnostic));
    if self.kept.len() > self.ceiling {
      // The greatest of the ceiling and one: the new diagnostic itself, when it is the greatest.
      let _ = self.kept.pop();
      self.truncated = true;
    }
  }

  /// Counts `errors` errors that have no diagnostic, and marks the list truncated when there is
  /// one: the list then holds fewer diagnostics than were found.
  pub(crate) fn count_unlisted(&mut self, errors: usize) {
    if errors > 0 {
      self.errors = self.errors.saturating_add(errors);
      self.truncated = true;
    }
  }

  /// The finished list, in order.
  pub(crate) fn finish(self) -> Diagnostics {
    let mut list: Vec<Diagnostic> = Vec::with_capacity(self.kept.len());
    for ranked in self.kept.into_sorted_vec() {
      list.push(ranked.0);
    }
    Diagnostics {
      list,
      errors: self.errors,
      truncated: self.truncated,
    }
  }
}

/// The one path the analysis reports through: every phase, smear's rules, and a consumer's walk.
///
/// It keeps what a `Collector` keeps. One made with `new`, for `analyze`'s own run, also notes
/// where each error was reported, kept or dropped by the ceiling, at the moment it is reported:
/// what an error marks (`Model::mark`) can then never depend on which diagnostics the ceiling
/// keeps. A parser's syntax error is the one error it notes nothing for: the parse step reports
/// those through `report_syntax`. One made with `without_marks`, for a consumer's walk, which
/// marks nothing, notes nothing beside the list, so it runs in memory bounded by the ceiling
/// however much is reported.
pub(crate) struct Reporter {
  collector: Collector,
  /// Whether it notes where each error points.
  marking: bool,
  /// The primary location of every error reported since the marks were last taken; always empty
  /// when it is not marking.
  marks: Vec<Location>,
}

impl Reporter {
  /// A reporter whose list keeps at most `ceiling` diagnostics, and which notes where each error
  /// points: the one `analyze` runs its phases and smear's rules through, whose errors mark the
  /// model.
  pub(crate) fn new(ceiling: usize) -> Self {
    Self {
      collector: Collector::new(ceiling),
      marking: true,
      marks: Vec::new(),
    }
  }

  /// A reporter whose list keeps at most `ceiling` diagnostics, and which notes nothing beside it:
  /// the one a consumer's walk reports through, which marks nothing.
  pub(crate) fn without_marks(ceiling: usize) -> Self {
    Self {
      collector: Collector::new(ceiling),
      marking: false,
      marks: Vec::new(),
    }
  }

  /// Reports `diagnostic`: notes where it points if it is an error and the reporter is marking,
  /// then hands it to the list.
  pub(crate) fn report(&mut self, diagnostic: Diagnostic) {
    if self.marking && diagnostic.severity == Severity::Error {
      self.marks.push(diagnostic.primary);
    }
    self.collector.report(diagnostic);
  }

  /// Reports a parser's syntax error: hands it to the list and notes nothing, whether the reporter
  /// is marking or not.
  ///
  /// A syntax error marks nothing in the model. What the parser could not read has no image in
  /// the document its file answers, and so no contribution to mark, and a definition the parser
  /// read past a reported mistake stands in the document unmarked. And the marks are a list the
  /// diagnostic ceiling does not bound, while a parser that recovers lends an error for every
  /// mistake of a file: noted, a file's syntax errors would grow it with the file.
  pub(crate) fn report_syntax(&mut self, diagnostic: Diagnostic) {
    self.collector.report(diagnostic);
  }

  /// Counts `errors` syntax errors a parser found and did not lend, as a layer that keeps a
  /// bounded number of records does past its ceiling: each is an error of the analysis with no
  /// diagnostic, so the list is truncated by them. Like every syntax error, they note no mark.
  pub(crate) fn count_unlent(&mut self, errors: usize) {
    self.collector.count_unlisted(errors);
  }

  /// How many diagnostics the list keeps.
  pub(crate) fn ceiling(&self) -> usize {
    self.collector.ceiling
  }

  /// Where the errors reported since the marks were last taken point, in the order they were
  /// reported: none, when the reporter is not marking.
  pub(crate) fn take_marks(&mut self) -> Vec<Location> {
    core::mem::take(&mut self.marks)
  }

  /// The finished list, and where the errors reported since the marks were last taken point: none,
  /// when the reporter is not marking.
  pub(crate) fn finish(self) -> (Diagnostics, Vec<Location>) {
    (self.collector.finish(), self.marks)
  }
}

/// A diagnostic ordered by [`compare`], which is what the heap needs.
struct Ranked(Diagnostic);

impl PartialEq for Ranked {
  fn eq(&self, other: &Self) -> bool {
    compare(&self.0, &other.0) == Ordering::Equal
  }
}

impl Eq for Ranked {}

impl PartialOrd for Ranked {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for Ranked {
  fn cmp(&self, other: &Self) -> Ordering {
    compare(&self.0, &other.0)
  }
}

/// The total order of [`Diagnostics`].
fn compare(a: &Diagnostic, b: &Diagnostic) -> Ordering {
  compare_locations(a.primary, b.primary)
    .then_with(|| a.code.cmp(&b.code))
    .then_with(|| severity_rank(a.severity).cmp(&severity_rank(b.severity)))
    .then_with(|| a.message.cmp(&b.message))
    .then_with(|| a.primary_label.cmp(&b.primary_label))
    .then_with(|| a.help.cmp(&b.help))
    .then_with(|| compare_labels(&a.labels, &b.labels))
}

/// Two locations: by source, then a whole source before every span of it, then span start, then
/// span end.
fn compare_locations(a: Location, b: Location) -> Ordering {
  a.source()
    .cmp(&b.source())
    .then_with(|| match (a.span(), b.span()) {
      (None, None) => Ordering::Equal,
      (None, Some(_)) => Ordering::Less,
      (Some(_), None) => Ordering::Greater,
      (Some(x), Some(y)) => x
        .start()
        .cmp(&y.start())
        .then_with(|| x.end().cmp(&y.end())),
    })
}

/// Two label lists, label by label, the shorter first when one begins the other.
fn compare_labels(a: &[Secondary], b: &[Secondary]) -> Ordering {
  for (left, right) in a.iter().zip(b.iter()) {
    let order =
      compare_locations(left.location, right.location).then_with(|| left.text.cmp(&right.text));
    if order != Ordering::Equal {
      return order;
    }
  }
  a.len().cmp(&b.len())
}

/// A severity's place in the order: errors, warnings, advice, then any rung tokora adds later.
fn severity_rank(severity: Severity) -> u8 {
  match severity {
    Severity::Error => 0,
    Severity::Warning => 1,
    Severity::Advice => 2,
    _ => 3,
  }
}

#[cfg(test)]
mod tests;
