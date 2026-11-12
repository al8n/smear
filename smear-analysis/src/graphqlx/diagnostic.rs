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
/// [`truncated`](Self::truncated) says whether any was dropped.
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

  /// Whether none was kept. A truncated list is never empty unless its ceiling was 0.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.list.is_empty()
  }

  /// Whether the ceiling dropped any diagnostic.
  #[inline]
  pub fn truncated(&self) -> bool {
    self.truncated
  }

  /// How many diagnostics of [`Severity::Error`] were reported, kept or dropped.
  #[inline]
  pub fn error_count(&self) -> usize {
    self.errors
  }

  /// Whether any diagnostic of [`Severity::Error`] was reported, kept or dropped.
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
/// It keeps what a `Collector` keeps, and it notes where each error was reported, kept or dropped
/// by the ceiling, at the moment it is reported: what an error marks (`Model::mark`) can then never
/// depend on which diagnostics the ceiling keeps.
pub(crate) struct Reporter {
  collector: Collector,
  /// The primary location of every error reported since the marks were last taken.
  marks: Vec<Location>,
}

impl Reporter {
  /// A reporter whose list keeps at most `ceiling` diagnostics.
  pub(crate) fn new(ceiling: usize) -> Self {
    Self {
      collector: Collector::new(ceiling),
      marks: Vec::new(),
    }
  }

  /// Reports `diagnostic`: notes where it points if it is an error, then hands it to the list.
  pub(crate) fn report(&mut self, diagnostic: Diagnostic) {
    if diagnostic.severity == Severity::Error {
      self.marks.push(diagnostic.primary);
    }
    self.collector.report(diagnostic);
  }

  /// How many diagnostics the list keeps.
  pub(crate) fn ceiling(&self) -> usize {
    self.collector.ceiling
  }

  /// Where the errors reported since the marks were last taken point, in the order they were
  /// reported.
  pub(crate) fn take_marks(&mut self) -> Vec<Location> {
    core::mem::take(&mut self.marks)
  }

  /// The finished list, and where the errors reported since the marks were last taken point.
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
