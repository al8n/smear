//! Lowering: the parsed documents into the model's modules and definitions.
//!
//! This is the last step that sees a parsed document, and so the last that is generic: a document
//! is `TypeSystemDocument<&str>`, or `TypeSystemDocument<&[u8]>` for a byte text that is not UTF-8,
//! never the caller's own text type. Lowering copies every spelling the model keeps, so the model
//! it builds borrows nothing, and everything after it is compiled once.

use super::parse::Documents;

/// Lowers `documents` into the model.
///
/// Nothing is lowered yet: the model's modules and definitions, and the phases and rules that
/// build and read them, arrive with the next part of the analyzer, which fills this in. Until then
/// the model holds the packages and their sources, and the documents are dropped once parsed.
pub(crate) fn lower(_documents: &Documents<'_>) {}
