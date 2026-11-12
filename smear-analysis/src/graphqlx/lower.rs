//! Lowering: what the declarations hold beyond their identity, their members, types, values and
//! applications, into the model.
//!
//! This is the last step that sees a parsed document, and so the last that is generic: a document
//! is `TypeSystemDocument<&str>`, or `TypeSystemDocument<&[u8]>` for a byte text that is not UTF-8,
//! never the caller's own text type. Lowering copies every spelling the model keeps, so the model
//! it builds borrows nothing, and everything after it is compiled once.

use super::declare::Declarations;

/// Lowers what `declarations` holds into the model.
///
/// Nothing is lowered yet: the definitions' members, types and values, and the phases and rules
/// that build and read them, arrive with the next parts of the analyzer, which fill this in. Until
/// then the model holds the packages, their sources, their modules and every definition's
/// identity.
pub(crate) fn lower(_declarations: &Declarations<'_, '_>) {}
