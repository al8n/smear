//! `smear/validator` claims to gate the schema builder — and, less visibly, the `Diagnose` impl on
//! the error it refuses with.
//!
//! A TRAIT IMPL, and the one that was omitted from the fence. `impl Diagnose for SchemaError` lives
//! behind `smear-schema/build` (`smear-schema/src/error.rs:1001`); `smear::diagnostic::Diagnose` is
//! smear's own public trait, and before this pair was asserted a consumer with `smear/validator`
//! off could apply it to a build-only error. The type is named through `smear-schema` because that
//! is what the leaking graph looks like: the consumer already has the member as a direct
//! dependency, which is how the feature got switched on behind `smear` in the first place.
pub fn render<D: smear::diagnostic::Diagnose>(_d: &D) {}

pub fn gated(error: &smear_schema::SchemaError) {
  render(error);
}
