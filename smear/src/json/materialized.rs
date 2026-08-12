//! [`WriteJson`] for the two materialised value trees.
//!
//! # Why these two and not their four siblings
//!
//! Each width declares two enums — `InputValue`, which can hold a `Variable`, and
//! `ConstInputValue`, which cannot. **Only the constant ones are implemented**, and the reason is
//! not tidiness: a response has no variables in it. Draft §6.1 coerces every variable before
//! execution begins, so a `$name` reaching a response value would mean a value that was never
//! resolved, and there is no JSON for it — writing the name would invent a string the schema
//! never promised, and writing `null` would answer a question nobody asked. An unimplemented trait
//! is a compile error at the site that tried; either alternative is a wrong response.
//!
//! # One body, two widths
//!
//! The two implementations are one macro at two instantiations rather than two hand-written
//! copies, for the reason `syntactic::value::materialized` gives about its own leaves: they are
//! the same production at two payloads, so there should be one of them. What the widths disagree
//! about is a single expression — how the integer reaches [`Json::int_leaf`] — and that
//! disagreement is the argument of the macro, so a reader can see the whole of the difference in
//! one place.
//!
//! # Where the widths show
//!
//! [`Json::int_leaf`] renders an integer outside draft §3.5.1's 32-bit range as a JSON string. The
//! `i32` tree cannot produce one — its parser refused the literal, as
//! `ErrorData::IntOverflow` — so for that width the branch is unreachable and every `Int` in the
//! response is a JSON number. The `i64` tree can, and `2147483648` comes out of it as
//! `"2147483648"`. That is the permissive reading and the specification's, told apart in the
//! response rather than only in the parse.

use core::fmt;

use smear_parser::graphql::ast::{materialized, materialized32};

use super::{Error, Json, WriteJson};

/// Declares [`WriteJson`] for one width's `ConstInputValue`.
///
/// `$int` says how that width's integer payload reaches [`Json::int_leaf`], which is the whole of
/// what the two widths disagree about.
macro_rules! const_input_value {
  ($tree:ident, $int:expr) => {
    impl<S> WriteJson for $tree::ConstInputValue<S>
    where
      S: AsRef<str>,
    {
      fn write_json<W: fmt::Write>(&self, out: &mut Json<W>) -> Result<(), Error> {
        match self {
          Self::Null(_) => out.null(),
          Self::Boolean(value) => out.bool(value.value()),
          // The leaf keeps its source slice, escapes and all — that is what makes
          // materialisation allocate nothing — so the value is cooked on the way out.
          Self::String(value) => out.graphql_string(value.source().as_ref()),
          // Draft §6.4.3 serialises an enum value as its member name, which is a `String` in the
          // response.
          Self::Enum(value) => out.string(value.source().as_ref()),
          Self::Int(value) => out.int_leaf($int(*value.source())),
          Self::Float(value) => out.double(*value.source()),
          Self::List(list) => {
            let mut array = out.array()?;
            for element in list.values() {
              element.write_json(array.element()?)?;
            }
            array.end()
          }
          Self::Object(object) => {
            let mut map = out.object()?;
            for field in object.fields() {
              field
                .value()
                .write_json(map.key(field.name().source().as_ref())?)?;
            }
            map.end()
          }
        }
      }
    }
  };
}

const_input_value!(materialized, core::convert::identity::<i64>);
const_input_value!(materialized32, i64::from);
