//! What reading `__type(name:)`'s literal costs: the allocations, and the passes charged for them.
//!
//! `smear/tests/proto_introspection.rs` holds the answers a literal gets, escapes applied and a
//! block string dedented, from outside. What it cannot see is what an answer costs, because both
//! instruments are this crate's own: the counting allocator, which only a `std` build of these
//! tests installs, and the visit ledger's `spent`, which only a test build has.

use smear_parser::{
  graphql::{
    GraphQL,
    ast::TypeSystemDocument,
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, type_system_document},
  },
  lexer::tokora::{Parse as _, Parser},
};
use smear_schema::Schema;

use super::{Read, literal_type};
use crate::{
  collect::{Visits, byte_units},
  response::allocations,
};

/// A type name longer than thirty-two bytes, so that a `String` grown a byte at a time would
/// reallocate on the way to holding it.
const NAME: &str = "IntrospectionNameLongerThanThirtyTwo";

/// The schema the cases look [`NAME`] up in.
const SDL: &str = "type Query { a: Int } type IntrospectionNameLongerThanThirtyTwo { a: Int }";

/// [`SDL`], built.
fn schema() -> Schema {
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
  .expect("the SDL parses");
  Schema::build(&document).expect("the SDL is a schema")
}

/// [`NAME`] as an inline string, delimiters included, that the walk hands out whole.
fn plain() -> String {
  format!("\"{NAME}\"")
}

/// [`NAME`] as an inline string whose every character is a fixed-width escape, so that the walk
/// hands its value out a character, and a piece, at a time.
fn escaped() -> String {
  let mut spelling = String::from("\"");
  for ch in NAME.chars() {
    // `\u{5c}` is the backslash, so this writes the escape the walk reads.
    spelling.push_str(&format!("\u{5c}u{:04X}", u32::from(ch)));
  }
  spelling.push('"');
  spelling
}

/// A name its escapes split is assembled in one allocation, reserved at the length the first walk
/// added up; a name the walk hands out whole is looked up in place, with none.
///
/// [`NAME`] is past thirty-two bytes and its escaped spelling hands it out a byte at a time, so a
/// `String` that grew as the pieces arrived would reallocate on the way and read as more than one.
#[test]
fn an_escaped_name_is_assembled_in_one_allocation() {
  let schema = schema();
  let declared = schema.type_by_name(NAME.as_bytes()).map(|(id, _)| id);
  assert!(declared.is_some(), "the fixture declares `{NAME}`");

  // A spelling, and the allocations reading it may make.
  for (spelling, allowed) in [(plain(), 0), (escaped(), 1)] {
    let mut visits = Visits::new(u32::MAX);
    let before = allocations();
    let read = literal_type(&schema, spelling.as_bytes(), &mut visits);
    let allocated = allocations() - before;
    let Read::Named(found) = read else {
      panic!("`{spelling}` is a name, and the ledger has room to read it")
    };
    assert_eq!(found, declared, "`{spelling}` names the type");
    assert_eq!(
      allocated, allowed,
      "reading `{spelling}` allocated {allocated} times"
    );
  }
}

/// A name is charged one pass over its spelling per walk: one for a name the walk hands out whole,
/// whose lookup rides on that walk, and three for a name it assembles — the walk, the copy, and
/// the lookup of the copy.
#[test]
fn a_name_is_charged_a_pass_per_walk() {
  let schema = schema();

  // A spelling, and the passes over it that reading it is charged.
  for (spelling, passes) in [(plain(), 1), (escaped(), 3)] {
    let mut visits = Visits::new(u32::MAX);
    let read = literal_type(&schema, spelling.as_bytes(), &mut visits);
    assert!(
      matches!(read, Read::Named(Some(_))),
      "`{spelling}` names the type"
    );
    assert_eq!(
      visits.spent(),
      byte_units(spelling.len()) * passes,
      "`{spelling}`"
    );
  }
}
