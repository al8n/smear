const ALL: &str = r###"
union SearchResult = Photo | Person

type Error {
    code: Int
}
"###;

use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

#[test]
fn union_type_definition_followed_by_object_definition() {}
