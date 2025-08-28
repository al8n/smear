use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
"Just one interface"
type One implements A { field: Int! }

"Several interfaces"
type Two implements A & B & C { field: Int! }

"&-prefixed"
type Three implements
  & A
  & B
  & C
{ field: Int! }
"###;

#[test]
fn implements_list() {}
