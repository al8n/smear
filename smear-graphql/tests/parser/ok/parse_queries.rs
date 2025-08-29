use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

use std::fs;

#[test]
fn parse_queries() {
  let mut current_dir = std::env::current_dir().expect("current directory should be existent");
  current_dir.push("tests");
  current_dir.push("fixtures");
  current_dir.push("queries");

  let dir =
    fs::read_dir(current_dir).expect("should be able to read the fixtures/queries directory");

  for entry in dir {
    let entry = entry.expect("should be able to read directory entry");
    let path = entry.path();
    if path.extension().and_then(|s| s.to_str()) == Some("graphql") {
      let content = fs::read_to_string(&path).expect("should be able to read file");
      let document = Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
        extra::Err<Rich<char>>,
      >(&content);
      match document {
        Ok(doc) => {
          let _ = doc.content();
        }
        Err(e) => {
          panic!("Failed to parse {}: {:?}", path.display(), e);
        }
      }
    }
  }
}
