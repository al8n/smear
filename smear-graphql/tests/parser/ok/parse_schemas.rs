use smear_graphql::parser::fast::{Document, ParseStr};

use std::fs;

#[test]
fn parse_schemas() {
  let mut current_dir = std::env::current_dir().expect("current directory should be existent");
  current_dir.push("tests");
  current_dir.push("fixtures");
  current_dir.push("schemas");

  let dir =
    fs::read_dir(current_dir).expect("should be able to read the fixtures/schemas directory");

  for entry in dir {
    let entry = entry.expect("should be able to read directory entry");
    let path = entry.path();
    if path.extension().and_then(|s| s.to_str()) == Some("graphql") {
      let content = fs::read_to_string(&path).expect("should be able to read file");
      let document = Document::<&str>::parse_str(&content).into_result();
      match document {
        Ok(doc) => {
          let _ = doc.definitions();
        }
        Err(e) => {
          println!("Source: {}", &content);
          panic!("Failed to parse {}: {:?}", path.display(), e);
        }
      }
    }
  }
}
