

use std::fs;

#[test]
#[cfg(feature = "graphql")]
fn parse_executables() {
  use smear::parser::graphql::ast::{Document, ParseStr};

  let mut current_dir = std::env::current_dir().expect("current directory should be existent");
  current_dir.push("tests");
  current_dir.push("fixtures");
  current_dir.push("executables");

  let dir =
    fs::read_dir(current_dir).expect("should be able to read the fixtures/executables directory");

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
          panic!("Failed to parse {}: {:?}", path.display(), e);
        }
      }
    }
  }
}

#[cfg(feature = "graphqlx")]
#[test]
fn parse_graphqlx_executables() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let mut current_dir = std::env::current_dir().expect("current directory should be existent");
  current_dir.push("tests");
  current_dir.push("fixtures");
  current_dir.push("executables");

  let dir =
    fs::read_dir(current_dir).expect("should be able to read the fixtures/executables directory");

  for entry in dir {
    let entry = entry.expect("should be able to read directory entry");
    let path = entry.path();
    if path.extension().and_then(|s| s.to_str()) == Some("graphql") {
      println!("Parsing {}", path.display());
      let content = fs::read_to_string(&path).expect("should be able to read file");
      let document = Document::<&str>::parse_str(&content).into_result();
      match document {
        Ok(doc) => {
          println!("Success! {}", path.display());
          let _ = doc.definitions();
        }
        Err(e) => {
          panic!("Failed to parse {}: {:?}", path.display(), e);
        }
      }
    }
  }
}

