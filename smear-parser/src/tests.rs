use std::path::{Path, PathBuf};

/// PathBuf of test fixtures directory.
fn test_data_dir() -> PathBuf {
  project_root().join("smear-parser/test_data")
}

/// project root.
fn project_root() -> PathBuf {
  Path::new(
    &std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
  )
  .ancestors()
  .nth(1)
  .unwrap()
  .to_path_buf()
}
