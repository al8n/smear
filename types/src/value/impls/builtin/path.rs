use std::path::PathBuf;

impl_diagnostic_and_encodable!(string(PathBuf::parse_path_buf,));
