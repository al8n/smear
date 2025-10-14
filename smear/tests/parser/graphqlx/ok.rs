macro_rules! define_module {
  ($($filename:ident),+$(,)?) => {
    $(
      paste::paste! {
        #[path = "ok/" $filename ".rs"]
        mod $filename;
      }
    )*
  };
}

define_module!(
  ok_0001_import_named,
  ok_0002_import_wildcard,
  ok_0003_import_with_alias,
  ok_0004_generics_simple,
  ok_0005_generics_multiple_params,
  ok_0006_where_clause_simple,
  ok_0007_where_clause_multiple_bounds,
  ok_0008_map_value_simple,
  ok_0009_map_value_nested,
  ok_0010_generics_with_default,
  ok_0011_interface_with_generics,
  ok_0012_path_type,
  ok_0013_complex_import,
  ok_0014_generics_nested,
  ok_0015_extend_with_generics,
  ok_0016_operation_with_generics,
  ok_0017_union_with_path_types,
  ok_0018_union_with_path_types_and_generics,
  ok_0019_input_with_map_default,
  ok_0020_where_multiple_predicates,
  ok_0021_fat_arrow_in_map,
  ok_0022_complex_fragments,
);
