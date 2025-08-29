
macro_rules! define_module {
  ($($filename:ident),+$(,)?) => {
    $(
      paste::paste! {
        #[path = "err/" $filename ".rs"]
        mod $filename;
      }
    )*
  };
}

define_module!(
  err_0001_directive_definition_missing_location,
  err_0062_kitchen_sink_ccn,
);
