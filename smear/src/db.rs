#[salsa::query_group(InputStorage)]
pub trait InputDatabase {
  #[salsa::input]
  fn input(&self) -> String;
}
