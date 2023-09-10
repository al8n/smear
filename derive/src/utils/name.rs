use darling::FromMeta;
use heck::{ToLowerCamelCase, ToPascalCase, ToShoutySnakeCase, ToSnakeCase};

#[derive(Debug, Default, FromMeta, PartialEq, Eq, Clone, Copy)]
pub(crate) enum RenameAll {
  #[darling(rename = "lowercase")]
  Lowercase,

  #[darling(rename = "UPPERCASE")]
  Uppercase,

  #[darling(rename = "PascalCase")]
  PascalCase,

  #[darling(rename = "camelCase")]
  CamelCase,

  #[darling(rename = "snake_case")]
  #[default]
  SnakeCase,

  #[darling(rename = "SCREAMING_SNAKE_CASE")]
  ScreamingSnakeCase,
}

impl RenameAll {
  pub(crate) fn apply(&self, name: &str) -> String {
    match self {
      Self::Lowercase => name.to_lowercase(),
      Self::Uppercase => name.to_uppercase(),
      Self::PascalCase => name.to_pascal_case(),
      Self::CamelCase => name.to_lower_camel_case(),
      Self::SnakeCase => name.to_snake_case(),
      Self::ScreamingSnakeCase => name.to_shouty_snake_case(),
    }
  }
}
