// use darling::FromMeta;
// use indexmap::IndexSet;

// use crate::{aliases::Aliases, long::Long, short::Short};

// #[derive(Debug, Default)]
// pub(crate) struct Name {
//   pub(crate) short: Short,
//   pub(crate) long: Long,
//   pub(crate) aliases: Aliases,
// }

// impl FromMeta for Name {
//   fn from_meta(item: &syn::Meta) -> darling::Result<Self> {
//     Ok(Name {
//       short: darling::FromMeta::from_meta(item)?,
//       long: darling::FromMeta::from_meta(item)?,
//       aliases: darling::FromMeta::from_meta(item)?,
//     })
//   }
// }

// impl Name {
//   pub(crate) fn validate(&self, anchor: String) -> syn::Result<()> {
//     let name = anchor.to_ascii_lowercase();
//     self.short.validate(&name)?;
//     self.long.validate(&name)?;
//     Ok(())
//   }

//   pub(crate) fn long(&self, anchor: String) -> String {
//     self.long.0.clone().unwrap_or(anchor.to_ascii_lowercase())
//   }

//   pub(crate) fn short(&self, anchor: String) -> Option<char> {
//     self
//       .short
//       .0
//       .as_ref()
//       .map(|s| s.unwrap_or_else(|| anchor.chars().next().unwrap().to_ascii_lowercase()))
//   }

//   pub(crate) fn possible_names(&self, anchor: String) -> Vec<String> {
//     let mut suggestions = IndexSet::new();

//     suggestions.insert(self.long(anchor.clone()));

//     if let Some(short) = self.short(anchor.clone()) {
//       suggestions.insert(short.to_string());
//     }

//     suggestions.extend(self.aliases.names.iter().cloned());
//     suggestions.into_iter().collect()
//   }
// }

