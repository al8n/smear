use darling::FromField;
use indexmap::IndexSet;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Ident, Visibility};

use crate::utils::{
  Aliases, DefaultAttribute, Deprecated, Description, Long, Optional, PathAttribute, RenameAll,
  Short,
};

use super::Helper;

#[derive(FromField)]
#[darling(attributes(smear))]
pub(super) struct Argument {
  ident: Option<Ident>,
  vis: Visibility,
  ty: syn::Type,
  #[darling(default)]
  description: Description,
  #[darling(default)]
  deprecated: Deprecated,
  #[darling(default)]
  attributes: crate::utils::Attributes,
  #[darling(default)]
  short: Short,
  #[darling(default)]
  long: Long,
  #[darling(default)]
  aliases: Aliases,
  optional: Option<Optional>,
  default: Option<DefaultAttribute>,
  #[darling(default)]
  validator: PathAttribute,
  #[darling(default)]
  parser: PathAttribute,
}

impl Argument {
  pub(super) fn ty(&self) -> &syn::Type {
    &self.ty
  }

  pub(super) fn vis(&self) -> &Visibility {
    &self.vis
  }

  pub(super) fn attributes(&self) -> &crate::utils::Attributes {
    &self.attributes
  }

  pub(super) fn optional(&self) -> bool {
    match self.optional {
      Some(optional) => optional.is_optional(),
      None => false,
    }
  }

  pub(super) fn default(&self) -> Option<&DefaultAttribute> {
    self.default.as_ref()
  }

  pub(super) fn parser(&self) -> &PathAttribute {
    &self.parser
  }

  pub(super) fn validator(&self) -> &PathAttribute {
    &self.validator
  }

  /// Returns the name of the field as it should appear in the SDL.
  pub(super) fn sdl_name(&self, rename_all: Option<RenameAll>) -> String {
    self.long.0.clone().unwrap_or_else(|| {
      let name = self.ident.as_ref().unwrap().to_string();
      rename_all
        .map(|rename_all| rename_all.apply(&name))
        .unwrap_or(name)
    })
  }

  pub(super) fn short(&self, rename_all: Option<RenameAll>) -> Option<char> {
    self.short.0.as_ref().map(|s| {
      s.unwrap_or_else(|| {
        self
          .sdl_name(rename_all)
          .chars()
          .next()
          .unwrap()
          .to_ascii_lowercase()
      })
    })
  }

  pub(super) fn possible_names(&self, rename_all: Option<RenameAll>) -> Vec<String> {
    let mut suggestions = IndexSet::new();

    suggestions.insert(self.sdl_name(rename_all));

    if let Some(short) = self.short(rename_all) {
      suggestions.insert(short.to_string());
    }

    suggestions.extend(self.aliases.names.iter().cloned());
    suggestions.into_iter().collect()
  }

  pub(super) fn to_tokens(
    &self,
    rename_all: Option<RenameAll>,
    helper: &mut Helper,
  ) -> syn::Result<()> {
    let Helper {
      struct_fields_definitions,
      parse_helper_struct_fields_definitions,
      parse_helper_struct_fields_default,
      missing_argument_value_handlers,
      argument_handlers,
      required_arguments_name,
      dirty_checks,
      dirty_definitions,
      converts,
      available_arguments,
      required_arguments,
      optional_arguments,
    } = helper;

    let field_name = self.sdl_name(rename_all);
    let field_name_ident = format_ident!("{field_name}");
    let dirty = format_ident!("{field_name_ident}_dirty");
    let field_vis = self.vis();
    let field_attrs = self.attributes();
    let short = match self.short(rename_all) {
      Some(ch) => quote!(::core::option::Option::Some(#ch)),
      None => quote!(::core::option::Option::None),
    };
    let aliases = &self.aliases.names;
    let field_possible_names = self.possible_names(rename_all);
    let field_parser = self.parser();
    let field_validator = self.validator();
    let optional = self.optional();
    let default_attr = self.default();

    let field_ty = if optional {
      let ty = self.ty();
      quote!(::core::option::Option<#ty>)
    } else {
      required_arguments_name.push(field_name.clone());
      let ty = self.ty();
      quote!(#ty)
    };
    let desc = self.description.to_tokens();
    let deprecated = &self.deprecated;
    let arg_descriptor = quote! {
      ::smear::directive::ArgumentDescriptor {
        name: #field_name,
        short: #short,
        aliases: &[#(#aliases),*],
        available_names: &[#(#field_possible_names),*],
        description: #desc,
        deprecated: #deprecated,
        value_descriptor: <#field_ty as ::smear::Diagnosticable>::descriptor(),
      }
    };
    available_arguments.push(arg_descriptor.clone());
    if optional {
      optional_arguments.push(arg_descriptor);
    } else {
      required_arguments.push(arg_descriptor);
    }

    let helper = match (optional, default_attr) {
      (true, None) => {
        CodegenHelper::optional_only(&field_name_ident, &field_ty, field_parser, field_validator)
      }
      (true, Some(default)) => CodegenHelper::optional_and_default(
        &field_name_ident,
        &field_ty,
        field_parser,
        field_validator,
        default,
      ),
      (false, None) => {
        CodegenHelper::required_only(&field_name_ident, &field_ty, field_parser, field_validator)
      }
      (false, Some(default)) => CodegenHelper::required_and_default(
        &field_name_ident,
        &field_ty,
        field_parser,
        field_validator,
        default,
      ),
    }?;

    struct_fields_definitions.push(quote!(
      #field_attrs
      #field_vis #field_name_ident: #field_ty,
    ));

    parse_helper_struct_fields_definitions.push(helper.parse_field_definition(&field_name_ident));
    parse_helper_struct_fields_default.push(helper.parse_field_default(&field_name_ident));

    let CodegenHelper {
      parser_handler,
      validator_handler,
      converter,
      ..
    } = helper;

    argument_handlers.push(quote! {
      #(#field_possible_names)|* => {
        if #dirty {
          errors.push(::smear::error::DirectiveError::duplicated_argument(&arg, directive_name.clone(), name_str));
          continue;
        }
        #dirty = true;

        #parser_handler

        #validator_handler
      },
    });
    missing_argument_value_handlers.push(quote! {
      #(#field_possible_names)|* => {
        if !#dirty {
          #dirty = true;
        } else {
          errors.push(::smear::error::DirectiveError::duplicated_argument(&arg, directive_name.clone(), name_str));
        }
      },
    });
    converts.push(converter);
    if !optional && default_attr.is_none() {
      dirty_checks.push(quote! {
        if !#dirty {
          missing_arguments.push(#field_name);
        }
      });
      dirty_definitions.push(quote! {
        let mut #dirty = false;
      });
    }
    Ok(())
  }
}

struct CodegenHelper {
  parser_field_ty: TokenStream,
  parser_field_default: TokenStream,
  validator_handler: TokenStream,
  parser_handler: TokenStream,
  converter: TokenStream,
}

impl CodegenHelper {
  fn parse_field_definition(&self, name: &Ident) -> TokenStream {
    let parser_field_ty = &self.parser_field_ty;
    quote! {
      #name: #parser_field_ty,
    }
  }

  fn parse_field_default(&self, name: &Ident) -> TokenStream {
    let parser_field_default = &self.parser_field_default;
    quote! {
      #name: #parser_field_default,
    }
  }

  fn required_and_default(
    field_name_ident: &Ident,
    field_ty: &TokenStream,
    field_parser: &PathAttribute,
    field_validator: &PathAttribute,
    default: &DefaultAttribute,
  ) -> syn::Result<Self> {
    let parser_fn = match field_parser.path()? {
      Some(p) => quote!(#p(&val)),
      None => quote!(<#field_ty as ::smear::value::Parser>::parse_value_nullable(&val)),
    };
    let parser = quote! {
      match #parser_fn {
        ::core::result::Result::Ok(parsed) => {
          parser.#field_name_ident = parsed;
        }
        ::core::result::Result::Err(err) => {
          errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
          continue;
        }
      }
    };
    let validator = match field_validator.path()? {
      Some(p) => quote! {
        if let ::core::result::Result::Err(e) = #p(&parser.#field_name_ident) {
          errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
        }
      },
      None => quote!(),
    };
    Ok(Self {
      parser_field_ty: quote!(#field_ty),
      parser_field_default: quote!(#default),
      parser_handler: parser,
      validator_handler: validator,
      converter: quote!(#field_name_ident: parser.#field_name_ident),
    })
  }

  fn required_only(
    field_name_ident: &Ident,
    field_ty: &TokenStream,
    field_parser: &PathAttribute,
    field_validator: &PathAttribute,
  ) -> syn::Result<Self> {
    let parser_fn = match field_parser.path()? {
      Some(p) => quote!(#p(&val)),
      None => quote!(<#field_ty as ::smear::value::Parser>::parse_value(&val)),
    };
    let parser = quote! {
      match #parser_fn {
        ::core::result::Result::Ok(parsed) => {
          parser.#field_name_ident = ::core::option::Option::Some(parsed);
        }
        ::core::result::Result::Err(err) => {
          errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
          continue;
        }
      }
    };
    let validator = match field_validator.path()? {
      Some(p) => quote! {
        if let ::core::option::Option::Some(ref parsed) = parser.#field_name_ident {
          if let ::core::result::Result::Err(e) = #p(parsed) {
            errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
          }
        }
      },
      None => quote! {},
    };

    Ok(Self {
      parser_field_ty: quote!(::core::option::Option<#field_ty>),
      parser_field_default: quote!(::core::option::Option::None),
      parser_handler: parser,
      validator_handler: validator,
      converter: quote!(#field_name_ident: parser.#field_name_ident.unwrap()),
    })
  }
  fn optional_and_default(
    field_name_ident: &Ident,
    field_ty: &TokenStream,
    field_parser: &PathAttribute,
    field_validator: &PathAttribute,
    default: &DefaultAttribute,
  ) -> syn::Result<Self> {
    let parser_fn = match field_parser.path()? {
      Some(p) => quote!(#p(&val)),
      None => quote!(<#field_ty as ::smear::value::Parser>::parse_value_nullable(&val)),
    };
    let parser = quote! {
      match #parser_fn {
        ::core::result::Result::Ok(parsed) => {
          parser.#field_name_ident = parsed;
        }
        ::core::result::Result::Err(err) => {
          errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
          continue;
        }
      }
    };
    let validator = match field_validator.path()? {
      Some(p) => quote! {
        if let ::core::option::Option::Some(ref parsed) = parser.#field_name_ident {
          if let ::core::result::Result::Err(e) = #p(parsed) {
            errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
          }
        }
      },
      None => quote!(),
    };

    Ok(Self {
      parser_field_ty: quote!(::core::option::Option<#field_ty>),
      parser_field_default: quote!(::core::option::Option::Some(#default)),
      parser_handler: parser,
      validator_handler: validator,
      converter: quote!(#field_name_ident: parser.#field_name_ident),
    })
  }

  fn optional_only(
    field_name_ident: &Ident,
    field_ty: &TokenStream,
    field_parser: &PathAttribute,
    field_validator: &PathAttribute,
  ) -> syn::Result<Self> {
    let parser_fn = match field_parser.path()? {
      Some(p) => quote!(#p(&val)),
      None => quote!(<#field_ty as ::smear::value::Parser>::parse_value_nullable(&val)),
    };
    let parser = quote! {
      match #parser_fn {
        ::core::result::Result::Ok(parsed) => {
          parser.#field_name_ident = parsed;
        }
        ::core::result::Result::Err(err) => {
          errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
          continue;
        }
      }
    };
    let validator = match field_validator.path()? {
      Some(p) => quote! {
        if let ::core::option::Option::Some(ref parsed) = parser.#field_name_ident {
          if let ::core::result::Result::Err(e) = #p(parsed) {
            errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
          }
        }
      },
      None => quote!(),
    };

    Ok(Self {
      parser_field_ty: quote!(::core::option::Option<#field_ty>),
      parser_field_default: quote!(::core::option::Option::None),
      parser_handler: parser,
      validator_handler: validator,
      converter: quote!(#field_name_ident: parser.#field_name_ident),
    })
  }
}
