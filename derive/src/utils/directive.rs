use darling::FromMeta;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
  parse::{Parse, Parser},
  Ident, Type,
};

use super::{DefaultAttribute, Optional, SafeIdent, Ty};

#[derive(FromMeta)]
pub(crate) struct Directive {
  #[darling(rename = "type")]
  pub(crate) ty: Ty,
  pub(crate) default: Option<DefaultAttribute>,
  #[darling(default)]
  pub(crate) optional: Optional,
}

impl Directive {
  pub(crate) fn field_name(&self) -> SafeIdent {
    self.ty.to_rust_ident()
  }

  pub(crate) fn dirty_check_variable_name(&self) -> Ident {
    format_ident!("__dirty_check_{}", self.field_name().safe())
  }

  pub(crate) fn ty(&self) -> Type {
    match self.optional.is_optional() {
      false => self.ty.ty.clone(),
      true => {
        let ty = &self.ty;
        Type::parse
          .parse2(quote!(::core::option::Option<#ty>))
          .unwrap()
      }
    }
  }

  pub(crate) fn parser_ty(&self) -> Type {
    match (self.default.is_some(), self.optional.is_optional()) {
      (true, false) => self.ty.ty.clone(),
      _ => {
        let ty = &self.ty;
        Type::parse
          .parse2(quote!(::core::option::Option<#ty>))
          .unwrap()
      }
    }
  }

  pub(crate) fn parser_field_default(&self) -> TokenStream {
    let field_name = self.field_name();
    match (&self.default, self.optional.is_optional()) {
      (None, true) => quote!(#field_name: ::core::option::Option::None),
      (None, false) => quote!(#field_name: ::core::option::Option::None),
      (Some(d), true) => quote!(#field_name: ::core::option::Option::Some(#d)),
      (Some(d), false) => quote!(#field_name: #d),
    }
  }

  pub(crate) fn parser_setter(&self) -> TokenStream {
    let field_name = self.field_name();
    match (self.default.is_some(), self.optional.is_optional()) {
      (true, false) => quote!(parser.#field_name = val;),
      _ => quote!(parser.#field_name = ::core::option::Option::Some(val);),
    }
  }

  pub(crate) fn null_check(&self) -> TokenStream {
    if !self.optional.is_optional() && self.default.is_none() {
      let field_name = self.field_name();
      let ty = &self.ty;
      quote! {
        if parser.#field_name.is_none() {
          missing_directives.push(<#ty as ::smear::__exports::Diagnosticable>::descriptor());
        }
      }
    } else {
      quote!()
    }
  }

  pub(crate) fn convert(&self) -> TokenStream {
    let field_name = self.field_name();
    match (self.default.is_some(), self.optional.is_optional()) {
      (false, false) => quote!(#field_name: parser.#field_name.unwrap()),
      _ => quote!(#field_name: parser.#field_name),
    }
  }
}

#[derive(Default)]
pub(crate) struct ProcessedDirectives {
  pub(crate) fields: Vec<TokenStream>,
  pub(crate) dirty_check_variable_names: Vec<Ident>,
  pub(crate) null_checks: Vec<TokenStream>,
  pub(crate) converts: Vec<TokenStream>,
  pub(crate) parser_fields: Vec<TokenStream>,
  pub(crate) parser_field_default: Vec<TokenStream>,
  pub(crate) directive_hanlders: Vec<TokenStream>,
  pub(crate) required_directives_descriptors: Vec<TokenStream>,
  pub(crate) optional_directives_descriptors: Vec<TokenStream>,
  pub(crate) available_directives_descriptors: Vec<TokenStream>,
}

impl ProcessedDirectives {
  pub(crate) fn directive_handlers(&self, error_ty: impl ToTokens) -> TokenStream {
    let handlers = &self.directive_hanlders;
    quote! {
      for directive in directives.directives() {
        if let ::core::option::Option::Some(n) = directive.name() {
          match () {
            #(#handlers),*
            _ => {
              errors.push(::smear::__exports::#error_ty::unknown_directive(&directive));
            }
          }
        } else {
          errors.push(::smear::__exports::#error_ty::missing_directive_name(&directive));
        }
      }
    }
  }
}

#[derive(Clone)]
pub(crate) struct Directives<'a, I: 'a> {
  directives: I,
  _marker: core::marker::PhantomData<&'a ()>,
}

impl<'a, I> Directives<'a, I>
where
  I: Iterator<Item = &'a Directive> + 'a
{
  pub(crate) fn new(directives: I) -> Self {
    Self {
      directives,
      _marker: core::marker::PhantomData,
    }
  }

  pub(crate) fn processed(self, error_ty: impl ToTokens) -> ProcessedDirectives {
    let mut this = ProcessedDirectives::default();
    for d in self.directives {
      let ty = &d.ty;
      let name = d.field_name();
      let field_ty = d.ty();
      let parser_field_ty = d.parser_ty();
      let parser_setter = d.parser_setter();
      let dirty_check_variable = d.dirty_check_variable_name();
      this.converts.push(d.convert());
      this.fields.push(quote!(#name: #field_ty));
      this.parser_fields.push(quote!(#name: #parser_field_ty));
      this.parser_field_default.push(d.parser_field_default());
      this.null_checks.push(d.null_check());
      this.dirty_check_variable_names.push(dirty_check_variable.clone());

      this.directive_hanlders.push(quote! {
        () if <#ty as ::smear::__exports::Diagnosticable>::descriptor().contains_name(n.text().as_str()) => {
          let directive_descriptor = <#ty as ::smear::__exports::Diagnosticable>::descriptor();
          if #dirty_check_variable {
            errors.push(::smear::__exports::#error_ty::duplicated_directive(node, descriptor.name(), directive_descriptor.name()));
            continue;
          } else {
            #dirty_check_variable = true;
          }
          match <#ty as ::smear::__exports::Diagnosticable>::parse(directive) {
            ::core::result::Result::Ok(val) => {
              #parser_setter
            },
            ::core::result::Result::Err(e) => errors.push(e),
          }
        }
      });
      if d.optional.is_optional() {
        this.optional_directives_descriptors.push(quote!(<#ty as ::smear::__exports::Diagnosticable>::descriptor()));
      } else {
        this.required_directives_descriptors.push(quote!(<#ty as ::smear::__exports::Diagnosticable>::descriptor()));
      }
      this.available_directives_descriptors.push(quote!(<#ty as ::smear::__exports::Diagnosticable>::descriptor()));
    }

    this
  }
}

