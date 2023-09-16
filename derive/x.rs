mod directive {
    use syn::Type;
    use darling::{FromMeta, Error};
    pub(crate) struct Directives {
        #[darling(multiple)]
        directives: Vec<Type>,
    }
    impl ::darling::FromMeta for Directives {
        fn from_list(__items: &[::darling::export::NestedMeta]) -> ::darling::Result<Self> {
            let mut directives: Vec<Type> = ::darling::export::Default::default();
            let mut __errors = ::darling::Error::accumulator();
            for __item in __items {
                match *__item {
                    ::darling::export::NestedMeta::Meta(ref __inner) => {
                        let __name = ::darling::util::path_to_string(__inner.path());
                        match __name.as_str() {
                            "directives" => {
                                let __len = directives.len();
                                if let ::darling::export::Some(__val) = __errors.handle(
                                    ::darling::FromMeta::from_meta(__inner).map_err(|e| {
                                        e.with_span(&__inner).at(&{
                                            let res = ::alloc::fmt::format(format_args!(
                                                "{0}[{1}]",
                                                "directives", __len
                                            ));
                                            res
                                        })
                                    }),
                                ) {
                                    directives.push(__val)
                                }
                            }
                            __other => {
                                __errors.push(
                                    ::darling::Error::unknown_field_with_alts(
                                        __other,
                                        &["directives"],
                                    )
                                    .with_span(__inner),
                                );
                            }
                        }
                    }
                    ::darling::export::NestedMeta::Lit(ref __inner) => {
                        __errors.push(
                            ::darling::Error::unsupported_format("literal").with_span(__inner),
                        );
                    }
                }
            }
            __errors.finish()?;
            ::darling::export::Ok(Self {
                directives: directives,
            })
        }
    }
}
