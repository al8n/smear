#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use apollo_encoder::DirectiveDefinition;
#[smear(
    short = 'f',
    aliases(bar),
    on(field_definition, object),
    description = "A foo directive",
    deprecated(version = "0.1.0")
)]
struct Foo {
    #[smear(aliases("rwd"), deprecated, default)]
    required_with_default: u64,
    # [smear (short , aliases (ro) , default = { 16 })]
    required_only: u32,
    #[smear(short(o), optional, default(default_optional_with_default))]
    optional_with_default: f64,
    #[smear(short = 'y', optional)]
    optional_only: String,
}
#[automatically_derived]
struct FooDirective {
    required_with_default: u64,
    required_only: u32,
    optional_with_default: ::core::option::Option<f64>,
    optional_only: ::core::option::Option<String>,
}
#[automatically_derived]
impl ::smear::__exports::Diagnosticable for FooDirective {
    type Error = ::smear::__exports::error::DirectiveError;
    type Node = ::smear::__exports::apollo_parser::ast::Directive;
    type Descriptor = ::smear::__exports::directive::DirectiveDescriptor;
    fn parse(directive: &Self::Node) -> ::core::result::Result<Self, Self::Error>
    where
        Self: Sized,
    {
        struct FooDirectiveParser {
            required_with_default: u64,
            required_only: u32,
            optional_with_default: ::core::option::Option<f64>,
            optional_only: ::core::option::Option<String>,
        }
        impl ::core::default::Default for FooDirectiveParser {
            fn default() -> Self {
                Self {
                    required_with_default: ::core::default::Default::default(),
                    required_only: {
                        {
                            16
                        }
                    },
                    optional_with_default: ::core::option::Option::Some(
                        ::core::default::Default::default(),
                    ),
                    optional_only: ::core::option::Option::None,
                }
            }
        }
        impl ::core::convert::From<FooDirectiveParser> for FooDirective {
            fn from(parser: FooDirectiveParser) -> Self {
                Self {
                    required_with_default: parser.required_with_default,
                    required_only: parser.required_only,
                    optional_with_default: parser.optional_with_default,
                    optional_only: parser.optional_only,
                }
            }
        }
        let directive_name = directive
            .name()
            .map(|n| n.text().to_string())
            .unwrap_or_default();
        if let ::core::option::Option::Some(args) = directive.arguments() {
            let mut parser = FooDirectiveParser::default();
            let mut errors = ::std::vec::Vec::new();
            let mut missing_arguments = ::std::vec::Vec::new();
            let mut required_with_default_dirty = false;
            let mut required_only_dirty = false;
            let mut optional_with_default_dirty = false;
            let mut optional_only_dirty = false;
            for arg in args.arguments() {
                match (arg.name(), arg.value()) {
                    (::core::option::Option::None, ::core::option::Option::None) => {
                        errors.push(::smear::__exports::error::DirectiveError::invalid_argument(
                            &arg,
                        ));
                    }
                    (::core::option::Option::None, ::core::option::Option::Some(_)) => {
                        errors.push(
                            ::smear::__exports::error::DirectiveError::missing_argument_name(&arg),
                        );
                    }
                    (::core::option::Option::Some(name), ::core::option::Option::None) => {
                        let name_text = name.text();
                        let name_str = name_text.as_str().trim();
                        match name_str {
                            "required_with_default" | "rwd" => {
                                if !required_with_default_dirty {
                                    required_with_default_dirty = true;
                                } else {
                                    errors . push (:: smear :: __exports :: error :: DirectiveError :: duplicated_argument (& arg , directive_name . clone () , name_str)) ;
                                }
                            }
                            "required_only" | "r" | "ro" => {
                                if !required_only_dirty {
                                    required_only_dirty = true;
                                } else {
                                    errors . push (:: smear :: __exports :: error :: DirectiveError :: duplicated_argument (& arg , directive_name . clone () , name_str)) ;
                                }
                            }
                            "optional_with_default" | "o" => {
                                if !optional_with_default_dirty {
                                    optional_with_default_dirty = true;
                                } else {
                                    errors . push (:: smear :: __exports :: error :: DirectiveError :: duplicated_argument (& arg , directive_name . clone () , name_str)) ;
                                }
                            }
                            "optional_only" | "y" => {
                                if !optional_only_dirty {
                                    optional_only_dirty = true;
                                } else {
                                    errors . push (:: smear :: __exports :: error :: DirectiveError :: duplicated_argument (& arg , directive_name . clone () , name_str)) ;
                                }
                            }
                            name => {
                                errors.push(
                                    ::smear::__exports::error::DirectiveError::unknown_argument(
                                        &arg,
                                        directive_name.clone(),
                                        name_str,
                                        &[
                                            "required_with_default",
                                            "rwd",
                                            "required_only",
                                            "r",
                                            "ro",
                                            "optional_with_default",
                                            "o",
                                            "optional_only",
                                            "y",
                                        ],
                                    ),
                                );
                            }
                        }
                    }
                    (::core::option::Option::Some(name), ::core::option::Option::Some(val)) => {
                        let name_text = name.text();
                        let name_str = name_text.as_str().trim();
                        match name_str {
                            "required_with_default" | "rwd" => {
                                if required_with_default_dirty {
                                    errors . push (:: smear :: __exports :: error :: DirectiveError :: duplicated_argument (& arg , directive_name . clone () , name_str)) ;
                                    continue;
                                }
                                required_with_default_dirty = true;
                                match < u64 as :: smear :: __exports :: value :: Parser > :: parse_value_nullable (& val) { :: core :: result :: Result :: Ok (parsed) => { if let :: core :: option :: Option :: Some (parsed) = parsed { parser . required_with_default = parsed ; } } :: core :: result :: Result :: Err (err) => { errors . push (:: smear :: __exports :: error :: DirectiveError :: invalid_argument_value (& arg , err)) ; continue ; } }
                            }
                            "required_only" | "r" | "ro" => {
                                if required_only_dirty {
                                    errors . push (:: smear :: __exports :: error :: DirectiveError :: duplicated_argument (& arg , directive_name . clone () , name_str)) ;
                                    continue;
                                }
                                required_only_dirty = true;
                                match < u32 as :: smear :: __exports :: value :: Parser > :: parse_value_nullable (& val) { :: core :: result :: Result :: Ok (parsed) => { if let :: core :: option :: Option :: Some (parsed) = parsed { parser . required_only = parsed ; } } :: core :: result :: Result :: Err (err) => { errors . push (:: smear :: __exports :: error :: DirectiveError :: invalid_argument_value (& arg , err)) ; continue ; } }
                            }
                            "optional_with_default" | "o" => {
                                if optional_with_default_dirty {
                                    errors . push (:: smear :: __exports :: error :: DirectiveError :: duplicated_argument (& arg , directive_name . clone () , name_str)) ;
                                    continue;
                                }
                                optional_with_default_dirty = true;
                                match < f64 as :: smear :: __exports :: value :: Parser > :: parse_value_nullable (& val) { :: core :: result :: Result :: Ok (parsed) => { parser . optional_with_default = parsed ; } :: core :: result :: Result :: Err (err) => { errors . push (:: smear :: __exports :: error :: DirectiveError :: invalid_argument_value (& arg , err)) ; continue ; } }
                            }
                            "optional_only" | "y" => {
                                if optional_only_dirty {
                                    errors . push (:: smear :: __exports :: error :: DirectiveError :: duplicated_argument (& arg , directive_name . clone () , name_str)) ;
                                    continue;
                                }
                                optional_only_dirty = true;
                                match < String as :: smear :: __exports :: value :: Parser > :: parse_value_nullable (& val) { :: core :: result :: Result :: Ok (parsed) => { parser . optional_only = parsed ; } :: core :: result :: Result :: Err (err) => { errors . push (:: smear :: __exports :: error :: DirectiveError :: invalid_argument_value (& arg , err)) ; continue ; } }
                            }
                            name => {
                                errors.push(
                                    ::smear::__exports::error::DirectiveError::unknown_argument(
                                        &arg,
                                        directive_name.clone(),
                                        name,
                                        &[
                                            "required_with_default",
                                            "rwd",
                                            "required_only",
                                            "r",
                                            "ro",
                                            "optional_with_default",
                                            "o",
                                            "optional_only",
                                            "y",
                                        ],
                                    ),
                                );
                            }
                        }
                    }
                }
            }
            if !missing_arguments.is_empty() {
                errors.push(
                    ::smear::__exports::error::DirectiveError::missing_arguments(
                        directive,
                        directive_name,
                        missing_arguments.into_iter(),
                    ),
                );
            }
            if !errors.is_empty() {
                return ::core::result::Result::Err(
                    ::smear::__exports::error::DirectiveError::multiple(directive, errors),
                );
            }
            ::core::result::Result::Ok(::core::convert::From::from(parser))
        } else {
            ::core::result::Result::Err(
                ::smear::__exports::error::DirectiveError::missing_arguments(
                    directive,
                    directive_name,
                    ["required_with_default", "required_only"].into_iter(),
                ),
            )
        }
    }
    fn descriptor() -> &'static Self::Descriptor {
        use ::smear::__exports::once_cell::sync::Lazy;
        static AVAILABLE_ARGUMENTS: Lazy<
            [::smear::__exports::directive::ArgumentDescriptor; 4usize],
        > = Lazy::new(|| {
            [:: smear :: __exports :: directive :: ArgumentDescriptor { name : "required_with_default" , short : :: core :: option :: Option :: None , aliases : & ["rwd"] , available_names : & ["required_with_default" , "rwd"] , description : :: core :: option :: Option :: None , deprecated : :: core :: option :: Option :: Some (:: smear :: __exports :: Deprecated :: new (:: core :: option :: Option :: None , :: core :: option :: Option :: None , :: core :: option :: Option :: None)) , value_descriptor : < u64 as :: smear :: __exports :: Diagnosticable > :: descriptor () , } , :: smear :: __exports :: directive :: ArgumentDescriptor { name : "required_only" , short : :: core :: option :: Option :: Some ('r') , aliases : & ["ro"] , available_names : & ["required_only" , "r" , "ro"] , description : :: core :: option :: Option :: None , deprecated : :: core :: option :: Option :: None , value_descriptor : < u32 as :: smear :: __exports :: Diagnosticable > :: descriptor () , } , :: smear :: __exports :: directive :: ArgumentDescriptor { name : "optional_with_default" , short : :: core :: option :: Option :: Some ('o') , aliases : & [] , available_names : & ["optional_with_default" , "o"] , description : :: core :: option :: Option :: None , deprecated : :: core :: option :: Option :: None , value_descriptor : < :: core :: option :: Option < f64 > as :: smear :: __exports :: Diagnosticable > :: descriptor () , } , :: smear :: __exports :: directive :: ArgumentDescriptor { name : "optional_only" , short : :: core :: option :: Option :: Some ('y') , aliases : & [] , available_names : & ["optional_only" , "y"] , description : :: core :: option :: Option :: None , deprecated : :: core :: option :: Option :: None , value_descriptor : < :: core :: option :: Option < String > as :: smear :: __exports :: Diagnosticable > :: descriptor () , }]
        });
        static REQUIRED_ARGUMENTS: Lazy<
            [::smear::__exports::directive::ArgumentDescriptor; 2usize],
        > = Lazy::new(|| {
            [
                ::smear::__exports::directive::ArgumentDescriptor {
                    name: "required_with_default",
                    short: ::core::option::Option::None,
                    aliases: &["rwd"],
                    available_names: &["required_with_default", "rwd"],
                    description: ::core::option::Option::None,
                    deprecated: ::core::option::Option::Some(::smear::__exports::Deprecated::new(
                        ::core::option::Option::None,
                        ::core::option::Option::None,
                        ::core::option::Option::None,
                    )),
                    value_descriptor: <u64 as ::smear::__exports::Diagnosticable>::descriptor(),
                },
                ::smear::__exports::directive::ArgumentDescriptor {
                    name: "required_only",
                    short: ::core::option::Option::Some('r'),
                    aliases: &["ro"],
                    available_names: &["required_only", "r", "ro"],
                    description: ::core::option::Option::None,
                    deprecated: ::core::option::Option::None,
                    value_descriptor: <u32 as ::smear::__exports::Diagnosticable>::descriptor(),
                },
            ]
        });
        static OPTIONAL_ARGUMENTS: Lazy<
            [::smear::__exports::directive::ArgumentDescriptor; 2usize],
        > = Lazy::new(|| {
            [:: smear :: __exports :: directive :: ArgumentDescriptor { name : "optional_with_default" , short : :: core :: option :: Option :: Some ('o') , aliases : & [] , available_names : & ["optional_with_default" , "o"] , description : :: core :: option :: Option :: None , deprecated : :: core :: option :: Option :: None , value_descriptor : < :: core :: option :: Option < f64 > as :: smear :: __exports :: Diagnosticable > :: descriptor () , } , :: smear :: __exports :: directive :: ArgumentDescriptor { name : "optional_only" , short : :: core :: option :: Option :: Some ('y') , aliases : & [] , available_names : & ["optional_only" , "y"] , description : :: core :: option :: Option :: None , deprecated : :: core :: option :: Option :: None , value_descriptor : < :: core :: option :: Option < String > as :: smear :: __exports :: Diagnosticable > :: descriptor () , }]
        });
        static DESCRIPTOR: ::std::sync::OnceLock<
            ::smear::__exports::directive::DirectiveDescriptor,
        > = ::std::sync::OnceLock::new();
        DESCRIPTOR.get_or_init(|| ::smear::__exports::directive::DirectiveDescriptor {
            name: "foo",
            short: ::core::option::Option::Some('f'),
            aliases: &["bar"],
            available_names: &["foo", "f", "bar"],
            locations: &[
                ::smear::__exports::directive::DirectiveLocation::FieldDefinition,
                ::smear::__exports::directive::DirectiveLocation::Object,
            ],
            description: ::core::option::Option::Some("A foo directive"),
            deprecated: ::core::option::Option::Some(::smear::__exports::Deprecated::new(
                ::core::option::Option::Some("0.1.0"),
                ::core::option::Option::None,
                ::core::option::Option::None,
            )),
            arguments: ::smear::__exports::directive::ArgumentsDescriptor {
                available_arguments: &*AVAILABLE_ARGUMENTS,
                required_arguments: &*REQUIRED_ARGUMENTS,
                optional_arguments: &*OPTIONAL_ARGUMENTS,
            },
        })
    }
}
impl ::smear::__exports::Encodable for FooDirective {
    type SDL = ::smear::__exports::apollo_encoder::DirectiveDefinition;
    fn encode() -> Self::SDL {
        ::core::convert::From::from(<Self as ::smear::__exports::Diagnosticable>::descriptor())
    }
}
fn default_optional_with_default() -> f64 {
    1.0
}
fn main() {
    use smear_types::Encodable;
    let def = FooDirective::encode();
    {
        ::std::io::_print(format_args!("{0}\n", def));
    };
}
