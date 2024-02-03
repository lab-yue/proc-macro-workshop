use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, DeriveInput, Expr,
    ExprLit, GenericArgument, Ident, Lit, Meta, MetaNameValue, Path, PathArguments, PathSegment,
    Type, TypePath,
};

fn wrapped_ty_name<'t>(ty: &'t Type) -> std::option::Option<(String, &'t Type)> {
    match ty {
        Type::Path(TypePath {
            qself: std::option::Option::None,
            path: Path { segments, .. },
            ..
        }) => match segments.first() {
            std::option::Option::Some(PathSegment {
                ident,
                arguments:
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
                ..
            }) => match args.first() {
                std::option::Option::Some(GenericArgument::Type(ty)) => {
                    std::option::Option::Some((ident.to_string(), ty))
                }
                _ => std::option::Option::None,
            },
            _ => std::option::Option::None,
        },
        _ => std::option::Option::None,
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let input_span = input.span();
    let builder_ident = Ident::new(&format!("{}Builder", &input.ident), input_span);
    let input_ident = input.ident;

    let gen = match &input.data {
        syn::Data::Struct(s) => {
            let mut option_fields = vec![];
            let mut none_fields = vec![];
            let mut setters = vec![];
            let mut build_checks = vec![];

            for f in &s.fields {
                let std::option::Option::Some(ident) = f.ident.as_ref() else {
                    continue;
                };
                let ty = &f.ty;

                let each = match f
                    .attrs
                    .iter()
                    .find_map(|a| {
                        if a.path().is_ident("builder") {
                            if let std::result::Result::Ok(meta) = a.parse_args::<Meta>() {
                                match meta {
                                    Meta::NameValue(MetaNameValue {
                                        value:
                                            Expr::Lit(ExprLit {
                                                lit: Lit::Str(s), ..
                                            }),
                                        path,
                                        ..
                                    }) if path.is_ident("each") => {
                                        if let std::result::Result::Ok(ident) = s.parse::<Ident>() {
                                            return std::option::Option::Some(
                                                std::result::Result::Ok(ident),
                                            );
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        };

                        std::option::Option::Some(std::result::Result::Err(
                            syn::Error::new(a.meta.span(), r#"expected `builder(each = "...")`"#)
                                .to_compile_error(),
                        ))
                    })
                    .transpose()
                {
                    std::result::Result::Ok(inner) => inner,
                    std::result::Result::Err(err) => return err.into(),
                };

                match wrapped_ty_name(ty) {
                    std::option::Option::Some((name, inner_ty)) if name == "Option" => {
                        option_fields.push(quote!( #ident: #ty));
                        none_fields.push(quote!( #ident: std::option::Option::None));
                        setters.push(quote!(
                            fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                                self.#ident = std::option::Option::Some(#ident);
                                self
                            }
                        ));

                        build_checks.push(quote!(
                            #ident: self.#ident.to_owned()
                        ));
                    }
                    std::option::Option::Some((name, inner_ty))
                        if name == "Vec" && each.is_some() =>
                    {
                        let each_ident = each.as_ref().unwrap_or(ident);
                        option_fields.push(quote!( #ident: #ty));
                        none_fields.push(quote!( #ident: vec![]));
                        setters.push(quote!(
                            fn #each_ident(&mut self, #each_ident: #inner_ty) -> &mut Self {
                                self.#ident.push(#each_ident);
                                self
                            }
                        ));

                        build_checks.push(quote!(
                            #ident: self.#ident.to_owned()
                        ));
                    }
                    _ => {
                        option_fields.push(quote!( #ident: std::option::Option<#ty>));
                        none_fields.push(quote!( #ident: std::option::Option::None));
                        setters.push(quote!(
                            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                                self.#ident = std::option::Option::Some(#ident);
                                self
                            }
                        ));

                        let expect = format!("expect {}", ident.to_string());
                        build_checks.push(quote!(
                            #ident: self.#ident.take().ok_or(#expect)?
                        ));
                    }
                }
            }

            let _struct = quote!(
                pub struct #builder_ident {
                    #(#option_fields),*
                }
            );

            let _impl = quote!(
                impl #input_ident {
                    pub fn builder() -> #builder_ident {
                        #builder_ident {
                            #(#none_fields),*
                        }
                    }
                }
                impl #builder_ident {
                    pub fn build(&mut self) -> std::result::Result<
                        Command,
                        std::boxed::Box<dyn std::error::Error
                    >> {
                        std::result::Result::Ok(#input_ident {
                            #(#build_checks),*
                        })
                    }
                    #(#setters)*
                }
            );

            quote!(
                #_struct
                #_impl
            )
        }
        _ => quote!(),
    };

    gen.into()
}
