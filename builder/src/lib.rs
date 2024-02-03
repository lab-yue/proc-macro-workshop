use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, DeriveInput, Expr,
    ExprLit, GenericArgument, Ident, Lit, Meta, MetaNameValue, Path, PathArguments, PathSegment,
    Type, TypePath,
};

fn wrapped_ty_name<'t>(ty: &'t Type) -> Option<(String, &'t Type)> {
    match ty {
        Type::Path(TypePath {
            qself: None,
            path: Path { segments, .. },
            ..
        }) => match segments.first() {
            Some(PathSegment {
                ident,
                arguments:
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
                ..
            }) => match args.first() {
                Some(GenericArgument::Type(ty)) => Some((ident.to_string(), ty)),
                _ => None,
            },
            _ => None,
        },
        _ => None,
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
                let Some(ident) = f.ident.as_ref() else {
                    continue;
                };
                let ty = &f.ty;

                let each = match f
                    .attrs
                    .iter()
                    .find_map(|a| {
                        if a.path().is_ident("builder") {
                            let meta = a.parse_args::<Meta>().expect("invalid arg");
                            match meta {
                                Meta::NameValue(MetaNameValue {
                                    value:
                                        Expr::Lit(ExprLit {
                                            lit: Lit::Str(s), ..
                                        }),
                                    path,
                                    ..
                                }) if path.is_ident("each") => {
                                    return Some(Ok(s.parse::<Ident>().expect("should be ident")));
                                }
                                _ => {
                                    return Some(Err(syn::Error::new(
                                        a.meta.span(),
                                        r#"expected `builder(each = "...")`"#,
                                    )
                                    .to_compile_error()))
                                }
                            }
                        }
                        None
                    })
                    .transpose()
                {
                    Ok(inner) => inner,
                    Err(err) => return err.into(),
                };

                match wrapped_ty_name(ty) {
                    Some((name, inner_ty)) if name == "Option" => {
                        option_fields.push(quote!( #ident: #ty));
                        none_fields.push(quote!( #ident: None));
                        setters.push(quote!(
                            fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                                self.#ident = Some(#ident);
                                self
                            }
                        ));

                        build_checks.push(quote!(
                            #ident: self.#ident.to_owned()
                        ));
                    }
                    Some((name, inner_ty)) if name == "Vec" && each.is_some() => {
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
                        option_fields.push(quote!( #ident: Option<#ty>));
                        none_fields.push(quote!( #ident: None));
                        setters.push(quote!(
                            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                                self.#ident = Some(#ident);
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
                    pub fn build(&mut self) -> Result<Command, Box<dyn std::error::Error>> {
                        Ok(#input_ident {
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
