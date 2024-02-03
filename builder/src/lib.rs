use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, Ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let builder_ident = Ident::new(&format!("{}Builder", &input.ident), input.span());
    let input_ident = input.ident;

    let gen = match &input.data {
        syn::Data::Struct(s) => {
            let mut option_fields = vec![];
            let mut none_fields = vec![];

            s.fields.iter().for_each(|f| {
                let Some(ident) = f.ident.as_ref() else {
                    return;
                };
                let ty = &f.ty;
                option_fields.push(quote!( #ident: Option<#ty>));
                none_fields.push(quote!( #ident: None));
            });

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
