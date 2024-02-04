use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, DeriveInput, ExprLit, Lit, MetaNameValue};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident_lit = input.ident.to_string();
    let ident = input.ident;
    let bounds = input
        .generics
        .params
        .iter()
        .map(|ty| match ty {
            syn::GenericParam::Lifetime(l) => l.to_token_stream(),
            syn::GenericParam::Type(ty) => {
                quote!(#ty: std::fmt::Debug)
            }
            syn::GenericParam::Const(c) => c.to_token_stream(),
        })
        .collect::<Vec<_>>();
    let generics = input.generics;

    let bound = if bounds.len() > 0 {
        quote!(
            <#(#bounds),*>
        )
    } else {
        quote!()
    };

    let fields = match &input.data {
        syn::Data::Struct(s) => s.fields.iter().filter_map(|f| {
            let field_name = f.ident.as_ref()?;
            let field_name_lit = field_name.to_string();

            let fmt = f.attrs.iter().find_map(|attr| {
                if attr.path().is_ident("debug") {
                    if let Ok(MetaNameValue {
                        value:
                            syn::Expr::Lit(ExprLit {
                                lit: Lit::Str(lit_str),
                                ..
                            }),
                        ..
                    }) = attr.meta.require_name_value()
                    {
                        return Some(lit_str.value().to_token_stream());
                    }
                }
                None
            });

            let display = match fmt {
                Some(fmt) => quote!(&format_args!(#fmt, &self.#field_name)),
                None => quote!(&self.#field_name),
            };
            Some(quote!(
                .field(#field_name_lit, #display)
            ))
        }),
        syn::Data::Enum(_) => unimplemented!(),
        syn::Data::Union(_) => unimplemented!(),
    };

    quote!(
        impl #bound std::fmt::Debug for #ident #generics {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
            {
                f.debug_struct(#ident_lit)
                    #(#fields)*
                    .finish()
            }
        }

    )
    .into()
}
