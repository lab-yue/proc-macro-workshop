use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident_lit = input.ident.to_string();
    let ident = input.ident;

    let fields = match &input.data {
        syn::Data::Struct(s) => s.fields.iter().filter_map(|f| {
            let field_name = f.ident.as_ref()?;
            let field_name_lit = field_name.to_string();
            Some(quote!(
                .field(#field_name_lit, &self.#field_name)
            ))
        }),
        syn::Data::Enum(_) => unimplemented!(),
        syn::Data::Union(_) => unimplemented!(),
    };

    quote!(
        impl std::fmt::Debug for #ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#ident_lit)
                    #(#fields)*
                    .finish()
            }
        }

    )
    .into()
}
