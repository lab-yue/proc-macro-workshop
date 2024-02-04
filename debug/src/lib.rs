use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, DeriveInput, ExprLit, GenericArgument, Lit,
    MetaNameValue, Path, PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident_lit = input.ident.to_string();
    let ident = input.ident;

    // Q: Are there any better options than String to link between Type vs TypeParam?
    let mut is_phantom_data = HashSet::<String>::new();
    let generics = input.generics;

    let mut fields = vec![];
    match &input.data {
        syn::Data::Struct(s) => {
            for f in &s.fields {
                let Some(field_name) = f.ident.as_ref() else {
                    continue;
                };
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

                match wrapped_ty_name(&f.ty) {
                    Some((name, ty)) if name == "PhantomData" => {
                        is_phantom_data.insert(ty.to_token_stream().to_string());
                    }
                    _ => {}
                };

                let display = match fmt {
                    Some(fmt) => quote!(&format_args!(#fmt, &self.#field_name)),
                    None => quote!(&self.#field_name),
                };
                fields.push(quote!(
                    .field(#field_name_lit, #display)
                ))
            }
        }
        syn::Data::Enum(_) => unimplemented!(),
        syn::Data::Union(_) => unimplemented!(),
    };

    let bounds = generics
        .type_params()
        .map(|ty| {
            if is_phantom_data.contains(&ty.to_token_stream().to_string()) {
                quote!(PhantomData<#ty>: std::fmt::Debug)
            } else {
                quote!(#ty: std::fmt::Debug)
            }
        })
        .collect::<Vec<_>>();

    let bound = if bounds.len() > 0 {
        quote!(
                #(#bounds),*
        )
    } else {
        quote!()
    };

    quote!(
        impl #generics std::fmt::Debug for #ident #generics
        where
            #bound
        {
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

fn wrapped_ty_name<'t>(ty: &'t Type) -> std::option::Option<(String, &'t Type)> {
    let Type::Path(TypePath {
        qself: std::option::Option::None,
        path: Path { segments, .. },
        ..
    }) = ty
    else {
        return None;
    };

    let std::option::Option::Some(PathSegment {
        ident,
        arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
        ..
    }) = segments.first()
    else {
        return None;
    };

    let std::option::Option::Some(GenericArgument::Type(ty)) = args.first() else {
        return None;
    };

    std::option::Option::Some((ident.to_string(), ty))
}
