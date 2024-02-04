use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, DeriveInput, ExprLit, GenericArgument, Lit,
    Meta, MetaNameValue, Path, PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident_lit = input.ident.to_string();
    let ident = input.ident;

    // Q: Are there any better options than String to link between Type vs TypeParam?
    let mut is_phantom_data = HashSet::<String>::new();
    let mut associated = HashMap::<String, Vec<proc_macro2::TokenStream>>::new();
    let generics = input.generics;

    let mut types = vec![];
    let mut bounds = vec![];

    generics.type_params().for_each(|ty| {
        associated.insert(ty.ident.to_string(), vec![]);
    });

    let custom_bound = input.attrs.iter().find_map(|attr| {
        if attr.path().is_ident("debug") {
            if let Ok(meta) = attr.parse_args::<Meta>() {
                if let Ok(MetaNameValue {
                    value:
                        syn::Expr::Lit(ExprLit {
                            lit: Lit::Str(lit_str),
                            ..
                        }),
                    ..
                }) = meta.require_name_value()
                {
                    return Some(
                        lit_str
                            .value()
                            .parse::<proc_macro2::TokenStream>()
                            .unwrap_or(
                                syn::Error::new(lit_str.span(), "expect to be token stream")
                                    .into_compile_error(),
                            ),
                    );
                }
            }
        }
        None
    });

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

                if custom_bound.is_none() {
                    collect_associated_types(&f.ty, &mut associated);
                }

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

    associated.retain(|_, v| !v.is_empty());

    generics.params.iter().for_each(|ty| match ty {
        syn::GenericParam::Lifetime(l) => types.push(l.to_token_stream()),
        syn::GenericParam::Type(t) => {
            let ty_ident = &t.ident;
            types.push(quote!(#ty_ident));
            let bound = if is_phantom_data.contains(&ty.to_token_stream().to_string()) {
                quote!(PhantomData<#ty>: std::fmt::Debug)
            } else if let Some(associated_list) = associated.remove(&ty_ident.to_string()) {
                let associated_bounds = associated_list
                    .into_iter()
                    .map(|a| quote!(#a: std::fmt::Debug).into())
                    .collect::<Vec<proc_macro2::TokenStream>>();
                quote!(#(#associated_bounds),*)
            } else {
                quote!(#ty: std::fmt::Debug)
            };
            bounds.push(bound);
        }
        syn::GenericParam::Const(c) => types.push(c.to_token_stream()),
    });

    let (bound, type_generics) = if bounds.len() > 0 {
        (
            custom_bound.unwrap_or(quote!(
                    #(#bounds),*
            )),
            quote!(
                    <#(#types),*>
            ),
        )
    } else {
        (quote!(), quote!())
    };

    quote!(
        impl #generics std::fmt::Debug for #ident #type_generics
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

fn collect_associated_types(
    ty: &Type,
    associated: &mut HashMap<String, Vec<proc_macro2::TokenStream>>,
) {
    match ty {
        Type::Array(_) => { /* unimplemented!() */ }
        Type::BareFn(_) => { /* unimplemented!() */ }
        Type::Group(_) => { /* unimplemented!() */ }
        Type::ImplTrait(_) => { /* unimplemented!() */ }
        Type::Infer(_) => { /* unimplemented!() */ }
        Type::Macro(_) => { /* unimplemented!() */ }
        Type::Never(_) => { /* unimplemented!() */ }
        Type::Paren(_) => { /* unimplemented!() */ }
        Type::Path(TypePath {
            qself: std::option::Option::None,
            path: Path { segments, .. },
            ..
        }) => {
            for PathSegment {
                ident, arguments, ..
            } in segments
            {
                if let Some(associated_list) = associated.get_mut(&ident.to_string()) {
                    associated_list.push(segments.to_token_stream());
                }

                let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
                    arguments
                else {
                    continue;
                };

                if let std::option::Option::Some(GenericArgument::Type(ty)) = args.first() {
                    collect_associated_types(ty, associated);
                };
            }
        }
        Type::Ptr(_) => { /* unimplemented!() */ }
        Type::Reference(_) => { /* unimplemented!() */ }
        Type::Slice(_) => { /* unimplemented!() */ }
        Type::TraitObject(_) => { /* unimplemented!() */ }
        Type::Tuple(_) => { /* unimplemented!() */ }
        Type::Verbatim(_) => { /* unimplemented!() */ }
        _ => unreachable!(),
    }
}
