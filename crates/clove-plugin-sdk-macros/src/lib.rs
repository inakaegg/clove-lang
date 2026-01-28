use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, ItemFn, Lit, Meta, MetaNameValue};

#[proc_macro_attribute]
pub fn clove_fn(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut name: Option<String> = None;
    let mut ty: Option<String> = None;
    let mut doc: Option<String> = None;
    let mut arity: Option<String> = None;
    let mut overloads: Vec<String> = Vec::new();

    let args = parse_macro_input!(args as syn::AttributeArgs);
    for arg in args {
        match arg {
            syn::NestedMeta::Meta(Meta::NameValue(MetaNameValue { path, lit, .. })) => {
                let Some(ident) = path.get_ident() else {
                    continue;
                };
                let key = ident.to_string();
                match (key.as_str(), lit) {
                    ("name", Lit::Str(lit)) => name = Some(lit.value()),
                    ("ty", Lit::Str(lit)) => ty = Some(lit.value()),
                    ("doc", Lit::Str(lit)) => doc = Some(lit.value()),
                    ("arity", Lit::Str(lit)) => arity = Some(lit.value()),
                    _ => {}
                }
            }
            syn::NestedMeta::Meta(Meta::List(list)) => {
                if list.path.is_ident("overloads") {
                    for nested in list.nested.iter() {
                        if let syn::NestedMeta::Lit(Lit::Str(lit)) = nested {
                            overloads.push(lit.value());
                        }
                    }
                }
            }
            _ => {}
        }
    }

    let name = match name {
        Some(value) => value,
        None => {
            return syn::Error::new_spanned(
                proc_macro2::TokenStream::from(input),
                "clove_fn requires name = \"...\"",
            )
            .to_compile_error()
            .into();
        }
    };
    let ty = match ty {
        Some(value) => value,
        None => {
            return syn::Error::new_spanned(
                proc_macro2::TokenStream::from(input),
                "clove_fn requires ty = \"...\"",
            )
            .to_compile_error()
            .into();
        }
    };
    let arity = match arity {
        Some(value) => value,
        None => {
            return syn::Error::new_spanned(
                proc_macro2::TokenStream::from(input),
                "clove_fn requires arity = \"min..max\"",
            )
            .to_compile_error()
            .into();
        }
    };

    let (arity_min, arity_max) = match parse_arity(&arity) {
        Ok(value) => value,
        Err(err) => return err.to_compile_error().into(),
    };

    let item_fn = parse_macro_input!(input as ItemFn);
    let fn_name = item_fn.sig.ident.clone();
    let register_name = format_ident!("__clove_register_{}", fn_name);
    let overloads_tokens = if overloads.is_empty() {
        quote! { &[] }
    } else {
        let overload_lits: Vec<_> = overloads.iter().map(|s| quote! { #s }).collect();
        quote! { &[#(#overload_lits),*] }
    };
    let doc_tokens = match doc {
        Some(value) => quote! { Some(#value) },
        None => quote! { None },
    };

    let expanded = quote! {
        #item_fn

        fn #register_name(
            host: &::clove_plugin_sdk::HostApiV1,
            env: ::clove_plugin_sdk::EnvHandle,
        ) -> bool {
            ::clove_plugin_sdk::register_fn(host, env, #name, #arity_min, #arity_max, #fn_name)
        }

        ::clove_plugin_sdk::inventory::submit! {
            ::clove_plugin_sdk::FnSpec {
                name: #name,
                ty: #ty,
                overloads: #overloads_tokens,
                doc: #doc_tokens,
                arity_min: #arity_min,
                arity_max: #arity_max,
                register: #register_name,
            }
        }
    };
    expanded.into()
}

fn parse_arity(arity: &str) -> Result<(u8, u8), syn::Error> {
    let parts: Vec<&str> = arity.split("..").collect();
    if parts.len() != 2 {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "arity must be in the form \"min..max\"",
        ));
    }
    let min: u8 = parts[0]
        .trim()
        .parse()
        .map_err(|_| syn::Error::new(proc_macro2::Span::call_site(), "invalid arity min"))?;
    let max: u8 = parts[1]
        .trim()
        .parse()
        .map_err(|_| syn::Error::new(proc_macro2::Span::call_site(), "invalid arity max"))?;
    Ok((min, max))
}
