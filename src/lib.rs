use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::parse::Parser;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Data, DeriveInput, Fields, Lit, Meta,
};

/// Given a field and a key (e.g. "update" or "create"), look for a
/// `#[crudcrate(...)]` attribute on the field and return the boolean value
/// associated with that key, if present.
fn get_crudcrate_bool(field: &syn::Field, key: &str) -> Option<bool> {
    for attr in &field.attrs {
        if attr.path().is_ident("crudcrate") {
            // Expect an attribute like: #[crudcrate(update = false, create = true)]
            if let Meta::List(meta_list) = &attr.meta {
                // Parse the inner tokens as a punctuated list of Meta.
                let metas: Punctuated<Meta, Comma> = Punctuated::parse_terminated
                    .parse2(meta_list.tokens.clone())
                    .ok()?;
                for meta in metas.iter() {
                    if let Meta::NameValue(nv) = meta {
                        if nv.path.is_ident(key) {
                            // nv.value is an expression; match it as a literal.
                            if let syn::Expr::Lit(expr_lit) = &nv.value {
                                if let Lit::Bool(b) = &expr_lit.lit {
                                    return Some(b.value);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Helper function to extract a string literal from a struct-level attribute,
/// expecting the attribute to be of the form:
///   #[active_model = "some::path"]
fn get_string_from_attr(attr: &syn::Attribute) -> Option<String> {
    if let Meta::NameValue(nv) = &attr.meta {
        if let syn::Expr::Lit(expr_lit) = &nv.value {
            if let Lit::Str(s) = &expr_lit.lit {
                return Some(s.value());
            }
        }
    }
    None
}

/// Generates a "create" model struct.
/// Fields marked with `#[crudcrate(create = false)]` are skipped.
#[proc_macro_derive(ToCreateModel, attributes(crudcrate))]
pub fn to_create_model(input: TokenStream) -> TokenStream {
    // Parse the input tokens.
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let create_name = format_ident!("{}Create", name);

    // Only support structs with named fields.
    let fields = if let Data::Struct(data) = input.data {
        if let Fields::Named(named) = data.fields {
            named.named
        } else {
            panic!("ToCreateModel only supports structs with named fields");
        }
    } else {
        panic!("ToCreateModel can only be derived for structs");
    };

    // Filter out any field that has #[crudcrate(create = false)]
    let filtered_fields = fields
        .into_iter()
        .filter(|field| match get_crudcrate_bool(field, "create") {
            Some(false) => false,
            _ => true,
        })
        .map(|field| {
            let ident = field.ident;
            let ty = field.ty;
            quote! {
                pub #ident: #ty
            }
        });

    let expanded = quote! {
        #[derive(Serialize, Deserialize, ToSchema)]
        pub struct #create_name {
            #(#filtered_fields),*
        }
    };

    TokenStream::from(expanded)
}

/// Generates an "update" model struct along with an impl block containing a
/// `merge_into_activemodel` method.
///
/// For each field not marked with `#[crudcrate(update = false)]`, the macro
/// generates an update field of type `Option<Option<T>>` (with serde attributes)
/// so you can distinguish between "field omitted" and "provided as null".
///
/// The active model is assumed to be named `<OriginalName>ActiveModel`, unless an
/// override is provided via a struct-level attribute:
///   #[active_model = "path::to::ActiveModel"]
#[proc_macro_derive(ToUpdateModel, attributes(crudcrate, active_model))]
pub fn to_update_model(input: TokenStream) -> TokenStream {
    // Parse the input tokens.
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let update_name = format_ident!("{}Update", name);

    // Look for a struct-level active_model override.
    let mut active_model_override = None;
    for attr in &input.attrs {
        if attr.path().is_ident("active_model") {
            if let Some(s) = get_string_from_attr(attr) {
                active_model_override =
                    Some(syn::parse_str::<syn::Type>(&s).expect("Invalid active_model type"));
            }
        }
    }
    let active_model_type = if let Some(ty) = active_model_override {
        quote! { #ty }
    } else {
        let ident = format_ident!("{}ActiveModel", name);
        quote! { #ident }
    };

    // Only support structs with named fields.
    let fields = if let Data::Struct(data) = input.data {
        if let Fields::Named(named) = data.fields {
            named.named
        } else {
            panic!("ToUpdateModel only supports structs with named fields");
        }
    } else {
        panic!("ToUpdateModel can only be derived for structs");
    };

    let mut update_fields_decl = Vec::new();
    let mut update_fields_impl = Vec::new();

    // Process each field not marked with #[crudcrate(update = false)].
    for field in fields
        .into_iter()
        .filter(|field| match get_crudcrate_bool(field, "update") {
            Some(false) => false,
            _ => true,
        })
    {
        let ident = field.ident.expect("Expected field to have an ident");
        let ty = field.ty.clone();

        // For the update model, if the field is Option<T>, generate type Option<Option<T>>.
        let (_is_option, update_field_ty) = if let syn::Type::Path(type_path) = &ty {
            if let Some(segment) = type_path.path.segments.first() {
                if segment.ident == "Option" {
                    if let syn::PathArguments::AngleBracketed(inner_args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner_ty)) = inner_args.args.first()
                        {
                            (true, inner_ty.clone())
                        } else {
                            (false, ty.clone())
                        }
                    } else {
                        (false, ty.clone())
                    }
                } else {
                    (false, ty.clone())
                }
            } else {
                (false, ty.clone())
            }
        } else {
            (false, ty.clone())
        };

        update_fields_decl.push(quote! {
            #[serde(
                default,
                skip_serializing_if = "Option::is_none",
                with = "::serde_with::rust::double_option"
            )]
            pub #ident: Option<Option<#update_field_ty>>
        });

        let is_option = if let syn::Type::Path(type_path) = &ty {
            type_path
                .path
                .segments
                .first()
                .map(|seg| seg.ident == "Option")
                .unwrap_or(false)
        } else {
            false
        };

        let assignment = if is_option {
            quote! { ActiveValue::Set(Some(value)) }
        } else {
            quote! { ActiveValue::Set(value) }
        };

        update_fields_impl.push(quote! {
            model.#ident = match self.#ident {
                Some(Some(value)) => #assignment,
                Some(_) => ActiveValue::NotSet,
                _ => ActiveValue::NotSet,
            };
        });
    }

    let expanded = quote! {
        #[derive(Serialize, Deserialize, ToSchema)]
        pub struct #update_name {
            #(#update_fields_decl),*
        }

        impl #update_name {
            pub fn merge_into_activemodel(self, mut model: #active_model_type) -> #active_model_type {
                #(#update_fields_impl)*
                model
            }
        }
    };

    TokenStream::from(expanded)
}
