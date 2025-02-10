use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::parse::Parser;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Data, DeriveInput, Fields, Lit, Meta,
};

/// Returns true if the field’s type is of the form Option<…>
fn field_is_optional(field: &syn::Field) -> bool {
    if let syn::Type::Path(type_path) = &field.ty {
        type_path
            .path
            .segments
            .first()
            .map(|seg| seg.ident == "Option")
            .unwrap_or(false)
    } else {
        false
    }
}

/// Given a field and a key (e.g. "create_model" or "update_model"),
/// look for a `#[crudcrate(...)]` attribute on the field and return the boolean value
/// associated with that key, if present.
fn get_crudcrate_bool(field: &syn::Field, key: &str) -> Option<bool> {
    for attr in &field.attrs {
        if attr.path().is_ident("crudcrate") {
            if let Meta::List(meta_list) = &attr.meta {
                let metas: Punctuated<Meta, Comma> = Punctuated::parse_terminated
                    .parse2(meta_list.tokens.clone())
                    .ok()?;
                for meta in metas.iter() {
                    if let Meta::NameValue(nv) = meta {
                        if nv.path.is_ident(key) {
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

/// Given a field and a key (e.g. "on_create" or "on_update"), returns the expression
/// provided in the `#[crudcrate(...)]` attribute for that key.
fn get_crudcrate_expr(field: &syn::Field, key: &str) -> Option<syn::Expr> {
    for attr in &field.attrs {
        if attr.path().is_ident("crudcrate") {
            if let Meta::List(meta_list) = &attr.meta {
                let metas: Punctuated<Meta, Comma> = Punctuated::parse_terminated
                    .parse2(meta_list.tokens.clone())
                    .ok()?;
                for meta in metas.iter() {
                    if let Meta::NameValue(nv) = meta {
                        if nv.path.is_ident(key) {
                            return Some(nv.value.clone());
                        }
                    }
                }
            }
        }
    }
    None
}

/// Extracts a string literal from a struct-level attribute of the form:
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

/// ===================
/// ToCreateModel Macro
/// ===================
/// This macro:
/// 1. Generates a struct named `<OriginalName>Create` that includes only the fields
///    where `#[crudcrate(create_model = false)]` is NOT specified (default is true).
/// 2. Generates an impl of `From<<OriginalName>Create> for <ActiveModelType>` where:
///    - For each field that is exposed (create_model = true), the value is taken from the
///      create struct.
///    - For fields that are not exposed but have an `on_create` expression, that expression
///      is used (with `.into()` called if necessary).
///    - Otherwise, the field is set to `ActiveValue::NotSet`.
#[proc_macro_derive(ToCreateModel, attributes(crudcrate))]
pub fn to_create_model(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let create_name = format_ident!("{}Create", name);

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

    // Support only structs with named fields.
    let fields = if let Data::Struct(data) = input.data {
        if let Fields::Named(named) = data.fields {
            named.named
        } else {
            panic!("ToCreateModel only supports structs with named fields");
        }
    } else {
        panic!("ToCreateModel can only be derived for structs");
    };

    // Build the Create struct with only fields where create_model is true.
    let create_struct_fields = fields
        .iter()
        .filter(|field| get_crudcrate_bool(field, "create_model").unwrap_or(true))
        .map(|field| {
            let ident = &field.ident;
            let ty = &field.ty;
            quote! {
                pub #ident: #ty
            }
        });

    // Generate conversion lines.
    let mut conv_lines = Vec::new();
    for field in fields.iter() {
        let ident = field.ident.as_ref().unwrap();
        let include = get_crudcrate_bool(field, "create_model").unwrap_or(true);
        let is_optional = field_is_optional(field);
        if include {
            // Pass the value directly.
            conv_lines.push(quote! {
                #ident: ActiveValue::Set(create.#ident)
            });
        } else if let Some(expr) = get_crudcrate_expr(field, "on_create") {
            // For on_create, if the field is optional, we assume the expression returns the inner type,
            // so we wrap it in Some.
            if is_optional {
                conv_lines.push(quote! {
                    #ident: ActiveValue::Set(Some((#expr).into()))
                });
            } else {
                conv_lines.push(quote! {
                    #ident: ActiveValue::Set((#expr).into())
                });
            }
        }
    }

    let expanded = quote! {
        #[derive(Serialize, Deserialize, ToSchema)]
        pub struct #create_name {
            #(#create_struct_fields),*
        }

        impl From<#create_name> for #active_model_type {
            fn from(create: #create_name) -> Self {
                #active_model_type {
                    #(#conv_lines),*
                }
            }
        }
    };

    TokenStream::from(expanded)
}

/// ===================
/// ToUpdateModel Macro
/// ===================
/// This macro:
/// 1. Generates a struct named `<OriginalName>Update` that includes only the fields
///    where `#[crudcrate(update_model = false)]` is NOT specified (default is true).
/// 2. Generates an impl for a method
///    `merge_into_activemodel(self, mut model: ActiveModelType) -> ActiveModelType`
///    that, for each field:
///    - For fields included in the update struct, if a value is provided, it is merged into the model.
///      If the field is optional, the value (of type T) must be wrapped in Some to match the ActiveModel’s field of type Option<T>.
///    - For fields excluded (update_model = false) but with an `on_update` expression, that expression is used
///      (wrapped with Some(...) if the field is optional).
///    - Other fields are left unchanged.
#[proc_macro_derive(ToUpdateModel, attributes(crudcrate, active_model))]
pub fn to_update_model(input: TokenStream) -> TokenStream {
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

    // Support only structs with named fields.
    let fields = if let Data::Struct(data) = input.data {
        if let Fields::Named(named) = data.fields {
            named.named
        } else {
            panic!("ToUpdateModel only supports structs with named fields");
        }
    } else {
        panic!("ToUpdateModel can only be derived for structs");
    };

    // Build the Update struct with only fields where update_model is true.
    let included_fields: Vec<_> = fields
        .iter()
        .filter(|field| get_crudcrate_bool(field, "update_model").unwrap_or(true))
        .collect();

    let update_struct_fields = included_fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;
        // For update, if the field is Option<T>, we want the update struct field to be Option<Option<T>>.
        let (_is_option, inner_ty) = if let syn::Type::Path(type_path) = ty {
            if let Some(seg) = type_path.path.segments.first() {
                if seg.ident == "Option" {
                    if let syn::PathArguments::AngleBracketed(inner_args) = &seg.arguments {
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
        quote! {
            #[serde(
                default,
                skip_serializing_if = "Option::is_none",
                with = "::serde_with::rust::double_option"
            )]
            pub #ident: Option<Option<#inner_ty>>
        }
    });

    // Generate merge code for fields included in the update struct.
    let included_merge: Vec<_> = included_fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let is_optional = field_is_optional(field);
            if is_optional {
                // For optional fields, wrap the inner value in Some.
                quote! {
                    model.#ident = match self.#ident {
                        Some(Some(value)) => ActiveValue::Set(Some(value)),
                        Some(_) => ActiveValue::NotSet,
                        _ => ActiveValue::NotSet,
                    };
                }
            } else {
                quote! {
                    model.#ident = match self.#ident {
                        Some(Some(value)) => ActiveValue::Set(value),
                        Some(_) => ActiveValue::NotSet,
                        _ => ActiveValue::NotSet,
                    };
                }
            }
        })
        .collect();

    // For fields excluded (update_model = false) that have an on_update expression,
    // generate merge code. Wrap the expression using `.into()` if needed.
    let excluded_merge: Vec<_> = fields
        .iter()
        .filter_map(|field| {
            if get_crudcrate_bool(field, "update_model") == Some(false) {
                if let Some(expr) = get_crudcrate_expr(field, "on_update") {
                    let ident = &field.ident;
                    if field_is_optional(field) {
                        Some(quote! {
                            model.#ident = ActiveValue::Set(Some((#expr).into()));
                        })
                    } else {
                        Some(quote! {
                            model.#ident = ActiveValue::Set((#expr).into());
                        })
                    }
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();

    let expanded = quote! {
        #[derive(Serialize, Deserialize, ToSchema)]
        pub struct #update_name {
            #(#update_struct_fields),*
        }

        impl #update_name {
            pub fn merge_into_activemodel(self, mut model: #active_model_type) -> #active_model_type {
                #(#included_merge)*
                #(#excluded_merge)*
                model
            }
        }
    };

    TokenStream::from(expanded)
}
