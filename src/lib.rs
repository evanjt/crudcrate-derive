use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::parse::Parser;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Data, DeriveInput, Fields, Lit, Meta,
};

/// Returns true if the field’s type is `Option<…>` (including `std::option::Option<…>`).
fn field_is_optional(field: &syn::Field) -> bool {
    if let syn::Type::Path(type_path) = &field.ty {
        // Look at the *last* segment in the path to see if its identifier is "Option"
        if let Some(last_seg) = type_path.path.segments.last() {
            last_seg.ident == "Option"
        } else {
            false
        }
    } else {
        false
    }
}

/// Given a field and a key (e.g. `"create_model"` or `"update_model"`),
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

/// Given a field and a key (e.g. `"on_create"` or `"on_update"`), returns the expression
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

/// Extracts a string literal from a struct‐level attribute of the form:
///   `#[active_model = "some::path"]`
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
///    where `#[crudcrate(create_model = false)]` is NOT specified (default = true).
///    If a field has an `on_create` expression, its type becomes `Option<…>`
///    (with `#[serde(default)]`) so the user can override that default.
/// 2. Generates an `impl From<<OriginalName>Create> for <ActiveModelType>>` where:
///    - For each field with `on_create`:
///       - If the original type was `Option<T>`, then `create.<field>` is `Option<Option<T>>`.
///         We match on that and do:
///           ```rust,ignore
///           match create.field {
///             Some(Some(v)) => Some(v.into()),      // user overrode with T
///             Some(None)    => None,                // user explicitly set null
///             None          => Some((expr).into()), // fallback to expr
///           }
///           ```
///       - If the original type was non‐optional `T`, then `create.<field>` is `Option<T>`.
///         We match on that and do:
///           ```rust,ignore
///           match create.field {
///             Some(v) => v.into(),
///             None    => (expr).into(),
///           }
///           ```
///    - For each field without `on_create`:
///       - If the original type was `Option<T>`, we do `create.<field>.map(|v| v.into())`.
///       - If it was non‐optional `T`, we do `create.<field>.into()`.
///    - For any field excluded (`create_model = false`) but having `on_create`, we do
///      `Some((expr).into())` if it was `Option<T>`, or just `(expr).into()` otherwise.
#[proc_macro_derive(ToCreateModel, attributes(crudcrate, active_model))]
pub fn to_create_model(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let create_name = format_ident!("{}Create", name);

    // Look for a struct‐level #[active_model = "…"] override
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

    // Only support structs with named fields
    let fields = if let Data::Struct(data) = input.data {
        if let Fields::Named(named) = data.fields {
            named.named
        } else {
            panic!("ToCreateModel only supports structs with named fields");
        }
    } else {
        panic!("ToCreateModel can only be derived for structs");
    };

    // Build the `<Name>Create` struct: include only fields where create_model != false
    let create_struct_fields = fields
        .iter()
        .filter(|field| get_crudcrate_bool(field, "create_model").unwrap_or(true))
        .map(|field| {
            let ident = &field.ident;
            let ty = &field.ty;

            // If #[crudcrate(non_db_attr)] and has a `default`, keep the original type
            if get_crudcrate_bool(field, "non_db_attr").unwrap_or(false) {
                if get_crudcrate_expr(field, "default").is_some() {
                    quote! {
                        #[serde(default)]
                        pub #ident: #ty
                    }
                } else {
                    quote! {
                        pub #ident: #ty
                    }
                }
            }
            // If there is an `#[crudcrate(on_create = …)]`, wrap in `Option<…>` so caller can override
            else if get_crudcrate_expr(field, "on_create").is_some() {
                quote! {
                    #[serde(default)]
                    pub #ident: Option<#ty>
                }
            }
            // Otherwise, just use the original type
            else {
                quote! {
                    pub #ident: #ty
                }
            }
        });

    // Now build the `impl From<Create> for ActiveModelType`
    let mut conv_lines = Vec::new();
    for field in fields.iter() {
        // Skip #[crudcrate(non_db_attr)] entirely
        if get_crudcrate_bool(field, "non_db_attr").unwrap_or(false) {
            continue;
        }
        let ident = field.ident.as_ref().unwrap();
        let include = get_crudcrate_bool(field, "create_model").unwrap_or(true);
        let is_optional = field_is_optional(field);

        if include {
            if let Some(expr) = get_crudcrate_expr(field, "on_create") {
                // CASE A: field has #[crudcrate(on_create = ...)]
                if is_optional {
                    // Original was Option<T> → create.<ident> is Option<Option<T>>
                    conv_lines.push(quote! {
                        #ident: ActiveValue::Set(match create.#ident {
                            Some(Some(inner)) => Some(inner.into()),       // user override
                            Some(None)         => None,                    // user explicitly set null
                            None               => Some((#expr).into()),    // fallback to expr
                        })
                    });
                } else {
                    // Original was T → create.<ident> is Option<T>
                    conv_lines.push(quote! {
                        #ident: ActiveValue::Set(match create.#ident {
                            Some(val) => val.into(),
                            None      => (#expr).into(),
                        })
                    });
                }
            }
            // CASE B: no on_create
            else if is_optional {
                // Original was Option<T> → do `map(|v| v.into())`
                conv_lines.push(quote! {
                    #ident: ActiveValue::Set(create.#ident.map(|v| v.into()))
                });
            } else {
                // Original was T → do `.into()` directly
                conv_lines.push(quote! {
                    #ident: ActiveValue::Set(create.#ident.into())
                });
            }
        }
        // CASE C: excluded from create but has #[crudcrate(on_create = ...)]
        else if let Some(expr) = get_crudcrate_expr(field, "on_create") {
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
        #[derive(Clone, Serialize, Deserialize, ToSchema)]
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
///    where `#[crudcrate(update_model = false)]` is NOT specified (default = true).
/// 2. Generates an impl for a method
///    `merge_into_activemodel(self, mut model: ActiveModelType) -> ActiveModelType`
///    that, for each field:
///    - If it’s included in the update struct, and the user provided a value:
///       - If the original field type was `Option<T>`, we match on
///         `Option<Option<T>>`:
///           ```rust,ignore
///           Some(Some(v)) => ActiveValue::Set(Some(v.into())),
///           Some(None)    => ActiveValue::Set(None),     // explicit set to None
///           None          => ActiveValue::NotSet,       // no change
///           ```  
///       - If the original field type was non‐optional `T`, we match on `Option<T>`:
///           ```rust,ignore
///           Some(val) => ActiveValue::Set(val.into()),
///           _         => ActiveValue::NotSet,
///           ```  
///    - If it’s excluded (`update_model = false`) but has `on_update = expr`, we do
///      `ActiveValue::Set(expr.into())` (wrapped in `Some(...)` if the original field was `Option<T>`).
///    - All other fields remain unchanged.
#[proc_macro_derive(ToUpdateModel, attributes(crudcrate, active_model))]
pub fn to_update_model(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let update_name = format_ident!("{}Update", name);

    // Look for a struct‐level #[active_model = "…"] override
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

    // Only support structs with named fields
    let fields = if let Data::Struct(data) = input.data {
        if let Fields::Named(named) = data.fields {
            named.named
        } else {
            panic!("ToUpdateModel only supports structs with named fields");
        }
    } else {
        panic!("ToUpdateModel can only be derived for structs");
    };

    // Collect only the fields where `update_model != false`
    let included_fields: Vec<_> = fields
        .iter()
        .filter(|field| get_crudcrate_bool(field, "update_model").unwrap_or(true))
        .collect();

    let update_struct_fields = included_fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;

        // If #[crudcrate(non_db_attr)] with a default, keep the original type
        if get_crudcrate_bool(field, "non_db_attr").unwrap_or(false) {
            if get_crudcrate_expr(field, "default").is_some() {
                quote! {
                    #[serde(default)]
                    pub #ident: #ty
                }
            } else {
                quote! {
                    pub #ident: #ty
                }
            }
        }
        // Otherwise, wrap in `Option<Option<Inner>>` so we can tell “not provided” vs “explicit None”
        else {
            // Pull out the inner T if this is Option<T>, else T itself.
            let (_is_option, inner_ty) = if let syn::Type::Path(type_path) = ty {
                if let Some(seg) = type_path.path.segments.last() {
                    if seg.ident == "Option" {
                        if let syn::PathArguments::AngleBracketed(inner_args) = &seg.arguments {
                            if let Some(syn::GenericArgument::Type(inner_ty)) =
                                inner_args.args.first()
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
                    with = "crudcrate::serde_with::rust::double_option"
                )]
                pub #ident: Option<Option<#inner_ty>>
            }
        }
    });

    // Generate the merge code for included fields:
    let included_merge: Vec<_> = included_fields
        .iter()
        .filter(|field| !get_crudcrate_bool(field, "non_db_attr").unwrap_or(false))
        .map(|field| {
            let ident = &field.ident;
            let is_optional = field_is_optional(field);

            if is_optional {
                // Original was Option<T> → `self.field` is Option<Option<T>>
                quote! {
                    model.#ident = match self.#ident {
                        Some(Some(value)) => ActiveValue::Set(Some(value.into())),
                        Some(None)      => ActiveValue::Set(None),    // explicitly set None
                        None            => ActiveValue::NotSet,      // no change
                    };
                }
            } else {
                // Original was T → `self.field` is Option<T>
                quote! {
                    model.#ident = match self.#ident {
                        Some(Some(value)) => ActiveValue::Set(value.into()),
                        _                 => ActiveValue::NotSet,
                    };
                }
            }
        })
        .collect();

    // Generate the merge code for fields excluded from update but having on_update:
    let excluded_merge: Vec<_> = fields
        .iter()
        .filter(|field| {
            get_crudcrate_bool(field, "update_model") == Some(false)
                && !get_crudcrate_bool(field, "non_db_attr").unwrap_or(false)
        })
        .filter_map(|field| {
            if let Some(expr) = get_crudcrate_expr(field, "on_update") {
                let ident = &field.ident;
                if field_is_optional(field) {
                    // Original was Option<T>
                    Some(quote! {
                        model.#ident = ActiveValue::Set(Some((#expr).into()));
                    })
                } else {
                    // Original was T
                    Some(quote! {
                        model.#ident = ActiveValue::Set((#expr).into());
                    })
                }
            } else {
                None
            }
        })
        .collect();

    let expanded = quote! {
        #[derive(Clone, Serialize, Deserialize, ToSchema)]
        pub struct #update_name {
            #(#update_struct_fields),*
        }

        // Inherent method that applies the merges:
        impl #update_name {
            pub fn merge_fields(self, mut model: #active_model_type) -> #active_model_type {
                #(#included_merge)*
                #(#excluded_merge)*
                model
            }
        }

        // Trait implementation that delegates to merge_fields:
        impl crudcrate::traits::MergeIntoActiveModel<#active_model_type> for #update_name {
            fn merge_into_activemodel(self, model: #active_model_type) -> #active_model_type {
                Self::merge_fields(self, model)
            }
        }
    };

    TokenStream::from(expanded)
}

/// =====================
/// EntityToModels Macro
/// =====================
/// This macro generates an API struct from a Sea-ORM entity Model struct, along with
/// ToCreateModel and ToUpdateModel implementations.
///
/// Usage:
/// ```ignore
/// #[derive(EntityToModels)]
/// #[crudcrate(api_struct = "Experiment", active_model = "spice_entity::experiments::ActiveModel")]
/// pub struct Model {
///     #[crudcrate(update_model = false, create_model = false, on_create = Uuid::new_v4())]
///     pub id: Uuid,
///     pub name: String,
///     #[crudcrate(non_db_attr = true, default = vec![])]
///     pub regions: Vec<RegionInput>,
/// }
/// ```
///
/// This generates:
/// - An API struct with the specified name (e.g., `Experiment`)
/// - ToCreateModel and ToUpdateModel implementations
/// - From<Model> implementation for the API struct
/// - Support for non-db attributes
#[proc_macro_derive(EntityToModels, attributes(crudcrate))]
pub fn entity_to_models(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    
    // Extract the struct name and fields
    let struct_name = &input.ident;
    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => &fields.named,
            _ => {
                return syn::Error::new_spanned(
                    &input,
                    "EntityToModels only supports structs with named fields",
                )
                .to_compile_error()
                .into();
            }
        },
        _ => {
            return syn::Error::new_spanned(&input, "EntityToModels only supports structs")
                .to_compile_error()
                .into();
        }
    };

    // Extract attributes from the struct level
    let mut api_struct_name = None;
    let mut active_model_path = None;
    
    for attr in &input.attrs {
        if attr.path().is_ident("crudcrate") {
            if let Meta::List(meta_list) = &attr.meta {
                if let Ok(metas) = Punctuated::<Meta, Comma>::parse_terminated
                    .parse2(meta_list.tokens.clone())
                {
                    for meta in metas.iter() {
                        if let Meta::NameValue(nv) = meta {
                            if nv.path.is_ident("api_struct") {
                                if let syn::Expr::Lit(expr_lit) = &nv.value {
                                    if let Lit::Str(s) = &expr_lit.lit {
                                        api_struct_name = Some(format_ident!("{}", s.value()));
                                    }
                                }
                            } else if nv.path.is_ident("active_model") {
                                if let syn::Expr::Lit(expr_lit) = &nv.value {
                                    if let Lit::Str(s) = &expr_lit.lit {
                                        active_model_path = Some(s.value());
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    let api_struct_name = match api_struct_name {
        Some(name) => name,
        None => {
            return syn::Error::new_spanned(
                &input,
                "EntityToModels requires an `api_struct` attribute, e.g., #[crudcrate(api_struct = \"Experiment\")]",
            )
            .to_compile_error()
            .into();
        }
    };

    let active_model_path = match active_model_path {
        Some(path) => path,
        None => {
            return syn::Error::new_spanned(
                &input,
                "EntityToModels requires an `active_model` attribute, e.g., #[crudcrate(active_model = \"spice_entity::experiments::ActiveModel\")]",
            )
            .to_compile_error()
            .into();
        }
    };

    // Parse the active model path
    let _active_model_type: syn::Type = match syn::parse_str(&active_model_path) {
        Ok(ty) => ty,
        Err(_) => {
            return syn::Error::new_spanned(
                &input,
                format!("Invalid active_model path: {}", active_model_path),
            )
            .to_compile_error()
            .into();
        }
    };

    // Separate DB fields from non-DB fields
    let mut db_fields = Vec::new();
    let mut non_db_fields = Vec::new();
    
    for field in fields.iter() {
        let is_non_db = get_crudcrate_bool(field, "non_db_attr").unwrap_or(false);
        if is_non_db {
            non_db_fields.push(field);
        } else {
            db_fields.push(field);
        }
    }

    // Generate the API struct fields
    let mut api_struct_fields = Vec::new();
    let mut from_model_assignments = Vec::new();
    
    // Add DB fields
    for field in &db_fields {
        let field_name = &field.ident;
        let field_type = &field.ty;
        
        // Filter out sea_orm attributes for the API struct
        let api_field_attrs: Vec<_> = field.attrs.iter()
            .filter(|attr| !attr.path().is_ident("sea_orm"))
            .collect();
        
        api_struct_fields.push(quote! {
            #(#api_field_attrs)*
            pub #field_name: #field_type
        });
        
        // Handle DateTime conversion from DateTimeWithTimeZone
        let assignment = if field_type.to_token_stream().to_string().contains("DateTimeWithTimeZone") {
            if field_is_optional(field) {
                quote! {
                    #field_name: model.#field_name.map(|dt| dt.with_timezone(&chrono::Utc))
                }
            } else {
                quote! {
                    #field_name: model.#field_name.with_timezone(&chrono::Utc)
                }
            }
        } else {
            quote! {
                #field_name: model.#field_name
            }
        };
        
        from_model_assignments.push(assignment);
    }
    
    // Add non-DB fields with default values
    for field in &non_db_fields {
        let field_name = &field.ident;
        let field_type = &field.ty;
        
        // Get default value from attribute
        let default_expr = get_crudcrate_expr(field, "default")
            .unwrap_or_else(|| syn::parse_quote!(Default::default()));
        
        api_struct_fields.push(quote! {
            #[crudcrate(non_db_attr = true, default = #default_expr)]
            pub #field_name: #field_type
        });
        
        from_model_assignments.push(quote! {
            #field_name: #default_expr
        });
    }

    // Generate the API struct
    let api_struct = quote! {
        #[derive(ToSchema, Serialize, Deserialize, ToUpdateModel, ToCreateModel, Clone)]
        #[active_model = #active_model_path]
        pub struct #api_struct_name {
            #(#api_struct_fields),*
        }
    };

    // Generate From<Model> implementation
    let from_impl = quote! {
        impl From<#struct_name> for #api_struct_name {
            fn from(model: #struct_name) -> Self {
                Self {
                    #(#from_model_assignments),*
                }
            }
        }
    };

    let expanded = quote! {
        #api_struct
        #from_impl
    };

    TokenStream::from(expanded)
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::{parse_quote, Attribute, Field};

    #[test]
    fn test_field_is_optional_with_option() {
        let field: Field = parse_quote! {
            pub field: Option<String>
        };
        assert!(field_is_optional(&field));
    }

    #[test]
    fn test_field_is_optional_with_std_option() {
        let field: Field = parse_quote! {
            pub field: std::option::Option<String>
        };
        assert!(field_is_optional(&field));
    }

    #[test]
    fn test_field_is_optional_with_non_option() {
        let field: Field = parse_quote! {
            pub field: String
        };
        assert!(!field_is_optional(&field));
    }

    #[test]
    fn test_field_is_optional_with_complex_type() {
        let field: Field = parse_quote! {
            pub field: Vec<String>
        };
        assert!(!field_is_optional(&field));
    }

    #[test]
    fn test_get_crudcrate_bool_create_model_false() {
        let field: Field = parse_quote! {
            #[crudcrate(create_model = false)]
            pub field: String
        };
        assert_eq!(get_crudcrate_bool(&field, "create_model"), Some(false));
    }

    #[test]
    fn test_get_crudcrate_bool_create_model_true() {
        let field: Field = parse_quote! {
            #[crudcrate(create_model = true)]
            pub field: String
        };
        assert_eq!(get_crudcrate_bool(&field, "create_model"), Some(true));
    }

    #[test]
    fn test_get_crudcrate_bool_update_model_false() {
        let field: Field = parse_quote! {
            #[crudcrate(update_model = false)]
            pub field: String
        };
        assert_eq!(get_crudcrate_bool(&field, "update_model"), Some(false));
    }

    #[test]
    fn test_get_crudcrate_bool_multiple_attributes() {
        let field: Field = parse_quote! {
            #[crudcrate(create_model = false, update_model = true)]
            pub field: String
        };
        assert_eq!(get_crudcrate_bool(&field, "create_model"), Some(false));
        assert_eq!(get_crudcrate_bool(&field, "update_model"), Some(true));
    }

    #[test]
    fn test_get_crudcrate_bool_no_attribute() {
        let field: Field = parse_quote! {
            pub field: String
        };
        assert_eq!(get_crudcrate_bool(&field, "create_model"), None);
    }

    #[test]
    fn test_get_crudcrate_bool_wrong_attribute() {
        let field: Field = parse_quote! {
            #[serde(skip)]
            pub field: String
        };
        assert_eq!(get_crudcrate_bool(&field, "create_model"), None);
    }

    #[test]
    fn test_get_crudcrate_bool_non_db_attr() {
        let field: Field = parse_quote! {
            #[crudcrate(non_db_attr = true)]
            pub field: String
        };
        assert_eq!(get_crudcrate_bool(&field, "non_db_attr"), Some(true));
    }

    #[test]
    fn test_get_crudcrate_expr_on_create() {
        let field: Field = parse_quote! {
            #[crudcrate(on_create = Uuid::new_v4())]
            pub field: String
        };
        let expr = get_crudcrate_expr(&field, "on_create");
        assert!(expr.is_some());
        // Test that it contains the expected expression
        let expr_str = quote::quote!(#expr).to_string();
        assert!(expr_str.contains("Uuid :: new_v4"));
    }

    #[test]
    fn test_get_crudcrate_expr_on_update() {
        let field: Field = parse_quote! {
            #[crudcrate(on_update = Utc::now())]
            pub field: DateTime<Utc>
        };
        let expr = get_crudcrate_expr(&field, "on_update");
        assert!(expr.is_some());
        let expr_str = quote::quote!(#expr).to_string();
        assert!(expr_str.contains("Utc :: now"));
    }

    #[test]
    fn test_get_crudcrate_expr_default() {
        let field: Field = parse_quote! {
            #[crudcrate(default = "default_value".to_string())]
            pub field: String
        };
        let expr = get_crudcrate_expr(&field, "default");
        assert!(expr.is_some());
        let expr_str = quote::quote!(#expr).to_string();
        assert!(expr_str.contains("default_value"));
    }

    #[test]
    fn test_get_crudcrate_expr_no_match() {
        let field: Field = parse_quote! {
            #[crudcrate(create_model = false)]
            pub field: String
        };
        assert!(get_crudcrate_expr(&field, "on_create").is_none());
    }

    #[test]
    fn test_get_string_from_attr_simple() {
        let attr: Attribute = parse_quote! {
            #[active_model = "test::ActiveModel"]
        };
        let result = get_string_from_attr(&attr);
        assert_eq!(result, Some("test::ActiveModel".to_string()));
    }

    #[test]
    fn test_get_string_from_attr_no_match() {
        let attr: Attribute = parse_quote! {
            #[other_attr = "value"]
        };
        let result = get_string_from_attr(&attr);
        // This function extracts any string value regardless of attribute name
        assert_eq!(result, Some("value".to_string()));
    }

    #[test]
    fn test_get_string_from_attr_wrong_type() {
        let attr: Attribute = parse_quote! {
            #[active_model = true]
        };
        let result = get_string_from_attr(&attr);
        assert_eq!(result, None);
    }
}
