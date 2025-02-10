use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Lit, Meta};

/// Helper function to extract a bool value from an attribute,
/// expecting the attribute to be of the form `#[update = false]`.
fn get_bool_from_attr(attr: &syn::Attribute) -> Option<bool> {
    if let Meta::NameValue(nv) = &attr.meta {
        // In syn 2.0, nv.value is an Expr, so we need to match it as a literal.
        if let syn::Expr::Lit(expr_lit) = &nv.value {
            if let Lit::Bool(b) = &expr_lit.lit {
                return Some(b.value);
            }
        }
    }
    None
}

/// Helper function to extract a string literal from an attribute,
/// expecting the attribute to be of the form `#[active_model = "some::path"]`.
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
/// Fields marked with `#[update = false]` are skipped.
#[proc_macro_derive(ToCreateModel, attributes(update))]
pub fn to_create_model(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let create_name = format_ident!("{}Create", name);

    // Ensure we have a struct with named fields.
    let fields = if let Data::Struct(data) = input.data {
        if let Fields::Named(named) = data.fields {
            named.named
        } else {
            panic!("ToCreateModel only supports structs with named fields");
        }
    } else {
        panic!("ToCreateModel can only be derived for structs");
    };

    // Filter out fields with #[update = false].
    let filtered_fields = fields
        .into_iter()
        .filter(|field| {
            field.attrs.iter().all(|attr| {
                if attr.path().is_ident("update") {
                    if let Some(val) = get_bool_from_attr(attr) {
                        return val;
                    }
                }
                true
            })
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
/// For each field in the original struct that is not marked with `#[update = false]`,
/// the macro generates an update field of type `Option<Option<T>>` (with serde attributes)
/// so that you can distinguish between "field omitted" and "provided as null".
///
/// The active model is assumed to be named `<OriginalName>ActiveModel`, unless an
/// override is provided via `#[active_model = "path::to::ActiveModel"]`.
#[proc_macro_derive(ToUpdateModel, attributes(update, active_model))]
pub fn to_update_model(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident; // e.g. PlotSample
    let update_name = format_ident!("{}Update", name); // e.g. PlotSampleUpdate

    // Check if an attribute `active_model` is provided on the struct.
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

    // Ensure we have a struct with named fields.
    let fields = if let Data::Struct(data) = input.data {
        if let Fields::Named(named) = data.fields {
            named.named
        } else {
            panic!("ToUpdateModel only supports structs with named fields");
        }
    } else {
        panic!("ToUpdateModel can only be derived for structs");
    };

    // Prepare vectors to accumulate field declarations and merge assignments.
    let mut update_fields_decl = Vec::new();
    let mut update_fields_impl = Vec::new();

    // Process each field that should be updateable.
    for field in fields.into_iter().filter(|field| {
        // Only include this field if none of its #[update = ...] attributes indicate exclusion.
        field.attrs.iter().all(|attr| {
            if attr.path().is_ident("update") {
                if let Some(val) = get_bool_from_attr(attr) {
                    return val;
                }
            }
            true
        })
    }) {
        let ident = field.ident.expect("Expected field to have an ident");
        let ty = field.ty.clone();

        // For the update model, unwrap Option<T> so that the field becomes Option<Option<T>>.
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

        // Determine if the original field type is Option<T>.
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

        // Generate the assignment conversion.
        // For fields that are Option<T> in the original model, generate:
        //    Some(Some(value)) => ActiveValue::Set(Some(value))
        // Otherwise:
        //    Some(Some(value)) => ActiveValue::Set(value)
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
