### Note
These are the procedural macros for the `crudcrate` crate. They are not meant to be used directly
and can be accessed through the `crudcrate` crate.

[GitHub](https://github.com/evanjt/crudcrate) | [crates.io](https://crates.io/crates/crudcrate)


# crudcrate-derive
[crates.io](https://crates.io/crates/crudcrate-derive)

This crate aims to reduce the amount of excess code required to generate structures
for CRUD APIs working with sea-orm and axum.

For example, the structs below, and their mutating functions with an ActiveModel:

```rust
use super::db::{ActiveModel, Model};
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;
use uuid::Uuid;
use sea_orm::{FromQueryResult, NotSet, Set};

#[derive(ToSchema, Serialize, Deserialize)]
pub struct Project {
    color: String,
    last_updated: NaiveDateTime,
    description: Option<String>,
    id: Uuid,
    name: String,
}

#[derive(ToSchema, Serialize, Deserialize, FromQueryResult)]
pub struct ProjectCreate {
    pub color: Option<String>,
    pub description: Option<String>,
    pub name: String,
}

#[derive(ToSchema, Serialize, Deserialize, FromQueryResult)]
pub struct ProjectUpdate {
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "::serde_with::rust::double_option"
    )]
    pub color: Option<Option<String>>,
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "::serde_with::rust::double_option"
    )]
    pub description: Option<Option<String>>,
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "::serde_with::rust::double_option"
    )]
    pub name: Option<Option<String>>,
}

impl ProjectUpdate {
    pub fn merge_into_activemodel(self, mut model: ActiveModel) -> ActiveModel {
        model.color = match self.color {
            Some(Some(color)) => Set(color),
            None => NotSet,
            _ => NotSet,
        };
        model.description = match self.description {
            Some(description) => Set(description),
            None => NotSet,
        };
        model.name = match self.name {
            Some(Some(name)) => Set(name),
            None => NotSet,
            _ => NotSet,
        };
        model
    }
}
```

Can be reduced to:

```rust
use crudcrate::{ToCreateModel, ToUpdateModel};
use sea_orm::FromQueryResult;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

#[derive(ToSchema, Serialize, Deserialize, FromQueryResult, ToUpdateModel, ToCreateModel)]
#[active_model = "super::db::ActiveModel"]
pub struct Project {
    color: String,
    #[crudcrate(update = false, create = false)]
    last_updated: NaiveDateTime,
    description: Option<String>,
    #[crudcrate(update = false, create = false)]
    id: Uuid,
    name: String,
}
```

Using the `ToUpdateModel` and `ToCreateModel` create the models
`ProjectUpdate` and `ProjectCreate` respectively. Using the `update` and
`create` attributes allow the flexibility of their inclusion in the
generated structs.

## Including Auxiliary (Non-DB) Fields

In some cases, you might want to include fields in your API models that do not directly map to columns in your database. For example, you might want to pass along auxiliary data that is handled separately in your business logic. You can achieve this by using the `non_db_attr` attribute. Fields marked with `#[crudcrate(non_db_attr = true, default = ...)]` will be included in the generated Create and Update models but will be omitted from the conversion into the database's ActiveModel.

For example, if you want to include a field to hold sensor data that is processed separately, you can annotate it as follows:

```rust
#[crudcrate(non_db_attr = true, default = vec![])]
pub data: Vec<crate::sensors::data::models::SensorData>,
```

This field will appear in both the generated Create and Update models (e.g. ProjectCreate/ProjectUpdate or your corresponding model's structs), allowing you to handle it in your custom update or create logic without affecting the database operations.
