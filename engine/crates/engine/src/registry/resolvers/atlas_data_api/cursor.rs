use super::{normalize, value::MongoValue, JsonMap};
use crate::{
    names::MONGODB_OUTPUT_FIELD_ID,
    registry::{
        resolvers::ResolverContext,
        type_kinds::{OutputType, SelectionSetTarget},
        MetaField, MetaType, TypeReference,
    },
    Context, ContextExt, ContextField, ServerError, ServerResult,
};
use indexmap::IndexMap;
use runtime::search::GraphqlCursor;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[repr(u8)]
pub(super) enum OrderByDirection {
    Ascending,
    Descending,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct CursorField {
    pub(super) name: String,
    pub(super) value: MongoValue,
    pub(super) direction: OrderByDirection,
}

impl CursorField {
    fn implicit_id(value: Value) -> Self {
        Self {
            name: MONGODB_OUTPUT_FIELD_ID.to_string(),
            value: MongoValue::from_json("ID", value),
            direction: OrderByDirection::Ascending,
        }
    }
}

/// A cursor definition for MongoDB Atlas.
/// Combines the id with possible sort fields together with the
/// sort direction.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub(super) struct AtlasCursor {
    pub(super) fields: Vec<CursorField>,
}

impl AtlasCursor {
    pub(super) fn new(
        ctx: &ContextField<'_>,
        resolver_ctx: &ResolverContext<'_>,
        order_by: Option<&[JsonMap]>,
        document: &JsonMap,
    ) -> ServerResult<Self> {
        match order_by {
            Some(order_by) => Self::from_ordering(ctx, resolver_ctx, order_by, document),
            None => {
                let id = document.get(MONGODB_OUTPUT_FIELD_ID).cloned().unwrap();
                Ok(Self::from_id(id))
            }
        }
    }

    fn from_id(id: Value) -> Self {
        AtlasCursor {
            fields: vec![CursorField::implicit_id(id)],
        }
    }

    fn from_ordering(
        ctx: &ContextField<'_>,
        resolver_ctx: &ResolverContext<'_>,
        order_by: &[JsonMap],
        document: &JsonMap,
    ) -> ServerResult<Self> {
        let mut fields = Vec::new();

        let selection_target: SelectionSetTarget<'_> = resolver_ctx.ty.try_into().unwrap();

        let selection_edges = selection_target
            .field("edges")
            .and_then(|field| ctx.registry().lookup(&field.ty).ok());

        let selection_node = selection_edges.as_ref().and_then(|output| output.field("node"));

        let node_type = selection_node.and_then(|field| ctx.registry().lookup(&field.ty).ok());
        let type_info = node_type.as_ref().and_then(OutputType::field_map).unwrap();
        let input_type = ctx.find_argument_type("orderBy")?;

        let order_by = order_by.iter().fold(JsonMap::new(), |mut acc, map| {
            let map = embed_type_info(ctx, map, type_info);
            let map = normalize::keys(ctx, map, input_type);
            let map = normalize::flatten_keys(map);

            acc.extend(map);
            acc
        });

        for (key, value) in order_by.iter() {
            let key_path = key.split('.');

            let field = key_path.fold(None, |acc, key| match acc {
                Some(Value::Object(ref object)) => Some(object.get(key).cloned().unwrap_or(Value::Null)),
                Some(value) => Some(value),
                None => Some(document.get(key).cloned().unwrap_or(Value::Null)),
            });

            let direction = match value.get("$value").and_then(serde_json::Value::as_str) {
                Some("DESC") => OrderByDirection::Descending,
                _ => OrderByDirection::Ascending,
            };

            let r#type = value
                .get("$type")
                .and_then(serde_json::Value::as_str)
                .map(std::string::ToString::to_string)
                .unwrap();

            let value = field
                .map(|value| MongoValue::from_json(&r#type, value))
                .unwrap_or(MongoValue::Null);

            fields.push(CursorField {
                name: key.clone(),
                value,
                direction,
            })
        }

        if !order_by.contains_key("_id") {
            // We always fetch the _id
            let value = document.get(MONGODB_OUTPUT_FIELD_ID).cloned().unwrap();
            fields.push(CursorField::implicit_id(value));
        }

        Ok(Self { fields })
    }
}

impl TryFrom<AtlasCursor> for GraphqlCursor {
    type Error = ServerError;

    fn try_from(value: AtlasCursor) -> Result<Self, Self::Error> {
        let mut serializer = flexbuffers::FlexbufferSerializer::new();

        value
            .serialize(&mut serializer)
            .map_err(|error| ServerError::new(format!("invalid cursor: {error}"), None))?;

        Ok(GraphqlCursor::from_bytes(serializer.take_buffer()))
    }
}

impl TryFrom<GraphqlCursor> for AtlasCursor {
    type Error = ServerError;

    fn try_from(value: GraphqlCursor) -> Result<Self, Self::Error> {
        let reader = flexbuffers::Reader::get_root(value.as_slice())
            .map_err(|error| ServerError::new(format!("invalid cursor: {error}"), None))?;

        Self::deserialize(reader).map_err(|error| ServerError::new(format!("invalid cursor: {error}"), None))
    }
}

fn embed_type_info(ctx: &ContextField<'_>, map: &JsonMap, type_info: &IndexMap<String, MetaField>) -> JsonMap {
    let mut result = JsonMap::new();

    for (key, value) in map {
        let meta_field = type_info.get(key).unwrap();
        let meta_type = ctx.get_type(meta_field.ty.base_type_name());

        match meta_type.and_then(MetaType::fields) {
            Some(fields) => {
                match value {
                    Value::Object(object) => {
                        result.extend(embed_type_info(ctx, object, fields));
                    }
                    value => {
                        let type_name = meta_field.ty.named_type();

                        let value = json!({
                            "$value": value,
                            "$type": type_name.as_str(),
                        });

                        result.insert(key.to_string(), value);
                    }
                };
            }
            None => {
                let type_name = meta_field.ty.named_type();

                let value = json!({
                    "$value": value,
                    "$type": type_name.as_str(),
                });

                result.insert(key.to_string(), value);
            }
        };
    }

    result
}
