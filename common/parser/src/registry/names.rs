use crate::{registry::ParentRelation, utils::to_lower_camelcase};
use case::CaseExt;
use dynaql_parser::types::TypeDefinition;

use super::NumericFieldKind;

pub const PAGINATION_INPUT_ARG_FIRST: &str = "first";
pub const PAGINATION_INPUT_ARG_LAST: &str = "last";
pub const PAGINATION_INPUT_ARG_BEFORE: &str = "before";
pub const PAGINATION_INPUT_ARG_AFTER: &str = "after";
pub const PAGINATION_INPUT_ARG_ORDER_BY: &str = "orderBy";

pub const PAGINATION_FIELD_EDGES: &str = "edges";
pub const PAGINATION_FIELD_PAGE_INFO: &str = "pageInfo";
pub const PAGINATION_FIELD_EDGE_NODE: &str = "node";
pub const PAGINATION_FIELD_EDGE_CURSOR: &str = "cursor";

pub const PAGE_INFO_TYPE: &str = "PageInfo";
pub const PAGE_INFO_FIELD_HAS_PREVIOUS_PAGE: &str = "hasPreviousPage";
pub const PAGE_INFO_FIELD_HAS_NEXT_PAGE: &str = "hasNextPage";
pub const PAGE_INFO_FIELD_START_CURSOR: &str = "startCursor";
pub const PAGE_INFO_FIELD_END_CURSOR: &str = "endCursor";

pub const INPUT_ARG_BY: &str = "by";
pub const INPUT_ARG_INPUT: &str = "input";

pub const INPUT_FIELD_RELATION_CREATE: &str = "create";
pub const INPUT_FIELD_RELATION_LINK: &str = "link";
pub const INPUT_FIELD_RELATION_UNLINK: &str = "unlink";
pub const INPUT_FIELD_NUM_OP_SET: &str = "set";
pub const INPUT_FIELD_NUM_OP_INCREMENT: &str = "increment";
pub const INPUT_FIELD_NUM_OP_DECREMENT: &str = "decrement";

pub struct MetaNames;

/// CONVENTIONS:
///     - Input must be suffixed by "input"
///     - The model name must be the prefix
///     - All types/inputs must be CamelCase, all fields must be camelCase.
impl MetaNames {
    pub fn model(model_type_definition: &TypeDefinition) -> String {
        model_type_definition.name.node.to_camel()
    }

    //
    // PAGINATION
    //
    pub fn query_collection(model_type_definition: &TypeDefinition) -> String {
        to_lower_camelcase(format!("{}Collection", Self::model(model_type_definition)))
    }

    pub fn pagination_edge_type(model_type_definition: &TypeDefinition) -> String {
        format!("{}Edge", Self::model(model_type_definition))
    }

    pub fn pagination_connection_type(model_type_definition: &TypeDefinition) -> String {
        format!("{}Connection", Self::model(model_type_definition))
    }

    pub fn pagination_orderby_input(model_type_definition: &TypeDefinition) -> String {
        format!("{}OrderByInput", Self::model(model_type_definition))
    }

    //
    // CREATE
    //
    pub fn mutation_create(model_type_definition: &TypeDefinition) -> String {
        to_lower_camelcase(format!("{}Create", Self::model(model_type_definition)))
    }

    pub fn create_payload_type(model_type_definition: &TypeDefinition) -> String {
        format!("{}CreatePayload", Self::model(model_type_definition))
    }

    /// Defines
    /// - without parent, the create mutation input type name.
    /// - with parent, the nested input type name to create said type when creating the parent.
    pub fn create_input(
        model_type_definition: &TypeDefinition,
        maybe_parent_relation: Option<&ParentRelation<'_>>,
    ) -> String {
        match maybe_parent_relation {
            None => format!("{}CreateInput", Self::model(model_type_definition)),
            Some(parent_relation) => format!(
                "{}Create{}",
                Self::relation_prefix(parent_relation),
                Self::model(model_type_definition),
            ),
        }
    }

    /// For a given relation, one can either link to an existing object or create a new one.
    pub fn create_relation_input(
        parent_relation: &ParentRelation<'_>,
        field_model_type_definition: &TypeDefinition,
    ) -> String {
        format!(
            "{}Create{}Relation",
            Self::relation_prefix(parent_relation),
            Self::model(field_model_type_definition)
        )
    }

    //
    // UPDATE
    //
    pub fn mutation_update(model_type_definition: &TypeDefinition) -> String {
        to_lower_camelcase(format!("{}Update", Self::model(model_type_definition)))
    }

    pub fn update_payload_type(model_type_definition: &TypeDefinition) -> String {
        format!("{}UpdatePayload", Self::model(model_type_definition))
    }

    pub fn update_input(model_type_definition: &TypeDefinition) -> String {
        format!("{}UpdateInput", Self::model(model_type_definition))
    }

    /// For a given relation, one can either change the (un)link to an existing object or create a new one
    pub fn update_relation_input(
        parent_relation: &ParentRelation<'_>,
        field_model_type_definition: &TypeDefinition,
    ) -> String {
        format!(
            "{}Update{}Relation",
            Self::relation_prefix(parent_relation),
            Self::model(field_model_type_definition)
        )
    }

    //
    // Numerical Operation
    //
    pub fn numerical_operation_input(kind: &NumericFieldKind) -> String {
        format!("{}OperationsInput", kind.as_str())
    }

    /// Prefix used for any input/output type created for a relation.
    fn relation_prefix(parent_relation: &ParentRelation<'_>) -> String {
        parent_relation.meta.name.to_camel()
    }

    // Name of the struct that looks up a model by a composite index.
    pub fn nested_order_by_input(model_name: &str, constraint_name: &str) -> String {
        format!("{model_name}By{}", constraint_name.to_camel())
    }
}
