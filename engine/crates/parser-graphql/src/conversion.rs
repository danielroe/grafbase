//! Allow converting a [`cynic_introspection::Schema`] into a new [`Registry`].
//!
//! The conversion takes all the known information from the introspection and adds it to the
//! registry.

use std::ops::Not;

use engine::registry::{
    Deprecation, MetaDirective, MetaEnumValue, MetaField, MetaInputValue, MetaType, ObjectType, Registry,
    __DirectiveLocation,
};

pub fn registry_from_introspection(schema: cynic_introspection::Schema) -> Registry {
    let mut registry = Registry::new();

    // root types
    registry.query_type = schema.query_type;
    registry.mutation_type = schema.mutation_type;
    registry.subscription_type = schema.subscription_type;

    // directives
    registry.directives = schema
        .directives
        .into_iter()
        .map(|d| (d.name.clone(), directive_from_introspection(d)))
        .collect();

    registry.implements = schema
        .types
        .iter()
        .filter_map(|ty| match ty {
            cynic_introspection::Type::Object(object) => object
                .interfaces
                .is_empty()
                .not()
                .then(|| (ty.name(), &object.interfaces)),
            cynic_introspection::Type::Interface(interface) => interface
                .interfaces
                .is_empty()
                .not()
                .then(|| (ty.name(), &interface.interfaces)),
            _ => None,
        })
        .map(|(name, interfaces)| (name.to_string(), interfaces.iter().cloned().collect()))
        .collect();

    // types
    registry.types = schema
        .types
        .into_iter()
        .map(|ty| (ty.name().to_owned(), type_from_introspection(ty)))
        .collect();

    registry
}

fn directive_from_introspection(directive: cynic_introspection::Directive) -> MetaDirective {
    MetaDirective {
        name: directive.name,
        description: directive.description,
        locations: directive
            .locations
            .into_iter()
            .map(directive_location_from_introspection)
            .collect(),
        args: directive
            .args
            .into_iter()
            .map(|v| (v.name.clone(), input_value_from_introspection(v)))
            .collect(),
        is_repeatable: false,
        visible: None,
    }
}

fn field_from_introspection(field: cynic_introspection::Field) -> MetaField {
    MetaField {
        name: field.name,
        description: field.description,
        args: field
            .args
            .into_iter()
            .map(|v| (v.name.clone(), input_value_from_introspection(v)))
            .collect(),
        ty: field.ty.to_string().into(),
        deprecation: deprecated_from_introspection(field.deprecated),
        ..Default::default()
    }
}

fn input_value_from_introspection(input: cynic_introspection::InputValue) -> MetaInputValue {
    MetaInputValue {
        name: input.name,
        description: input.description,
        ty: input.ty.to_string().into(),
        default_value: input.default_value.map(Into::into),
        visible: None,
        validators: None,
        is_secret: false,
        rename: None,
    }
}

fn directive_location_from_introspection(location: cynic_introspection::DirectiveLocation) -> __DirectiveLocation {
    use __DirectiveLocation as EngineLocation;
    use cynic_introspection::DirectiveLocation as IntrospectionLocation;

    match location {
        IntrospectionLocation::Query => EngineLocation::QUERY,
        IntrospectionLocation::Mutation => EngineLocation::MUTATION,
        IntrospectionLocation::Subscription => EngineLocation::SUBSCRIPTION,
        IntrospectionLocation::Field => EngineLocation::FIELD,
        IntrospectionLocation::FragmentDefinition => EngineLocation::FRAGMENT_DEFINITION,
        IntrospectionLocation::FragmentSpread => EngineLocation::FRAGMENT_SPREAD,
        IntrospectionLocation::InlineFragment => EngineLocation::INLINE_FRAGMENT,
        IntrospectionLocation::VariableDefinition => EngineLocation::VARIABLE_DEFINITION,
        IntrospectionLocation::Schema => EngineLocation::SCHEMA,
        IntrospectionLocation::Scalar => EngineLocation::SCALAR,
        IntrospectionLocation::Object => EngineLocation::OBJECT,
        IntrospectionLocation::FieldDefinition => EngineLocation::FIELD_DEFINITION,
        IntrospectionLocation::ArgumentDefinition => EngineLocation::ARGUMENT_DEFINITION,
        IntrospectionLocation::Interface => EngineLocation::INTERFACE,
        IntrospectionLocation::Union => EngineLocation::UNION,
        IntrospectionLocation::Enum => EngineLocation::ENUM,
        IntrospectionLocation::EnumValue => EngineLocation::ENUM_VALUE,
        IntrospectionLocation::InputObject => EngineLocation::INPUT_OBJECT,
        IntrospectionLocation::InputFieldDefinition => EngineLocation::INPUT_FIELD_DEFINITION,
    }
}

fn type_from_introspection(ty: cynic_introspection::Type) -> MetaType {
    use cynic_introspection::Type;

    match ty {
        Type::Object(v) => object_from_introspection(v),
        Type::InputObject(v) => input_object_from_introspection(v),
        Type::Enum(v) => enum_from_introspection(v),
        Type::Interface(v) => interface_from_introspection(v),
        Type::Union(v) => union_from_introspection(v),
        Type::Scalar(v) => scalar_from_introspection(v),
    }
}

fn object_from_introspection(object: cynic_introspection::ObjectType) -> MetaType {
    ObjectType::new(object.name, object.fields.into_iter().map(field_from_introspection))
        .with_description(object.description)
        .into()
}

fn deprecated_from_introspection(deprecated: cynic_introspection::Deprecated) -> Deprecation {
    use cynic_introspection::Deprecated as IntrospectionDeprecated;

    match deprecated {
        IntrospectionDeprecated::No => Deprecation::NoDeprecated,
        IntrospectionDeprecated::Yes(reason) => Deprecation::Deprecated { reason },
    }
}

fn input_object_from_introspection(input: cynic_introspection::InputObjectType) -> MetaType {
    engine::registry::InputObjectType::new(input.name, input.fields.into_iter().map(input_value_from_introspection))
        .with_description(input.description)
        .into()
}

fn enum_from_introspection(enum_type: cynic_introspection::EnumType) -> MetaType {
    engine::registry::EnumType::new(
        enum_type.name,
        enum_type.values.into_iter().map(enum_value_from_introspection),
    )
    .with_description(enum_type.description)
    .into()
}

fn enum_value_from_introspection(enum_value: cynic_introspection::EnumValue) -> MetaEnumValue {
    MetaEnumValue {
        name: enum_value.name,
        description: enum_value.description,
        deprecation: deprecated_from_introspection(enum_value.deprecated),
        visible: None,
        value: None,
    }
}

fn interface_from_introspection(interface: cynic_introspection::InterfaceType) -> MetaType {
    MetaType::Interface(engine::registry::InterfaceType {
        name: interface.name.clone(),
        description: interface.description,
        fields: interface
            .fields
            .into_iter()
            .map(|v| (v.name.clone(), field_from_introspection(v)))
            .collect(),
        possible_types: interface.possible_types.into_iter().collect(),
        extends: false,
        visible: None,
        rust_typename: interface.name,
    })
}

fn union_from_introspection(union: cynic_introspection::UnionType) -> MetaType {
    MetaType::Union(engine::registry::UnionType {
        name: union.name.clone(),
        description: union.description,
        possible_types: union.possible_types.into_iter().collect(),
        visible: None,
        rust_typename: union.name,
        discriminators: None,
    })
}

fn scalar_from_introspection(scalar: cynic_introspection::ScalarType) -> MetaType {
    MetaType::Scalar(engine::registry::ScalarType {
        name: scalar.name,
        description: scalar.description,
        is_valid: None,
        visible: None,
        specified_by_url: None,
        parser: engine::registry::ScalarParser::PassThrough,
    })
}

#[cfg(test)]
mod tests {
    use cynic_introspection::query::IntrospectionQuery;

    use crate::conversion::registry_from_introspection;

    #[test]
    fn conversion() {
        let data = include_str!("../tests/swapi_introspection.json");
        let schema = serde_json::from_str::<IntrospectionQuery>(data)
            .unwrap()
            .into_schema()
            .unwrap();

        insta::with_settings!({sort_maps => true}, {
            insta::assert_json_snapshot!(registry_from_introspection(schema));
        });
    }

    #[test]
    fn array_input_value() {
        let data = include_str!("../tests/countries_introspection.json");
        let schema = serde_json::from_str::<IntrospectionQuery>(data)
            .unwrap()
            .into_schema()
            .unwrap();

        insta::with_settings!({sort_maps => true}, {
            insta::assert_json_snapshot!(registry_from_introspection(schema));
        });
    }
}
