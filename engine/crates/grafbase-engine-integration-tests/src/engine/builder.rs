use super::Inner;
use crate::Engine;
use grafbase_engine::{registry::resolvers::graphql::QueryBatcher, Schema};
use runtime::{
    udf::{CustomResolverRequestPayload, CustomResolversEngine, UdfInvoker},
    GraphqlRequestExecutionContext,
};
use postgresql_types::transport::NeonTransport;
use sdl_parser::{ConnectorParsers, GraphqlDirective, NeonDirective, OpenApiDirective, ParseResult, Registry};
use std::{collections::HashMap, sync::Arc};

#[must_use]
pub struct EngineBuilder {
    schema: String,
    openapi_specs: HashMap<String, String>,
    environment_variables: HashMap<String, String>,
    custom_resolvers: Option<CustomResolversEngine>,
}

impl EngineBuilder {
    pub fn new(schema: impl Into<String>) -> Self {
        EngineBuilder {
            schema: schema.into(),
            openapi_specs: HashMap::new(),
            environment_variables: HashMap::new(),
            custom_resolvers: None,
        }
    }

    pub fn with_openapi_schema(mut self, url: impl Into<String>, spec: impl Into<String>) -> Self {
        self.openapi_specs.insert(url.into(), spec.into());
        self
    }

    pub fn with_env_var(mut self, name: impl Into<String>, value: impl Into<String>) -> Self {
        self.environment_variables.insert(name.into(), value.into());
        self
    }

    pub fn with_custom_resolvers(
        self,
        invoker: impl UdfInvoker<CustomResolverRequestPayload> + Send + Sync + 'static,
    ) -> Self {
        Self {
            custom_resolvers: Some(CustomResolversEngine::new(Box::new(invoker))),
            ..self
        }
    }

    pub async fn build(self) -> Engine {
        let ParseResult { registry, .. } = sdl_parser::parse(&self.schema, &self.environment_variables, &self)
            .await
            .unwrap();

        let registry = serde_json::from_value(serde_json::to_value(registry).unwrap()).unwrap();

        let mut schema_builder =
            Schema::build(registry)
                .data(QueryBatcher::new())
                .data(GraphqlRequestExecutionContext {
                    ray_id: String::new(),
                    headers: Default::default(),
                });

        if let Some(custom_resolvers) = self.custom_resolvers {
            schema_builder = schema_builder.data(custom_resolvers);
        }

        let schema = schema_builder.finish();

        Engine {
            inner: Arc::new(Inner { schema }),
        }
    }
}

#[async_trait::async_trait]
impl ConnectorParsers for EngineBuilder {
    async fn fetch_and_parse_openapi(&self, directive: OpenApiDirective) -> Result<Registry, Vec<String>> {
        let url = directive.schema_url.clone();

        let spec = self
            .openapi_specs
            .get(&url)
            .unwrap_or_else(|| panic!("tried to test with an unexpected openapi url: {url}"));

        let mut registry = Registry::new();

        parser_openapi::parse_spec(
            spec.clone(),
            parser_openapi::Format::guess(None, &url),
            directive.into(),
            &mut registry,
        )
        .map_err(|errors| errors.into_iter().map(|error| error.to_string()).collect::<Vec<_>>())?;

        Ok(registry)
    }

    async fn fetch_and_parse_graphql(&self, _directive: GraphqlDirective) -> Result<Registry, Vec<String>> {
        todo!("someone should implement this sometime, similar to the above")
    }

    async fn fetch_and_parse_neon(&self, directive: &NeonDirective) -> Result<Registry, Vec<String>> {
        let transport = NeonTransport::new(directive.postgresql_url()).map_err(|error| vec![error.to_string()])?;

        parser_postgresql::introspect(&transport, directive.name(), directive.namespace())
            .await
            .map_err(|error| vec![error.to_string()])
    }
}
