#![allow(dead_code)]
use reqwest::{header::HeaderMap, StatusCode};
use serde_json::json;
use std::{
    marker::PhantomData,
    thread::sleep,
    time::{Duration, SystemTime},
};

use crate::utils::consts::INTROSPECTION_QUERY;

pub struct Client {
    endpoint: String,
    playground_endpoint: String,
    client: reqwest::blocking::Client,
    headers: HeaderMap,
    snapshot: Option<String>,
}

#[allow(clippy::module_name_repetitions)]
#[derive(derive_builder::Builder, Clone, Copy)]
#[builder(build_fn(name = "build_fallible"))]
pub struct ClientOptions {
    #[builder(default = "5")]
    http_timeout: u64,
}

impl Default for ClientOptions {
    fn default() -> Self {
        ClientOptionsBuilder::default().build()
    }
}

impl ClientOptionsBuilder {
    pub fn build(&mut self) -> ClientOptions {
        self.build_fallible().unwrap()
    }
}

impl Client {
    pub fn new(endpoint: String, playground_endpoint: String, client_options: ClientOptions) -> Self {
        Self {
            endpoint,
            playground_endpoint,
            headers: HeaderMap::new(),
            client: reqwest::blocking::Client::builder()
                .connect_timeout(Duration::from_secs(1))
                .timeout(Duration::from_secs(client_options.http_timeout))
                .build()
                .unwrap(),
            snapshot: None,
        }
    }

    pub fn with_api_key(self) -> Self {
        self.with_header("x-api-key", "any")
    }

    pub fn with_header(mut self, key: &'static str, value: &str) -> Self {
        self.headers.insert(key, value.parse().unwrap());
        self
    }

    pub fn with_cleared_headers(mut self) -> Self {
        self.headers.clear();
        self
    }

    pub fn gql<Response>(&self, query: impl Into<String>) -> GqlRequestBuilder<Response>
    where
        Response: for<'de> serde::de::Deserialize<'de>,
    {
        let reqwest_builder = self.client.post(&self.endpoint).headers(self.headers.clone());

        GqlRequestBuilder {
            query: query.into(),
            variables: None,
            phantom: PhantomData,
            reqwest_builder,
            bearer: None,
        }
    }

    fn introspect(&self) -> String {
        self.client
            .post(&self.endpoint)
            .body(json!({"operationName":"IntrospectionQuery", "query": INTROSPECTION_QUERY}).to_string())
            .headers(self.headers.clone())
            .send()
            .unwrap()
            .text()
            .unwrap()
    }

    fn safe_introspect(&self) -> Option<String> {
        if let Ok(response) = self
            .client
            .post(&self.endpoint)
            .body(json!({"operationName":"IntrospectionQuery", "query": INTROSPECTION_QUERY}).to_string())
            .headers(self.headers.clone())
            .send()
        {
            if response.status() != StatusCode::SERVICE_UNAVAILABLE {
                if let Ok(text) = response.text() {
                    return Some(text);
                }
            }
        }

        None
    }

    /// # Panics
    ///
    /// panics if the set timeout is reached
    pub fn poll_endpoint(&self, timeout_secs: u64, interval_millis: u64) {
        let start = SystemTime::now();

        loop {
            let valid_response = self
                .client
                .head(&self.endpoint)
                .send()
                .is_ok_and(|response| response.status() != StatusCode::SERVICE_UNAVAILABLE);
            if valid_response {
                break;
            }

            assert!(start.elapsed().unwrap().as_secs() < timeout_secs, "timeout");

            sleep(Duration::from_millis(interval_millis));
        }
    }

    pub fn snapshot(&mut self) {
        self.snapshot = Some(self.introspect());
    }

    pub fn poll_endpoint_for_changes(&mut self, timeout_secs: u64, interval_millis: u64) {
        let start = SystemTime::now();

        loop {
            // panic if a snapshot was not taken
            let snapshot = self.snapshot.clone().unwrap();

            match self.safe_introspect() {
                Some(current) => {
                    if snapshot != current {
                        self.snapshot = Some(current);
                        break;
                    }
                }
                None => continue,
            };

            assert!(start.elapsed().unwrap().as_secs() < timeout_secs, "timeout");
            sleep(Duration::from_millis(interval_millis));
        }
    }

    pub fn get_playground_html(&self) -> String {
        self.client
            .get(&self.playground_endpoint)
            .send()
            .unwrap()
            .text()
            .unwrap()
    }
}

#[derive(serde::Serialize)]
#[must_use]
pub struct GqlRequestBuilder<Response> {
    // These two will be serialized into the request
    query: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    variables: Option<serde_json::Value>,

    // These won't
    #[serde(skip)]
    phantom: PhantomData<fn() -> Response>,
    #[serde(skip)]
    reqwest_builder: reqwest::blocking::RequestBuilder,
    #[serde(skip)]
    bearer: Option<String>,
}

impl<Response> GqlRequestBuilder<Response> {
    pub fn variables(mut self, variables: impl serde::Serialize) -> Self {
        self.variables = Some(serde_json::to_value(variables).expect("to be able to serialize variables"));
        self
    }

    pub fn bearer(mut self, token: &str) -> Self {
        self.bearer = Some(format!("Bearer {token}"));
        self
    }

    pub fn header(self, name: &str, value: &str) -> Self {
        let Self {
            bearer,
            phantom,
            query,
            mut reqwest_builder,
            variables,
        } = self;
        reqwest_builder = reqwest_builder.header(name, value);
        Self {
            query,
            variables,
            phantom,
            reqwest_builder,
            bearer,
        }
    }

    pub fn send(self) -> Response
    where
        Response: for<'de> serde::de::Deserialize<'de>,
    {
        let json = serde_json::to_value(&self).expect("to be able to serialize gql request");

        if let Some(bearer) = self.bearer {
            self.reqwest_builder.header("authorization", bearer)
        } else {
            self.reqwest_builder
        }
        .json(&json)
        .send()
        .unwrap()
        .json::<Response>()
        .unwrap()
    }
}
