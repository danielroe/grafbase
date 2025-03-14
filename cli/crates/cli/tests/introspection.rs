#![allow(unused_crate_dependencies)]
#![recursion_limit = "256"]

#[path = "graphql-directive/server.rs"]
mod server;

mod utils;

use serde_json::json;
use utils::environment::Environment;
use wiremock::{
    matchers::{body_json, header, method, path},
    Mock, MockServer, ResponseTemplate,
};

#[tokio::test(flavor = "multi_thread")]
async fn subgraph() {
    let env = Environment::init();
    let server = MockServer::start().await;

    let request = json!({
        "query": "query {\n  _service {\n    sdl\n  }\n}    \n",
        "variables": {}
    });

    let response = ResponseTemplate::new(200).set_body_json(json!({
        "data": {
            "_service": {
                "sdl": indoc::indoc! {r#"
                    type Test {
                      id: ID!
                    }
                "#}
            }
        }
    }));

    Mock::given(method("POST"))
        .and(path("/graphql"))
        .and(body_json(request))
        .respond_with(response)
        .mount(&server)
        .await;

    let address = server.address();
    let url = format!("http://localhost:{}/graphql", address.port());

    let output = env.grafbase_introspect(&url, &[]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);

    println!("{stderr}");
    assert!(output.stderr.is_empty());

    insta::assert_snapshot!(&stdout, @r###"
    type Test {
      id: ID!
    }

    "###);
}

#[tokio::test(flavor = "multi_thread")]
async fn header_no_whitespace() {
    let env = Environment::init();
    let server = MockServer::start().await;

    let request = json!({
        "query": "query {\n  _service {\n    sdl\n  }\n}    \n",
        "variables": {}
    });

    let response = ResponseTemplate::new(200).set_body_json(json!({
        "data": {
            "_service": {
                "sdl": indoc::indoc! {r#"
                    type Test {
                      id: ID!
                    }
                "#}
            }
        }
    }));

    Mock::given(method("POST"))
        .and(path("/graphql"))
        .and(body_json(request))
        .and(header("x-api-key", "foo"))
        .respond_with(response)
        .mount(&server)
        .await;

    let address = server.address();
    let url = format!("http://localhost:{}/graphql", address.port());

    let output = env.grafbase_introspect(&url, &["x-api-key:foo"]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);

    println!("{stderr}");
    assert!(output.stderr.is_empty());

    insta::assert_snapshot!(&stdout, @r###"
    type Test {
      id: ID!
    }

    "###);
}

#[tokio::test(flavor = "multi_thread")]
async fn header_with_whitespace() {
    let env = Environment::init();
    let server = MockServer::start().await;

    let request = json!({
        "query": "query {\n  _service {\n    sdl\n  }\n}    \n",
        "variables": {}
    });

    let response = ResponseTemplate::new(200).set_body_json(json!({
        "data": {
            "_service": {
                "sdl": indoc::indoc! {r#"
                    type Test {
                      id: ID!
                    }
                "#}
            }
        }
    }));

    Mock::given(method("POST"))
        .and(path("/graphql"))
        .and(body_json(request))
        .and(header("x-api-key", "foo"))
        .respond_with(response)
        .mount(&server)
        .await;

    let address = server.address();
    let url = format!("http://localhost:{}/graphql", address.port());

    let output = env.grafbase_introspect(&url, &["x-api-key: foo"]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);

    println!("{stderr}");
    assert!(output.stderr.is_empty());

    insta::assert_snapshot!(&stdout, @r###"
    type Test {
      id: ID!
    }

    "###);
}

#[tokio::test(flavor = "multi_thread")]
async fn two_headers() {
    let env = Environment::init();
    let server = MockServer::start().await;

    let request = json!({
        "query": "query {\n  _service {\n    sdl\n  }\n}    \n",
        "variables": {}
    });

    let response = ResponseTemplate::new(200).set_body_json(json!({
        "data": {
            "_service": {
                "sdl": indoc::indoc! {r#"
                    type Test {
                      id: ID!
                    }
                "#}
            }
        }
    }));

    Mock::given(method("POST"))
        .and(path("/graphql"))
        .and(body_json(request))
        .and(header("x-api-key", "foo"))
        .and(header("x-other-key", "bar"))
        .respond_with(response)
        .mount(&server)
        .await;

    let address = server.address();
    let url = format!("http://localhost:{}/graphql", address.port());

    let output = env.grafbase_introspect(&url, &["x-api-key: foo", "x-other-key: bar"]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);

    println!("{stderr}");
    assert!(output.stderr.is_empty());

    insta::assert_snapshot!(&stdout, @r###"
    type Test {
      id: ID!
    }

    "###);
}

#[tokio::test(flavor = "multi_thread")]
#[allow(clippy::too_many_lines)]
async fn standard() {
    let port = server::run().await;

    let env = Environment::init();

    let url = format!("http://localhost:{port}/");

    let output = env.grafbase_introspect(&url, &[]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);

    println!("{stderr}");
    assert!(output.stderr.is_empty());

    insta::assert_snapshot!(&stdout, @r###"
    type Bot {
      id: ID!
    }
    type Header {
      name: String!
      value: String!
    }
    type Issue implements PullRequestOrIssue {
      title: String!
      author: UserOrBot!
    }
    type PullRequest implements PullRequestOrIssue {
      title: String!
      checks: [String!]!
      author: UserOrBot!
    }
    type Query {
      serverVersion: String!
      pullRequestOrIssue(id: ID!): PullRequestOrIssue
      headers: [Header!]!
    }
    type User {
      name: String!
      email: String!
    }
    interface PullRequestOrIssue {
      title: String!
      author: UserOrBot!
    }
    union UserOrBot = User | Bot

    "###);
}
