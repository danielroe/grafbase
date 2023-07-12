#![allow(unused_crate_dependencies)]
mod utils;

use assert_matches::assert_matches;
use backend::project::ConfigType;
use serde_json::{json, Value};
use utils::consts::{DEFAULT_CREATE, DEFAULT_QUERY, DEFAULT_SCHEMA, DEFAULT_UPDATE};
use utils::environment::Environment;

#[test]
fn dev() {
    let mut env = Environment::init();
    env.grafbase_init(ConfigType::GraphQL);
    env.write_schema(DEFAULT_SCHEMA);
    env.grafbase_dev();
    let client = env.create_client().with_api_key();
    client.poll_endpoint(30, 300);

    //
    // CREATE
    //
    let response = client.gql::<Value>(DEFAULT_CREATE).send();
    let todo_list: Value = dot_get!(response, "data.todoListCreate.todoList");
    let todo_list_id: String = dot_get!(todo_list, "id");
    assert!(!todo_list_id.is_empty());
    assert_eq!(dot_get!(todo_list, "status", String), "BACKLOG");
    assert_eq!(dot_get!(todo_list, "title", String), "My todo list");

    let todos: Vec<Value> = dot_get!(todo_list, "todos.edges", Value)
        .as_array()
        .map(|array| {
            array
                .iter()
                .map(|element| dot_get!(element, "node", Value))
                .collect::<Vec<_>>()
        })
        .unwrap();
    assert_eq!(todos.len(), 2);
    assert_eq!(dot_get!(todos[0], "title", String), "My first todo!");
    assert!(dot_get!(todos[0], "complete", bool));
    assert_eq!(dot_get!(todos[1], "title", String), "My second todo!");
    assert!(!dot_get!(todos[1], "complete", bool));
    assert_ne!(dot_get!(todos[0], "id", String), dot_get!(todos[1], "id", String));

    //
    // QUERY
    //
    let response = client.gql::<Value>(DEFAULT_QUERY).send();
    let edges: Value = dot_get!(response, "data.todoListCollection.edges");
    assert_eq!(edges.as_array().map(Vec::len).unwrap(), 1);

    let query_todo_list: Value = dot_get!(edges, "0.node");
    assert_eq!(dot_get!(query_todo_list, "id", String), todo_list_id);
    assert_eq!(
        dot_get!(query_todo_list, "title", String),
        dot_get!(todo_list, "title", String)
    );

    let query_todo_list0: Value = dot_get!(query_todo_list, "todos.edges.0.node");
    assert_eq!(
        dot_get!(query_todo_list0, "id", String),
        dot_get!(todos[0], "id", String)
    );
    assert_eq!(
        dot_get!(query_todo_list0, "title", String),
        dot_get!(todos[0], "title", String)
    );

    let query_todo_list1: Value = dot_get!(query_todo_list, "todos.edges.1.node");
    assert_eq!(
        dot_get!(query_todo_list1, "id", String),
        dot_get!(todos[1], "id", String)
    );
    assert_eq!(
        dot_get!(query_todo_list1, "title", String),
        dot_get!(todos[1], "title", String)
    );

    //
    // UPDATE
    //
    let response = client
        .gql::<Value>(DEFAULT_UPDATE)
        .variables(json!({ "id": todo_list_id, "input": {
            "title": "Sweet and Sour",
            "tags": ["Plum"],
            "likes": { "set": 10 }
        }}))
        .send();
    let updated_todo_list: Value = dot_get!(response, "data.todoListUpdate.todoList");
    assert_eq!(dot_get!(updated_todo_list, "title", String), "Sweet and Sour");
    assert_eq!(dot_get!(updated_todo_list, "likes", i32), 10);
    assert_eq!(dot_get!(updated_todo_list, "tags", Vec<String>), vec!["Plum"]);

    let response = client
        .gql::<Value>(DEFAULT_UPDATE)
        .variables(json!({ "id": todo_list_id, "input": { "tags": Value::Null, "status": "IN_PROGRESS" } }))
        .send();
    let updated_todo_list: Value = dot_get!(response, "data.todoListUpdate.todoList");
    assert_eq!(dot_get!(updated_todo_list, "title", String), "Sweet and Sour");
    assert_eq!(dot_get!(updated_todo_list, "likes", i32), 10);
    assert_eq!(dot_get!(updated_todo_list, "status", String), "IN_PROGRESS");
    assert_eq!(dot_get_opt!(updated_todo_list, "tags", Vec<String>), None);
}

#[test]
fn resolver_crashing_on_spawn() {
    let mut env = Environment::init();
    env.grafbase_init(ConfigType::GraphQL);
    env.write_schema(
        r#"
            extend type Query {
                hello: String! @resolver(name: "hello")
            }
        "#,
    );
    env.write_resolver(
        "hello.js",
        r#"
            const userAgent = navigator.userAgent;
            export default function Resolver(parent, args, context, info) {
                return userAgent;
            }
        "#,
    );
    let pid = env.grafbase_dev();
    let client = env.create_client().with_api_key();
    client.poll_endpoint(30, 300);

    let mut response = client
        .gql::<Value>(
            r#"
                {
                    hello
                }
            "#,
        )
        .send();
    assert_matches!(response.get("data").filter(|value| !value.is_null()), None);
    assert_eq!(
        response
            .get_mut("errors")
            .and_then(|errors| errors.as_array_mut())
            .and_then(|errors| {
                assert_matches!(errors.as_slice(), [_]);
                errors.pop()
            }),
        Some(serde_json::json!({
            "path": vec!["hello"],
            "locations": vec![serde_json::json!({
                "line": 3,
                "column": 21
            })],
            "message": "Invocation failed."
        }))
    );

    let kill_thread = std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_secs(1));
        nix::sys::signal::kill(nix::unistd::Pid::from_raw(pid as i32), nix::sys::signal::Signal::SIGINT).unwrap();
    });

    let stderr = env.last_command_output(utils::environment::OutputType::Stdout);
    insta::assert_snapshot!(stderr);

    kill_thread.join().unwrap();
}
