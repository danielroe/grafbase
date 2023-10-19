#![allow(clippy::panic, unused_crate_dependencies)]

use std::{fs, path::Path, sync::OnceLock};

fn update_expect() -> bool {
    static UPDATE_EXPECT: OnceLock<bool> = OnceLock::new();
    *UPDATE_EXPECT.get_or_init(|| std::env::var("UPDATE_EXPECT").is_ok())
}

#[allow(clippy::unnecessary_wraps)] // we can't change the signature expected by datatest_stable
fn run_test(path: &Path) -> datatest_stable::Result<()> {
    let graphql_schema = fs::read_to_string(path)?;
    let expected = fs::read_to_string(path.with_file_name("expected.out"))?;
    let actual = typed_resolvers::generate_type_extensions();

    
    panic!("failed {path:?}");
}

datatest_stable::harness! { run_test, "./tests/resolver_discovery", r#"^.*\.graphql$"# }
