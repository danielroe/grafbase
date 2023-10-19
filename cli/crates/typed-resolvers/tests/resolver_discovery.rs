#![allow(clippy::panic, unused_crate_dependencies)]

use std::{fs, path::Path, sync::OnceLock};

fn update_expect() -> bool {
    static UPDATE_EXPECT: OnceLock<bool> = OnceLock::new();
    *UPDATE_EXPECT.get_or_init(|| std::env::var("UPDATE_EXPECT").is_ok())
}

#[allow(clippy::unnecessary_wraps)] // we can't change the signature expected by datatest_stable
fn run_test(path: &Path) -> datatest_stable::Result<()> {
    let expected_file_path = path.with_file_name("expected.out");
    let expected = fs::read_to_string(&expected_file_path).unwrap_or_default();
    let actual = match typed_resolvers::generate_type_extensions_from_resolvers(path) {
        Ok(graphql) => graphql,
        Err(errs) => errs,
    };

    if expected == actual {
        return Ok(());
    }

    if update_expect() {
        std::fs::write(expected_file_path, &actual).unwrap();
        return Ok(());
    }

    panic!(
        "{}\n\n\n=== Hint: run the tests again with UPDATE_EXPECT=1 to update the snapshot. ===",
        similar::udiff::unified_diff(
            similar::Algorithm::default(),
            &expected,
            &actual,
            5,
            Some(("Expected", "Actual"))
        )
    );
}

datatest_stable::harness! { run_test, "./tests/resolver_discovery", r#"^.*\.graphql$"# }
