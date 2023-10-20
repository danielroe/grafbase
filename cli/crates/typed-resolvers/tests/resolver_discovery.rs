#![allow(clippy::panic, unused_crate_dependencies)]

use std::{
    fmt::Write,
    fs,
    path::Path,
    sync::{Once, OnceLock},
};

fn update_expect() -> bool {
    static UPDATE_EXPECT: OnceLock<bool> = OnceLock::new();
    *UPDATE_EXPECT.get_or_init(|| std::env::var("UPDATE_EXPECT").is_ok())
}

fn init_miette() {
    static MIETTE_SETUP: Once = Once::new();
    MIETTE_SETUP.call_once(|| {
        miette::set_hook(Box::new(|_| {
            Box::new(
                miette::GraphicalReportHandler::new()
                    .with_theme(miette::GraphicalTheme::unicode_nocolor())
                    .with_links(false)
                    .with_urls(true),
            )
        }))
        .unwrap();
    });
}

#[allow(clippy::unnecessary_wraps)] // we can't change the signature expected by datatest_stable
fn run_test(graphql_file_path: &Path) -> datatest_stable::Result<()> {
    init_miette();

    let expected_file_path = graphql_file_path.with_file_name("expected.out");
    let expected = fs::read_to_string(&expected_file_path).unwrap_or_default();
    let actual = {
        let graphql_sdl = fs::read_to_string(&graphql_file_path).unwrap();
        let analyzed_schema = typed_resolvers::analyze_schema(&graphql_sdl).unwrap();
        let typed_resolvers::AnalyzedResolvers {
            type_extensions: mut actual,
            errs,
        } = typed_resolvers::generate_type_extensions_from_resolvers(
            &graphql_file_path.with_file_name("resolvers"),
            &analyzed_schema,
        );
        if !errs.is_empty() {
            actual.push_str("=== ERRORS ===\n");
            for err in errs {
                writeln!(&mut actual, "\n{err:?}").unwrap();
            }
        }
        actual
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
