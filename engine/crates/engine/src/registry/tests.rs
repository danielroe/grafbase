use super::*;
use sha2::{Digest, Sha256};
use std::borrow::Cow;

const EXPECTED_SHA: &str = "d7e48694f72c0a6246eabb79bae2ce2aa44bf27f5e2b1e8b9e245fc7b5d08964";

#[test]
fn test_serde_roundtrip() {
    let id = r#"
            This test ensures the default `VersionedRegistry` serialization output remains stable.

            When this test fails, it likely means the shape of the `Registry` type was updated,
            which can cause backward-incompatibility issues.

            Before updating this test to match the expected result, please ensure the changes to
            `Registry` are applied in a backward compatible way.

            One way to do so, is to have the `Default` trait return a value that keeps the existing
            expectation, and `#[serde(default)]` is applied to any newly added field.

            Once you are satisfied your changes are backward-compatible, update `EXPECTED_SHA` with
            the new output presented in the test result.
        "#;

    let registry = Cow::Owned(Registry::new().with_sample_data());
    let versioned_registry = VersionedRegistry {
        registry,
        deployment_id: Cow::Borrowed(id),
    };
    let serialized_versioned_registry = serde_json::to_string(&versioned_registry).unwrap();
    let serialized_sha = Sha256::digest(serialized_versioned_registry);

    assert_eq!(&format!("{serialized_sha:x}"), EXPECTED_SHA);
}
