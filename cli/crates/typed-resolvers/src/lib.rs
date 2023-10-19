#![allow(unused_crate_dependencies)]

mod analyze;
mod codegen;
mod error;
mod type_extensions_from_resolver;

use self::error::CodegenError;
use std::{ffi, fmt, path::Path};

/// Generate a TypeScript module that contains input and output type definitions for resolver
/// authoring purposes, based on the passed in SDL schema.
pub fn generate_ts_resolver_types<O>(graphql_sdl: &str, out: &mut O) -> Result<(), CodegenError>
where
    O: fmt::Write,
{
    let parsed_schema = graphql_parser::parse_schema::<&str>(graphql_sdl)?;
    let analyzed_schema = analyze::analyze(&parsed_schema);
    codegen::generate_module(&analyzed_schema, out)?;
    Ok(())
}

/// Returns either a GraphQL SDL string that defines the resolvers as type extensions, or errors.
pub fn generate_type_extensions_from_resolvers(resolvers_root: &Path) -> Result<String, String> {
    let mut out = String::new();
    let mut errs = String::new();

    for entry in walkdir::WalkDir::new(resolvers_root)
        .into_iter()
        .filter_map(|entry| entry.ok())
    {
        if entry.path().extension() != Some(ffi::OsStr::new("ts")) {
            continue;
        }

        type_extensions_from_resolver::object_extension_for_resolver(entry.path(), &mut out, &mut errs);
    }

    match errs.as_str() {
        "" => Ok(out),
        _ => Err(errs),
    }
}
