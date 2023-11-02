use crate::{cli_input::SchemaCommand, errors::CliError};

#[tokio::main]
pub(crate) async fn schema(cmd: SchemaCommand) -> Result<(), CliError> {
    let SchemaCommand {
        project_ref,
        subgraph_name,
    } = cmd;
    let schema = backend::api::schema::schema(
        project_ref.account(),
        project_ref.project(),
        project_ref.branch(),
        subgraph_name.as_deref(),
    )
    .await
    .map_err(CliError::BackendApiError)?;

    if let Some(schema) = schema {
        print!("{schema}")
    } else {
        eprintln!("🤲 Found no schema")
    }

    Ok(())
}
