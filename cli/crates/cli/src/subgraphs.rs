use crate::{cli_input::SubgraphsCommand, errors::CliError};

#[tokio::main]
pub(super) async fn subgraphs(cmd: SubgraphsCommand) -> Result<(), CliError> {
    let project_ref = cmd.project_ref;
    let subgraphs =
        backend::api::subgraphs::subgraphs(project_ref.account(), project_ref.project(), project_ref.branch())
            .await
            .map_err(CliError::BackendApiError)?;

    if subgraphs.is_empty() {
        println!("🈳 There are no published subgraphs in this branch\n");
        return Ok(());
    }

    println!("👑 Found the subgraphs 👑\n");

    let emojis = ["🕊️🛖🧗🌠🎐🌌🗺️🧘🔆🪁"];

    for (subgraph, emoji) in subgraphs.iter().zip(emojis.iter().cycle()) {
        let name = &subgraph.name;
        println!("- {emoji} {name}");
    }

    println!();

    Ok(())
}
