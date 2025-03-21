use super::{log, query};
use crate::{
    registry::resolvers::{postgres::context::PostgresContext, ResolvedValue},
    Error,
};
use grafbase_sql_ast::renderer::{self, Renderer};
use postgres_connector_types::transport::TransportExt;

pub(crate) async fn execute(ctx: PostgresContext<'_>) -> Result<ResolvedValue, Error> {
    let (sql, params) = renderer::Postgres::build(query::delete::build(&ctx, ctx.filter()?)?);

    if ctx.mutation_is_returning() {
        let operation = ctx.transport().collect_query(&sql, params);
        let response = log::query(&ctx, &sql, operation).await?;
        let rows: Vec<_> = response.into_iter().map(|row| row.root).collect();
        let row_count = rows.len();

        Ok(ResolvedValue::new(serde_json::json!({
            "returning": rows,
            "rowCount": row_count,
        })))
    } else {
        let operation = ctx.transport().parameterized_execute(&sql, params);
        let row_count = log::execute(&ctx, &sql, operation).await?;

        Ok(ResolvedValue::new(serde_json::json!({
            "rowCount": row_count,
        })))
    }
}
