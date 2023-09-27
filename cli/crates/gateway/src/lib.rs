use gateway_core::CacheConfig;
// Only included to force local feature
use log as _;
use runtime_noop::cache::NoopCache;
use std::{collections::HashMap, ops::Deref, sync::Arc};

use self::executor::Executor;

mod auth;
mod context;
mod error;
mod executor;
mod response;
mod serving;

pub(crate) use context::Context;
pub(crate) use error::Error;
pub(crate) use response::Response;
pub use runtime_local::Bridge;

pub type GatewayInner = gateway_core::Gateway<Executor, NoopCache<engine::Response>>;

#[derive(Clone)]
pub struct Gateway {
    inner: Arc<GatewayInner>,
}

impl Gateway {
    #[must_use]
    pub fn new(env_vars: HashMap<String, String>, bridge: Bridge, registry: Arc<engine::Registry>) -> Self {
        let cache_config: CacheConfig = CacheConfig::default();
        let authorizer = Box::new(auth::Authorizer {
            auth_config: registry.auth.clone(),
            bridge: bridge.clone(),
        });
        let executor = Arc::new(Executor::new(env_vars, bridge, registry));
        Gateway {
            inner: Arc::new(gateway_core::Gateway::new(
                executor,
                Arc::new(NoopCache::new()),
                cache_config,
                authorizer,
            )),
        }
    }

    pub fn into_router(self) -> axum::Router {
        serving::router(self)
    }
}

impl Deref for Gateway {
    type Target = GatewayInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
