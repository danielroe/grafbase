#![allow(dead_code)]

use crate::consts::AUTHORIZERS_DIRECTORY_NAME;
use crate::types::UdfKind;
use crate::{
    consts::{
        DATABASE_DIRECTORY, DOT_GRAFBASE_DIRECTORY, GRAFBASE_DIRECTORY_NAME, GRAFBASE_HOME, GRAFBASE_SCHEMA_FILE_NAME,
        GRAFBASE_TS_CONFIG_FILE_NAME, PACKAGE_JSON_DEV_DEPENDENCIES, PACKAGE_JSON_FILE_NAME, REGISTRY_FILE,
        RESOLVERS_DIRECTORY_NAME, WRANGLER_DIRECTORY_NAME,
    },
    errors::CommonError,
};
use serde_json::{Map, Value};
use std::fs::File;
use std::io::BufReader;
use std::{
    borrow::Cow,
    env, fs, io,
    path::{Path, PathBuf},
    sync::OnceLock,
};

#[derive(Debug)]
pub enum SchemaLocation {
    /// The path of `$PROJECT/grafbase/grafbase.config.ts`,
    /// if exits.
    TsConfig(PathBuf),
    /// The path of `$PROJECT/grafbase/schema.graphql`, the
    /// Grafbase schema, in the nearest ancestor directory
    /// with said directory and file
    Graphql(PathBuf),
}

/// Points to the location of the Grafabase schema file.
#[derive(Debug)]
pub struct GrafbaseSchemaPath {
    location: SchemaLocation,
}

impl GrafbaseSchemaPath {
    /// The location of the schema file.
    #[must_use]
    pub fn location(&self) -> &SchemaLocation {
        &self.location
    }

    fn ts_config(path: PathBuf) -> Self {
        Self {
            location: SchemaLocation::TsConfig(path),
        }
    }

    fn graphql(path: PathBuf) -> Self {
        Self {
            location: SchemaLocation::Graphql(path),
        }
    }

    fn parent(&self) -> Option<&Path> {
        self.path().parent()
    }

    #[must_use]
    pub fn path(&self) -> &Path {
        use SchemaLocation::{Graphql, TsConfig};
        match self.location() {
            TsConfig(path) | Graphql(path) => path,
        }
    }
}

#[derive(Debug)]
pub struct Warning {
    message: Cow<'static, str>,
    hint: Option<Cow<'static, str>>,
}

impl Warning {
    pub fn new(message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            message: message.into(),
            hint: None,
        }
    }

    #[must_use]
    pub fn with_hint(mut self, message: impl Into<Cow<'static, str>>) -> Self {
        self.hint = Some(message.into());
        self
    }

    #[must_use]
    pub fn message(&self) -> &str {
        &self.message
    }

    #[must_use]
    pub fn hint(&self) -> Option<&str> {
        self.hint.as_deref()
    }
}

#[derive(Debug)]
pub struct Project {
    /// the path of the (assumed) user project root (`$PROJECT`), the nearest ancestor directory
    /// with a `grafbase/schema.graphql` file
    pub path: PathBuf,
    /// the path of the Grafbase schema, in the nearest ancestor directory with
    /// said directory and file
    pub schema_path: GrafbaseSchemaPath,
    /// the path of `$PROJECT/.grafbase/`, the Grafbase local developer tool cache and database directory,
    /// in the nearest ancestor directory with `grafbase/schema.graphql`
    pub dot_grafbase_directory_path: PathBuf,
    /// the path of `$PROJECT/grafbase/`, the Grafbase schema directory in the nearest ancestor directory
    /// with `grafbase/schema.graphql`
    pub grafbase_directory_path: PathBuf,
    /// the path of `$PROJECT/.grafbase/registry.json`, the registry derived from `schema.graphql`,
    /// in the nearest ancestor directory with a `grabase/schema.graphql` file
    pub registry_path: PathBuf,
    /// the path within '$PROJECT/.grafbase' containing the database
    pub database_directory_path: PathBuf,
    /// the location of package.json either in '$PROJECT/grafbase' or '$PROJECT'
    pub package_json_path: Option<PathBuf>,
}

impl Project {
    /// the path of the directory containing the sources corresponding to the UDF type (resolvers, authorizers).
    #[must_use]
    pub fn udfs_source_path(&self, kind: UdfKind) -> std::path::PathBuf {
        let subdirectory_name = match kind {
            UdfKind::Resolver => RESOLVERS_DIRECTORY_NAME,
            UdfKind::Authorizer => AUTHORIZERS_DIRECTORY_NAME,
        };
        self.grafbase_directory_path.join(subdirectory_name)
    }

    /// the path of the directory containing the build artifacts corresponding to the UDF type (resolvers, authorizers).
    #[must_use]
    pub fn udfs_build_artifact_path(&self, kind: UdfKind) -> std::path::PathBuf {
        let subdirectory_name = match kind {
            UdfKind::Resolver => RESOLVERS_DIRECTORY_NAME,
            UdfKind::Authorizer => AUTHORIZERS_DIRECTORY_NAME,
        };
        self.dot_grafbase_directory_path.join(subdirectory_name)
    }

    // reads and deserializes to json the contents of `&self.registry_path` in a blocking fashion
    pub fn registry(&self) -> Result<Value, CommonError> {
        let file = File::open(&self.registry_path)
            .map_err(|err| CommonError::RegistryRead(self.registry_path.clone(), err))?;
        let reader = BufReader::new(file);
        serde_json::from_reader(reader)
            .map_err(|err| CommonError::RegistryDeserialization(self.registry_path.clone(), err))
    }
}

/// a static representation of the current environment
///
/// must be initialized before use
#[derive(Debug)]
pub struct Environment {
    /// data related to the current project
    pub project: Option<Project>,
    /// the path of `$HOME/.grafbase`, the user level local developer tool cache directory
    pub user_dot_grafbase_path: PathBuf,
    /// warnings when loading the environment
    pub warnings: Vec<Warning>,
    /// the path within `$HOME/.grafbase` where wrangler gets installed
    pub wrangler_installation_path: PathBuf,
}

/// static singleton for the environment struct
static ENVIRONMENT: OnceLock<Environment> = OnceLock::new();

#[must_use]
pub fn get_default_user_dot_grafbase_path() -> Option<PathBuf> {
    dirs::home_dir().map(|home| home.join(DOT_GRAFBASE_DIRECTORY))
}

pub fn get_user_dot_grafbase_path_from_env() -> Option<PathBuf> {
    env::var(GRAFBASE_HOME)
        .ok()
        .map(PathBuf::from)
        .map(|env_override| env_override.join(DOT_GRAFBASE_DIRECTORY))
}

pub fn get_user_dot_grafbase_path(r#override: Option<PathBuf>) -> Option<PathBuf> {
    r#override
        .map(|r#override| r#override.join(DOT_GRAFBASE_DIRECTORY))
        .or_else(get_user_dot_grafbase_path_from_env)
        .or_else(get_default_user_dot_grafbase_path)
}

impl Project {
    fn try_init(warnings: &mut Vec<Warning>) -> Result<Self, CommonError> {
        let schema_path = get_project_grafbase_path(warnings)?.ok_or(CommonError::FindGrafbaseDirectory)?;

        let grafbase_directory_path = schema_path
            .parent()
            .expect("the schema directory must have a parent by definiton")
            .to_path_buf();

        let path = grafbase_directory_path
            .parent()
            .expect("the grafbase directory must have a parent directory by definition")
            .to_path_buf();

        let dot_grafbase_directory_path = path.join(DOT_GRAFBASE_DIRECTORY);
        let registry_path = dot_grafbase_directory_path.join(REGISTRY_FILE);
        let database_directory_path = dot_grafbase_directory_path.join(DATABASE_DIRECTORY);
        let package_json_path = [grafbase_directory_path.as_path(), path.as_path()]
            .into_iter()
            .map(|candidate| candidate.join(PACKAGE_JSON_FILE_NAME))
            .find(|candidate| candidate.exists());

        Ok(Project {
            path,
            schema_path,
            dot_grafbase_directory_path,
            grafbase_directory_path,
            registry_path,
            database_directory_path,
            package_json_path,
        })
    }

    /// returns a reference to the static Project instance
    ///
    /// # Panics
    ///
    /// panics if the Environment object was not previously initialized using `Environment::try_init_with_project()`
    #[must_use]
    pub fn get() -> &'static Project {
        Environment::get()
            .project
            .as_ref()
            .expect("Environment must be initialized with try_init_with_project to access the project details")
    }
}

impl Environment {
    /// initializes the static Environment instance, including the current project details
    ///
    /// # Errors
    ///
    /// returns [`CommonError::FindGrafbaseDirectory`] if the grafbase directory path cannot be read
    ///
    /// returns [`CommonError::FindHomeDirectory`] if the home directory is not found
    pub fn try_init_with_project(home_override: Option<PathBuf>) -> Result<(), CommonError> {
        let mut warnings = Vec::new();

        let user_dot_grafbase_path = get_user_dot_grafbase_path(home_override).ok_or(CommonError::FindHomeDirectory)?;

        let wrangler_installation_path = user_dot_grafbase_path.join(WRANGLER_DIRECTORY_NAME);

        let project = Project::try_init(&mut warnings)?;

        ENVIRONMENT
            .set(Self {
                project: Some(project),
                user_dot_grafbase_path,
                warnings,
                wrangler_installation_path,
            })
            .expect("cannot set environment twice");

        Ok(())
    }

    /// initializes the static Environment instance, outside the context of a project
    ///
    /// # Errors
    ///
    /// returns [`CommonError::FindHomeDirectory`] if the home directory is not found
    pub fn try_init(home_override: Option<PathBuf>) -> Result<(), CommonError> {
        let user_dot_grafbase_path = get_user_dot_grafbase_path(home_override).ok_or(CommonError::FindHomeDirectory)?;

        let wrangler_installation_path = user_dot_grafbase_path.join(WRANGLER_DIRECTORY_NAME);

        ENVIRONMENT
            .set(Self {
                project: None,
                user_dot_grafbase_path,
                warnings: Vec::new(),
                wrangler_installation_path,
            })
            .expect("cannot set environment twice");

        Ok(())
    }

    /// returns a reference to the static Environment instance
    ///
    /// # Panics
    ///
    /// panics if the Environment object was not previously initialized using `Environment::try_init()`
    #[must_use]
    pub fn get() -> &'static Self {
        match ENVIRONMENT.get() {
            Some(environment) => environment,
            // must be initialized in `main`
            #[allow(clippy::panic)]
            None => panic!("the environment object is uninitialized"),
        }
    }
}

/// searches for the closest ancestor directory named "grafbase" which
/// contains either a "grafbase.config.ts" or a "schema.graphql" file. if
/// already inside a `grafbase` directory, looks for `schema.graphql` inside
/// the current ancestor as well
///
/// # Errors
///
/// returns [`CommonError::ReadCurrentDirectory`] if the current directory path cannot be read
fn get_project_grafbase_path(warnings: &mut Vec<Warning>) -> Result<Option<GrafbaseSchemaPath>, CommonError> {
    let path_to_file = env::current_dir()
        .map_err(|_| CommonError::ReadCurrentDirectory)?
        .ancestors()
        .find_map(|ancestor| {
            let mut path = PathBuf::from(ancestor);

            // if we're looking at a directory called `grafbase`,
            // also check for the file in the current directory
            let config = path
                .components()
                .next()
                .filter(|first| Path::new(&first) == PathBuf::from(GRAFBASE_DIRECTORY_NAME))
                .and_then(|_| find_grafbase_configuration(&path, warnings));

            if let Some(config) = config {
                return Some(config);
            }

            path.push(GRAFBASE_DIRECTORY_NAME);

            find_grafbase_configuration(&path, warnings)
        });

    Ok(path_to_file)
}

fn find_grafbase_configuration(path: &Path, warnings: &mut Vec<Warning>) -> Option<GrafbaseSchemaPath> {
    let ts_config = path.join(GRAFBASE_TS_CONFIG_FILE_NAME);
    let gql = path.join(GRAFBASE_SCHEMA_FILE_NAME);

    match (ts_config.is_file(), gql.is_file()) {
        (true, true) => {
            let warning = Warning::new("Found both grafbase.config.ts and schema.graphql files")
                .with_hint("Delete one of them to avoid conflicts");

            warnings.push(warning);

            Some(GrafbaseSchemaPath::ts_config(ts_config))
        }
        (true, false) => Some(GrafbaseSchemaPath::ts_config(ts_config)),
        (false, true) => Some(GrafbaseSchemaPath::graphql(gql)),
        (false, false) => None,
    }
}

pub fn add_dev_dependency_to_package_json(project_dir: &Path, package: &str, version: &str) -> Result<(), CommonError> {
    let package_json_path = project_dir.join(PACKAGE_JSON_FILE_NAME);

    let mut package_json = if package_json_path.exists() {
        let file = fs::File::open(&package_json_path).map_err(CommonError::AccessPackageJson)?;
        let Ok(Value::Object(package_json)) = serde_json::from_reader(&file) else {
            return Err(CommonError::AccessPackageJson(io::Error::new(
                io::ErrorKind::InvalidData,
                "the file is not a JSON object",
            )));
        };
        package_json
    } else {
        let name = project_dir
            .file_name()
            .map_or(Cow::Borrowed("grafbase-project"), std::ffi::OsStr::to_string_lossy);
        match serde_json::json!(
        {
          "name": name,
          "version": "1.0.0",
          "description": "",
          "main": "index.js",
          "scripts": {
            "test": "echo \"Error: no test specified\" && exit 1"
          },
          "keywords": [],
          "author": "",
          "license": "ISC",
        }
        ) {
            Value::Object(package_json) => package_json,
            _ => unreachable!("must be an object"),
        }
    };

    match package_json
        .entry(PACKAGE_JSON_DEV_DEPENDENCIES)
        .or_insert_with(|| Value::Object(Map::new()))
    {
        Value::Object(ref mut obj) if !obj.contains_key(package) => {
            obj.insert(package.to_string(), Value::String(version.to_string()));
        }
        Value::Object(_) => return Ok(()),
        _ => {
            return Err(CommonError::AccessPackageJson(io::Error::new(
                io::ErrorKind::InvalidData,
                "the devDependencies value is not an object",
            )));
        }
    }

    let file = fs::File::create(&package_json_path).map_err(CommonError::AccessPackageJson)?;
    serde_json::to_writer_pretty(&file, &package_json).map_err(CommonError::SerializePackageJson)?;

    Ok(())
}
