use std::{
    fmt::Display,
    fs,
    ops::RangeInclusive,
    path::{Path, PathBuf},
};

use directories::ProjectDirs;
use lazy_static::lazy_static;

lazy_static! {
    static ref PROJECT_DIRECTORIES: ProjectDirs =
        ProjectDirs::from("", "ishbosamiya", "gpu_switcheroo").unwrap();
    static ref PROJECT_CONFIG_DIR_PATH: &'static Path = {
        let path = PROJECT_DIRECTORIES.config_dir();
        if !path.exists() {
            fs::create_dir_all(path).unwrap();
        }
        path
    };
}

lazy_static! {
    static ref PROJECT_CONFIG_FILE_PATH: PathBuf = PROJECT_CONFIG_DIR_PATH.join("config.toml");
}

/// Current version of the config file that would be written.
const CURRENT_CONFIG_VERSION: u32 = 0;
/// The supported range of config file versions.
const CURRENT_SUPPORTED_VERSION_RANGE: RangeInclusive<u32> = 0..=0;

/// Project directories.
pub fn project_directories() -> &'static ProjectDirs {
    &PROJECT_DIRECTORIES
}

/// Path to project config directory.
pub fn project_config_dir_path() -> &'static Path {
    &PROJECT_CONFIG_DIR_PATH
}

/// Path to project config file.
pub fn project_config_file_path() -> &'static Path {
    &PROJECT_CONFIG_FILE_PATH
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct Config {
    config_version: u32,
    pub nvidia_prime_select_path: Option<PathBuf>,
}

impl Config {
    /// Create new config.
    pub fn new() -> Self {
        Self {
            config_version: CURRENT_CONFIG_VERSION,
            nvidia_prime_select_path: None,
        }
    }

    /// Write the config to disk. Location is determined by
    /// [`project_config_file_path()`].
    pub fn write(&self) -> Result<(), Error> {
        std::fs::write(project_config_file_path(), toml::to_string_pretty(self)?)?;
        Ok(())
    }

    /// Read the config from disk. Location is determined by
    /// [`project_config_file_path()`].
    pub fn read() -> Result<Self, Error> {
        let config_file_contents = std::fs::read(project_config_file_path())?;
        let config = toml::from_slice::<Config>(&config_file_contents)?;

        if !CURRENT_SUPPORTED_VERSION_RANGE.contains(&config.config_version) {
            return Err(Error::UnsupportedConfigVersion(
                config.config_version,
                CURRENT_SUPPORTED_VERSION_RANGE,
            ));
        }
        Ok(config)
    }
}

impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum Error {
    TomlSerialization(toml::ser::Error),
    TomlDeserialization(toml::de::Error),
    Io(std::io::Error),
    /// Unsupported config version (got version, supported versions)
    UnsupportedConfigVersion(u32, RangeInclusive<u32>),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::TomlSerialization(err) => write!(f, "toml serialization: {}", err),
            Error::TomlDeserialization(err) => write!(f, "toml deserialization: {}", err),
            Error::Io(err) => write!(f, "io: {}", err),
            Error::UnsupportedConfigVersion(got_version, supported_versions) => write!(
                f,
                "unsupported config version: {} must be in {}..={}",
                got_version,
                supported_versions.start(),
                supported_versions.end()
            ),
        }
    }
}

impl std::error::Error for Error {}

impl From<toml::ser::Error> for Error {
    fn from(err: toml::ser::Error) -> Self {
        Self::TomlSerialization(err)
    }
}

impl From<toml::de::Error> for Error {
    fn from(err: toml::de::Error) -> Self {
        Self::TomlDeserialization(err)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::Io(err)
    }
}
