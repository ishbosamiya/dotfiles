use std::{
    fmt::Display,
    path::{Path, PathBuf},
    str::FromStr,
};

use clap::{ArgEnum, Parser};
use gpu_switcheroo::config::{self, Config};

fn main() {
    let args = Args::parse();

    sudo::escalate_if_needed().unwrap();

    println!(
        "info: attempting to read config from {}",
        config::project_config_file_path().to_str().unwrap()
    );

    let mut config = match Config::read() {
        Ok(config) => config,
        Err(err) => {
            if !config::project_config_file_path().exists() {
                println!("config file does not exist, creating new config");
                let config = Config::default();
                config.write().unwrap();
                config
            } else {
                eprintln!(
                    "error {} while attempting to read config, check {}",
                    err,
                    config::project_config_file_path().to_str().unwrap()
                );
                std::process::exit(-1);
            }
        }
    };

    if let Some(path) = args.set_nvidia_prime_select_path {
        config.nvidia_prime_select_path = Some(PathBuf::from_str(&path).unwrap());

        config.write().unwrap();
    }

    match args.switch_to_gpu.switch(&config) {
        Ok(_) => {
            println!("successfully switched to {:?} GPU", args.switch_to_gpu);
        }
        Err(err) => {
            eprintln!(
                "error while switching to {:?} GPU: {}",
                args.switch_to_gpu, err
            );
            eprintln!("see `gpu_switcheroo --help` for details on usage");
        }
    }
}

#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
    #[clap(long)]
    set_nvidia_prime_select_path: Option<String>,

    #[clap(arg_enum)]
    switch_to_gpu: GPU,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, ArgEnum)]
pub enum GPU {
    Intel,
    Optimus,
}

impl GPU {
    pub fn switch(&self, config: &Config) -> Result<(), Error> {
        let nvidia_prime_select_path = match &config.nvidia_prime_select_path {
            Some(path) => path,
            None => {
                return Err(Error::NvidiaPrimeSelectPathUnknown);
            }
        };

        if !nvidia_prime_select_path.is_dir() {
            return Err(Error::NvidiaPrimeSelectPathDoesNotExist);
        }

        match self {
            GPU::Intel => Self::switch_intel(&nvidia_prime_select_path),
            GPU::Optimus => Self::switch_nvidia(&nvidia_prime_select_path),
        }
    }

    fn switch_intel(nvidia_prime_select_path: impl AsRef<Path>) -> Result<(), Error> {
        // TODO: need to figure out a way to know if the sub process
        // failed.
        std::process::Command::new("make")
            .arg("install")
            .current_dir(nvidia_prime_select_path)
            .output()
            .unwrap();
        std::process::Command::new("nvidia-prime-select")
            .arg("intel")
            .output()
            .unwrap();

        Ok(())
    }

    fn switch_nvidia(nvidia_prime_select_path: impl AsRef<Path>) -> Result<(), Error> {
        // TODO: need to figure out a way to know if the sub process
        // failed.
        std::process::Command::new("make")
            .arg("uninstall")
            .current_dir(nvidia_prime_select_path)
            .output()
            .unwrap();

        Ok(())
    }
}

#[derive(Debug)]
pub enum Error {
    NvidiaPrimeSelectPathUnknown,
    NvidiaPrimeSelectPathDoesNotExist,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NvidiaPrimeSelectPathUnknown => write!(f, "nvidia-prime-select path unknown"),
            Error::NvidiaPrimeSelectPathDoesNotExist => {
                write!(f, "nvidia-prime-select path does not exist")
            }
        }
    }
}

impl std::error::Error for Error {}
