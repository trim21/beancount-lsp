use std::fs::OpenOptions;
use std::iter;
use std::path::PathBuf;

mod checkers;
mod providers;
mod server;
mod text;

use clap::{Parser, ValueEnum, error::ErrorKind as ClapErrorKind};
use server::Backend;
use tower_lsp_server::jsonrpc::{Error, Result};
use tower_lsp_server::{LspService, Server};
use tracing::{Level, info};

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum LogLevel {
    Error,
    Warn,
    Info,
    Debug,
    Trace,
}

impl LogLevel {
    fn to_tracing_level(self) -> Level {
        match self {
            LogLevel::Error => Level::ERROR,
            LogLevel::Warn => Level::WARN,
            LogLevel::Info => Level::INFO,
            LogLevel::Debug => Level::DEBUG,
            LogLevel::Trace => Level::TRACE,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Parser)]
#[command(
    name = "beancount-lsp",
    version,
    about = "Beancount LSP server",
    bin_name = "beancount-lsp"
)]
struct Cli {
    /// No-op compatibility flag; stdio is always used.
    #[arg(long)]
    stdio: bool,

    /// Log level (error, warn, info, debug, trace).
    #[arg(long, value_enum, default_value_t = LogLevel::Info)]
    log_level: LogLevel,

    /// Log file path; defaults to stderr when omitted.
    #[arg(long)]
    log_file: Option<PathBuf>,
}

// Server implementation lives in server.rs; lib.rs keeps CLI parsing and bootstrapping only.

fn parse_args(argv: &[String]) -> Result<Option<Cli>> {
    match Cli::try_parse_from(iter::once(String::from("beancount-lsp")).chain(argv.iter().cloned()))
    {
        Ok(cli) => Ok(Some(cli)),
        Err(err) => match err.kind() {
            ClapErrorKind::DisplayHelp | ClapErrorKind::DisplayVersion => {
                // Clap already prints to stdout/stderr; treat as graceful exit.
                err.print().ok();
                Ok(None)
            }
            _ => Err(Error::invalid_params(err.to_string())),
        },
    }
}

fn init_logging(cli: &Cli) -> Result<()> {
    let level = cli.log_level.to_tracing_level();
    let base = tracing_subscriber::fmt()
        .with_max_level(level)
        .with_ansi(false)
        .with_writer(tracing_subscriber::fmt::writer::BoxMakeWriter::new(
            std::io::stderr,
        ));

    if let Some(path) = &cli.log_file {
        let file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .map_err(|e| {
                Error::invalid_params(format!("failed to open log file {}: {e}", path.display()))
            })?;

        base.with_writer(move || file.try_clone().expect("failed to clone log file handle"))
            .try_init()
            .map_err(|e| Error::invalid_params(format!("failed to init logging: {e}")))?;
    } else {
        base.try_init()
            .map_err(|e| Error::invalid_params(format!("failed to init logging: {e}")))?;
    }

    Ok(())
}

/// Run the async server on a fresh multi-threaded Tokio runtime.
/// This is useful for non-Rust embeddings (e.g., the Python wrapper) that need a blocking entrypoint.
pub fn run_server_blocking(argv: Vec<String>) -> Result<()> {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .map_err(|err| tower_lsp_server::jsonrpc::Error {
            code: tower_lsp_server::jsonrpc::ErrorCode::InternalError,
            message: format!("failed to start tokio runtime: {err}").into(),
            data: None,
        })?;

    rt.block_on(main(argv))
}

pub async fn main(argv: Vec<String>) -> Result<()> {
    let Some(cli) = parse_args(&argv)? else {
        return Ok(());
    };

    init_logging(&cli)?;

    let log_dest = cli
        .log_file
        .as_ref()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "stderr".to_owned());
    info!(level = ?cli.log_level, destination = %log_dest, "logging initialized");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(Backend::new).finish();
    Server::new(stdin, stdout, socket).serve(service).await;

    Ok(())
}
