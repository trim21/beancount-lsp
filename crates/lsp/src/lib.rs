use std::fs::OpenOptions;
use std::iter;
use std::path::PathBuf;

mod checkers;
mod indexer;
pub use crate::indexer::Indexer;
mod doc;
pub use crate::doc::{Document, build_document};
mod providers;
pub use crate::providers::account::account_at_position;
mod server;
#[cfg(test)]
pub mod test_utils;
mod text;

use clap::{Parser, ValueEnum, error::ErrorKind as ClapErrorKind};
use server::Backend;
use spdlog::sink::FileSink;
use spdlog::{Level, LevelFilter, Logger};
use tower_lsp_server::jsonrpc::{Error, Result};
use tower_lsp_server::{LspService, Server};

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum LogLevel {
  Error,
  Warn,
  Info,
  Debug,
  Trace,
}

impl LogLevel {
  fn to_spdlog_level_filter(self) -> LevelFilter {
    match self {
      LogLevel::Error => LevelFilter::MoreSevereEqual(Level::Error),
      LogLevel::Warn => LevelFilter::MoreSevereEqual(Level::Warn),
      LogLevel::Info => LevelFilter::MoreSevereEqual(Level::Info),
      LogLevel::Debug => LevelFilter::MoreSevereEqual(Level::Debug),
      LogLevel::Trace => LevelFilter::MoreSevereEqual(Level::Trace),
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
  match Cli::try_parse_from(
    iter::once(String::from("beancount-lsp")).chain(argv.iter().cloned()),
  ) {
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
  let level_filter = cli.log_level.to_spdlog_level_filter();

  if let Some(path) = &cli.log_file {
    let _ = OpenOptions::new()
      .create(true)
      .append(true)
      .open(path)
      .map_err(|err| {
        Error::invalid_params(format!(
          "failed to open log file {}: {err}",
          path.display()
        ))
      })?;

    let file_sink = FileSink::builder().path(path).build_arc().map_err(|err| {
      Error::invalid_params(format!(
        "failed to create file sink for {}: {err}",
        path.display()
      ))
    })?;
    let logger = Logger::builder()
      .sink(file_sink)
      .level_filter(level_filter)
      .build_arc()
      .map_err(|err| Error::invalid_params(format!("failed to init logging: {err}")))?;
    spdlog::set_default_logger(logger);
  } else {
    spdlog::default_logger().set_level_filter(level_filter);
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
  spdlog::info!(
    "logging initialized level={:?} destination={}",
    cli.log_level,
    log_dest
  );

  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let (service, socket) = LspService::build(Backend::new).finish();
  Server::new(stdin, stdout, socket).serve(service).await;

  Ok(())
}

pub fn parse_document_for_bench(text: &str, filename: &std::path::Path) {
  crate::indexer::Indexer::parse_document_for_bench(text, filename);
}

pub struct BenchDoc(crate::doc::Document);

impl BenchDoc {
  pub fn semantic_tokens_full(
    &self,
  ) -> Option<tower_lsp_server::ls_types::SemanticTokensResult> {
    crate::providers::semantic_tokens::semantic_tokens_full(&self.0)
  }
}

pub fn build_semantic_tokens_bench_doc(
  text: &str,
  filename: &std::path::Path,
) -> Option<BenchDoc> {
  let doc = crate::indexer::Indexer::parse_document(text, filename)?;
  Some(BenchDoc(doc))
}
