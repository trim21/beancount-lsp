use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::anyhow;
use beancount_core::ParseError;
use beancount_core::inference::InferenceError;
use serde::Deserialize;
use tokio::sync::{Mutex, RwLock, mpsc, watch};
use tokio::task;
use tower_lsp_server::jsonrpc::{Error, Result};
use tower_lsp_server::ls_types::{
  ClientCapabilities, CompletionOptions, CompletionParams, CompletionResponse,
  Diagnostic, DiagnosticOptions, DiagnosticServerCapabilities, DiagnosticSeverity,
  DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
  DidSaveTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport,
  DocumentDiagnosticReportResult, FullDocumentDiagnosticReport, GotoDefinitionParams,
  GotoDefinitionResponse, Hover, HoverParams, HoverProviderCapability,
  InitializeParams, InitializeResult, InitializedParams, MessageType, Position, Range,
  RelatedFullDocumentDiagnosticReport, SaveOptions, SemanticTokensFullOptions,
  SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
  SemanticTokensServerCapabilities, ServerCapabilities, ServerInfo,
  TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
  TextDocumentSyncSaveOptions, Uri as Url, WorkspaceDiagnosticParams,
  WorkspaceDiagnosticReport, WorkspaceDiagnosticReportResult,
  WorkspaceDocumentDiagnosticReport, WorkspaceFullDocumentDiagnosticReport,
};
use tower_lsp_server::{Client, LanguageServer};

use crate::checkers::{self, Checker, CheckerDiagnostic};
use crate::indexer::{Indexer, canonical_or_original};
use crate::providers::definition;
use crate::providers::{completion, hover, semantic_tokens};

pub(crate) fn documents_bfs(
  documents: &HashMap<Url, Arc<Document>>,
  root: &Url,
) -> Vec<(Url, Arc<Document>)> {
  spdlog::debug!(
    "keys: {:?}",
    documents.keys().map(|f| f.to_string()).collect::<Vec<_>>()
  );
  spdlog::debug!("root: {:?}", root.to_string());

  let mut ordered = Vec::new();
  let mut queue: VecDeque<Url> = VecDeque::new();
  let mut visited: HashSet<String> = HashSet::new();

  let Some(root_path) = root.to_file_path() else {
    return ordered;
  };
  let root_key = Indexer::normalized_path_key(root_path.as_ref());
  if !visited.insert(root_key) {
    return ordered;
  }
  queue.push_back(root.clone());

  while let Some(uri) = queue.pop_front() {
    let Some(doc) = documents.get(&uri) else {
      continue;
    };
    ordered.push((uri.clone(), Arc::clone(doc)));

    for include in &doc.includes {
      let path = Path::new(include);
      let canonical = canonical_or_original(path);
      let child_key = Indexer::normalized_path_key(canonical.as_ref());
      if !visited.insert(child_key) {
        continue;
      }
      if let Some(child_uri) = Url::from_file_path(&canonical) {
        queue.push_back(child_uri);
      } else {
        spdlog::warn!(
          "failed to convert include path to URI file={} ",
          canonical.display()
        );
      }
    }
  }

  ordered
}

fn normalize_uri(uri: &Url) -> Url {
  let Some(path) = uri.to_file_path() else {
    return uri.clone();
  };

  let canonical = canonical_or_original(path.as_ref());
  Url::from_file_path(canonical).unwrap_or_else(|| uri.clone())
}

fn resolve_document_from_snapshot(
  documents: &HashMap<Url, Arc<Document>>,
  uri: &Url,
) -> Option<Arc<Document>> {
  if let Some(doc) = find_document(documents, uri) {
    return Some(doc);
  }

  let path = uri.to_file_path()?;
  let content = fs::read_to_string(path.as_ref()).ok()?;
  Indexer::parse_document(&content, path.as_ref()).map(Arc::new)
}

async fn reparse_document_from_disk_async(uri: Url) -> Option<Arc<Document>> {
  let path: PathBuf = uri.to_file_path()?.to_path_buf();

  let content = tokio::fs::read_to_string(&path).await.ok()?;

  task::spawn_blocking(move || {
    Indexer::parse_document(&content, path.as_ref()).map(Arc::new)
  })
  .await
  .ok()?
}

async fn parse_document_from_text_async(
  uri: Url,
  text: Arc<String>,
) -> Option<Arc<Document>> {
  let path: PathBuf = uri.to_file_path()?.to_path_buf();

  task::spawn_blocking(move || {
    Indexer::parse_document(text.as_str(), path.as_ref()).map(Arc::new)
  })
  .await
  .ok()?
}

pub type Document = crate::doc::Document;
pub(crate) use crate::doc::find_document;

#[derive(Clone)]
pub struct Backend {
  client: Client,
  inner: Arc<RwLock<Option<InnerBackend>>>, // initialized state
  documents_text: Arc<RwLock<HashMap<Url, Arc<String>>>>,
  checker: Option<Arc<dyn Checker>>,
  // Desired checker generation; a bump asks the worker for a new run.
  checker_wanted_tx: watch::Sender<u64>,
  checker_wanted_rx: Arc<Mutex<Option<watch::Receiver<u64>>>>,
  checker_results_tx: watch::Sender<Arc<CheckerResults>>,
  checker_results_rx: watch::Receiver<Arc<CheckerResults>>,
  // Last diagnostics handed to a push-mode client, so only changes are re-published.
  published: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>>,
  indexer_tx: mpsc::UnboundedSender<IndexerCommand>,
  indexer_rx: Arc<Mutex<Option<mpsc::UnboundedReceiver<IndexerCommand>>>>,
}

struct InnerBackend {
  snapshot_rx: watch::Receiver<Arc<HashMap<Url, Arc<Document>>>>,
  root_uri: Option<Url>,
  pull_diagnostics: bool,
}

/// Output of the ledger-wide checker, tagged with the generation it was requested for.
#[derive(Default)]
struct CheckerResults {
  generation: u64,
  by_uri: HashMap<Url, Vec<Diagnostic>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct InitializeConfig {
  pub root_file: Option<PathBuf>,
}

enum IndexerCommand {
  Update { uri: Url, text: String },
}

fn checker_diagnostic_to_lsp(diag: CheckerDiagnostic, source: &str) -> Diagnostic {
  let line = diag.lineno.unwrap_or(0);
  let message = diag.message;

  Diagnostic {
    range: Range {
      start: Position::new(line, 0),
      end: Position::new(line, u32::MAX),
    },
    severity: Some(DiagnosticSeverity::ERROR),
    source: Some(format!("beancount ({source})")),
    message,
    ..Diagnostic::default()
  }
}

/// Attribute checker diagnostics to the file they were reported for. Beancount does not always
/// know which file an error came from, so those fall back to the journal root.
fn checker_diagnostics_by_uri(
  diags: Vec<CheckerDiagnostic>,
  source: &str,
  root_uri: &Url,
) -> HashMap<Url, Vec<Diagnostic>> {
  let mut by_uri: HashMap<Url, Vec<Diagnostic>> = HashMap::new();

  for diag in diags {
    let target_uri = diag
      .filename
      .as_deref()
      .and_then(|name| Url::from_file_path(canonical_or_original(Path::new(name))))
      .unwrap_or_else(|| root_uri.clone());

    by_uri
      .entry(target_uri)
      .or_default()
      .push(checker_diagnostic_to_lsp(diag, source));
  }

  by_uri
}

fn parse_inference_error_to_lsp(err: &InferenceError) -> Diagnostic {
  let line = err.line.saturating_sub(1) as u32;
  let col = err.column.saturating_sub(1) as u32;

  Diagnostic {
    range: Range {
      start: Position::new(line, col),
      end: Position::new(line, col.saturating_add(1)),
    },
    severity: Some(DiagnosticSeverity::ERROR),
    source: Some("beancount (inference)".to_string()),
    message: err.message.clone(),
    ..Diagnostic::default()
  }
}

fn parse_error_to_lsp(err: &ParseError) -> Diagnostic {
  let line = err.line.saturating_sub(1) as u32;
  let col = err.column.saturating_sub(1) as u32;

  Diagnostic {
    range: Range {
      start: Position::new(line, col),
      end: Position::new(line, col.saturating_add(1)),
    },
    severity: Some(DiagnosticSeverity::ERROR),
    source: Some("beancount (parser)".to_string()),
    message: err.message.clone(),
    ..Diagnostic::default()
  }
}

fn document_diagnostics(doc: &Document) -> Vec<Diagnostic> {
  let mut items = Vec::new();
  items.extend(doc.errors.iter().map(parse_error_to_lsp));
  items.extend(
    doc
      .inferrence_errors
      .iter()
      .map(parse_inference_error_to_lsp),
  );
  items
}

fn merge_diagnostics(
  doc: Option<&Document>,
  checker: Option<&[Diagnostic]>,
) -> Vec<Diagnostic> {
  let mut items = doc.map(document_diagnostics).unwrap_or_default();
  if let Some(checker) = checker {
    items.extend(checker.iter().cloned());
  }
  items
}

/// Pull diagnostics are negotiated through capabilities: only clients that declare
/// `textDocument/diagnostic` support get `diagnosticProvider`, everything else keeps
/// receiving pushed diagnostics.
fn client_supports_pull_diagnostics(capabilities: &ClientCapabilities) -> bool {
  capabilities
    .text_document
    .as_ref()
    .and_then(|capability| capability.diagnostic.as_ref())
    .is_some()
}

/// Checker output covering the requested generation. Waiting here is what makes a pull
/// request block until the ledger check it depends on has finished; concurrent pulls all wait
/// on the same generation, so the checker still runs once per save.
async fn wait_for_checker_results(
  wanted: u64,
  results_rx: &watch::Receiver<Arc<CheckerResults>>,
) -> Arc<CheckerResults> {
  let current = Arc::clone(&results_rx.borrow());
  if wanted == 0 || current.generation >= wanted {
    return current;
  }

  let mut results_rx = results_rx.clone();
  match results_rx
    .wait_for(|results| results.generation >= wanted)
    .await
  {
    Ok(results) => Arc::clone(&results),
    // The checker worker is gone, answer with whatever completed last.
    Err(_) => current,
  }
}

pub fn parse_initialize_config(params: &InitializeParams) -> Result<InitializeConfig> {
  let value = params
    .initialization_options
    .as_ref()
    .ok_or_else(|| Error::invalid_params("missing initializationOptions"))?;

  spdlog::debug!("init opts: {}", value.clone());

  serde_json::from_value(value.clone()).map_err(|err| {
    Error::invalid_params(format!("invalid initializationOptions: {err}"))
  })
}

impl Backend {
  pub fn new(client: Client) -> Self {
    let (checker_wanted_tx, checker_wanted_rx) = watch::channel(0u64);
    let (checker_results_tx, checker_results_rx) =
      watch::channel(Arc::new(CheckerResults::default()));
    let (indexer_tx, indexer_rx) = mpsc::unbounded_channel();

    Self {
      client,
      inner: Arc::new(RwLock::new(None)),
      documents_text: Arc::new(RwLock::new(HashMap::new())),
      checker: checkers::create().map(Arc::<dyn Checker>::from),
      checker_wanted_tx,
      checker_wanted_rx: Arc::new(Mutex::new(Some(checker_wanted_rx))),
      checker_results_tx,
      checker_results_rx,
      published: Arc::new(Mutex::new(HashMap::new())),
      indexer_tx,
      indexer_rx: Arc::new(Mutex::new(Some(indexer_rx))),
    }
  }

  /// Ask the checker worker for a run against the current on-disk journal. Only called
  /// when the journal on disk may have changed, because the checker never sees unsaved
  /// buffer content.
  async fn enqueue_checker_run(&self) {
    let initialized = self
      .inner
      .read()
      .await
      .as_ref()
      .and_then(|inner| inner.root_uri.as_ref())
      .is_some();
    if !initialized {
      return;
    }

    let next = *self.checker_wanted_tx.borrow() + 1;
    self.checker_wanted_tx.send_replace(next);
  }

  fn spawn_checker_worker(
    &self,
    root_file: PathBuf,
    mut wanted_rx: watch::Receiver<u64>,
  ) {
    let this = self.clone();

    let root_uri: Url = Url::from_file_path(&root_file)
      .ok_or_else(|| {
        anyhow!(
          "failed to convert root file to URI: {}",
          root_file.display()
        )
      })
      .unwrap();

    tokio::spawn(async move {
      while wanted_rx.changed().await.is_ok() {
        let generation = *wanted_rx.borrow_and_update();
        let by_uri = this.run_checker(&root_uri, &root_file).await;
        let _ = this
          .checker_results_tx
          .send(Arc::new(CheckerResults { generation, by_uri }));

        if this.uses_pull_diagnostics().await {
          // The results changed underneath the client, ask it to pull again.
          if let Err(err) = this.client.workspace_diagnostic_refresh().await {
            spdlog::debug!("client does not accept diagnostic refresh: {}", err);
          }
        } else {
          this.refresh_diagnostics().await;
        }
      }
    });
  }

  async fn uses_pull_diagnostics(&self) -> bool {
    self
      .inner
      .read()
      .await
      .as_ref()
      .map(|inner| inner.pull_diagnostics)
      .unwrap_or(false)
  }

  async fn snapshot(&self) -> Option<Arc<HashMap<Url, Arc<Document>>>> {
    self
      .inner
      .read()
      .await
      .as_ref()
      .map(|inner| inner.snapshot_rx.borrow().clone())
  }

  /// Checker output covering the generation requested so far.
  async fn checker_results(&self) -> Arc<CheckerResults> {
    let wanted = *self.checker_wanted_tx.borrow();
    wait_for_checker_results(wanted, &self.checker_results_rx).await
  }

  fn spawn_indexer_worker(
    &self,
    mut indexer: Indexer,
    mut rx: mpsc::UnboundedReceiver<IndexerCommand>,
    snapshot_tx: watch::Sender<Arc<HashMap<Url, Arc<Document>>>>,
  ) {
    let this = self.clone();

    tokio::spawn(async move {
      let mut pending: HashMap<Url, String> = HashMap::new();

      while let Some(cmd) = rx.recv().await {
        match cmd {
          IndexerCommand::Update { uri, text } => {
            pending.insert(uri, text);
          }
        }

        while let Ok(cmd) = rx.try_recv() {
          match cmd {
            IndexerCommand::Update { uri, text } => {
              pending.insert(uri, text);
            }
          }
        }

        let mut updated_any = false;
        for (uri, text) in pending.drain() {
          if indexer.update_document(uri, text) {
            updated_any = true;
          }
        }

        if updated_any {
          let snapshot = Arc::new(indexer.documents().clone());
          let _ = snapshot_tx.send(snapshot);
          // Push-mode clients have no other way to see buffer diagnostics.
          this.refresh_diagnostics().await;
        }
      }
    });
  }

  /// Run the ledger-wide checker and map its diagnostics back to the files they belong to.
  async fn run_checker(
    &self,
    root_uri: &Url,
    root_file: &Path,
  ) -> HashMap<Url, Vec<Diagnostic>> {
    let root_path = canonical_or_original(root_file);
    let root_file_str = root_path.display().to_string();

    let Some(checker) = self.checker.as_ref().map(Arc::clone) else {
      return HashMap::new();
    };

    let result = task::spawn_blocking(move || {
      let name = checker.name();
      spdlog::info!("run checker: {}", name);
      (name, checker.run(&root_file_str))
    })
    .await;

    match result {
      Ok((source, Ok(diags))) => checker_diagnostics_by_uri(diags, source, root_uri),
      Ok((name, Err(err))) => {
        spdlog::warn!("failed to run checker: {}", err);
        self
          .client
          .log_message(MessageType::ERROR, format!("{name} checker failed: {err}"))
          .await;
        HashMap::new()
      }
      Err(err) => {
        self
          .client
          .log_message(MessageType::ERROR, format!("checker task panicked: {err}"))
          .await;
        HashMap::new()
      }
    }
  }

  /// Push-mode delivery for clients without pull diagnostics support. Diagnostics for every
  /// known file are recomputed and only the ones that changed are sent, so typing does not
  /// re-publish the whole ledger.
  async fn refresh_diagnostics(&self) {
    if self.uses_pull_diagnostics().await {
      return;
    }

    let snapshot = self.snapshot().await.unwrap_or_default();
    // Never wait for a pending run here: buffer diagnostics must show up immediately, and the
    // checker worker publishes again once its own results land.
    let checker = Arc::clone(&self.checker_results_rx.borrow());
    let mut current: HashMap<Url, Vec<Diagnostic>> = HashMap::new();

    for (uri, doc) in snapshot.iter() {
      current
        .entry(uri.clone())
        .or_default()
        .extend(document_diagnostics(doc.as_ref()));
    }

    for (uri, diags) in &checker.by_uri {
      current
        .entry(uri.clone())
        .or_default()
        .extend(diags.iter().cloned());
    }

    let mut published = self.published.lock().await;

    // Clear diagnostics that disappeared so the client drops them.
    let stale = published
      .keys()
      .filter(|uri| !current.contains_key(*uri))
      .cloned()
      .collect::<Vec<_>>();
    for uri in stale {
      self
        .client
        .publish_diagnostics(uri.clone(), Vec::new(), None)
        .await;
      published.remove(&uri);
    }

    for (uri, diags) in &current {
      match published.get(uri) {
        // Nothing was published for this file yet and there is still nothing to show.
        None if diags.is_empty() => {}
        Some(previous) if previous == diags => {}
        _ => {
          self
            .client
            .publish_diagnostics(uri.clone(), diags.clone(), None)
            .await;
        }
      }
    }

    *published = current;
  }

  /// Diagnostics for one document, computed from the freshest text available: the checker
  /// results always describe the on-disk journal, so the buffer is re-parsed here instead of
  /// being read back from the indexer snapshot, which may not have caught up yet.
  async fn document_diagnostics_for(&self, uri: &Url) -> Vec<Diagnostic> {
    let doc = self.document_for_diagnostics(uri).await;
    let checker = self.checker_results().await;
    let checker = checker.by_uri.get(uri).map(Vec::as_slice);

    merge_diagnostics(doc.as_deref(), checker)
  }

  async fn document_for_diagnostics(&self, uri: &Url) -> Option<Arc<Document>> {
    let text = self.documents_text.read().await.get(uri).cloned();
    if let Some(text) = text {
      return parse_document_from_text_async(uri.clone(), text).await;
    }

    if let Some(doc) = reparse_document_from_disk_async(uri.clone()).await {
      return Some(doc);
    }

    self
      .snapshot()
      .await
      .and_then(|snapshot| find_document(snapshot.as_ref(), uri))
  }

  async fn with_inner<R>(&self, f: impl FnOnce(&InnerBackend) -> R) -> Result<R> {
    let guard = self.inner.read().await;
    let inner = guard.as_ref().ok_or_else(Error::invalid_request)?;
    Ok(f(inner))
  }

  async fn update_document(&self, uri: Url, text: String) {
    let normalized_uri = normalize_uri(&uri);
    {
      let mut guard = self.documents_text.write().await;
      guard.insert(normalized_uri.clone(), Arc::new(text.clone()));
    }
    let _ = self.indexer_tx.send(IndexerCommand::Update {
      uri: normalized_uri,
      text,
    });
  }
}

impl LanguageServer for Backend {
  async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
    let config = parse_initialize_config(&params)?;

    if let Some(root_file) = &config.root_file {
      spdlog::info!(
        "received initialization config root_file={}",
        root_file.display()
      );
    } else {
      spdlog::info!("received initialization config without root_file");
    }

    let root_path = config
      .root_file
      .as_ref()
      .map(|path| canonical_or_original(path));
    let root_uri = root_path.as_ref().and_then(Url::from_file_path);
    let pull_diagnostics = client_supports_pull_diagnostics(&params.capabilities);
    {
      let mut guard = self.inner.write().await;
      if guard.is_some() {
        return Err(Error::invalid_params("initializationOptions already set"));
      }

      let indexer = match root_path.as_ref() {
        Some(path) => Indexer::from_journal(path).unwrap_or_else(|e| {
          spdlog::warn!("failed to load journal tree: {e}");
          Indexer::new()
        }),
        None => Indexer::new(),
      };

      let initial_snapshot = Arc::new(indexer.documents().clone());
      log_collected_accounts(&initial_snapshot);
      let (snapshot_tx, snapshot_rx) = watch::channel(initial_snapshot);

      *guard = Some(InnerBackend {
        snapshot_rx,
        root_uri,
        pull_diagnostics,
      });

      if let Some(rx) = self.indexer_rx.lock().await.take() {
        self.spawn_indexer_worker(indexer, rx, snapshot_tx);
      }
    }

    // Start checker worker after initialization so the root file path is known.
    if let (Some(root_path), Some(rx)) =
      (root_path, self.checker_wanted_rx.lock().await.take())
    {
      self.spawn_checker_worker(root_path, rx);
    }

    let text_document_sync =
      TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
        open_close: Some(true),
        change: Some(TextDocumentSyncKind::FULL),
        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
          include_text: Some(true),
        })),
        ..TextDocumentSyncOptions::default()
      });

    let semantic_tokens =
      SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
        work_done_progress_options:
          tower_lsp_server::ls_types::WorkDoneProgressOptions::default(),
        legend: semantic_tokens::legend(),
        range: None,
        full: Some(SemanticTokensFullOptions::Bool(true)),
      });

    let diagnostic_provider = pull_diagnostics.then(|| {
      DiagnosticServerCapabilities::Options(DiagnosticOptions {
        identifier: Some("beancount".to_owned()),
        // Editing one journal file changes diagnostics of every file it includes.
        inter_file_dependencies: true,
        workspace_diagnostics: true,
        ..DiagnosticOptions::default()
      })
    });

    Ok(InitializeResult {
      capabilities: ServerCapabilities {
        text_document_sync: Some(text_document_sync),
        completion_provider: Some(CompletionOptions {
          resolve_provider: Some(false),
          trigger_characters: Some({
            let mut chars = vec![
              ":".to_string(),
              "#".to_string(),
              "^".to_string(),
              " ".to_string(),
            ];
            chars.extend(('0'..='9').map(|ch| ch.to_string()));
            chars.extend(('a'..='z').map(|ch| ch.to_string()));
            chars.extend(('A'..='Z').map(|ch| ch.to_string()));
            chars
          }),
          work_done_progress_options: Default::default(),
          all_commit_characters: None,
          completion_item: None,
          ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(tower_lsp_server::ls_types::OneOf::Left(true)),
        semantic_tokens_provider: Some(semantic_tokens),
        diagnostic_provider,
        ..Default::default()
      },
      server_info: Some(ServerInfo {
        name: "beancount-lsp".to_owned(),
        version: None,
      }),
      ..Default::default()
    })
  }

  async fn initialized(&self, _: InitializedParams) {
    self
      .client
      .log_message(MessageType::INFO, "beancount-lsp initialized")
      .await;

    // Run checker once after initialization without blocking this handler.
    self.enqueue_checker_run().await;
  }

  async fn shutdown(&self) -> Result<()> {
    Ok(())
  }

  async fn did_open(&self, params: DidOpenTextDocumentParams) {
    self
      .update_document(params.text_document.uri, params.text_document.text)
      .await;
  }

  async fn did_close(&self, params: DidCloseTextDocumentParams) {
    let normalized_uri = normalize_uri(&params.text_document.uri);
    let mut guard = self.documents_text.write().await;
    guard.remove(&normalized_uri);
  }

  async fn did_change(&self, params: DidChangeTextDocumentParams) {
    if let Some(change) = params.content_changes.into_iter().last() {
      self
        .update_document(params.text_document.uri, change.text)
        .await;
    }
  }

  async fn did_save(&self, params: DidSaveTextDocumentParams) {
    if let Some(text) = params.text {
      self.update_document(params.text_document.uri, text).await;
    }

    // The checker reads the journal from disk, so saving is the only point where its
    // input actually changed.
    self.enqueue_checker_run().await;
  }

  async fn diagnostic(
    &self,
    params: DocumentDiagnosticParams,
  ) -> Result<DocumentDiagnosticReportResult> {
    let uri = normalize_uri(&params.text_document.uri);
    let items = self.document_diagnostics_for(&uri).await;

    Ok(DocumentDiagnosticReportResult::Report(
      DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
        full_document_diagnostic_report: FullDocumentDiagnosticReport {
          result_id: None,
          items,
        },
        related_documents: None,
      }),
    ))
  }

  async fn workspace_diagnostic(
    &self,
    _params: WorkspaceDiagnosticParams,
  ) -> Result<WorkspaceDiagnosticReportResult> {
    let snapshot = self.snapshot().await.unwrap_or_default();
    let checker = self.checker_results().await;

    let mut items = Vec::with_capacity(snapshot.len());
    for (uri, doc) in snapshot.iter() {
      items.push(WorkspaceDocumentDiagnosticReport::Full(
        WorkspaceFullDocumentDiagnosticReport {
          uri: uri.clone(),
          version: None,
          full_document_diagnostic_report: FullDocumentDiagnosticReport {
            result_id: None,
            items: merge_diagnostics(
              Some(doc.as_ref()),
              checker.by_uri.get(uri).map(Vec::as_slice),
            ),
          },
        },
      ));
    }

    Ok(WorkspaceDiagnosticReportResult::Report(
      WorkspaceDiagnosticReport { items },
    ))
  }

  async fn completion(
    &self,
    params: CompletionParams,
  ) -> Result<Option<CompletionResponse>> {
    spdlog::debug!("completion triggered");
    let current_uri =
      normalize_uri(&params.text_document_position.text_document.uri.clone());

    let (snapshot, root_uri) = self
      .with_inner(|inner| {
        let snapshot = inner.snapshot_rx.borrow().clone();
        let root_uri = inner.root_uri.clone();
        (snapshot, root_uri)
      })
      .await?;

    // Clone the snapshot so we can refresh the current document with the latest on-disk content.
    let mut docs = (*snapshot).clone();

    // Prefer the latest in-editor text (unsaved edits) for completions.
    if let Some(text) = self.documents_text.read().await.get(&current_uri).cloned() {
      if let Some(fresh) =
        parse_document_from_text_async(current_uri.clone(), text).await
      {
        docs.insert(current_uri.clone(), fresh);
      }
    } else if let Some(fresh) =
      reparse_document_from_disk_async(current_uri.clone()).await
    {
      docs.insert(current_uri.clone(), fresh);
    }

    let list = match root_uri.as_ref() {
      Some(root_uri) => completion::completion(&docs, root_uri, &params),
      None => completion::completion(&docs, &current_uri, &params),
    };

    match &list {
      Some(list) => {
        if spdlog::default_logger().should_log(spdlog::Level::Debug) {
          let labels: Vec<&str> =
            list.items.iter().map(|item| item.label.as_str()).collect();
          spdlog::debug!(
            "completion result: count={}, labels={:?}",
            list.items.len(),
            labels
          );
        } else {
          spdlog::debug!("completion result: count={}", list.items.len());
        }
      }
      None => {
        spdlog::debug!("completion result: none");
      }
    }

    Ok(list.map(CompletionResponse::List))
  }

  async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
    let current_uri =
      normalize_uri(&params.text_document_position_params.text_document.uri);
    self
      .with_inner(|inner| {
        let snapshot = inner.snapshot_rx.borrow().clone();
        match &inner.root_uri {
          Some(root_uri) => hover::hover(snapshot.as_ref(), root_uri, &params),
          None => hover::hover(snapshot.as_ref(), &current_uri, &params),
        }
      })
      .await
  }

  async fn goto_definition(
    &self,
    params: GotoDefinitionParams,
  ) -> Result<Option<GotoDefinitionResponse>> {
    let current_uri =
      normalize_uri(&params.text_document_position_params.text_document.uri);
    self
      .with_inner(|inner| {
        let snapshot = inner.snapshot_rx.borrow().clone();
        match &inner.root_uri {
          Some(root_uri) => {
            definition::goto_definition(snapshot.as_ref(), root_uri, &params)
          }
          None => {
            let doc = resolve_document_from_snapshot(snapshot.as_ref(), &current_uri)?;
            let mut docs = HashMap::new();
            docs.insert(current_uri.clone(), doc);
            definition::goto_definition(&docs, &current_uri, &params)
          }
        }
      })
      .await
  }

  async fn semantic_tokens_full(
    &self,
    params: SemanticTokensParams,
  ) -> Result<Option<SemanticTokensResult>> {
    let uri = normalize_uri(&params.text_document.uri);

    spdlog::debug!("semantic tokens requested uri={:?}", uri);

    // Prefer the latest in-memory text from didOpen/didChange/didSave to avoid stale
    // highlighting when the indexer snapshot lags behind fast edits.
    if let Some(text) = self.documents_text.read().await.get(&uri).cloned() {
      spdlog::debug!("semantic tokens resolved uri={:?} source=memory", uri);
      return Ok(semantic_tokens::semantic_tokens_full_from_text(
        text.as_str(),
      ));
    }

    // Prefer the latest on-disk content (async) so semantic tokens stay in sync even when the
    // snapshot/indexer is slightly behind or the file changed externally.
    if let Some(doc) = reparse_document_from_disk_async(uri.clone()).await {
      spdlog::debug!("semantic tokens resolved uri={:?} source=disk", uri);
      return Ok(semantic_tokens::semantic_tokens_full(doc.as_ref()));
    }

    spdlog::debug!("semantic tokens resolved uri={:?} source=snapshot", uri);

    let doc = self
      .with_inner(|inner| {
        let snapshot = inner.snapshot_rx.borrow().clone();
        resolve_document_from_snapshot(snapshot.as_ref(), &uri)
      })
      .await?;

    Ok(doc.and_then(|doc| semantic_tokens::semantic_tokens_full(doc.as_ref())))
  }
}

fn log_collected_accounts(docs: &HashMap<Url, Arc<Document>>) {
  let mut accounts: HashSet<String> = HashSet::new();
  for doc in docs.values() {
    accounts.extend(doc.accounts.iter().cloned());
  }

  spdlog::info!("collected accounts at init count={}", accounts.len());
  // Keep the full list at debug level to avoid noisy info logs.
  spdlog::debug!("accounts={:?}", accounts);
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::indexer::{Indexer, canonical_or_original};
  use crate::test_utils::lines;
  use std::fs;
  use std::path::Path;
  use tower_lsp_server::ls_types::Uri as Url;

  fn write_file(path: &Path, content: &str) {
    if let Some(parent) = path.parent() {
      fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, content).unwrap();
  }

  fn build_doc(text: &str) -> Document {
    crate::doc::build_document(text.to_owned(), "/tmp/main.bean").expect("document")
  }

  #[test]
  fn load_journal_tree_expands_includes_transitively() {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path().join("main.bean");

    // main -> accounts/*.bean + common.bean
    write_file(
      &root,
      r#"include "accounts/*.bean"
include "common.bean"
"#,
    );

    let account_one = dir.path().join("accounts/one.bean");
    let account_two = dir.path().join("accounts/two.bean");
    write_file(
      &account_one,
      r#"include "../nested/n1.bean"
"#,
    );
    write_file(&account_two, "\n");

    let common = dir.path().join("common.bean");
    write_file(
      &common,
      r#"include "nested/*.bean"
"#,
    );

    let nested1 = dir.path().join("nested/n1.bean");
    let nested2 = dir.path().join("nested/n2.bean");
    write_file(&nested1, "\n");
    write_file(&nested2, "\n");

    let indexer = Indexer::from_journal(&root).unwrap();
    let docs = indexer.documents();

    let expected_paths = [
      &root,
      &account_one,
      &account_two,
      &common,
      &nested1,
      &nested2,
    ];
    for p in expected_paths {
      let p = canonical_or_original(p);
      let uri = Url::from_file_path(&p).unwrap();
      assert!(docs.contains_key(&uri), "missing document: {}", p.display());
    }

    assert_eq!(docs.len(), 6);
  }

  fn ledger_uri(path: &Path) -> Url {
    Url::from_file_path(canonical_or_original(path)).expect("absolute path")
  }

  #[test]
  fn checker_diagnostics_are_attributed_to_reported_file() {
    let dir = tempfile::tempdir().unwrap();
    let root_uri = ledger_uri(&dir.path().join("main.bean"));
    let accounts = dir.path().join("accounts.bean");
    let diags = vec![CheckerDiagnostic {
      message: "Balance failed for 'Assets:Cash'".to_owned(),
      filename: Some(accounts.to_string_lossy().into_owned()),
      lineno: Some(5),
    }];

    let by_uri = checker_diagnostics_by_uri(diags, "python-script", &root_uri);

    assert!(!by_uri.contains_key(&root_uri), "{by_uri:?}");

    let items = by_uri
      .get(&ledger_uri(&accounts))
      .expect("diagnostic on accounts.bean");
    assert_eq!(items.len(), 1);
    assert_eq!(items[0].range.start.line, 5);
    assert_eq!(
      items[0].source.as_deref(),
      Some("beancount (python-script)")
    );
  }

  #[test]
  fn checker_diagnostics_without_file_fall_back_to_root() {
    let dir = tempfile::tempdir().unwrap();
    let root_uri = ledger_uri(&dir.path().join("main.bean"));
    let diags = vec![CheckerDiagnostic {
      message: "Cannot infer currency".to_owned(),
      filename: None,
      lineno: None,
    }];

    let by_uri = checker_diagnostics_by_uri(diags, "pyo3", &root_uri);

    let items = by_uri.get(&root_uri).expect("diagnostic on root file");
    assert_eq!(items.len(), 1);
    assert_eq!(items[0].range.start.line, 0);
  }

  #[test]
  fn document_diagnostics_reports_unbalanced_transaction() {
    let doc = build_doc(&lines(&[
      r#"2022-01-01 * "lunch" "unbalanced""#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food 5 USD"#,
    ]));

    let items = document_diagnostics(&doc);
    assert!(!items.is_empty(), "expected an inference diagnostic");
    assert!(
      items
        .iter()
        .all(|item| { item.source.as_deref() == Some("beancount (inference)") })
    );
  }

  #[test]
  fn document_diagnostics_is_empty_for_valid_ledger() {
    let doc = build_doc(&lines(&[
      r#"2022-01-01 * "lunch" "balanced""#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food 10 USD"#,
    ]));

    assert!(document_diagnostics(&doc).is_empty());
  }

  #[test]
  fn merge_diagnostics_appends_checker_items() {
    let doc = build_doc(&lines(&[
      r#"2022-01-01 * "lunch" "balanced""#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food 10 USD"#,
    ]));
    let checker = vec![Diagnostic {
      range: Range::new(Position::new(4, 0), Position::new(4, 4)),
      message: "balance failed".to_owned(),
      ..Diagnostic::default()
    }];

    let items = merge_diagnostics(Some(&doc), Some(&checker));
    assert_eq!(items.len(), 1);
    assert_eq!(items[0].message, "balance failed");
  }

  #[test]
  fn merge_diagnostics_without_document_keeps_checker_items() {
    let checker = vec![Diagnostic::default()];

    assert_eq!(merge_diagnostics(None, Some(&checker)).len(), 1);
    assert!(merge_diagnostics(None, None).is_empty());
  }

  #[test]
  fn pull_diagnostics_requires_client_capability() {
    let with_pull: ClientCapabilities = serde_json::from_value(serde_json::json!({
      "textDocument": { "diagnostic": { "dynamicRegistration": true } }
    }))
    .unwrap();
    let without_pull: ClientCapabilities = serde_json::from_value(serde_json::json!({
      "textDocument": { "hover": { "dynamicRegistration": true } }
    }))
    .unwrap();

    assert!(client_supports_pull_diagnostics(&with_pull));
    assert!(!client_supports_pull_diagnostics(&without_pull));
  }

  #[tokio::test]
  async fn waits_for_requested_checker_generation() {
    let (results_tx, results_rx) = watch::channel(Arc::new(CheckerResults::default()));
    let (started_tx, started_rx) = tokio::sync::oneshot::channel::<()>();

    let worker = tokio::spawn(async move {
      started_rx.await.unwrap();
      let mut by_uri = HashMap::new();
      by_uri.insert(
        Url::from_file_path("/tmp/main.bean").unwrap(),
        vec![Diagnostic::default()],
      );
      results_tx
        .send(Arc::new(CheckerResults {
          generation: 2,
          by_uri,
        }))
        .unwrap();
    });

    started_tx.send(()).unwrap();

    let results = wait_for_checker_results(2, &results_rx).await;
    assert_eq!(results.generation, 2);
    assert_eq!(results.by_uri.len(), 1);

    worker.await.unwrap();
  }
}
