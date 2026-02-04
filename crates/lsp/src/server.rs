use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use anyhow::anyhow;
use serde::Deserialize;
use tokio::sync::{Mutex, RwLock, mpsc, watch};
use tokio::task;
use tower_lsp_server::jsonrpc::{Error, Result};
use tower_lsp_server::ls_types::{
    CompletionOptions, CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, MessageType, Position, Range,
    SaveOptions, SemanticTokensFullOptions, SemanticTokensOptions, SemanticTokensParams,
    SemanticTokensResult, SemanticTokensServerCapabilities, ServerCapabilities, ServerInfo,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, Uri as Url,
};
use tower_lsp_server::{Client, LanguageServer};
use tracing::info;

use crate::checkers::{self, Checker, CheckerDiagnostic};
use crate::indexer::{Indexer, canonical_or_original};
use crate::providers::definition;
use crate::providers::{completion, hover, semantic_tokens};

pub(crate) fn documents_bfs(
    documents: &HashMap<Url, Arc<Document>>,
    root: &Url,
) -> Vec<(Url, Arc<Document>)> {
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
                tracing::warn!(file = %canonical.display(), "failed to convert include path to URI");
            }
        }
    }

    ordered
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

pub type Document = crate::doc::Document;
pub(crate) use crate::doc::find_document;

#[derive(Clone)]
pub struct Backend {
    client: Client,
    inner: Arc<RwLock<Option<InnerBackend>>>, // initialized state
    checker: Option<Arc<dyn Checker>>,
    checker_queued: Arc<AtomicBool>,
    checker_tx: mpsc::UnboundedSender<()>,
    checker_rx: Arc<Mutex<Option<mpsc::UnboundedReceiver<()>>>>,
    indexer_tx: mpsc::UnboundedSender<IndexerCommand>,
    indexer_rx: Arc<Mutex<Option<mpsc::UnboundedReceiver<IndexerCommand>>>>,
}

struct InnerBackend {
    snapshot_rx: watch::Receiver<Arc<HashMap<Url, Arc<Document>>>>,
    root_uri: Option<Url>,
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
    let message = match diag.filename.filter(|name| !name.is_empty()) {
        Some(name) => format!("{name}: {}", diag.message),
        None => diag.message,
    };

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

pub fn parse_initialize_config(params: &InitializeParams) -> Result<InitializeConfig> {
    let value = params
        .initialization_options
        .as_ref()
        .ok_or_else(|| Error::invalid_params("missing initializationOptions"))?;

    tracing::debug!("init opts: {}", value.clone());

    serde_json::from_value(value.clone())
        .map_err(|err| Error::invalid_params(format!("invalid initializationOptions: {err}")))
}

impl Backend {
    pub fn new(client: Client) -> Self {
        let (checker_tx, checker_rx) = mpsc::unbounded_channel();
        let (indexer_tx, indexer_rx) = mpsc::unbounded_channel();

        Self {
            client,
            inner: Arc::new(RwLock::new(None)),
            checker: checkers::create().map(Arc::<dyn Checker>::from),
            checker_queued: Arc::new(AtomicBool::new(false)),
            checker_tx,
            checker_rx: Arc::new(Mutex::new(Some(checker_rx))),
            indexer_tx,
            indexer_rx: Arc::new(Mutex::new(Some(indexer_rx))),
        }
    }

    fn enqueue_checker_run(&self) {
        if self.checker_queued.swap(true, Ordering::SeqCst) {
            return;
        }

        let _ = self.checker_tx.send(());
    }

    fn spawn_checker_worker(&self, root_file: PathBuf, mut rx: mpsc::UnboundedReceiver<()>) {
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
            let mut published: HashSet<Url> = HashSet::new();
            while rx.recv().await.is_some() {
                published = this
                    .run_checker_once(&published, &root_uri, &root_file)
                    .await;

                this.checker_queued.store(false, Ordering::SeqCst);
            }
        });
    }

    fn spawn_indexer_worker(
        &self,
        mut indexer: Indexer,
        mut rx: mpsc::UnboundedReceiver<IndexerCommand>,
        snapshot_tx: watch::Sender<Arc<HashMap<Url, Arc<Document>>>>,
    ) {
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
                }
            }
        });
    }

    async fn run_checker_once(
        &self,
        published: &HashSet<Url>,
        root_uri: &Url,
        root_file: &Path,
    ) -> HashSet<Url> {
        let root_path = canonical_or_original(root_file);
        let root_file_str = root_path.display().to_string();
        let mut diags_by_uri: HashMap<Url, Vec<Diagnostic>> = HashMap::new();

        if let Some(checker) = self.checker.as_ref().map(Arc::clone) {
            let result = task::spawn_blocking(move || {
                let name = checker.name();
                tracing::info!("run checker: {}", name);

                match checker.run(&root_file_str) {
                    Ok(diags) => {
                        let mapped = diags
                            .into_iter()
                            .map(|diag| {
                                let filename = diag.filename.clone();
                                let lsp = checker_diagnostic_to_lsp(diag, name);
                                (filename, lsp)
                            })
                            .collect::<Vec<_>>();
                        (mapped, Vec::new())
                    }
                    Err(err) => {
                        tracing::warn!("failed to run checker: {}", err);
                        (Vec::new(), vec![(name, err)])
                    }
                }
            })
            .await;

            match result {
                Ok((extra_diags, errors)) => {
                    for (filename, diag) in extra_diags {
                        let target_uri = filename
                            .and_then(|name| {
                                Url::from_file_path(canonical_or_original(Path::new(&name)))
                            })
                            .unwrap_or_else(|| root_uri.clone());

                        diags_by_uri.entry(target_uri).or_default().push(diag);
                    }

                    for (name, err) in errors {
                        self.client
                            .log_message(
                                MessageType::ERROR,
                                format!("{name} checker failed: {err}"),
                            )
                            .await;
                    }
                }
                Err(err) => {
                    self.client
                        .log_message(MessageType::ERROR, format!("checker task panicked: {err}"))
                        .await;
                }
            }
        }

        let new_keys: HashSet<Url> = diags_by_uri.keys().cloned().collect();

        // Clear diagnostics that disappeared in this run so the client drops them.
        let stale = published.difference(&new_keys).cloned().collect::<Vec<_>>();

        for uri in stale {
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }

        for (target_uri, diags) in diags_by_uri {
            self.client
                .publish_diagnostics(target_uri.clone(), diags, None)
                .await;
        }

        new_keys
    }

    async fn with_inner<R>(&self, f: impl FnOnce(&InnerBackend) -> R) -> Result<R> {
        let guard = self.inner.read().await;
        let inner = guard.as_ref().ok_or_else(Error::invalid_request)?;
        Ok(f(inner))
    }

    async fn update_document(&self, uri: Url, text: String) {
        let _ = self.indexer_tx.send(IndexerCommand::Update { uri, text });
        if self
            .inner
            .read()
            .await
            .as_ref()
            .and_then(|inner| inner.root_uri.as_ref())
            .is_some()
        {
            self.enqueue_checker_run();
        }
    }

    async fn reparse(&self, _uri: &Url) {
        if self
            .inner
            .read()
            .await
            .as_ref()
            .and_then(|inner| inner.root_uri.as_ref())
            .is_some()
        {
            self.enqueue_checker_run();
        }
    }
}

impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let config = parse_initialize_config(&params)?;

        if let Some(root_file) = &config.root_file {
            info!(root_file = %root_file.display(), "received initialization config");
        } else {
            info!("received initialization config without root_file");
        }

        let root_path = config
            .root_file
            .as_ref()
            .map(|path| canonical_or_original(path));
        let root_uri = root_path.as_ref().and_then(Url::from_file_path);
        {
            let mut guard = self.inner.write().await;
            if guard.is_some() {
                return Err(Error::invalid_params("initializationOptions already set"));
            }

            let indexer = match root_path.as_ref() {
                Some(path) => Indexer::from_journal(path).unwrap_or_else(|e| {
                    tracing::warn!("failed to load journal tree: {e}");
                    Indexer::new()
                }),
                None => Indexer::new(),
            };

            let initial_snapshot = Arc::new(indexer.documents().clone());
            let (snapshot_tx, snapshot_rx) = watch::channel(initial_snapshot);

            *guard = Some(InnerBackend {
                snapshot_rx,
                root_uri,
            });

            if let Some(rx) = self.indexer_rx.lock().await.take() {
                self.spawn_indexer_worker(indexer, rx, snapshot_tx);
            }
        }

        // Start checker worker after initialization so the root file path is known.
        if let (Some(root_path), Some(rx)) = (root_path, self.checker_rx.lock().await.take()) {
            self.spawn_checker_worker(root_path, rx);
        }

        let text_document_sync = TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
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

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(text_document_sync),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![
                        ":".to_string(),
                        "0".to_string(),
                        "1".to_string(),
                        "2".to_string(),
                        "3".to_string(),
                        "4".to_string(),
                        "5".to_string(),
                        "6".to_string(),
                        "7".to_string(),
                        "8".to_string(),
                        "9".to_string(),
                    ]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(tower_lsp_server::ls_types::OneOf::Left(true)),
                semantic_tokens_provider: Some(semantic_tokens),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: "beancount-lsp".to_owned(),
                version: None,
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "beancount-lsp initialized")
            .await;

        // Run checker once after initialization without blocking this handler.
        self.enqueue_checker_run();
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.update_document(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            self.update_document(params.text_document.uri, change.text)
                .await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.update_document(params.text_document.uri, text).await;
        } else {
            self.reparse(&params.text_document.uri).await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        tracing::debug!("completion triggered");
        let current_uri = params.text_document_position.text_document.uri.clone();

        self.with_inner(|inner| {
            let snapshot = inner.snapshot_rx.borrow().clone();
            match &inner.root_uri {
                Some(root_uri) => completion::completion(snapshot.as_ref(), root_uri, &params),
                None => {
                    let doc = resolve_document_from_snapshot(snapshot.as_ref(), &current_uri)?;
                    let mut docs = HashMap::new();
                    docs.insert(current_uri.clone(), doc);
                    completion::completion(&docs, &current_uri, &params)
                }
            }
            .map(CompletionResponse::List)
        })
        .await
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let current_uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();
        self.with_inner(|inner| {
            let snapshot = inner.snapshot_rx.borrow().clone();
            match &inner.root_uri {
                Some(root_uri) => hover::hover(snapshot.as_ref(), root_uri, &params),
                None => {
                    let doc = resolve_document_from_snapshot(snapshot.as_ref(), &current_uri)?;
                    let mut docs = HashMap::new();
                    docs.insert(current_uri.clone(), doc);
                    hover::hover(&docs, &current_uri, &params)
                }
            }
        })
        .await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let current_uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();
        self.with_inner(|inner| {
            let snapshot = inner.snapshot_rx.borrow().clone();
            match &inner.root_uri {
                Some(root_uri) => definition::goto_definition(snapshot.as_ref(), root_uri, &params),
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
        let uri = params.text_document.uri;
        let text = self
            .with_inner(|inner| {
                let snapshot = inner.snapshot_rx.borrow().clone();
                resolve_document_from_snapshot(snapshot.as_ref(), &uri)
            })
            .await?;
        Ok(text.and_then(|doc| semantic_tokens::semantic_tokens_full(doc.as_ref())))
    }
}

#[cfg(test)]
mod tests {
    use crate::indexer::{Indexer, canonical_or_original};
    use std::fs;
    use std::path::Path;
    use tower_lsp_server::ls_types::Uri as Url;

    fn write_file(path: &Path, content: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, content).unwrap();
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
}
