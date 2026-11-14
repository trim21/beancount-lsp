//! End-to-end diagnostics tests over stdio, covering both delivery models the server negotiates
//! through capabilities: pull (`textDocument/diagnostic`) and push
//! (`textDocument/publishDiagnostics`). They also cover the ledger checker's contribution, which
//! only shows up when a Python with beancount is reachable.

use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use serde_json::{Value, json};
use tower_lsp_server::ls_types::Uri;

/// A message that never shows up has to fail the test instead of wedging the whole job.
const READ_TIMEOUT: Duration = Duration::from_secs(30);

struct Server {
  child: Child,
  stdin: ChildStdin,
  messages: Receiver<Result<Value, String>>,
  stderr: Arc<Mutex<String>>,
  refreshes: usize,
}

impl Server {
  fn start(working_dir: &Path) -> Self {
    let mut child = Command::new(env!("CARGO_BIN_EXE_beancount-lsp"))
      .args(["--log-level", "error"])
      // The server looks for a Python with beancount in `.venv` relative to where it runs.
      .current_dir(working_dir)
      .stdin(Stdio::piped())
      .stdout(Stdio::piped())
      .stderr(Stdio::piped())
      .spawn()
      .expect("failed to spawn beancount-lsp");

    let stdin = child.stdin.take().expect("stdin");
    let stdout = child.stdout.take().expect("stdout");

    let stderr = Arc::new(Mutex::new(String::new()));
    let child_stderr = child.stderr.take().expect("stderr");
    let collected = Arc::clone(&stderr);
    std::thread::spawn(move || {
      let mut reader = BufReader::new(child_stderr);
      let mut line = String::new();
      while reader.read_line(&mut line).unwrap_or(0) > 0 {
        collected.lock().unwrap().push_str(&line);
        line.clear();
      }
    });

    Self {
      child,
      stdin,
      messages: spawn_message_reader(stdout),
      stderr,
      refreshes: 0,
    }
  }

  fn send(&mut self, message: Value) {
    let body = message.to_string();
    write!(self.stdin, "Content-Length: {}\r\n\r\n{}", body.len(), body)
      .expect("failed to write message");
    self.stdin.flush().expect("failed to flush message");
  }

  fn notify(&mut self, method: &str, params: Value) {
    self.send(json!({"jsonrpc": "2.0", "method": method, "params": params}));
  }

  fn read(&mut self) -> Value {
    match self.messages.recv_timeout(READ_TIMEOUT) {
      Ok(Ok(message)) => message,
      Ok(Err(err)) => panic!("{err}, stderr:\n{}", self.stderr.lock().unwrap()),
      Err(RecvTimeoutError::Timeout) => panic!(
        "no message from the server within {READ_TIMEOUT:?}, stderr:\n{}",
        self.stderr.lock().unwrap()
      ),
      Err(RecvTimeoutError::Disconnected) => panic!(
        "server message reader stopped, stderr:\n{}",
        self.stderr.lock().unwrap()
      ),
    }
  }

  /// Server -> client requests leave the server waiting for a response, so answer them.
  fn answer_request(&mut self, message: &Value) {
    let id = message.get("id").cloned().unwrap_or(Value::Null);
    if message.get("method").is_some() && !id.is_null() {
      self.send(json!({"jsonrpc": "2.0", "id": id, "result": null}));
    }
  }

  /// Send a request and wait for its response, answering anything the server asks in between.
  fn request(&mut self, id: i64, method: &str, params: Value) -> Value {
    self.send(json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params}));

    loop {
      let message = self.read();

      if message.get("id").and_then(Value::as_i64) == Some(id)
        && message.get("method").is_none()
      {
        assert!(message.get("error").is_none(), "request failed: {message}");
        return message.get("result").cloned().unwrap_or(Value::Null);
      }

      if message.get("method").and_then(Value::as_str)
        == Some("workspace/diagnostic/refresh")
      {
        self.refreshes += 1;
      }
      self.answer_request(&message);
    }
  }

  fn initialize(&mut self, pull: bool, root_file: &Path) -> Value {
    let capabilities = if pull {
      json!({"textDocument": {"diagnostic": {}}})
    } else {
      json!({"textDocument": {}})
    };

    self.request(
      1,
      "initialize",
      json!({
        "processId": null,
        "rootUri": null,
        "capabilities": capabilities,
        "initializationOptions": {"root_file": root_file},
      }),
    )
  }

  fn shutdown(mut self) {
    self.request(99, "shutdown", Value::Null);
    self.notify("exit", Value::Null);

    let deadline = Instant::now() + Duration::from_secs(10);
    loop {
      match self.child.try_wait().expect("failed to check the server") {
        Some(_) => return,
        None if Instant::now() < deadline => {
          std::thread::sleep(Duration::from_millis(50))
        }
        None => panic!(
          "server did not exit after shutdown, stderr:\n{}",
          self.stderr.lock().unwrap()
        ),
      }
    }
  }
}

impl Drop for Server {
  fn drop(&mut self) {
    let _ = self.child.kill();
    let _ = self.child.wait();
  }
}

/// Read one framed JSON-RPC message from the server's stdout.
fn read_message(stdout: &mut impl BufRead) -> Result<Value, String> {
  let mut length = 0usize;
  loop {
    let mut line = String::new();
    let read = stdout.read_line(&mut line).map_err(|err| err.to_string())?;
    if read == 0 {
      return Err("server closed stdout".to_owned());
    }

    let line = line.trim_end();
    if line.is_empty() {
      break;
    }

    if let Some(value) = line.to_ascii_lowercase().strip_prefix("content-length:") {
      length = value
        .trim()
        .parse()
        .map_err(|err: std::num::ParseIntError| {
          format!("invalid content length: {err}")
        })?;
    }
  }

  let mut body = vec![0u8; length];
  stdout
    .read_exact(&mut body)
    .map_err(|err| err.to_string())?;
  serde_json::from_slice(&body).map_err(|err| err.to_string())
}

/// Own stdout on its own thread: reading a pipe has no timeout, so the test would block forever
/// waiting for a message the server never sends.
fn spawn_message_reader(stdout: ChildStdout) -> Receiver<Result<Value, String>> {
  let (tx, rx) = mpsc::channel();

  std::thread::spawn(move || {
    let mut stdout = BufReader::new(stdout);
    loop {
      let message = read_message(&mut stdout);
      let failed = message.is_err();
      if tx.send(message).is_err() || failed {
        return;
      }
    }
  });

  rx
}

/// The server canonicalizes paths before publishing diagnostics, so the URI it reports only
/// matches the ledger's path after both sides go through `canonicalize`: on macOS the tempdir is
/// `/var/folders/...` while canonicalize resolves it to `/private/var/folders/...`.
fn same_file(reported_uri: &Value, path: &Path) -> bool {
  reported_uri
    .as_str()
    .and_then(|uri| uri.parse::<Uri>().ok())
    .and_then(|uri| uri.to_file_path().map(|uri_path| uri_path.into_owned()))
    .and_then(|uri_path| std::fs::canonicalize(uri_path).ok())
    .zip(std::fs::canonicalize(path).ok())
    .is_some_and(|(reported, expected)| reported == expected)
}

fn workspace_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

/// The server only runs a ledger checker when it can find a Python that imports beancount.
fn has_beancount(workspace_root: &Path) -> bool {
  let venv = workspace_root.join(".venv");
  let venv_python = if cfg!(windows) {
    venv.join("Scripts/python.exe")
  } else {
    venv.join("bin/python")
  };

  [
    venv_python,
    PathBuf::from("python3"),
    PathBuf::from("python"),
  ]
  .iter()
  .any(|python| {
    Command::new(python)
      .args(["-c", "import beancount"])
      .stdout(Stdio::null())
      .stderr(Stdio::null())
      .status()
      .map(|status| status.success())
      .unwrap_or(false)
  })
}

fn ledger_uri(path: &Path) -> String {
  Uri::from_file_path(path)
    .expect("path should be absolute")
    .to_string()
}

/// A ledger whose only problem is a balance assertion failure, which only beancount itself can
/// detect: our own parser and inference pass it cleanly.
struct Ledger {
  _dir: tempfile::TempDir,
  root: PathBuf,
  accounts: PathBuf,
}

impl Ledger {
  fn new() -> Self {
    let dir = tempfile::tempdir().expect("tempdir");
    let accounts = dir.path().join("accounts.bean");
    std::fs::write(
      &accounts,
      [
        "2022-01-01 open Assets:Cash",
        "2022-01-01 open Expenses:Food",
        "2022-01-02 * \"lunch\"",
        "  Assets:Cash -10 USD",
        "  Expenses:Food 10 USD",
        "2022-01-03 balance Assets:Cash 0 USD",
        "",
      ]
      .join("\n"),
    )
    .expect("write accounts.bean");

    let root = dir.path().join("main.bean");
    std::fs::write(&root, "include \"accounts.bean\"\n").expect("write main.bean");

    Self {
      _dir: dir,
      root,
      accounts,
    }
  }
}

fn balance_failures(items: &[Value]) -> Vec<&Value> {
  items
    .iter()
    .filter(|item| {
      item
        .get("message")
        .and_then(Value::as_str)
        .is_some_and(|message| message.contains("Balance failed"))
    })
    .collect()
}

#[test]
fn pull_diagnostics_include_checker_errors() {
  let workspace = workspace_root();
  if !has_beancount(&workspace) {
    eprintln!("skipping: no python interpreter with beancount installed");
    return;
  }

  let ledger = Ledger::new();
  let accounts_uri = ledger_uri(&ledger.accounts);
  let root_uri = ledger_uri(&ledger.root);

  let mut server = Server::start(&workspace);
  let initialized = server.initialize(true, &ledger.root);
  assert!(
    initialized["capabilities"]["diagnosticProvider"].is_object(),
    "pull clients must be offered a diagnostic provider: {initialized}"
  );

  server.notify("initialized", json!({}));

  // The included file is never opened: the report has to come from the index plus the checker.
  let report = server.request(
    2,
    "textDocument/diagnostic",
    json!({"textDocument": {"uri": accounts_uri}}),
  );
  assert_eq!(report["kind"], "full", "{report}");

  let items = report["items"].as_array().expect("items").clone();
  let failures = balance_failures(&items);
  assert_eq!(failures.len(), 1, "{items:?}");
  assert_eq!(failures[0]["range"]["start"]["line"], 5, "{items:?}");
  assert!(
    failures[0]["source"]
      .as_str()
      .is_some_and(|source| source.starts_with("beancount (")),
    "{items:?}"
  );

  // Checker errors must not be attributed to the journal root.
  let report = server.request(
    3,
    "textDocument/diagnostic",
    json!({"textDocument": {"uri": root_uri}}),
  );
  assert_eq!(
    report["items"].as_array().map(Vec::len),
    Some(0),
    "{report}"
  );

  // A save makes the server re-run the checker and ask pull clients to refresh.
  let refreshes_before_save = server.refreshes;
  server.notify(
    "textDocument/didSave",
    json!({"textDocument": {"uri": root_uri}}),
  );
  while server.refreshes == refreshes_before_save {
    let message = server.read();
    if message.get("method").and_then(Value::as_str)
      == Some("workspace/diagnostic/refresh")
    {
      server.refreshes += 1;
    }
    server.answer_request(&message);
  }

  drop(ledger);
  server.shutdown();
}

#[test]
fn push_diagnostics_include_checker_errors() {
  let workspace = workspace_root();
  if !has_beancount(&workspace) {
    eprintln!("skipping: no python interpreter with beancount installed");
    return;
  }

  let ledger = Ledger::new();

  let mut server = Server::start(&workspace);
  let initialized = server.initialize(false, &ledger.root);
  assert!(
    initialized["capabilities"]["diagnosticProvider"].is_null(),
    "clients without pull support must not be offered a diagnostic provider: {initialized}"
  );

  server.notify("initialized", json!({}));

  loop {
    let message = server.read();
    match message.get("method").and_then(Value::as_str) {
      Some("textDocument/publishDiagnostics") => {
        let params = &message["params"];
        if same_file(&params["uri"], &ledger.accounts)
          && !balance_failures(params["diagnostics"].as_array().expect("diagnostics"))
            .is_empty()
        {
          break;
        }
      }
      Some("workspace/diagnostic/refresh") => {
        panic!("push clients must not receive diagnostic refresh requests");
      }
      _ => server.answer_request(&message),
    }
  }

  drop(ledger);
  server.shutdown();
}

/// A client that opened a file through a symlinked directory keeps that path, so diagnostics have
/// to come back on the URI it sent instead of the canonicalized one, which it does not know.
#[cfg(unix)]
#[test]
fn push_diagnostics_use_the_uri_the_client_opened() {
  let ledger = Ledger::new();
  let link_dir = tempfile::tempdir().expect("tempdir");
  let ledger_link = link_dir.path().join("ledger");
  std::os::unix::fs::symlink(ledger.root.parent().expect("ledger dir"), &ledger_link)
    .expect("create symlink");

  let client_uri = ledger_uri(&ledger_link.join("accounts.bean"));
  assert_ne!(
    client_uri,
    ledger_uri(&ledger.accounts),
    "the client path must differ from the canonical one"
  );

  let mut server = Server::start(&workspace_root());
  server.initialize(false, &ledger.root);
  server.notify("initialized", json!({}));
  server.notify(
    "textDocument/didOpen",
    json!({
      "textDocument": {
        "uri": client_uri,
        "languageId": "beancount",
        "version": 1,
        // Unbalanced, so the buffer alone produces a diagnostic.
        "text": "2022-01-01 * \"lunch\" \"unbalanced\"\n  Assets:Cash -10 USD\n  Expenses:Food 5 USD\n",
      },
    }),
  );

  loop {
    let message = server.read();
    if message.get("method").and_then(Value::as_str)
      == Some("textDocument/publishDiagnostics")
    {
      let params = &message["params"];
      if params["uri"] == client_uri.as_str()
        && !params["diagnostics"]
          .as_array()
          .expect("diagnostics")
          .is_empty()
      {
        break;
      }
    }
    server.answer_request(&message);
  }

  drop(ledger);
  server.shutdown();
}
