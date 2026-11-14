//! End-to-end diagnostics tests over stdio, covering both delivery models the server negotiates
//! through capabilities: pull (`textDocument/diagnostic`) and push
//! (`textDocument/publishDiagnostics`). They also cover the ledger checker's contribution, which
//! only shows up when a Python with beancount is reachable.

use std::io::{BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::{Arc, Mutex};

use serde_json::{Value, json};
use tower_lsp_server::ls_types::Uri;

struct Server {
  child: Child,
  stdin: ChildStdin,
  stdout: BufReader<ChildStdout>,
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
    let stdout = BufReader::new(child.stdout.take().expect("stdout"));

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
      stdout,
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
    let mut length = 0usize;
    loop {
      let mut line = String::new();
      let read = self.stdout.read_line(&mut line).expect("failed to read");
      assert_ne!(
        read,
        0,
        "server closed stdout, stderr:\n{}",
        self.stderr.lock().unwrap()
      );

      let line = line.trim_end();
      if line.is_empty() {
        break;
      }

      if let Some(value) = line.to_ascii_lowercase().strip_prefix("content-length:") {
        length = value.trim().parse().expect("invalid content length");
      }
    }

    let mut body = vec![0u8; length];
    self.stdout.read_exact(&mut body).expect("truncated body");
    serde_json::from_slice(&body).expect("invalid json")
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

      if let Some(method) = message.get("method").and_then(Value::as_str) {
        if method == "workspace/diagnostic/refresh" {
          self.refreshes += 1;
        }

        let response_id = message.get("id").cloned().unwrap_or(Value::Null);
        if !response_id.is_null() {
          // Server -> client request: answer so the server does not block on us.
          self.send(json!({"jsonrpc": "2.0", "id": response_id, "result": null}));
        }
      }
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
    let _ = self.child.wait();
  }
}

impl Drop for Server {
  fn drop(&mut self) {
    let _ = self.child.kill();
    let _ = self.child.wait();
  }
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
      let id = message.get("id").cloned().unwrap_or(Value::Null);
      server.send(json!({"jsonrpc": "2.0", "id": id, "result": null}));
    }
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
  let accounts_uri = ledger_uri(&ledger.accounts);

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
        if params["uri"] == accounts_uri.as_str()
          && !balance_failures(params["diagnostics"].as_array().expect("diagnostics"))
            .is_empty()
        {
          break;
        }
      }
      Some("workspace/diagnostic/refresh") => {
        panic!("push clients must not receive diagnostic refresh requests");
      }
      _ => {
        let id = message.get("id").cloned().unwrap_or(Value::Null);
        if message.get("method").is_some() && !id.is_null() {
          server.send(json!({"jsonrpc": "2.0", "id": id, "result": null}));
        }
      }
    }
  }

  drop(ledger);
  server.shutdown();
}
