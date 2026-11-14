#![allow(dead_code)]
use std::path::PathBuf;
use std::process::Command;

use super::{Checker, CheckerDiagnostic};
use serde::Deserialize;

const SCRIPT: &str = include_str!("scripts/python_json_checker.py");

pub struct PythonScriptChecker {
  interpreter: PathBuf,
}

impl PythonScriptChecker {
  pub fn new(interpreter: PathBuf) -> Self {
    Self { interpreter }
  }
}

pub fn find_python_with_beancount() -> Option<PathBuf> {
  // Prefer a project-local virtualenv if present.
  const WIN_VENV: &str = ".venv/Scripts/python.exe";
  const UNIX_VENV: &str = ".venv/bin/python";
  const CANDIDATES: &[&str] = &["python", "python3", "py"];

  for venv in [WIN_VENV, UNIX_VENV] {
    let venv_path = PathBuf::from(venv);
    if venv_path.is_file() && can_import_beancount(&venv_path) {
      return Some(venv_path);
    }
  }

  for cand in CANDIDATES {
    let candidate = PathBuf::from(cand);
    if can_import_beancount(&candidate) {
      return Some(candidate);
    }
  }

  None
}

fn can_import_beancount(python: &PathBuf) -> bool {
  Command::new(python)
    .arg("-c")
    .arg("import beancount")
    .status()
    .map(|status| status.success())
    .unwrap_or(false)
}

#[derive(Deserialize)]
struct ScriptError {
  message: String,
  #[serde(default)]
  lineno: Option<u32>,
  filename: Option<String>,
}

impl Checker for PythonScriptChecker {
  fn name(&self) -> &'static str {
    "python-script"
  }

  fn run(&self, root_file: &str) -> Result<Vec<CheckerDiagnostic>, String> {
    let output = Command::new(&self.interpreter)
      .arg("-c")
      .arg(SCRIPT)
      .arg(root_file)
      .output()
      .map_err(|e| format!("spawn python: {e}"))?;

    if !output.status.success() {
      let stderr = String::from_utf8_lossy(&output.stderr);
      return Err(format!("python exited with {}: {stderr}", output.status));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let parsed: Vec<ScriptError> = serde_json::from_str(&stdout)
      .map_err(|e| format!("parse json: {e}; stdout: {stdout}"))?;

    let diagnostics = parsed
      .into_iter()
      .map(|err| {
        let ScriptError {
          message,
          lineno,
          filename,
        } = err;

        CheckerDiagnostic {
          message,
          // beancount reports human-friendly (1-based) line numbers; file-level errors use 0,
          // which must not underflow when converted to the LSP's 0-based lines.
          lineno: lineno.map(|v| v.saturating_sub(1)),
          filename: filename.filter(|name| !name.is_empty()),
        }
      })
      .collect();

    Ok(diagnostics)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utils::lines;
  use std::fs;
  use std::path::Path;

  /// The checker needs a Python that can import beancount. Prefer the repo-local venv (the
  /// server itself probes `.venv/bin/python` relative to its working directory) and fall back
  /// to whatever `find_python_with_beancount` discovers, so this test skips instead of failing
  /// on machines without beancount.
  fn beancount_checker() -> Option<PythonScriptChecker> {
    let venv = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../.venv");
    let venv_python = if cfg!(windows) {
      venv.join("Scripts/python.exe")
    } else {
      venv.join("bin/python")
    };

    if venv_python.is_file() && can_import_beancount(&venv_python) {
      return Some(PythonScriptChecker::new(venv_python));
    }

    find_python_with_beancount().map(PythonScriptChecker::new)
  }

  fn write_ledger(dir: &Path, name: &str, content: &str) -> PathBuf {
    let path = dir.join(name);
    fs::write(&path, content).unwrap();
    path
  }

  #[test]
  fn reports_balance_assertion_failure() {
    let Some(checker) = beancount_checker() else {
      eprintln!("skipping: no python interpreter with beancount installed");
      return;
    };

    let dir = tempfile::tempdir().unwrap();
    let accounts = write_ledger(
      dir.path(),
      "accounts.bean",
      &lines(&[
        r#"2022-01-01 open Assets:Cash"#,
        r#"2022-01-01 open Expenses:Food"#,
        r#"2022-01-02 * "lunch""#,
        r#"  Assets:Cash -10 USD"#,
        r#"  Expenses:Food 10 USD"#,
        r#"2022-01-03 balance Assets:Cash 0 USD"#,
      ]),
    );
    let root = write_ledger(dir.path(), "main.bean", r#"include "accounts.bean""#);

    let diags = checker
      .run(root.to_str().unwrap())
      .expect("checker should succeed");

    assert_eq!(diags.len(), 1, "{diags:?}");
    let diag = &diags[0];
    assert!(
      diag.message.contains("Balance failed for 'Assets:Cash'"),
      "{diag:?}"
    );
    // beancount reports 1-based line numbers, the checker stores 0-based ones.
    assert_eq!(diag.lineno, Some(5));
    assert_eq!(
      diag.filename.as_deref().map(Path::new),
      Some(accounts.as_path())
    );
  }

  #[test]
  fn reports_nothing_for_valid_ledger() {
    let Some(checker) = beancount_checker() else {
      eprintln!("skipping: no python interpreter with beancount installed");
      return;
    };

    let dir = tempfile::tempdir().unwrap();
    let root = write_ledger(
      dir.path(),
      "main.bean",
      &lines(&[
        r#"2022-01-01 open Assets:Cash"#,
        r#"2022-01-01 open Expenses:Food"#,
        r#"2022-01-02 * "lunch""#,
        r#"  Assets:Cash -10 USD"#,
        r#"  Expenses:Food 10 USD"#,
        r#"2022-01-03 balance Assets:Cash -10 USD"#,
      ]),
    );

    let diags = checker
      .run(root.to_str().unwrap())
      .expect("checker should succeed");

    assert!(diags.is_empty(), "{diags:?}");
  }

  #[test]
  fn missing_ledger_is_reported_without_line() {
    let Some(checker) = beancount_checker() else {
      eprintln!("skipping: no python interpreter with beancount installed");
      return;
    };

    let diags = checker
      .run("/nonexistent/main.bean")
      .expect("beancount reports a missing ledger as an error entry");

    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(
      diags[0].message.contains("does not exist"),
      "{:?}",
      diags[0]
    );
    // beancount uses line 0 for file-level errors; it must survive the 0-based conversion.
    assert_eq!(diags[0].lineno, Some(0));
  }
}
