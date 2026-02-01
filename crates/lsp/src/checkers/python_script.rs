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
                    // beancount reports human-friendly (1-based) line numbers; keep them as-is and
                    // let the LSP layer treat missing values as 0.
                    lineno: lineno.map(|v| v - 1),
                    filename: filename.filter(|name| !name.is_empty()),
                }
            })
            .collect();

        Ok(diagnostics)
    }
}
