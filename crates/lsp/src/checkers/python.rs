#![allow(dead_code)]

use super::{Checker, CheckerDiagnostic};
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyAnyMethods};

// Run beancount's Python loader to surface its native errors.
#[derive(Default)]
pub struct PythonChecker;

impl Checker for PythonChecker {
  fn name(&self) -> &'static str {
    "pyo3"
  }

  fn run(&self, root_file: &str) -> Result<Vec<CheckerDiagnostic>, String> {
    Python::attach(|py| {
      let loader = py.import("beancount.loader")?;
      let parsed = loader.call_method("load_file", (root_file,), None)?;
      let errors = parsed.get_item(1)?;
      let errors: Vec<Py<PyAny>> = errors.extract()?;

      let mut diagnostics = Vec::with_capacity(errors.len());
      for error in errors {
        let error = error.into_bound(py);
        match error_to_diagnostic(error) {
          Ok(diagnostic) => diagnostics.push(diagnostic),
          Err(err) => tracing::warn!("failed to convert Python error: {err}"),
        }
      }

      Ok(diagnostics)
    })
    .map_err(|e: PyErr| e.to_string())
  }
}

fn error_to_diagnostic(error: Bound<'_, PyAny>) -> PyResult<CheckerDiagnostic> {
  let message: String = error.getattr("message")?.extract()?;

  let (lineno, filename) = {
    let entry_meta = error
      .getattr("entry")
      .ok()
      .and_then(|entry| entry.getattr("meta").ok());

    let lineno = error
      .getattr("lineno")
      .ok()
      .and_then(|line| line.extract::<usize>().ok())
      .or_else(|| {
        entry_meta
          .as_ref()
          .and_then(|meta| meta.get_item("lineno").ok())
          .and_then(|line| line.extract::<usize>().ok())
      })
      .map(|line| line.saturating_sub(1) as u32)
      .unwrap_or(0);

    let filename = error
      .getattr("filename")
      .ok()
      .and_then(|value| value.extract::<String>().ok())
      .filter(|name| !name.is_empty())
      .or_else(|| {
        entry_meta
          .as_ref()
          .and_then(|meta| meta.get_item("filename").ok())
          .and_then(|value| value.extract::<String>().ok())
          .filter(|name| !name.is_empty())
      });

    (lineno, filename)
  };

  let message = match filename.as_ref() {
    Some(name) => format!("{name}: {message}"),
    None => message,
  };

  Ok(CheckerDiagnostic {
    message,
    filename,
    lineno: Some(lineno),
  })
}
