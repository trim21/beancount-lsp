use pyo3::prelude::*;
use pyo3::types::PyAny;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

// Run beancount's Python loader to surface its native errors.
pub fn diagnostics(filename: &str) -> PyResult<Vec<Diagnostic>> {
    Python::with_gil(|py| {
        let loader = py.import("beancount.loader")?;
        let parsed = loader.call_method("load_file", (filename,), None)?;
        let errors = parsed.get_item(1)?;
        let errors: Vec<Py<PyAny>> = errors.extract()?;

        let mut diagnostics = Vec::with_capacity(errors.len());
        for error in errors {
            match error_to_diagnostic(py, error.as_ref(py)) {
                Ok(diagnostic) => diagnostics.push(diagnostic),
                Err(err) => tracing::warn!("failed to convert Python error: {err}"),
            }
        }

        Ok(diagnostics)
    })
}

fn error_to_diagnostic(py: Python<'_>, error: &PyAny) -> PyResult<Diagnostic> {
    let message: String = error.getattr("message")?.extract()?;

    let lineno = error
        .getattr("lineno")
        .ok()
        .and_then(|line| line.extract::<usize>().ok())
        .or_else(|| {
            error
                .getattr("entry")
                .ok()
                .and_then(|entry| entry.getattr("meta").ok())
                .and_then(|meta| meta.get_item("lineno").ok())
                .and_then(|line| line.extract::<usize>().ok())
        })
        .map(|line| line.saturating_sub(1) as u32)
        .unwrap_or(0);

    Ok(Diagnostic {
        range: Range {
            start: Position::new(lineno, 0),
            end: Position::new(lineno, 0),
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("beancount (python)".to_owned()),
        message,
        ..Diagnostic::default()
    })
}
