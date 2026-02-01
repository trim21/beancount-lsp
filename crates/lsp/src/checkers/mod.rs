#![allow(dead_code)]

#[cfg(feature = "pyo3")]
pub mod python;
pub mod python_script;

pub trait Checker: Send + Sync {
    fn name(&self) -> &'static str;
    fn run(&self, root_file: &str) -> Result<Vec<CheckerDiagnostic>, String>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckerDiagnostic {
    pub message: String,
    pub filename: Option<String>,
    pub lineno: Option<u32>, // zero-based line number when known
}

/// Prefer the native pyo3 checker when available; otherwise use the external script checker.
#[cfg(feature = "pyo3")]
pub fn create() -> Option<Box<dyn Checker>> {
    Some(Box::new(python::PythonChecker))
}

#[cfg(not(feature = "pyo3"))]
pub fn create() -> Option<Box<dyn Checker>> {
    python_script::find_python_with_beancount()
        .map(|python| Box::new(python_script::PythonScriptChecker::new(python)) as Box<dyn Checker>)
}
