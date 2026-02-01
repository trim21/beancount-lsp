use pyo3::exceptions::PyRuntimeError;
use pyo3::prelude::*;

#[pyfunction]
fn main(py: Python<'_>, argv: Vec<String>) -> PyResult<()> {
    py.detach(|| {
        beancount_lsp::run_server_blocking(argv)
            .map_err(|err| PyRuntimeError::new_err(err.to_string()))
    })
}

#[pymodule]
fn _lsp(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(main, m)?)?;
    Ok(())
}
