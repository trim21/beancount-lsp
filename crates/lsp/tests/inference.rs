use beancount_lsp::build_document;
use std::str::FromStr;
use tower_lsp_server::ls_types::Uri as Url;

fn uri() -> Url {
  Url::from_str("file:///test.bean").unwrap()
}

fn build_doc(text: &str) -> beancount_lsp::Document {
  build_document(text.to_string(), uri().as_str()).expect("build_document")
}

/// When one posting declares a currency (CNY) but other postings in the same
/// transaction have amounts without currency, the inference engine should
/// derive the currency for those postings from the context.
#[test]
fn infers_currency_from_sibling_posting() {
  let text = [
    r##"2022-01-01 * "Payee" "Narration""##,
    r#"  Assets:Cash1 1"#,
    r#"  Assets:Cash2 2 CNY"#,
  ]
  .join("\n");

  let doc = build_doc(&text);

  // Currency of Assets:Cash1 should be inferred as CNY from Assets:Cash2.
  let currency_errors: Vec<_> = doc
    .inferrence_errors
    .iter()
    .filter(|e| e.message.contains("missing a currency"))
    .collect();
  assert!(
    currency_errors.is_empty(),
    "currency should be inferred from sibling posting, got {} errors",
    currency_errors.len(),
  );
}

#[test]
fn infers_currency_for_multiple_postings_without_currency() {
  let text = [
    r##"2022-01-01 * "Payee" "Narration""##,
    r#"  Assets:Cash1 1"#,
    r#"  Assets:Cash2 2 CNY"#,
    r#"  Assets:Cash3 -3"#,
  ]
  .join("\n");

  let doc = build_doc(&text);

  // All three postings should be inferred with CNY and balance to zero.
  assert!(
    doc.inferrence_errors.is_empty(),
    "expected no inference errors, got: {:?}",
    doc.inferrence_errors
      .iter()
      .map(|e| &e.message)
      .collect::<Vec<_>>()
  );

  // Check that all inferred postings have CNY.
  for inferred in &doc.inferred {
    if let beancount_core::inference::InferredDirective::Transaction(tx) = inferred {
      assert_eq!(tx.postings.len(), 3);
      for p in &tx.postings {
        assert_eq!(
          p.amount.currency, "CNY",
          "expected CNY for posting {}, got {}",
          p.account, p.amount.currency
        );
      }
      // Verify balanced: 1 + 2 + (-3) = 0
      let numbers: Vec<&str> = tx
        .postings
        .iter()
        .map(|p| p.amount.raw.as_str())
        .collect();
      assert!(
        numbers.contains(&"1") && numbers.contains(&"2") && numbers.contains(&"-3"),
        "expected numbers [1, 2, -3], got {numbers:?}"
      );
    }
  }
}

/// When no posting declares a currency, MissingCurrency errors should still be
/// reported (no currency to infer from).
#[test]
fn errors_when_no_currency_in_any_posting() {
  let text = [
    r##"2022-01-01 * "Payee" "Narration""##,
    r#"  Assets:Cash 1"#,
    r#"  Expenses:Food -1"#,
  ]
  .join("\n");

  let doc = build_doc(&text);

  // No currency declared → should still get MissingCurrency errors.
  assert!(
    !doc.inferrence_errors.is_empty(),
    "should error when no currency is declared in any posting"
  );
}
