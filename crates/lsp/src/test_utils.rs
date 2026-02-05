/// Join multiple raw Beancount lines into a single `String`, trimming only the trailing whitespace
/// each caller line might include for alignment.
pub fn lines(lines: &[&str]) -> String {
    lines
        .iter()
        .map(|v| v.trim_end())
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::lines;

    #[test]
    fn trims_and_joins_lines() {
        let input = lines(&[
            r#"2022-01-01 * "..." "..." #food  "#,
            r#"  Assets:Cash -10 USD               "#,
            r#"  Expenses:Food                     "#,
        ]);

        let expected =
            "2022-01-01 * \"...\" \"...\" #food  \n  Assets:Cash -10 USD\n  Expenses:Food";
        assert_eq!(input, expected);
    }
}
