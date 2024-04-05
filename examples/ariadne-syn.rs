use ariadne::{Color, Label, Report, ReportKind, Source};
use proc_macro2::{LineColumn, Span};
use ropey::Rope;
use std::{error::Error, fmt, ops::Range};

fn main() -> Result<(), Box<dyn Error>> {
    let source_text = std::fs::read_to_string("/dev/stdin")?;
    let source_rope = Rope::from_str(&source_text);

    let ast = syn::parse_str::<syn::DeriveInput>(&source_text)?;

    Report::build(ReportKind::Custom("info", Color::Blue), (), 0)
        .with_label(
            Label::new(conv_span(&source_rope, ast.ident.span())).with_message("this is the ident"),
        )
        .finish()
        .print(Source::from(source_text))?;

    Ok(())
}

fn conv_span(source_text: &Rope, span: Span) -> Range<usize> {
    let num_lines = source_text.len_lines();
    assert!(
        num_lines >= span.end().line,
        "span exceeds end of file. span is {} but there are {} lines",
        FmtSpan(span),
        num_lines
    );

    let start = span.start();
    assert_column(source_text, start, span);
    let start = source_text.line_to_char(start.line - 1) + start.column;

    let end = span.end();
    assert_column(source_text, end, span);
    let end = source_text.line_to_char(end.line - 1) + end.column;

    start..end
}
fn assert_column(source_text: &Rope, coord: LineColumn, span: Span) {
    let num_cols = source_text.line(coord.line - 1).len_chars();
    assert!(
        num_cols >= coord.column,
        "span exceeds end of line. span is {} but there are {} columns",
        FmtSpan(span),
        num_cols
    );
}

struct FmtSpan(Span);

impl fmt::Display for FmtSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let start = self.0.start();
        let end = self.0.end();
        f.write_fmt(format_args!(
            "{}:{}..{}:{}",
            start.line, start.column, end.line, end.column
        ))
    }
}
