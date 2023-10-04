//! A [`syn::Error`] wrapper that provides pretty diagnostic messages using [`miette`].
//!
//! ```text
//!   × expected identifier
//!    ╭─[1:1]
//!  1 │
//!  2 │ pub struct {
//!    ·           ┬
//!    ·           ╰── expected identifier
//!  3 │     num_yaks: usize
//!    ╰────
//! ```
//!
//! # Usage
//! ```
//! let source = r"
//! pub struct {
//!     num_yaks: usize
//! }";
//!
//! let error = syn::parse_str::<syn::DeriveInput>(source).unwrap_err();
//! let error = syn_miette::Error::new(error, source);
//!
//! let rendered = render(error); // See miette documentation for usage
//!
//! println!("{}", rendered);
//!
//! # fn render(error: syn_miette::Error) -> String { String::new() }
//! ```
//!
//!
//! Notably, [`Error`] properly renders children that have been [`syn::Error::combine`]-ed:
//! ```text
//!  × duplicate definition of `Foo`
//!   ╭─[1:1]
//! 1 │ struct Foo;
//!   ·        ─┬─
//!   ·         ╰── initial definition here
//! 2 │ enum Bar {}
//! 3 │ union Foo {}
//!   ·       ─┬─
//!   ·        ╰── duplicate definition of `Foo`
//!   ╰────
//! ```

use std::{fmt, sync::Arc};

use miette::{
    Diagnostic, LabeledSpan, MietteSpanContents, SourceCode, SourceOffset, SourceSpan, SpanContents,
};

/// A [`syn::Error`] wrapper that provides pretty diagnostic messages.
///
/// See the [module documentation](mod@self) for more.
#[derive(Debug, Clone)]
pub struct Error {
    source_code: MaybeNamedSource<Arc<str>>,
    syn_error: syn::Error,
}

impl Error {
    /// Create an error without a filename.
    ///
    /// ```text
    ///   ╭────
    /// 1 │ struct Foo
    ///   · ▲
    ///   · ╰── unexpected end of input, expected one of: `where`, parentheses, curly braces, `;`
    ///   ╰────
    /// ```
    ///
    /// Note: if the source code and the [`syn::Error`] don't correlate, then
    /// [rendering](miette::ReportHandler) will be incorrect, and may fail.
    ///
    /// This is because the [`syn::Error::span`] may be out-of-bounds.
    pub fn new(syn_error: syn::Error, source_code: impl Into<Arc<str>>) -> Self {
        Self {
            source_code: MaybeNamedSource {
                file_name: None,
                source_code: source_code.into(),
            },
            syn_error,
        }
    }
    /// Create an error with a filename for the source code.
    ///
    /// ```text
    ///   ╭─[/path/to/file:1:1]
    /// 1 │ struct Foo
    ///   · ▲
    ///   · ╰── unexpected end of input, expected one of: `where`, parentheses, curly braces, `;`
    ///   ╰────
    /// ```
    ///
    /// Note: if the source code and the [`syn::Error`] don't correlate, then
    /// [rendering](miette::ReportHandler) will be incorrect, and may fail.
    ///
    /// This is because the [`syn::Error::span`] may be out-of-bounds.
    pub fn new_named(
        syn_error: syn::Error,
        source_code: impl Into<Arc<str>>,
        file_name: impl fmt::Display,
    ) -> Self {
        Self {
            source_code: MaybeNamedSource {
                file_name: Some(file_name.to_string()),
                source_code: source_code.into(),
            },
            syn_error,
        }
    }
    /// Get a reference to the source code.
    pub fn source_code(&self) -> &Arc<str> {
        &self.source_code.source_code
    }
    /// Get an exclusive reference to the [`syn::Error`], for e.g calling [`syn::Error::combine`]
    pub fn get_mut(&mut self) -> &mut syn::Error {
        &mut self.syn_error
    }
    /// Convert this back to the original [`syn::Error`], discarding the source code.
    pub fn into_inner(self) -> syn::Error {
        self.into()
    }
}

impl From<Error> for syn::Error {
    fn from(value: Error) -> Self {
        value.syn_error
    }
}

impl Diagnostic for Error {
    fn source_code(&self) -> Option<&dyn SourceCode> {
        Some(&self.source_code as &dyn SourceCode)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        Some(Box::new((&self.syn_error).into_iter().map(
            move |syn_error| {
                let span = syn_error.span();
                let span_start = span.start();
                let span_end = span.end();
                let start_offset = SourceOffset::from_location(
                    &self.source_code.source_code,
                    span_start.line,
                    span_start.column + 1,
                );
                let end_offset = SourceOffset::from_location(
                    &self.source_code.source_code,
                    span_end.line,
                    span_end.column + 1,
                );
                let length = SourceOffset::from(end_offset.offset() - start_offset.offset());
                LabeledSpan::new_with_span(
                    Some(syn_error.to_string()),
                    SourceSpan::new(start_offset, length),
                )
            },
        )))
    }
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.syn_error.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MaybeNamedSource<T> {
    file_name: Option<String>,
    source_code: T,
}

impl<T> SourceCode for MaybeNamedSource<T>
where
    T: SourceCode,
{
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, miette::MietteError> {
        let contents =
            self.source_code
                .read_span(span, context_lines_before, context_lines_after)?;
        let data = contents.data();
        let span = *contents.span();
        let line = contents.line();
        let column = contents.column();
        let line_count = contents.line_count();
        match self.file_name.clone() {
            Some(name) => Ok(Box::new(MietteSpanContents::new_named(
                name, data, span, line, column, line_count,
            ))),
            None => Ok(Box::new(MietteSpanContents::new(
                data, span, line, column, line_count,
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use miette::{GraphicalReportHandler, GraphicalTheme};
    use proc_macro2::{Ident, Span};
    use syn::{parse::Parse, parse::ParseStream, DeriveInput};

    use super::*;

    enum Behaviour {
        IncludeSource,
        IncludeFilename,
    }

    #[test]
    fn basic_parse() {
        insta::assert_snapshot!(test_parse::<DeriveInput>(
            "struct Foo",
            Behaviour::IncludeSource
        ));
        insta::assert_snapshot!(test_parse::<DeriveInput>(
            "struct Foo",
            Behaviour::IncludeFilename
        ));
    }

    #[test]
    fn call_site() {
        insta::assert_snapshot!(render(Error::new(
            syn::Error::new(Span::call_site(), "the whole thing is fucked"),
            "this is the source code"
        )));
        insta::assert_snapshot!(render(Error::new(
            syn::Error::new(Span::call_site(), "the whole thing is fucked"),
            "this is the source code\nand it's fucked on multiple\nlines"
        )));
    }

    #[test]
    fn combined() {
        insta::assert_snapshot!(test_parse::<UniqueDeriveInputs>(
            "struct Foo;\nenum Bar {}\nunion Foo {}",
            Behaviour::IncludeSource
        ));
    }

    struct UniqueDeriveInputs {}

    impl Parse for UniqueDeriveInputs {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let mut seen = HashMap::<Ident, DeriveInput>::new();
            while !input.is_empty() {
                let parsed = input.parse::<DeriveInput>()?;
                match seen.remove(&parsed.ident) {
                    Some(duplicate) => {
                        let mut root = syn::Error::new(
                            parsed.ident.span(),
                            format!("duplicate definition of `{}`", parsed.ident),
                        );
                        root.combine(syn::Error::new(
                            duplicate.ident.span(),
                            "initial definition here",
                        ));
                        return Err(root);
                    }
                    None => {
                        seen.insert(parsed.ident.clone(), parsed);
                    }
                }
            }
            Ok(Self {})
        }
    }

    #[track_caller]
    fn test_parse<T: Parse>(source_code: &str, behaviour: Behaviour) -> String {
        let Err(error) = syn::parse_str::<T>(source_code) else {
            panic!("parsing succeeded where it was expected to fail")
        };
        let error = match behaviour {
            Behaviour::IncludeSource => Error::new(error, source_code),
            Behaviour::IncludeFilename => Error::new_named(error, source_code, "/path/to/file"),
        };
        render(error)
    }

    fn render(error: Error) -> String {
        let renderer =
            GraphicalReportHandler::new_themed(GraphicalTheme::unicode_nocolor()).with_width(80);
        let mut out = String::new();
        renderer
            .render_report(&mut out, &error)
            .expect("failed to render error");
        out
    }
}
