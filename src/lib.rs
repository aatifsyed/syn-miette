//! A [`syn::Error`] wrapper that provides pretty diagnostic messages using [`miette`].
//!
//! # Usage
//! ```rust
//! let source = r"
//! pub struct {
//!     num_yaks: usize
//! }";
//!
//! let error = syn::parse_str::<syn::DeriveInput>(source).unwrap_err();
//! let error = syn_miette::Error::new(error, source);
//!
//! assert_eq!(
//!     error.render(), // only with `--feature render`
//! "  × expected identifier
//!    ╭─[2:12]
//!  1 │ 
//!  2 │ pub struct {
//!    ·            ┬
//!    ·            ╰── expected identifier
//!  3 │     num_yaks: usize
//!    ╰────
//! "
//! );
//! ```
//!
//!
//! Notably, [`Error`] properly renders children that have been [`syn::Error::combine`]-ed:
//! ```text
#![doc = include_str!("doc-snapshots/multiple")]
//! ```

#![allow(rustdoc::redundant_explicit_links)] // required for cargo-rdme
#![cfg_attr(do_doc_cfg, feature(doc_cfg))]

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
    ///   ╭────[1:1]
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
    /// Get a shared reference to the [`syn::Error`].
    pub fn get(&mut self) -> &syn::Error {
        &self.syn_error
    }
    /// Get an exclusive reference to the [`syn::Error`], for e.g calling [`syn::Error::combine`].
    pub fn get_mut(&mut self) -> &mut syn::Error {
        &mut self.syn_error
    }
    /// Convert this back to the original [`syn::Error`], discarding the source code.
    pub fn into_inner(self) -> syn::Error {
        self.into()
    }
    /// Convenience method for fancy-rendering this error with [`miette::GraphicalTheme::unicode_nocolor`].
    /// # Panics
    /// - if [`miette::GraphicalReportHandler::render_report`] fails.
    #[cfg_attr(do_doc_cfg, doc(cfg(feature = "render")))]
    #[cfg(feature = "render")]
    pub fn render(&self) -> String {
        use miette::{GraphicalReportHandler, GraphicalTheme};
        let mut s = String::new();
        GraphicalReportHandler::new_themed(GraphicalTheme::unicode_nocolor())
            .with_width(80)
            .render_report(&mut s, self)
            .unwrap();
        s
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
                let length = end_offset.offset() - start_offset.offset();
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

#[cfg(all(test, feature = "render"))]
mod tests {
    use std::{collections::HashMap, fs, path::Path};

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
        insta::assert_snapshot!(Error::new(
            syn::Error::new(Span::call_site(), "the whole thing is fucked"),
            "this is the source code"
        )
        .render());
        insta::assert_snapshot!(Error::new(
            syn::Error::new(Span::call_site(), "the whole thing is fucked"),
            "this is the source code\nand it's fucked on multiple\nlines"
        )
        .render());
    }

    #[test]
    fn combined() {
        insta::assert_snapshot!(test_parse::<UniqueDeriveInputs>(
            "struct Foo;\nenum Bar {}\nunion Foo {}",
            Behaviour::IncludeSource
        )
        .and_doc_snapshot("multiple"));
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
        error.render()
    }

    trait AndDocSnapshot: AsRef<str> {
        #[track_caller]
        fn and_doc_snapshot(&self, name: &str) -> &Self {
            let path = Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("src/doc-snapshots")
                .join(name);
            match fs::read_to_string(&path)
                .map(|it| it == self.as_ref())
                .unwrap_or(false)
            {
                true => {}
                false => match fs::write(path, self.as_ref()) {
                    Ok(()) => panic!(
                        "doc snapshot {} was out of date - a new one has been written",
                        name
                    ),
                    Err(e) => panic!(
                        "doc snapshot {} was out of date, and a new one couldn't be written: {}",
                        name, e
                    ),
                },
            }

            self
        }
    }
    impl<T> AndDocSnapshot for T where T: AsRef<str> {}
}
