use std::{fmt, sync::Arc};

use miette::{
    Diagnostic, LabeledSpan, MietteSpanContents, SourceCode, SourceOffset, SourceSpan, SpanContents,
};

#[derive(Debug, Clone)]
pub struct Error {
    source_code: Option<MaybeNamedSource<Arc<str>>>,
    syn_error: syn::Error,
}

impl Error {
    pub fn new(syn_error: syn::Error, source_code: impl Into<Arc<str>>) -> Self {
        Self {
            source_code: Some(MaybeNamedSource {
                file_name: None,
                source_code: source_code.into(),
            }),
            syn_error,
        }
    }
    pub fn new_named(
        syn_error: syn::Error,
        source_code: impl Into<Arc<str>>,
        file_name: impl fmt::Display,
    ) -> Self {
        Self {
            source_code: Some(MaybeNamedSource {
                file_name: Some(file_name.to_string()),
                source_code: source_code.into(),
            }),
            syn_error,
        }
    }
}

impl From<syn::Error> for Error {
    fn from(syn_error: syn::Error) -> Self {
        Self {
            source_code: None,
            syn_error,
        }
    }
}

impl From<Error> for syn::Error {
    fn from(value: Error) -> Self {
        value.syn_error
    }
}

impl Diagnostic for Error {
    fn source_code(&self) -> Option<&dyn SourceCode> {
        self.source_code.as_ref().map(|it| it as &dyn SourceCode)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        match &self.source_code {
            Some(MaybeNamedSource {
                file_name: _,
                source_code,
            }) => Some(Box::new((&self.syn_error).into_iter().map(
                move |syn_error| {
                    let span = syn_error.span();
                    let span_start = span.start();
                    let span_end = span.end();
                    let start_offset = SourceOffset::from_location(
                        source_code,
                        span_start.line,
                        span_start.column,
                    );
                    let end_offset =
                        SourceOffset::from_location(source_code, span_end.line, span_end.column);
                    let length = SourceOffset::from(end_offset.offset() - start_offset.offset());
                    LabeledSpan::new_with_span(
                        Some(syn_error.to_string()),
                        SourceSpan::new(start_offset, length),
                    )
                },
            ))),
            None => None,
        }
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
        dbg!(&span, &context_lines_before, &context_lines_after);
        let contents =
            self.source_code
                .read_span(span, context_lines_before, context_lines_after)?;
        dbg!("read contents");
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
    use std::collections::HashSet;

    use miette::{GraphicalReportHandler, GraphicalTheme};
    use proc_macro2::{Ident, Span};
    use syn::{parse::Parse, parse::ParseStream, DeriveInput};

    use super::*;

    enum Behaviour {
        Bare,
        IncludeSource,
        IncludeFilename,
    }

    #[test]
    fn basic_parse() {
        insta::assert_snapshot!(test_parse::<DeriveInput>("struct Foo", Behaviour::Bare));
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
        insta::assert_snapshot!(render(Error::from(syn::Error::new(
            Span::call_site(),
            "the whole thing is fucked"
        ))));
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
        insta::assert_snapshot!(test_parse::<UniqueIdents>(
            "hello\nworld\nhello",
            Behaviour::IncludeSource
        ));
        insta::assert_snapshot!(test_parse::<UniqueIdents>(
            "hello\nworld\nhello",
            Behaviour::Bare
        ));
        insta::assert_snapshot!(test_parse::<UniqueIdents>(
            "hello\nworld\nhello",
            Behaviour::IncludeFilename
        ));
    }

    struct UniqueIdents {
        _idents: HashSet<Ident>,
    }

    impl Parse for UniqueIdents {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let mut seen = HashSet::new();
            while !input.is_empty() {
                let ident = input.parse::<Ident>()?;
                let mut duplicate = syn::Error::new(ident.span(), "duplicate ident found");
                if let Some(first) = seen.replace(ident) {
                    duplicate.combine(syn::Error::new(first.span(), "first definition here"));
                    return Err(duplicate);
                }
            }
            Ok(Self { _idents: seen })
        }
    }

    #[track_caller]
    fn test_parse<T: Parse>(source_code: &str, behaviour: Behaviour) -> String {
        let Err(error) = syn::parse_str::<T>(source_code) else {
            panic!("parsing succeeded where it was expected to fail")
        };
        let error = match behaviour {
            Behaviour::Bare => Error::from(error),
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
