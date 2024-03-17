use ariadne::{Color, Label, Report, ReportKind, Source};

fn main() {
    let source = "hello my name is عاطف and I like to 𓁀 all day and 🕺 all night";
    let spanned = source.chars().enumerate().collect::<Vec<_>>();
    let words = spanned.split(|(_ix, char)| char.is_whitespace());
    Report::build(ReportKind::Custom("info", Color::Blue), (), 0)
        .with_labels(words.enumerate().map(|(word_ix, word)| {
            let first_char_ix = word.first().unwrap().0;
            let last_char_ix = word.last().unwrap().0;
            Label::new(first_char_ix..last_char_ix + 1)
                .with_message(format!("this is word {}", word_ix))
        }))
        .finish()
        .print(Source::from(source))
        .unwrap();
}
