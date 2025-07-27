use ariadne::{Color, Label, Report, ReportKind};
use chumsky::prelude::Rich;

pub fn report_syntax_errors<T: std::clone::Clone + ToString>(errors: Vec<Rich<T>>, source: &str) {
    errors
        .iter()
        .map(|e| e.clone().map_token(|c| c.to_string()))
        .for_each(|e| {
            Report::build(ReportKind::Error, e.span().into_range())
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                .with_message(e.to_string())
                .with_label(
                    Label::new(e.span().into_range())
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new(span.into_range())
                        .with_message(format!("while parsing this {label}"))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .print(&ariadne::Source::from(source))
                .unwrap()
        });
}
