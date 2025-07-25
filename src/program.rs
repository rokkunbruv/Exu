use crate::ast::ProgramItem;
use crate::Spanned;

#[derive(Debug)]
pub struct Program<'a> {
    pub items: Vec<Spanned<ProgramItem<'a>>>,
}

impl<'a> Program<'a> {
    pub fn new(items: Vec<Spanned<ProgramItem<'a>>>) -> Self {
        Self { items: items }
    }
}
