use crate::ast::Global;
use crate::Spanned;

#[derive(Debug)]
pub struct Program<'a> {
    pub items: Vec<Spanned<Global<'a>>>,
}

impl<'a> Program<'a> {
    pub fn new(items: Vec<Spanned<Global<'a>>>) -> Self {
        Self { items: items }
    }
}
