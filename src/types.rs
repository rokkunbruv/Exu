#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Num,
    Str,
    Bool,
    Fn {
        params: Vec<Self>,
        ret_type: Box<Self>,
    },
    Procedure {
        params: Vec<Self>,
        ret: Option<Box<Self>>,
    },
    None,
}
