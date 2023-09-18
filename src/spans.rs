#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub unspanned: T,
    pub span: Span,
}
impl<T> Spanned<T> {
    pub fn new(unspanned: T, span: Span) -> Self {
        Self { unspanned, span }
    }
}
pub type Span = (usize, usize);
pub trait ToSpanned {
    fn to_spanned(self, span: Span) -> Spanned<Self>
    where
        Self: Sized,
    {
        Spanned::new(self, span)
    }
}

impl<T> ToSpanned for T {}
