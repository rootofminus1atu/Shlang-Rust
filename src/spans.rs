#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub unspanned: T,
    pub span: Span,
}
impl<T> Spanned<T> {
    pub fn new(unspanned: T, span: Span) -> Self {
        Self { unspanned, span }
    }
    pub fn box_unspanned(self) -> Spanned<Box<T>> {
        return Spanned::new(Box::new(self.unspanned), self.span);
    }
}
impl<T> From<(T, Span)> for Spanned<T> {
    fn from(value: (T, Span)) -> Self {
        Self::new(value.0, value.1)
    }
}
pub type Span = (usize, usize);
