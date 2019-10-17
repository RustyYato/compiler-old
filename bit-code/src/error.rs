pub type Result<'alloc, 'input, T, E> = std::result::Result<T, Error<'alloc, 'input, E>>;

impl<'alloc, 'input, I> From<parser_ext::error::Error<'alloc, 'input, I>>
    for Error<'alloc, 'input, I>
{
    fn from(err: parser_ext::error::Error<'alloc, 'input, I>) -> Self {
        Self::Lex(err)
    }
}

// FIXME: If we start storing Error types, it may be best to box `MissingArg`
// to reduce the size of `Error`
#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq)]
pub enum Error<'alloc, 'input, I> {
    ExpectedValue,
    ExpectedPattern,
    BindingNotFound(&'input [u8]),
    Lex(parser_ext::error::Error<'alloc, 'input, I>),
}
