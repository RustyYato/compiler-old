pub mod ast {
    use lexer_ext::token::Token;

    pub type AstPtr<'alloc, 'input> = &'alloc mut Ast<'alloc, 'input>;

    #[derive(Debug, PartialEq)]
    pub enum Ast<'alloc, 'input> {
        Uninit,
        SemiColon(Token<'input>),
        Value(Token<'input>),
        Call(Vec<Ast<'alloc, 'input>>),
        Block {
            open: Token<'input>,
            inner: Vec<Ast<'alloc, 'input>>,
            close: Token<'input>,
        },
        Loop {
            loop_kw: Token<'input>,
            open: Token<'input>,
            inner: Vec<Ast<'alloc, 'input>>,
            close: Token<'input>,
        },
        Group {
            open: Token<'input>,
            inner: AstPtr<'alloc, 'input>,
            close: Token<'input>,
        },
        PostOp {
            expr: AstPtr<'alloc, 'input>,
            op: Token<'input>,
        },
        PreOp {
            op: Token<'input>,
            expr: AstPtr<'alloc, 'input>,
        },
        BinOp {
            left: AstPtr<'alloc, 'input>,
            op: Token<'input>,
            right: AstPtr<'alloc, 'input>,
        },
        Match {
            match_kw: Token<'input>,
            cond: AstPtr<'alloc, 'input>,
            open: Token<'input>,
            patterns: Option<AstPtr<'alloc, 'input>>,
            close: Token<'input>,
        },
        Items {
            values: Vec<Ast<'alloc, 'input>>,
            commas: Vec<Token<'input>>,
        },
    }

    use std::fmt;
    impl fmt::Display for Ast<'_, '_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::SemiColon(token) => write!(f, "{}", token),
                Self::Value(token) => write!(f, "{}", token),
                Self::Call(items) => {
                    write!(f, "(")?;

                    for (i, ast) in items.iter().enumerate() {
                        if i != 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{}", ast)?
                    }

                    write!(f, ")")
                }
                Self::Block { open, inner, close } => {
                    write!(f, "{}", open)?;

                    for ast in inner {
                        write!(f, "{}", ast)?
                    }

                    write!(f, "{}", close)
                }
                Self::Loop {
                    loop_kw,
                    open,
                    inner,
                    close,
                } => {
                    write!(f, "{}{}", loop_kw, open)?;

                    for ast in inner {
                        write!(f, "{}", ast)?
                    }

                    write!(f, "{}", close)
                }
                Self::Group { open, inner, close } => write!(f, "{}{}{}", open, inner, close),
                Self::PostOp { expr, op } => write!(f, "({}{})", expr, op),
                Self::PreOp { expr, op } => write!(f, "({}{})", op, expr),
                Self::BinOp { left, op, right } => write!(f, "({}{}{})", left, op, right),
                Self::Match {
                    match_kw,
                    cond,
                    open,
                    patterns: None,
                    close,
                } => write!(f, "({} {}{}{})", match_kw, cond, open, close),
                Self::Match {
                    match_kw,
                    cond,
                    open,
                    patterns: Some(patterns),
                    close,
                } => write!(f, "({} {}{}{}{})", match_kw, cond, open, patterns, close),
                Self::Items { values, commas } => {
                    let mut values = values.iter();
                    let mut commas = commas.iter();

                    write!(f, "(")?;

                    loop {
                        if let Some(x) = values.next() {
                            write!(f, "{}", x)?
                        }

                        match commas.next() {
                            Some(x) => write!(f, "{}", x)?,
                            None => break,
                        }
                    }

                    write!(f, ")")?;

                    Ok(())
                }
                Self::Uninit => unreachable!(),
            }
        }
    }

    pub trait Parser<'alloc, 'input: 'alloc, A: ?Sized>
    where
        A: arena::Allocator<Item = Ast<'alloc, 'input>>,
    {
        type Input;

        fn parse(
            &mut self,
            alloc: &'alloc A,
        ) -> crate::error::AstResult<'alloc, 'input, Self::Input>;
    }

    pub struct WithAllocator<'alloc, P, A: ?Sized> {
        inner: P,
        allocator: &'alloc A,
    }

    impl<'alloc, 'input: 'alloc, P, A: ?Sized> WithAllocator<'alloc, P, A>
    where
        A: arena::Allocator<Item = Ast<'alloc, 'input>>,
        P: Parser<'alloc, 'input, A>,
    {
        pub fn new(inner: P, allocator: &'alloc A) -> Self {
            Self { inner, allocator }
        }

        pub fn parse(
            &mut self,
        ) -> crate::error::Result<'alloc, 'input, &'alloc mut Ast<'alloc, 'input>, P::Input>
        {
            let ast = self.inner.parse(self.allocator)?;
            Ok(self.allocator.alloc(ast))
        }
    }
}

pub mod error {
    use lexer_ext::token::Token;

    pub type AstResult<'alloc, 'input, E> =
        std::result::Result<crate::ast::Ast<'alloc, 'input>, Error<'alloc, 'input, E>>;
    pub type Result<'alloc, 'input, T, E> = std::result::Result<T, Error<'alloc, 'input, E>>;

    impl<I> From<lexer_ext::error::Error<I>> for Error<'_, '_, I> {
        fn from(err: lexer_ext::error::Error<I>) -> Self {
            Self::Lex(err)
        }
    }

    // FIXME: If we start storing Error types, it may be best to box `MissingArg`
    // to reduce the size of `Error`
    #[allow(clippy::large_enum_variant)]
    #[derive(Debug, PartialEq)]
    pub enum Error<'alloc, 'input, I> {
        EmptyInput,
        NoExpression,
        StartOfGroupNotFound,
        EndOfGroupNotFound,
        EndOfBlockNotFound,
        ExpectedComma,
        MissingArg {
            left: crate::ast::Ast<'alloc, 'input>,
            op: Token<'input>,
            err: lexer_ext::error::Error<I>,
        },
        Token(Token<'input>),
        ExpectedSymbol(&'static [&'static str]),
        Lex(lexer_ext::error::Error<I>),
    }
}
