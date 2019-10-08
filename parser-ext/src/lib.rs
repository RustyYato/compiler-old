pub mod ast {
    use lexer_ext::token::{self, Token};

    pub type AstPtr<'alloc, 'input> = &'alloc mut Ast<'alloc, 'input>;

    #[derive(Debug, PartialEq)]
    pub enum Ast<'alloc, 'input> {
        Uninit,
        SemiColon(Token<'input>),
        Value(Token<'input>),
        Block {
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
            patterns: AstPtr<'alloc, 'input>,
            close: Token<'input>,
        },
        Items {
            values: Vec<Ast<'alloc, 'input>>,
            commas: Vec<Token<'input>>
        }
    }

    use std::fmt;
    impl fmt::Display for Ast<'_, '_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::SemiColon(token) => write!(f, "{}", token),
                Self::Value(token) => write!(f, "{}", token),
                Self::Block { open, inner, close } => write!(f, "{}{}{}", open, inner, close),
                Self::PostOp { expr, op } => write!(f, "({}{})", expr, op),
                Self::PreOp { expr, op } => write!(f, "({}{})", op, expr),
                Self::BinOp { left, op, right } => write!(f, "({}{}{})", left, op, right),
                Self::Match { match_kw, cond, open, patterns, close }
                    => write!(f, "({} {}{}{}{})", match_kw, cond, open, patterns, close),
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
                },
                Self::Uninit => unreachable!(),
            }
        }
    }
}

pub mod error {
    use lexer_ext::token::{Block, Token};

    pub type Result<'input, T, E> = std::result::Result<T, Error<'input, E>>;

    impl<I> From<lexer_ext::error::Error<I>> for Error<'_, I> {
        fn from(err: lexer_ext::error::Error<I>) -> Self {
            Self::Lex(err)
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Error<'input, I> {
        EmptyInput,
        NoExpression,
        StartOfGroupNotFound,
        EndOfGroupNotFound,
        EndOfBlockNotFound,
        ExpectedComma,
        Token(Token<'input>),
        ExpectedSymbol(&'static [&'static str]),
        Lex(lexer_ext::error::Error<I>),
    }
}
