pub mod ast {
    use lexer_ext::token::{self, Token};

    pub type AstPtr<'alloc, 'input> = &'alloc mut Ast<'alloc, 'input>;

    #[derive(Debug, PartialEq)]
    pub enum Ast<'alloc, 'input> {
        Uninit,
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
    }

    use std::fmt;
    impl fmt::Display for Ast<'_, '_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::Value(token) => write!(f, "{}", token),
                Self::Block { open, inner, close } => write!(f, "{}{}{}", open, inner, close),
                Self::PostOp { expr, op } => write!(f, "({}{})", expr, op),
                Self::PreOp { expr, op } => write!(f, "({}{})", op, expr),
                Self::BinOp { left, op, right } => write!(f, "({}{}{})", left, op, right),
                Self::Uninit => unreachable!(),
            }
        }
    }

    pub mod item {
        use super::*;

        #[derive(Debug, Clone)]
        pub enum Literal<'input> {
            Int(u128),
            Float(f64),
            Str(&'input [u8]),
        }

        #[derive(Debug)]
        pub struct Let<'alloc, 'input> {
            pub kw_let: Token<'input>,
            pub ws_1: Option<Token<'input>>,
            pub ident: Token<'input>,
            pub ws_2: Option<Token<'input>>,
            pub sym_eq: Token<'input>,
            pub ws_3: Option<Token<'input>>,
            pub expr: AstPtr<'alloc, 'input>,
            pub ws_4: Option<Token<'input>>,
            pub semi: Token<'input>,
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
        EndOfBlockNotFound,
        Token(Token<'input>),
        ExpectedSymbol(&'static [&'static str]),
        Lex(lexer_ext::error::Error<I>),
    }
}
