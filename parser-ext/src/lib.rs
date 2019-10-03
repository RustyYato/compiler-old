pub mod ast {
    use lexer_ext::token::{self, Token};
    
    pub type AstPtr<'alloc, 'input> = &'alloc Ast<'alloc, 'input>;

    #[derive(Debug, Clone, Copy)]
    pub enum Ast<'alloc, 'input> {
        // Literal(item::Literal<'input>),
        Ident(Token<'input>),
        Block {
            open: Token<'input>,
            inner: AstPtr<'alloc, 'input>,
            close: Token<'input>,
        },
        PostOp {
            expr: AstPtr<'alloc, 'input>,
            op: Token<'input>,
        }
    }

    use std::fmt;
    impl fmt::Display for Ast<'_, '_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::Ident(token) => write!(f, "{}", token),
                Self::Block {
                    open, inner, close
                } => write!(f, "{}{}{}", open, inner, close),
                Self::PostOp {
                    expr, op
                } => write!(f, "{}{}", expr, op),
            }
        }
    }

    pub mod item {
        use super::*;

        #[derive(Debug, Clone)]
        pub enum Literal<'input> {
            Int(u128),
            Float(f64),
            Str(&'input [u8])
        }

        #[derive(Debug, Clone)]
        pub struct Let<'alloc, 'input> {
            pub kw_let: Token<'input>,
            pub ws_1: Option<Token<'input>>,
            pub ident: Token<'input>,
            pub ws_2: Option<Token<'input>>,
            pub sym_eq: Token<'input>,
            pub ws_3: Option<Token<'input>>,
            pub expr: AstPtr<'alloc, 'input>,
            pub ws_4: Option<Token<'input>>,
            pub semi: Token<'input>
        }
    }
}

pub mod error {
    use lexer_ext::token::{Token, Block};

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
        EndOfBlockNotFound(Block, Token<'input>),
        ExpectedSymbol(&'static [&'static str]),
        Lex(lexer_ext::error::Error<I>)
    }
}
