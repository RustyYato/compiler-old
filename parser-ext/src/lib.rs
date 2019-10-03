pub mod ast {
    use lexer_ext::token::{self, Token as RawToken};
    
    pub type AstPtr<'alloc, 'input> = &'alloc Ast<'alloc, 'input>;

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct Token<'input> {
        pub whitespace: Option<&'input str>,
        pub value: RawToken<'input>,
        private: ()
    }

    #[derive(Debug, Clone)]
    pub struct Elaborate<I> {
        pub iter: I,
    }

    impl<'input, Iter: Iterator<Item = RawToken<'input>>> Iterator for Elaborate<Iter> {
        type Item = Token<'input>;

        fn next(&mut self) -> Option<Self::Item> {
            let token = self.iter.next()?;

            if let token::Type::WhiteSpace = token.ty {
                if let Some(next) = self.iter.next() {
                    return Some(Token {
                        whitespace: Some(token.lexeme),
                        value: next,
                        private: ()
                    })
                }
            }

            Some(Token {
                whitespace: None,
                value: token,
                private: ()
            })
        }
    }

    #[derive(Debug, Clone)]
    pub enum Ast<'alloc, 'input> {
        // Literal(item::Literal<'input>),
        Ident(Token<'input>),
        Block {
            open: Token<'input>,
            inner: AstPtr<'alloc, 'input>,
            close: Token<'input>,
        }
    }

    use std::fmt;
    impl fmt::Display for Token<'_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.value)
        }
    }

    impl fmt::Display for Ast<'_, '_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::Ident(token) => write!(f, "{}", token),
                Self::Block {
                    open, inner, close
                } => write!(f, "{}{}{}", open, inner, close),
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
    pub type Result<'input, T, E> = std::result::Result<T, Error<'input, E>>;

    impl<I> From<lexer_ext::error::Error<I>> for Error<'_, I> {
        fn from(err: lexer_ext::error::Error<I>) -> Self {
            Self::Lex(err)
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Error<'input, I> {
        EmptyInput,
        EndOfBlockNotFound(lexer_ext::token::Block, crate::ast::Token<'input>),
        ExpectedSymbol(&'static [&'static str]),
        Lex(lexer_ext::error::Error<I>)
    }
}
