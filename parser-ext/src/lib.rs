pub mod ast {
    use lexer_ext::token::{self, Token as RawToken};
    
    pub type AstPtr<'alloc, 'input> = &'alloc Ast<'alloc, 'input>;

    #[derive(Debug, Clone, Copy)]
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
        // Let(item::Let<'alloc, 'input>),
        Literal(item::Literal<'input>),
        Ident(Token<'input>),
        Block {
            open: Token<'input>,
            inner: AstPtr<'alloc, 'input>,
            close: Token<'input>,
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
    pub type Result<T, E> = std::result::Result<T, Error<E>>;

    #[derive(Debug, PartialEq)]
    pub struct Error<I> {
        pub input: Option<I>,
        pub ty: Type,
    }

    impl<I> From<lexer_ext::error::Error<I>> for Error<I> {
        fn from(err: lexer_ext::error::Error<I>) -> Self {
            Self {
                input: Some(err.input),
                ty: Type::Lex(err.meta, err.ty),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Type {
        EmptyInput,
        ExpectedSymbol(&'static [&'static str]),
        Lex(lexer_ext::error::Meta, lexer_ext::error::Type),
    }
}
