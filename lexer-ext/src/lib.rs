pub mod token {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct Token<'input> {
        pub lexeme: &'input str,
        pub ty: Type<'input>,
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum Type<'input> {
        Ident,
        Symbol,
        Keyword,
        WhiteSpace,
        SemiColon,
        Int(u128),
        Float(f64),
        Str(&'input [u8]),
        BlockStart(Block),
        BlockEnd(Block),
    }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Block {
        Paren = 1,
        Square = 2,
        Curly = 3
    }

    impl<'input> Type<'input> {
        pub fn as_num(&self) -> Num {
            match *self {
                Type::Int(x) => Num::Int(x),
                Type::Float(x) => Num::Float(x),
                _ => panic!("called `Type::as_num()` on an `{:?}`", self),
            }
        }

        pub fn as_int(&self) -> u128 {
            if let Type::Int(x) = *self {
                x
            } else {
                panic!("called `Type::as_int()` on an `{:?}`", self)
            }
        }

        pub fn as_float(&self) -> f64 {
            if let Type::Float(x) = *self {
                x
            } else {
                panic!("called `Type::as_float()` on an `{:?}`", self)
            }
        }

        pub fn as_str(&self) -> &'input [u8] {
            if let Type::Str(x) = *self {
                x
            } else {
                panic!("called `Type::as_str()` on an `{:?}`", self)
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum Num {
        Int(u128),
        Float(f64),
    }

    #[derive(Debug, Clone)]
    pub struct Iter<'input, L: ?Sized> {
        pub mark: std::marker::PhantomData<fn() -> Token<'input>>,
        pub lexer: L,
    }

    use crate::error;

    pub type TokenRes<'input, E> = Result<Token<'input>, error::Error<E>>;

    pub trait Lexer<'input> {
        type Input: 'input + std::fmt::Debug;

        fn parse_block(&mut self) -> TokenRes<'input, Self::Input>;

        fn parse_num(&mut self) -> TokenRes<'input, Self::Input>;
        
        fn parse_str(&mut self) -> TokenRes<'input, Self::Input>;
        
        fn parse_ident(&mut self) -> TokenRes<'input, Self::Input>;
        
        fn parse_keyword(&mut self, keyword: &'static str) -> TokenRes<'input, Self::Input>;
        
        fn parse_semicolon(&mut self) -> TokenRes<'input, Self::Input>;
        
        fn parse_symbol(&mut self, symbol: &'static str) -> TokenRes<'input, Self::Input>;

        fn parse_white_space(&mut self) -> TokenRes<'input, Self::Input>;

        fn parse_token(&mut self) -> TokenRes<'input, Self::Input>;
    }

    use std::fmt;

    impl<'input, L: Lexer<'input>> Iterator for Iter<'input, L> {
        type Item = Token<'input>;

        fn next(&mut self) -> Option<Self::Item> {
            self.lexer.parse_token().ok()
        }
    }
}

pub mod error {
    pub type Result<'input, L, T> = std::result::Result<T, Error<<L as crate::token::Lexer<'input>>::Input>>;

    #[derive(Debug, PartialEq)]
    pub struct Error<I> {
        pub input: I,
        pub meta: Meta,
        pub ty: Type,
    }

    #[derive(Debug, PartialEq)]
    pub enum Meta {
        Block,
        Num,
        Str,
        Ident,
        Keyword,
        SemiColon,
        Symbol,
        WhiteSpace,
        Token,
        Other(&'static str),
    }

    #[derive(Debug, PartialEq)]
    pub enum Type {
        EmptyInput,
        InvalidIdentifier,
        ParseInt,
        ParseFloat,
        IntStartError,
        IdentStartError,
        BlockStartError,
        StrStartError,
        StrEndError,
        InvalidToken,

        InvalidChar(char),
        BlockEndError(crate::token::Block),
        InvalidStr(&'static str),
    }
}
