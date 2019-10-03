pub mod token {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct Token<'input> {
        pub white_space: Option<&'input str>,
        pub lexeme: &'input str,
        pub ty: Type<'input>,
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum Type<'input> {
        Ident,
        Symbol,
        Keyword,
        SemiColon,
        Int(u128),
        Float(f64),
        Str(&'input [u8]),
        BlockStart(Block),
        BlockEnd(Block),
    }

    impl fmt::Display for Token<'_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self.ty {
                Type::Ident => write!(f, "{}", self.lexeme),
                Type::Symbol => write!(f, "{}", self.lexeme),
                Type::Keyword => write!(f, "{}", self.lexeme),
                Type::SemiColon => write!(f, ";"),
                Type::Int(_) => write!(f, "{}", self.lexeme),
                Type::Float(_) => write!(f, "{}", self.lexeme),
                Type::Str(_) => write!(f, "{:?}", self.lexeme),
                Type::BlockStart(Block::Paren) => write!(f, "("),
                Type::BlockStart(Block::Square) => write!(f, "["),
                Type::BlockStart(Block::Curly) => write!(f, "{{"),
                Type::BlockEnd(Block::Paren) => write!(f, ")"),
                Type::BlockEnd(Block::Square) => write!(f, "]"),
                Type::BlockEnd(Block::Curly) => write!(f, "}}"),
            }
        }
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
        mark: std::marker::PhantomData<fn() -> Token<'input>>,
        lexer: L,
    }

    use crate::error::TokenRes;

    pub trait Lexer<'input> {
        type Input: 'input + std::fmt::Debug + Clone + PartialEq;

        fn parse_token(&mut self) -> TokenRes<'input, Self>;

        fn iter(self) -> Iter<'input, Self> where Self: Sized {
            Iter {
                lexer: self,
                mark: std::marker::PhantomData
            }
        }
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
    pub type TokenRes<'input, L> = Result<'input, L, crate::token::Token<'input>>;

    #[derive(Debug, Clone, PartialEq)]
    pub struct Error<I> {
        pub input: I,
        pub meta: Meta,
        pub ty: Type,
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
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

    #[derive(Debug, Clone, Copy, PartialEq)]
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
        UnkownSymbol,
        InvalidToken,

        InvalidChar(char),
        BlockEndError(crate::token::Block),
        InvalidStr(&'static str),
    }
}
