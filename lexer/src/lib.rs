#![deny(unsafe_code)]

use lexer_ext::{
    error::{self, Meta, Error},
    token::{self, Block, Iter, Type, Token, TokenRes},
};

pub type Result<I, T> = std::result::Result<(I, T), Error<I>>;

mod block_vec;

#[inline]
fn match_char(c: char) -> impl FnMut(&str) -> Result<&str, &str> {
    move |input| match input.chars().next() {
        Some(x) if x == c => {
            let (c, input) = input.split_at(c.len_utf8());
            Ok((input, c))
        }
        _ => Err(Error {
            input,
            meta: Meta::Other("match_char"),
            ty: error::Type::InvalidChar(c),
        }),
    }
}

#[inline]
fn match_str(s: &'static str) -> impl FnMut(&str) -> Result<&str, &str> {
    move |input| {
        if input.starts_with(s) {
            let (s, input) = input.split_at(s.len());
            Ok((input, s))
        } else {
            Err(Error {
                input,
                meta: Meta::Other("match_str"),
                ty: error::Type::InvalidStr(s),
            })
        }
    }
}

#[inline]
pub fn parse_num(input: &str) -> Result<&str, Token<'_>> {
    #[inline]
    pub fn parse_integer_stub(input: &str) -> Result<&str, &str> {
        let mut chars = input.chars();

        if let Some(c) = chars.next() {
            if !c.is_numeric() {
                return Err(Error {
                    input,
                    meta: Meta::Num,
                    ty: error::Type::IntStartError,
                });
            }

            let len: usize = chars
                .take_while(|c| c.is_numeric())
                .map(|c| c.len_utf8())
                .sum();

            let len = c.len_utf8() + len;
            let (num, input) = input.split_at(len);

            Ok((input, num))
        } else {
            Err(Error {
                input,
                meta: Meta::Num,
                ty: error::Type::EmptyInput,
            })
        }
    }

    #[inline]
    pub fn parse_integer<'a>(
        input: &'a str,
        num_str: &'a str,
        next_input: &'a str,
    ) -> Result<&'a str, (&'a str, u128)> {
        let num = match num_str.parse() {
            Ok(num) => num,
            Err(_) => {
                return Err(Error {
                    input,
                    meta: Meta::Num,
                    ty: error::Type::ParseInt,
                })
            }
        };

        Ok((next_input, (num_str, num)))
    }

    #[inline]
    pub fn parse_float<'a>(
        input: &'a str,
        first: &'a str,
        next_input: &'a str,
    ) -> Result<&'a str, (&'a str, f64)> {
        let (next_input, period) = match_char('.')(next_input)?;
        let (next_input, second) = parse_integer_stub(next_input)?;

        let len = first.len() + period.len() + second.len();

        let num_str = &input[..len];

        let num = match num_str.parse() {
            Ok(num) => num,
            Err(_) => {
                return Err(Error {
                    input,
                    meta: Meta::Num,
                    ty: error::Type::ParseInt,
                })
            }
        };

        Ok((next_input, (num_str, num)))
    }

    let (next_input, first) = parse_integer_stub(input)?;

    if let Ok((next_input, (lexeme, num))) = parse_float(input, first, next_input) {
        Ok((
            next_input,
            Token {
                ty: Type::Float(num),
                lexeme,
            },
        ))
    } else {
        let (next_input, (lexeme, num)) = parse_integer(input, first, next_input)?;

        Ok((
            next_input,
            Token {
                ty: Type::Int(num),
                lexeme,
            },
        ))
    }
}

#[inline]
pub fn parse_white_space(input: &str) -> Result<&str, Token<'_>> {
    let len: usize = input
        .chars()
        .take_while(|c| c.is_whitespace())
        .map(|c| c.len_utf8())
        .sum();

    if len == 0 {
        Err(Error {
            input,
            meta: Meta::WhiteSpace,
            ty: error::Type::EmptyInput,
        })
    } else {
        let (lexeme, input) = input.split_at(len);

        Ok((
            input,
            Token {
                ty: Type::WhiteSpace,
                lexeme,
            },
        ))
    }
}

#[inline]
pub fn parse_ident(input: &str) -> Result<&str, Token<'_>> {
    #[inline]
    pub fn parse_ident_str(input: &str) -> Result<&str, &str> {
        let mut chars = input.chars();

        if let Some(c) = chars.next() {
            if c != '_' && !c.is_alphabetic() {
                return Err(Error {
                    input,
                    meta: Meta::Ident,
                    ty: error::Type::IdentStartError,
                });
            }

            let len: usize = chars
                .take_while(|&c| c == '_' || c.is_alphanumeric())
                .map(|c| c.len_utf8())
                .sum();

            let len = c.len_utf8() + len;
            let (ident, input) = input.split_at(len);

            Ok((input, ident))
        } else {
            Err(Error {
                input,
                meta: Meta::Ident,
                ty: error::Type::EmptyInput,
            })
        }
    }

    let (input, lexeme) = parse_ident_str(input)?;

    let ty = if is_keyword(lexeme) {
        Type::Keyword
    } else {
        Type::Ident
    };

    Ok((input, Token { ty, lexeme }))
}

#[inline]
pub fn is_keyword(lexeme: &str) -> bool {
    match lexeme {
        "if" | "else" | "let" => true,
        _ => false,
    }
}

#[inline]
pub fn parse_symbol(input: &str) -> Result<&str, Token<'_>> {
    let len: usize = input
        .chars()
        .take_while(|&c| is_symbol_char(c))
        .map(|c| c.len_utf8())
        .sum();

    if len == 0 {
        Err(Error {
            input,
            meta: Meta::Symbol,
            ty: error::Type::EmptyInput,
        })
    } else {
        let (lexeme, input) = input.split_at(len);

        Ok((
            input,
            Token {
                ty: Type::Symbol,
                lexeme,
            },
        ))
    }
}

pub fn is_symbol_char(c: char) -> bool {
    match c {
        '!' | '@' | '#' | '$' | '%' | '^' | '&' | '*' | '|' | '<' | '>' | '=' | '-' | '+' | '/'
        | '\\' | '~' | '`' | '?' | ',' | '.' | '\'' | ':' => true,
        _ => false,
    }
}

pub fn is_symbol(input: &str) -> bool {
    input.chars().all(is_symbol_char)
}

// #[inline]
// pub fn parse_block(input: &str) -> Result<&str, Token<'_>> {
//     #[inline]
//     pub fn parse_block_inner(input: &str) -> Result<&str, Iter<'_, LexerImpl>> {
//         let iter = iter(input);

//         let input = {
//             let mut iter = iter.clone();
//             iter.for_each(drop);
//             iter.lexer.input
//         };

//         Ok((input, iter))
//     }

//     if input.is_empty() {
//         return Err(Error {
//             input,
//             meta: Meta::Block,
//             ty: error::Type::EmptyInput,
//         });
//     }

//     let first = input.get(0..1);
//     let ty = match first {
//         Some("(") => Block::Paren,
//         Some("[") => Block::Square,
//         Some("{") => Block::Curly,
//         _ => {
//             return Err(Error {
//                 input,
//                 meta: Meta::Block,
//                 ty: error::Type::BlockStartError,
//             })
//         }
//     };

//     let next_input = &input[1..];
//     let (next_input, block) = parse_block_inner(next_input).expect("Infallible");

//     let first = input.get(0..1);

//     match (ty, first) {
//         (Block::Paren, Some("(")) | (Block::Square, Some("[")) | (Block::Curly, Some("{")) => (),
//         _ => {
//             return Err(Error {
//                 input,
//                 meta: Meta::Block,
//                 ty: error::Type::BlockEndError(ty),
//             })
//         }
//     }

//     let next_input = &next_input[1..];

//     let len = input.len() - next_input.len();

//     Ok((
//         next_input,
//         Token {
//             lexeme: &input[..len],
//             ty: Type::Block(ty, lexer_ext::token::BlockInner::Lazy(block)),
//         },
//     ))
// }

#[inline]
pub fn parse_str(input: &str) -> Result<&str, Token<'_>> {
    if input.is_empty() {
        return Err(Error {
            input,
            meta: Meta::Str,
            ty: error::Type::EmptyInput,
        });
    }

    if input.get(0..1) != Some("\"") {
        return Err(Error {
            input,
            meta: Meta::Str,
            ty: error::Type::StrStartError,
        });
    }

    let next_input = &input[1..];
    let len = next_input
        .chars()
        .take_while(|&c| c != '"')
        .map(|c| c.len_utf8())
        .sum();

    let inner = &next_input[..len];
    let next_input = &next_input[len + 1..];

    Ok((
        next_input,
        Token {
            lexeme: &input[..len + 2],
            ty: Type::Str(inner.as_bytes()),
        },
    ))
}

#[inline]
fn parse_semicolon(input: &str) -> Result<&str, Token<'_>> {
    let (input, lexeme) = match_char(';')(input)?;

    Ok((
        input,
        Token {
            ty: Type::SemiColon,
            lexeme,
        },
    ))
}

#[inline]
pub fn iter(input: &str) -> Iter<'_, LexerImpl> {
    Iter {
        lexer: LexerImpl::new(input),
        mark: std::marker::PhantomData
    }
}

#[derive(Debug, Clone)]
pub struct LexerImpl<'input> {
    input: &'input str,
    blocks: block_vec::BlockVec,
}

impl<'input> LexerImpl<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            blocks: block_vec::BlockVec::new(),
        }
    }
}

macro_rules! try_parse {
    ($self:ident, $func:expr) => {{
        let (input, token) = $func($self.input)?;
        $self.input = input;
        Ok(token)
    }}
}

impl<'input> lexer_ext::token::Lexer<'input> for LexerImpl<'input> {
    type Input = &'input str;

    #[inline]
    fn parse_block(&mut self) -> TokenRes<'input, Self::Input> {
        let input = self.input;

        if input.is_empty() {
            return Err(Error {
                input,
                meta: Meta::Block,
                ty: error::Type::EmptyInput,
            });
        }

        let first = input.get(0..1);
        match first {
            Some(lexeme@"(") => {
                self.blocks.push(Block::Paren);

                Token {
                    lexeme,
                    ty: Type::BlockStart(Block::Paren)
                }
            },
            Some(lexeme@"[") => {
                self.blocks.push(Block::Square);

                Token {
                    lexeme,
                    ty: Type::BlockStart(Block::Square)
                }
            },
            Some(lexeme@"{") => {
                self.blocks.push(Block::Curly);

                Token {
                    lexeme,
                    ty: Type::BlockStart(Block::Curly)
                }
            },
            Some(lexeme@")") => {
                if let Some(Block::Paren) = self.blocks.pop() {
                    Token {
                        lexeme,
                        ty: Type::BlockEnd(Block::Paren)
                    }
                } else {
                    return Err(Error {
                        input: self.input,
                        meta: Meta::Block,
                        ty: error::Type::BlockEndError(Block::Paren)
                    })
                }
            },
            Some(lexeme@"]") => {
                if let Some(Block::Square) = self.blocks.pop() {
                    Token {
                        lexeme,
                        ty: Type::BlockEnd(Block::Square)
                    }
                } else {
                    return Err(Error {
                        input: self.input,
                        meta: Meta::Block,
                        ty: error::Type::BlockEndError(Block::Square)
                    })
                }
            },
            Some(lexeme@"}") => {
                if let Some(Block::Curly) = self.blocks.pop() {
                    Token {
                        lexeme,
                        ty: Type::BlockEnd(Block::Curly)
                    }
                } else {
                    return Err(Error {
                        input: self.input,
                        meta: Meta::Block,
                        ty: error::Type::BlockEndError(Block::Curly)
                    })
                }
            },
            _ => {
                return Err(Error {
                    input,
                    meta: Meta::Block,
                    ty: error::Type::BlockStartError,
                })
            }
        };



        // let next_input = &input[1..];
        // let (next_input, block) = parse_block_inner(next_input).expect("Infallible");

        // let first = next_input.get(0..1);

        // match (ty, first) {
        //     (Block::Paren, Some(")")) | (Block::Square, Some("]")) | (Block::Curly, Some("}")) => (),
        //     _ => {
        //         return Err(Error {
        //             input,
        //             meta: Meta::Block,
        //             ty: error::Type::BlockEndError(ty),
        //         })
        //     }
        // }

        // let next_input = &next_input[1..];

        // let len = input.len() - next_input.len();

        // Ok((
        //     next_input,
        //     Token {
        //         lexeme: &input[..len],
        //         ty: Type::Block(ty, lexer_ext::token::BlockInner::Eager(block)),
        //     },
        // ))
        unimplemented!()
    }

    #[inline]
    fn parse_num(&mut self) -> TokenRes<'input, Self::Input> {
        try_parse!(self, parse_num)
    }

    #[inline]
    fn parse_str(&mut self) -> TokenRes<'input, Self::Input> {
        try_parse!(self, parse_str)
    }

    #[inline]
    fn parse_ident(&mut self) -> TokenRes<'input, Self::Input> {
        let (input, token) = parse_ident(self.input)?;
        if let token::Type::Ident = token.ty {
            self.input = input;
            Ok(token)
        } else {
            Err(error::Error {
                input: self.input,
                meta: Meta::Ident,
                ty: error::Type::InvalidIdentifier,
            })
        }
    }

    #[inline]
    fn parse_keyword(&mut self, keyword: &'static str) -> TokenRes<'input, Self::Input> {
        assert!(is_keyword(keyword));
        let (input, lexeme) = match_str(keyword)(self.input)?;
        self.input = input;
        Ok(Token { lexeme, ty: Type::Keyword })
    }

    #[inline]
    fn parse_semicolon(&mut self) -> TokenRes<'input, Self::Input> {
        try_parse!(self, parse_semicolon)
    }

    #[inline]
    fn parse_symbol(&mut self, symbol: &'static str) -> TokenRes<'input, Self::Input> {
        assert!(is_symbol(symbol));
        let (input, lexeme) = match_str(symbol)(self.input)?;
        self.input = input;
        Ok(Token { lexeme, ty: Type::Symbol })
    }

    #[inline]
    fn parse_white_space(&mut self) -> TokenRes<'input, Self::Input> {
        try_parse!(self, parse_white_space)
    }

    #[inline]
    fn parse_token(&mut self) -> TokenRes<'input, Self::Input> {
        macro_rules! forward {
            ($res:expr) => {
                if let Ok((input, token)) = $res {
                    self.input = input;
                    return Ok(token);
                }
            }
        }

        forward!(parse_white_space(self.input));
        forward!(parse_semicolon(self.input));
        forward!(parse_ident(self.input));
        forward!(parse_symbol(self.input));
        forward!(parse_str(self.input));

        self.parse_block().map_err(|_| Error {
            input: self.input,
            meta: Meta::Token,
            ty: error::Type::InvalidToken
        })
    }
}
