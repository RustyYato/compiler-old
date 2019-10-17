#![deny(unsafe_code)]

use lexer_ext::{
    error::{self, Error, Meta, TokenRes},
    token::{Block, Token, Type},
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
pub fn parse_num<'input>(
    input: &'input str,
    white_space: Option<&'input str>,
) -> Result<&'input str, Token<'input>> {
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
                white_space,
                ty: Type::Float(num),
                lexeme: lexeme.as_bytes(),
            },
        ))
    } else {
        let (next_input, (lexeme, num)) = parse_integer(input, first, next_input)?;

        Ok((
            next_input,
            Token {
                white_space,
                ty: Type::Int(num),
                lexeme: lexeme.as_bytes(),
            },
        ))
    }
}

#[inline]
pub fn parse_white_space(input: &str) -> Result<&str, &str> {
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
        let (white_space, input) = input.split_at(len);

        Ok((input, white_space))
    }
}

#[inline]
pub fn parse_ident<'input>(
    input: &'input str,
    white_space: Option<&'input str>,
) -> Result<&'input str, Token<'input>> {
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

    Ok((
        input,
        Token {
            white_space,
            ty,
            lexeme: lexeme.as_bytes(),
        },
    ))
}

#[inline]
pub fn is_keyword(lexeme: &str) -> bool {
    match lexeme {
        "match" | "in" | "loop" => true,
        _ => false,
    }
}

#[inline]
pub fn parse_symbol<'input>(
    input: &'input str,
    white_space: Option<&'input str>,
) -> Result<&'input str, Token<'input>> {
    macro_rules! ret_op {
        ($lexeme:expr, $input:expr) => {
            return Ok((
                $input,
                Token {
                    white_space,
                    lexeme: $lexeme,
                    ty: lexer_ext::token::Type::Symbol,
                },
            ));
        };
    }

    if input.get(0..3).is_some() {
        let (lexeme, input) = input.split_at(3);
        let lexeme = lexeme.as_bytes();

        match lexeme {
            b">>>" | b"<<<" => ret_op!(lexeme, input),
            _ => (),
        }
    }

    if input.get(0..2).is_some() {
        let (lexeme, input) = input.split_at(2);
        let lexeme = lexeme.as_bytes();

        match lexeme {
            b".*" | b"::" | b"==" | b"!=" | b"<=" | b">=" | b"&&" | b"||" | b"->" | b"=>"
            | b":=" => ret_op!(lexeme, input),
            _ => (),
        }
    }

    if input.get(0..1).is_some() {
        let (lexeme, input) = input.split_at(1);
        let lexeme = lexeme.as_bytes();

        match lexeme {
            b"+" | b"-" | b"*" | b"/" | b"!" | b"?" | b"." | b"$" | b"&" | b"|" | b"^" | b"~"
            | b">" | b"<" | b"=" | b"," | b":" => ret_op!(lexeme, input),
            _ => (),
        }
    }

    Err(Error {
        input,
        meta: Meta::Symbol,
        ty: error::Type::UnkownSymbol,
    })
}

#[inline]
pub fn parse_str<'input>(
    input: &'input str,
    white_space: Option<&'input str>,
) -> Result<&'input str, Token<'input>> {
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
            white_space,
            lexeme: &input.as_bytes()[..len + 2],
            ty: Type::Str(inner.as_bytes()),
        },
    ))
}

#[inline]
fn parse_semicolon<'input>(
    input: &'input str,
    white_space: Option<&'input str>,
) -> Result<&'input str, Token<'input>> {
    let (input, lexeme) = match_char(';')(input)?;

    Ok((
        input,
        Token {
            white_space,
            ty: Type::SemiColon,
            lexeme: lexeme.as_bytes(),
        },
    ))
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

#[inline]
pub fn parse_block<'input>(
    input: &'input str,
    blocks: &mut block_vec::BlockVec,
    white_space: Option<&'input str>,
) -> Result<&'input str, Token<'input>> {
    if input.is_empty() {
        return Err(Error {
            input,
            meta: Meta::Block,
            ty: error::Type::EmptyInput,
        });
    }

    let first = input.get(0..1);

    match first {
        Some("(") => {
            let (lexeme, input) = input.split_at(1);
            blocks.push(Block::Paren);

            Ok((
                input,
                Token {
                    white_space,
                    lexeme: lexeme.as_bytes(),
                    ty: Type::BlockStart(Block::Paren),
                },
            ))
        }
        Some("[") => {
            let (lexeme, input) = input.split_at(1);
            blocks.push(Block::Square);

            Ok((
                input,
                Token {
                    white_space,
                    lexeme: lexeme.as_bytes(),
                    ty: Type::BlockStart(Block::Square),
                },
            ))
        }
        Some("{") => {
            let (lexeme, input) = input.split_at(1);
            blocks.push(Block::Curly);

            Ok((
                input,
                Token {
                    white_space,
                    lexeme: lexeme.as_bytes(),
                    ty: Type::BlockStart(Block::Curly),
                },
            ))
        }
        Some(")") => {
            let (lexeme, input) = input.split_at(1);

            if let Some(Block::Paren) = blocks.pop() {
                Ok((
                    input,
                    Token {
                        white_space,
                        lexeme: lexeme.as_bytes(),
                        ty: Type::BlockEnd(Block::Paren),
                    },
                ))
            } else {
                Err(Error {
                    input,
                    meta: Meta::Block,
                    ty: error::Type::BlockEndError(Block::Paren),
                })
            }
        }
        Some("]") => {
            let (lexeme, input) = input.split_at(1);

            if let Some(Block::Square) = blocks.pop() {
                Ok((
                    input,
                    Token {
                        white_space,
                        lexeme: lexeme.as_bytes(),
                        ty: Type::BlockEnd(Block::Square),
                    },
                ))
            } else {
                Err(Error {
                    input,
                    meta: Meta::Block,
                    ty: error::Type::BlockEndError(Block::Square),
                })
            }
        }
        Some("}") => {
            let (lexeme, input) = input.split_at(1);

            if let Some(Block::Curly) = blocks.pop() {
                Ok((
                    input,
                    Token {
                        white_space,
                        lexeme: lexeme.as_bytes(),
                        ty: Type::BlockEnd(Block::Curly),
                    },
                ))
            } else {
                Err(Error {
                    input,
                    meta: Meta::Block,
                    ty: error::Type::BlockEndError(Block::Curly),
                })
            }
        }
        _ => Err(Error {
            input,
            meta: Meta::Block,
            ty: error::Type::BlockStartError,
        }),
    }
}

impl<'input> lexer_ext::token::Lexer<'input> for LexerImpl<'input> {
    type Input = &'input str;

    #[inline]
    fn parse_token(&mut self) -> TokenRes<'input, Self> {
        let white_space = match parse_white_space(self.input) {
            Ok((input, white_space)) => {
                self.input = input;
                Some(white_space)
            }
            Err(_) => None,
        };

        if self.input.is_empty() {
            return Err(Error {
                input: self.input,
                meta: Meta::Token,
                ty: error::Type::EmptyInput,
            });
        }

        let input = self.input;
        let blocks = &mut self.blocks;

        let (input, token) = parse_semicolon(input, white_space)
            .or_else(move |_| parse_ident(input, white_space))
            .or_else(move |_| parse_symbol(input, white_space))
            .or_else(move |_| parse_num(input, white_space))
            .or_else(move |_| parse_str(input, white_space))
            .or_else(move |_| parse_block(input, blocks, white_space))
            .map_err(move |_| Error {
                input,
                meta: Meta::Token,
                ty: error::Type::UnkownCharacter,
            })?;

        self.input = input;

        Ok(token)
    }
}
