use lexer_ext::{
    token::{self, Lexer, Token},
    error::TokenRes
};
use lln_peek::LLNPeek;

use parser_ext::{
    ast::{item::*, Ast},
    error::{self, Error, Result},
};

use arena::Arena;

macro_rules! ws {
    ($lexer:expr, $id:ident) => {
        let output = $lexer.parse_white_space();
        let $id = output.ok();
    };
}

type Alloc<'alloc, 'input> = &'alloc Arena<Ast<'alloc, 'input>>;
type Iter<'input, L> = LLNPeek<L, TokenRes<'input, L>>;

#[derive(Debug, Clone)]
pub struct ParserImpl<'input, L: Lexer<'input>> {
    lexer: Iter<'input, L>,
}

macro_rules! parse_right_assoc {
    (
        ($self:ident, $alloc:ident, $expr:ident, $token:ident, $next:ident = $next_parse:ident)
        $($pattern:pat => $eval:expr)*
    ) => {
        macro_rules! $next {
            () => { parse_right_assoc!(@next $self $alloc $next_parse) };
        }
        let mut $expr = $next!()?;
        
        loop {
            $self.lexer.reserve_tokens(1);

            let $token = match $self.lexer.parse_token() {
                e@Err(_) => {
                    $self.lexer.push(e);
                    break
                },
                Ok(token) => token
            };

            let mut is_done = true;

            if let token::Type::Symbol = $token.ty {
                is_done = false;

                match $token.lexeme {
                    $($pattern => $eval),*
                    _ => is_done = true,
                }
            }

            if is_done {
                $self.lexer.push(Ok($token));
                break
            }
        }
        
        Ok($expr)
    };
    (@next $self:ident $alloc:ident parse_base) => { $self.parse_base() };
    (@next $self:ident $alloc:ident $next_parse:ident) => { $self.$next_parse($alloc) };
}

impl<'input, L: Lexer<'input>> ParserImpl<'input, L> {
    pub fn new(lexer: L) -> Self {
        Self {
            lexer: LLNPeek::new(lexer, 4),
        }
    }

    #[inline]
    pub fn parse_expr<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        self.parse_bit_or(alloc)
    }

    #[inline]
    fn parse_base<'alloc>(&mut self) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        match self.lexer.parse_token()? {
            ident@Token { ty: token::Type::Ident, .. } => Ok(Ast::Ident(ident)),
            // ident@Token { ty: token::Type::Symbol, lexeme: b"(", .. } => Ok(Ast::Ident(ident)),
            _ => unimplemented!(),
        }
    }

    #[inline]
    fn parse_dot<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, expr, token, parse = parse_base)

            b".*" => {
                expr = Ast::PostOp {
                    expr: alloc.insert(expr),
                    op: token
                };
            }
            
            b"." => {
                expr = Ast::BinOp {
                    right: alloc.insert(parse!()?),
                    left: alloc.insert(expr),
                    op: token
                };
            }
        }
    }

    #[inline]
    fn parse_shift<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, expr, token, parse = parse_dot)

            b">>>" => {
                expr = Ast::BinOp {
                    right: alloc.insert(parse!()?),
                    left: alloc.insert(expr),
                    op: token
                };
            }
            
            b"<<<" => {
                expr = Ast::BinOp {
                    right: alloc.insert(parse!()?),
                    left: alloc.insert(expr),
                    op: token
                };
            }
        }
    }

    #[inline]
    fn parse_bit_and<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, expr, token, parse = parse_shift)

            b"&" => {
                expr = Ast::BinOp {
                    right: alloc.insert(parse!()?),
                    left: alloc.insert(expr),
                    op: token
                };
            }
        }
    }

    #[inline]
    fn parse_bit_or<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, expr, token, parse = parse_bit_and)

            b"|" => {
                expr = Ast::BinOp {
                    right: alloc.insert(parse!()?),
                    left: alloc.insert(expr),
                    op: token
                };
            }
        }
    }
}
