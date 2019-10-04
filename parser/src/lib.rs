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

impl<'input, L: Lexer<'input>> ParserImpl<'input, L> {
    pub fn new(lexer: L) -> Self {
        Self {
            lexer: LLNPeek::new(lexer, 4),
        }
    }

    pub fn parse_expr<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        self.parse_dot(alloc)
    }

    fn parse_base<'alloc>(&mut self) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        match self.lexer.parse_token()? {
            ident@Token { ty: token::Type::Ident, .. } => Ok(Ast::Ident(ident)),
            // ident@Token { ty: token::Type::Symbol, lexeme: b"(", .. } => Ok(Ast::Ident(ident)),
            _ => unimplemented!(),
        }
    }

    fn parse_dot<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        let mut expr = self.parse_base()?;
        
        loop {
            self.lexer.reserve_tokens(1);

            let token = match self.lexer.parse_token() {
                e@Err(_) => {
                    self.lexer.push(e);
                    break
                },
                Ok(token) => token
            };

            let mut is_done = true;

            if let token::Type::Symbol = token.ty {
                is_done = false;

                match token.lexeme {
                    b".*" => {
                        expr = Ast::PostOp {
                            expr: alloc.insert(expr),
                            op: token
                        };
                    },
                    b"." => {
                        expr = Ast::BinOp {
                            right: alloc.insert(self.parse_base()?),
                            left: alloc.insert(expr),
                            op: token
                        };
                    }
                    _ => is_done = true,
                }
            }

            if is_done {
                self.lexer.push(Ok(token));
                break
            }
        }
        
        Ok(expr)
    }

    // fn parse_shift()
}
