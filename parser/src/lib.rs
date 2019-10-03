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
            lexer: LLNPeek::new(lexer),
        }
    }

    pub fn parse_expr<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        let mut ast: Option<Ast<'alloc, 'input>> = None;

        macro_rules! return_or {
            ($($or:tt)*) => {
                match ast {
                    Some(ast) => return Ok(ast),
                    None => {
                        $($or)*
                    }
                }
            }
        }

        loop {
            self.lexer.reserve_tokens(1);
            
            let token = match self.lexer.peek() {
                Some(&Ok(token)) => token,
                Some(&Err(_)) => return_or!(self.lexer.parse_token()?),
                None => return_or!(return Err(Error::EmptyInput))
            };
        
            match token.ty {
                token::Type::Ident => {
                    let _ = self.lexer.parse_token();
                    ast = Some(Ast::Ident(token))
                }
                token::Type::BlockStart(token::Block::Paren) => {
                    let _ = self.lexer.parse_token();
                    let open = token;

                    let expr = self.parse_expr(alloc)?;
                    let expr = alloc.insert(expr);

                    let close = self.lexer.parse_token()?;

                    if let token::Type::BlockEnd(token::Block::Paren) = close.ty {
                        ast = Some(Ast::Block {
                            open,
                            close,
                            inner: expr
                        });
                    } else {
                        return Err(Error::EndOfBlockNotFound(token::Block::Paren, close))
                    }
                }
                token::Type::Symbol => {
                    match token.lexeme {
                        ".*" => {
                            ast = Some(Ast::PostOp {
                                expr: alloc.insert(ast.ok_or(Error::NoExpression)?),
                                op: token
                            });
                        },
                        _ => return_or!(unimplemented!("symbol {:?}", token.lexeme))
                    }
                    let _ = self.lexer.parse_token();
                }
                x => return_or!(unimplemented!("unknown {:?}", x))
            }
        }
    }
}
