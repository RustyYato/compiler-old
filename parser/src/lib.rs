use lexer_ext::token::{self, Lexer, Token as RawToken};
use lln_peek::LLNPeek;

use parser_ext::{
    ast::{item::*, Ast, Token, Elaborate},
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
type Iter<'input, L> = LLNPeek<Elaborate<token::Iter<'input, L>>>;

#[derive(Debug, Clone)]
pub struct ParserImpl<'input, L: Lexer<'input>> {
    pub lexer: Iter<'input, L>,
}

impl<'input, L: Lexer<'input>> ParserImpl<'input, L> {
    pub fn new(lexer: L) -> Self {
        Self {
            lexer: LLNPeek::new(Elaborate {
                iter: token::Iter {
                    lexer,
                    mark: std::marker::PhantomData,
                },
            }),
        }
    }

    pub fn parse_expr<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        self.parse_expr_base(alloc)
    }

    fn parse_expr_base<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        let token = self.lexer.next().ok_or(Error::EmptyInput)?;
        
        match token.value.ty {
            token::Type::BlockStart(token::Block::Paren) => {
                let open = token;
                let expr = self.parse_expr(alloc)?;
                let expr = alloc.insert(expr);

                let close = self.lexer.next().unwrap();

                if let token::Type::BlockEnd(token::Block::Paren) = close.value.ty {
                    Ok(Ast::Block {
                        open,
                        close,
                        inner: expr
                    })
                } else {
                    Err(Error::EndOfBlockNotFound(token::Block::Paren, close))
                }
            }
            token::Type::Ident => Ok(Ast::Ident(token)),
            x => unimplemented!("{:?}", x),
        }
    }
}
