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
    ) -> Result<Ast<'alloc, 'input>, L::Input> {
        self.parse_expr_base(alloc)
    }

    fn parse_expr_base<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<Ast<'alloc, 'input>, L::Input> {
        let token = self.lexer.peek_iter(1).next().copied().ok_or(Error {
            input: None,
            ty: error::Type::EmptyInput,
        })?;

        match token.value.ty {
            token::Type::BlockStart(block) => {
                self.lexer.next();

                let open = token;
                let expr = self.parse_expr(alloc)?;
                let expr = alloc.insert(expr);

                let close = self.lexer.next().unwrap();
                assert!(close.value.ty == token::Type::BlockEnd(block));

                Ok(Ast::Block {
                    open,
                    close,
                    inner: expr
                })
            }
            token::Type::Ident => {
                Ok(Ast::Ident(token))
            }
            _ => unimplemented!(),
        }
    }
}
