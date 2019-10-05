use lexer_ext::{
    error::TokenRes,
    token::{self, Lexer, Token},
};
use lln_peek::LLNPeek;

use parser_ext::{
    ast::{item::*, Ast},
    error::{self, Error, Result},
};

use arena::Arena;

type Alloc<'alloc, 'input> = &'alloc Arena<Ast<'alloc, 'input>>;
type Iter<'input, L> = LLNPeek<L, TokenRes<'input, L>>;

#[derive(Debug, Clone)]
pub struct ParserImpl<'input, L: Lexer<'input>> {
    lexer: Iter<'input, L>,
}

macro_rules! parse_right_assoc {
    (
        ($self:ident, $alloc:ident, $next_parse:ident)
        $($pattern:pat => $eval_ty:ident)*
    ) => {
        macro_rules! parse {
            () => { $self.$next_parse($alloc) };
        }
        let mut expr = parse!()?;

        loop {
            let token = match $self.lexer.parse_token() {
                e@Err(_) => {
                    $self.lexer.push(e);
                    break
                },
                Ok(token) => token
            };

            let mut is_done = true;

            if let token::Type::Symbol = token.ty {
                is_done = false;

                match token.lexeme {
                    $($pattern => {
                        expr = parse_right_assoc!(@eval $eval_ty $alloc, parse, expr, token)
                    }),*
                    _ => is_done = true,
                }
            }

            if is_done {
                $self.lexer.push(Ok(token));
                break
            }
        }

        Ok(expr)
    };
    (@eval post $alloc:ident, $next:ident, $expr:expr, $op:expr) => {
        Ast::PostOp {
            expr: $alloc.insert($expr),
            op: $op
        }
    };
    (@eval bin $alloc:ident, $next:ident, $expr:expr, $op:expr) => {
        Ast::BinOp {
            right: $alloc.insert($next!()?),
            left: $alloc.insert($expr),
            op: $op
        }
    };
    (@eval $eval_ty:ident $alloc:ident, $next:ident, $expr:expr, $op:expr) => {
        compile_error!(stringify!($eval_ty));
    };
}

macro_rules! parse_left_assoc {
    (
        ($self:ident, $alloc:ident, $next_parse:ident)
        $($pattern:pat => $eval_ty:ident)*
    ) => {
        macro_rules! parse {
            () => {
                $self.$next_parse($alloc)
            };
        }

        let mut expr_val = parse!()?;

        let mut expr = &mut expr_val;

        loop {
            let token = match $self.lexer.parse_token() {
                e @ Err(_) => {
                    $self.lexer.push(e);
                    break;
                }
                Ok(token) => token,
            };

            let mut is_done = true;

            if let token::Type::Symbol = token.ty {
                is_done = false;

                match token.lexeme {
                    $($pattern => {
                        parse_left_assoc! { @eval $eval_ty $alloc, parse, expr, token }
                    }),*
                    _ => is_done = true,
                }
            }

            if is_done {
                $self.lexer.push(Ok(token));
                break;
            }
        }

        Ok(expr_val)
    };
    (@eval bin $alloc:ident, $next:ident, $expr:expr, $op:expr) => {
        let right = $alloc.insert($next!()?);
        let left = std::mem::replace($expr, Ast::Uninit);

        *$expr = Ast::BinOp {
            left: $alloc.insert(left),
            right,
            op: $op,
        };

        $expr = if let Ast::BinOp { right, .. } = $expr {
            right
        } else {
            unreachable!()
        }
    };
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
        self.parse_assign(alloc)
    }

    #[inline]
    fn parse_base<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        let token = self.lexer.parse_token()?;
        match token.ty {
            | token::Type::Ident
            | token::Type::Int(_)
            | token::Type::Float(_)
            | token::Type::Str(_) => Ok(Ast::Value(token)),

            token::Type::BlockStart(token::Block::Paren) => {
                let open = token;
                let inner = self.parse_expr(alloc)?;
                let close = self.lexer.parse_token()?;
                let inner = alloc.insert(inner);

                if let Token { ty: token::Type::BlockEnd(token::Block::Paren), .. } = close {
                    Ok(Ast::Block { open, inner, close })
                } else {
                    Err(Error::EndOfBlockNotFound)
                }
            }

            _ => unimplemented!("TOKEN = {:?}", token),
        }
    }

    #[inline]
    #[allow(unused, clippy::type_complexity)]
    fn parse_right_assoc<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
        mut expr: Ast<'alloc, 'input>,
        ops: &mut [(
            &[u8],
            &mut dyn FnMut(
                &mut Self,
                Ast<'alloc, 'input>,
                Token<'input>,
            ) -> Result<'input, Ast<'alloc, 'input>, L::Input>,
        )],
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        loop {
            let token = match self.lexer.parse_token() {
                e @ Err(_) => {
                    self.lexer.push(e);
                    break;
                }
                Ok(token) => token,
            };

            let mut is_done = true;

            if let token::Type::Symbol = token.ty {
                let op = ops.iter_mut().find(|(op, _)| op == &token.lexeme);

                if let Some((_, op)) = op {
                    is_done = false;
                    expr = op(self, expr, token)?;
                }
            }

            if is_done {
                self.lexer.push(Ok(token));
                break;
            }
        }

        Ok(expr)
    }

    #[inline]
    fn parse_dot<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, parse_base)

            b".*" => post
            b"?" => post
            b"." => bin
        }
    }
    
    #[inline]
    fn parse_negation<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        let mut tokens = Vec::new();

        loop {
            let token = match self.lexer.parse_token() {
                Ok(token) => token,
                e@Err(_) => {
                    self.lexer.push(e);
                    break
                },
            };

            let mut is_done = true;

            if let token::Type::Symbol = token.ty {
                is_done = token.lexeme != b"-";
            }

            if is_done {
                self.lexer.push(Ok(token));
                break
            } else {
                tokens.push(token);
            }
        }

        let mut expr = self.parse_dot(alloc)?;

        for op in tokens {
            expr = Ast::PreOp { op, expr: alloc.insert(expr) };
        }

        Ok(expr)
    }

    #[inline]
    fn parse_shift<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, parse_negation)

            b">>>" => bin
            b"<<<" => bin
        }
    }

    #[inline]
    fn parse_bit_and<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, parse_shift)

            b"&" => bin
        }
    }

    #[inline]
    fn parse_bit_or<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, parse_bit_and)

            b"|" => bin
        }
    }

    #[inline]
    fn parse_arith_prod<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, parse_bit_or)

            b"*" => bin
            b"/" => bin
        }
    }

    #[inline]
    fn parse_arith_sum<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, parse_arith_prod)

            b"+" => bin
            b"-" => bin
        }
    }

    #[inline]
    fn parse_comparison<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, parse_arith_sum)

            b"==" => bin
            b"!=" => bin
            b">" => bin
            b"<" => bin
            b"<=" => bin
            b">=" => bin
        }
    }

    #[inline]
    fn parse_boolean_and<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, parse_comparison)

            b"&&" => bin
        }
    }

    #[inline]
    fn parse_boolean_or<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, parse_boolean_and)

            b"||" => bin
        }
    }

    #[inline]
    fn parse_function<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_left_assoc! {
            (self, alloc, parse_boolean_or)

            b"->" => bin
        }
    }

    #[inline]
    fn parse_assign<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_left_assoc! {
            (self, alloc, parse_function)

            b"=" => bin
            b":=" => bin
        }
    }
}

pub fn asm<'alloc, 'input>(
    parser: &mut ParserImpl<'input, &mut dyn Lexer<'input, Input = &'input [u8]>>,
    alloc: Alloc<'alloc, 'input>
) -> Result<'input, Ast<'alloc, 'input>, &'input [u8]> {
    parser.parse_expr(alloc)
}