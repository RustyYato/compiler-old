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
        ($self:ident, $alloc:ident, $expr:ident, $token:ident, $next:ident = $next_parse:ident)
        $($pattern:pat => $eval:expr)*
    ) => {
        macro_rules! $next {
            () => { parse_right_assoc!(@next $self $alloc $next_parse) };
        }
        let mut $expr = $next!()?;

        loop {
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
                    $($pattern => {
                        $expr = $eval
                    }),*
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
        self.parse_function(alloc)
    }

    #[inline]
    fn parse_base<'alloc>(&mut self) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        let token = self.lexer.parse_token()?;
        match token.ty {
            | token::Type::Ident
            | token::Type::Int(_)
            | token::Type::Float(_)
            | token::Type::Str(_) => Ok(Ast::Value(token)),
            _ => unimplemented!(),
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
            (self, alloc, expr, token, parse = parse_base)

            b".*" => Ast::PostOp {
                expr: alloc.insert(expr),
                op: token
            }

            b"." => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
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

            b">>>" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }

            b"<<<" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
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

            b"&" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
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

            b"|" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }
        }
    }

    #[inline]
    fn parse_arith_prod<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, expr, token, parse = parse_bit_or)

            b"*" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }

            b"/" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }
        }
    }

    #[inline]
    fn parse_arith_sum<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, expr, token, parse = parse_arith_prod)

            b"+" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }

            b"|" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }
        }
    }

    #[inline]
    fn parse_comparison<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, expr, token, parse = parse_arith_sum)

            b"==" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }

            b"!=" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }

            b">" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }

            b"<" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }

            b"<=" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }

            b">=" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }
        }
    }

    #[inline]
    fn parse_boolean_and<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, expr, token, parse = parse_comparison)

            b"&&" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }
        }
    }

    #[inline]
    fn parse_boolean_or<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        parse_right_assoc! {
            (self, alloc, expr, token, parse = parse_boolean_and)

            b"||" => Ast::BinOp {
                right: alloc.insert(parse!()?),
                left: alloc.insert(expr),
                op: token
            }
        }
    }

    #[inline]
    fn parse_function<'alloc>(
        &mut self,
        alloc: Alloc<'alloc, 'input>,
    ) -> Result<'input, Ast<'alloc, 'input>, L::Input> {
        macro_rules! parse {
            () => {
                self.parse_boolean_or(alloc)
            };
        }

        let mut expr_val = parse!()?;

        let mut expr = &mut expr_val;

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
                is_done = false;

                match token.lexeme {
                    b"->" => {
                        let right = alloc.insert(parse!()?);
                        let left = std::mem::replace(expr, Ast::Uninit);

                        *expr = Ast::BinOp {
                            left: alloc.insert(left),
                            right,
                            op: token,
                        };

                        expr = if let Ast::BinOp { right, .. } = expr {
                            right
                        } else {
                            unreachable!()
                        }
                    }
                    _ => is_done = true,
                }
            }

            if is_done {
                self.lexer.push(Ok(token));
                break;
            }
        }

        Ok(expr_val)
    }
}
