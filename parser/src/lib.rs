use lexer_ext::{
    error::TokenRes,
    token::{self, Lexer, Token},
};
use lln_peek::LLNPeek;

use parser_ext::{
    ast::Ast,
    error::{AstResult, Error},
};

use arena::Allocator;

type Iter<'input, L> = LLNPeek<L, TokenRes<'input, L>>;

#[derive(Debug, Clone)]
pub struct ParserImpl<'input, L: Lexer<'input>> {
    lexer: Iter<'input, L>,
}

macro_rules! parse_right_assoc {
    (
        $(
            $curr_parse:ident -> $($next_parse:ident)|+ {
                $($pattern:pat => $eval_ty:ident)*
            }
        )*
    ) => {$(
        #[inline]
        fn $curr_parse<'alloc, A: ?Sized + Allocator<Item = Ast<'alloc, 'input>>>(
            &mut self,
            alloc: &'alloc A
        ) -> AstResult<'alloc, 'input, L::Input> {
            #![allow(clippy::let_and_return)]

            parse_right_assoc! {
                (self, alloc, $($next_parse)|+)

                $($pattern => $eval_ty)*
            }
        }
    )*};
    (
        ($self:ident, $alloc:ident, $($next_parse:ident)+)
        $($pattern:pat => $eval_ty:ident)*
    ) => {
        macro_rules! parse_impl {
            () => {{
                let out = Err(());
                $(let out = match out {
                    Ok(out) => Ok(out),
                    Err(_) => $self.$next_parse($alloc)
                };)+

                out
            }};
        }

        let mut expr = parse_impl!()?;

        loop {
            let token = match $self.lexer.parse_token() {
                e@Err(_) => {
                    $self.lexer.push(e);
                    break
                },
                Ok(token) => token
            };

            macro_rules! parse {
                () => {{
                    match parse_impl!() {
                        Ok(ast) => ast,
                        Err(Error::Lex(err)) => return Err(Error::MissingArg {
                            left: expr,
                            op: token,
                            err
                        }),
                        Err(e) => return Err(e)
                    }
                }};
            }

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
            expr: $alloc.alloc($expr),
            op: $op
        }
    };
    (@eval bin $alloc:ident, $next:ident, $expr:expr, $op:expr) => {
        Ast::BinOp {
            right: $alloc.alloc($next!()),
            left: $alloc.alloc($expr),
            op: $op
        }
    };
    (@eval $eval_ty:ident $alloc:ident, $next:ident, $expr:expr, $op:expr) => {
        compile_error!(stringify!($eval_ty));
    };
}

macro_rules! parse_left_assoc {
    (
        $(
            $curr_parse:ident -> $($next_parse:ident)|+ {
                $($pattern:pat => $eval_ty:ident $(($build:expr))?),* $(,)?
            }
        )*
    ) => {$(
        #[inline]
        fn $curr_parse<'alloc, A: ?Sized + Allocator<Item = Ast<'alloc, 'input>>>(
            &mut self,
            alloc: &'alloc A
        ) -> AstResult<'alloc, 'input, L::Input> {
            #![allow(clippy::let_and_return)]

            parse_left_assoc! {
                (self, alloc, $($next_parse)|+)

                $($pattern => $eval_ty $(($build))?),*
            }
        }
    )*};
    (
        ($self:ident, $alloc:ident, $($next_parse:ident)|+)
        $($pattern:pat => $eval_ty:ident $(($build:expr))?),*
    ) => {
        macro_rules! parse_impl {
            () => {{
                let out = Err(());
                $(let out = match out {
                    Ok(out) => Ok(out),
                    Err(_) => $self.$next_parse($alloc)
                };)+

                out
            }};
        }

        let mut expr_val = parse_impl!()?;

        let mut expr = &mut expr_val;

        loop {
            let token = match $self.lexer.parse_token() {
                e @ Err(_) => {
                    $self.lexer.push(e);
                    break;
                }
                Ok(token) => token,
            };

            macro_rules! parse {
                () => {{
                    match parse_impl!() {
                        Ok(ast) => ast,
                        Err(Error::Lex(err)) => return Err(Error::MissingArg {
                            left: expr_val,
                            op: token,
                            err
                        }),
                        Err(e) => return Err(e)
                    }
                }};
            }

            let mut is_done = true;

            if let token::Type::Symbol = token.ty {
                is_done = false;

                match token.lexeme {
                    $($pattern => {
                        parse_left_assoc! { @eval $eval_ty $alloc, parse, expr, token $(, $build)? }
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
        let right = $alloc.alloc($next!());
        let left = std::mem::replace($expr, Ast::Uninit);

        *$expr = Ast::BinOp {
            left: $alloc.alloc(left),
            right,
            op: $op,
        };

        $expr = if let Ast::BinOp { right, .. } = $expr {
            right
        } else {
            debug_assert!(false, "unreachable: UNSOUND this will be unchecked in release mode!");
            unsafe { std::hint::unreachable_unchecked() }
        }
    };
    (@eval custom $alloc:ident, $next:ident, $expr:expr, $op:expr, $build:expr) => {
        let right = $alloc.alloc($next!());
        let left = std::mem::replace($expr, Ast::Uninit);
        let left = $alloc.alloc(left);

        *$expr = $build($op, left, right);

        $expr = if let Ast::BinOp { right, .. } = $expr {
            right
        } else {
            debug_assert!(false, "unreachable: UNSOUND this will be unchecked in release mode!");
            unsafe { std::hint::unreachable_unchecked() }
        }
    };
    (@eval $ty:ident $($rest:tt)*) => {
        compile_error!(stringify!($ty))
    };
}

pub struct ParseIterator<'parser, 'alloc, 'input, L: Lexer<'input>, A: ?Sized> {
    parser: &'parser mut ParserImpl<'input, L>,
    alloc: &'alloc A,
    err: Option<Error<'alloc, 'input, L::Input>>,
}

impl<'alloc, 'input, L: Lexer<'input>, A: ?Sized + Allocator<Item = Ast<'alloc, 'input>>>
    ParseIterator<'_, 'alloc, 'input, L, A>
{
    pub fn iter(&mut self) -> &mut Self {
        self
    }

    pub fn has_err(&self) -> bool {
        self.err.is_some()
    }

    pub fn err(self) -> Option<Error<'alloc, 'input, L::Input>> {
        self.err
    }
}

impl<'alloc, 'input, L: Lexer<'input>, A: ?Sized + Allocator<Item = Ast<'alloc, 'input>>> Iterator
    for &mut ParseIterator<'_, 'alloc, 'input, L, A>
{
    type Item = Ast<'alloc, 'input>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.err.is_some() {
            return None;
        }

        match self.parser.lexer.parse_token() {
            Ok(
                token @ Token {
                    ty: token::Type::SemiColon,
                    ..
                },
            ) => Some(Ast::SemiColon(token)),
            Err(e) => {
                self.err = Some(e.into());
                None
            }
            Ok(token) => {
                self.parser.lexer.push(Ok(token));
                match self.parser.parse_expr(self.alloc) {
                    Ok(ast) => Some(ast),
                    Err(err) => {
                        self.err = Some(err);
                        None
                    }
                }
            }
        }
    }
}

struct BufOne<T> {
    one: Option<T>,
    value: Vec<T>,
}

impl<T> BufOne<T> {
    fn new() -> Self {
        Self {
            one: None,
            value: Vec::new(),
        }
    }

    fn push(&mut self, value: T) {
        if self.one.is_none() && self.value.is_empty() {
            self.one = Some(value);
        } else {
            if let Some(one) = self.one.take() {
                self.value.push(one)
            }

            self.value.push(value)
        }
    }

    fn is_empty(&self) -> bool {
        self.one.is_none() && self.value.is_empty()
    }

    pub fn make<F: FnOnce(Vec<T>) -> T>(self, f: F) -> T {
        match self.one {
            Some(one) => one,
            None => f(self.value),
        }
    }
}

impl<'input, L: Lexer<'input>> ParserImpl<'input, L> {
    pub fn new(lexer: L) -> Self {
        Self {
            lexer: LLNPeek::new(lexer, 4),
        }
    }

    pub fn parse<'parser, 'alloc, A: ?Sized + Allocator<Item = Ast<'alloc, 'input>>>(
        &'parser mut self,
        alloc: &'alloc A,
    ) -> ParseIterator<'parser, 'alloc, 'input, L, A> {
        ParseIterator {
            parser: self,
            alloc,
            err: None,
        }
    }

    #[inline]
    pub fn parse_expr<'alloc, A: ?Sized + Allocator<Item = Ast<'alloc, 'input>>>(
        &mut self,
        alloc: &'alloc A,
    ) -> AstResult<'alloc, 'input, L::Input> {
        self.parse_comma(alloc)
    }

    #[inline]
    fn parse_base<'alloc, A: ?Sized + Allocator<Item = Ast<'alloc, 'input>>>(
        &mut self,
        alloc: &'alloc A,
    ) -> AstResult<'alloc, 'input, L::Input> {
        let token = self.lexer.parse_token()?;

        match token.ty {
            token::Type::Ident
            | token::Type::Int(_)
            | token::Type::Float(_)
            | token::Type::Str(_) => Ok(Ast::Value(token)),

            token::Type::BlockStart(token::Block::Paren) => {
                let open = token;

                match self.lexer.parse_token() {
                    Ok(
                        close @ Token {
                            ty: token::Type::BlockEnd(token::Block::Paren),
                            ..
                        },
                    ) => return Ok(Ast::Unit { open, close }),
                    res => self.lexer.push(res),
                }

                let inner = self.parse_expr(alloc)?;
                let close = self.lexer.parse_token()?;
                let inner = alloc.alloc(inner);

                if let Token {
                    ty: token::Type::BlockEnd(token::Block::Paren),
                    ..
                } = close
                {
                    Ok(Ast::Group { open, inner, close })
                } else {
                    Err(Error::EndOfBlockNotFound)
                }
            }

            token::Type::Keyword if token.lexeme == b"match" => {
                let match_kw = token;

                let cond = self.parse_expr(alloc)?;

                let open = self.lexer.parse_token()?;

                match open {
                    Token {
                        ty: token::Type::BlockStart(token::Block::Curly),
                        ..
                    } => (),
                    _ => return Err(Error::StartOfGroupNotFound),
                }

                let patterns = self.parse_expr(alloc).ok();

                let close = self.lexer.parse_token()?;

                match close {
                    Token {
                        ty: token::Type::BlockEnd(token::Block::Curly),
                        ..
                    } => (),
                    _ => return Err(Error::StartOfGroupNotFound),
                }

                let cond = alloc.alloc(cond);
                let patterns = patterns.map(|patterns| alloc.alloc(patterns));

                Ok(Ast::Match {
                    match_kw,
                    cond,
                    open,
                    patterns,
                    close,
                })
            }

            token::Type::Keyword if token.lexeme == b"loop" => {
                let block = self.parse_block(alloc)?;

                if let Ast::Block { open, inner, close } = block {
                    Ok(Ast::Loop {
                        loop_kw: token,
                        open,
                        inner,
                        close,
                    })
                } else {
                    unreachable!()
                }
            }

            _ => {
                self.lexer.push(Ok(token));
                Err(Error::Token(token))
            }
        }
    }

    #[inline]
    fn parse_block<'alloc, A: ?Sized + Allocator<Item = Ast<'alloc, 'input>>>(
        &mut self,
        alloc: &'alloc A,
    ) -> AstResult<'alloc, 'input, L::Input> {
        self.lexer.reserve_tokens(1);

        let open = self.lexer.parse_token()?;

        let open = match open {
            open @ Token {
                ty: token::Type::BlockStart(token::Block::Curly),
                ..
            } => open,
            _ => {
                self.lexer.push(Ok(open));
                return Err(Error::Token(open));
            }
        };

        let inner = self.parse(alloc).collect::<Vec<_>>();

        let close = self.lexer.parse_token()?;

        Ok(Ast::Block { open, close, inner })
    }

    #[inline]
    fn parse_negation<'alloc, A: ?Sized + Allocator<Item = Ast<'alloc, 'input>>>(
        &mut self,
        alloc: &'alloc A,
    ) -> AstResult<'alloc, 'input, L::Input> {
        let mut tokens = Vec::new();

        loop {
            let token = match self.lexer.parse_token() {
                Ok(token) => token,
                e @ Err(_) => {
                    self.lexer.push(e);
                    break;
                }
            };

            let mut is_done = true;

            if let token::Type::Symbol = token.ty {
                is_done = token.lexeme != b"-";
            }

            if is_done {
                self.lexer.push(Ok(token));
                break;
            } else {
                tokens.push(token);
            }
        }

        let mut expr = self.parse_dot(alloc)?;

        for op in tokens {
            expr = Ast::PreOp {
                op,
                expr: alloc.alloc(expr),
            };
        }

        Ok(expr)
    }

    pub fn parse_comma<'alloc, A: ?Sized + Allocator<Item = Ast<'alloc, 'input>>>(
        &mut self,
        alloc: &'alloc A,
    ) -> AstResult<'alloc, 'input, L::Input> {
        let mut commas = Vec::new();

        let mut values = BufOne::new();

        let mut state = true;

        loop {
            self.lexer.reserve_tokens(1);

            if state {
                let ast = self.parse_assign(alloc);

                match ast {
                    Ok(ast) => values.push(ast),
                    Err(e) => {
                        if values.is_empty() {
                            return Err(e);
                        } else {
                            break;
                        }
                    }
                }
            } else if let Some(&Ok(
                token @ Token {
                    ty: token::Type::Symbol,
                    lexeme: b",",
                    ..
                },
            )) = self.lexer.peek()
            {
                let _ = self.lexer.parse_token();
                commas.push(token);
            } else {
                break;
            }

            state ^= true;
        }

        Ok(values.make(|values| Ast::Items { values, commas }))
    }

    pub fn parse_call<'alloc, A: ?Sized + Allocator<Item = Ast<'alloc, 'input>>>(
        &mut self,
        alloc: &'alloc A,
    ) -> AstResult<'alloc, 'input, L::Input> {
        let mut calls = BufOne::new();

        loop {
            match self.parse_base(alloc) {
                Ok(ast) => calls.push(ast),
                Err(err) => {
                    break if calls.is_empty() {
                        Err(err)
                    } else {
                        Ok(calls.make(Ast::Call))
                    }
                }
            }
        }
    }

    parse_right_assoc! {
        parse_dot -> parse_call {
            b".*" => post
            b"?" => post
            b"." => bin
        }

        parse_shift -> parse_negation {
            b">>>" => bin
            b"<<<" => bin
        }

        parse_bit_and -> parse_shift {
            b"&&" => bin
        }

        parse_bit_or -> parse_bit_and {
            b"||" => bin
        }

        parse_arith_prod -> parse_bit_or {
            b"*" => bin
            b"/" => bin
        }

        parse_arith_sum -> parse_arith_prod {
            b"+" => bin
            b"-" => bin
        }

        parse_comparison -> parse_arith_sum {
            b"==" => bin
            b"!=" => bin
            b">" => bin
            b"<" => bin
            b">=" => bin
            b"<=" => bin
        }

        parse_boolean_and -> parse_comparison {
            b"&&" => bin
        }

        parse_boolean_or -> parse_boolean_and {
            b"||" => bin
        }
    }

    parse_left_assoc! {
        parse_colon -> parse_function {
            b":" => bin
        }

        parse_function -> parse_block | parse_boolean_or {
            b"->" => bin
        }

        parse_assign -> parse_block | parse_colon {
            b"=" => bin,
            b":=" => bin,
        }
    }
}

impl<'alloc, 'input: 'alloc, L: Lexer<'input>, A: ?Sized> parser_ext::ast::Parser<'alloc, 'input, A>
    for ParserImpl<'input, L>
where
    A: arena::Allocator<Item = Ast<'alloc, 'input>>,
{
    type Input = L::Input;

    fn parse(&mut self, alloc: &'alloc A) -> AstResult<'alloc, 'input, Self::Input> {
        match self.lexer.parse_token()? {
            token @ Token {
                ty: token::Type::SemiColon,
                ..
            } => Ok(Ast::SemiColon(token)),
            token => {
                self.lexer.push(Ok(token));
                self.parse_expr(alloc)
            }
        }
    }
}
