pub mod builder;

pub mod repr {
    use parser_ext::ast::Ast;

    #[derive(PartialEq, Clone, Copy)]
    pub struct Instruction<'alloc, 'input> {
        pub ty: Type<'input>,
        pub ast: &'alloc Ast<'alloc, 'input>
    }

    use std::fmt;
    impl fmt::Debug for Instruction<'_, '_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            self.ty.fmt(f)
            // f.debug_struct("Instruction")
            //     .field("type", &self.ty)
            //     .finish()
        }
    }

    #[derive(PartialEq, Clone, Copy)]
    pub struct Register(pub(crate) u64);

    impl fmt::Debug for Register {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "$")?;
            self.0.fmt(f)
        }
    }


    pub enum Reg {
        Temp(u64),
        Load(u64)
    }

    impl From<Register> for Reg {
        fn from(Register(reg): Register) -> Self {
            let val = reg >> 1;

            if reg & 1 == 0 {
                Reg::Temp(val)
            } else {
                Reg::Load(val)
            }
        }
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct BinOp {
        pub out: Register,
        pub left: Register,
        pub right: Register
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum Type<'input> {
        Add(BinOp),
        Sub(BinOp),
        Mul(BinOp),
        Div(BinOp),
        Assign(Register, Register),
        AssignLiteral(Register, Literal<'input>),
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum Literal<'input> {
        Str(&'input [u8]),
        Int(u128),
        Float(f64),
    }
}

pub mod error {
    pub type Result<'alloc, 'input, T, E> = std::result::Result<T, Error<'alloc, 'input, E>>;

    impl<'alloc, 'input, I> From<parser_ext::error::Error<'alloc, 'input, I>> for Error<'alloc, 'input, I> {
        fn from(err: parser_ext::error::Error<'alloc, 'input, I>) -> Self {
            Self::Lex(err)
        }
    }

    // FIXME: If we start storing Error types, it may be best to box `MissingArg`
    // to reduce the size of `Error`
    #[allow(clippy::large_enum_variant)]
    #[derive(Debug, PartialEq)]
    pub enum Error<'alloc, 'input, I> {
        ExpectedValue,
        ExpectedPattern,
        BindingNotFound(&'input [u8]),
        Lex(parser_ext::error::Error<'alloc, 'input, I>),
    }
}
