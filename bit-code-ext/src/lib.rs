pub mod builder;

pub mod repr {
    use parser_ext::ast::Ast;

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct Instruction<'alloc, 'input> {
        pub ty: Type<'input>,
        pub ast: &'alloc Ast<'alloc, 'input>
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct Register(pub(crate) u64);

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
    pub struct FusedBinOp {
        out: Register,
        left: Register,
        right: Register
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum Type<'input> {
        Add(FusedBinOp),
        Sub(FusedBinOp),
        Mul(FusedBinOp),
        Div(FusedBinOp),
        Assign(),
        AssignLiteral(Literal<'input>),
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum Literal<'input> {
        Str(&'input str),
        Int(i128),
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
        Lex(parser_ext::error::Error<'alloc, 'input, I>),
    }
}
