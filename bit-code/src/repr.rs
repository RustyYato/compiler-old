use parser_ext::ast::Ast;

#[derive(PartialEq, Clone, Copy)]
pub struct Instruction<'alloc, 'input> {
    pub ty: Type<'input>,
    pub ast: &'alloc Ast<'alloc, 'input>,
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
    Load(u64),
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
    pub right: Register,
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
