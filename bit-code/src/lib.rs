
use arena::Allocator;
use parser_ext::ast::{Ast, Parser, WithAllocator};
use bit_code_ext::{
    builder::Table,
    error::Result
};

pub struct Encoder<'table, 'alloc, 'input, P, A> {
    parser: WithAllocator<'alloc, P, A>,
    table: &'table mut Table<'alloc, 'input>
}

impl<'table, 'alloc, 'input: 'alloc, P, A> Encoder<'table, 'alloc, 'input, P, A>
where
    A: Allocator<Item = Ast<'alloc, 'input>>,
    P: Parser<'alloc, 'input, A>
{
    pub fn new(table: &'table mut Table<'alloc, 'input>, parser: WithAllocator<'alloc, P, A>) -> Self {
        Self { parser, table }
    }

    pub fn encode(mut self) -> Result<'alloc, 'input, (), P::Input> {
        Ok(())
    }
}