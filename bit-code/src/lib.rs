
use arena::Allocator;
use lexer_ext::token::Type as TType;
use parser_ext::ast::{Ast, Parser, WithAllocator};
use bit_code_ext::{
    builder::{Table, ContextBuilder},
    repr::{Instruction, Type, FusedBinOp, Literal, Register},
    error::{Result, Error}
};

pub struct Encoder<'table, 'alloc, 'input, P, A> {
    parser: WithAllocator<'alloc, P, A>,
    table: &'table mut Table<'alloc, 'input>
}

enum Partial<'input> {
    Literal(Literal<'input>),
    Binding(&'input [u8]),
    Complete
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
        let mut context = self.table.new_context();

        loop {
            let ast = match self.parser.parse() {
                Ok(Ast::SemiColon(_)) => continue,
                Ok(ast) => ast,
                e@Err(_) => break
            };

            encode_ast(ast, &mut context)?;
        }

        Ok(())
    }
}

fn encode_ast<'alloc, 'input, I>(
    ast: &'alloc Ast<'alloc, 'input>,
    context: &mut ContextBuilder<'_, 'alloc, 'input>,
) -> Result<'alloc, 'input, Partial<'input>, I> {
    match ast {
        Ast::Value(value) => {
            let lit = match value.ty {
                TType::Int(val) => Literal::Int(val),
                TType::Float(val) => Literal::Float(val),
                TType::Str(val) => Literal::Str(val),
                TType::Ident => return Ok(Partial::Binding(value.lexeme)),
                _ => unreachable!("Invalid Value Token: {:?}", value),
            };

            Ok(Partial::Literal(lit))
        }
        Ast::BinOp { op, left, right } => {
            match op.lexeme {
                b":=" | b"=" => {
                    let right = encode_ast(right, context)?;
                    
                    match right {
                        | Partial::Literal(_)
                        | Partial::Binding(_) => (),
                        _ => return Err(Error::ExpectedValue)
                    }

                    let left = encode_ast(left, context)?;

                    let left = if let Partial::Binding(reg) = left {
                        if op.lexeme == b"=" {
                            context.get(reg)?
                        } else {
                            context.bind(reg)
                        }
                    } else {
                        return Err(Error::ExpectedPattern)
                    };

                    let instr = match right {
                        Partial::Literal(lit) => Instruction { ast, ty: Type::AssignLiteral(left, lit) },
                        Partial::Binding(binding) => {
                            let right = context.get(binding)?;
                            Instruction { ast, ty: Type::Assign(left, right) }
                        },
                        _ => unreachable!()
                    };

                    context.insert(instr);

                    Ok(Partial::Complete)
                },
                _ => unimplemented!("{:?}", ast),
            }
        },
        _ => unimplemented!("{:?}", ast),
    }
}