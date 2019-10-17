use self::{
    builder::{ContextBuilder, Table},
    error::{Error, Result},
    repr::{BinOp, Instruction, Literal, Register, Type},
};
use arena::Allocator;
use lexer_ext::token::Type as TType;
use parser_ext::ast::{Ast, Parser, WithAllocator};

pub mod builder;
pub mod error;
pub mod repr;

pub struct Encoder<'table, 'alloc, 'input, P, A> {
    parser: WithAllocator<'alloc, P, A>,
    table: &'table mut Table<'alloc, 'input>,
}

enum Partial<'input> {
    Literal(Literal<'input>),
    Binding(&'input [u8]),
    Register(Register),
    Complete,
}

impl<'table, 'alloc, 'input: 'alloc, P, A> Encoder<'table, 'alloc, 'input, P, A>
where
    A: Allocator<Item = Ast<'alloc, 'input>>,
    P: Parser<'alloc, 'input, A>,
{
    pub fn new(
        table: &'table mut Table<'alloc, 'input>,
        parser: WithAllocator<'alloc, P, A>,
    ) -> Self {
        Self { parser, table }
    }

    pub fn encode(mut self) -> Result<'alloc, 'input, (), P::Input> {
        let mut context = self.table.new_context();

        loop {
            let ast = match self.parser.parse() {
                Ok(Ast::SemiColon(_)) => continue,
                Ok(ast) => ast,
                Err(_) => break,
            };

            encode_ast(ast, &mut context)?;
        }

        Ok(())
    }
}

fn encode_ast<'alloc, 'input, I>(
    mut ast: &'alloc Ast<'alloc, 'input>,
    context: &mut ContextBuilder<'_, 'alloc, 'input>,
) -> Result<'alloc, 'input, Partial<'input>, I> {
    loop {
        match ast {
            Ast::Unit { .. } => break Ok(Partial::Literal(Literal::Unit)),
            Ast::Group { inner, .. } => ast = inner,
            Ast::Value(value) => {
                let lit = match value.ty {
                    TType::Int(val) => Literal::Int(val),
                    TType::Float(val) => Literal::Float(val),
                    TType::Str(val) => Literal::Str(val),
                    TType::Ident => return Ok(Partial::Binding(value.lexeme)),
                    _ => unreachable!("Invalid Value Token: {:?}", value),
                };

                break Ok(Partial::Literal(lit));
            }
            Ast::BinOp { op, left, right } => match op.lexeme {
                b":=" | b"=" => {
                    let right = encode_ast(right, context)?;

                    match right {
                        Partial::Literal(_) | Partial::Binding(_) | Partial::Register(_) => (),
                        _ => return Err(Error::ExpectedValue),
                    }

                    let left = encode_ast(left, context)?;

                    let left = if let Partial::Binding(reg) = left {
                        if op.lexeme == b"=" {
                            context.get(reg)?
                        } else {
                            context.bind(reg)
                        }
                    } else {
                        return Err(Error::ExpectedPattern);
                    };

                    let instr = match right {
                        Partial::Literal(lit) => Instruction {
                            ast,
                            ty: Type::AssignLiteral(left, lit),
                        },
                        Partial::Register(right) => Instruction {
                            ast,
                            ty: Type::Assign(left, right),
                        },
                        Partial::Binding(binding) => {
                            let right = context.get(binding)?;
                            Instruction {
                                ast,
                                ty: Type::Assign(left, right),
                            }
                        }
                        _ => unreachable!(),
                    };

                    context.insert(instr);

                    break Ok(Partial::Complete);
                }
                b"+" | b"-" | b"*" | b"/" => {
                    let left = {
                        let ast = left;
                        let left = encode_ast(left, context)?;
                        value(context, ast, left)?
                    };

                    let right = {
                        let ast = right;
                        let right = encode_ast(right, context)?;
                        value(context, ast, right)?
                    };

                    let out = context.temp();

                    let bin_op = BinOp { out, left, right };

                    let ty = match op.lexeme {
                        b"+" => Type::Add(bin_op),
                        b"-" => Type::Sub(bin_op),
                        b"*" => Type::Mul(bin_op),
                        b"/" => Type::Div(bin_op),
                        _ => unreachable!(),
                    };

                    context.insert(Instruction { ast, ty });

                    break Ok(Partial::Register(out));
                }
                b":" => {
                    let left = {
                        let ast = left;
                        let left = encode_ast(left, context)?;
                        value(context, ast, left)?
                    };

                    let right = {
                        let ast = right;
                        let right = encode_ast(right, context)?;
                        value(context, ast, right)?
                    };

                    context.insert(Instruction {
                        ast,
                        ty: Type::Type(left, right),
                    });

                    break Ok(Partial::Complete);
                }
                _ => unimplemented!("{:?}", ast),
            },
            _ => unimplemented!("{:?}", ast),
        }
    }
}

fn value<'alloc, 'input, I>(
    context: &mut ContextBuilder<'_, 'alloc, 'input>,
    ast: &'alloc Ast<'alloc, 'input>,
    value: Partial<'input>,
) -> Result<'alloc, 'input, Register, I> {
    match value {
        Partial::Register(reg) => Ok(reg),
        Partial::Literal(lit) => {
            let temp = context.temp();
            context.insert(Instruction {
                ast,
                ty: Type::AssignLiteral(temp, lit),
            });
            Ok(temp)
        }
        Partial::Binding(binding) => context.get(binding),
        _ => Err(Error::ExpectedValue),
    }
}
