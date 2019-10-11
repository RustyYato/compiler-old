use super::repr::*;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Table<'alloc, 'input> {
    contexts: Vec<Context<'alloc, 'input>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Context<'alloc, 'input> {
    parent: usize,
    bindings: Vec<&'input [u8]>,
    instrs: Vec<Instruction<'alloc, 'input>>,
    children: Vec<usize>,
}

pub struct ContextBuilder<'table, 'alloc, 'input> {
    table: &'table mut Table<'alloc, 'input>,
    index: usize,
    temp_count: u64,
    context: Context<'alloc, 'input>,
}

impl<'alloc, 'input> Context<'alloc, 'input> {
    pub fn parent<'table>(&self, table: &'table Table<'alloc, 'input>) -> Option<&'table Self> {
        let parent = &table.contexts[self.parent];

        if std::ptr::eq(self, parent) {
            None
        } else {
            Some(parent)
        }
    }

    pub fn instructions(&self) -> &[Instruction<'alloc, 'input>] {
        &self.instrs
    }

    pub fn bindings(&self) -> &[&'input [u8]] {
        &self.bindings
    }

    pub fn children<'table>(
        &self,
        table: &'table Table<'alloc, 'input>,
    ) -> Children<'_, 'table, 'alloc, 'input> {
        Children {
            table,
            children: self.children.iter(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Children<'ctx, 'table, 'alloc, 'input> {
    table: &'table Table<'alloc, 'input>,
    children: std::slice::Iter<'ctx, usize>,
}

impl<'table, 'alloc, 'input> ExactSizeIterator for Children<'_, 'table, 'alloc, 'input> {}
impl<'table, 'alloc, 'input> std::iter::FusedIterator for Children<'_, 'table, 'alloc, 'input> {}

impl<'table, 'alloc, 'input> Iterator for Children<'_, 'table, 'alloc, 'input> {
    type Item = &'table Context<'alloc, 'input>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let &child = self.children.next()?;
        Some(&self.table.contexts[child])
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.children.size_hint()
    }

    #[inline]
    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        let &child = self.children.nth(n)?;
        Some(&self.table.contexts[child])
    }
}

impl<'table, 'alloc, 'input> DoubleEndedIterator for Children<'_, 'table, 'alloc, 'input> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        let &child = self.children.next_back()?;
        Some(&self.table.contexts[child])
    }

    #[inline]
    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        let &child = self.children.nth_back(n)?;
        Some(&self.table.contexts[child])
    }
}

impl<'alloc, 'input> Table<'alloc, 'input> {
    pub fn new_context(&mut self) -> ContextBuilder<'_, 'alloc, 'input> {
        let index = self.contexts.len();
        let context = Context {
            parent: index,
            bindings: Vec::new(),
            instrs: Vec::new(),
            children: Vec::new(),
        };

        self.contexts.push(context.clone());

        ContextBuilder {
            index,
            context,
            temp_count: 0,
            table: self
        }
    }
}

impl<'table, 'alloc, 'input> ContextBuilder<'table, 'alloc, 'input> {
    pub fn new_context(&mut self) -> ContextBuilder<'_, 'alloc, 'input> {
        let index = self.table.contexts.len();

        let context = Context {
            parent: self.index,
            bindings: Vec::new(),
            instrs: Vec::new(),
            children: Vec::new(),
        };

        self.table.contexts.push(context.clone());

        ContextBuilder {
            index,
            context,
            temp_count: 0,
            table: &mut self.table,
        }
    }

    pub fn get<I>(&mut self, binding: &'input [u8]) -> crate::error::Result<'alloc, 'input, Register, I> {
        let index = self.context.bindings.iter().position(|&bind| bind == binding);

        match index {
            Some(index) => Ok(Register(((index as u64) << 1) | 1)),
            None => Err(crate::error::Error::BindingNotFound(binding))
        }
    }

    pub fn bind(&mut self, binding: &'input [u8]) -> Register {
        let index = self.context.bindings.len() as u64;
        self.context.bindings.push(binding);
        Register((index << 1) | 1)
    }

    pub fn temp(&mut self) -> Register {
        let index = self.temp_count;
        self.temp_count += 1;
        Register(index << 1)
    }

    pub fn insert(&mut self, instr: Instruction<'alloc, 'input>) {
        self.context.instrs.push(instr);
    }
}

impl Drop for ContextBuilder<'_, '_, '_> {
    fn drop(&mut self) {
        std::mem::swap(&mut self.table.contexts[self.index], &mut self.context);
    }
}
