
pub use lexer_ext::token::Block;

#[derive(Clone, PartialEq, Eq)]
pub struct BlockVec {
    inner: Vec<u8>,
    offset: u8
}

impl BlockVec {
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: Vec::with_capacity(1),
            offset: 0
        }
    }

    pub fn push(&mut self, block: Block) {
        let block = block as u8;

        if self.offset == 0 {
            self.inner.push(block);
            self.offset = 2;
        } else {
            let slot = self.inner.last_mut().unwrap();
            *slot |= block << self.offset;
            self.offset = (self.offset + 2) & 0b111;
        }
    }

    pub fn pop(&mut self) -> Option<Block> {
        self.offset = self.offset.wrapping_sub(2) & 0b111;

        let slot = if self.offset == 0 {
            self.inner.pop()?
        } else {
            *self.inner.last()? >> self.offset
        };

        match slot & 0b11 {
            1 => Some(Block::Paren),
            2 => Some(Block::Square),
            3 => Some(Block::Curly),
            _ => None
        }
    }
}

#[test]
fn block_vec() {
    use Block::*;
    let mut vec = BlockVec::new();
    let fallback = vec![
        Paren, Paren, Square, Square,
        Square, Paren, Curly, Curly,
        Curly, Paren, Square, Curly,
        Curly, Square, Paren
    ];

    for &i in &fallback {
        vec.push(i);
    }

    let bytes = fallback.chunks(4)
        .map(|x| match *x {
            [a] => a as u8,
            [a, b] => a as u8 | ((b as u8) << 2),
            [a, b, c] => a as u8 | ((b as u8) << 2) | ((c as u8) << 4),
            [a, b, c, d] => a as u8 | ((b as u8) << 2) | ((c as u8) << 4) | ((d as u8) << 6),
            _ => unreachable!(),
        })
        .collect::<Vec<_>>();

    assert_eq!(bytes, vec.inner);

    let vec = std::iter::from_fn(move || vec.pop());

    assert!(vec.eq(fallback.into_iter().rev()));
}

use std::fmt;

impl fmt::Debug for BlockVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lens = std::iter::repeat(4)
            .take(self.inner.len().saturating_sub(1))
            .chain(Some(self.offset >> 1));

        let iter = self.inner.iter()
            .zip(lens)
            .flat_map(|(&slot, len)| {
                (0..len).map(move |offset| slot.wrapping_shr((offset * 2) as u32))
            }).map(|block| match block & 0b11 {
                1 => Block::Paren,
                2 => Block::Square,
                3 => Block::Curly,
                _ => unreachable!(),
            });

        f.debug_list().entries(iter).finish()
    }
}
