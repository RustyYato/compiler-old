
use array_deque::{ArrayDeque, IterMut};

const N: usize = 10;
type Buffer<T> = ArrayDeque<[T; N]>;

#[derive(Debug, PartialEq, Clone)]
pub struct LLNPeek<I: Iterator + ?Sized> {
    peek: Buffer<I::Item>,
    iter: I
}

impl<I: Iterator> IteratorExt for I {}
pub trait IteratorExt: Iterator + Sized {
    fn lln_peek(self) -> LLNPeek<Self> {
        LLNPeek {
            peek: Buffer::INIT,
            iter: self
        }
    }
}

impl<I: Iterator> LLNPeek<I> {
    pub fn new(iter: I) -> LLNPeek<I> {
        Self {
            peek: Buffer::INIT,
            iter
        }
    }
}

impl<I: Iterator + ?Sized> Iterator for LLNPeek<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let Self { peek, iter } = self;
        peek.remove().or_else(move || iter.next())
    }
}

impl<I: Iterator + ?Sized> LLNPeek<I> {
    pub fn peek_iter(&mut self, n: usize) -> IterMut<'_, I::Item> {
        let Self { peek, iter } = self;
        
        if peek.len() < n {
            let n = N.min(n);
            let len = peek.len();
            iter.take(n - len).fold((), |(), next| peek.insert(next));
        }

        peek.iter_mut()
    }

    pub fn peek_len(&self) -> usize {
        self.peek.len()
    }

    pub fn try_push(&mut self, value: I::Item) -> Result<(), I::Item> {
        self.peek.try_insert(value)
    }

    pub fn push(&mut self, value: I::Item) {
        self.peek.insert(value)
    }
}

pub fn peek(iter: &mut LLNPeek<dyn Iterator<Item = u32>>, f: fn(&mut u32)) {
    iter.peek_iter(4).for_each(f);
}

#[test]
fn peek_test() {
    fn copy<T: Copy>(&mut t: &mut T) -> T { t }
    let mut peek = [0, 1, 2, 3, 4, 5].iter().copied().lln_peek();

    assert_eq!(peek.peek_iter(2).map(copy).collect::<Vec<_>>(), [0, 1]);
    assert_eq!(peek.peek_iter(4).map(copy).collect::<Vec<_>>(), [0, 1, 2, 3]);
    
    peek.by_ref().take(2).for_each(drop);
    
    assert_eq!(peek.peek_iter(2).map(copy).collect::<Vec<_>>(), [2, 3]);
    assert_eq!(peek.peek_iter(4).map(copy).collect::<Vec<_>>(), [2, 3, 4, 5]);
}