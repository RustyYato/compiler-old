#![forbid(unsafe_code)]

use array_deque::{ArrayDeque, Iter};
use lexer_ext::{
    error::TokenRes,
    token::{Lexer, Token},
};

// const N: usize = 10;
// type Buffer<T> = ArrayDeque<[T; N]>;

use std::collections::VecDeque;

#[derive(Debug, PartialEq, Clone)]
pub struct LLNPeek<Inner: ?Sized, T> {
    peek: VecDeque<T>,
    inner: Inner,
}

impl<L, T> LLNPeek<L, T> {
    pub fn new(inner: L, capacity: usize) -> Self {
        Self {
            peek: VecDeque::with_capacity(capacity),
            inner,
        }
    }
}

impl<I: Iterator + ?Sized> Iterator for LLNPeek<I, I::Item> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        let Self { peek, inner } = self;

        peek.pop_back().or_else(move || inner.next())
    }
}

impl<'input, L: Lexer<'input> + ?Sized> Lexer<'input> for LLNPeek<L, TokenRes<'input, L>> {
    type Input = L::Input;

    fn parse_token(&mut self) -> TokenRes<'input, Self> {
        let Self { peek, inner } = self;
        match peek.pop_back() {
            Some(res) => res,
            None => inner.parse_token(),
        }
    }
}

impl<'input, L: Lexer<'input> + ?Sized> LLNPeek<L, TokenRes<'input, L>> {
    #[inline]
    pub fn reserve_tokens(&mut self, n: usize) {
        let Self { peek, inner } = self;

        if peek.len() < n {
            let n = peek.capacity().min(n);
            let len = peek.len();

            for _ in 0..(n - len) {
                peek.push_front(inner.parse_token());
            }
        }
    }
}

impl<T, Inner: ?Sized> LLNPeek<Inner, T> {
    #[inline]
    pub fn peek_iter(&self) -> impl Iterator<Item = &T> {
        self.peek.iter()
    }

    #[inline]
    pub fn peek(&self) -> Option<&T> {
        self.peek.iter().next()
    }

    #[inline]
    pub fn try_push(&mut self, value: T) -> Result<(), T> {
        if self.peek.len() < self.peek.capacity() {
            self.peek.push_front(value);
            Ok(())
        } else {
            Err(value)
        }
    }

    #[inline]
    pub fn push(&mut self, value: T) {
        assert!(
            self.peek.len() < self.peek.capacity(),
            "Tried to push into a full buffer"
        );
        self.peek.push_front(value);
    }

    #[inline]
    pub fn peek_len(&self) -> usize {
        self.peek.len()
    }
}

// pub fn peek(iter: &mut LLNPeek<dyn Iterator<Item = u32>>, f: fn(&mut u32)) {
//     iter.peek_iter(4).for_each(f);
// }

// #[test]
// fn peek_test() {
//     fn copy<T: Copy>(&mut t: &mut T) -> T { t }
//     let mut peek = [0, 1, 2, 3, 4, 5].iter().copied().lln_peek();

//     assert_eq!(peek.peek_iter(2).map(copy).collect::<Vec<_>>(), [0, 1]);
//     assert_eq!(peek.peek_iter(4).map(copy).collect::<Vec<_>>(), [0, 1, 2, 3]);

//     peek.by_ref().take(2).for_each(drop);

//     assert_eq!(peek.peek_iter(2).map(copy).collect::<Vec<_>>(), [2, 3]);
//     assert_eq!(peek.peek_iter(4).map(copy).collect::<Vec<_>>(), [2, 3, 4, 5]);
// }
