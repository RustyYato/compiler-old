
use array_deque::{ArrayDeque, Iter};
use lexer_ext::{
    token::{Lexer, Token},
    error::TokenRes
};

const N: usize = 10;
type Buffer<T> = ArrayDeque<[T; N]>;

#[derive(Debug, PartialEq, Clone)]
pub struct LLNPeek<Inner: ?Sized, T> {
    peek: Buffer<T>,
    inner: Inner
}

impl<L, T> LLNPeek<L, T> {
    pub fn new(inner: L) -> Self {
        Self {
            peek: Buffer::INIT,
            inner
        }
    }
}

impl<I: Iterator + ?Sized> Iterator for LLNPeek<I, I::Item> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        let Self { peek, inner } = self;

        peek.remove().or_else(move || inner.next())
    }
}

impl<'input, L: Lexer<'input> + ?Sized> Lexer<'input> for LLNPeek<L, TokenRes<'input, L>> {
    type Input = L::Input;

    fn parse_token(&mut self) -> TokenRes<'input, Self> {
        let Self { peek, inner } = self;
        match peek.remove() {
            Some(res) => res,
            None => inner.parse_token()
        }
    }
}

impl<'input, L: Lexer<'input> + ?Sized> LLNPeek<L, TokenRes<'input, L>> {
    pub fn reserve_tokens(&mut self, n: usize) {
        let Self { peek, inner } = self;
        
        if peek.len() < n {
            let n = N.min(n);
            let len = peek.len();

            for i in 0..(n - len) {
                peek.insert(inner.parse_token());
            }
        }
    }
    
    pub fn peek_len(&self) -> usize {
        self.peek.len()
    }
}
impl<T, Inner: ?Sized> LLNPeek<Inner, T> {
    pub fn peek_iter(&self) -> Iter<'_, T> {
        self.peek.iter()
    }
    
    pub fn peek(&self) -> Option<&T> {
        self.peek.iter().next()
    }

    pub fn try_push(&mut self, value: T) -> Result<(), T> {
        self.peek.try_insert(value)
    }

    pub fn push(&mut self, value: T) {
        self.peek.insert(value)
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