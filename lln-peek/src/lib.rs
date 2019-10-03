
use array_deque::{ArrayDeque, IterMut};
use lexer_ext::{
    token::{Lexer, Token},
    error::TokenRes
};

const N: usize = 10;
type Buffer<T> = ArrayDeque<[T; N]>;

#[derive(Debug, PartialEq, Clone)]
pub struct LLNPeek<'input, L: Lexer<'input> + ?Sized> {
    peek: Buffer<TokenRes<'input, L>>,
    iter: L
}

impl<'input, L: Lexer<'input>> LLNPeek<'input, L> {
    pub fn new(iter: L) -> Self {
        Self {
            peek: Buffer::INIT,
            iter
        }
    }
}

impl<'input, L: Lexer<'input> + ?Sized> Lexer<'input> for LLNPeek<'input, L> {
    type Input = L::Input;

    fn parse_token(&mut self) -> TokenRes<'input, Self> {
        let Self { peek, iter } = self;
        match peek.remove() {
            Some(res) => res,
            None => iter.parse_token()
        }
    }
}

impl<'input, L: Lexer<'input> + ?Sized> LLNPeek<'input, L> {
    pub fn peek_iter(&mut self, n: usize) -> IterMut<'_, TokenRes<'input, L>> {
        let Self { peek, iter } = self;
        
        if peek.len() < n {
            let n = N.min(n);
            let len = peek.len();

            for i in 0..(n - len) {
                peek.insert(iter.parse_token());
            }
        }

        peek.iter_mut()
    }

    pub fn peek_len(&self) -> usize {
        self.peek.len()
    }

    pub fn try_push(&mut self, value: TokenRes<'input, L>) -> Result<(), TokenRes<'input, L>> {
        self.peek.try_insert(value)
    }

    pub fn push(&mut self, value: TokenRes<'input, L>) {
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