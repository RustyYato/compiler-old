#![feature(test)]

extern crate test;

use test::Bencher;

// #[bench]
// pub fn time_me(b: &mut Bencher) {
//     let file = std::fs::read_to_string("/test.rs").expect("Could not read file");

//     b.iter(|| {
//         let mut iter = lexer::iter(&file);

//         (&mut iter).for_each(drop);

//         iter
//     });
// }

fn main() {
    // let alloc = arena::Arena::new(10);
    
    let file = std::env::args().nth(1).expect("Please provide a file name");
    let file = std::fs::read_to_string(file).expect("Could not read file");
    
    let mut parser = parser::ParserImpl::new(
        lexer::LexerImpl::new(&file)
    );

    // println!("{:#?}", parser.parse_let(&alloc));
}
