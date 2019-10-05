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
    use lexer_ext::token::Lexer;

    let alloc = arena::Arena::new(10);

    let file = std::env::args().nth(1).expect("Please provide a file name");
    let file = std::fs::read_to_string(file).expect("Could not read file");

    // for token in lexer::LexerImpl::new(&file).iter() {
    //     println!("{:?}", token);
    // }

    let mut lexer = lexer::LexerImpl::new(&file);
    let mut parser = parser::ParserImpl::<&mut dyn Lexer<Input = _>>::new(&mut lexer);

    println!("{}", parser.parse_expr(&alloc).unwrap());
    // println!("{}\n------------------------------------------\n{0:#?}", parser.parse_expr(&alloc).unwrap());
}
