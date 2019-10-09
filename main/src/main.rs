fn main() {
    use lexer_ext::token::Lexer;

    let alloc = arena::Arena::builder().build();

    let file = std::env::args().nth(1).expect("Please provide a file name");
    let file = std::fs::read_to_string(file).expect("Could not read file");

    // for token in lexer::LexerImpl::new(&file).iter() {
    //     println!("{:?}", token);
    // }

    let mut lexer = lexer::LexerImpl::new(&file);
    let mut parser = parser::ParserImpl::<&mut dyn Lexer<Input = _>>::new(&mut lexer);

    let mut parse_iter = parser.parse(&alloc);

    for ast in parse_iter.iter() {
        println!("{}", ast);
    }

    println!("ERR = {:?}", parse_iter.err().unwrap());

    // println!("------------------------------------------");

    // for ast in ast {
    //     println!("{:?}", ast);
    // }
}
