fn main() {
    use parser_ext::ast::WithAllocator;
    use bit_code_ext::builder::Table;
    use bit_code::Encoder;

    let alloc = arena::LocalArena::builder().build();
    let mut table = Table::default();

    let file = std::env::args().nth(1).expect("Please provide a file name");
    let file = std::fs::read_to_string(file).expect("Could not read file");

    let mut lexer = lexer::LexerImpl::new(&file);
    let parser = parser::ParserImpl::new(&mut lexer);
    let parser = WithAllocator::new(parser, &alloc);
    let encoder = Encoder::new(&mut table, parser);

    encoder.encode().unwrap();

    println!("{:#?}", table);

    // let mut parse_iter = parser.parse(&alloc);

    // for ast in parse_iter.iter() {
    //     println!("{}", ast);
    // }

    // println!("ERR = {:?}", parse_iter.err().unwrap());
}
