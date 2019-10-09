use lln_peek::LLNPeek;

#[test]
fn peek_test() {
    fn copy<T: Copy>(&t: &T) -> T {
        t
    }
    let mut peek = LLNPeek::new([0, 1, 2, 3, 4, 5].iter().copied(), 4);

    peek.reserve_items(2);
    assert_eq!(peek.peek_iter().map(copy).collect::<Vec<_>>(), [0, 1]);
    peek.reserve_items(4);
    assert_eq!(peek.peek_iter().map(copy).collect::<Vec<_>>(), [0, 1, 2, 3]);

    assert_eq!(peek.by_ref().take(2).collect::<Vec<_>>(), [0, 1]);

    peek.reserve_items(2);
    assert_eq!(peek.peek_iter().map(copy).collect::<Vec<_>>(), [2, 3]);
    peek.reserve_items(4);
    assert_eq!(peek.peek_iter().map(copy).collect::<Vec<_>>(), [2, 3, 4, 5]);
}
