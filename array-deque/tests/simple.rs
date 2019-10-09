use array_deque::ArrayDeque;

#[test]
fn simple() {
    let mut array = ArrayDeque::<[_; 10]>::new();

    for i in 0..10 {
        array.insert(i);
    }

    for i in 0..10 {
        assert_eq!(array.remove(), Some(i))
    }

    for i in 0..10 {
        array.insert(i);
    }

    for i in 0..10 {
        assert_eq!(array.remove(), Some(i))
    }
}

#[test]
fn remove() {
    let mut array = ArrayDeque::<[_; 10]>::new();

    for i in 0..10 {
        array.insert(i);
    }

    for i in 0..5 {
        assert_eq!(array.remove(), Some(i))
    }

    for i in 0..5 {
        array.insert(i);
    }

    for i in (5..10).chain(0..5) {
        assert_eq!(array.remove(), Some(i))
    }
}
