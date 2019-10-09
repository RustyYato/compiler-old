use array_deque::ArrayDeque;

#[test]
fn iter_mut() {
    let mut deque = ArrayDeque::<[u32; 4]>::INIT;

    deque.insert(0);

    deque.remove();
    deque.insert(0);

    deque.insert(1);
    deque.insert(2);
    deque.insert(3);

    assert!(deque.iter_mut().eq([0, 1, 2, 3].iter_mut()));
    assert!(deque.iter_mut().rev().eq([0, 1, 2, 3].iter_mut().rev()));
}

#[test]
fn skip_iter_mut() {
    let mut deque = ArrayDeque::<[u32; 8]>::INIT;

    deque.insert(0);
    deque.insert(0);

    deque.remove();
    deque.remove();

    deque.insert(0);
    deque.insert(1);
    deque.insert(2);
    deque.insert(3);
    deque.insert(4);
    deque.insert(5);
    deque.insert(6);
    deque.insert(7);

    let mut array = [0, 1, 2, 3, 4, 5, 6, 7];

    assert!(deque.iter_mut().step_by(2).eq(array.iter_mut().step_by(2)));
    assert!(deque.iter_mut().step_by(8).eq(array.iter_mut().step_by(8)));

    assert!(deque
        .iter_mut()
        .rev()
        .step_by(2)
        .eq(array.iter_mut().rev().step_by(2)));
    assert!(deque
        .iter_mut()
        .rev()
        .step_by(8)
        .eq(array.iter_mut().rev().step_by(8)));
}
