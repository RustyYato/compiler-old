
use arena::Arena;
use std::sync::{Arc, Barrier};
use crossbeam::scope;

#[test]
pub fn seq_insert() {
    let arena = Arena::builder().build();
    
    for i in 0..1000 {
        assert_eq!(*arena.insert(i), i);
    }
}

#[test]
pub fn par_insert() {
    let arena = Arena::builder().build();
    let arena = Arc::new(arena);
    
    for _ in 0..10 {
        std::thread::spawn({
            let arena = Arc::clone(&arena);
            move || {
                for i in 0..1000 {
                    assert_eq!(*arena.insert(i), i);
                }
            }
        });
    }
}

#[test]
pub fn stampede_par_insert() {
    let threads = 100;
    let arena = Arena::builder().build();
    let arena = Arc::new(arena);
    let barrier = Arc::new(Barrier::new(threads));
    
    for _ in 0..threads {
        std::thread::spawn({
            let arena = Arc::clone(&arena);
            let barrier = Arc::clone(&barrier);
            move || {
                barrier.wait();
                for i in 0..10000 {
                    assert_eq!(*arena.insert(i), i);
                }
            }
        });
    }
}

#[test]
pub fn list_check() {
    enum Node<'a> {
        Prev(&'a mut Node<'a>, usize),
        Item(usize)
    }

    let threads = 10;
    let size = 10_000;
    
    let arena = Arena::builder().slab_capacity(1000).build();
    let barrier = Barrier::new(threads);
    let arena = &arena;
    let barrier = &barrier;

    let nodes: Vec<_> = scope(move |s| {
        let mut joins = Vec::new();
        let mut nodes = Vec::new();

        for _ in 0..threads {
            let join = s.spawn(move |_| {
                let mut node = Node::Item(0);
                barrier.wait();
                for i in 1..=size {
                    let prev = arena.insert(node);
                    node = Node::Prev(prev, i);
                }
                node
            });

            joins.push(join);
        }

        for join in joins {
            nodes.push(join.join().unwrap())
        }

        nodes
    }).unwrap();

    scope(move |s| {
        for mut node in nodes {
            s.spawn(move |_| {
                let mut node = &mut node;

                for i in (1..=size).rev() {
                    match node {
                        Node::Item(_) => panic!(),
                        Node::Prev(prev, x) => {
                            assert_eq!(*x, i);
                            node = prev
                        }
                    }
                }

                match node {
                    Node::Item(0) => (),
                    _ => panic!()
                }
                

            });
        }    
    }).unwrap();
}
