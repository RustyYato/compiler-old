use arena::Arena;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn arena(cap: usize, size: usize) -> Arena<usize> {
    let arena = Arena::builder().slab_capacity(cap).build();

    for i in 0..size {
        assert_eq!(*arena.insert(i), i);
    }

    arena
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut min_cap = 1000;
    for _ in 0..3 {
        let mut size = 100;
        for _ in 0..4 {
            let name = format!("sync capacity {}, size: {}", min_cap, size);
            c.bench_function(&name, |b| b.iter(|| arena(min_cap, black_box(size))));
            size *= 10;
        }
        min_cap *= 10;
    }
}

criterion_group!(benches, criterion_benchmark);
// criterion_main!(benches);

fn main() {}
