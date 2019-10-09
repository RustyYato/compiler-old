use criterion::{black_box, criterion_group, criterion_main, Criterion};
use arena::LocalArena;

fn arena(cap: usize, size: usize) -> LocalArena<usize> {
    let arena = LocalArena::builder()
        .slab_capacity(cap)
        .build();

    for i in 0..size {
        assert_eq!(*arena.insert(i), i);
    }
    
    arena
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut min_cap = 1000;
    for _ in 0..13 {
        let mut size = 100;
        for _ in 0..15 {
            let name = format!("local capacity {}, size: {}", min_cap, size);
            c.bench_function(&name, |b| b.iter(|| arena(min_cap, black_box(size))));
            size *= 2;
        }
        min_cap *= 2;
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);