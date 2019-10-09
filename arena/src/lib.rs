#![allow(clippy::mut_from_ref)]

use crossbeam::sync::ShardedLock;
use std::cell::UnsafeCell;

pub mod slab;
use self::slab::Slab;
use slab::TryInsert;

use std::marker::PhantomData;

pub trait Allocator {
    type Item;

    fn alloc(&self, item: Self::Item) -> &mut Self::Item;
}

impl<T: ?Sized + Allocator> Allocator for &T {
    type Item = T::Item;

    fn alloc(&self, item: Self::Item) -> &mut Self::Item {
        T::alloc(self, item)
    }
}

impl<T: ?Sized + Allocator> Allocator for &mut T {
    type Item = T::Item;

    fn alloc(&self, item: Self::Item) -> &mut Self::Item {
        T::alloc(self, item)
    }
}

impl<T: ?Sized + Allocator> Allocator for Box<T> {
    type Item = T::Item;

    fn alloc(&self, item: Self::Item) -> &mut Self::Item {
        T::alloc(self, item)
    }
}

pub struct ArenaBuilder<T> {
    slab_capacity: usize,
    slab_count: usize,
    ty: PhantomData<T>
}

impl<T> ArenaBuilder<T> {
    fn new() -> Self {
        Self {
            slab_capacity: 100,
            slab_count: 4,
            ty: PhantomData
        }
    }

    pub fn slab_capacity(&mut self, capacity: usize) -> &mut Self {
        assert_ne!(capacity, 0, "An slabs of an arena must have a non-zero capacity");
        self.slab_capacity = capacity;
        self
    } 
}

impl<T> ArenaBuilder<Arena<T>> {
    pub fn slab_count(&mut self, count: usize) -> &mut Self {
        assert_ne!(count, 0, "An arena must have a non-zero number of slabs");
        self.slab_count = count;
        self
    }

    pub fn build(&mut self) -> Arena<T> {
        Arena {
            slab_capacity: self.slab_capacity,
            slab_count: self.slab_count,
            data: ShardedLock::new(ArenaData {
                slabs: Vec::new(),
                index: 0
            }),
        }
    }
}

impl<T> ArenaBuilder<LocalArena<T>> {
    pub fn build(&mut self) -> LocalArena<T> {
        LocalArena {
            slab_capacity: self.slab_capacity,
            slabs: UnsafeCell::new(Vec::new()),
        }
    }
}

struct ArenaData<T> {
    slabs: Vec<Slab<T>>,
    index: usize
}

pub struct Arena<T> {
    data: ShardedLock<ArenaData<T>>,
    slab_capacity: usize,
    slab_count: usize
}

impl<T> Allocator for Arena<T> {
    type Item = T;

    fn alloc(&self, item: T) -> &mut T {
        self.insert(item)
    }
}

impl<T> Arena<T> {
    pub fn builder() -> ArenaBuilder<Arena<T>> {
        ArenaBuilder::new()
    }

    pub fn insert(&self, mut value: T) -> &mut T {
        use Result::{Err as Break, Ok as Continue};

        loop {
            let (no_capacity, next_value) = {
                let data = self.data.read().unwrap();
                
                let result = data.slabs[data.index..].iter().try_fold((true, value), |(cap, value), slab| {
                    match slab.try_insert(value) {
                        TryInsert::ReachedCapacity(value) => Continue((cap, value)),
                        TryInsert::Blocked(value) => Continue((false, value)),
                        TryInsert::Complete(value) => Break(value),
                    }
                });

                match result {
                    Continue((no_capacity, value)) => (no_capacity, value),
                    Break(value) => return unsafe { &mut *value },
                }
            };

            value = next_value;

            if no_capacity {
                let mut data = self.data.write().unwrap();

                data.index = data.slabs.len();

                let add_len = (data.index / 2).max(self.slab_count);

                data.slabs.extend((0..add_len).map(|_| Slab::new(self.slab_capacity)));
            }
        }
    }
}

pub struct LocalArena<T> {
    slabs: UnsafeCell<Vec<Vec<T>>>,
    slab_capacity: usize
}

impl<T> Allocator for LocalArena<T> {
    type Item = T;

    fn alloc(&self, item: T) -> &mut T {
        self.insert(item)
    }
}

impl<T> LocalArena<T> {
    pub fn builder() -> ArenaBuilder<LocalArena<T>> {
        ArenaBuilder::new()
    }

    fn try_insert(slabs: &mut Vec<T>, value: T) -> Result<*mut T, T> {
        if slabs.len() < slabs.capacity() {
            let len = slabs.len();

            slabs.push(value);

            unsafe { Ok(slabs.get_unchecked_mut(len)) }
        } else {
            Err(value)
        }
    }

    pub fn insert(&self, mut value: T) -> &mut T {
        let slabs = unsafe {
            &mut *self.slabs.get()
        };
        
        loop {
            if let Some(slab) = slabs.last_mut() {
                match Self::try_insert(slab, value) {
                    Ok(value) => return unsafe { &mut *value },
                    Err(old_value) => value = old_value,
                }
            }

            slabs.push(Vec::with_capacity(self.slab_capacity));
        }
    }
}
