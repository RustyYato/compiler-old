use std::sync::RwLock;

pub mod slab;
use self::slab::Slab;

pub struct Arena<T> {
    slabs: RwLock<Vec<Slab<T>>>,
    slab_capacity: usize,
}

impl<T> Arena<T> {
    pub fn new(slab_capacity: usize) -> Self {
        Self {
            slabs: RwLock::new(Vec::new()),
            slab_capacity,
        }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn insert(&self, mut value: T) -> &mut T {
        use Result::{Err as Break, Ok as Continue};

        loop {
            let (no_capacity, next_value) = {
                let slabs = self.slabs.read().unwrap();

                let result = slabs.iter().try_fold((true, value), |(cap, value), slab| {
                    use slab::TryInsert;
                    match slab.try_insert(value) {
                        TryInsert::ReachedCapacity(value) => Continue((cap, value)),
                        TryInsert::Blocked(value) => Continue((false, value)),
                        TryInsert::Complete(value) => Break(value),
                    }
                });

                match result {
                    Ok((no_capacity, value)) => (no_capacity, value),
                    Err(value) => return unsafe { &mut *value },
                }
            };

            value = next_value;

            if no_capacity {
                let mut slabs = self.slabs.write().unwrap();

                let add_len = slabs.len() / 2;
                let add_len = if add_len == 0 { 4 } else { add_len };

                slabs.extend((0..add_len).map(|_| Slab::new(self.slab_capacity)))
            }
        }
    }
}
