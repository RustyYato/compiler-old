use std::sync::Mutex;

pub struct Slab<T> {
    data: Mutex<Vec<T>>,
}

pub enum TryInsert<T> {
    Blocked(T),
    ReachedCapacity(T),
    Complete(*mut T),
}

impl<T> Slab<T> {
    pub fn new(capacity: usize) -> Self {
        Self {
            data: Mutex::new(Vec::with_capacity(capacity)),
        }
    }

    pub fn try_insert(&self, value: T) -> TryInsert<T> {
        let mut data = match self.data.try_lock() {
            Ok(data) => data,
            Err(_) => return TryInsert::Blocked(value),
        };

        if data.len() < data.capacity() {
            let len = data.len();

            data.push(value);

            unsafe { TryInsert::Complete(data.get_unchecked_mut(len)) }
        } else {
            TryInsert::ReachedCapacity(value)
        }
    }
}
