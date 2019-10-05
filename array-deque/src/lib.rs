use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::ptr::NonNull;

pub unsafe trait Array {
    type T;
    const LEN: usize;
}

macro_rules! impl_array {
    ($($count:literal),*) => {$(
        unsafe impl<T> Array for [T; $count] {
            type T = T;
            const LEN: usize = $count;
        }
    )*};
}

impl_array! {
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
}

pub struct ArrayDeque<A: Array> {
    first: usize,
    len: usize,
    array: MaybeUninit<A>,
}

use std::fmt;
impl<A: Array> fmt::Debug for ArrayDeque<A>
where
    A::T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self).finish()
    }
}

impl<A: Array, B: Array> PartialEq<ArrayDeque<B>> for ArrayDeque<A>
where
    A::T: PartialEq<B::T>,
{
    fn eq(&self, other: &ArrayDeque<B>) -> bool {
        self.iter().eq(other)
    }
}

impl<A: Array> Clone for ArrayDeque<A>
where
    A::T: Clone,
{
    fn clone(&self) -> Self {
        let mut new = Self::new();

        self.iter().cloned().for_each(|item| new.insert(item));

        new
    }
}

impl<A: Array> Default for ArrayDeque<A> {
    fn default() -> Self {
        Self::INIT
    }
}

impl<A: Array> ArrayDeque<A> {
    #[allow(clippy::declare_interior_mutable_const)]
    pub const INIT: Self = Self {
        first: 0,
        len: 0,
        array: MaybeUninit::uninit(),
    };

    pub fn new() -> Self {
        Self::INIT
    }

    pub fn as_ptr(&self) -> *const A::T {
        self.array.as_ptr() as *const A::T
    }

    pub fn as_mut_ptr(&mut self) -> *mut A::T {
        self.array.as_mut_ptr() as *mut A::T
    }

    pub fn as_slice(&self) -> &[A::T] {
        unsafe { std::slice::from_raw_parts(self.as_ptr(), self.len) }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn as_slice_mut(&mut self) -> &mut [A::T] {
        unsafe { std::slice::from_raw_parts_mut(self.as_mut_ptr(), self.len) }
    }

    pub fn try_insert(&mut self, value: A::T) -> Result<(), A::T> {
        if A::LEN == 0 || self.len == A::LEN {
            Err(value)
        } else {
            unsafe {
                let idx = (self.first + self.len) % A::LEN;
                self.as_mut_ptr().add(idx).write(value);
                self.len += 1;
            }
            Ok(())
        }
    }

    pub fn insert(&mut self, value: A::T) {
        self.try_insert(value)
            .ok()
            .expect("Could not push value onto ArrayVec<T>")
    }

    pub fn remove(&mut self) -> Option<A::T> {
        if A::LEN == 0 || self.len == 0 {
            None
        } else {
            unsafe {
                let idx = self.first;
                self.first = if self.first == A::LEN {
                    0
                } else {
                    self.first + 1
                };
                self.len -= 1;
                Some(self.as_mut_ptr().add(idx).read())
            }
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, A::T> {
        self.into_iter()
    }

    pub fn iter(&self) -> Iter<'_, A::T> {
        self.into_iter()
    }
}

impl<A: Array> Drop for ArrayDeque<A> {
    fn drop(&mut self) {}
}

// pub struct IntoIter<A: Array> {
//     first: usize,
//     len: usize,
//     array: MaybeUninit<A>,
// }

// impl<A: Array> Drop for IntoIter<A> {
//     fn drop(&mut self) {
//         self.for_each(drop);
//     }
// }

// impl<A: Array> IntoIterator for ArrayDeque<A> {
//     type Item = A::T;
//     type IntoIter = IntoIter<A>;

//     fn into_iter(mut self) -> Self::IntoIter {
//         IntoIter {
//             first: self.first,
//             len: std::mem::replace(&mut self.len, 0),
//             array: std::mem::replace(&mut self.array, MaybeUninit::uninit())
//         }
//     }
// }

// impl<A: Array> IntoIter<A> {
//     pub fn as_ptr(&self) -> *const A::T {
//         self.array.as_ptr() as *const A::T
//     }

//     pub fn as_mut_ptr(&mut self) -> *mut A::T {
//         self.array.as_mut_ptr() as *mut A::T
//     }
// }

// impl<A: Array> ExactSizeIterator for IntoIter<A> {}
// impl<A: Array> std::iter::FusedIterator for IntoIter<A> {}

// impl<A: Array> Iterator for IntoIter<A> {
//     type Item = A::T;

//     fn next(&mut self) -> Option<Self::Item> {
//         if A::LEN == 0 || self.len == 0 {
//             None
//         } else {
//             unsafe {
//                 let idx = self.first;
//                 self.first = if self.first == A::LEN {
//                     0
//                 } else {
//                     self.first + 1
//                 };
//                 self.len -= 1;
//                 Some(self.as_mut_ptr().add(idx).read())
//             }
//         }
//     }

//     fn size_hint(&self) -> (usize, Option<usize>) {
//         (self.len, Some(self.len))
//     }
// }

// impl<A: Array> DoubleEndedIterator for IntoIter<A> {
//     fn next_back(&mut self) -> Option<Self::Item> {
//         if A::LEN == 0 {
//             None
//         } else {
//             unsafe {
//                 self.len -= 1;
//                 let idx = (self.first + self.len) % A::LEN;
//                 Some(self.as_mut_ptr().add(idx).read())
//             }
//         }
//     }
// }

pub struct Iter<'a, T> {
    first: usize,
    len: usize,
    cap: usize,
    ptr: NonNull<T>,
    mark: PhantomData<&'a [T]>,
}

impl<'a, A: Array> IntoIterator for &'a ArrayDeque<A> {
    type Item = &'a A::T;
    type IntoIter = Iter<'a, A::T>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            first: self.first,
            len: self.len,
            cap: A::LEN,
            ptr: unsafe { NonNull::new_unchecked(self.as_ptr() as *mut _) },
            mark: PhantomData,
        }
    }
}

impl<T> ExactSizeIterator for Iter<'_, T> {}
impl<T> std::iter::FusedIterator for Iter<'_, T> {}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(len) = self.len.checked_sub(1) {
            self.len = len;

            unsafe {
                let idx = if self.first == self.cap {
                    0
                } else {
                    self.first
                };
                self.first = idx + 1;

                Some(&mut *self.ptr.as_ptr().add(idx))
            }
        } else {
            None
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.len = self.len.saturating_sub(n);
        let first = self.first + n;
        self.first = first.checked_sub(self.cap).unwrap_or(first);
        self.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

impl<T> DoubleEndedIterator for Iter<'_, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(len) = self.len.checked_sub(1) {
            self.len = len;

            unsafe {
                let idx = self.first + self.len;

                let idx = if idx >= self.cap { idx - self.cap } else { idx };

                Some(&mut *self.ptr.as_ptr().add(idx))
            }
        } else {
            None
        }
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        self.len = self.len.saturating_sub(n);
        self.next_back()
    }
}

pub struct IterMut<'a, T> {
    first: usize,
    len: usize,
    cap: usize,
    ptr: NonNull<T>,
    mark: PhantomData<&'a mut [T]>,
}

impl<'a, A: Array> IntoIterator for &'a mut ArrayDeque<A> {
    type Item = &'a mut A::T;
    type IntoIter = IterMut<'a, A::T>;

    fn into_iter(self) -> Self::IntoIter {
        IterMut {
            first: self.first,
            len: self.len,
            cap: A::LEN,
            ptr: unsafe { NonNull::new_unchecked(self.as_mut_ptr()) },
            mark: PhantomData,
        }
    }
}

impl<T> ExactSizeIterator for IterMut<'_, T> {}
impl<T> std::iter::FusedIterator for IterMut<'_, T> {}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(len) = self.len.checked_sub(1) {
            self.len = len;

            unsafe {
                let idx = if self.first == self.cap {
                    0
                } else {
                    self.first
                };
                self.first = idx + 1;

                Some(&mut *self.ptr.as_ptr().add(idx))
            }
        } else {
            None
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.len = self.len.saturating_sub(n);
        let first = self.first + n;
        self.first = first.checked_sub(self.cap).unwrap_or(first);
        self.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

impl<T> DoubleEndedIterator for IterMut<'_, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(len) = self.len.checked_sub(1) {
            self.len = len;

            unsafe {
                let idx = self.first + self.len;

                let idx = if idx >= self.cap { idx - self.cap } else { idx };

                Some(&mut *self.ptr.as_ptr().add(idx))
            }
        } else {
            None
        }
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        self.len = self.len.saturating_sub(n);
        self.next_back()
    }
}

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
