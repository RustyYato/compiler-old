
use std::hash::{Hash, Hasher};
use std::collections::HashMap;

#[repr(transparent)]
#[derive(Debug)]
pub struct Intern<T: ?Sized>(T);

impl<T: ?Sized> Eq for Intern<T> {}
impl<T: ?Sized> PartialEq for Intern<T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<T: ?Sized> Hash for Intern<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::ptr::hash(self, state)
    }
}

pub struct Interner<'a, T: ?Sized> {
    data: HashMap<&'a T, ()>
}

impl<T: ?Sized + Eq + Hash> Default for Interner<'_, T> {
    #[inline]
    fn default() -> Self { Self::new() }
}

impl<'a, T: ?Sized + Eq + Hash> Interner<'a, T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            data: HashMap::new()
        }
    }

    #[inline]
    pub fn intern<K: AsRef<T> + ?Sized>(&mut self, value: &'a K) -> &'a Intern<T> {
        let entry = self.data.entry(value.as_ref());

        let key: &'a T = *entry.key();

        entry.or_insert(());

        unsafe {
            &*(key as *const T as *const Intern<T>)
        }
    }
}

#[test]
fn intern() {
    let mut interner = Interner::<[u8]>::new();

    let a = String::from("foo");
    let b = String::from("bar");
    let c = String::from("foo");

    let ai = interner.intern(&a);
    let bi = interner.intern(&b);
    let ci = interner.intern(&c);
    let di = interner.intern("bar");

    assert_ne!(ai, bi);
    assert_eq!(ai, ci);
    assert_ne!(ai, di);

    assert_ne!(bi, ci);
    assert_eq!(bi, di);

    assert_ne!(ci, di);
}
