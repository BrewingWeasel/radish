use std::{collections::HashMap, hash::Hash};

pub struct HashOptions<'a, K, V>
where
    K: Eq + Hash,
{
    pub orig: &'a HashMap<K, V>,
    pub secondary: Option<&'a HashMap<K, V>>,
}

impl<'a, K, V> HashOptions<'a, K, V>
where
    K: Eq + Hash,
{
    pub fn get(&self, key: &K) -> Option<&V> {
        if let Some(hash) = &self.secondary {
            if let Some(val) = hash.get(key) {
                return Some(val);
            }
        }
        self.orig.get(key)
    }

    pub fn keys(&self) -> Vec<&K> {
        if self.secondary.is_some() {
            self.orig
                .keys()
                .chain(self.secondary.unwrap().keys())
                .collect()
        } else {
            self.orig.keys().collect()
        }
    }
}
