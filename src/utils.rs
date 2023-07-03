use std::{collections::HashMap, hash::Hash};

pub struct HashOptions<'a, K, V>
where
    K: Eq + Hash,
{
    pub orig: &'a HashMap<K, V>,
    pub secondary: Option<Vec<&'a HashMap<K, V>>>,
}

impl<'a, K, V> HashOptions<'a, K, V>
where
    K: Eq + Hash,
{
    pub fn get(&self, key: &K) -> Option<&V> {
        if let Some(hashes) = &self.secondary {
            for hash in hashes {
                if let Some(val) = hash.get(key) {
                    return Some(val);
                }
            }
        }
        self.orig.get(key)
    }

    pub fn keys(&self) -> Vec<&K> {
        if let Some(workspace_hashes) = &self.secondary {
            let workspace_keys: Vec<&K> = workspace_hashes
                .into_iter()
                .map(|x| x.keys())
                .flatten()
                .collect();
            // workspace_keys.append(&self.orig.keys().collect());
            [workspace_keys, self.orig.keys().collect()].concat()
        } else {
            self.orig.keys().collect()
        }
    }
}
