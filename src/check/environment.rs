use std::collections::HashMap;
use std::hash::Hash;

/// An environment is a stack of scopes.
/// Each scope is a map of keys to values with a parent scope.
#[derive(Debug)]
pub struct Environment<K, V> {
    parent: Option<Box<Environment<K, V>>>,
    values: HashMap<K, V>,
}

#[allow(dead_code)]
impl<K, V> Environment<K, V>
where
    K: Hash + Eq,
    V: Hash + Eq,
{
    pub fn new() -> Self {
        Self {
            parent: None,
            values: HashMap::new(),
        }
    }

    pub fn new_with_parent(parent: Box<Environment<K, V>>) -> Self {
        Self {
            parent: Some(parent),
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, key: K, value: V) {
        self.values.insert(key, value);
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        if let Some(value) = self.values.get(key) {
            return Some(value);
        }

        if let Some(parent) = &self.parent {
            return parent.get(key);
        }

        None
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        if let Some(value) = self.values.get_mut(key) {
            return Some(value);
        }

        if let Some(parent) = &mut self.parent {
            return parent.get_mut(key);
        }

        None
    }

    pub fn assign(&mut self, key: &K, value: V) -> Option<V> {
        if let Some(destination) = self.values.get_mut(key) {
            return Some(std::mem::replace(destination, value));
        }

        if let Some(parent) = &mut self.parent {
            return parent.assign(key, value);
        }

        None
    }

    pub fn push(&mut self) {
        let parent = std::mem::replace(self, Self::new());
        self.parent = Some(Box::new(parent));
    }

    pub fn pop(&mut self) {
        if let Some(parent) = self.parent.take() {
            *self = *parent;
        }
    }

    pub fn parent(&self) -> Option<&Box<Environment<K, V>>> {
        self.parent.as_ref()
    }

    pub fn get_root(&self) -> &Self {
        if let Some(parent) = &self.parent {
            return parent.get_root();
        }

        self
    }
}
