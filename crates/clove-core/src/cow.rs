use im::{HashMap as ImHashMap, Vector as ImVector};
use std::collections::{HashMap as StdHashMap, VecDeque};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

#[derive(Clone)]
pub struct Vector<T>(Arc<VecDeque<T>>);
// TODO: Vector mutations clone the full VecDeque when shared; consider a persistent/small vector.

impl<T> Vector<T> {
    pub fn new() -> Self {
        Self(Arc::new(VecDeque::new()))
    }

    pub fn into_vec(self) -> Vec<T>
    where
        T: Clone,
    {
        match Arc::try_unwrap(self.0) {
            Ok(deque) => deque.into_iter().collect(),
            Err(shared) => shared.iter().cloned().collect(),
        }
    }
}

impl<T> Default for Vector<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> From<Vec<T>> for Vector<T> {
    fn from(items: Vec<T>) -> Self {
        Self(Arc::new(VecDeque::from(items)))
    }
}

impl<T> From<VecDeque<T>> for Vector<T> {
    fn from(items: VecDeque<T>) -> Self {
        Self(Arc::new(items))
    }
}

impl<T: Clone> From<ImVector<T>> for Vector<T> {
    fn from(items: ImVector<T>) -> Self {
        Self(Arc::new(VecDeque::from_iter(items.into_iter())))
    }
}

impl<T> FromIterator<T> for Vector<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(Arc::new(VecDeque::from_iter(iter)))
    }
}

impl<T: Clone> IntoIterator for Vector<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_vec().into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Vector<T> {
    type Item = &'a T;
    type IntoIter = std::collections::vec_deque::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T> Deref for Vector<T> {
    type Target = VecDeque<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Clone> DerefMut for Vector<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Arc::make_mut(&mut self.0)
    }
}

impl<T: PartialEq> PartialEq for Vector<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Eq> Eq for Vector<T> {}

impl<T: Hash> Hash for Vector<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for item in self.0.iter() {
            item.hash(state);
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Vector<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.0.iter()).finish()
    }
}

const SMALL_MAP_MAX: usize = 24;

#[derive(Clone)]
enum MapRepr<K, V> {
    Small(Vec<(K, V)>),
    Large(ImHashMap<K, V>),
}

#[derive(Clone)]
pub struct HashMap<K, V>(MapRepr<K, V>);

impl<K, V> HashMap<K, V>
where
    K: Eq + Hash,
{
    pub fn new() -> Self {
        Self(MapRepr::Small(Vec::new()))
    }

    pub fn reserve(&mut self, additional: usize) {
        if let MapRepr::Small(entries) = &mut self.0 {
            entries.reserve(additional);
        }
    }

    pub fn len(&self) -> usize {
        match &self.0 {
            MapRepr::Small(entries) => entries.len(),
            MapRepr::Large(map) => map.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn contains_key(&self, key: &K) -> bool {
        match &self.0 {
            MapRepr::Small(entries) => entries.iter().any(|(k, _)| k == key),
            MapRepr::Large(map) => map.contains_key(key),
        }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        match &self.0 {
            MapRepr::Small(entries) => entries.iter().find(|(k, _)| k == key).map(|(_, v)| v),
            MapRepr::Large(map) => map.get(key),
        }
    }

    pub fn iter(&self) -> MapIter<'_, K, V> {
        match &self.0 {
            MapRepr::Small(entries) => MapIter::Small(entries.iter()),
            MapRepr::Large(map) => MapIter::Large(map.iter()),
        }
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.iter().map(|(k, _)| k)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.iter().map(|(_, v)| v)
    }
}

impl<K, V> HashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        match &mut self.0 {
            MapRepr::Small(entries) => {
                for (k, v) in entries.iter_mut() {
                    if k == &key {
                        return Some(std::mem::replace(v, value));
                    }
                }
                entries.push((key, value));
                if entries.len() > SMALL_MAP_MAX {
                    let mut large = ImHashMap::new();
                    for (k, v) in entries.drain(..) {
                        large.insert(k, v);
                    }
                    self.0 = MapRepr::Large(large);
                }
                None
            }
            MapRepr::Large(map) => map.insert(key, value),
        }
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        match &mut self.0 {
            MapRepr::Small(entries) => {
                let idx = entries.iter().position(|(k, _)| k == key)?;
                Some(entries.swap_remove(idx).1)
            }
            MapRepr::Large(map) => map.remove(key),
        }
    }

    pub fn into_std(self) -> StdHashMap<K, V> {
        match self.0 {
            MapRepr::Small(entries) => StdHashMap::from_iter(entries),
            MapRepr::Large(map) => StdHashMap::from_iter(map.into_iter()),
        }
    }
}

impl<K, V> Default for HashMap<K, V>
where
    K: Eq + Hash,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> From<StdHashMap<K, V>> for HashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn from(items: StdHashMap<K, V>) -> Self {
        if items.len() <= SMALL_MAP_MAX {
            Self(MapRepr::Small(items.into_iter().collect()))
        } else {
            let mut map = ImHashMap::new();
            for (k, v) in items {
                map.insert(k, v);
            }
            Self(MapRepr::Large(map))
        }
    }
}

impl<K, V> From<ImHashMap<K, V>> for HashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn from(items: ImHashMap<K, V>) -> Self {
        if items.len() <= SMALL_MAP_MAX {
            Self(MapRepr::Small(items.into_iter().collect()))
        } else {
            Self(MapRepr::Large(items))
        }
    }
}

impl<K, V> FromIterator<(K, V)> for HashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut map = HashMap::new();
        for (k, v) in iter {
            map.insert(k, v);
        }
        map
    }
}

impl<K, V> IntoIterator for HashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_std().into_iter().collect::<Vec<_>>().into_iter()
    }
}

pub enum MapIter<'a, K, V> {
    Small(std::slice::Iter<'a, (K, V)>),
    Large(im::hashmap::Iter<'a, K, V>),
}

impl<'a, K, V> Iterator for MapIter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MapIter::Small(iter) => iter.next().map(|(k, v)| (k, v)),
            MapIter::Large(iter) => iter.next(),
        }
    }
}

impl<'a, K, V> IntoIterator for &'a HashMap<K, V>
where
    K: Eq + Hash,
{
    type Item = (&'a K, &'a V);
    type IntoIter = MapIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<K, V> Extend<(K, V)> for HashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        for (k, v) in iter {
            self.insert(k, v);
        }
    }
}

impl<K, V> PartialEq for HashMap<K, V>
where
    K: Eq + Hash,
    V: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (k, v) in self.iter() {
            if other.get(k) != Some(v) {
                return false;
            }
        }
        true
    }
}

impl<K, V> Eq for HashMap<K, V>
where
    K: Eq + Hash,
    V: Eq,
{
}

impl<K, V> Hash for HashMap<K, V>
where
    K: Eq + Hash,
    V: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut entries = Vec::with_capacity(self.len());
        for (k, v) in self.iter() {
            let mut entry_hasher = std::collections::hash_map::DefaultHasher::new();
            k.hash(&mut entry_hasher);
            v.hash(&mut entry_hasher);
            entries.push(entry_hasher.finish());
        }
        entries.sort_unstable();
        for hash in entries {
            hash.hash(state);
        }
    }
}

impl<K, V> fmt::Debug for HashMap<K, V>
where
    K: Eq + Hash + fmt::Debug,
    V: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}
