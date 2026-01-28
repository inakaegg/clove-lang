use clove_core::ast::{HashMap, Key, Value};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

const SMALL_KEYS: usize = 16;
const SMALL_ITERS: usize = 100_000;
const MEDIUM_KEYS: usize = 1_000;
const MEDIUM_ITERS: usize = 10_000;
const WORLD_ITERS: usize = 10_000;

fn make_keys(size: usize, prefix: &str) -> Vec<Key> {
    (0..size)
        .map(|i| Key::Keyword(format!("{prefix}{i}")))
        .collect()
}

fn make_map(keys: &[Key]) -> HashMap<Key, Value> {
    let mut map = HashMap::new();
    for (idx, key) in keys.iter().enumerate() {
        map.insert(key.clone(), Value::Int(idx as i64));
    }
    map
}

fn bench_assoc_small(c: &mut Criterion) {
    let keys = make_keys(SMALL_KEYS, "k");
    let base = make_map(&keys);
    c.bench_function("assoc_small_16x100k", |b| {
        b.iter(|| {
            let mut map = base.clone();
            for i in 0..SMALL_ITERS {
                let mut next = map.clone();
                let key = keys[i % keys.len()].clone();
                next.insert(key, Value::Int(i as i64));
                map = next;
            }
            black_box(map);
        })
    });
}

fn bench_assoc_medium(c: &mut Criterion) {
    let keys = make_keys(MEDIUM_KEYS, "m");
    let base = make_map(&keys);
    c.bench_function("assoc_medium_1k_10k", |b| {
        b.iter(|| {
            let mut map = base.clone();
            for i in 0..MEDIUM_ITERS {
                let mut next = map.clone();
                let key = keys[i % keys.len()].clone();
                next.insert(key, Value::Int(i as i64));
                map = next;
            }
            black_box(map);
        })
    });
}

#[derive(Clone)]
struct WorldKeys {
    player: Key,
    frame: Key,
    pos: Key,
    hp: Key,
    x: Key,
    y: Key,
}

fn world_keys() -> WorldKeys {
    WorldKeys {
        player: Key::Keyword("player".into()),
        frame: Key::Keyword("frame".into()),
        pos: Key::Keyword("pos".into()),
        hp: Key::Keyword("hp".into()),
        x: Key::Keyword("x".into()),
        y: Key::Keyword("y".into()),
    }
}

fn map_or_empty(value: Option<&Value>) -> HashMap<Key, Value> {
    match value {
        Some(Value::Map(map)) => map.clone(),
        _ => HashMap::new(),
    }
}

fn make_world(keys: &WorldKeys) -> HashMap<Key, Value> {
    let mut pos = HashMap::new();
    pos.insert(keys.x.clone(), Value::Int(0));
    pos.insert(keys.y.clone(), Value::Int(0));

    let mut player = HashMap::new();
    player.insert(keys.pos.clone(), Value::Map(pos));
    player.insert(keys.hp.clone(), Value::Int(100));

    let mut world = HashMap::new();
    world.insert(keys.player.clone(), Value::Map(player));
    world.insert(keys.frame.clone(), Value::Int(0));
    world
}

fn update_world(map: &HashMap<Key, Value>, keys: &WorldKeys, frame: i64) -> HashMap<Key, Value> {
    let player = map_or_empty(map.get(&keys.player));
    let pos = map_or_empty(player.get(&keys.pos));

    let mut next_pos = pos.clone();
    next_pos.insert(keys.x.clone(), Value::Int(frame));
    next_pos.insert(keys.y.clone(), Value::Int(frame + 1));

    let mut next_player = player.clone();
    next_player.insert(keys.pos.clone(), Value::Map(next_pos));
    next_player.insert(keys.hp.clone(), Value::Int(100));

    let mut next_world = map.clone();
    next_world.insert(keys.player.clone(), Value::Map(next_player));
    next_world.insert(keys.frame.clone(), Value::Int(frame));
    next_world
}

fn bench_world_update(c: &mut Criterion) {
    let keys = world_keys();
    let base = make_world(&keys);
    c.bench_function("world_nested_update_10k", |b| {
        b.iter(|| {
            let mut map = base.clone();
            for i in 0..WORLD_ITERS {
                map = update_world(&map, &keys, i as i64);
            }
            black_box(map);
        })
    });
}

criterion_group!(
    map_update_benches,
    bench_assoc_small,
    bench_assoc_medium,
    bench_world_update
);
criterion_main!(map_update_benches);
