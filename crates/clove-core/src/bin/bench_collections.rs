use clove_core::cow::{HashMap as CowHashMap, Vector as CowVector};
use im::{HashMap as ImHashMap, Vector as ImVector};
use std::collections::{HashMap as StdHashMap, VecDeque};
use std::env;
use std::hint::black_box;
use std::process;
use std::time::{Duration, Instant};

#[cfg(feature = "bench-rpds")]
use rpds::{HashTrieMap as RpdsHashMap, Vector as RpdsVector};

#[derive(Debug, Clone, Copy)]
struct Config {
    size: usize,
    iters: usize,
    get_ops: usize,
}

impl Config {
    fn from_args() -> Self {
        let mut size = 100_000;
        let mut iters = 5;
        let mut get_ops = 1_000_000;
        let mut args = env::args().skip(1);
        while let Some(arg) = args.next() {
            match arg.as_str() {
                "--size" => size = parse_usize(&mut args, "--size"),
                "--iters" => iters = parse_usize(&mut args, "--iters"),
                "--get-ops" => get_ops = parse_usize(&mut args, "--get-ops"),
                "--help" | "-h" => {
                    print_usage();
                    process::exit(0);
                }
                _ => {
                    eprintln!("unknown argument: {arg}");
                    print_usage();
                    process::exit(1);
                }
            }
        }
        Self {
            size,
            iters,
            get_ops,
        }
    }
}

fn parse_usize(args: &mut impl Iterator<Item = String>, flag: &str) -> usize {
    match args.next() {
        Some(value) => value.parse::<usize>().unwrap_or_else(|_| {
            eprintln!("invalid value for {flag}: {value}");
            process::exit(1);
        }),
        None => {
            eprintln!("missing value for {flag}");
            process::exit(1);
        }
    }
}

fn print_usage() {
    eprintln!(
        "usage: bench_collections [--size N] [--iters N] [--get-ops N]\n\
default: --size 100000 --iters 5 --get-ops 1000000"
    );
}

fn run_bench(label: &str, ops: u64, mut f: impl FnMut()) {
    let start = Instant::now();
    f();
    let elapsed = start.elapsed();
    report(label, elapsed, ops);
}

fn report(label: &str, elapsed: Duration, ops: u64) {
    let total_ms = elapsed.as_secs_f64() * 1000.0;
    let per_op_ns = if ops == 0 {
        0.0
    } else {
        elapsed.as_secs_f64() * 1_000_000_000.0 / ops as f64
    };
    println!("{label:<26} {total_ms:>10.3} ms  {per_op_ns:>10.1} ns/op");
}

fn main() {
    let config = Config::from_args();
    println!(
        "config: size={} iters={} get_ops={}",
        config.size, config.iters, config.get_ops
    );
    println!();

    bench_vector_build(config);
    bench_vector_update(config);
    bench_map_build(config);
    bench_map_update(config);
    bench_map_get(config);

    #[cfg(feature = "bench-rpds")]
    println!("note: rpds enabled");
    #[cfg(not(feature = "bench-rpds"))]
    println!("note: rpds disabled (enable with --features bench-rpds)");
}

fn bench_vector_build(config: Config) {
    println!("vector build");
    run_bench("vec", (config.size * config.iters) as u64, || {
        for _ in 0..config.iters {
            let mut v = Vec::with_capacity(config.size);
            for i in 0..config.size {
                v.push(i as i64);
            }
            black_box(v);
        }
    });
    run_bench("vec_deque", (config.size * config.iters) as u64, || {
        for _ in 0..config.iters {
            let mut v = VecDeque::with_capacity(config.size);
            for i in 0..config.size {
                v.push_back(i as i64);
            }
            black_box(v);
        }
    });
    run_bench("cow_vector", (config.size * config.iters) as u64, || {
        for _ in 0..config.iters {
            let mut v = CowVector::new();
            for i in 0..config.size {
                v.push_back(i as i64);
            }
            black_box(v);
        }
    });
    run_bench("im_vector", (config.size * config.iters) as u64, || {
        for _ in 0..config.iters {
            let mut v = ImVector::new();
            for i in 0..config.size {
                v.push_back(i as i64);
            }
            black_box(v);
        }
    });
    #[cfg(feature = "bench-rpds")]
    run_bench("rpds_vector", (config.size * config.iters) as u64, || {
        for _ in 0..config.iters {
            let mut v = RpdsVector::new();
            for i in 0..config.size {
                v.push_back_mut(i as i64);
            }
            black_box(v);
        }
    });
    println!();
}

fn bench_vector_update(config: Config) {
    println!("vector clone+update");
    let base_vec: Vec<i64> = (0..config.size as i64).collect();
    run_bench("vec", config.iters as u64, || {
        for i in 0..config.iters {
            let mut v = base_vec.clone();
            let idx = i % config.size;
            v[idx] += 1;
            black_box(v);
        }
    });
    let base_deque: VecDeque<i64> = VecDeque::from_iter(0..config.size as i64);
    run_bench("vec_deque", config.iters as u64, || {
        for i in 0..config.iters {
            let mut v = base_deque.clone();
            let idx = i % config.size;
            if let Some(slot) = v.get_mut(idx) {
                *slot += 1;
            }
            black_box(v);
        }
    });
    let base_cow: CowVector<i64> = CowVector::from_iter(0..config.size as i64);
    run_bench("cow_vector", config.iters as u64, || {
        for i in 0..config.iters {
            let mut v = base_cow.clone();
            let idx = i % config.size;
            if let Some(slot) = v.get_mut(idx) {
                *slot += 1;
            }
            black_box(v);
        }
    });
    let base_im: ImVector<i64> = ImVector::from_iter(0..config.size as i64);
    run_bench("im_vector", config.iters as u64, || {
        for i in 0..config.iters {
            let mut v = base_im.clone();
            let idx = i % config.size;
            let new_value = v[idx] + 1;
            v.set(idx, new_value);
            black_box(v);
        }
    });
    #[cfg(feature = "bench-rpds")]
    {
        let base_rpds: RpdsVector<i64> = RpdsVector::from_iter(0..config.size as i64);
        run_bench("rpds_vector", config.iters as u64, || {
            for i in 0..config.iters {
                let mut v = base_rpds.clone();
                let idx = i % config.size;
                if let Some(value) = v.get(idx) {
                    v.set_mut(idx, value + 1);
                }
                black_box(v);
            }
        });
    }
    println!();
}

fn bench_map_build(config: Config) {
    println!("map build");
    run_bench("std_hashmap", (config.size * config.iters) as u64, || {
        for _ in 0..config.iters {
            let mut map = StdHashMap::with_capacity(config.size);
            for i in 0..config.size {
                map.insert(i as i64, i as i64);
            }
            black_box(map);
        }
    });
    run_bench("cow_hashmap", (config.size * config.iters) as u64, || {
        for _ in 0..config.iters {
            let mut map = CowHashMap::new();
            map.reserve(config.size);
            for i in 0..config.size {
                map.insert(i as i64, i as i64);
            }
            black_box(map);
        }
    });
    run_bench("im_hashmap", (config.size * config.iters) as u64, || {
        for _ in 0..config.iters {
            let mut map = ImHashMap::new();
            for i in 0..config.size {
                map.insert(i as i64, i as i64);
            }
            black_box(map);
        }
    });
    #[cfg(feature = "bench-rpds")]
    run_bench("rpds_hashmap", (config.size * config.iters) as u64, || {
        for _ in 0..config.iters {
            let mut map = RpdsHashMap::new();
            for i in 0..config.size {
                map.insert_mut(i as i64, i as i64);
            }
            black_box(map);
        }
    });
    println!();
}

fn bench_map_update(config: Config) {
    println!("map clone+update");
    let base_std: StdHashMap<i64, i64> = (0..config.size as i64).map(|i| (i, i)).collect();
    run_bench("std_hashmap", config.iters as u64, || {
        for i in 0..config.iters {
            let mut map = base_std.clone();
            map.insert((config.size + i) as i64, i as i64);
            black_box(map);
        }
    });
    let base_cow: CowHashMap<i64, i64> =
        CowHashMap::from_iter((0..config.size as i64).map(|i| (i, i)));
    run_bench("cow_hashmap", config.iters as u64, || {
        for i in 0..config.iters {
            let mut map = base_cow.clone();
            map.insert((config.size + i) as i64, i as i64);
            black_box(map);
        }
    });
    let base_im: ImHashMap<i64, i64> =
        ImHashMap::from_iter((0..config.size as i64).map(|i| (i, i)));
    run_bench("im_hashmap", config.iters as u64, || {
        for i in 0..config.iters {
            let mut map = base_im.clone();
            map.insert((config.size + i) as i64, i as i64);
            black_box(map);
        }
    });
    #[cfg(feature = "bench-rpds")]
    {
        let base_rpds: RpdsHashMap<i64, i64> =
            RpdsHashMap::from_iter((0..config.size as i64).map(|i| (i, i)));
        run_bench("rpds_hashmap", config.iters as u64, || {
            for i in 0..config.iters {
                let mut map = base_rpds.clone();
                map.insert_mut((config.size + i) as i64, i as i64);
                black_box(map);
            }
        });
    }
    println!();
}

fn bench_map_get(config: Config) {
    println!("map get");
    let base_std: StdHashMap<i64, i64> = (0..config.size as i64).map(|i| (i, i)).collect();
    run_bench("std_hashmap", config.get_ops as u64, || {
        let mut acc = 0i64;
        for i in 0..config.get_ops {
            let key = (i % config.size) as i64;
            if let Some(value) = base_std.get(&key) {
                acc += *value;
            }
        }
        black_box(acc);
    });
    let base_cow: CowHashMap<i64, i64> =
        CowHashMap::from_iter((0..config.size as i64).map(|i| (i, i)));
    run_bench("cow_hashmap", config.get_ops as u64, || {
        let mut acc = 0i64;
        for i in 0..config.get_ops {
            let key = (i % config.size) as i64;
            if let Some(value) = base_cow.get(&key) {
                acc += *value;
            }
        }
        black_box(acc);
    });
    let base_im: ImHashMap<i64, i64> =
        ImHashMap::from_iter((0..config.size as i64).map(|i| (i, i)));
    run_bench("im_hashmap", config.get_ops as u64, || {
        let mut acc = 0i64;
        for i in 0..config.get_ops {
            let key = (i % config.size) as i64;
            if let Some(value) = base_im.get(&key) {
                acc += *value;
            }
        }
        black_box(acc);
    });
    #[cfg(feature = "bench-rpds")]
    {
        let base_rpds: RpdsHashMap<i64, i64> =
            RpdsHashMap::from_iter((0..config.size as i64).map(|i| (i, i)));
        run_bench("rpds_hashmap", config.get_ops as u64, || {
            let mut acc = 0i64;
            for i in 0..config.get_ops {
                let key = (i % config.size) as i64;
                if let Some(value) = base_rpds.get(&key) {
                    acc += *value;
                }
            }
            black_box(acc);
        });
    }
    println!();
}
