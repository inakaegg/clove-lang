fn main() {
    let reps_fast: i64 = 3_000_000;
    let reps_mid: i64 = 400_000;
    let reps_slow: i64 = 200;

    // map / filter / reduce
    let mut sum_map: i64 = 0;
    let mut sum_filter: i64 = 0;
    for i in 0..reps_fast {
        sum_map += i + 1;
        if i % 2 == 0 {
            sum_filter += i;
        }
    }

    // hash-map / conj / nth / rest / interleave / count / empty?
    let mut sum_collections: i64 = 0;
    for x in 0..reps_mid {
        let n0 = x;
        let c = 2;
        sum_collections += c + n0;
    }

    // remove / drop-while / keep / keep-indexed
    let mut sum_keep: i64 = 0;
    let mut idx: i64 = 0;
    for i in 0..reps_mid {
        if i % 2 == 0 && i >= 10 {
            if idx % 2 == 0 {
                sum_keep += i;
            }
            idx += 1;
        }
    }

    // sort / sort-by / some
    let small: Vec<i64> = (0..20_000).collect();
    let mut sum_sort: i64 = 0;
    for _ in 0..reps_slow {
        let mut sorted = small.clone();
        sorted.sort();
        let mut sortby = small.clone();
        sortby.sort_by_key(|x| -(*x));
        let some_flag = if small.iter().any(|x| x % 2 == 0) { 1 } else { 0 };
        sum_sort += (sorted.len() + sortby.len()) as i64 + some_flag as i64;
    }

    // split / join
    let text = "a,b,c,d,e,f,g,h,i,j";
    let mut sum_text: i64 = 0;
    for _ in 0..reps_mid {
        let parts: Vec<&str> = text.split(',').collect();
        let joined = parts.join("|");
        sum_text += (joined.len() + parts.len()) as i64;
    }

    let total = sum_map + sum_filter + sum_collections + sum_keep + sum_sort + sum_text;
    println!("{}", total);
}
