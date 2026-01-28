(def reps-fast 3000000)
(def reps-mid 400000)
(def reps-slow 200)

;; map / filter / reduce
(def sum-map (reduce + (map inc (range reps-fast))))
(def sum-filter (reduce + (filter even? (range reps-fast))))

;; hash-map / conj / nth / rest / interleave / count / empty?
(def sum-collections
  (reduce
    +
    (map
      (fn [x]
        (let [m (hash-map :a x :b x)
              v (conj [x] x)
              n0 (nth v 0)
              r (rest v)
              i (interleave v r)
              c (count i)]
          (if (empty? m) 0 (+ c n0))))
      (range reps-mid))))

;; remove / drop-while / keep / keep-indexed
(def rm (remove odd? (range reps-mid)))
(def dw (drop-while #(< % 10) rm))
(def kp (keep (fn [x] (when (even? x) x)) dw))
(def kpi (keep-indexed (fn [i x] (when (even? i) x)) kp))
(def sum-keep (reduce + 0 kpi))

;; sort / sort-by / some
(def small (vec (range 20000)))
(def sum-sort
  (reduce
    +
    (map
      (fn [_]
        (let [sorted (sort small)
              sortby (sort-by (fn [x] (- 0 x)) small)
              somev (some (fn [x] (when (even? x) x)) small)
              some-flag (if somev 1 0)]
          (+ (count sorted) (count sortby) some-flag)))
      (range reps-slow))))

;; split / join
(def text "a,b,c,d,e,f,g,h,i,j")
(def sum-text
  (reduce
    +
    (map
      (fn [_]
        (let [parts (clojure.string/split text #",")
              joined (clojure.string/join "|" parts)]
          (+ (count joined) (count parts))))
      (range reps-mid))))

(def total (+ sum-map sum-filter sum-collections sum-keep sum-sort sum-text))
(println total)
