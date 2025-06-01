; exercises from https://4clojure.oxal.org/

; 19 last element
#(first (reverse %))

; 20 penultimate element
#(second (reverse %))

; 21 n-th element
(fn [xs n]
  (if (= 0 n)
    (first xs)
    (recur (rest xs) (- n 1))))

; 22 count
#(reduce + 0 (map (fn [_] 1) %))

; 23 reverse
#(into (list) (vec %))

; 24 sum
#(reduce + %)

; 25 find odd numbers
(partial filter odd?)

; 26 Fibonacci sequence
(fn [n]
  (case n
    0  '()
    1  '(1)
    (as-> '(1 1) $
      (iterate
       #(cons (+ (first %) (second %))
              %)
       $)
      (nth $ (- n 2))
      (reverse $))))

; 27 is palindrome
#(= (seq %) (reverse %))

; 28 flatten
(fn f [arg]
  (cond
    (string? arg)        (seq arg)
    (not (seqable? arg)) (list arg)
    :else (mapcat f arg)))

; 29 get the caps
(fn [s]
  (->> s
    (remove
     #(= (clojure.string/lower-case %)
         %))
    (apply str)))

; 30 compress sequence
(fn [arg]
  (if (empty? arg)
    '()
    (cons
     (first arg)
     (->> arg
       (#(map vector % (rest %)))
       (filter #(not (= (first %) (second %))))
       (map second)))))

; 31 pack sequence
(fn [arg]
  (loop [data (seq arg)
         res  '()]
    (if (empty? data)
      (reverse res)
      (let [a     (split-with #(= % (first data)) data)
            block (first a)
            tail  (second a)]
        (recur tail
               (cons block res))))))

; 32 duplicate elements
(fn [arg]
  (loop [data (seq arg)
         res  '()]
    (if (empty? data)
      (reverse res)
      (let [x (first data)]
        (recur (rest data)
               (cons x (cons x res)))))))

; 33 replicate elements
(fn [arg n]
  (loop [data (seq arg)
         res  '()]
    (if (empty? data)
      (reverse res)
      (let [x (first data)]
        (recur (rest data)
               (concat (repeat n x) res))))))

; 34 range
(fn [start stop]
  (take (- stop start) (iterate inc start)))

; 38 maximum
(fn [arg]
  (loop [xs  (rest arg)
         res (first x)]
    (if (empty? xs)
      res
      (let [[x & rest] xs
            best       (if (> x res) x res)]
        (recur rest best)))))

; 39 interleave
#(apply mapcat vector %&)

; 40 interpose
(fn [x coll]
  (if (empty? coll)
    coll
    (cons
      (first coll)
      (mapcat vector (repeat x) (rest coll)))))

; 41 drop every n-th
(fn [coll n]
  (->> coll
    (partition (- n 1) n [])
    (apply concat)))

; 42 factorial
(fn [n]
  (->> (range)
    rest
    (reductions *)
    (take n)
    last))

; 43 reverse interleave
(fn [coll n]
  (as-> coll $
    (partition n n [] $)
    (map vec $)
    (map (fn [k]
           (->> $
             (take-while #(< k (count %)))
             (map #(nth % k))))
         (range n))))
(fn [coll n]
  (map
   (fn [k]
     (->> coll
       (drop k)
       (take-nth n)))
   (range n)))

; 44 rotate
(fn [n coll]
  (let [v   (vec coll)
        k   (mod n (count v))]
    (concat (subvec v k)
            (subvec v 0 k))))

; 46 flip arguments
(fn [f] #(apply f (reverse %&)))
