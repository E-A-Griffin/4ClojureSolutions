(ns fourclojuresolutions.core)

(defn nth-clone [coll n]
  (if (zero? n)
    (first coll)
    (recur (rest coll) (dec n))))

(defn count-clone [coll]
  (loop [remaining coll acc 0]
    (if (empty? remaining)
      acc
      (recur (rest remaining) (inc acc)))))

(defn rseq-clone [coll]
  (into '() coll))

(defn sum [coll] (reduce + sum))

(defn odd-elements [coll] (filter odd? coll))

(defn fibonacci
  ([] fibonacci [])
  ([coll] (if (empty? coll)
           [1 1]
           (conj (vec coll) (+ (last coll) (last (butlast coll)))))))

(defn n-fibonacci
  "Take n first elements of fibonacci sequence."
  [n]
  (#(last (take % (iterate fibonacci []))) n))

(defn palindrome? [coll] (let [rev-fn (if (reversible? coll) rseq reverse)]
                           (= (rev-fn coll) (seq coll))))

;; "deep" functions work like their "shallow" counterparts but will go to the innermost
;; nested data structure to retrieve the scalar or information necessary

(defn deep-empty? [coll] (if (= (count coll) 1)
                           (if (coll? (first coll)) (deep-empty? (first coll)) false)
                           (empty? coll)))

(defn deep-first [coll]
  (if (coll? (first coll)) (deep-first (first coll)) (first coll)))

;; Probably not efficient, needs reworking
(defn deep-rest [coll] (if (and (coll? (first coll)) (not-empty (first coll)))
                             (let [first-pass (cons (deep-rest (first coll)) (rest coll))]
                               (if (deep-empty? (first first-pass))
                                 (rest first-pass)
                                 first-pass))
                         (rest coll)))

(defn flatten-clone [coll]
  (loop [remaining coll
         ret []]
    (if (empty? remaining)
      (lazy-seq ret)
      (recur (deep-rest remaining) (conj ret (deep-first remaining))))))

(defn filter-upper
  "Takes in a string and returns only the upper case letters of
   the string."
  [^String s] (transduce (filter #(and (>= (long %) (long \A)) (<= (long %) (long \Z)))) str s))

(defn max-clone [& args] (reduce #(if (> %1 %2) %1 %2) args))

(defn interleave-clone [coll1 coll2] (reduce concat (map vector coll1 coll2)))

(defn interpose-clone [value coll]
  (butlast (interleave coll (repeat (count coll) value))))

(defn drop-nth "Drop every nth item in coll."
  [coll n] (apply concat (map (partial take (dec n)) (partition-all n coll))))

(defn factorial [n] (loop [ret (if (zero? n) 1 n)
                           cur (dec n)]
                      (if (pos? cur)
                        (recur (* ret cur) (dec cur))
                        ret)))

(defn rev-interleave
  "Reverses the interleave process into x number of subsequences."
  [coll n]
  (for [i (range n)] (take-nth n (drop i coll))))

(defn rotate-seq
  "Rotates a sequence in either direction (positive->left, negative->right).
  Returns a lazy-sequence.
  Ex: (rotate-seq 2 [1 2 3 4 5]) => '(3 4 5 1 2)
      (rotate-seq -4 '(:a :b :c)) => '(:c :a :b)"
  [n coll]
  (apply concat (rseq (split-at (mod n (count coll)) coll))))
