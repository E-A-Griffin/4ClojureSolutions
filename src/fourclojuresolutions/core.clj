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

(defn set-clone?
  "Takes in a collection coll and returns true if
   the coll is a set and false otherwise."
  [coll]
  (if (= (count coll) 0)
    (= (conj coll 0) (conj (conj coll 0) 0))
    (= coll (conj coll (first coll)))))

(defn get-coll-type [coll]
  (let [first-token [(gensym) (gensym)]
        second-token [(gensym) (gensym)]
        conjd-coll (conj (conj coll first-token) second-token)]
    ;; Check if first-token is inserted twice
    (if (= (count conjd-coll) (count (conj conjd-coll first-token)))
      ;; Check if vector of two elements is treated like a key-value pair
      ;; (hence implying coll is a map)
      (if (= (count (conj coll [1 1])) (count (conj (conj coll [1 1]) [1 0])))
        :map :set)
      (if (= (last conjd-coll) second-token) :vector :list))))

(defn comp-clone
  [& fns]
  (loop [rem-fns (butlast fns)
         compd-fns (fn [& args] (apply (last fns) args))]
    (if (empty? rem-fns)
      compd-fns
      (recur (butlast rem-fns) (fn [& args] ((last rem-fns) (apply compd-fns args)))))))

(defn juxt-clone
  [& fns]
  (fn [& args]
    (map #(apply % args) fns)))

(defn reductions-clone
  ([f coll]
   (lazy-seq
    (if-let [sec (second coll)]
      (cons (first coll)
            (reductions-clone f (cons (f (first coll) sec) (drop 2 coll))))
      coll)))
  ([f init coll]
   (lazy-seq
    (reductions-clone f (cons init coll)))))

(defn zipmap-clone
  [ks vs]
  (reduce conj (map #(array-map %1 %2) ks vs)))

(defn iterate-clone
  ([f init]
   (lazy-seq
    (cons init (iterate-clone f (f init))))))

(defn merge-two-with [f m1 m2]
  (conj m1 (into {} (for [[k2 v2] m2]
                      (if-let [[k1 v1] (find m1 k2)]
                        [k1 (f v1 v2)]
                        [k2 v2])))))

(defn merge-with-clone [f m1 & rest-ms]
  (reduce (partial merge-two-with f) m1 rest-ms))

(defn nil-key?
  "Given a key and map, returns true iff the map contains an entry with that
   key and its value is nil"
  [k m]
  (and
   (contains? m k)
   (nil? (k m))))
