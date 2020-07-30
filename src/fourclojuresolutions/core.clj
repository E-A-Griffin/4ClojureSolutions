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
