(ns week2
  (:require [clojure.java.io :as io]))

(defn ints* [filename]
  (with-open [rdr (io/reader filename)]
    (into [] (map #(Integer/valueOf %) (line-seq rdr)))))

(def ints
  (memoize ints*))

(defn merge-sort [coll]
  (if (= 1 (count coll))
    coll
    (let [[left right] (split-at (/ (count coll) 2) coll)]
      (loop [acc []
             ll (merge-sort left)
             rr (merge-sort right)]
        (cond
          (empty? ll)
          (into acc rr)

          (empty? rr)
          (into acc ll)

          :else
          (if (< (first ll) (first rr))
            (recur (conj acc (first ll)) (rest ll) rr)
            (recur (conj acc (first rr)) ll (rest rr))))))))

(merge-sort [4 1 99 6 23 8])

;; (take 10 (merge-sort (take 5000 (ints "resources/IntegerArray.txt"))))

;; (time (merge-sort (ints "resources/IntegerArray.txt")))
