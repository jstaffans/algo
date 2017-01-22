(ns week2
  (:require [util :refer [ints]]))

(defn merge-sort-and-count-inversions [coll]
  (if (= 1 (count coll))
    [coll 0]
    (let [[left right]      (split-at (/ (count coll) 2) coll)
          [sorted-left il]  (merge-sort-and-count-inversions left)
          [sorted-right ir] (merge-sort-and-count-inversions right)]
      (loop [acc        []
             ll         sorted-left
             rr         sorted-right
             inversions (+ il ir)]
        (cond
          (empty? ll)
          [(into acc rr) inversions]

          (empty? rr)
          [(into acc ll) inversions]

          :else
          (if (< (first ll) (first rr))
            (recur (conj acc (first ll)) (rest ll) rr inversions)
            (recur (conj acc (first rr)) ll (rest rr) (+ inversions (count ll)))))))))

;; (println (second (merge-sort-and-count-inversions (ints "resources/IntegerArray.txt"))))

;; (time (merge-sort (ints "resources/IntegerArray.txt")))
