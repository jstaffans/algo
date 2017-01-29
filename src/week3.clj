(ns week3
  (:require [util :refer [ints]]))

(defn pivot-first [coll]
  [(first coll) (rest coll)])

(defn swap [v i1 i2]
  (assoc v i2 (v i1) i1 (v i2)))

(defn pivot-last [coll]
  (let [pivoted-coll (swap coll 0 (- (count coll) 1))]
    (pivot-first pivoted-coll)))

(defn pivot-middle [coll]
  (let [m (int (/ (count coll) 2))
        pivoted-coll (swap coll 0 m)]
    (pivot-first pivoted-coll)))

(defn pivot-median [coll]
  (if (< (count coll) 3)
    (pivot-first coll)
    (let [pivot-fn ((comp second second)
                    (sort
                     {(first coll)                  pivot-first
                      (nth coll (/ (count coll) 2)) pivot-middle
                      (last coll)                   pivot-last}))]
      (pivot-fn coll))))

(defn qsort [pivot-fn coll]
  (if (< (count coll) 2)
    [coll 0]
    (let [[pivot to-partition] (pivot-fn coll)]
      (loop [to-partition to-partition
             left         []
             right        []]
        (if (empty? to-partition)
          (let [[sorted-left ops-left]   (qsort pivot-fn left)
                [sorted-right ops-right] (qsort pivot-fn right)]
            [(concat sorted-left [pivot] sorted-right)
             (+ ops-right ops-left (count coll) -1)])
          (let [elem (first to-partition)]
            (if (< elem pivot)
              (recur (rest to-partition) (conj left elem) right)
              (recur (rest to-partition) left (conj right elem)))))))))

(qsort pivot-last [3 9 8 4 6 10 2 5 7 1])

;; note: number of comparisons is lower than test cases --> do imperative version in Python

;; (second (qsort pivot-median (ints "resources/QuickSort.txt")))
