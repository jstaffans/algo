(ns algo.core)

(def ^:const base (BigInteger. "10"))

(defn ksplit [x n2]
  [(bigint (/ x (.pow base n2)))
   (bigint (mod x (.pow base n2)))])

(defn kmult
  [x y]
  (if (or
       (= (count (str x)) 1)
       (= (count (str y)) 1))
    ;; base case
    (* x y)
    (let [n        (max (count (str x)) (count (str y)))
          n2       (int (/ n 2))
          [a b]    (ksplit x n2)
          [c d]    (ksplit y n2)
          ac       (kmult a c)
          bd       (kmult b d)
          part_sum (kmult (+ a b) (+ c d))]

      (+
        (* (.pow base (* n2 2)) ac)
        (* (.pow base n2) (- part_sum ac bd))
        bd))))

(kmult 1234 5678)

#_(println (kmult 3141592653589793238462643383279502884197169399375105820974944592 2718281828459045235360287471352662497757247093699959574966967627))
