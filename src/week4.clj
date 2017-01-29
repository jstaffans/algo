(ns week4
  (:require [clojure.java.io :as io]))

(defn to-entry [ints]
  {(first ints) (rest ints)})

(defn vertex-with-edges [ints]
  (->> (clojure.string/split ints #"\s")
       (map #(Integer/valueOf %))
       to-entry))

(defn read-graph [filename]
  (with-open [rdr (io/reader filename)]
    (into {} (map vertex-with-edges (line-seq rdr)))))

(defn random-edge [graph]
  (let [[v1 neighbors] (->> graph seq shuffle first)]
    #{v1 (first (shuffle neighbors))}))

(defn all-edges [graph edge]
  (concat (get graph (first edge)) (get graph (second edge))))

(defn neighbors [graph edge]
  (-> (all-edges graph edge)
      set
      (clojure.set/difference (set edge))))

(defn update-neighbor [graph neighbor edge replacement]
  (let [verts-to-consider (get graph neighbor)
        replace-fn        (fn [v] (if (edge v) replacement v))]
    (assoc graph neighbor (map replace-fn verts-to-consider))))

(defn update-neighbors [graph edge replacement]
  (let [neighbors (neighbors graph edge)]
    (doall
     (reduce
      (fn [graph neighbor]
        (update-neighbor graph neighbor edge replacement))
      graph
      neighbors))))

(defn contract-edge [graph edge replacement]
  (let [new-edges (all-edges graph edge)
        outgoing-edges (remove edge new-edges)]
    (-> (apply dissoc (concat [graph] edge))
        (assoc replacement outgoing-edges))))

(defn contract [graph edge]
  (let [replacement (first edge)]
    (-> graph
        (contract-edge edge replacement)
        (update-neighbors edge replacement))))

(defn find-cut [graph]
  (loop [graph graph
         edge  (random-edge graph)]
    (if (= 2 (count (keys graph)))
      graph
      (recur (contract graph edge) (random-edge graph)))))

(comment
  (def graph (read-graph "resources/GraphTest2.txt"))

  (let [min-cut (atom Integer/MAX_VALUE)]
    (dotimes [i (Math/pow (count graph) 2)]
      (when (zero? (mod i 100))
        (println (str "iteration=" i ", min cut=" @min-cut)))
      (let [cut (-> (find-cut graph)
                    vals
                    first
                    count)]
        (swap! min-cut #(min % cut))))
    @min-cut)

  )
