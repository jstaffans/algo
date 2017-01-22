(ns util
  (:require [clojure.java.io :as io]))

(defn- ints* [filename]
  (with-open [rdr (io/reader filename)]
    (into [] (map #(Integer/valueOf %) (line-seq rdr)))))

(def ints
  (memoize ints*))
