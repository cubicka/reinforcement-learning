(ns util.core
  (:require [clojure.core.matrix.random :refer [sample-rand-int sample-binomial]]))

(defn pick-random [options]
  (nth options (->> options count (sample-rand-int 1) first)))

(comment
  (pick-random [1 2 3 4 5])
  )

(defn is-happen? [prob]
  (= 1 (->> prob (sample-binomial 1) first)))

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs)
      (reduce #(rec-merge %1 %2) v vs)
      (last vs))))
