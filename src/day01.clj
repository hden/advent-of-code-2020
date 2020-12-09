(ns day1
  (:require [util]
            [clojure.math.combinatorics :refer [combinations]]))

(def ^:const target 2020)

(def inputs
  (util/into (sorted-set)
             (map #(Integer/parseInt %))
             "inputs/day01.txt"))

;; Part I - 997899
(defn part-1 [& args]
  (println (reduce (fn [results x]
                     (let [y (- target x)]
                       (if (contains? inputs y)
                         (conj results (* x y))
                         results)))
                   #{}
                   inputs)))

;; Part II
(defn part-2 [& args]
  (println (reduce (fn [results [x y z]]
                     (if (= target (+ x y z))
                       (conj results (* x y z))
                       results))
                   #{}
                   (combinations inputs 3))))
