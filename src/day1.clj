(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math.combinatorics :refer [combinations]]))

(def ^:const target 2020)

(def inputs
  (->> (slurp "inputs/day1.txt")
       (string/split-lines)
       (map #(Integer/parseInt %))
       (apply sorted-set)))

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
