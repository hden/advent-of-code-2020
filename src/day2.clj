(ns day2
  (:refer-clojure :exclude [nth])
  (:require [util]
            [clojure.string :as string]
            [clojure.math.combinatorics :refer [combinations]]))

(def inputs
  (util/into []
             (map (fn [s]
                    (let [[_ least most word password] (first (re-seq #"^(\d+)-(\d+) (\w): (\w+)" s))]
                      {:least (Integer/parseInt least)
                       :most (Integer/parseInt most)
                       :word word
                       :password password
                       :stats (group-by identity (string/split password #""))})))
             "inputs/day2.txt"))

;; Part I
(defn part-1 [& args]
  (let [invalid-tuples (filter (fn [{:keys [least most word stats]}]
                                 (let [n (count (get stats word []))]
                                  (>= most n least)))
                               inputs)]
    (println (count invalid-tuples))))

;; Part II
(defn xor [x y]
  (and (or x y)
       (not (and x y))))

(defn nth [coll x]
  {:pre [(> x 0)]}
  (get coll (- x 1)))

(defn part-2 [& args]
  (let [valid-tuples (filter (fn [{:keys [least most word password]}]
                               (let [coll (into [] (string/split password #""))]
                                 (xor (= word (nth coll least))
                                      (= word (nth coll most)))))
                             inputs)]
    (println (count valid-tuples))))
