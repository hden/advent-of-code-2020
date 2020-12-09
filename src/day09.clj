(ns day09
  (:require [util]
            [clojure.math.combinatorics :refer [combinations]]))

(def inputs
  (util/into []
             (map #(BigInteger. %))
             "inputs/day09.txt"))

(def ^:const n 25)

(defn initial-state [coll]
  (let [preamble (subvec coll 0 n)
        data (subvec coll n)]
    {:preamble preamble
     :data data}))

(defn target-number []
  (let [{:keys [preamble data]} (initial-state inputs)]
    (reduce (fn [preamble x]
              (let [valid-numbers (into #{}
                                        (map #(apply + %))
                                        (combinations preamble 2))]
                (if (contains? valid-numbers x)
                  (conj (subvec preamble 1) x)
                  (reduced x))))
            preamble
            data)))

(defn part-1 [& args]
  (println (target-number)))

(defn part-2 [& args]
  (let [target (target-number)
        [subvec] (into []
                       (comp (filter (fn [[from to]]
                                       (>= (- to from)
                                           2)))
                             (map (fn [[from to]]
                                    (subvec inputs from to)))
                             (filter (fn [coll]
                                       (= target (reduce + coll)))))
                       (combinations (range (inc (count inputs)))
                                     2))]
    (println (+ (apply min subvec)
                (apply max subvec)))))
