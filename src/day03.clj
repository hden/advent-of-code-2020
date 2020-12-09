(ns day3
  (:refer-clojure :exclude [nth])
  (:require [util]
            [clojure.string :as string]
            [clojure.math.combinatorics :refer [combinations]]))

(def inputs
  (util/into []
             (map (fn [s]
                    (let [row (string/split s #"")]
                      (into [] (flatten (repeat 100 row))))))
             "inputs/day03.txt"))

;; Part I
(defn part-1 [& args]
  (println (reduce (fn [result y]
                     (let [x (* 3 y)
                           value (get-in inputs [y x])]
                       (println {:x x :y y :value value})
                       (if (= value "#")
                         (+ 1 result)
                         result)))
                   0
                   (range (count inputs)))))

;; Part II
(defn part-2 [& args]
  (println
    (reduce
      (fn [result [right down]]
        (* result (reduce (fn [result y]
                            (let [x (* right y)
                                  value (get-in inputs [y x])]
                              (if (= value "#")
                                (+ 1 result)
                                result)))
                          0
                          (range 0 (count inputs) down))))
      1
      [[1 1]
       [3 1]
       [5 1]
       [7 1]
       [1 2]])))
