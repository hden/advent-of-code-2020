(ns day05
  (:refer-clojure :exclude [set])
  (:require [util]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [me.tonsky.persistent-sorted-set :as set]))

(def rows (apply set/sorted-set (range 128)))
(def cols (apply set/sorted-set (range 8)))

(defn -lower-half [coll]
  (apply set/sorted-set (take (/ (count coll) 2) coll)))

(defn -upper-half [coll]
  (apply set/sorted-set (drop (/ (count coll) 2) coll)))

(def lower-half (memoize -lower-half))
(def upper-half (memoize -upper-half))

(defn encode-rows [{:as arg-map :keys [coll x codes] :or {codes "" coll rows}}]
  (let [lhs (lower-half coll)
        [code coll] (if (contains? lhs x)
                      ["F" lhs]
                      ["B" (upper-half coll)])
        codes (str codes code)]
    (if (= 1 (count lhs))
      codes
      (encode-rows (merge arg-map {:coll coll :codes codes})))))

(defn encode-cols [{:as arg-map :keys [coll x codes] :or {codes "" coll cols}}]
  (let [lhs (lower-half coll)
        [code coll] (if (contains? lhs x)
                      ["L" lhs]
                      ["R" (upper-half coll)])
        codes (str codes code)]
    (if (= 1 (count lhs))
      codes
      (encode-cols (merge arg-map {:coll coll :codes codes})))))

(def inputs
  (util/into []
             (map (fn [s]
                    (let [[_ row col] (re-matches #"^(\w{7})(\w{3})$" s)]
                      {:s s
                       :row row
                       :col col})))
             "inputs/day05.txt"))

(def decode-rows (into {}
                       (map (fn [x]
                              [(encode-rows {:x x})
                               x]))
                       rows))

(def decode-cols (into {}
                       (map (fn [x]
                              [(encode-cols {:x x})
                               x]))
                       cols))

(defn seat-id [row col]
  (+ (* row
        8)
     col))

(def seat-ids (apply set/sorted-set (map (fn [{:keys [row col]}]
                                           (seat-id (decode-rows row)
                                                    (decode-cols col)))
                                         inputs)))

(defn part-1 [& args]
  (println (last seat-ids)))

(defn part-2 [& args]
  (let [possible-seat-ids (into #{}
                                (map (fn [[row col]]
                                       (seat-id row col)))
                                ;; not in the front row or back row
                                (cartesian-product (range 1 127) (range 8)))]
    (println (filter (fn [id]
                       (and (not (contains? seat-ids id))
                            (contains? seat-ids (inc id))
                            (contains? seat-ids (dec id))))
                     possible-seat-ids))))
