(ns day06
  (:require [clojure.string :as string]
            [cuid.core :refer [cuid]]
            [datascript.core :as d]))

(def schema
  {:questions {:db/cardinality :db.cardinality/many}})

(def inputs
  (into []
        (mapcat (fn [s]
                  (let [group-id (cuid)
                        row (string/split s #"\n")]
                    (into []
                          (map (fn [s]
                                 (let [passenger-id (cuid)
                                       questions (into #{}
                                                       (filter #(not= "" %))
                                                       (string/split s #""))]
                                   {:group-id     group-id
                                    :passenger-id passenger-id
                                    :questions    questions})))
                          row))))
        (string/split (slurp "inputs/day06.txt")
                      #"\n\n")))

(defn create-conn [tx-data]
  (let [conn (d/create-conn schema)]
    (d/transact conn tx-data)
    conn))

(def conn (create-conn inputs))

(defn part-1 [& args]
  (let [coll (d/q '[:find ?g (count-distinct ?q)
                    :keys group-id count
                    :where [?e :group-id ?g]
                           [?e :questions ?q]]
                  @conn)]
    (println (reduce (fn [result {:keys [count]}]
                       (+ result count))
                     0
                     coll))))

(defn part-2 [& args]
  (let [group-size (into {} (d/q '[:find ?g (count-distinct ?e)
                                   :where [?e :group-id ?g]]
                                 @conn))
        coll (filter (fn [{:keys [group-id count]}]
                       (= count (get group-size group-id)))
                     (d/q '[:find ?g ?q (count-distinct ?e)
                            :keys group-id question count
                            :where [?e :group-id ?g]
                                   [?e :questions ?q]]
                          @conn))]
    (println (reduce +
                     (map (fn [[group-id coll]]
                            (count (into #{} (map :question) coll)))
                          (group-by :group-id coll))))))
