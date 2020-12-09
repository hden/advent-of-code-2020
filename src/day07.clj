(ns day7
  (:require [util]
            [clojure.string :as string]
            [datascript.core :as d]))

(def schema
  {:bag/color {:db/unique :db.unique/identity
               :db/index true}
   :rule/from {:db/valueType :db.type/ref
               :db/index true}
   :rule/to   {:db/valueType :db.type/ref
               :db/index true}})

(defn parse [s]
  (let [[origin & targets] (string/split s #" contain |, |\.")
        origin (last (re-matches #"(\w+ \w+).+" origin))]
    (into []
          (comp (filter #(not= "no other bags" %))
                (map (fn [s]
                       (let [[_ count color] (re-matches #"^(\d+) (\w+ \w+) \w+$" s)]
                         {:rule/from {:bag/color origin}
                          :rule/to   {:bag/color color}
                          :rule/count (Integer/parseInt count)}))))
          targets)))

(def inputs
  (util/into []
             (mapcat parse)
             "inputs/day07.txt"))

(defn create-conn [tx-data]
  (let [conn (d/create-conn schema)]
    (d/transact conn tx-data)
    conn))

(def conn (create-conn inputs))

(def ^:const my-color "shiny gold")

(defn part-1 [& args]
  (let [colors (reduce (fn [result _]
                         (let [next (d/q '[:find ?out
                                           :in $ [[?in]]
                                           :where [?to :bag/color ?in]
                                                  [?x :rule/to ?to]
                                                  [?x :rule/from ?from]
                                                  [?from :bag/color ?out]]
                                         @conn
                                         result)
                               colors (into result next)]
                           (if (= result colors)
                             (reduced colors)
                             colors)))
                       #{[my-color]}
                       (range))]
    (println (count (disj colors [my-color])))))

(defn part-2 [& args]
  (let [db @conn
        root {:eid [:bag/color my-color]
              :count 1}
        children (fn [{:keys [eid count]}]
                   (let [coll (d/q '[:find ?color ?count
                                     :keys child-color child-count
                                     :in $ ?from
                                     :where [?x :rule/from ?from]
                                            [?x :rule/count ?count]
                                            [?x :rule/to ?to]
                                            [?to :bag/color ?color]]
                                   db
                                   eid)]
                     (map (fn [{:keys [child-color child-count]}]
                             {:eid [:bag/color child-color]
                              :count (* count child-count)})
                          coll)))]
    ;; The shiny gold bag itself doesn't count.
    (println (- (reduce (fn [result {:keys [count]}]
                          (+ result count))
                        0
                        (tree-seq children children root))
                1))))
