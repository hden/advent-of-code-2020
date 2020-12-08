(ns day8
  (:require [util]
            [clojure.string :as string]))

(def schema
  {:bag/color {:db/unique :db.unique/identity
               :db/index true}
   :rule/from {:db/valueType :db.type/ref
               :db/index true}
   :rule/to   {:db/valueType :db.type/ref
               :db/index true}})

(defn parse [id s]
  (let [[op val] (string/split s #" ")]
    {:id id
     :op op
     :val (Integer/parseInt val)}))

(def inputs
  (util/into []
             (map-indexed parse)
             "inputs/day8.txt"))

(def ^:const init {:position 0
                   :accumulator 0
                   :program inputs
                   :operations #{}})

(defn evaluator
  [{:as state :keys [program position accumulator operations]} _]
  (let [{:keys [id op val]} (get program position {})]
    (cond
      ;; normal termination
      (nil? op)
      (reduced (merge state {:exit 0}))

      ;; infinity loop
      (contains? operations id)
      (reduced (merge state {:exit 1}))

      ;; next iteration
      :else
      (merge state (case op
                     "nop" {:position (inc position)}
                     "acc" {:position (inc position)
                            :accumulator (+ accumulator val)
                            :operations (conj operations id)}
                     "jmp" {:position (+ position val)
                            :operations (conj operations id)})))))

(defn part-1 [& args]
  (println (:accumulator (reduce evaluator init (range)))))

(defn swap-ops [op]
  (case op
    "nop" "jmp"
    "jmp" "nop"
    op))

(defn part-2 [& args]
  (println (:accumulator (reduce (fn [_ {:as m :keys [id op val]}]
                                   (let [init (assoc init :program (update-in inputs [id :op] swap-ops))
                                         {:as result :keys [exit]} (reduce evaluator init (range))]
                                     (if (= exit 0)
                                       (reduced result)
                                       {})))
                                 {}
                                 inputs))))
