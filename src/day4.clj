(ns day4
  (:require [clojure.string :as string]
            [clojure.alpha.spec :as s]
            [datascript.core :as d]))

;; https://github.com/clojure/spec-alpha2/wiki/Schema-and-select#unqualified-keys
(s/def ::passport (s/schema {:byr (s/and string?
                                         #(re-matches #"^\d{4}$" %)
                                         #(<= 1920 (Integer/parseInt %) 2002))
                             :iyr (s/and string?
                                         #(re-matches #"^\d{4}$" %)
                                         #(<= 2010 (Integer/parseInt %) 2020))
                             :eyr (s/and string?
                                         #(re-matches #"^\d{4}$" %)
                                         #(<= 2020 (Integer/parseInt %) 2030))
                             :hgt (s/and string? (s/or :in (s/and #(re-matches #"^\d{2}in$" %)
                                                                  #(<= 59
                                                                       (Integer/parseInt (re-find #"\d{2}" %))
                                                                       76))
                                                       :cm (s/and #(re-matches #"^\d{3}cm$" %)
                                                                  #(<= 150
                                                                       (Integer/parseInt (re-find #"\d{3}" %))
                                                                       193))))
                             :hcl (s/and string?
                                         #(re-matches #"^#[0-9a-f]{6}$" %))
                             :ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
                             :pid (s/and string?
                                         #(re-matches #"^\d{9}$" %))
                             :cid string?}))

(s/def ::valid-passport (s/select ::passport [:byr :iyr :eyr :hgt :hcl :ecl :pid]))

(def inputs
  (into []
        (map (fn [s]
               (let [tuples (string/split s #"\n|\s")]
                 (into {}
                       (map (fn [s]
                              (let [[k v] (string/split s #":")]
                                [(keyword k) v])))
                       tuples))))
        (string/split (slurp "inputs/day4.txt")
                      #"\n\n")))

(defn create-conn [tx-data]
  (let [conn (d/create-conn {})]
    (d/transact conn tx-data)
    conn))

(defn part-1 [& args]
  (let [conn (create-conn inputs)]
    (println (count (d/q '[:find ?e
                           :where [?e :byr _]
                                  [?e :iyr _]
                                  [?e :eyr _]
                                  [?e :hgt _]
                                  [?e :hcl _]
                                  [?e :ecl _]
                                  [?e :pid _]]
                         @conn)))))

(defn valid? [db eid]
  (let [m (d/pull db
                  [:byr :iyr :eyr :hgt :hcl :ecl :pid]
                  eid)]
    (s/valid? ::valid-passport m)))

(defn part-2 [& args]
  (let [conn (create-conn inputs)]
    (println (count (d/q '[:find ?e
                           :in $ valid?
                           :where [?e :byr _]
                                  [?e :iyr _]
                                  [?e :eyr _]
                                  [?e :hgt _]
                                  [?e :hcl _]
                                  [?e :ecl _]
                                  [?e :pid _]
                                  [(valid? $ ?e)]]
                         @conn valid?)))))
