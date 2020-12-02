(ns util
  (:refer-clojure :exclude [into])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn into [to xf from]
  (clojure.core/into to
                     (comp (filter #(not= "" %))
                           xf)
                     (string/split-lines (slurp from))))
