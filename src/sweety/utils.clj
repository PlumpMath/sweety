(ns sweety.utils
  "TODO: doc"
  (:require [clojure.string :as string]))

(defn de-camel
  [s]
  (->> (partition-by #(Character/isUpperCase %) s)
       (partition 2)
       (map #(string/lower-case (apply str (apply concat %))))
       (string/join "-")))

(defn lower-case [s]
  (-> s (string/replace "_" "-") string/lower-case))

(letfn [(methods-starting-with [start c]
          (->> (.getMethods c)
               (map #(.getName %))
               (filter #(.startsWith % start))
               distinct
               (map symbol)))]

  (def getters (partial methods-starting-with "get"))
  (def setters (partial methods-starting-with "set")))

(defn method->keyword [sym]
  (let [lower #(let [s (de-camel %)] (if (seq s) s %))]
    (-> sym str (subs 3) lower keyword)))
