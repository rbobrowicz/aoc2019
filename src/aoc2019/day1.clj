(ns aoc2019.day1
  (:require [clojure.string :as str]))

(defn fuel-required [mass]
  (-> (/ mass 3)
       int
       (- 2)))

(defn read-input-file [path]
  (->> (slurp path)
       str/split-lines
       (map #(Integer/parseInt %))))

(defn calculate-total-fuel [fuel-fun ms]
  (reduce + (map fuel-fun ms)))

(defn fuel-required-2 [mass]
  (->> (iterate fuel-required mass)
       (take-while #(< 0 %))
       next
       (reduce +)))

(comment
  (calculate-total-fuel fuel-required (read-input-file "./resources/day1.input"))
  (calculate-total-fuel fuel-required-2 (read-input-file "./resources/day1.input"))
)


