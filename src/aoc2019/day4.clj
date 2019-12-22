(ns aoc2019.day4
  (:require [clojure.string :as str]))

(defn- run-lengths [n]
  (if-let [c (first n)]
    (let [xs (take-while #(= c %) n)]
      (cons (count xs) (run-lengths (.substring n (count xs)))))))

(defn- has-double? [n]
  (some #(= 2 %) (run-lengths n)))

(defn- ascending? [n]
  (every? identity (for [i (range (- (.length n) 1))]
                     (<= (int (nth n i)) (int (nth n (+ i 1)))))))

(defn generate-candidates [min max]
  (->> (range min (+ 1 max))
       (map str)
       (filter ascending?)
       (filter has-double?)))

(comment

  (count (generate-candidates 146810 612564))

)
