(ns aoc2019.day2
  (:require [clojure.string :as str]))

(defn- intcode-f! [f i p]
  (let [a1 (nth p (+ 1 i))
        a2 (nth p (+ 2 i))
        a3 (nth p (+ 3 i))
        v1 (nth p a1)
        v2 (nth p a2)]
    (aset p a3 (f v1 v2))))

(defn- intcode-add! [i p] (intcode-f! + i p))
(defn- intcode-mul! [i p] (intcode-f! * i p))

(defn run-intcode [prog]
  (let [p (into-array prog)]
    (loop [i 0]
      (case (nth p i)
        99 (into [] p)
        1 (do
            (intcode-add! i p)
            (recur (+ 4 i)))
        2 (do
            (intcode-mul! i p)
            (recur (+ 4 i))))
      )))

(defn read-input-file [path]
  (-> (slurp path)
      (str/split #",")
      ((fn [xs] (map #(Long/parseLong %) xs)))))

(comment

  ; First puzzle
  (let [input (read-input-file "./resources/day2.input")
        i1 (first input)
        is (drop 3 input)
        patched (lazy-seq (cons i1 (cons 12 (cons 2 is))))]
    (first (run-intcode patched)))

  ; second puzzle
  (let [input (read-input-file "./resources/day2.input")
        i1 (first input)
        is (drop 3 input)]
    (for [noun (range 100)
          verb (range 100)
          :let [mem (lazy-seq (cons i1 (cons noun (cons verb is))))
                res (first (run-intcode mem))]
          :when (= res 19690720)]
      (+ verb (* 100 noun))))
)
