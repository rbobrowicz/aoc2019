(ns aoc2019.day6
  (:require [clojure.string :as str]))

(defn parse-entry [entry]
  (let [p-idx (.indexOf entry ")")]
    [(keyword (.substring entry 0 p-idx))
     (keyword (.substring entry (+ 1 p-idx)))]))

(defn read-input-file [path]
  (->> (slurp path)
       (str/split-lines)
       (map parse-entry)))

(defn make-orbit-map
  ([data]
   (make-orbit-map data {}))
  ([data m]
   (if-let [[x y] (first data)]
     (recur (rest data) (update m x conj y))
     m)))

(defn make-reverse-map
  ([data]
   (make-reverse-map data {}))
  ([data m]
   (if-let [[x y] (first data)]
     (recur (rest data) (assoc m y x))
     m)))

(defn find-path [m s]
  (when (not= s :COM)
    (let [n (get m s)]
      (cons n (find-path m n)))))

(defn find-root [p1 p2]
  (let [[x1 & xs] p1
        [y1 & ys] p2]
    (if (and (= x1 y1) (= (first xs) (first ys)))
      (recur xs ys)
      x1)))

(defn solve-1 [data]
  (let [orbit-counts {:COM 0}
        orbit-map (make-orbit-map data)]
    (loop [orbit-counts orbit-counts
           work-stack '(:COM)]
      (if (seq work-stack)
        (let [x (first work-stack)
              c (get orbit-counts x)
              ys (get orbit-map x)]
          (recur
           (reduce #(assoc %1 %2 (inc c)) orbit-counts ys)
           (concat ys (rest work-stack))))
        (reduce + (vals orbit-counts))))))

(defn solve-2 [data]
  (let [rev-map (make-reverse-map data)
        you-path (reverse (find-path rev-map :YOU))
        san-path (reverse (find-path rev-map :SAN))
        common-root (find-root you-path san-path)
        path (concat (reverse (drop-while #(not= common-root %) san-path))
                     (rest (drop-while #(not= common-root %) you-path)))]
    (count (butlast path))))

(comment

  (solve-1 (read-input-file "./resources/day6.input"))
  (solve-2 (read-input-file "./resources/day6.input"))

  (solve-2
   (->> "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
        (str/split-lines)
        (map parse-entry)))
  )
