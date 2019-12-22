(ns aoc2019.day3
  (:require [clojure.string :as str]))

(defn- parse-segment [seg]
  (let [dir (keyword (.substring seg 0 1))
        cnt (Long/parseLong (.substring seg 1))]
    [dir cnt]))

(defn- parse-path [path]
  (-> path
    (str/split #",")
    (#(map parse-segment %))))

(defn- make-coordinate [x y]
  (keyword (str x "x" y)))

(defn- from-coordinate [c]
  (let [s (name c)
        i (.indexOf s "x")
        x (Long/parseLong (.substring s 0 i))
        y (Long/parseLong (.substring s (+ i 1)))]
    [x y]))

(defn- const [x y] x)

(defn- path-coords [x y cnt xf yf]
  (for [c (range 1 (+ 1 cnt))]
    (make-coordinate (xf x c) (yf y c))))

(defn- right-coords [x y cnt]
  (path-coords x y cnt + const))

(defn- left-coords [x y cnt]
  (path-coords x y cnt - const))

(defn- up-coords [x y cnt]
  (path-coords x y cnt const +))

(defn- down-coords [x y cnt]
  (path-coords x y cnt const -))

(defn- mark-point [id board c]
  (update board c #(if (or (nil? %)
                           (= id %))
                     id
                     :C)))

(defn- manhattan-distance
  ([[x y]] (+ (Math/abs x) (Math/abs y)))
  ([[x1 y1] [x2 y2]] (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1)))))

(defn- coordinates-in-direction [x y dir cnt]
  (case dir
    :R (right-coords x y cnt)
    :L (left-coords x y cnt)
    :D (down-coords x y cnt)
    :U (up-coords x y cnt)))

(defn- trace-path
  ([path id board]
   (trace-path [0 0] path id board))
  ([[x y] path id board]
   (if-let [[dir cnt] (first path)]
     (let [coords (coordinates-in-direction x y dir cnt)
           new-board (reduce (partial mark-point id) board coords)
           new-start (from-coordinate (last coords))]
       (recur new-start (rest path) id new-board))
     board)))

(defn calculate-intersection-distance [path1 path2]
  (->> {:0x0 :O}
       (trace-path (parse-path path1) :x)
       (trace-path (parse-path path2) :y)
       (filter (comp #(= :C %) last))
       (map (comp manhattan-distance from-coordinate first))
       sort
       first))

(defn- read-input-file [path]
  (->> path
       slurp
       str/split-lines))

(defn- calculate-steps-till-point
  ([path board target]
   (calculate-steps-till-point [0 0] 0 path board target))
  ([[x y] sum path board target]
   (if-let [[dir cnt] (first path)]
     (let [coords (coordinates-in-direction x y dir cnt)
           new-start (from-coordinate (last coords))]
       (if (some #(= % target) coords)
         (+ sum (manhattan-distance [x y] (from-coordinate target)))
         (recur new-start (+ cnt sum) (rest path) board target))))))

(defn find-step-sum [path1 path2]
  (let [path1 (parse-path path1)
        path2 (parse-path path2)
        board (->> {:0x0 :O}
                   (trace-path path1 :x)
                   (trace-path path2 :y))
        ints (map first (filter (comp #(= :C %) last) board))
        steps (for [c ints
                    :let [d1 (calculate-steps-till-point path1 board c)
                          d2 (calculate-steps-till-point path2 board c)]]
                (+ d1 d2))]
    (first (sort steps))))

(comment

  (apply calculate-intersection-distance (read-input-file "./resources/day3.input"))
  (apply find-step-sum (read-input-file "./resources/day3.input"))

)
