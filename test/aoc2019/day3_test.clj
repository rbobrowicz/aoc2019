(ns aoc2019.day3-test
  (:require [clojure.test :refer :all]
            [aoc2019.day3 :refer :all]))

(deftest calculate-intersection-distance-tests
  (testing "example input"
    (is (= 159 (calculate-intersection-distance "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                                "U62,R66,U55,R34,D71,R55,D58,R83")))
    (is (= 135 (calculate-intersection-distance "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                                                "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))

(deftest find-step-sum-tests
  (testing "example input"
    (is (= 610 (find-step-sum "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                              "U62,R66,U55,R34,D71,R55,D58,R83")))
    (is (= 410 (find-step-sum "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                              "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))
