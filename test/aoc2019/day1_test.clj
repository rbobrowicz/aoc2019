(ns aoc2019.day1-test
  (:require [clojure.test :refer :all]
            [aoc2019.day1 :refer :all]))

(deftest fuel-required-test
  (testing "fuel required equation"
    (is (= 2 (fuel-required 12)))
    (is (= 2 (fuel-required 14)))
    (is (= 654 (fuel-required 1969)))
    (is (= 33583 (fuel-required 100756)))))

(deftest fuel-required-2-test
  (testing "fuel required equation"
    (is (= 2 (fuel-required-2 14)))
    (is (= 966 (fuel-required-2 1969)))
    (is (= 50346 (fuel-required-2 100756)))))
