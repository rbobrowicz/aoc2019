(ns aoc2019.day2-test
  (:require [clojure.test :refer :all]
            [aoc2019.day2 :refer :all]))

(deftest run-intcode-test
  (testing "test sample programs"
    (is (= [2,0,0,0,99] (run-intcode [1,0,0,0,99])))
    (is (= [2,3,0,6,99] (run-intcode [2,3,0,3,99])))
    (is (= [2,4,4,5,99,9801] (run-intcode [2,4,4,5,99,0])))
    (is (= [30,1,1,4,2,5,6,0,99] (run-intcode [1,1,1,4,99,5,6,0,99])))))
