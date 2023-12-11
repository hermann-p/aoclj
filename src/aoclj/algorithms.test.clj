(ns aoclj.algorithms.test
  (:require  [clojure.test :as t]
             [aoclj.algorithms :as algo]))

(t/deftest greatest-common-divisor
  (t/are [expected result] (= expected result)
    21 (algo/gcd 1071 462)
    7  (algo/gcd 1071 462 7)
    1  (algo/gcd 1071 462 1)))

(t/deftest least-common-multiple
  (t/are [expected result] (= expected result)
    2520 (algo/lcm 140 72)
    2520 (algo/lcm 72 140)
    3528 (algo/lcm 1 147 72)
    3528 (algo/lcm 147 72 1)))

(t/deftest quadratic-roots
  (t/are [expected result] (= expected result)
    [2.0 -10.0] (algo/quadratic-roots 1 8 -20)
    [5.0 -5.0]  (algo/quadratic-roots 1 0 -25)
    [0]         (algo/quadratic-roots 1 0 0)
    nil         (algo/quadratic-roots 1 0 1)))

(t/deftest manhattan
  (t/are [expected result] (= expected result)
    0 (algo/manhattan [0 0] [0 0])
    3 (algo/manhattan [0 0] [3 0])
    3 (algo/manhattan [0 0] [0 3])
    6 (algo/manhattan [1 1] [4 4])))

#_(time (t/run-tests))
