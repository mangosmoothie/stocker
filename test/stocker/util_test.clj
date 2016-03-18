(ns stocker.util-test
  (:require [clojure.test :refer :all]
            [stocker.util :refer :all]))

(deftest test-abs
  (testing "can compute the absolute value"
    (is (= 1 (abs -1)))
    (is (= 1 (abs 1)))
    (is (= 2/10 (abs 2/10)))
    (is (= 2/10 (abs -2/10)))))
