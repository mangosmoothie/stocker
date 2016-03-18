(ns stocker.volumes-test
  (:require [clojure.test :refer :all]
            [stocker.volumes :refer :all]))

(deftest test-significant?
  (testing "able to detect significant intra-day volume"
    (is (significant? 4000 10))
    (is (significant? 10 4000))
    (is (not (significant? 10 11)))
    (is (not (significant? 11 10)))))
