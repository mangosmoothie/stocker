(ns stocker.util
  (:import (java.util Calendar)))

(defn market-open? []
  (and (<= 9 (.get (Calendar/getInstance) Calendar/HOUR_OF_DAY) 15)
           (< 1 (.get (Calendar/getInstance) Calendar/DAY_OF_WEEK) 7)))

(defn abs [num] (max num (- num)))

(defn day-portion []
  (if (not (market-open?))
    1
    (/ (+ (.get (Calendar/getInstance) Calendar/MINUTE)
          (.get (Calendar/getInstance) Calendar/HOUR_OF_DAY))
     (* 6.5 60))))
