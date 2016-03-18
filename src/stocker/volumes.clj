(ns stocker.volumes
  (:require [stocker.util :refer :all])
  (:import (java.util Calendar)
           (java.text SimpleDateFormat)))

(defn significant? [vol avg-daily-volume]
  (< 0.2 
     (*  (day-portion) (abs (/ (- avg-daily-volume vol) vol)))))

