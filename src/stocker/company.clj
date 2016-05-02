(ns stocker.company
  (:require [clojure.java.jdbc :as j]
            [stocker.util :refer :all]))

(defrecord Company 
    [#^String cik
     #^String ticker
     #^String company_name])

(defn add-company [company]
  (let [co (map->Company company)]
    (j/insert! db-spec "company" co)
    co))

(defn get-all []
  (reduce
   (fn [acc n]
     (assoc acc (:cik n) n))
   {}
   (j/query db-spec ["SELECT * FROM company"] {:row-fn map->Company})))
