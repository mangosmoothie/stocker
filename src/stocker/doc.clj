(ns stocker.doc
  (:require [medley.core :refer [map-kv]]
            [stocker.util :refer :all]
            [stocker.xbrl :refer [parse-xbrl-dei]])
  (:import (java.io File)))

(defrecord Doc 
    [documentPeriodEndDate documentFiscalYearFocus documentFiscalPeriodFocus
     entityCentralIndexKey entityCommonStockSharesOutstanding amendmentFlag
     documentType entityFilerCategory tradingSymbol entityRegistrantName 
     currentFiscalYearEndDate path])

(defn- re-key
  [kw]
  (let [kwp (subs (str kw) 5)]
    (keyword (str (.toLowerCase (subs kwp 0 1)) (subs kwp 1)))))

(defn make-doc-xbrl-dei
  [xbrl path]
  (map->Doc 
   (assoc (map-kv #(vector (re-key %1) (first (first (vals %2)))) xbrl) 
          :path path)))

(defn parse-all-doc-info []
  (reduce
   (fn [acc n]
     (let [path (.getAbsolutePath n)]
       (assoc acc path (make-doc-xbrl-dei (parse-xbrl-dei path) path))))
   {}
   (filter #(re-matches #"^\w+-\d{8}\.xml$" (.getName %))
           (file-seq (clojure.java.io/file doc-store)))))

(defn write-docs
  [docsmap]
  (write-json (str working-dir File/separator "docs.data") docsmap))

(defn read-docs "read all saved doc info" []
  (read-json (str working-dir File/separator "docs.data")))
