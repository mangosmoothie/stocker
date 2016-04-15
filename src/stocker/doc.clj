(ns stocker.doc
  (:require [medley.core :refer [map-kv]]
            [stocker.util :refer :all])
  (:import (java.io File)))

(defrecord Doc 
    [documentPeriodEndDate documentFiscalYearFocus documentFiscalPeriodFocus
     entityCentralIndexKey entityCommonStockSharesOutstanding amendmentFlag
     documentType entityFilerCategory tradingSymbol entityRegistrantName 
     currentFiscalYearEndDate])

(defn- re-key
  [kw]
  (let [kwp (subs (str kw) 5)]
    (keyword (str (.toLowerCase (subs kwp 0 1)) (subs kwp 1)))))

(defn make-doc-xbrl-dei
  [xbrl path]
  (map->Doc 
   (assoc (map-kv #(vector (re-key %1) (first (first (vals %2)))) xbrl) 
          :path path)))

(defn read-all-docs []
  (let [filepath (str File/separator )]))

(defn read-docs
  "read all saved doc info"
  [dir]
  )
