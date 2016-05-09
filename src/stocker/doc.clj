(ns stocker.doc
  (:require [medley.core :refer [filter-kv map-kv map-keys]]
            [stocker.util :refer :all]
            [stocker.xbrl :refer [parse-xbrl-dei]]
            [clojure.java.jdbc :as j]
            [clojure.pprint :refer [pprint]])
  (:import (java.io File)
           [java.util Calendar UUID]
           [java.text SimpleDateFormat]
           [java.sql Date]))

(defn to-cal [date-str]
  (let [cal (Calendar/getInstance)
        sdf (SimpleDateFormat. "yyyy-MM-dd")
        dat (.parse sdf date-str)]
    (.setTime cal dat)
    cal))

(def trans
  {:dei:DocumentPeriodEndDate [:document_period_end_date 
                               #(Date. (.getTime (.getTime (to-cal (first (first (vals %)))))))]
   :dei:DocumentFiscalYearFocus [:document_fiscal_year_focus
                                 #(Short/parseShort (first (first (vals %))))]
   :dei:DocumentFiscalPeriodFocus [:document_fiscal_period_focus
                                   #(first (first (vals %)))]
   :dei:EntityCentralIndexKey [:cik
                               #(first (first (vals %)))]
   :dei:EntityCommonStockSharesOutstanding [:common_stock_shares_outstanding
                                            #(Long/parseLong (first (first (vals %))))]
   :dei:AmendmentFlag [:amendment_flag
                       #(Boolean/parseBoolean (first (first (vals %))))]
   :dei:DocumentType [:document_type
                      #(first (first (vals %)))]
   :dei:EntityFilerCategory [:entity_filer_category
                             #(first (first (vals %)))]
   :dei:CurrentFiscalYearEndDate [:current_fiscal_year_end_date
                                  #(do % (Date. (.getTime (.getTime (Calendar/getInstance)))))]}) ;;fix this - you're better than this

(defrecord Doc 
    [#^long id
     #^String cik
     #^String hash
     #^Calendar document_period_end_date
     #^short document_fiscal_year_focus
     #^Calendar current_fiscal_year_end_date
     #^long common_stock_shares_outstanding
     #^String document_path
     #^String document_fiscal_period_focus
     #^String entity_filer_category
     #^String document_type
     #^boolean amendment_flag])

(defn get-next-id []
  (:nextval (first (j/query db-spec ["SELECT nextval('document_id_seq')"]))))

(defn make-docmap-xbrl-dei [xbrl-dei path]
  (let [keyed-doc (dissoc 
                        (map-kv #(if (%1 trans) 
                                   (list ((%1 trans) 0) (((%1 trans) 1) %2))
                                   nil) 
                                xbrl-dei) 
                        nil)]
    (-> keyed-doc
        (assoc :id (get-next-id))
        (assoc :hash (hash-string (slurp path)))
        (assoc :document_path path)
        (assoc :hash (UUID/randomUUID)))))

(defn add-doc [xbrl-dei path]
  (let [adoc (map->Doc (make-docmap-xbrl-dei xbrl-dei path))]
    (j/insert! db-spec "document" adoc)
    adoc))

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
  (map-kv #(vector %1 (map-keys keyword %2)) 
          (read-json (str working-dir File/separator "docs.data"))))

(defn get-latest-10k [cik]
  ())
