(ns stocker.util
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io])
  (:import (java.util Calendar)
           (java.util.zip ZipInputStream)
           (java.io File)
           (java.io FileOutputStream)
           (java.io FileInputStream)))

(def config (json/read-str (slurp "config")))

(def edgar-monthly-feed-base (get config "edgar-monthly-feed-base"))
(def working-dir (get config "working-dir"))
(def landingzone (get config "landingzone"))
(def feed-cache (get config "feed-cache"))
(def doc-store (get config "doc-store"))
(def ticker-search-url (get config "ticker-search-url"))
(def starter-tickers (get config "starter-tickers"))
(def starter-ciks (get config "starter-ciks"))
(def form-types #{"10-K" "10-Q"})

(defn read-json
  [filepath]
  (if (.exists (File. filepath))
    (json/read-str (slurp filepath))
    {}))

(defn write-json
  [filepath data]
  (with-open [wrtr (io/writer filepath)]
    (.write wrtr (json/write-str data))))

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

(defn strip-extension
  [filename]
  (.substring filename 0 (.lastIndexOf filename ".")))

(defn get-files
  "return all files in given dir path"
  [dirpath]
  (let [dir (File. dirpath)]
    (filter #(.isFile %) (.listFiles dir))))

(defn- get-zip-out-dir
  [zipfile outdirpath]
  (File. (str outdirpath (File/separator) (strip-extension (.getName zipfile)))))

(defn get-zip-files
  "return all .zip files in a given dir path"
  [dirpath]
  (filter #(.endsWith (.getName %) ".zip") (get-files dirpath)))

(defn- zip-entries
  [zipfile]
  (lazy-seq
   (if-let [entry (.getNextEntry zipfile)]
     (cons entry (zip-entries zipfile)))))

(defn unzip-file
  "decompress zip file and save to location - preserving filenames"
  [zipfile outdirpath]
  (let [buffer (byte-array 1024)
        outdir (get-zip-out-dir zipfile outdirpath)]
    (.mkdir outdir)
    (println "unzipping " (.getPath zipfile))
    (with-open [zis (ZipInputStream. (FileInputStream. zipfile))]
      (loop [e (.getNextEntry zis)]
        (if e
          (let [filename (.getName e)]
            (let [outfile (File. (str outdir (File/separator) filename))]
              (with-open [outstream (FileOutputStream. outfile)]
                (println "writing file: " (.getPath outfile))
                (.mkdirs (File. (.getParent outfile)))
                (loop [len (.read zis buffer)]
                  (if (< 0 len)
                    (do
                      (.write outstream buffer 0 len)
                      (recur (.read zis buffer)))))))
            (recur (.getNextEntry zis))))))))

(defn unzip-all
  "unzip all .zip files in dirpath"
  [dirpath outdirpath]
  (doseq [zipfile (get-zip-files dirpath)]
    (if (.exists (get-zip-out-dir zipfile outdirpath))
      (println "skipping file, output dir exists for " (.getPath zipfile))
      (unzip-file zipfile outdirpath))))

