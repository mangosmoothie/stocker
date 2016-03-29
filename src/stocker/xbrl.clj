(ns stocker.xbrl
  (:require [clojure.xml :as xml]
            [clj-http.client :as client]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io])
  (:import (java.util Calendar)
           (java.io File)))

;;(clojure.core/refer 'clojure.core)
;;(clojure.core/refer 'clojure.repl)

(def amzn-test-q1  "/Users/nlloyd/projects/stocker/testdata/amzn-20150331.xml")
(def edgar-monthly-feed-base "https://www.sec.gov/Archives/edgar/monthly/")
(def working-dir "/Users/nlloyd/sec/")
(def landingzone (str working-dir "landingzone/"))
(def feed-cache (str working-dir "feeds/"))
(def ciks {"AMZN" "0001018724"})
(def form-types #{"10-K" "10-Q"})
(def tst-rss "/Users/nlloyd/Downloads/xbrlrss-2015-04.xml")

(defn in? 
  "coll contains item?"
  [coll item]  
  (some #(= item %) coll))

(defn tags= [tag xml]
  (filter #(= tag (:tag %)) xml))

(defn tag= [tag xml]
  (first (tags= tag xml)))

(defn filename-from-url
  [url]
  (let [slash-index (.lastIndexOf url "/")]
    (subs url (inc slash-index))))

(defn gen-cache-filepath
  "generate a filename, local file path and file"
  [url cache-dir]
  (let [filename (filename-from-url url)
        filepath (str cache-dir filename)]
    (vector  filename filepath (File. filepath))))

(defn download-file [url output-dir]
  (let [[filename filepath file] (gen-cache-filepath url output-dir)]
    (if (.exists file)
      (println "skipping download, file already exists: " filename)
      (with-open [in (io/input-stream url)
                  out (io/output-stream filepath)]
        (println "downloading file from: " url)
        (io/copy in out)))))

(defn get-cached-file
  "return cached filename if available; else return remote url"
  [url cache-dir]
  (let [[_ filepath file] (gen-cache-filepath url cache-dir)]
    (if (.exists file)
      filepath
      url)))

(defn xbrlrss-filepaths
  "returns the local cached rss feed & will cache feed if needed"
  [xbrlrss-urls output-dir]
  (doseq [url xbrlrss-urls]
    (download-file url output-dir))
  (map #(get-cached-file % output-dir) xbrlrss-urls))

(defn xbrlrss-urls
  "return the rss feed urls for xbrls downloads from the SEC EDGAR system (beg-month & end-month are inclusive)"
  ([year] (if (= year (.get (Calendar/getInstance) (Calendar/YEAR)))
            (xbrlrss-urls year 1 (inc (.get (Calendar/getInstance) (Calendar/MONTH))))
            (xbrlrss-urls year 1 12)))
  ([year beg-month end-month] 
   (map #(str edgar-monthly-feed-base "xbrlrss-" year "-"
              (if (< % 10) (str "0" %) (str %)) ".xml") (range beg-month (inc end-month)))))

(defn get-xbrlrss-feeds
  "return the monthly feeds preferring local cached"
  [year cache-dir]
  (xbrlrss-filepaths (xbrlrss-urls year) cache-dir))

(defn parse-xbrlrss
  "return the urls of resources we are interested in from the rss feed"
  [rss-url form-types cik-nums]
  (map #(->> % :content (tag= :enclosure) :attrs :url)
       (->> (xml/parse rss-url) :content
            (tag= :channel) :content
            (tags= :item)
            (filter #(and (->> % :content (tag= :edgar:xbrlFiling) :content 
                               (tag= :edgar:formType) :content first (contains? form-types))
                          (->> % :content (tag= :edgar:xbrlFiling) :content 
                               (tag= :edgar:cikNumber) :content first (in? cik-nums)))))))

(defn get-xbrl-zip-urls
  "return the xbrl zip urls given a year, form types and CIKs (company Ids)"
  [year form-types cik-nums]
  (flatten (map #(parse-xbrlrss % form-types cik-nums) (get-xbrlrss-feeds year feed-cache))))

(defn parse-xbrl-gaap [filename context]
  (into {} 
        (map #(vector (:tag %) (:content %)) 
             (->> (xml/parse filename) 
                  :content 
                  (filter #(and 
                            (.startsWith (str (:tag %)) ":us-gaap:")
                            (= context (->> % :attrs :contextRef))
                            (not (.endsWith (str (:tag %)) "TextBlock"))))))))

(defn find-entries 
  "filter the map by keys containing a string"
  [a-map contains]
  (select-keys a-map
               (filter #(and (.contains (clojure.string/upper-case (str  %))
                                        (clojure.string/upper-case contains))
                             (not (.endsWith (str %) "TextBlock"))) 
                       (keys a-map))))

(defn dic [a-map contains]
  (pprint (find-entries a-map contains)))

(defn get-artifacts-for-year
  "retrieves monthly historical rss feeds for year"
  [year output-dir]
  (println "fetching rss feeds for year: " year)
  (println "output dir is: " output-dir)
  (println "duplicates will not be overwritten")
  (let [urls (get-xbrl-zip-urls year form-types (vals ciks))]
    (doseq [url urls]
      (download-file url output-dir))
    (println "all files accounted for. processed url count: " (count urls))))


(defn parse-rss-feed-links-for-year
  "get rss links from index for a given year"
  [year]
  )

