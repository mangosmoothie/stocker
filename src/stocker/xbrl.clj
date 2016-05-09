(ns stocker.xbrl
  (:require [stocker.util :refer :all]
            [clojure.xml :as xml]
            [clj-http.client :as client]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [stocker.company :as co])
  (:import (java.util Calendar)
           (java.io File)))

;;(clojure.core/refer 'clojure.core)
;;(clojure.core/refer 'clojure.repl)

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

(defn get-cik
  "grab the cik using the sec quick search"
  [ticker]
  (second (re-find #"CIK=(\d{10})"
                   (str (client/get (format ticker-search-url ticker))))))

(defn gen-cache-filepath
  "generate a filename, local filepath and file"
  [url cache-dir]
  (let [filename (filename-from-url url)
        filepath (str cache-dir File/separator filename)]
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
  "return the rss feed urls for xbrls downloads from the SEC EDGAR system 
  (beg-month & end-month are inclusive)"
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
  [year form-types cik-nums feed-dir]
  (flatten (map #(parse-xbrlrss % form-types cik-nums) (get-xbrlrss-feeds year feed-dir))))

(defn parse-xbrl
  "parse xbrl file at filepath (str) with tag-prefix (ex: \":us-gaap:\")
  
  if no conext: returns nested map of :tag \"CONTEXT\" [vals] for given tag prefix
  
  if context: returns map of :tag [vals] for given tag prefix 
  context is a string (ex: \"FD2015Q1YTD\")"
  ([filepath tag-prefix]
   (into {} 
         (apply merge-with merge
                (map #(assoc-in {} [(% 0) (% 1)] (% 2))
                     (map #(vector (:tag %) (:contextRef (:attrs %)) (:content %))
                          (->> (xml/parse filepath)
                               :content
                               (filter #(and
                                         (.startsWith (str (:tag %)) tag-prefix)
                                         (not (.endsWith (str (:tag %)) "TextBlock"))))))))))
  ([filepath tag-prefix context]
   (into {} 
         (map #(vector (:tag %) (:content %)) 
              (->> (xml/parse filepath) 
                   :content 
                   (filter #(and 
                             (.startsWith (str (:tag %)) tag-prefix)
                             (= context (->> % :attrs :contextRef))
                             (not (.endsWith (str (:tag %)) "TextBlock")))))))))

(defn parse-xbrl-gaap 
  "retrieve gaap items for given context (ex: \"FD2015Q1YTD\")"
  [filepath context]
  (parse-xbrl filepath ":us-gaap:" context))

(defn parse-xbrl-dei
  "retrieve the doc metadata"
  [filename]
  (parse-xbrl filename ":dei:"))

(defn find-entries 
  "filter the map by keys containing a string"
  [a-map contains]
  (select-keys a-map
               (filter #(and (.contains (clojure.string/upper-case (str  %))
                                        (clojure.string/upper-case contains))
                             (not (.endsWith (str %) "TextBlock"))) 
                       (keys a-map))))

(defn remove-current-rss-feed
  "removes the cached rss feed for the current month"
  [dir]
  (let [curryear (.get (Calendar/getInstance) (Calendar/YEAR))
        calmonth (.get (Calendar/getInstance) (Calendar/MONTH))
        currmonth (if (< calmonth 9) (str "0" (inc calmonth)) (str (inc calmonth)))]
    (io/delete-file (str dir File/separator "xbrlrss-" curryear "-" currmonth ".xml"))))

(defn get-report-dirs [cik dir year]
  (filter #(.startsWith (.getName %) (str cik "-" (.substring (str year) 2))) 
          (.listFiles (File. dir))))

;; (defn get-artifacts-for-year
;;   "retrieves artifacts by monthly historical rss feeds for year"
;;   [year]
;;   (println "fetching rss feeds for year: " year)
;;   (println "output dir is: " landingzone)
;;   (println "duplicates will not be overwritten")
;;   (let [ciks (co/get-all)
;;         urls (get-xbrl-zip-urls year form-types (vals (read-ciks working-dir)) feed-cache)]
;;     (write-ciks working-dir ciks)
;;     (doseq [url urls]
;;       (download-file url landingzone))
;;     (println "all files accounted for. processed url count: " (count urls))
;;     (if (= year (.get (Calendar/getInstance) (Calendar/YEAR)))
;;       (do
;;         (println "removing current rss feed cache")
;;         (remove-current-rss-feed feed-cache)))))

(defn parse-rss-feed-links-for-year
  "get rss links from index for a given year"
  [year]
  )
