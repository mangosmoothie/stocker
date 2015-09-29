(ns stocker.core
  (:gen-class)
  (:require [clj-http.client :as client]
            [clojure-csv.core :as csv]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :refer [print-table]])
  (:import (java.util Calendar)
           (java.text SimpleDateFormat)))

(def indv-stock-query-data-description
  (let [data-desc {:symbol             {:code "s" :orderHint 10 :typeHint "string"}
                   :avgDailyVolume     {:code "a2" :orderHint 20 :typeHint 1000000000}
                   :volume             {:code "v" :orderHint 30 :typeHint 1000000000}
                   :peRatio            {:code "r" :orderHint 40 :typeHint 1.01}
                   :yearHigh           {:code "k" :orderHint 50 :typeHint 1.01}
                   :yearLow            {:code "j" :orderHint 60 :typeHint 1.01}
                   :lastTradePrice     {:code "l1" :orderHint 70 :typeHint 1.01}
                   :openPrice          {:code "o" :orderHint 80 :typeHint 1.01}
                   :previousClosePrice {:code "p" :orderHint 90 :typeHint 1.01}}]
    (into (sorted-map-by (fn [key1 key2]
                           (compare (:orderHint (key1 data-desc))
                                    (:orderHint (key2 data-desc)))))
          data-desc)))

(def styles
  {:blue      "\033[94m"
   :green     "\033[92m"
   :yellow    "\033[93m"
   :red       "\033[91m"
   :bold      "\033[1m"
   :underline "\033[4m"
   :endStyle  "\033[0m"})

(defn add-pct-change [data]
  (map #(assoc % :percentChange
                 (/ (- (:lastTradePrice %) (:previousClosePrice %)) (:previousClosePrice %)))
       data))

(defn format-pct-change [pct-change]
  (let [pct-str (str (:bold styles) (.toString (format "%.2f" (* 100 pct-change))) "%" (:endStyle styles))]
    (if (pos? pct-change)
      (str (:green styles) pct-str)
      (str (:red styles) pct-str))))

(defn print-update [stockdata]
  (print-table [:symbol :lastTradePrice :yearHigh :yearLow :volume :avgDailyVolume :change] (map #(assoc % :change (format-pct-change (:percentChange %))) stockdata)))

(defn scan-for-notable-movement [query-results]
  (reduce
    (fn [processed raw]
      (let [pctchange (:percentChange raw)]
        (if (< 0.05 (Math/abs pctchange))
          (conj processed (vector (:symbol raw) pctchange) )
          processed)))
    []
    query-results))

(defn get-stock-price-csv [stock-symbols options]
  (:body
    (client/get
      "PUT SERVICE HERE")))

(defn match-to-type [input example]
  (if (and (string? input) (.equals "N/A" input))
    nil
    (cond (string? example) (.toString input)
          (integer? example) (Integer/parseInt input)
          (float? example) (Float/parseFloat input))))

(defn coerce->typeHint [data data-desc]
  (reduce
    (fn [resultmap nextkey]
      (assoc resultmap nextkey (match-to-type (nextkey resultmap) (:typeHint (nextkey data-desc)))))
    data
    (keys data-desc)))

(defn build-options [data-description]
  (let [base-options (clojure.string/join (map :code (vals data-description)))]
    (if
      (and (<= 9 (.get (Calendar/getInstance) Calendar/HOUR_OF_DAY) 15)
           (< 1 (.get (Calendar/getInstance) Calendar/DAY_OF_WEEK) 7))
      base-options
      base-options)))                                       ;do something different if it's after hours?

(defn csv->objects [data-desc csv]
  (reduce
    (fn [result next]
      (conj result
            (coerce->typeHint (zipmap (keys data-desc) next) data-desc)))
    []
    csv))

(defn get-stock-quotes [portfolio]
  (let [data-desc indv-stock-query-data-description]
    (->> (get-stock-price-csv portfolio (build-options data-desc))
         (csv/parse-csv)
         (csv->objects data-desc)
         (add-pct-change))))

(defn current-datetime []
  (.format (SimpleDateFormat. "yyyy/MM/dd HH:mm:ss") (.getTime (Calendar/getInstance))))

(defn poll [portfolio]
  (println (str "RETRIEVING QUOTE AT: " (current-datetime)))
  (print-update (get-stock-quotes portfolio))
  (Thread/sleep 300000))                                    ;poll every 5 minutes

(defn session [portfolio]
  (while true
    (poll portfolio)))

(def cli-options
  [["-p" "--portfolio PORTFOLIO" "Your stocks to watch"
    :parse-fn #(clojure.string/split % #",")
    :default ["GOOG" "YHOO"]]
   ["-s" "--super" "super mode"]
   ["-h" "--help"]])

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [options (parse-opts args cli-options)
        portfolio (:portfolio (:options options))]
    (cond
      (:help (:options options)) (exit 0 (:summary options))
      (seq portfolio) (session (:portfolio (:options options)))
      :else (exit 1 ("NEED STOCKS TO POLL ->> OPTION -h FOR HELP")))))
