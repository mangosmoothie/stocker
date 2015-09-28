(ns stocker.core
  (:gen-class)
  (:require [clj-http.client :as client]
            [clojure-csv.core :as csv])
  (:import (java.util Calendar)))

(def mystocks
  ["PUT STOCKS HERE"])

(def indv-stock-query-data-description
  (let [data-desc {:symbol             {:code "s" :orderHint 10 :typeHint "string"}
                   :avgDailyVolume     {:code "a2" :orderHint 20 :typeHint 1000000000}
                   :volume             {:code "v" :orderHint 30 :typeHint 1000000000}
                   :peRatio            {:code "r" :orderHint 40 :typeHint 1.01}
                   :yearHigh           {:code "k" :orderHint 50 :typeHint 1.01}
                   :yearLow            {:code "j" :orderHint 60 :typeHint 1.01}
                   :lastTradePrice     {:code "l1" :orderHint 70 :typeHint 1.01}
                   :openPrice          {:code "o" :orderHint 80 :typeHint 1.01}
                   :previousClosePrice {:code "p" :orderHint 90 :typeHint 1.01}
                   :afterHoursRealtime {:code "c8" :orderHint 100 :typeHint 1.01}}]
    (into (sorted-map-by (fn [key1 key2]
                           (compare (:orderHint (key1 data-desc))
                                    (:orderHint (key2 data-desc)))))
          data-desc)))

(def styles
  {:header    "\033[95m"
   :blue      "\033[94m"
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
  (loop [data stockdata]
    (when (seq data)
      (let [curr (first data)]
        (println (str (:symbol curr) " " (:lastTradePrice curr) " " (format-pct-change (:percentChange curr))))
        (recur (rest data))))))

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
      (str base-options "c8"))))

(defn csv->objects [data-desc csv]
  (reduce
    (fn [result next]
      (conj result
            (coerce->typeHint (zipmap (keys data-desc) next) data-desc)))
    []
    csv))

(defn get-stock-quotes []
  (let [data-desc indv-stock-query-data-description
        portfolio (clojure.string/join "," mystocks)]
    (->> (get-stock-price-csv portfolio (build-options data-desc))
         (csv/parse-csv)
         (csv->objects data-desc)
         (add-pct-change))))

(defn interact [stocks]
  (let [input (read-line) ]
    (print-update (get-stock-quotes))))

(def session
  (let [portfolio (clojure.string/join "," mystocks)]
    (while true
      (println "<return> to poll again")
      (flush)
      (interact portfolio))))

(defn -main [& args]
  (session))

;(defn -main [& args]
;  (let [[opts args banner] (cli args
;  [& args]
;                                ["-h" "--help" "Print this help"
;                                 :default false :flag true])]
;    (when (:help opts)
;      (println banner))))

;class bcolors:
;HEADER = '\033[95m'
;OKBLUE = '\033[94m'
;OKGREEN = '\033[92m'
;WARNING = '\033[93m'
;FAIL = '\033[91m'
;ENDC = '\033[0m'
;BOLD = '\033[1m'
;UNDERLINE = '\033[4m'
;
;
;