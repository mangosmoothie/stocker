(defproject stocker "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-http "2.0.0"]
                 [org.clojure/tools.cli "0.3.3"]
                 [clojure-csv "2.0.1"]
                 [org.clojure/data.json "0.2.6"]
                 [medley "0.7.4"]
                 [org.postgresql/postgresql "9.4.1208"]
                 [org.clojure/java.jdbc "0.6.0-alpha2"]]
  :profiles {:dev {:dependencies [[ring/ring-devel "1.4.0"]]}}
  :main stocker.core
  :aot [stocker.core])
