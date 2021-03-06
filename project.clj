(defproject scsrapper "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-http "2.1.0"]
                 [hikari-cp "1.6.1"]
                 [cheshire "5.5.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/java.jdbc "0.3.0"]
                 [java-jdbc/dsl "0.1.0"]
                 [mysql/mysql-connector-java "5.1.18"]
                 [com.climate/claypoole "1.1.2"] ;lib for thread pool management
                  ]
  :main ^:skip-aot scsrapper.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
