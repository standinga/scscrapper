(ns scsrapper.core
  (:require [clojure.java.io :as io]
            [scsrapper.downloaders :as downloaders])
  (:gen-class :main true))


(defn -main
  [& args]
  (if args
    (if
      (or (= (first args) "-r") (= (first args) "-range")) (downloaders/downloadRange)
  (println "bad flag" (first args)))
  (downloaders/downloadSelected)))

