(ns scsrapper.core
  (:require [clojure.java.io :as io]
            [scsrapper.downloaders :as downloaders])
  (:gen-class :main true))


(defn -main
  [& args]
  (if args
    (cond
      (or (= (first args) "-r") (= (first args) "-range")) (downloaders/downloadRange)

  (println "RRRRR" (first args))
  (println "i need flags...")))




;;   (downloaders/-main))

;; (-main)
