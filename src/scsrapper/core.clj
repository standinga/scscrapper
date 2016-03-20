(ns scsrapper.core
  (:require [clojure.java.io :as io]
            [scsrapper.downloaders :as downloaders])
  (:gen-class :main true))





(defn help []
  (println "Usage:")
  (println "-r or -range: enter id from and id to")
  (println "-e or -followers: retry failed followers")
(println "-f or -followings: retry failed followings")
(println "-h or -help: help") )


(defn -main
  [& args]
  (if args
    (cond
       (or (= (first args) "-r") (= (first args) "-range")) (downloaders/downloadRange)
       (or (= (first args) "-e") (= (first args) "-followers")) (downloaders/retryFollowers)
        (or (= (first args) "-f") (= (first args) "-followings")) (downloaders/retryFollowings)
      (or (= (first args) "-h") (= (first args) "-help")) (help)
     )
  (downloaders/downloadSelected)))



