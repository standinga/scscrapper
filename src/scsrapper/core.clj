(ns scsrapper.core
  (:gen-class)
    (:require [scsrapper.downloaders :as downloaders]))

(defn -main
  [& args]
  (downloaders/-main))

(-main)
