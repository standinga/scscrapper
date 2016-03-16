;; (ns scsrapper.midi
;;   (:import [java.sql SQLException])
;;   (:require [clj-http.client :as client]
;;             [cheshire.core :refer :all]
;;             [clojure.string]
;;             [clojure.edn :as edn]
;;             [scsrapper.inits :refer :all]
;;             [clojure.java.jdbc :as jdbc]
;;             [scsrapper.emails :as emails]
;;             [scsrapper.db :as db]
;;             ))


;; (def midi "/Users/michal/1.mid")

;; (slurp midi)

;; (defn f1 [x]
;;   (do (println "started f1") (Thread/sleep 4000) (println "done f1 " x) x))

;; (defn f2 [y]
;;   (do (println "started f2") (Thread/sleep 5000) (println "done f2 " y) y))



;; (time (do (f1 1) (f2 2)))
;; (time (pmap #(%) [f1 f2]))
;; (time (pvalues (f1 1) (f2 2)))

;; (def letters (mapv (comp str char (partial + 65)) (range 26)))
;; (defn random-string [n]
;;   (apply str (take n (repeatedly #(rand-nth letters)))))

;; (defn rand-string [ll sl]
;;   (doall (take ll (repeatedly (partial random-string sl)))))



;; (def llist (rand-string 2000 10000))

;; (time (dorun (map clojure.string/lower-case llist)))

;; (time (dorun (map sort llist)))

;; (time (dorun (pmap sort llist)))

;; (time (doall (pmap (fn [x] (doall (pmap sort x))) (partition-all (/ (count llist) 16) llist))))

;; (defn multia
;;   ([x] (multia x "zero"))
;;   ([x y]
;;    (println x y)

;;    ))

;; (def v ["a" "b"])
;; (def vv [["a" "b"]["c" "d"]["e" "f"]["g" "h"]["i" "j"]])
;; (multia "a")
;; (multia "a" "b")
;; (apply multia v)

;;  (map (fn [x] (apply multia x)) vv)
;; (defn retry [x]
;;   (println "future available and the answer is:")
;;   )
;; (defn when-done [future-to-watch function-to-call]
;;           (future (function-to-call @future-to-watch)))

;;          (let [f (future (do (println "started ")(Thread/sleep 2000)) 42)]
;;             (when-done f #(retry %)))
