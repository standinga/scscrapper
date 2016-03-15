(ns scsrapper.downloaders
  (:import [java.sql SQLException])
  (:require [clj-http.client :as client]
            [cheshire.core :refer :all]
            [clojure.string]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [scsrapper.inits :refer :all]
            [clojure.java.jdbc :as jdbc]
            [scsrapper.emails :as emails]
            [scsrapper.db :as db]
            ))


(def baseUrl "http://api.soundcloud.com/users/")

(def followersString "/followers?client_id=af3e5e31e2e63ddad94791906ebddaec&page_size=200")

(def errorFollowersFile "errorFollowers.txt")

(def errorFollowingsFile "errorFollowings.txt")

(def followingsString "/followings?client_id=af3e5e31e2e63ddad94791906ebddaec&page_size=200")

(def userString "?client_id=af3e5e31e2e63ddad94791906ebddaec")

(def gra 23955110)

(def bou 990322)

(def boj 107508374)

(defn httpCall [url]
  (client/get url {:socket-timeout 2000 :conn-timeout 2000}))


(defn user_map "function converts string ids to key ids" [user]
  {:id (user "id"), :username (user "username"),
   :followers_count (user "followers_count"), :country (user "country"), :full_name (user "full_name"),
   :track_count (user "track_count") :plan (user "plan"), :followings_count (user "followings_count"),
   :description (user "description"), :reposts_count (user "reposts_count"),
   :likes_count (user "likes_count"), :permalink (user "permalink")
   })


(defn escapeChars [string]
  (if (> (count string) 40) "NULL"
    (if (= string nil) "NULL"
      (-> string
          (clojure.string/replace "\\" "")
          (clojure.string/replace "'" "\\'")))))


(defn userVector [userData]
  (let [mappedUser (user_map userData)
        id (get mappedUser :id)
        userName (escapeChars (get mappedUser :username))
        countryRuff (if (not= (get mappedUser :country) nil) (get mappedUser :country) "NULL")
        country (escapeChars countryRuff)
        followers (get mappedUser :followers_count)
        followings (get mappedUser :followings_count)
        reposts (get mappedUser :reposts_count)
        likes (get mappedUser :likes_count)
        tracks (get mappedUser :track_count)
        url (get mappedUser :permalink)
        desl (count (get mappedUser :description))
        fake 0
        plan (if (= (get mappedUser :plan) "Pro Unlimited") "U" (if  (= (get mappedUser :plan) "Pro") "P" "F"))
        emailRuff (emails/extractEmailFromDescription (get mappedUser :description))
        email (escapeChars emailRuff)]
    {:id id :userData [id url fake userName country email followers followings tracks 0 0 desl plan]}))


(defn httpCallAndRetry "http call with choosen url will try to retry " [url]
  (let [call100 (try (httpCall url) (catch Exception e))]
    (if (= (:status call100) 200)
      call100
      (if (= (:status call100) 404)
        (println (str "user doesn't exist!!!" url))
        (loop [retry 1]
          (if (< retry 20)
            (let [retry_call (do
                               (Thread/sleep 500)
                               (try (httpCall url)
                                 (catch Exception e (println "e"))))]
              (if (= (:status retry_call) 200)
                retry_call
                (recur (inc retry))))))))))


(defn idToUrl [id]
  (str baseUrl id followersString))


(defn urlCall [urlIn]
  (loop [url urlIn i 0 acc []]
    (if (and (not= url nil) (< i 400000))
      (let [collection (get (parse-string (:body (httpCallAndRetry url)))"collection")
            newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")
            _ (println (str i "  " url "   " (count collection)))]
        (recur newUrl (inc i) (into acc collection)))
      acc)))


(defn saveFollowersToDB [acc userId]
  (let [rawData (mapv userVector acc)
        userData (mapv #(get % :userData) rawData)
        userIDS (mapv (fn [x] [(get x :id) userId]) rawData)
                  _ (print "*")]
    (if (and
        (db/insertIgnoreUsers userData)
        (db/insertIgnoreGraph userIDS)) true)))





(defn saveFollowingsToDB [acc userId]
  (let [rawData (mapv userVector acc)
        userData (mapv #(get % :userData) rawData)
        userIDS (mapv (fn [x] [userId (get x :id)]) rawData) ; changed order in graph comparing to followers
                  _ (print "*")]
    (if (and
        (db/insertIgnoreUsers userData)
        (db/insertIgnoreGraph userIDS)) true)))



(defn parseUserInfo [userInfo]
  (-> (get userInfo :body)
      parse-string
      userVector
      :userData))


(defn dloadAndSave_Followers [userId]
    (let [userInfo (httpCallAndRetry (str baseUrl userId userString))]
      (if (not= userInfo nil) ;if nil it means error 404
        (let [userURL (str baseUrl userId followersString)]
          (loop [url userURL i 0]
            (if (not= url nil)
              (let [collection (get (parse-string (:body (httpCallAndRetry url)))"collection")
                    newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")]
                (do
                  (try (db/insertIgnoreUsers [(parseUserInfo userInfo)]) (catch Exception e	(do (println e))))
                  (if (not= collection [])
                    (saveFollowersToDB collection userId "followers"))
                  (recur newUrl (inc i))))
              (do
                (db/insertSavedFollower userId)
                (print (str userId "|"))))))
        (db/insert404 userId))))



(defn saveUserInfo [userInfo]
  (try (db/insertIgnoreUsers [(parseUserInfo userInfo)]) (catch Exception e	(do (println e) false))))


(defn dloadUser [userId]
  (let [userInfo (httpCallAndRetry (str baseUrl userId userString))]
      (if (not= userInfo nil) ;if nil it means error 404
        (if (saveUserInfo userInfo) true false)
        (do (db/insert404 userId) (println "404")))))



(defn saveErrorFollowers [userId url]
  (if (.exists (clojure.java.io/as-file errorFollowersFile))
    (spit errorFollowersFile (str userId "," url "\n") :append true)
    (spit errorFollowersFile (str userId "," url "\n"))
    ))


(defn saveErrorFollowings [userId url]
  (if (.exists (clojure.java.io/as-file errorFollowingsFile))
    (spit errorFollowingsFile (str userId "," url "\n") :append true)
    (spit errorFollowingsFile (str userId "," url "\n"))
    ))



(defn dlFollowers [userId]
(let [userURL (str baseUrl userId followersString)]
          (loop [url userURL]
            (if (not= url nil)
              (let [collection (get (parse-string (:body (httpCallAndRetry url)))"collection")
                    newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")]
                  (if (not= collection [])
                    (if (saveFollowersToDB collection userId)
                      (recur newUrl)
                      (do (println "error saving followers") (saveErrorFollowers userId url) false))))
              (do
                (db/insertSavedFollower userId)
                (print (str userId "|"))
                true)))))



(defn dlFollowings [userId]
(let [userURL (str baseUrl userId followingsString)]
          (loop [url userURL]
            (if (not= url nil)
              (let [collection (get (parse-string (:body (httpCallAndRetry url)))"collection")
                    newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")]
                  (if (not= collection [])
                    (if (saveFollowingsToDB collection userId)
                      (recur newUrl)
                      (do (println "error saving followings") (saveErrorFollowings userId url) false))))
              (do
                (db/insertSavedFollowing userId)
                (print (str userId "|"))
                true)))))






;; (dlFollowers 19)


(dlFollowings 19)





(defn dloadAndSave_Followings [userId]
    (let [userInfo (httpCallAndRetry (str baseUrl userId userString))]
      (if (not= userInfo nil) ;if nil it means error 404
        (let [userURL (str baseUrl userId followingsString)]
          (loop [url userURL i 0]
            (if (not= url nil)
              (let [collection (get (parse-string (:body (httpCallAndRetry url)))"collection")
                    newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")]
                (do
                  (try (db/insertIgnoreUsers [(parseUserInfo userInfo)]) (catch Exception e	(do (println e))))
                  (if (not= collection [])
                    (saveFollowersToDB collection userId))
                  (recur newUrl (inc i))))
              (do
                (db/insertSavedFollowing userId)
                (print (str userId "|"))))))
        (db/insert404 userId))))


(defn errorNotFound [xs]
  (try
    (db/insertIgnoreUsers (mapv #(get % :userData) (mapv userVector xs)))
    (catch SQLException e#
      nil)))


(defn findBadQuerry
  "this function perforns binary search through vector of queries
  trying inserting then into db
  if there is bad syntax querry it will be returned"
  [xs]
  (println "find bad")
  (if (not= xs [])
    (if (not (errorNotFound xs))
      (let [length (count xs)]
        (if (> length 2)
          (let [half (/ length 2)
                L (subvec xs 0 half)
                R (subvec xs half)]
            (findBadQuerry L)
            (findBadQuerry R))
          (println (get (userVector (get xs 0)) :userData)))))))


(defn testQuery [xs]
  (doall (map findBadQuerry (map vec (partition-all 500 xs)))))


(defn insertToDB [userId]
  (let [userURL (str "http://api.soundcloud.com/users/" userId "/followers?client_id=af3e5e31e2e63ddad94791906ebddaec&page_size=200")
        rawData (mapv userVector (urlCall userURL))
        userData (mapv #(get % :userData) rawData)
        userIDS (mapv (fn [x] [(get x :id) userId]) rawData)]
    (db/insertIgnoreUsers userData)
    (db/insertIgnoreGraph userIDS)))


(defn get_user_id "returns user id from user name" [user_name]
  "takes name of user and returns user id"
  (let [call (httpCall (str "http://api.soundcloud.com/users/" user_name "?client_id=" soundcloud_client_id))
        parsed_call (parse-string (:body call))]
    (get parsed_call "id")))


(defn -main []
  (do
    (println "enter ids range: ")
    (let
      [ids (map read-string (re-seq #"\w+" (read-line)))
       a (first ids)
       b (second ids)
       followers (db/followersToDownload (range a b))
       _ (println "followers " followers)
       followings (db/followingsToDownload (range a b))
       _ (println "followings " followings)]
      (time (doall
              (pmap (fn [x] (doall (pmap dloadAndSave_Followers x))) (partition-all 20 followers)))))))
;;                    (pmap (fn [x] (doall (pmap dloadAndSave_Followings x))) (partition-all 20 (db/followingsToDownload (range a b)))))))))

(defn downloadRange []
  (do
    (println "enter ids range: ")
    (let
      [ids (map read-string (re-seq #"\w+" (read-line)))
       a (first ids)
       b (second ids)
       followers (db/followersToDownload (range a b))
       _ (println "followers " followers)
       followings (db/followingsToDownload (range a b))
       _ (println "followings " followings)]
      (time (doall
              (pmap (fn [x] (doall (pmap dloadAndSave_Followers x))) (partition-all 20 followers)))))))

;;;todo

;; (defn downloadSelected []
;;     (do (println "enter user id or ids: ")
;;     (let [ids (map read-string (re-seq #"\w+" (read-line)))]
;;       (time (doall (map id_sucker ids))))))
