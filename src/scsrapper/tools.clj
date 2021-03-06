(ns scsrapper.tools
  (:import [java.sql SQLException])
  (:require [clj-http.client :as client]
            [cheshire.core :refer :all]
            [clojure.string]
            [clojure.edn :as edn]
            [scsrapper.inits :refer :all]
            [clojure.java.jdbc :as jdbc]
            [scsrapper.emails :as emails]
            [scsrapper.db :as db]
            [clojure.java.io :as io]
            ))


(def baseUrl "http://api.soundcloud.com/users/")

(def followersString "/followers?client_id=af3e5e31e2e63ddad94791906ebddaec&page_size=200")

(def followingsString "/followings?client_id=af3e5e31e2e63ddad94791906ebddaec&page_size=200")

(def userString "?client_id=af3e5e31e2e63ddad94791906ebddaec")

(def gra 23955110)

(def bou 990322)

(def boj 107508374)

(def errorFollowersFileBase (str "/Volumes/ssd/db/errorFollowers"))


(def baseDir "/Volumes/ssd/db/")

(def errorFollowingsFileBase (str "/Volumes/ssd/db/errorFollowings"))

(def f (clojure.java.io/file baseDir))

(def fs (file-seq f))




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
            (let [retry_call (do (Thread/sleep 500) (try (httpCall url) (catch Exception e (println "e"))))]
              (if (= (:status retry_call) 200)
                retry_call
                (recur (inc retry))))))))))

;; (httpCallAndRetry "http://httpstat.us/500")



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


(defn saveToDB [acc userId method]
  (let [rawData (mapv userVector acc)
        userData (mapv #(get % :userData) rawData)
        userIDS (if (= method "followers")
                  (mapv (fn [x] [(get x :id) userId]) rawData)
                  (mapv (fn [x] [userId (get x :id)]) rawData))
        _ (print "*")]
    (try (db/insertIgnoreUsers userData) (catch Exception e (println e)))
    (try (db/insertIgnoreGraph userIDS) (catch Exception e (println e)))))


(defn getUserInfo [userInfo]
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
                (try (db/insertIgnoreUsers [(getUserInfo userInfo)]) (catch Exception e	(do (println e))))
                (if (not= collection [])
                  (saveToDB collection userId "followers"))
                (recur newUrl (inc i))))
            (do
              (db/insertSavedFollower userId)
              (print (str userId "|"))))))
      (db/insert404 userId))))


(defn get_user_id "returns user id from user name" [user_name]
  "takes name of user and returns user id"
  (let [call (httpCall (str "http://api.soundcloud.com/users/" user_name "?client_id=" soundcloud_client_id))
        parsed_call (parse-string (:body call))]
    (get parsed_call "id")))


(defn get_Hrefs [userId]
  (let [userURL (str baseUrl userId followersString)]
    (loop [url userURL i 0 acc []]
      (if (not= url nil)
        (let [newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")]
          (recur newUrl (inc i) (if (not= newUrl nil) (conj acc (subs newUrl 112 119)) acc)))
        acc))))


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


(defn idToUrl [id]
  (str baseUrl id followersString))

(defn names
  "Return the .getName property of a sequence of files"
  [file-s]
  (map #(.getName %) file-s))

(defn only-files
  "Filter a sequence of files/directories by the .isFile property of
  java.io.File"
  [file-s]
  (filter #(.isFile %) file-s))

(defn testQuery [xs]
  (doall (map findBadQuerry (map vec (partition-all 500 xs)))))

(defn followerNames [fnames]
  (filter #(and (> (count %) 14) (= (subs % 0 14) "errorFollowers")) fnames))

(defn followingNames [fnames]
  (filter #(and (> (count %) 14) (= (subs % 0 14) "errorFollowing")) fnames))


(defn readDeleteErrorFile [fname]
  "reads error followers, returns [[id1 url1] [id2 url2]...] or [] if file doesn't exist"
  (with-open [rdr (clojure.java.io/reader (str baseDir fname))]
    (let [textVectors (doall (map #(clojure.string/split % #",") (line-seq rdr)))]
      (do
        (io/delete-file (str baseDir fname))
        (set (map (fn [x] [(read-string (get x 0)) (get x 1)]) textVectors))))))


(defn getErrorFilesNames [fnames]
  (map #(readDeleteErrorFile %) fnames))

(defn reduceEFiles [errorFiles]
  (reduce #(into %1 %2) errorFiles))


(defn getErrorFollowers
  "returns seq of [id, url] of followers that were saved in error files
  removes followers that were already dloaded returns empty if there are no files with errors
  also deletes those files"
  [files]
  (try
    (let [idsUrls (try (-> files
                           only-files
                           names
                           followerNames
                           getErrorFilesNames
                           reduceEFiles)
                    (catch Exception e))
          ids (map (fn [x] (get x 0)) idsUrls)
          savedFollowers (db/savedOrNoFollowers ids)]
      (filter (fn [x] (not (contains? savedFollowers (get x 0)))) idsUrls))
    (catch Exception e)))


(defn getErrorFollowings [files]
  (try (-> files
           only-files
           names
           followingNames
           getErrorFilesNames
           reduceEFiles)
    (catch Exception e)))


(defn getProgressFiles []
  (->> "/Volumes/ssd/db/progress/"
      clojure.java.io/file
      file-seq
      only-files
      names
      (filter #(= (first %) \p))
       (map #(clojure.string/split % #"[_.p]"))
       (map (fn [x] [(read-string (x 1)) (read-string (x 2))]))))

(getProgressFiles)



;; (getErrorFollowers fs)
(defn larger [[user1 offset1] [user2 offset2]]
  (if (= user1 user2)
    (if (> offset1 offset2) [user1 offset1] [user1 offset2])
    [user1 offset1]))


(defn t [[_ x y ] [_ a b ]]
  (+ x y a b))



(t [12 1 2] [13 3 4])




;; (larger [1 4] [1 3])



  (reduce  #(into (larger %1 %2) {}) (getProgressFiles))
