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
  (client/get url {:throw-exceptions false :socket-timeout 2000 :conn-timeout 2000}))


(defn user_map
  "converts string ids to key ids"
  [user]
  {:id (user "id"), :username (user "username"),
   :followers_count (user "followers_count"), :country (user "country"), :full_name (user "full_name"),
   :track_count (user "track_count") :plan (user "plan"), :followings_count (user "followings_count"),
   :description (user "description"), :reposts_count (user "reposts_count"),
   :likes_count (user "likes_count"), :permalink (user "permalink")
   })


(defn escapeChars
  "characters for mysql syntax"
  [string]
  (if (> (count string) 40) "NULL"
    (if (= string nil) "NULL"
      (-> string
          (clojure.string/replace "\\" "")
          (clojure.string/replace "'" "\\'")))))


(defn userVector
  "returns vector of user info"
  [userData]
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


(defn httpCallAndRetry
  "http call with choosen url will try to retry"
  [url]
  (let [call100 (try (httpCall url ) (catch Exception e))]
    (if (= (:status call100) 200)
      call100
      (if (= (:status call100) 404)
        (println (str "e404"))
        (loop [retry 1]
          (if (< retry 5)
            (let [retry_call (do (Thread/sleep 300)
                               (try (httpCall url)
                                 (catch Exception e (println e url))))]
              (if (= (:status retry_call) 200)
                retry_call
                (recur (inc retry))))))))))

(get (client/get "http://api.soundcloud.com/users/297?client_id=af3e5e31e2e63ddad94791906ebddaec
" {:throw-exceptions false}) :status)

(httpCallAndRetry "http://api.soundcloud.com/users/297?client_id=af3e5e31e2e63ddad94791906ebddaec
")


(defn existUser? [userId]
  (let [userInfo (httpCallAndRetry (str baseUrl userId userString))]
    (not= userInfo nil)))


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


(defn saveUserInfo [userInfo]
  (try (db/insertIgnoreUsers [(parseUserInfo userInfo)]) (catch Exception e	(do (println e) false))))


(defn saveErrorFollowers
  "save error log in format userid, url (to be able to resume later)"
  [userId url]
  (if (.exists (clojure.java.io/as-file errorFollowersFile))
    (spit errorFollowersFile (str userId "," url "\n") :append true)
    (spit errorFollowersFile (str userId "," url "\n"))))


(defn saveErrorFollowings
  "save error log in format userid, url (to be able to resume later)"
  [userId url]
  (if (.exists (clojure.java.io/as-file errorFollowingsFile))
    (spit errorFollowingsFile (str userId "," url "\n") :append true)
    (spit errorFollowingsFile (str userId "," url "\n"))))


(defn dlFollowers
  "takes id or id and url, downloads all user infos of followers
  adds to grapf pairs user, follows. when succesful inserts user id to savedfollowers
  when encounters db error, saves id, url to error file"
  ([userId] (dlFollowers userId (str baseUrl userId followersString)))
  ([userId userURL]
   (if (existUser? userId)
     (loop [url userURL]
       (if (not= url nil)
         (let [collection (get (parse-string (:body (httpCallAndRetry url)))"collection")
               newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")]
           (if (not= collection [])
             (if (saveFollowersToDB collection userId)
               (recur newUrl)
               (do (println "error saving followers") (saveErrorFollowers userId url) false))
             (do
           (db/insertSavedFollower userId)
           (print (str userId "F"))
           true)))
         (do
           (db/insertSavedFollower userId)
           (print (str userId "F"))
           true)))
     (do
           (db/insert404 userId)
           (print (str userId "e404"))))))

;; (dlFollowers 6)


(defn dlFollowings
  "same as dlFollowers"
  ([userId] (dlFollowings userId (str baseUrl userId followingsString)))
  ([userId userURL]
   (if (existUser? userId)
     (loop [url userURL]
       (if (not= url nil)
         (let [collection (get (parse-string (:body (httpCallAndRetry url)))"collection")
               newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")
;;                _ (println "dl followings" url)
               ]
           (if (not= collection [])
             (if (saveFollowingsToDB collection userId)
               (recur newUrl)
               (do (println "error saving followings") (saveErrorFollowings userId url) false))
             (do
           (db/insertSavedFollowing userId)
           (print (str userId "f"))
           true)
             ))
         (do
           (db/insertSavedFollowing userId)
           (print (str userId "f"))
           true)))
     (do
           (db/insert404 userId)
           (print (str userId "e404"))))))
;; (dlFollowings 3000000)

(defn readErrorFollowers
  "reads error followers, returns [[id1 url1] [id2 url2]...] or [] if file doesn't exist"
  []
  (if (.exists (clojure.java.io/as-file errorFollowersFile))
     (with-open [rdr (clojure.java.io/reader errorFollowersFile)]
       (let [textVectors (doall (map #(clojure.string/split % #",") (line-seq rdr)))]
           (set (map (fn [x] [(read-string (get x 0)) (get x 1)]) textVectors))))
    []))

(defn readErrorFollowings
  "same as readErrorFollowers"
  []
  (if (.exists (clojure.java.io/as-file errorFollowingsFile))
     (with-open [rdr (clojure.java.io/reader errorFollowingsFile)]
       (let [textVectors (doall (map #(clojure.string/split % #",") (line-seq rdr)))]
           (set (map (fn [x] [(read-string (get x 0)) (get x 1)]) textVectors))))
    []))




(defn dloadUserInfo
  "dload and save user info if user doesn't exist update db 404 table "
  [userId]
  (let [userInfo (httpCallAndRetry (str baseUrl userId userString))]
    (if (not= userInfo nil) ;if nil it means error 404
      (if (saveUserInfo userInfo)
        true)
      (do (db/insert404 userId) (println "404")))))


(defn dlMultFollowers [ids]
  (time (doall (pmap (fn [x] (doall (pmap dlFollowers x))) (partition-all (/ (count ids) 16) ids)))))


(defn dlMultFollowings [ids]
  (time (doall (pmap (fn [x] (doall (pmap dlFollowings x))) (partition-all (/ (count ids) 16) ids)))))


(defn dlMultUsers [ids]
  (time (doall (pmap (fn [x] (doall (pmap dloadUserInfo x))) (partition-all (/ (count ids) 16) ids)))))


(defn retryDloadErrors [x]
  (let [followers (readErrorFollowers)
        followings (readErrorFollowings)
        _ (println "retry " followers followings)
        ]
    (do
      (try (io/delete-file errorFollowersFile) (catch Exception e))
      (try (io/delete-file errorFollowingsFile) (catch Exception e))
      (pvalues (doall (pmap (fn [x] (apply dlFollowers x)) followers))
               (doall (pmap (fn [x] (apply dlFollowings x)) followings))))))


(defn when-done [future-to-watch function-to-call]
          (future (function-to-call @future-to-watch)))

;; (dlMultUsers (range 8000000 8000300))
(defn multDload
  "takes multiple ids, filters already downloaded or non existing users and
  download them into db"
  [ids]
  (let [savedFollowers (db/savedOrNoFollowers ids)
        savedfollowings (db/savedOrNoFollowings ids)
        followersToDownload (filter #(not (contains? savedFollowers %)) ids)
        followingssToDownload (filter #(not (contains? savedfollowings %)) ids)
        usersToDownload (set (into followersToDownload followingssToDownload))
        _(println usersToDownload)]

    (time
      (do
         (pvalues (dlMultFollowers followersToDownload)
                   (dlMultFollowings followingssToDownload))
                   (dlMultUsers usersToDownload)
        (retryDloadErrors 1)
        )



;;     (let [f (future (pvalues (dlMultFollowers followersToDownload)
;;                    (dlMultFollowings followingssToDownload)
;;                    (dlMultUsers usersToDownload)) 1)]
;;             (when-done f #(retryDloadErrors %)))


      )))





(defn downloadRange
  "downloads range of users from to"
  []
  (do
    (println "enter ids range: ")
    (let
      [values (map read-string (re-seq #"\w+" (read-line)))
       a (first values)
       b (second values)
       ids (range a b)]
      (multDload ids))))


(defn downloadSelected
  "downloads one or more users"
  []
  (do (println "enter user id or ids: ")
    (let [ids (map read-string (re-seq #"\w+" (read-line)))]
      (multDload ids))))
