(ns scsrapper.db
  (:import [java.sql SQLException])
  (:require [clojure.java.jdbc :as jdbc]
            [scsrapper.inits :as inits]
            ))

(def db {:classname "com.mysql.jdbc.Driver"
         :subprotocol "mysql"
         :subname inits/host1
         :user inits/user1
         :password inits/pass1})



(defn retryExecute [query n error]
  (loop [i 0]
    (if (> i n)
      (do (println "failed retryExecute error code: " error) false) ;return false could not retry
      (do (Thread/sleep 1500)
        (println (str  "retry thread no. "(.getId(Thread/currentThread))))
        (let [retry (try (jdbc/execute! db [query]) (catch java.sql.SQLException e))]
          (if (vector? retry) (do (println "retry ok ") true)
              (recur (inc i))))))))


(defn insertIgnoreGraph
  "bulk insert of values [user follows] in vector xs into graph table, ignores duplicates
  returns true if executed else false"
  [xs]
  (let [strings (map #(str "(" (get % 0) "," (get % 1) ")") xs)
        stringCall (reduce #(str %1 "," %2) strings)
        query (str "INSERT IGNORE INTO `graph` (`user`, `follows`) VALUES " stringCall ";")]
    (try
      (jdbc/execute! db [query])
      (catch java.sql.SQLException e
        (retryExecute query 6 (.getErrorCode e))))))


;; (vector? (jdbc/execute! db ["INSERT IGNORE INTO `graph` (`user`, `follows`) VALUES(1, 2),(1, 6),(1, 8),(1, 12);"]))

(defn insertIgnoreUsers
  "bulk insert of values
  [id url fake name country email followers followings tracks likes reposts desl plan]
  in vector xs into users table, ignores duplicates"
  [xs]
  (let [strings (map #(str "(" (get % 0) ",'" (get % 1) "'," (get % 2) ",'" (get % 3) "','" (get % 4)
                           "','" (get % 5) "'," (get % 6) "," (get % 7) "," (get % 8) "," (get % 9)
                           "," (get % 10) "," (get % 11)  ",'" (get % 12) "')") xs)
        stringCall (reduce #(str %1 "," %2) strings)
        query (str
                "INSERT IGNORE INTO `users` (`id`, `url`, `fake`, `name`, `country`, `email`, `followers`, `followings`, `tracks`, `likes`, `reposts`, `desl`, `plan`) VALUES "
                stringCall ";")]
    (try
      (jdbc/execute! db [query])
      (catch java.sql.SQLException e
        (retryExecute query 5 (.getErrorCode e))))))


(defn insertComments
  "bulk insert of comments"
  [xs userId]
    (let [strings (map #(str "(" userId "," (get % 0) ",'" (get % 1) "')") xs)
        stringCall (reduce #(str %1 "," %2) strings)
        query (str
                "INSERT IGNORE INTO `comments` (`user_id`, `track_id`, `comment`) VALUES "
                stringCall ";")]
    (try
      (jdbc/execute! db [query])
      (catch java.sql.SQLException e
        (retryExecute query 5 (.getErrorCode e))))))


(defn insertSavedFollower "insert user whose followers are already saved" [id]
  (let [query (str "INSERT IGNORE INTO `savedfollowers` (`id`) VALUES (" id ");")]
    (jdbc/execute! db [query])))


(defn insertSavedFollowing "insert user whose followings are already saved" [id]
  (let [query (str "INSERT IGNORE INTO `savedfollowings` (`id`) VALUES (" id ");")]
    (jdbc/execute! db [query])))

(defn insert404 "insert non existing user ids" [id]
  (let [query (str "INSERT IGNORE INTO `e404` (`id`) VALUES (" id ");")]
    (jdbc/execute! db [query])))


(defn notSavedFollowings [id]
  (empty? (jdbc/query db [(str "SELECT * FROM `e404` JOIN `savedfollowings` where e404.id="
                               id " or savedfollowings.id=" id ";")])))

(defn notSavedFollowers [id]
  (empty? (jdbc/query db [(str "SELECT * FROM `e404` JOIN `savedfollowers` where e404.id="
                               id " or savedfollowers.id=" id ";")])))

(defn multIdsQuery_AUX
  "auxillary function used by multIdsQuery"
  [table xs]
  (let [strings (map #(str %) xs)
        stringCall (reduce #(str %1 "," %2) strings)
          query (str "SELECT * FROM " table " where id in (" stringCall ");")]
    (set (map #(get % :id) (jdbc/query db [query])))))



(defn multIdsQuery
  "returns set of ids run on selected table splits into multiple bulk queries of max size 800"
  [table xs]
  (let [queries (map (fn [x] (multIdsQuery_AUX table x)) (partition-all 800 xs))]
    (reduce #(into %1 %2) queries)))


(defn savedOrNoFollowers [xs]
  (into (multIdsQuery "savedfollowers" xs) (multIdsQuery "e404" xs)))


(defn savedOrNoFollowings [xs]
  (into (multIdsQuery "savedfollowings" xs) (multIdsQuery "e404" xs)))

(defn savedOrNoUsers [xs]
  (into (multIdsQuery "users" xs) (multIdsQuery "e404" xs)))

(defn followersToDownload [xs]
  (let [ys (savedOrNoFollowers xs)]
  (filter #(not (contains? ys %)) xs)))


(defn followersToDownload [xs]
  (let [ys (savedOrNoFollowers xs)]
  (filter #(not (contains? ys %)) xs)))

(defn followingsToDownload [xs]
  (let [ys (savedOrNoFollowings xs)]
  (filter #(not (contains? ys %)) xs)))

(defn getbigs []
  (->>
   (jdbc/query db ["SELECT id FROM bigs WHERE id NOT IN (SELECT * FROM savedfollowers)"])
   (map :id)))


(defn getQuery [query]
    (->>
   (jdbc/query db [query])
   (map :id)))



;; (getbigs)

;; ({:id 675480} :id)

;; (time (count (savedOrNoUsers (range 1 5000))))
