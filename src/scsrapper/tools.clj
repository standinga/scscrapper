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
            ))


(def baseUrl "http://api.soundcloud.com/users/")

(def followersString "/followers?client_id=af3e5e31e2e63ddad94791906ebddaec&page_size=200")

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


(defn get_Hrefs [userId]
        (let [userURL (str baseUrl userId followersString)]
          (loop [url userURL i 0 acc []]
            (if (not= url nil)
              (let [newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")]
                  (recur newUrl (inc i) (if (not= newUrl nil) (conj acc (subs newUrl 112 119)) acc)))
              acc))))



(def get_Hrefs2 ["2338398" "8636496" "7635152" "6674788" "5707338" "4938371" "4247346" "3095290" "2008521" "1132042"
                 "9282909" "8349869" "7505149" "6732450" "5739072" "4944106" "4187428" "3551644" "2905802" "2292585"
                 "9960501" "9326684" "8691424" "8076363" "7449928" "6797208" "6175300" "5598142" "4956893" "4341010"
                 "3764846" "3150236" "2648651" "2062568" "1705599" "1121166" "9883377" "9161406" "8501116" "7791778"
                 "7076240" "6365651" "5707219" "5096094" "4462061" "3892841" "3267536" "2698320" "2041531" "1282545"
                 "9490418" "8923019" "8392631" "7772114" "7107837" "6341025" "5651375" "5012440" "4336886" "3616914"
                 "2999894" "2353911" "1818398" "1228830" "9510488" "9036595" "8467821" "7826855"
                 "7180400" "6627908" "6009768" "5461260" "4850730" "4303554" "3781718" "3284058" "2829107" "2238845"
                 "1764017" "1288852" "9738462" "9149683" "8626437" "8144618" "7672655" "7283435"
                 "7012519" "6625117" "6233301" "5859087" "5487672" "5160986" "4845265" "4537967" "4208384" "3830442"
                 "3418688" "2989394" "2616677" "2302560" "1927507" "1587384" "1265826"
                 "9972714" "9688578" "9327826" "8911518" "8579000" "8232192" "7843866" "7509360" "7134553" "6840196"
                 "6487510" "6065205" "5677501" "5311390" "5020579" "4676846" "4329952" "3978602" "3576723" "3225437"
                 "2864868" "2541371" "2190112" "1839557" "1531784" "1203527" "9939715"
                 "9684969" "9391947" "9073118" "8739377" "8437706" "8050931" "7747262" "7405644" "7081419" "6737868"
                 "6411213" "6116766" "5769486" "5462034" "5122451" "4792145" "4429854" "4153663" "3768624" "3434058"
                 "3106002" "2719512" "2363477" "1981088" "1590830" "1314272" "1046219" "9899244"
                 "9479904" "8989539" "8666179" "8311449" "7964313" "7513553" "7054231" "6303968" "5651021" "4950762"
                 "3251294" "9703898" "2990329" "7464589" "2179744" "7287548" "2780501" "9168554" "6834296" "4356219"
                 "2841833" "1648311" "7619398" "4640251" "2054669" "1616607" "1197314"  "9443145"
                 "8905659" "8317722" "7803972" "7392428" "6871274" "6334218" "5663344" "4921782" "4181057" "3051572"
                 "1974266" "6998763" "7997367" "2527246"  "1461034"])

(def s (sort (map read-string get_Hrefs2)))

(loop [xs s acc[]]
  (if (nil? (second xs))
    acc
    (recur (rest xs) (conj acc (- (second xs) (first xs))))))
