(ns scsrapper.emails
  (:require [clojure.string]))


(def badchars (hash-set \/ \? \! \' \; \" \& \) \( \ \: \, \. \  \' \∆ \∞ \▬ \# \% \@ \*))

(def lessbadchars (hash-set \/ \? \! \' \; \" \& \) \( \ \: \  \, \' \∆ \∞ \▬ \# \% \*))

(def numbers (hash-set \0 \1 \2 \3 \4 \5 \6 \7 \8 \9))

(defn isnumber? [chara] (contains? numbers chara))

(defn badchar? [chara] (contains? badchars chara))

(defn lessbadchar? [chara] (contains? lessbadchars chara))

(defn hashfollowers [followers acc]
  (if (empty? followers) acc
    (recur (rest followers) (conj acc (hash-map (first followers) false)))))


(defn nonils [followers]
  (loop [i 0 acc []]
    (if (= i (count followers)) (sort acc)
      (if (= (followers i) nil) (recur  (inc i) acc)
        (recur  (inc i) (conj acc (followers i)))))))


(defn check_dup_followers [hashedfollowers S2]
  (if (empty? S2) hashedfollowers
    (if (= (hashedfollowers (first S2)) nil)
      (recur hashedfollowers (rest S2))

      (let [new_hashedfollowers (assoc hashedfollowers (first S2) true)]
        (recur new_hashedfollowers S2)))))


(defn same_followers [hashedfollowers followers]
  (if (empty? followers) hashedfollowers
    (if (nil? (hashedfollowers (first followers))) (recur hashedfollowers (rest followers))
      (recur (assoc hashedfollowers (first followers) true) (rest followers)))))


(defn extract_mail [description]
  "extract email from string this one is used in main function"
  (reduce conj [] (re-seq #"\S+@\S+|\{(?:\w+, *)+\w+\}@[\w.-]+"
                          description)))

(defn remove_less_bad
  "aux function for nobreaks"
  [string acc]
  (if (< (count string) 7)
    "badaddress"
    (if (= (get string 0) \@)
      (str acc string)
      (if (lessbadchar? (get string 0))
        (recur (subs string 1) "")
        (recur (subs string 1) (str acc (get string 0)))))))


(defn nobreaks [string]
  "leaves only valid email address without booking: contact: etc."
  (remove_less_bad string ""))


(defn check_last_letter [string]
  "keeps checking last letter of address until valid letter, removes . , ; etc from the end of address"
  (let [length (count string)]
    (if (< length 7)
      "bad address"
      (if (or (badchar? (get string (dec length))) (isnumber? (get string (dec length))))
        (recur (subs string 0 (dec length)))
        string))))


(defn check_first_letter [string]
  "removes from the  beginning of email address  , . ( etc. "
  (if (< (count string) 7) "bad address"
    (if (badchar? (get string 0))
      (recur (subs string 1))
      string)))


(defn check_first_last [string]
  "checks address for and illegal characters and cleans beginning and end of address"
  (nobreaks (check_first_letter (check_last_letter string))))


(defn getFirst [stringVector]
  (if (not= stringVector []) (stringVector 0)))


(defn extractEmailFromDescription [description]
  (if (not= description nil)
    (let [ext (extract_mail description)]
      (if (not= ext [])
        (let [extracted (check_first_last (get ext 0))
              cleared (if (and (not= extracted nil) (not= (subs extracted 0 1) "@")) extracted nil)]
          cleared)))))
