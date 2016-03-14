(ns scsrapper.inits)

(def soundcloud_client_id "af3e5e31e2e63ddad94791906ebddaec")

(def htmlText_1 "<!DOCTYPE html><html><body><h1>progress...</h1><p>")

(def htmlText_2 "</p></body></html>")

(def htmlFile "/usr/share/nginx/html/000/scrape.html")

(defn writeHtml [text]
  (spit htmlFile text))

