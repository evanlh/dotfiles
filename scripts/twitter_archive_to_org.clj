#!/usr/bin/env bb
(require '[cheshire.core :as json]
         '[clojure.java.io :as io]
         '[clojure.string :as string]
         '[clojure.set :as set]
         '[clojure.instant :as instant])

;; *********** BEGIN org-mode helper library
(defn org-link [text url]
  (format "[[%s][%s]]" url text))

(defn oprops->str [m]
  (if (= 0 (count m))
    ""
    (apply str (flatten [":PROPERTIES:\n"
                         (map #(format "%s:       %s\n" (string/upper-case (first %)) (second %)) m)
                         ":END:\n"]))))

(defmacro deforgheader [name headerstr]
  `(defn ~name
    ([title body props]
     (format "%s %s\n%s%s\n" ~headerstr title (oprops->str props) body))
    ([title body] (oh1 title body {}))
     ([title] (oh1 title "" {}))))

(deforgheader oh1 "*")
(deforgheader oh2 "**")
(deforgheader oh3 "***")
(deforgheader oh4 "****")
;; ************* END

(def USERNAME "evanlh")
(def TOFILE "/Users/elh/writing/capture/tweets.org")
(def tweets-str (slurp "/Users/elh/Dropbox/Archive/twitter-2023-06-18/data/tweets.js"))
(def tweets-json-str (subs tweets-str 26)) ;; trim the first 26 chars of JS so it's just JSON
(def tweets-json (json/parse-string tweets-json-str))

;; (first tweets-json)
;; (def td (first tweets-json))

(defn select-tweet-data [json]
  (let [m {"id_str" :id
           "created_at" :created_at
           "full_text" :text
           "entities" :entities}]
    (set/rename-keys (select-keys (json "tweet") (keys m)) m)))

(defn org-id-from-record [r] (str "TW-" (r :id)))
;; (def tw (select-tweet-data td))
;; tw

(defn replace-entities [text entities]
  (let [urls (entities "urls")]
    (reduce #(let [source (%2 "url")
                   dest (%2 "expanded_url")]
               (string/replace %1 source dest))
      text urls)))
;; (replace-entities (tw :text) (tw :entities))

(defn org-timestamp-from-date [dt]
  (.format (java.text.SimpleDateFormat. "[YYYY-MM-dd EEE HH:mm]") dt))
;; (org-timestamp-from-date (java.util.Date. (tw :created_at)))

(defn escape-text [text]
  (string/replace (string/replace text "*" "-") "&amp;" "&"))

(defn tweet->url [tw]
  (format "https://twitter.com/%s/status/%s" USERNAME (tw :id)))

(defn normalize-tweet [tw]
  (assoc tw
         :text (escape-text (replace-entities (tw :text) (tw :entities)))
         :created_at (java.util.Date. (tw :created_at))
         :url (tweet->url tw)))

(defn tweet->title [tw]
  (str (org-link (.format (java.text.SimpleDateFormat. "YYYY-MM-dd") (tw :created_at)) (tweet->url tw)) " | "
       (subs (tw :text) 0 (min 80 (count (tw :text)))) (if (< (count (tw :text)) 80) "...")))

(defn tweet->org [tw]
  (oh2
   (tweet->title tw)
   (tw :text)
   {:id (org-id-from-record tw)
    :timestamp (org-timestamp-from-date (tw :created_at))}))

(defn json->tweets [tweets-json]
  (map #(normalize-tweet (select-tweet-data %)) tweets-json))

(def all-tweets-sorted (reverse (sort-by :created_at (json->tweets tweets-json))))

;; find specific tweet based on id
;; (filter #(= (%1 :id) "1339365450904383490") all-tweets-sorted)

(spit TOFILE (apply str (map tweet->org all-tweets-sorted)))
