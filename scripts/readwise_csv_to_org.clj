#!/usr/bin/env bb
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(def readwise-seq (with-open [reader (io/reader "/Users/elh/Downloads/readwise-data.csv")]
                    (doall
                     (csv/read-csv reader))))


(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map keyword) ;; Drop if you want string keys instead
            repeat)
	  (rest csv-data)))

(def readwisemap (csv-data->maps readwise-seq))



(def book-id (keyword "Amazon Book ID"))
(def book-title (keyword "Book Title"))
(def book-author (keyword "Book Author"))
(def highlighted-at (keyword "Highlighted at"))
(def highlight (keyword "Highlight"))
(def highlight-location (keyword "Location"))
(def highlight-color (keyword "Color"))
(def tags (keyword "Tags"))

;; (def rwt (filter #(> (count ((keyword "Tags") %)) 0) readwisemap))

(def book-key [book-id book-title book-author])

(def book-to-highlights-map (group-by #(select-keys % book-key) readwisemap))

(defn filename-str [k]
  (format  "%s - %s - %s.org" (book-title k) (book-author k) (book-id k)))

(defn org-id-from-record [k] (str "RW-" (book-id k) "-L" (highlight-location k)))

(defn org-prop-tag [k]
  (let [tagstr (tags k)]
    (if (> (count tagstr) 0)
      (format "TAGS:       %s\n" tagstr)
      "")))

(defn org-props-str [k] (format ":PROPERTIES:\n:ID:       %s\n:TIMESTAMP:       %s\n:%s\n:END:"
                                (org-id-from-record k) (subs (highlighted-at k) 0 10) (org-prop-tag k)))

;; (org-props-str (first rwt))
;; (org-props-str (first readwisemap))

(defn title-str [k]
  (format "* %s - %s\n" (book-title k) (book-author k)))

(defn highlight-title-str [l]
  (let [c (count (highlight l))]
    (str (subs (highlight l) 0 (min 50 c)) (if (> c 50) "..."))))

(defn highlight-str [l]
  (format "** [[shell:open 'kindle://book?action=open&asin=%s&location=%s'][%s]]\n%s\n%s\n"
          (book-id l) (highlight-location l) (highlight-title-str l) (org-props-str l) (highlight l)))

(defn highlight-loc-num [r] (bigdec (highlight-location r)))

(defn highlights-str [k] (reduce str (map highlight-str (sort #(compare (highlight-loc-num %1) (highlight-loc-num %2)) (book-to-highlights-map k)))))

(def book-title-keys (sort #(compare (title-str %1) (title-str %2)) (keys book-to-highlights-map)))
(def output (reduce str (map #(str (title-str %) (highlights-str %)) book-title-keys)))

(spit *out* output)
