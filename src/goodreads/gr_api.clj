(ns goodreads.gr-api
  (:require [byte-streams :as bs]
            [aleph.http :as http]
            [clojure.data.xml :as xml]
            [clojure.data.json :as json]))

(defrecord Book [title edition_count subject link])

(defn openlib-key->link [key] (str "http://openlibrary.org" key))

(defn xml->json [element]
  (cond
    (nil? element) nil
    (string? element) element
    (and (map? element) (empty? element)) {}
    (sequential? element) (if (> (count element) 1)
                            (map xml->json element)
                            (xml->json (first element)))
    (map? element) {(:tag element) (xml->json (:content element))}
    :else nil))

(defn http-get->str [url]
  (-> @(http/get url)
      :body
      bs/to-string))

(defn http-get-xml->json [url]
  (def response (http-get->str url))
  (def parsed-xml (let [input-xml (java.io.StringReader. response)]
                    (xml/parse input-xml)))
  (xml->json parsed-xml))

(defn retrieve-shelf-books-info [shelf dev-key target-key]
  "Retrieves books in specified self and returns books info by specified key. Info source - goodreads"
  (defn extract-books-info [resp]
    (map (fn [el] (apply merge (:book el)))
         (map (fn [el] (apply merge (:review el)))
              (:reviews (first
                          (filter (fn [el] (contains? el :reviews))
                                  (:GoodreadsResponse resp)))))))
  (def read-list-url (str "https://www.goodreads.com/review/list/78268448.xml?&v=2&shelf=" shelf "&key=" dev-key))
  (filter (comp not nil?) (map target-key (extract-books-info (http-get-xml->json read-list-url)))))

(defn retrieve-shelf-books-ids [shelf dev-key] (retrieve-shelf-books-info shelf dev-key :id))
(defn retrieve-shelf-books-isbn [shelf dev-key] (retrieve-shelf-books-info shelf dev-key :isbn))

(def retrieve-read-books-ids (partial retrieve-shelf-books-ids "read"))
(def retrieve-read-books-isbn (partial retrieve-shelf-books-isbn "read"))
(def retrieve-curr-reading-books-ids (partial retrieve-shelf-books-ids "currently-reading"))

(defn retrieve-similar-books [read-books-ids dev-key]
  "Retrieves similar books by given read books. Info source - goodreads"
  (defn book-info-url [id] (str "https://www.goodreads.com/book/show/" id ".xml?key=" dev-key))
  (defn parse-books-info [response]
    (map (fn [book-info] (apply merge (:book book-info)))
         (:similar_books (first (filter
                                  (fn [x] (contains? x :similar_books))
                                  (:book (first (filter
                                                  (fn [el] (contains? el :book))
                                                  (:GoodreadsResponse response)))))))))
  (set (mapcat (fn [book-id] (parse-books-info (http-get-xml->json (book-info-url book-id)))) read-books-ids)))

(defn retrieve-similar-books-isbn [read-books dev-key] (filter (comp not nil?) (map :isbn (retrieve-similar-books read-books dev-key))))

(defn retrieve-book-info-by-isbn [book-isbn]
  (def isbn-key (str "ISBN:" book-isbn))
  (def book-info-url (str "http://openlibrary.org/api/books?bibkeys=" isbn-key "&format=json&jscmd=data"))
  (get (json/read-str (http-get->str book-info-url)) isbn-key))

(defn book-info->book [book-info]
  (Book.
    (get book-info "title")
    (get book-info "edition_count" 0)
    (map (fn [subj] (get subj "name")) (get book-info "subjects" []))
    (openlib-key->link (get book-info "key"))))

(defn retrieve-book-subjects [book-isbn]
  "Retrieves book subjects by given book ISBN. Info source - openlibrary"
  (def raw-subjects (get (retrieve-book-info-by-isbn book-isbn) "subjects"))
  (map (fn [subj] {:name (get subj "name") :url (get subj "url")}) raw-subjects))

(defn retrieve-target-subjects [books-read, dev-key]
  "Retrieves books similar to that you`ve already read and finds subjects.
   Returns top 25 (empirical value) most frequent subjects weighted by occ num. Info source - openlibrary
   books-read - List of Goodreads books ids"
  (def subjects (mapcat retrieve-book-subjects (retrieve-similar-books-isbn books-read dev-key)))
  (def top-subj (take 25 (sort-by (fn [el] (- (get el 1))) (apply vector (frequencies subjects)))))
  (map (fn [el] (assoc (first el) :weight (get el 1))) top-subj))

(defn retrieve-books-by-subject [subject]
  (map
    (fn [raw-book]
      (Book.
        (get raw-book "title")
        (get raw-book "edition_count")
        (get raw-book "subject")
        (openlib-key->link (get raw-book "key"))))
    (get (json/read-str (http-get->str (str (:url subject) ".json?details=true"))) "works")))

(defn book->features [target-subjects, book]
  (def book-subjects (set (:subject book)))
  (def subj-vector (apply merge (map
                                  (fn [subject] (let [s-name (:name subject)]
                                                  (hash-map s-name (if (contains? book-subjects s-name) (:weight subject) 0))))
                                  target-subjects)))

  (def sorted-subjects (sort (map :name target-subjects)))
  (hash-map :title (:title book)
            :link (:link book)
            :edition_count (:edition_count book)
            :subject_vector (map (partial get subj-vector) sorted-subjects)))


