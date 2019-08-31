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

(defn str->xml [input]
  (let [input-xml (java.io.StringReader. input)]
    (xml/parse input-xml)))

(defn http-get-xml->json [url]
  (let [response (http-get->str url)]
    (xml->json (str->xml response))))

(defn retrieve-shelf-books-info [shelf dev-key target-key]
  "Retrieves books in specified self and returns books info by specified key. Info source - goodreads"
  (defn extract-books-info [resp]
    (map (fn [el] (apply merge (:book el)))
         (map (fn [el] (apply merge (:review el)))
              (:reviews (first
                          (filter (fn [el] (contains? el :reviews))
                                  (:GoodreadsResponse resp)))))))

  (let [read-list-url (str "https://www.goodreads.com/review/list/78268448.xml?&v=2&shelf=" shelf "&key=" dev-key)]
    (filter (comp not nil?) (map target-key (extract-books-info (http-get-xml->json read-list-url))))))

(defn retrieve-shelf-books-ids [shelf dev-key] (retrieve-shelf-books-info shelf dev-key :id))
(defn retrieve-shelf-books-isbn [shelf dev-key] (retrieve-shelf-books-info shelf dev-key :isbn))

(def retrieve-read-books-ids (partial retrieve-shelf-books-ids "read"))
(def retrieve-read-books-isbn (partial retrieve-shelf-books-isbn "read"))
(def retrieve-curr-reading-books-ids (partial retrieve-shelf-books-ids "currently-reading"))

(defn retrieve-similar-books [read-books-ids dev-key]
  "Retrieves similar books by given read books. Info source - goodreads"
  (defn book-info-url [id, dev-key] (str "https://www.goodreads.com/book/show/" id ".xml?key=" dev-key))
  (defn parse-books-info [response]
    (map (fn [book-info] (apply merge (:book book-info)))
         (:similar_books (first (filter
                                  (fn [x] (contains? x :similar_books))
                                  (:book (first (filter
                                                  (fn [el] (contains? el :book))
                                                  (:GoodreadsResponse response)))))))))
  (set (mapcat (fn [book-id] (parse-books-info (http-get-xml->json (book-info-url book-id dev-key)))) read-books-ids)))

(defn retrieve-similar-books-isbn [read-books dev-key] (filter (comp not nil?) (map :isbn (retrieve-similar-books read-books dev-key))))

(defn retrieve-book-info-by-isbn [book-isbn]
  (let [isbn-key (str "ISBN:" book-isbn)
        book-info-url (str "http://openlibrary.org/api/books?bibkeys=" isbn-key "&format=json&jscmd=data")]
    (get (json/read-str (http-get->str book-info-url)) isbn-key)))

(defn book-info->book [book-info]
  (Book.
    (get book-info "title")
    (get book-info "edition_count" 0)
    (map (fn [subj] (get subj "name")) (get book-info "subjects" []))
    (openlib-key->link (get book-info "key"))))

(defn retrieve-book-subjects [book-isbn]
  "Retrieves book subjects by given book ISBN. Info source - openlibrary"
  (let [raw-subjects (get (retrieve-book-info-by-isbn book-isbn) "subjects")]
    (map (fn [subj] {:name (get subj "name") :url (get subj "url")}) raw-subjects)))

(defn retrieve-target-subjects [books-read, dev-key]
  "Retrieves books similar to that you`ve already read and finds subjects.
   Returns top 25 (empirical value) most frequent subjects weighted by occ num. Info source - openlibrary
   books-read - List of Goodreads books ids"
  (let [subjects (mapcat retrieve-book-subjects (retrieve-similar-books-isbn books-read dev-key))]
    (let [top-subjects (take 25 (sort-by (fn [el] (- (second el))) (apply vector (frequencies subjects))))]
      (map (fn [el] (assoc (first el) :weight (second el))) top-subjects))))

(defn retrieve-books-by-subject [subject]
  (map
    book-info->book
    (get (json/read-str (http-get->str (str (:url subject) ".json?details=true"))) "works")))

(defn book->features [target-subjects, book]
  (defn book-subj->feature-map [subj-set, target-subj]
    "Returns book feature map by book subjects set and given weighted target subjects."
    (apply merge (map
                   (fn [subject] (let [s-name (:name subject)]
                                   (hash-map s-name (if (contains? subj-set s-name) (:weight subject) 0))))
                   target-subj)))

  (defn feature-map->ordered-vector [feature-map, ordered-features]
    "Returns feature vectored ordered by ordered-features constructed from given feature-map"
    (map (partial get feature-map) ordered-features))

  (let [subj-set (set (:subject book))]
    (let [feature-map (book-subj->feature-map subj-set target-subjects)
          ordered-subjects (sort (map :name target-subjects))]
      (hash-map :title (:title book)
                :link (:link book)
                :edition_count (:edition_count book)
                :subject_vector (feature-map->ordered-vector feature-map ordered-subjects)))))
