(ns goodreads.gr-api
  (:require [byte-streams :as bs]
            [aleph.http :as http]
            [clojure.data.xml :as xml]
            [clojure.data.json :as json]))

(defrecord Book [title edition_count subject link authors])

(defn openlib-key->link [key] (str "http://openlibrary.org" key))

(defn flip [f]
  (fn [& args]
    (apply f (reverse args))))

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

(defn http-get->str
  ([url] (-> @(http/get url)
             :body
             bs/to-string))
  ([url params] (-> @(http/get url
                               {:query-params params})
                    :body
                    bs/to-string)))

(defn str->xml [input]
  (let [input-xml (java.io.StringReader. input)]
    (xml/parse input-xml)))

(defn retrieve-user-id [credentials-provider]
  (let [url "https://www.goodreads.com/api/auth_user"
        credentials (credentials-provider :GET url)]
    (let [user-info (first (filter (fn [el] (= (:tag el) :user))
                                   (apply vector (:content (str->xml (http-get->str url credentials))))))]
      (get-in user-info [:attrs :id]))))

(defn retrieve-shelf-books-info [shelf credentials-provider target-key]
  "Retrieves books in specified self and returns books info by specified key. Info source - goodreads"
  (defn extract-books-info [resp]
    (map (fn [el] (apply merge (:book el)))
         (map (fn [el] (apply merge (:review el)))
              (:reviews (first
                          (filter (fn [el] (contains? el :reviews))
                                  (:GoodreadsResponse resp)))))))

  (let [user-id (retrieve-user-id credentials-provider)
        list-url (str "https://www.goodreads.com/review/list/" user-id ".xml")
        params {:v 2 :shelf shelf}
        credentials (credentials-provider :GET list-url params)]
    (filter (comp not nil?)
            (map target-key
                 (extract-books-info (xml->json (str->xml (http-get->str list-url (merge params credentials)))))))))

(defn retrieve-shelf-books-ids [shelf credentials-provider] (retrieve-shelf-books-info shelf credentials-provider :id))
(defn retrieve-shelf-books-isbn [shelf credentials-provider] (retrieve-shelf-books-info shelf credentials-provider :isbn))

(def retrieve-read-books-ids (partial retrieve-shelf-books-ids "read"))
(def retrieve-read-books-isbn (partial retrieve-shelf-books-isbn "read"))
(def retrieve-curr-reading-books-ids (partial retrieve-shelf-books-ids "currently-reading"))

(defn retrieve-similar-books [credentials-provider read-book-id]
  "Retrieves similar books by given read books. Info source - goodreads"
  (defn parse-books-info [response]
    (map (fn [book-info]
           (let [book-descr (apply merge (:book book-info))
                 authors (filter (partial (flip contains?) :name) (get-in book-descr [:authors :author]))]
             (assoc-in book-descr [:authors] authors)))
         (:similar_books (first (filter
                                  (fn [x] (contains? x :similar_books))
                                  (:book (first (filter
                                                  (fn [el] (contains? el :book))
                                                  (:GoodreadsResponse response)))))))))
  (let [url (str "https://www.goodreads.com/book/show/" read-book-id ".xml")
        credentials (credentials-provider :GET url)
        params {:key (:oauth_consumer_key credentials)}]
    (parse-books-info (xml->json (str->xml (http-get->str url (merge params credentials)))))))

(defn retrieve-similar-books-isbn [read-book-id credentials-provider]
  (filter (comp not nil?)
          (map :isbn (retrieve-similar-books credentials-provider read-book-id))))

(defn retrieve-book-info-by-isbn [book-isbn]
  (let [isbn-key (str "ISBN:" book-isbn)
        params {:bibkeys isbn-key :format "json" :jscmd "data"}
        book-info-url (str "http://openlibrary.org/api/books")]
    (get (json/read-str (http-get->str book-info-url params)) isbn-key)))

(defn retrieve-book-subjects [book-isbn]
  "Retrieves book subjects by given book ISBN. Info source - openlibrary"
  (let [raw-subjects (get (retrieve-book-info-by-isbn book-isbn) "subjects" [])]
    (map (fn [subj] {:name (get subj "name") :url (get subj "url")}) raw-subjects)))

(defn retrieve-num-of-works-by-subject [subject]
  (let [url (str "https://openlibrary.org/subjects/" (.replace (.toLowerCase subject) " " "_") ".json")
        subj-descr (try
                     (json/read-str (http-get->str url))
                     (catch Exception e (println (str "Can`t find subject info. Subject: " subject " Skipping..."))))]
    (+ (get subj-descr "work_count" 0) (get subj-descr "ebook_count" 0))))

(def get-num-of-books-by-mc-subj
  "Retrieves LOW bound of book count estimation."
  (let [most-common-subjects ["art", "fantasy", "biographies", "science", "recipes", "romance", "religion", "mystery and detective stories",
                              "music", "medicine", "history", "children", "science fiction", "textbook"]]
    (apply + (map retrieve-num-of-works-by-subject most-common-subjects))))

(defn book-info->book [book-info]
  "Extracts relevant info from book-info json to book structure."
  (Book.
    (get book-info "title")
    (get book-info "edition_count" 0)
    (get book-info "subject" [])
    (openlib-key->link (get book-info "key"))
    (let [authors (map (partial (flip get) "Unknown" "name") (get book-info "authors"))]
      (map (fn [name] {:name name}) authors))))

(defn retrieve-books-by-subject [subject max-books]
  (let [subj-descr (json/read-str (http-get->str (str (:url subject) ".json")))
        works_count (+ (get subj-descr "work_count" 0) (get subj-descr "ebook_count" 0))
        limit (if (> works_count max-books)
                max-books
                works_count)
        params {:details "true" :limit limit}]
    (map
      book-info->book
      (get (json/read-str (http-get->str (str (:url subject) ".json") params)) "works"))))
