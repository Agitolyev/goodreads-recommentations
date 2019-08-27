(ns goodreads.gr-api
  (:require [byte-streams :as bs]
            [aleph.http :as http]
            [clojure.data.xml :as xml]))

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

(defn http-get->json [url]
  (def response (-> @(http/get url)
                    :body
                    bs/to-string))
  (def parsed-xml (let [input-xml (java.io.StringReader. response)]
                    (xml/parse input-xml)))
  (xml->json parsed-xml))

(defn retrieve-shelf-books-ids [shelf dev-key]
  (defn extract-books-info [resp]
    (map (fn [el] (apply merge (:book el)))
         (map (fn [el] (apply merge (:review el)))
              (:reviews (first
                         (filter (fn [el] (contains? el :reviews))
                                 (:GoodreadsResponse resp)))))))
  (def read-list-url (str "https://www.goodreads.com/review/list/78268448.xml?&v=2&shelf=" shelf "&key=" dev-key))
  (map :id (extract-books-info (http-get->json read-list-url))))

(def retrieve-read-books-ids (partial retrieve-shelf-books-ids "read"))

(def retrieve-curr-reading-books-ids (partial retrieve-shelf-books-ids "currently-reading"))

(defn select-similar-books [read-books dev-key]
  (defn book-info-url [id] (str "https://www.goodreads.com/book/show/" id ".json?key=" dev-key))
  (defn parse-books-info [response]
    (map (fn [book-info] (apply merge (:book book-info)))
         (:similar_books (first (filter
                                 (fn [x] (contains? x :similar_books))
                                 (:book (first (filter
                                                (fn [el] (contains? el :book))
                                                (:GoodreadsResponse response)))))))))
  (set (mapcat (fn [book-id] (parse-books-info (http-get->json (book-info-url book-id)))) read-books)))
