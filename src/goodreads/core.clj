(ns goodreads.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.tools.cli :as cli]
            [goodreads.gr-api :as ga]
            [manifold.deferred :as d]
            [oauth.client :as oauth]))

(defn get-credetials-provider [key, secret]
  (let [consumer (oauth/make-consumer key
                                      secret
                                      "https://www.goodreads.com/oauth/request_token"
                                      "https://www.goodreads.com/oauth/access_token"
                                      "https://www.goodreads.com/oauth/authorize"
                                      :hmac-sha1)
        request-token (oauth/request-token consumer nil)]
    (do
      (let [url (oauth/user-approval-uri consumer
                                         (:oauth_token request-token))]
        (println (str "Please, visit this URL in your browser to authorize: " url)))
      (println "Please, enter 'y' when authorization approved, or any key to exit.")
      (let [input (read-line)]
        (if (= "y" input)
          (println "Ok, thanks.")
          (System/exit 0)))
      (let [access-token-response (oauth/access-token consumer
                                                      request-token
                                                      nil)]
        (partial oauth/credentials consumer
                 (:oauth_token access-token-response)
                 (:oauth_token_secret access-token-response))))))

(defn build-recommentations-part-1 [config]
  (d/success-deferred
    (let [credentials-provider (get-credetials-provider (:key config) (:secret config))]
      (take (:number-books config)
            (sort-by (fn [book-info] (- (edn/read-string (:average_rating book-info))))
                     (filter
                       (fn [book-info]
                         (not (contains? (vector (ga/retrieve-curr-reading-books-ids credentials-provider)) (:id book-info))))
                       (set (mapcat (partial ga/retrieve-similar-books credentials-provider) (ga/retrieve-read-books-ids credentials-provider)))))))))

(defn scalar-mul [v1 v2]
  (reduce + (map (partial reduce *) (map vector v1 v2))))

(defn retrieve-target-subjects [books-read, credentials-provider]
  "Retrieves books similar to that you`ve already read and finds subjects.
   Returns top 25 (empirical value) most frequent subjects weighted by frequencies * IDF(inverse document frequency) of the topic.
   Info source - openlibrary
   books-read - List of Goodreads books ids"
  (let [total-books-count ga/get-num-of-books-by-mc-subj
        subjects (mapcat ga/retrieve-book-subjects (mapcat (partial (ga/flip ga/retrieve-similar-books-isbn) credentials-provider) books-read))
        weighted-subjects (map (fn [el]
                                 (let [subj-books-count (ga/retrieve-num-of-works-by-subject (:name (first el)))]
                                   (assoc (first el) :weight (* (second el) (Math/log (/ total-books-count (+ 1 subj-books-count)))))))
                               (apply vector (frequencies subjects)))]
    (take 25 (sort-by (fn [el] (- (:weight el))) weighted-subjects))))

(defn book->features [target-subjects, book]
  (defn book-subj->feature-map [subj-set, target-subj]
    "Returns book feature map by book subjects set weighted by TF(term frequency) and given weighted target subjects."
    (let [subj-count (count subj-set)]
      (apply merge (map
                     (fn [subject] (let [s-name (:name subject)]
                                     (hash-map s-name (if (contains? subj-set s-name) (/ 1.0 subj-count) 0))))
                     target-subj))))

  (defn feature-map->ordered-vector [feature-map, ordered-features]
    "Returns feature vectored ordered by ordered-features constructed from given feature-map"
    (map (partial get feature-map) ordered-features))

  (let [subj-set (set (:subject book))]
    (let [feature-map (book-subj->feature-map subj-set target-subjects)
          ordered-subjects (sort (map :name target-subjects))]
      (hash-map :title (:title book)
                :authors (:authors book)
                :link (:link book)
                :edition_count (:edition_count book)
                :subject_vector (feature-map->ordered-vector feature-map ordered-subjects)))))

(defn build-recommentations [config]
  (d/success-deferred
    (let [credentials-provider (get-credetials-provider (:key config) (:secret config))
          target-subjects (retrieve-target-subjects (ga/retrieve-read-books-ids credentials-provider) credentials-provider)]
      (def book->target-features (partial book->features target-subjects))
      (let [target-vector (map :weight (sort-by (fn [subj] (:name subj)) target-subjects))]
        (take (:number-books config) (sort-by
                                       (fn [book] (- (scalar-mul (:subject_vector book) target-vector)))
                                       (map book->target-features
                                            (distinct (mapcat (partial (ga/flip ga/retrieve-books-by-subject) 100) target-subjects)))))))))

(def cli-options [["-t"
                   "--timeout-ms"
                   "Wait before finished"
                   :default 5000
                   :parse-fn #(Integer/parseInt %)]
                  ["-n"
                   "--number-books"
                   "How many books do you want to recommend"
                   :default 10
                   :parse-fn #(Integer/parseInt %)]
                  ["-h" "--help"]])

(defn book->str [{:keys [title link authors]}]
  (format "\"%s\" by %s\nMore: %s"
          title
          (->> authors
               (map :name)
               (clojure.string/join ", "))
          link))

(defn -main [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (contains? options :help) (do (println summary) (System/exit 0))
      (some? errors) (do (println errors) (System/exit 1))
      (empty? args) (do (println "Please, specify user's token") (System/exit 1))
      :else (let [config {:key (first args) :secret (second args) :number-books (:number-books options)}
                  books (-> (build-recommentations config)
                            (d/timeout! (:timeout-ms options) ::timeout)
                            deref)]
              (cond
                (= ::timeout books) (println "Not enough time :(")
                (empty? books) (println "Nothing found, leave me alone :(")
                :else (doseq [[i book] (map-indexed vector books)]
                        (println (str "#" (inc i)))
                        (println (book->str book))
                        (println)))))))
