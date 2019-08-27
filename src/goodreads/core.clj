(ns goodreads.core
  (:gen-class)
  (:require [clojure.tools.cli :as cli]
            [manifold.deferred :as d]
            [goodreads.gr-api :as ga]
            [clojure.edn :as edn]))

(defn build-recommendations-part1 [config]
  (def read-books (ga/retrieve-read-books-ids (config :token)))
  (def reading-books (vector (ga/retrieve-curr-reading-books-ids (config :token))))
  (def similar-books (ga/select-similar-books read-books (config :token)))

  (d/success-deferred
    (take-last 10 (sort-by (fn [book-info] (edn/read-string (:average_rating book-info)))
                           (filter
                             (fn [book-info] (not (contains? reading-books (:id book-info))))
                             similar-books)))))

(defn build-recommentations [config]
  (def read-books (ga/retrieve-read-books-ids (config :token)))
  (def reading-books (vector (ga/retrieve-curr-reading-books-ids (config :token))))
  (def similar-books (ga/select-similar-books read-books (config :token)))

  (d/success-deferred
    (take-last 10 (sort-by (fn [book-info] (edn/read-string (:average_rating book-info)))
                           (filter
                             (fn [book-info] (not (contains? reading-books (:id book-info))))
                             similar-books)))))

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

(defn book->str [{:keys [title link authors average_rating]}]
  (format "\"%s\" by %s\nMore: %s\nAverage rating: %s"
          title
          (->> authors
               (map :name)
               (clojure.string/join ", "))
          link
          average_rating))

(defn -main [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (contains? options :help) (do (println summary) (System/exit 0))
      (some? errors) (do (println errors) (System/exit 1))
      (empty? args) (do (println "Please, specify user's token") (System/exit 1))
      :else (let [config {:token (first args)}
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
