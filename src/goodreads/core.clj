(ns goodreads.core
  (:gen-class)
  (:require [clojure.tools.cli :as cli]
            [manifold.deferred :as d]
            [goodreads.gr-api :as ga]
            [clojure.edn :as edn]))

(defn flip [f]
  "Flips arguments"
  (fn [& rev-args]
    (apply f (reverse rev-args))))

(defn pow [x n]
  (Math/pow x n))

(def sqr (partial (flip pow) 2))

(defn eucl-norm [v1 v2]
  (Math/sqrt (reduce + (map sqr (map (partial reduce -) (map vector v1 v2))))))

(defn build-recommendations-part1 [config]
  (d/success-deferred
    (take (:number-books config) (sort-by (fn [book-info] (- (edn/read-string (:average_rating book-info))))
                                          (filter
                                            (fn [book-info]
                                              (not (contains? (vector (ga/retrieve-curr-reading-books-ids (:token config))) (:id book-info))))
                                            (ga/retrieve-similar-books (ga/retrieve-read-books-ids (:token config)) (:token config)))))))

(defn build-recommentations [config]
  (d/success-deferred
    (let [target-subjects (ga/retrieve-target-subjects (ga/retrieve-read-books-ids (:token config)) (:token config))]
      (def book->target-features (partial ga/book->features target-subjects))
      (let [target-vector (map :weight (sort-by (fn [subj] (:name subj)) target-subjects))]
        (take (:number-books config) (sort-by
                                       (fn [book] (eucl-norm (:subject_vector book) target-vector))
                                       (map book->target-features
                                            (distinct (mapcat ga/retrieve-books-by-subject target-subjects)))))))))

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
      :else (let [config {:token (first args) :number-books (:number-books options)}
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
