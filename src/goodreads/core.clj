(ns goodreads.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.tools.cli :as cli]
            [goodreads.gr-api :as ga]
            [manifold.deferred :as d]
            [oauth.client :as oauth]))

(defn scalar-mul [v1 v2]
  (reduce + (map (partial reduce *) (map vector v1 v2))))

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

(defn build-recommentations [config]
  (d/success-deferred
    (let [credentials-provider (get-credetials-provider (:key config) (:secret config))]
      (take (:number-books config)
            (sort-by (fn [book-info] (- (edn/read-string (:average_rating book-info))))
                     (filter
                       (fn [book-info]
                         (not (contains? (vector (ga/retrieve-curr-reading-books-ids credentials-provider)) (:id book-info))))
                       (set (mapcat (partial ga/retrieve-similar-books credentials-provider) (ga/retrieve-read-books-ids credentials-provider)))))))))

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
