(defproject goodreads "0.1.0-SNAPSHOT"
  :description "Books recommendation tool"
  :license {:name "Proprietary"}
  :jvm-opts ^:replace ["-server"
                       "-XX:+UseConcMarkSweepGC"
                       "-Xmx256m"
                       "-XX:-OmitStackTraceInFastThrow"]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [aleph "0.4.4"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.json "0.2.6"]
                 [clj-oauth "1.5.2"]]
  :main ^:skip-aot goodreads.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :plugins [[lein-cljfmt "0.6.4"]])
