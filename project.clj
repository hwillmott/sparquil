(defproject sparquil "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.9.0-alpha8"]
                 [org.clojure/algo.generic "0.1.2"]
                 [org.tobereplaced/mapply "1.0.0"]
                 [com.stuartsierra/component "0.3.1"]
                 [com.taoensso/carmine "2.13.1"]
                 [quil "2.4.0"]]
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.3"]
                                  [org.clojure/java.classpath "0.2.0"]
                                  [org.clojure/test.check "0.9.0"]]}})
