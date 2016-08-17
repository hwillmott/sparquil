(defproject sparquil "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.9.0-alpha8"]
                 [org.clojure/clojurescript "1.9.216"]
                 [org.clojure/algo.generic "0.1.2"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.tobereplaced/mapply "1.0.0"]
                 [com.stuartsierra/component "0.3.1"]
                 [com.taoensso/carmine "2.13.1"]
                 [http-kit "2.2.0"]
                 [bidi "2.0.9"]
                 [reagent "0.6.0-rc"]
                 [bigml/sampling "3.0"]
                 [org.clojure/data.json "0.2.6"]
                 [quil "2.4.0"]]
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.3"]
                                  [org.clojure/java.classpath "0.2.0"]
                                  [org.clojure/test.check "0.9.0"]]}
             :uberjar {:aot :all
                       :main ^:skip-aot sparquil.core
                       :target-path "target/%s"}}
  :resource-paths ["resources"]
  :jvm-opts ^:replace []
  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-figwheel "0.5.4-7"]]
  :cljsbuild
    {:builds [{:id "dev"
               :source-paths ["src-cljs"]
               :figwheel {:on-jsload "sparquil.web-client/mount-root"}
               :compiler {:output-to "resources/public/js/app.js"
                          :output-dir "resources/public/js/out"
                          :asset-path "js/out"
                          :main "sparquil.web-client"
                          :optimizations :none
                          :pretty-print true}}
              {:id "prod"
               :source-paths ["src-cljs"]
               :compiler {:output-to "resources/public/js/app.js"
                          :asset-path "js/out"
                          :main "sparquil.web-client"
                          :optimizations :advanced
                          :pretty-print true}}]}
  :figwheel {:css-dirs ["resources/public/css"]})