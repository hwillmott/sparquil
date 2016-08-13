(ns user
  (:require [clojure.spec :as s]
            [clojure.stacktrace :refer [print-cause-trace]]
            [com.stuartsierra.component :as component]
            [clojure.tools.namespace.repl :refer [refresh disable-reload!]]
            [taoensso.carmine :as carm :refer [wcar]]
            [sparquil.core :refer :all]
            [sparquil.layer :as l]))

(def system nil)

(def config (atom (read-string (slurp "configs/dev.edn"))))

(defn init []
  (alter-var-root #'system
                  (fn [_] (sparquil-system @config))))

(defn start []
  (alter-var-root #'system component/start))

(defn stop []
  (alter-var-root #'system
                  (fn [s] (when s (component/stop s)))))

(defn go []
  (init)
  (start))

(defn reset
  ([]
   (try (do (stop)
            (refresh :after 'user/go))
        (catch Exception e (print-cause-trace e))))
  ([config-path]
   (reset! config (read-string (slurp config-path)))
   (reset)))


(def redis-conn {:pool {} :spec {:host "127.0.0.1" :port 6379}}) ; See `wcar` docstring for opts
(defmacro wcar* [& body] `(wcar redis-conn ~@body))