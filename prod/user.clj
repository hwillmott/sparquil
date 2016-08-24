(ns user
  (:gen-class)
  (:require [clojure.main :refer [repl]]
            [clojure.stacktrace :refer [print-cause-trace]]
            [com.stuartsierra.component :as component]
            [clojure.tools.namespace.repl :refer [refresh disable-reload!]]
            [taoensso.carmine :as carm :refer [wcar]]
            [sparquil.core :refer :all]
            [sparquil.layer :as l]
            [sparquil.sketch :as s]))

(def system nil)

(def config-path (atom nil))

(defn init []
  (alter-var-root #'system
                  (fn [_] (sparquil-system (read-config @config-path)))))

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
  ([config-path-]
   (reset! config-path config-path-)
   (reset)))

(defn load-scene [scene]
  (s/load-scene (:sketch system) scene))

(def s "Alias for load-scene" load-scene)

(defn list-scenes []
  (let [scenes (:scenes (s/get-config (:sketch system)))
        scene-ids (map :id scenes)]
    (println scene-ids)))

(def ls list-scenes)

(def redis-conn {:pool {} :spec {:host "127.0.0.1" :port 6379}}) ; See `wcar` docstring for opts
(defmacro wcar* [& body] `(wcar redis-conn ~@body))

(defn -main [config-path-]
  (reset! config-path config-path-)
  (reset)
  (repl))