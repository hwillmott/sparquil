(ns user
  (:require [clojure.spec :as s]
            [clojure.stacktrace :refer [print-cause-trace]]
            [com.stuartsierra.component :as component]
            [clojure.tools.namespace.repl :refer [refresh disable-reload!]]
            [taoensso.carmine :as carm :refer [wcar]]
            [sparquil.core :refer :all]))

(def system nil)

(defn init []
  (alter-var-root #'system
                  (constantly (sparquil-system))))

(defn start []
  (alter-var-root #'system component/start))

(defn stop []
  (alter-var-root #'system
                  (fn [s] (when s (component/stop s)))))

(defn go []
  (init)
  (start))

(defn reset []
  (try (do (stop)
           (refresh :after 'user/go))
       (catch Exception e (print-cause-trace e))))
