(ns sparquil.environment
  (:require [clojure.spec :as spec]
            [com.stuartsierra.component :as component]
            [taoensso.carmine :as carm :refer [wcar]]
            [sparquil.spec]
            [sparquil.kv-store :as kv]))

;; ---- External environment state ----

(spec/fdef valid-env-key?
           :args (spec/cat :key string?))

(defn valid-env-key? [key]
  "Returns whether the string key is a valid env key."
  (re-matches #"^env(?:\.[\w-]+)*/[\w-]+$" key))

(defn update-env-cache! [cache key value]
  "If key is valid, sets that key in env atom to value."
  (when (valid-env-key? key) (swap! cache assoc (keyword key) value)))

(defn env-get
  ([env key]
   (env-get env key nil))
  ([env key not-found]
   (get @(:cache env) key not-found)))

(defn current [env]
  (merge @(:cache env)
         {:env/time (System/currentTimeMillis)}))

(defrecord Env [cache kv-store]
  component/Lifecycle
  (start [env]
    (dorun (map #(apply update-env-cache! cache %) (kv/get-pattern kv-store "env*")))
    (kv/subscribe-key kv-store ::env-updates "env*" #(update-env-cache! cache %1 %2))
    env)

  (stop [env]
    (kv/unsubscribe-key kv-store ::env-updates)
    (assoc env :update-listener nil)))

(defn new-env []
  (map->Env {:cache (atom {})
             :kv-store nil}))