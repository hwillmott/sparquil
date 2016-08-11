(ns sparquil.kv-store
  (:require [com.stuartsierra.component :as component]
            [taoensso.carmine :as carm :refer [wcar]]
            [sparquil.spec]))

;; ---- Redis ----

; TODO: set up a "key mirroring" abstraction? Provide a set of key patternss,
;       it keeps any updates to keys matching those patterns in sync with redis

(defmacro try-redis
  ([default & body]
   `(try ~@body
         (catch Exception e#
           (println (str "Redis command failed. Returning default: " ~default ". "
                         "Exception: " (.getMessage e#) "\n"
                         "Failed command: " (quote ~@body)))
           ~default))))

(defprotocol KVStore
  "A protocol for key-value stores like Redis."
  (get-key [kv-store key]
    "Return the value of key in kv-store or nil if it doesn't exist or if the operation fails")
  (get-pattern [kv-store pattern]
    "Return a map containing all keys from kv-store starting with prefix")
  (subscribe-key [kv-store name key-pattern callback]
    "Upon update to a key that matches pattern, calls (callback key new-value).
    Assigns name to listener so it can be closed via that name with unsubscribe-key.
    Returns nil if unable to subscribe listener or the listener otherwise.")
  (unsubscribe-key [kv-store name]
    "Unsubscribe the listener named name. Names are set with calls to subscribe-key."))

(defrecord RedisClient [host port conn open-listeners]

  component/Lifecycle
  (start [redis-client]
    (let [new-conn {:pool {} :spec {:host host :port port}}]
      (try (wcar new-conn (carm/ping))
           (catch Exception e
             (println (str "Ping to Redis (" host ":" port") failed: "
                           (.getMessage e)))))
      (assoc redis-client :conn new-conn)))

  (stop [redis-client]
    (dissoc redis-client :conn))

  KVStore
  (get-key [redis-client key]
    (try-redis nil (wcar conn (carm/get key))))

  (get-pattern [redis-client pattern]
    (let [keys (try-redis [] (wcar conn (carm/keys pattern)))]
      (into {} (map (fn [k] {k (wcar conn (carm/get k))})
                    keys))))

  (subscribe-key [redis-client name key-pattern callback]
    (let [channel (str "__keyspace@0__:" key-pattern)]
      (when-let [listener
                 (try-redis nil
                            (carm/with-new-pubsub-listener (:spec conn)
                                                           {channel (fn [[type _ chan-name _ :as msg]]
                                                                      (when (= type "pmessage")
                                                                        (let [k (second (re-find #"^__keyspace@0__:(.*)$" chan-name))]
                                                                          (callback k (wcar conn (carm/get k))))))}
                                                           (carm/psubscribe channel)))]
        (swap! open-listeners assoc name listener))))

  (unsubscribe-key [redis-client name]
    (swap! open-listeners (fn [listeners]
                            (if-let [listener (listeners name)]
                              (do (carm/close-listener listener)
                                  (dissoc listeners name))
                              (do (println "Failed to unsubscribe listener with name \"" name "\"."
                                           "No open listeners with that name.")
                                  listeners))))))

(defn new-redis-client [host port]
  (->RedisClient host port nil (atom nil)))