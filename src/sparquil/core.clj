(ns sparquil.core
  (:require [clojure.spec :as s]
            [quil.core :as q]
            [quil.middleware :as m]
            [taoensso.carmine :as car :refer (wcar)]))

; TODO: Spec everything
; TODO: Add a proper logging library, get rid of printlns

;; ---- External environment state ----

(def env (atom {}))

(s/fdef valid-env-key?
  :args (s/cat :key string?))

(defn valid-env-key? [key]
  "Returns whether the string key is a valid env key."
  (re-matches #"^env(?:\.[\w-]+)*/[\w-]+$" key))

(defn update-env! [key value]
  "If key is valid, sets that key in env atom to value."
  (if (valid-env-key? key)
    (do (println "Updating env:" key "->" value)
        (swap! env assoc (keyword key) value))
    (println "Ingoring invalid env key:" key)))

;; ---- Redis ----

; TODO: Componentize the Redis connection (and also the Quil sketch)
; TODO: set up a "key mirroring" abstraction? Provide a set of key patternss,
;       it keeps any updates to keys matching those patterns in sync with redis

(def redis-conn {:pool {} :spec {:host "127.0.0.1" :port 6379}}) ; See `wcar` docstring for opts

(defmacro wcar* [& body] `(car/wcar redis-conn ~@body))

(defn handle-env-notification [[type _ chan-name _ :as msg]]
  (println "Recieved env update notification: " msg)
  (when (= type "pmessage")
    (let [env-key (second (re-find #"^__keyspace@0__:(env.*)$" chan-name))]
      (update-env! env-key (wcar* (car/get env-key))))))

(def listener
  (car/with-new-pubsub-listener (:spec redis-conn)
      {"__keyspace@0__:env*" handle-env-notification}
    (car/psubscribe  "__keyspace@0__:env*")))

(defn refresh-env-key!
  "Gets the latest value of key from Redis, updates the env atom with it."
  [key]
  (update-env! key (wcar* (car/get key))))

(defn refresh-env! []
  "Gets the latest values of all env* keys from Redis, updates the env atom with them."
  (let [env-keys (wcar* (car/keys "env*"))]
    (dorun (map #(refresh-env-key! %) env-keys))))

;; ---- Quil ----

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; Get all existing env keys from redis
  (refresh-env!)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

(s/fdef parse-number
  :args (s/cat :s (s/nilable string?))
  :ret (s/nilable number?))
; From http://stackoverflow.com/a/12285023/1028969
(defn parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (when s
    (if (re-find #"^-?\d+\.?\d*$" s)
      (read-string s))))

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (or (parse-number (:env/color @env))
              (mod (+ (:color state) 0.7) 255))
   :angle (+ (:angle state) 0.1)})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  (q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  (let [angle (:angle state)
        x (* 150 (q/cos angle))
        y (* 150 (q/sin angle))]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      ; Draw the circle.
      (q/ellipse x y 100 100))))

(q/defsketch sparquil
  :title "You spin my circle right round"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
