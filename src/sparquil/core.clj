(ns sparquil.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [taoensso.carmine :as car :refer (wcar)]))

;; ---- External environment state ----

(def env (atom {}))

(defn update-env! [key value]
  (println "Updating env:" key "->" value)
  (swap! env assoc (keyword key) value))

; TODO: Populate with env values on startup (currently waits for update to get)

;; ---- Redis ----
; TODO: Componentize the Redis connection (and also the Quil sketch)

(def redis-conn {:pool {} :spec {:host "127.0.0.1" :port 6379}}) ; See `wcar` docstring for opts

(defmacro wcar* [& body] `(car/wcar redis-conn ~@body))

(defn handle-env-notification [[type _ chan-name _ :as msg]]
  (println "Redis recieved message: " msg)
  (when (= type "pmessage")
    (when-let [key (second (re-find #"^__keyspace@0__:(env(?:\.[\w-]+)*/[\w-]+)$" chan-name))]
      (println "I think I have an update for a key:" key)
      (update-env! key (wcar* (car/get key))))))

(def listener
  (car/with-new-pubsub-listener (:spec redis-conn)
      {"__keyspace@0__:env*" handle-env-notification}
    (car/psubscribe  "__keyspace@0__:env*")))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

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
