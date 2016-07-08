(ns sparquil.core
  (:require [clojure.spec :as spec]
            [org.tobereplaced.mapply :refer [mapply]]
            [com.stuartsierra.component :as component]
            [quil.core :as q]
            [quil.middleware :as m]
            [taoensso.carmine :as carm :refer [wcar]]
            [sparquil.spec]
            [sparquil.opc :as opc]))

; TODO: Spec everything
; TODO: Add a proper logging library, get rid of printlns

;; ---- Redis ----

; TODO: set up a "key mirroring" abstraction? Provide a set of key patternss,
;       it keeps any updates to keys matching those patterns in sync with redis

(defmacro try-redis
  ([default & body]
   `(try ~@body
     (catch Exception e#
       (println (str "Redis command failed. Returning default: " ~default ". "
                     "Exception: " (.getMessage e#)))
       ~default))))

(defprotocol KVStore
  "A protocol for key-value stores like Redis."
  (get-pattern [kv-store pattern]
    "Return a map containing all keys from kv-store starting with prefix")
  (subscribe-pattern [kv-store pattern callback]
    "Upon update to a key matching pattern, calls (callback key new-value).
    Returns a listener which should be closed by callee when listener should
    stop listening."))

(defrecord RedisClient [host port conn]

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
  (get-pattern [redis-client pattern]
    (let [keys (try-redis [] (wcar conn (carm/keys pattern)))]
      (into {} (map (fn [k] {k (wcar conn (carm/get k))})
                    keys))))

  (subscribe-pattern [redis-client pattern callback]
    (let [channel (str "__keyspace@0__:" pattern)]
      (try-redis nil
        (carm/with-new-pubsub-listener (:spec conn)
          {channel (fn [[type _ chan-name _ :as msg]]
                    (when (= type "pmessage")
                      (let [k (second (re-find #"^__keyspace@0__:(.*)$" chan-name))]
                        (callback k (wcar conn (carm/get k))))))}
          (carm/psubscribe channel))))))

(defn new-redis-client [host port]
  (->RedisClient host port nil))

;; ---- External environment state ----

(spec/fdef valid-env-key?
           :args (spec/cat :key string?))

(defn valid-env-key? [key]
  "Returns whether the string key is a valid env key."
  (re-matches #"^env(?:\.[\w-]+)*/[\w-]+$" key))

(defn update-env-cache! [cache key value]
  "If key is valid, sets that key in env atom to value."
  (if (valid-env-key? key)
    (do (println "Updating env:" key "->" value)
        (swap! cache assoc (keyword key) value))
    (println "Ingoring invalid env key:" key)))

(defn env-get
  ([env key]
   (env-get env key nil))
  ([env key not-found]
   (get @(:cache env) key not-found)))

(defn current [env]
  @(:cache env))

(defrecord Env [cache kv-store update-listener]
; TODO: Remove update-listener from Env record. Env shouldn't need to deal with
; kv-store issues like closing listeners. Do it with channels maybe?
  component/Lifecycle
  (start [env]
    (dorun (map #(apply update-env-cache! cache %) (get-pattern kv-store "env*")))
    (assoc env :update-listener (subscribe-pattern kv-store "env*" #(update-env-cache! cache %1 %2))))

  (stop [env]
    (when update-listener
      (carm/close-listener update-listener))
    (assoc env :update-listener nil)))

(defn new-env []
  (map->Env {:cache (atom {})
             :kv-store nil}))

;; ---- Display to LEDs ----

(defprotocol Displayer
  "A protocol for things that are capable of displaying frames"
  (display [displayer frame] "Display a frame"))

(defrecord FadecandyDisplayer [host port connection]
  component/Lifecycle
  (start [displayer]
    (assoc displayer :connection (opc/open-connection host port)))

  (stop [displayer]
    (opc/close-connection connection)
    (dissoc displayer :connection))

  Displayer
  (display [_ frame]
    (opc/push-pixels {0 frame} connection)))

(defn new-fadecandy-displayer [host port]
  (map->FadecandyDisplayer {:host host :port port}))

;; ---- Quil ----

(defn point->pixel [[height width] [x y]]
  (if (and (< x width) (< y height))
    (+ x (* width y))
    nil))

; sketch-setup, sketch-update, and sketch-draw return valid quil
; setup, update, and draw functions. State at the sketch level is
; a vector of the states of the layers)
;
; In a layer, any of setup, update, and draw can be nil. If nil, they
; will be replaced by:
; setup: (constantly nil)
; update: (fn [env state] state) ; indentity for state, ignore env
; draw: (constantly nil)

(defn sketch-setup
  "Returns a top-level setup function that will realize env and call layer
  setup functions."
  [env layer-setup-fns]
  (let [safe-setup-fns (map #(or % (constantly nil)) layer-setup-fns)]
    (fn []
      (let [current-env (current env)]
        (mapv #(% current-env) safe-setup-fns))))) ; TODO: try pmap

(defn sketch-update
  "Returns a top-level update function that will realize env and call layer
  update functions to update state."
  [env layer-update-fns]
  (let [safe-update-fns (map #(or % (fn [env state] state))
                             layer-update-fns)]
    (fn [layer-states]
      (let [current-env (current env)]
        (mapv #(%1 current-env %2) safe-update-fns layer-states))))) ;TODO: Try pmap

(defn sketch-draw [layer-draw-fns display-fn]
  "Returns a top-level draw function that will call each layer's draw
  function with its state."
  (let [safe-draw-fns (map #(or % (constantly nil)) layer-draw-fns)]
    (fn [layer-states]
      (dorun (map #(%1 %2) layer-draw-fns layer-states)) ; TODO: safe-draw-fns
      (q/color-mode :rgb 255)
      (display-fn (q/pixels)))))

(defmulti inflate
  (fn [size shape] (:leds/type shape)))

(defmethod inflate :leds/strip [size {:keys [:leds/count :leds/offset :leds/spacing]}]
  (let [[x-offset y-offset] offset]
    (map (partial point->pixel size)
         (map vector (range x-offset (+ x-offset (* spacing count)) spacing)
                     (repeat y-offset)))))

(defrecord Sketch [opts applet env displayer]

  component/Lifecycle
  (start [sketch]
    ; TODO: Force fun-mode middleware
    (let [{:keys [size layers led-shapes]} opts
          led-pixel-indices (mapcat (partial inflate size) led-shapes)
          display-fn (fn [pixels]
                       (display displayer (map #(aget pixels %) led-pixel-indices))
                       pixels)
          opts (-> opts
                   (assoc :setup (sketch-setup env (map :setup layers)))
                   (assoc :update (sketch-update env (map :update layers)))
                   (assoc :draw (sketch-draw (map :draw layers) display-fn)))]

      (assoc sketch :applet (mapply q/sketch opts))))

  (stop [sketch]
    (. applet exit)
    env))

(defn new-sketch
  "Sketch component constructor. Opts will be passed to quil/sketch. See
   quil/defsketch for documentation of possible options. Do not provide :setup,
   :update, and :draw functions directly in opts. Provide a :layers key with a
   vector of {:setup :update :draw} functions. Must also provide fun-mode
   middleware."
  [opts]
  (if (spec/valid? :sketch/opts opts)
    (map->Sketch {:opts opts})
    (throw (Exception. (str "Invalid sketch options: "
                            (spec/explain-str :sketch/opts opts))))))

(defn subsketch-setup [env]
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Get all existing env keys from redis
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

(spec/fdef parse-number
           :args (spec/cat :s (spec/nilable string?))
           :ret (spec/nilable number?))

; From http://stackoverflow.com/a/12285023/1028969
(defn parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (when s
    (if (re-find #"^-?\d+\.?\d*$" s)
      (read-string s))))

(defn subsketch-update [env state]
  ; Update sketch state by changing circle color and position.
  {:color (or (parse-number (:env/color env))
              (mod (+ (:color state) 0.7) 255))
   :angle (+ (:angle state) 0.1)})

(defn subsketch-draw [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 0)
  ; Set circle color
  (q/color-mode :hsb)
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

(def subsketch {:setup subsketch-setup
                :update subsketch-update
                :draw subsketch-draw})

(def partially-nil-subsketch {:setup (constantly "Grady wuz here")
                              :draw (fn [state]
                                      (q/fill 200 255 255)
                                      (q/text state 10 10))})

(defn full-horizontal-strip [y-offset]
  {:leds/type :leds/strip
   :leds/offset [0 y-offset]
   :leds/spacing (/ 500 36)
   :leds/count 36})

(defn sparquil-system []
  (component/system-map
    :sketch (component/using
              (new-sketch
                {:title "You spin my circle right round"
                 :size [500 500]
                 :layers [subsketch partially-nil-subsketch]
                 :led-shapes [(full-horizontal-strip 200)
                              (full-horizontal-strip 220)
                              (full-horizontal-strip 240)
                              (full-horizontal-strip 260)
                              (full-horizontal-strip 280)]
                 :middleware [m/fun-mode]})
              [:env :displayer])
    :displayer (new-fadecandy-displayer "127.0.0.1" 7890)
    :env (component/using (new-env)
                          [:kv-store])
    :kv-store (new-redis-client "127.0.0.1" 6379)))
