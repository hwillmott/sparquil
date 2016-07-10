(ns sparquil.core
  (:require [clojure.spec :as spec]
            [clojure.core.match :refer [match]]
            [org.tobereplaced.mapply :refer [mapply]]
            [com.stuartsierra.component :as component]
            [quil.core :as q]
            [quil.middleware :as m]
            [taoensso.carmine :as carm :refer [wcar]]
            [sparquil.spec]
            [sparquil.opc :as opc]
            [sparquil.layer :as l]
            [clojure.data.json :as json]))

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
  (merge @(:cache env)
         {:env/time (System/currentTimeMillis)}))

(defrecord Env [cache kv-store]
  component/Lifecycle
  (start [env]
    (dorun (map #(apply update-env-cache! cache %) (get-pattern kv-store "env*")))
    (subscribe-key kv-store ::env-updates "env*" #(update-env-cache! cache %1 %2))
    env)

  (stop [env]
    (unsubscribe-key kv-store ::env-updates)
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

;; ---- Sketch ----

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
      (l/background 0) ; Make default background black instead of grey
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

(defn point->pixel-index
  "Given a size and a point, returns the corresponding index in quil/pixel array"
  [[width height] [x y]]
  (if (and (< x width) (< y height))
    (+ (int x) (* width (int y)))
    nil))

(defmulti inflate
  (fn [size shape] (:leds/type shape)))

(defmethod inflate :leds/strip [size {:keys [:leds/count :leds/offset :leds/spacing]}]
  (let [[x-offset y-offset] offset]
    (map (partial point->pixel-index size)
         (map vector (range x-offset (+ x-offset (* spacing count)) spacing)
                     (repeat y-offset)))))

; TODO: Different led-pixel mapping modes. Average over region rather than single pixel?

(defmethod inflate :leds/stretch-grid [[width height :as size] {:keys [:leds/dimensions]}]
  (let [[rows cols] dimensions
        x-interval (/ width cols)
        y-interval (/ height rows)]
    (for [i (range 0 rows) ; i indexes rows (y-dim)
          j (range 0 cols)] ; j indexes cols  (x-dim)
      (point->pixel-index size
        [(+ (/ x-interval 2) (* j x-interval))
         (+ (/ y-interval 2) (* i y-interval))]))))

(defn resolve-layer-name
  "Returns the value bound to the symbol named by name in the sparquil.layer ns"
  [name]
  (eval (symbol "sparquil.layer" name)))

(defn realize-layer
  "Takes a layer spec, returns a fully realized layer or nil if realization fails.

  Layer specs are either strings that name a layer defined directly in the
  sparquil.layer namespace, or a vector. If a vector, the vector is treated almost
  like a Clojure form, where the first element is a string that names a function in
  the sparquil.clojure namespace and the tail is interpreted as args to that function.

  Layer specs are unrelated core.spec"
  [layer-spec]
  (try
    (match layer-spec
      (name :guard string?) (resolve-layer-name name)
      [name & params]       (apply (resolve-layer-name name) params))
    (catch Exception e
      (println "Unable to realize layer spec:" layer-spec)
      (println "Exception: " (.getMessage e))
      nil)))

(defn read-layers
  "Returns a vector of deserialized layer specs or nil if unable to deserialize"
  [layers-str]
  (when-let [deserialized
             (try
               (json/read-str layers-str :key-fn keyword)
               (catch Exception e
                 (println "Value at 'sketch/layers' is not valid json:" layers-str)))]
    (if (or (vector? deserialized) (string? deserialized))
      deserialized
      (println "Value at 'sketch/layers' deserialized to an invalid type:" deserialized))))


(defn thaw-layers
  "Deserializes a string describing a vector of layer specs into a vector of layers"
  [layers-str]
  (when-let [layer-specs (read-layers layers-str)]
    (filter (complement nil?) (mapv realize-layer layer-specs))))

(defn start-applet [env opts layers display-fn]
  "Creates and returns a new sketch applet"
  (let [applet-opts (-> opts
                        (assoc :setup (sketch-setup env (map :setup layers)))
                        (assoc :update (sketch-update env (map :update layers)))
                        (assoc :draw (sketch-draw (map :draw layers) display-fn)))]
    (mapply q/sketch applet-opts)))

(defn get-layers
  "Returns the thawed layers (not just layer specs) specified by the 'sketch/layer'
  key in the kv-store"
  [kv-store]
  (when-let [layers-str (get-key kv-store :sketch/layers)]
    (thaw-layers layers-str)))

(defrecord Sketch [opts applet env displayer kv-store]

  component/Lifecycle
  (start [sketch]
    ; TODO: Force fun-mode middleware
    (let [{init-layers :layers :keys [size led-shapes]} opts
          layers (or (get-layers kv-store) init-layers)
          led-pixel-indices (mapcat (partial inflate size) led-shapes)
          display-fn (fn [pixels]
                       (display displayer (map #(if (nil? %)
                                                  (q/color 0)
                                                  (aget pixels %))
                                               led-pixel-indices))
                       pixels)]
      (reset! applet (start-applet env opts layers display-fn))
      (subscribe-key kv-store ::layer-updates "sketch/layers"
        (fn [_ new-layer-str]
          (. @applet exit)
          (reset! applet (start-applet env opts
                                       (or (thaw-layers new-layer-str) init-layers)
                                       display-fn))))
      sketch))

  (stop [sketch]
    (. @applet exit)
    (reset! applet nil)
    (unsubscribe-key kv-store ::layer-updates)
    sketch))

(defn new-sketch
  "Sketch component constructor. Opts will be passed to quil/sketch. See
   quil/defsketch for documentation of possible options. Do not provide :setup,
   :update, and :draw functions directly in opts. Provide a :layers key with a
   vector of {:setup :update :draw} functions. Must also provide fun-mode
   middleware."
  [opts]
  (if (spec/valid? :sketch/opts opts)
    (map->Sketch {:applet (atom nil) :opts opts})
    (throw (Exception. (str "Invalid sketch options: "
                            (spec/explain-str :sketch/opts opts))))))

; ---- LED configurations ----

(defn full-horizontal-strip [y-offset]
  {:leds/type :leds/strip
   :leds/offset [0 y-offset]
   :leds/spacing (/ 500 36)
   :leds/count 36})

(defn grid [rows cols]
  {:leds/type :leds/stretch-grid
   :leds/dimensions [rows cols]})

; ---- System definition ----

; TODO: Add a :regions opt that maps names to rectangles on the sketch
;       Uses:
;       - if :layers is a map, it maps region names to layer vectors
;       - LED shapes can use regions to specify offsets, sizes, etc.})

(defn sparquil-system []
  (component/system-map
    :sketch (component/using
              (new-sketch
                {:title "You spin my circle right round"
                 :size [1200 200]
                 :layers [(l/conways 6 36 125)
                          l/rainbow-orbit
                          (l/text "Grady wuz here" {:color [255] :offset [10 20]})]
                 :led-shapes [(grid 6 36)]
                 :middleware [m/fun-mode]})
              [:env :displayer :kv-store])
    :displayer (new-fadecandy-displayer "127.0.0.1" 7890)
    :env (component/using (new-env)
                          [:kv-store])
    :kv-store (new-redis-client "127.0.0.1" 6379)))
