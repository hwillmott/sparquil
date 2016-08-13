(ns sparquil.core
  (:gen-class)
  (:require [clojure.spec :as spec]
            [clojure.core.match :refer [match]]
            [clojure.algo.generic.functor :refer [fmap]]
            [org.tobereplaced.mapply :refer [mapply]]
            [com.stuartsierra.component :as component]
            [quil.core :as q]
            [quil.middleware :as m]
            [sparquil.spec]
            [sparquil.opc :as opc]
            [sparquil.layer :as l]
            [sparquil.kv-store :as kv]
            [sparquil.environment :as env]
            [clojure.data.json :as json]))

; TODO: Spec everything
; TODO: Add a proper logging library, get rid of printlns


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

(defn region-setup
  "Calls the setup fn for each layer in region and returns a vector of the
  resulting states.

  region-setup-fns is a vector of the setup fns for each layer in a region."
  [current-env region-setup-fns]
  (mapv #(% current-env) region-setup-fns)) ; TODO try pmap

(defn sketch-setup
  "Returns a top-level setup function that will realize env and call layer
  setup functions."
  [env region-setup-map]
  (let [safe-setup-fn-map (fmap (partial mapv #(or % (constantly nil)))
                                region-setup-map)]
    (fn []
      (l/background 0) ; Make default background black instead of grey
      (let [current-env (env/current env)]
        (fmap (partial region-setup current-env) safe-setup-fn-map)))))

(defn region-update [current-env layer-update-fns layer-states]
  "Update the states of all the layers for a region.

  layer-update-fns and layer-states hold the update fns and states for each
  layer in a region."
  (mapv #(%1 current-env %2) layer-update-fns layer-states)) ;TODO: Try pmap

(defn sketch-update
  "Returns a top-level update function that will realize env and call layer
  update functions to update state.

  region-update-map is a map from keyword region names to vectors of layer
  update fns"
  [env region-update-map]
  (let [safe-update-map (fmap (partial map #(or % (fn [env state] state)))
                              region-update-map)]
    (fn [region-states]
      (let [current-env (env/current env)]
        (reduce-kv (fn [updated-states region-name layer-update-fns]
                     (assoc updated-states region-name
                       (region-update current-env layer-update-fns (region-states region-name))))
          {} safe-update-map)))))

(defn region-draw
  "Run a vector of draw fns with their states on region.

  layer-draw-fns holds the draw fns for each layer in region."
  [[x y _ _ :as bounds] layer-draw-fns layer-states]
  (q/with-translation [x y]
    (dorun (map #(%1 %2) layer-draw-fns layer-states))))

(defn sketch-draw [regions region-draw-map display-fn]
  "Returns a top-level draw function that will call each layer's draw
  function with its state.

  regions is a map from keyword region names to region specs ([x y width height])

  region-draw-map is a map from keyword region names to vectors of draw fns

  display-fn will be called with the result of (q/pixels) after all layer draw fns
  have executed."
  (let [safe-draw-map (fmap (partial map #(or % (constantly nil)))
                            region-draw-map)]
    (fn [region-states]
      (l/background 0)
      (doseq [{region-name :name bounds :bounds} regions]
        (region-draw bounds (safe-draw-map region-name) (region-states region-name)))
      (q/color-mode :rgb 255)
      (let [pixels (q/pixels)
            led-pixel-indices (display-fn pixels)]
        (doseq [i led-pixel-indices]
          (when i
            (aset-int pixels i (q/color 255 255 255))))
        (q/update-pixels)))))

(defn point->pixel-index
  "Given a size and a point, returns the corresponding index in quil/pixel array"
  [[width height] [x y]]
  (if (and (< 0 x width) (< 0 y height))
    (+ (Math/round (double x)) (* width (Math/round (double y))))
    nil))

(defn degrees->radians [degrees]
  (* degrees (/ Math/PI 180)))

(defmulti inflate
  (fn [size shape] (:leds/type shape)))

(defmethod inflate :leds/strip [size {:keys [:leds/count :leds/offset :leds/length :leds/angle]}]
  (let [[x-offset y-offset] offset]
    (->> (range 0 length (/ length count))
         (map (fn [radius]
                [(+ x-offset (* radius (Math/cos (degrees->radians angle))))
                 (+ y-offset (* radius (Math/sin (degrees->radians angle))))]))
         (map (partial point->pixel-index size)))))

; TODO: Different led-pixel mapping modes. Average over region rather than single pixel?

; TODO: rewrite stretch-grid inflate to take an angle. Can make use of strip inflate.
(defmethod inflate :leds/stretch-grid [[width height :as size] {:keys [:leds/dimensions]}]
  (let [[rows cols] dimensions
        x-interval (/ width cols)
        y-interval (/ height rows)]
    (for [i (range 0 rows) ; i indexes rows (y-dim)
          j (range 0 cols)] ; j indexes cols  (x-dim)
      (point->pixel-index size
        [(+ (/ x-interval 2) (* j x-interval))
         (+ (/ y-interval 2) (* i y-interval))]))))

(defmethod inflate :leds/circle [size {:keys [:leds/center :leds/count :leds/radius :leds/angle]}]
  (let [[x-offset y-offset] center]
    (->> (range 0 360 (/ 360 count))
         (map (partial + angle))
         (map (fn [angle]
                [(+ x-offset (* radius (Math/cos (degrees->radians angle))))
                 (+ y-offset (* radius (Math/sin (degrees->radians angle))))]))
         (map (partial point->pixel-index size)))))

(defmethod inflate :leds/coordinate-sequence [size {:keys [:leds/offset :leds/dims :leds/coords]}]
  (let [max-x (apply max (map first coords))
        max-y (apply max (map second coords))]
    (->> coords
         (map (fn [[x y]] [(* x (/ (first dims) max-x))
                           (* y (/ (second dims) max-y))]))
         (map (fn [[x y]] [(+ x (first offset))
                           (+ y (second offset))]))
         (map (partial point->pixel-index size)))))


(defn resolve-layer-name
  "Returns the value bound to the symbol named by name in the sparquil.layer ns"
  [name]
  (eval (symbol "sparquil.layer" (str name))))

(defn realize-layer
  "Takes a layer spec, returns a fully realized layer or nil if realization fails.

  Layer specs are either strings that name a layer defined directly in the
  sparquil.layer namespace, or a vector. If a vector, the vector is treated almost
  like a Clojure form, where the first element is a string that names a function in
  the sparquil.clojure namespace and the tail is interpreted as args to that function.

  Layer specs are unrelated core.spec"
  [region [layer-name & params :as layer-spec]]
  (try
    (apply (resolve-layer-name layer-name) region params)
    (catch Exception e
      (println "Unable to realize layer spec:" layer-spec)
      (println "Exception: " (.getMessage e))
      nil)))

(defn realize-layers
  [region-map layer-map]
  (reduce-kv (fn [realized-map region-name layer-vec]
               (assoc realized-map region-name
                 (mapv (partial realize-layer
                         (get-in region-map [region-name :bounds]))
                   layer-vec)))
    {} layer-map))

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

(defn get-layers
  "Returns the thawed layers (not just layer specs) specified by the 'sketch/layer'
  key in the kv-store, or nil if key not present or value invalid."
  [kv-store]
  (when-let [layers-str (kv/get-key kv-store :sketch/layers)]
    (thaw-layers layers-str)))

(defn start-applet
  "Creates and returns a new sketch applet"
  ([{:keys [config] :as sketch}]
   (start-applet sketch (get (:scenes config) (:init-scene config))))
  ([{:keys [config env displayer] :as sketch} scene]
   (let [led-layout (get (:led-layouts config) (:led-layout scene))
         layer-map (get (:layer-maps config) (:layer-map scene))
         [width height :as size] (:size config)
         ; TODO: global-top/global-bottom regions?
         regions (into [{:name :global :bounds [0 0 width height]}] (:regions config))
         region-map (zipmap (map :name regions) regions)
         layers (realize-layers region-map layer-map)
         led-pixel-indices (mapcat (partial inflate size) led-layout)
         display-fn (fn [pixels]
                      (display displayer (map #(if (nil? %)
                                                (q/color 0)
                                                (aget pixels %))
                                              led-pixel-indices))
                      led-pixel-indices)]
     (mapply q/sketch {:title (:title config)
                       :size size
                       :middleware [m/fun-mode]
                       :setup (sketch-setup env (fmap (partial map :setup) layers))
                       :update (sketch-update env (fmap (partial map :update) layers))
                       :draw (sketch-draw regions
                                          (fmap (partial map :draw) layers)
                                          display-fn)}))))

(defrecord Sketch [config applet env displayer kv-store]

  component/Lifecycle
  (start [sketch]
    (reset! applet (start-applet sketch))
    sketch)

  (stop [sketch]
    (. @applet exit)
    ; To make sure the applet is totally done exiting before letting other components spin down.
    ; Without this, when FadecandyDisplayer tries to close the fcserver connection, it throws an
    ; exception because the applet still tries to send a frame after the exit call above.
    (Thread/sleep 250)
    (reset! applet nil)
    sketch))

(defn new-sketch
  "Sketch component constructor. Opts will be passed to quil/sketch. See
   quil/defsketch for documentation of possible options. Do not provide :setup,
   :update, and :draw functions directly in config. Provide a :layers key with a
   vector of {:setup :update :draw} functions. Must also provide fun-mode
   middleware."
  [config]
  (if (spec/valid? :sketch/config config)
    (map->Sketch {:applet (atom nil) :config config})
    (throw (Exception. (str "Invalid sketch options: "
                            (spec/explain-str :sketch/config config))))))

; ---- LED configurations ----

(defn grid [rows cols]
  {:leds/type :leds/stretch-grid
   :leds/dimensions [rows cols]})

; ---- System definition ----

; TODO: LED shapes can use regions to specify offsets, sizes, etc.
; TODO: Give an explicit ordering to regions.
;       Maybe make into a map so it can have more params, like :background-color

(defn sparquil-system [{sketch-config :sketch redis-config :redis fc-config :fadecandy :as config}]
  (component/system-map
    :sketch (component/using (new-sketch sketch-config)
              [:env :displayer :kv-store])
    :displayer (new-fadecandy-displayer (:host fc-config) (:port fc-config))
    :env (component/using (env/new-env)
                          [:kv-store])
    :kv-store (kv/new-redis-client (:host redis-config) (:port redis-config))))

(defn -main [config-path]
  (let [config (read-string (slurp config-path))]
    (component/start (sparquil-system config))))
