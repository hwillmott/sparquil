(ns sparquil.sketch
  (:require [clojure.spec :as spec]
            [clojure.algo.generic.functor :refer [fmap]]
            [org.tobereplaced.mapply :refer [mapply]]
            [com.stuartsierra.component :as component]
            [quil.core :as q]
            [quil.middleware :as m]
            [sparquil.spec]
            [sparquil.layer :as l]
            [sparquil.environment :as env]
            [sparquil.display :refer [display]]
            [sparquil.util :as u]))

(defmulti inflate
          (fn [size shape] (:leds/type shape)))

(defmethod inflate :leds/strip [size {:keys [:leds/count :leds/offset :leds/length :leds/angle]}]
  (let [[x-offset y-offset] offset]
    (->> (range 0 length (/ length count))
         (map (fn [radius]
                [(+ x-offset (* radius (Math/cos (u/degrees->radians angle))))
                 (+ y-offset (* radius (Math/sin (u/degrees->radians angle))))]))
         (map (partial u/point->pixel-index size)))))

; TODO: Different led-pixel mapping modes. Average over region rather than single pixel?

; TODO: rewrite stretch-grid inflate to take an angle. Can make use of strip inflate.
(defmethod inflate :leds/stretch-grid [[width height :as size] {:keys [:leds/dimensions]}]
  (let [[rows cols] dimensions
        x-interval (/ width cols)
        y-interval (/ height rows)]
    (for [i (range 0 rows) ; i indexes rows (y-dim)
          j (range 0 cols)] ; j indexes cols  (x-dim)
      (u/point->pixel-index size
                            [(+ (/ x-interval 2) (* j x-interval))
                             (+ (/ y-interval 2) (* i y-interval))]))))

(defmethod inflate :leds/circle [size {:keys [:leds/center :leds/count :leds/radius :leds/angle]}]
  (let [[x-offset y-offset] center]
    (->> (range 0 360 (/ 360 count))
         (map (partial + angle))
         (map (fn [angle]
                [(+ x-offset (* radius (Math/cos (u/degrees->radians angle))))
                 (+ y-offset (* radius (Math/sin (u/degrees->radians angle))))]))
         (map (partial u/point->pixel-index size)))))

(defmethod inflate :leds/coordinate-sequence [size {:keys [:leds/offset :leds/dims :leds/coords :leds/coords-path]}]
  (let [coords (if coords-path
                 (read-string (slurp coords-path))
                 coords)
        max-x (apply max (map first coords))
        max-y (apply max (map second coords))]
    (->> coords
         (map (fn [[x y]] [(* x (/ (first dims) max-x))
                           (* y (/ (second dims) max-y))]))
         (map (fn [[x y]] [(+ x (first offset))
                           (+ y (second offset))]))
         (map (partial u/point->pixel-index size)))))

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
  [env scene]
  (let [region-setup-map (fmap (fn [layer-vec]
                                 (mapv (comp #(or % (constantly nil)) :setup)
                                       layer-vec))
                               (:layer-map scene))]
    (fn []
      (l/background 0) ; Make default background black instead of grey
      (let [current-env (env/current env)]
        (fmap (partial region-setup current-env) region-setup-map)))))

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
  [env scene]
  (let [region-update-map (fmap (fn [layer-vec]
                                  (mapv (comp #(or % (fn [env state] state)) :update)
                                        layer-vec))
                                (:layer-map scene))]
    (fn [region-states]
      (let [current-env (env/current env)]
        (reduce-kv (fn [updated-states region-name layer-update-fns]
                     (assoc updated-states region-name
                                           (region-update current-env layer-update-fns (region-states region-name))))
                   {} region-update-map)))))

(defn region-draw
  "Run a vector of draw fns with their states on region.

  layer-draw-fns holds the draw fns for each layer in region."
  [[x y _ _ :as bounds] layer-draw-fns layer-states]
  (q/with-translation [x y] ; TODO: Reset the translation before each layer, not once per frame
                      (dorun (map #(%1 %2) layer-draw-fns layer-states))))

(defn sketch-draw
  "Returns a top-level draw function that will call each layer's draw
  function with its state.

  regions is a map from keyword region names to region specs ([x y width height])

  region-draw-map is a map from keyword region names to vectors of draw fns

  display-fn will be called with the result of (q/pixels) after all layer draw fns
  have executed."
  [config displayer scene]
  (let [region-draw-map (fmap (fn [layer-vec]
                                (mapv (comp #(or % (constantly nil)) :draw)
                                      layer-vec))
                              (:layer-map scene))
        led-pixel-indices (mapcat (partial inflate (:size config)) (:led-layout scene))]
    (fn [region-states]
      (l/background 0)
      (doseq [{region-name :name bounds :bounds} (:regions config)]
        (region-draw bounds (region-draw-map region-name) (region-states region-name)))
      (q/color-mode :rgb 255)
      (let [pixels (q/pixels)]
        (display displayer (map #(if (nil? %)
                                  (q/color 0)
                                  (aget pixels %))
                                led-pixel-indices))
        (doseq [i led-pixel-indices]
          (when i
            (aset-int pixels i (q/color 255 255 255))))
        (q/update-pixels)))))

(defn resolve-layer-symbol
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
  [bounds layer-spec]
  (try
    (let [resolved-spec (mapv #(if (symbol? %) (resolve-layer-symbol %) %)
                              layer-spec)]
      (apply (first resolved-spec) bounds (rest resolved-spec)))
    (catch Exception e
      (println "Unable to realize layer spec:" layer-spec)
      (println "Exception: " (.getMessage e))
      nil)))

(defn realize-layer-map
  [layer-map {:keys [regions] :as config}]
  (let [region-map (zipmap (map :name regions) regions)]
    (into {} (map (fn [[region-name layers]]
                    (let [bounds (get-in region-map [region-name :bounds])]
                      [region-name (mapv (partial realize-layer bounds) layers)]))
                  layer-map))))

(defn realize-scene [config scene-spec]
  (let [scene-map (if (keyword? scene-spec)
                    (get-in config [:scenes scene-spec])
                    scene-spec)]
    (-> scene-map
        (update :led-layout #(get-in config [:led-layouts %]))
        (update :layer-map realize-layer-map config))))

(defn render-scene
  "Creates and returns a new sketch applet"
  ([{:keys [config env displayer] :as sketch} scene-spec]
   (let [scene (realize-scene config scene-spec)]
     (mapply q/sketch {:title (:title config)
                       :size (:size config)
                       :renderer :p2d
                       :middleware [m/fun-mode]
                       :setup (sketch-setup env scene) ;(fmap (partial map :setup) layers))
                       :update (sketch-update env scene)
                       :draw (sketch-draw config displayer scene)}))))

(defn stop-sketch [{:keys [applet] :as sketch}]
  (when @applet
    (.exit @applet)
    ; To make sure the applet is totally done exiting before letting other components spin down.
    ; Without this, when FadecandyDisplayer tries to close the fcserver connection, it throws an
    ; exception because the applet still tries to send a frame after the exit call above.
    (Thread/sleep 250)
    (reset! applet nil)))

(defn load-scene [sketch scene]
  (stop-sketch sketch)
  (reset! (:applet sketch) (render-scene sketch scene))
  nil)

(defrecord Sketch [config applet env displayer kv-store]

  component/Lifecycle
  (start [sketch]
    (load-scene sketch (:init-scene config))
    sketch)

  (stop [sketch]
    (stop-sketch sketch)
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