(ns sparquil.layer
  (:require [clojure.spec :as spec]
            [clojure.core.match :refer [match]]
            [bigml.sampling.simple :as simple]
            [quil.core :as q]))

(defn color-mode
  "Sets the color mode to whatever is appropriate for color."
  [color]
  (match color
    (brightness :guard number?) (q/color-mode :rgb 255)
    [brightness]                (q/color-mode :rgb 255)
    [(:or :rgb "rgb") _ _ _]    (q/color-mode :rgb 255)
    [(:or :hsb "hsb") _ _ _]    (q/color-mode :hsb 360 100 100)))

(defn color-apply
  "Calls f, a Quil function that takes params like fill, with color"
  [f color]
  (match color
    (brightness :guard number?) (f brightness)
    [brightness]                (f brightness)
    [(:or :rgb "rgb") r g b]    (f r g b)
    [(:or :hsb "hsb") h s v]    (f h s v)))

(defn color-wrap [quil-fn]
  "Wraps a Quil function that takes params like fill so that it can
  take Sparquil colors."
  (fn [color]
    (color-mode color)
    (color-apply quil-fn color)))

(def background
  "Like Quil's background, but takes Sparquil colors"
  (color-wrap q/background))

(def fill
  "Like Quil's fill, but takes Sparquil colors"
  (color-wrap q/fill))

(def stroke
  "Like Quil's stroke, but takes Sparquil colors"
  (color-wrap q/stroke))

(defn region-background
  "Like Quil's background, but sets the background only for the region specified by bounds
  to color"
  [[_ _ width height :as bounds] color]
  (fill color)
  (stroke color)
  (q/rect 0 0 width height))

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

(defn cellwise-grid-init [new-cell rows cols]
  "Given new-cell, a function of no args that returns a new cell value, return a grid of size
  [rows cols] filled by repeated calls to the new-cell function"
  (into [] (repeatedly rows
                       (fn []
                         (into []
                               (repeatedly cols new-cell))))))

(defn coord-seq [rows cols]
  (for [i (range rows)
        j (range cols)]
    [i j]))

(defn rainbow-orbit [[_ _ width height :as bounds]]
  "A disk rotating around the center of the sketch, hue cylcing around
  the color wheel."
  {:setup
   (fn [env]
     (q/frame-rate 120)
     {:hue   0
      :angle 0})

   :update
   (fn [env state]
     {:hue   (or (parse-number (:env/color env))
                 (mod (+ (:hue state) 0.1) 360))
      :angle (+ (:angle state) 0.0125)})

   :draw
   (fn [state]
     (fill [:hsb (:hue state) 100 100])
     ; Calculate x and y coordinates of the circle.
     (let [angle (:angle state)
           x (* (/ width 2.5) (q/cos angle))
           y (* (/ height 2.5) (q/sin angle))]
       ; Move origin point to the center of the sketch.
       (q/with-translation [(/ width 2)
                            (/ height 2)]
                           ; Draw the circle.
                           (q/ellipse x y 100 100))))})

(defn gradient
  "gradient background for testing"
  [[x y width height]] 
   {:draw 
    (fn [_]  
     (let [x-interval (/ width 360)]  
                      (doseq [h (range 360)]
                        (fill [:hsb h 50 50])  
                        (stroke [:hsb h 50 50 ])
                        (q/rect (* h x-interval) 0 x-interval height))))})

(defn beacon
  "low light beacon visualization"
  [[x y width height] {:keys [center-x center-y interval offset max-diameter color stroke-width]}]
  (let [center-x (or center-x (+ (/ width 2) x))
        center-y (or center-y (+ (/ height 2) y))
        interval (or interval 200)
        offset (or offset 0)
        max-diameter (or max-diameter (max width height))
        color (or color [:hsb 115 50 50])
        stroke-width (or stroke-width 20)]
    {:setup
       (fn [_]
         {:diameter 0})

     :update
       (fn [{:keys [:env/time]} state]
         {:diameter (q/map-range (mod (- time offset) interval) 0 interval 0 max-diameter)})

     :draw
       (fn [state]
         (q/no-fill)
         (stroke color)
         (q/stroke-weight stroke-width)
         (q/ellipse center-x center-y (:diameter state) (:diameter state)))}))

(defn twinkle
  "pulsing twinkle"
  [bounds width height cols rows step-interval]
  (let [cell-x (/ width cols)
        cell-y (/ height rows)]
    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time
        :grid (cellwise-grid-init #(rand 5) cols rows)})

     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time grid] :as state}]
       (if (< time (+ last-step-time step-interval))
         state
         {:last-step-time time
          :grid (let [cells (apply concat (:grid state))]
                  (mapv vec (partition cols
                              (mapv (partial + 0.5) cells))))}))

     :draw
     (fn [state]
       (q/no-stroke)
       (doseq [[i j] (coord-seq rows cols)]
         (let [brightness (q/map-range (q/noise (get-in (:grid state) [i j])) 0 1 0 40)]
           (fill [:hsb 115 40 brightness])
           (q/rect (* i cell-x) (* j cell-y) cell-x cell-y))))}))

(defn text [_ text {:keys [color offset] :or {color [0] offset [0 0]}}]
  "A layer that writes text in color at offerset. Defaults to black at [0 0]"
  {:draw (fn [_]
           (fill color)
           (apply q/text text offset))})

(defn fill-bounds [bounds color]
  "A layer that fills the region specified by bounds with color"
  {:draw (fn [_] (region-background bounds color))})

; Note that the way you specify grid (matrix) positions is different than
; the way you typically specify pixel positions. Grid coordinates
; throughout are i (row), j (col), while pixels are x (col), y (row).

(defn dims [grid]
  "Return the dims of grid as a vector of [rows cols]"
  [(count grid) (count (first grid))])

(defn moore-neighbor-coords
  "Return a coll of the coords of the neighbors of the cell with coords (i, j) in a grid
  of dimensions (rows, cols)."
  [[rows cols] [i j]]
  (for [i-offset (range -1 2)
        j-offset (range -1 2)
        :when (not= [i-offset j-offset] [0 0])]
    [(mod (+ i i-offset) rows) (mod (+ j j-offset) cols)]))

(defn moore-neighbors
  "Return a coll of the values of the neighbors of the cell at coords"
  [grid coords]
  (map (partial get-in grid) (moore-neighbor-coords (dims grid) coords)))

(defn coord-seq [rows cols]
  (for [i (range rows)
        j (range cols)]
    [i j]))

(defn pmapv
  "An eager version of pmap. Spawns a thread for _every_ element in coll. Use
   for parallel IO."
  [f & colls]
  (->> colls
       (apply mapv (fn [& args] (future (apply f args))))
       (mapv deref)))

(defn moore-automaton
  "Return a layer that will run a cellular automaton whose cell transitions are a function
  of the cell's Moore neighborhood on a grid of size [rows cols] stepping once per
  step-interval milliseconds.

  The particular cellular automaton is specified by three functions:
  - grid-init: returns an initial grid state as a vector of vectors. See also
               cellwise-grid-init.
  - cell-transition: given a cell value and that cell's Moore neighborhood (its 8 neighbor
                     cell values), return the next value for the cell
  - cell-color: given a cell value, return a color that should fill that cell in the sketch

  Stretches to fit the size of the region specified by bounds."
  [[_ _ width height :as bounds] rows cols step-interval grid-init cell-transition cell-color]
  {:setup
   (fn [{:keys [:env/time]}]
     {:last-step-time time
      :grid (grid-init rows cols)})

   :update
   (fn [{:keys [:env/time]} {:keys [last-step-time grid] :as state}]
     (if (< time (+ last-step-time step-interval))
       state
       {:last-step-time time
        :grid (let [neighbors (partial moore-neighbors grid)
                    cells (apply concat grid)]
                (mapv vec (partition cols
                                     (pmapv cell-transition
                                            cells
                                            (map neighbors (coord-seq rows cols))))))}))

   :draw
   (fn [{:keys [grid]}]
     (let [x-interval (/ width cols)
           y-interval (/ height rows)]
       (doseq [[i j] (coord-seq rows cols)]
         (let [c (cell-color (get-in grid [i j]))]
           (fill c)
           (stroke c)
           (q/rect (* j x-interval) (* i y-interval) x-interval y-interval)))))})


(defn conways-cell []
  (first (simple/sample #{true false}
                        :replace true
                        :weigh {true 0.25 false 0.75})))

(defn conways-cell-transition
  "Given the value of a cell and a coll of the values of its neighbors,
   return the next value for the cell."
  [cell neighbors]
  (let [live-neighbors (count (filter identity neighbors))]
    (if cell
      (cond (< live-neighbors 2) false
            (<= 2 live-neighbors 3) true
            (< 3 live-neighbors) false)
      (if (= 3 live-neighbors) true false))))

(defn conways-cell-color [cell]
  "Given a conway cell, return the color it should be drawn"
  (if cell
    [:rgb 51 204 51]
    [:rgb 0 0 204]))

(defn conways [bounds rows cols step-interval]
  (moore-automaton bounds rows cols step-interval
                   (partial cellwise-grid-init conways-cell)
                   conways-cell-transition
                   conways-cell-color))

(defn brians-brain-cell []
  (first (simple/sample #{:firing :ready}
                        :replace true
                        :weigh {:firing 0.33 :ready 0.67})))

(defn brians-brain-cell-transition
  "Given the value of a cell and a coll of the values of its neighbors,
   return the next value for the cell."
  [cell neighbors]
  (let [firing-neighbors (count (filter #{:firing} neighbors))]
    (match cell
      :firing     :refractory
      :refractory :ready
      :ready      (if (= firing-neighbors 2) :firing :ready))))

(defn brians-brain-cell-color [cell]
  "Given a conway cell, return the color it should be drawn"
  (match cell
    :firing     [:rgb 125 249 255]
    :refractory [:rgb 3 80 150]
    :ready      [:rgb 0 0 0]))

(defn brians-brain [bounds rows cols step-interval]
  (moore-automaton bounds rows cols step-interval
                   (partial cellwise-grid-init brians-brain-cell)
                   brians-brain-cell-transition
                   brians-brain-cell-color))
