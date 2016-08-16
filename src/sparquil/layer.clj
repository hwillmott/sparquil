(ns sparquil.layer
  (:require [clojure.spec :as spec]
            [clojure.core.match :refer [match]]
            [bigml.sampling.simple :as simple]
            [quil.core :as q]
            [sparquil.util :as u]))

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

(def color-int
  (color-wrap q/color))

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

(defn draw-star
  "Draws a star centered around x and y"
  [x y inner-radius outer-radius points]
  (let [angle (/ q/TWO-PI points)
        half-angle (/ angle 2)]
    (q/begin-shape)
    (doseq [i (range points)]
      (let [a (* i angle)]
        (q/vertex (+ x (* outer-radius (q/cos a)))
                  (+ y (* outer-radius (q/sin a))))
        (q/vertex (+ x (* inner-radius (q/cos (+ a half-angle))))
                  (+ y (* inner-radius (q/sin (+ a half-angle)))))))
    (q/end-shape :close)))

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

(defn fill-gradient
  "Gradient background for testing. Can set the hue to change vertically or horizontally, with horizontal as the default."
  [[x y width height] {:keys [direction]}] 
  (let [direction (or direction :horizontal)]
   {:draw 
    (fn [_]  
     (let [y-interval (/ height 360)
           x-interval (/ width 360 )]
       (doseq [h (range 360)]
         (fill [:hsb h 50 50])  
         (stroke [:hsb h 50 50 ])
         (cond
           (= direction :horizontal) (q/rect 0 (* h y-interval) width y-interval)
           (= direction :vertical) (q/rect (* h x-interval) 0 x-interval height)))))}))

(defn beacon
  "Low light beacon visualization centered around center-x and center-y. The beacon expands from 0 to max-diameter over the specified interval. You can specify the color and stroke-width of the beacon."
  [[x y width height] {:keys [center-x center-y interval offset max-diameter color stroke-width]}]
  (let [center-x (or center-x (/ width 2))
        center-y (or center-y (/ height 2))
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
         (cond (< (:diameter state) width) (q/ellipse center-x center-y (:diameter state) (:diameter state))))}))

(defn inverted-beacon
  "Makes the whole visualization dark except for a beacon expanding from center-x and center-y, exposing the layer underneath."
  [[x y width height] {:keys [center-x center-y interval offset max-diameter stroke-width]}]
  (let [center-x (or center-x (/ width 2))
        center-y (or center-y (/ height 2))
        interval (or interval 200)
        offset (or offset 0)
        max-diameter (or max-diameter (max width height))]
    {:setup
     (fn [_]
       {:inner-diameter 0
        :outer-diameter stroke-width})

     :update
     (fn [{:keys [:env/time]} state]
       {:inner-diameter (q/map-range (mod (- time offset) interval) 0 interval 0 max-diameter)
        :outer-diameter (+ (+ 400 stroke-width) (q/map-range (mod (- time offset) interval) 0 interval 0 max-diameter))})

     :draw
     (fn [state]
       (q/no-fill)
       (stroke 0)
       (q/stroke-weight 400)
       (q/ellipse center-x center-y (:outer-diameter state) (:outer-diameter state))
       (fill 0)
       (q/no-stroke)
       (q/ellipse center-x center-y (:inner-diameter state) (:inner-diameter state)))}))

(defn twinkle
  "Randomized grid of twinkling lights. The brightness is a function of Perlin noise, with the low and high range as available parameters. You can specify the hue, or it is 160 by default, or set :gradient to true for a color gradient."
  [[x y width height] {:keys [cols rows interval twinkle-step hue low-brightness high-brightness gradient]}]
  (let [cols (or cols 30)
        rows (or rows 30)
        cell-x (/ width cols)
        cell-y (/ height rows)
        interval (or interval 100)
        hue (or hue 160)
        low-brightness (or low-brightness -20)
        high-brightness (or high-brightness 50)
        gradient (or gradient false)]

    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time
        :grid (cellwise-grid-init #(rand 10) cols rows)})

     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time grid] :as state}]
       (if (< time (+ last-step-time interval))
         state
         {:last-step-time time
          :grid (let [cells (apply concat (:grid state))]
                  (mapv vec (partition cols
                              (mapv (partial + twinkle-step) cells))))}))

     :draw
     (fn [state]
       (q/no-stroke)
       (doseq [[i j] (coord-seq rows cols)]
         (let [brightness (q/map-range (q/noise (get-in (:grid state) [i j])) 0 1 low-brightness high-brightness)]
           (if (= gradient false)
             (fill [:hsb hue 60 brightness])
             (fill [:hsb (q/map-range i 0 rows 0 360) 60 brightness]))
           (q/rect (* i cell-x) (* j cell-y) cell-x cell-y))))}))

(defn rotating-star
  "Rotating star around center-x and center-y with specified color and stroke-width"
  [[x y width height] {:keys [center-x center-y interval offset color stroke-width]}]
  (let [center-x (or center-x (/ width 2))
        center-y (or center-y (/ height 2))
        interval (or interval 1000)
        offset (or offset 0)
        color (or color [:hsb 200 50 50])
        stroke-width (or stroke-width 10)]

    {:setup
     (fn [_]
       (q/rect-mode :center)
       {:angle 0})

     :update
     (fn [{:keys [:env/time]} state]
       {:angle (q/map-range (mod (- time offset) interval) 0 interval 0 q/TWO-PI)})

     :draw
     (fn [state]
         (q/no-fill)
         (stroke color)
         (q/stroke-weight stroke-width)
         (q/push-matrix)
         (q/translate center-x center-y)
         (q/rotate (:angle state))
         (draw-star 0 0 30 50 5)
         (q/pop-matrix))}))

(defn pinwheel
  "Rotating color wheel around center-x and center-y"
  [[x y width height] {:keys [center-x center-y interval offset radius]}]
  (let [center-x (or center-x (/ width 2))
        center-y (or center-y (/ height 2))
        interval (or interval 2000)
        offset (or offset 0)
        radius (or radius (/ width 2))]

    {:setup
     (fn [_]
       {:angle-offset 0})

     :update
     (fn [{:keys [:env/time]} state]
       {:angle-offset (q/map-range (mod (- time offset) interval) 0 interval 0 q/TWO-PI)})

     :draw
     (fn [state]
       (q/begin-shape :triangle-fan)
       (q/vertex center-x center-y)
       (doseq [i (range 361)]
         (fill [:hsb i 50 50])
         (stroke [:hsb i 50 50])
         (q/vertex (+ center-x (* radius (q/cos (+ (:angle-offset state) (q/radians i)))))
                   (+ center-y (* radius (q/sin (+ (:angle-offset state) (q/radians i)))))))
       (q/end-shape :close))}))

(defn plasma
  "Plasma hue effect"
  [[x y width height] {:keys [cols rows interval perlin-step]}]
  (let [cols (or cols 30)
        rows (or rows 30)
        cell-x (/ width cols)
        cell-y (/ height rows)
        interval (or interval 30)
        perlin-step (or perlin-step 0.2)]
    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time
        :perlin-offset 0})
     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time perlin-offset] :as state}]
       (if (< time (+ last-step-time interval))
         state
         {:last-step-time time
          :perlin-offset (+ perlin-offset perlin-step)}))
     :draw
     (fn [state]
       (q/no-stroke)
       (doseq [[i j] (coord-seq rows cols)]
         (let [noise (q/sin (* q/TWO-PI (q/noise (* i 0.1) (* j 0.1) (:perlin-offset state))))]
           (fill [:hsb (q/map-range noise -1 1 150 300) 60 50])
           (q/rect (* i cell-x) (* j cell-y) cell-x cell-y))))}))

(defn rain
  "Falling/fading colored drops."
  [[x y width height] {:keys [interval num-droplets hue]}]
  (let [interval (or interval 5)
        num-droplets (or num-droplets 50)
        hue (or hue 200)
        droplet-width (/ width num-droplets)]
    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time
        :droplets (into [] (repeatedly num-droplets #(rand height)))})

     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time droplets] :as state}]
       (if (< time (+ last-step-time interval))
         state
         {:last-step-time time
          :droplets (mapv #(mod (inc %) height) droplets)}))
     :draw
     (fn [state]
       (doseq [[i j] (coord-seq num-droplets 5)]
         (fill [:hsb hue 50 (* j 10)])
         (stroke [:hsb hue 50 (* j 10)])
         (q/rect (* i droplet-width) (+ (* j droplet-width) (get (:droplets state) i)) droplet-width droplet-width)))}))

(defn buzzing-bee
  "Horizontal yellow bars moving downwards"
  [[x y width height] {:keys [interval stripe-width]}]
  (let [interval (or interval 500)
        stripe-width (or stripe-width 20)]
    {:setup
      (fn [_]
        {:offset 0})

      :update
      (fn [{:keys [:env/time]} state]
        {:offset (q/map-range (mod time interval) 0 interval 0 (* 2 stripe-width))})

      :draw
      (fn [state]
        (stroke [:hsb 50 70 50])
        (fill [:hsb 50 70 50])
        (doseq [i (range (/ height stripe-width))]
          (cond (= (mod i 2) 0) (q/rect 0 (+ (:offset state) (* i stripe-width)) width stripe-width))))}))


(defn text [_ text {:keys [color offset] :or {color [0] offset [0 0]}}]
  "A layer that writes text in color at offerset. Defaults to black at [0 0]"
  {:draw (fn [_]
           (fill color)
           (apply q/text text offset))})

(defn fill-color [bounds color]
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

(defn transformation [[x-offset y-offset width height :as bounds] transform]
  {:draw
   (fn [_]
     (let [pixels (q/pixels)
           coords (for [y (range y-offset (+ y-offset height))
                        x (range x-offset (+ x-offset width))]
                    [x y])]
       (->> coords
            (map (fn [[x y :as coord]]
                    (let [pixel-index (int (u/point->pixel-index coord))]
                      (aset-int pixels pixel-index
                                (color-int (transform x y (u/pixel-int->rgb (aget ^ints pixels pixel-index))))))))
            (dorun))
       (q/update-pixels)))})

(defn switch-rb [bounds]
  (transformation bounds (fn [x y [r g b]]
                           [:rgb g b r])))