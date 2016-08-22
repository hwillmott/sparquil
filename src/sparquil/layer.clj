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

(defn stroke-and-fill
  "Applies Quil's stroke and Quil's fill, but takes Sparquil colors"
  [color]
  (color-mode color)
  (color-apply q/stroke color)
  (color-apply q/fill color))

(defn region-background
  "Like Quil's background, but sets the background only for the region specified by bounds
  to color"
  [[_ _ width height :as bounds] color]
  (fill color)
  (stroke color)
  (q/rect 0 0 width height))

(defn pmapv
  "An eager version of pmap. Spawns a thread for _every_ element in coll. Use
   for parallel IO."
  [f & colls]
  (->> colls
       (apply mapv (fn [& args] (future (apply f args))))
       (mapv deref)))

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

(defn draw-heart
  "Draws a heart with the bottom at x and y"
  [x y scale]
  (q/bezier x y x (- y (/ scale 2)) (+ x scale) (- y (/ scale 4)) x (+ y (/ scale 2)))
  (q/bezier x y x (- y (/ scale 2)) (- x scale) (- y (/ scale 4)) x (+ y (/ scale 2))))

(defn plasma-value
  "Returns the plasma value for "
  [x y t scale-x scale-y]
  (let [x (- (* x scale-x) (/ scale-x 2))
        y (- (* y scale-y) (/ scale-y 2))
        v (+ (q/sin (+ x t))
             (q/sin (/ (+ y t) 2))
             (q/sin (/ (+ x y t) 2)))
        c-x (+ x (* (/ scale-x 2) (q/sin (/ t 3))))
        c-y (+ y (* (/ scale-y 2) (q/cos (/ t 2))))
        v2 (/
             (+ v
                (q/sin
                  (+ t
                     (q/sqrt (+
                               (* c-x c-x)
                               (* c-y c-y)
                               1)))))
             2)]))


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
  [[x y width height] {:keys [direction upper-limit-h lower-limit-h upper-limit-b lower-limit-b lower-bound hue variable]}] 
  (let [direction (or direction :horizontal)
        variable (or variable :color)
        hue (or hue 70)
        upper-limit-h (or upper-limit-h 360)
        lower-limit-h (or lower-limit-h 0)
        upper-limit-b (or upper-limit-b 0)
        lower-limit-b (or lower-limit-b 50)]
   {:draw 
    (fn [_]  
     (let [y-interval (/ height 360)
           x-interval (/ width 360 )]
       (doseq [h (range 360)]
         (stroke-and-fill [:hsb h 50 50])
         (let [h-val (q/map-range h 0 360 lower-limit-h upper-limit-h)
               b-val (q/map-range h 0 360 lower-limit-b upper-limit-b)]
           (cond
             (= variable :color) (stroke-and-fill [:hsb h-val 60 50])
             (= variable :brightness) (stroke-and-fill [:hsb hue 60 b-val])
             (= variable :color-and-brightness) (stroke-and-fill [:hsb h-val 60 b-val])))
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

(defn beacon-odroid
  "Low light beacon visualization centered around center-x and center-y. The beacon expands from 0 to max-diameter over the specified interval. You can specify the color and stroke-width of the beacon."
  [[x y width height] {:keys [center-x center-y interval size-step offset max-diameter restrict-size color stroke-width]}]
  (let [center-x (or center-x (/ width 2))
        center-y (or center-y (/ height 2))
        interval (or interval 200)
        size-step (or size-step 40)
        offset (or offset 0)
        max-diameter (or max-diameter (max width height))
        color (or color [:hsb 115 50 50])
        stroke-width (or stroke-width 20)
        restrict-size (or restrict-size true)]

    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time
        :diameter 0})

     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time grid] :as state}]
       (if (< time (+ last-step-time interval offset))
         state
         {:last-step-time time
          :diameter (mod (+ size-step (:diameter state)) max-diameter)}))

     :draw
     (fn [state]
       (q/no-fill)
       (stroke color)
       (q/stroke-weight stroke-width)
       (if restrict-size (q/ellipse center-x center-y (:diameter state) (:diameter state))
                         (cond (< (:diameter state) width) (q/ellipse center-x center-y (:diameter state) (:diameter state)))))}))

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
  [[x y width height] {:keys [cols rows interval twinkle-step hue lower-limit-b upper-limit-b lower-limit-h upper-limit-h gradient shift]}]
  (let [cols (or cols 30)
        rows (or rows 30)
        cell-x (/ width cols)
        cell-y (/ height rows)
        interval (or interval 100)
        twinkle-step (or twinkle-step 0.1)
        hue (or hue 160)
        lower-limit-b (or lower-limit-b -10)
        upper-limit-b (or upper-limit-b 60)
        lower-limit-h (or lower-limit-h -10)
        upper-limit-h (or upper-limit-h 60)
        gradient (or gradient false)
        shift (or shift false)]

    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time
        :offset 0
        :grid (cellwise-grid-init #(rand 10) cols rows)})

     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time grid] :as state}]
       (if (< time (+ last-step-time interval))
         state
         {:last-step-time time
          :offset (inc (:offset state))
          :grid (let [cells (apply concat (:grid state))]
                  (pmapv vec (partition cols
                               (pmapv (partial + twinkle-step) cells))))}))

     :draw
     (fn [state]
       (q/no-stroke)
       (doseq [[i j] (coord-seq rows cols)]
         (let [brightness (q/map-range (q/noise (get-in (:grid state) [i j])) 0 1 lower-limit-b upper-limit-b)]
           (cond
             (= gradient false) (stroke-and-fill [:hsb hue 60 brightness])
             (= shift true) (stroke-and-fill [:hsb (q/map-range (mod (+ i (:offset state)) rows) 0 rows lower-limit-h upper-limit-h) 60 brightness])
             (= shift false) (stroke-and-fill [:hsb (q/map-range (mod i rows) 0 rows lower-limit-h upper-limit-h) 60 brightness]))
           (q/rect (* i cell-x) (* j cell-y) cell-x cell-y))))}))

(defn twinkle-odroid
  "Randomized grid of twinkling lights. The brightness is a function of Perlin noise, with the low and high range as available parameters. You can specify the hue, or it is 160 by default, or set :gradient to true for a color gradient."
  [[x y width height] {:keys [cols rows interval twinkle-step hue lower-limit-b upper-limit-b lower-limit-h upper-limit-h gradient]}]
  (let [cols (or cols 20)
        rows (or rows 20)
        cell-x (/ width cols)
        cell-y (/ height rows)
        interval (or interval 100)
        twinkle-step (or twinkle-step 1)
        hue (or hue 160)
        lower-limit-b (or lower-limit-b -10)
        upper-limit-b (or upper-limit-b 60)
        lower-limit-h (or lower-limit-h -10)
        upper-limit-h (or upper-limit-h 60)
        gradient (or gradient false)
        coords (coord-seq rows cols)]

    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time
        :offset 0
        :grid (cellwise-grid-init #(rand 10) cols rows)})

     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time grid] :as state}]
       (if (< time (+ last-step-time interval))
         state
         {:last-step-time time
          :offset (+ (:offset state) twinkle-step)
          :grid grid}))

     :draw
     (fn [state]
       (q/no-stroke)
       (doseq [[i j] coords]
         (let [brightness (q/map-range (q/sin (+ (:offset state) (get-in (:grid state) [i j]))) -1 1 lower-limit-b upper-limit-b)]
           (if (= gradient false)
             (stroke-and-fill [:hsb hue 60 brightness])
             (stroke-and-fill [:hsb (q/map-range i 0 rows lower-limit-h upper-limit-h) 60 brightness]))
           (q/rect (* i cell-x) (* j cell-y) cell-x cell-y))))}))

(defn checkers
  "checker pattern of specified color or gradient. Can do non-update"
  [[x y width height] {:keys [cols rows interval hue low-brightness high-brightness gradient update]}]
  (let [cols (or cols 20)
        rows (or rows 20)
        cell-x (/ width cols)
        cell-y (/ height rows)
        interval (or interval 100)
        hue (or hue 160)
        low-brightness (or low-brightness -10)
        high-brightness (or high-brightness 60)
        gradient (or gradient false)
        coords (coord-seq rows cols)]

    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time
        :offset 0})

     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time] :as state}]
       (if (< time (+ last-step-time interval) update)
         state
         {:last-step-time time
          :offset (inc (:offset state))}))

     :draw
     (fn [state]
       (q/no-stroke)
       (doseq [[i j] coords]
         (let [h (if gradient (q/map-range (mod (+ i (:offset state)) rows) 0 rows 0 360) hue)
               b (if (= (mod (+ i j (:offset state)) 2) 0) low-brightness high-brightness)]
           (stroke-and-fill [:hsb h 60 b])
           (q/rect (* i cell-x) (* j cell-y) cell-x cell-y))))}))

(defn checkers-no-update
  "checker pattern of specified color or gradient. Can do non-update"
  [[x y width height] {:keys [cols rows interval hue low-brightness high-brightness gradient]}]
  (let [cols (or cols 20)
        rows (or rows 20)
        cell-x (/ width cols)
        cell-y (/ height rows)
        hue (or hue 160)
        low-brightness (or low-brightness -10)
        high-brightness (or high-brightness 60)
        gradient (or gradient false)
        coords (coord-seq rows cols)]

    {:draw
     (fn [state]
       (q/no-stroke)
       (doseq [[i j] coords]
         (let [h (if gradient (q/map-range (mod i rows) 0 rows 0 360) hue)
               b (if (= (mod (+ i j) 2) 0) low-brightness high-brightness)]
           (stroke-and-fill [:hsb h 60 b])
           (q/rect (* i cell-x) (* j cell-y) cell-x cell-y))))}))

(defn pulse
  "Randomized grid of twinkling lights. The brightness is a function of Perlin noise, with the low and high range as available parameters. You can specify the hue, or it is 160 by default, or set :gradient to true for a color gradient."
  [[x y width height] {:keys [cols rows interval twinkle-step hue low-brightness high-brightness gradient]}]
  (let [interval (or interval 100)]

    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time
        :brightness 50})

     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time grid] :as state}]
       (if (< time (+ last-step-time interval))
         state
         {:last-step-time time
          :brightness (cond (= (:brightness state) 50) 10
                            (= (:brightness state) 10) 50)}))

     :draw
     (fn [state]
       (q/no-stroke)
       (stroke-and-fill [:hsb 280 60 (:brightness state)])
       (q/rect 0 0 width height))}))


(defn kaleidoscope
  "Rotating, pulsing shapes around center-x and center-y with specified color and stroke-width. Star, rect, and heart available."
  [[x y width height] {:keys [center-x center-y interval-r interval-b offset size-change size-1 size-2 color stroke-width shape]}]
  (let [center-x (or center-x (/ width 2))
        center-y (or center-y (/ height 2))
        interval-r (or interval-r 1000)
        interval-b (or interval-b 700)
        offset (or offset 0)
        size-change (or size-change 10)
        size-1 (or size-1 25)
        size-2 (or size-2 45)
        color (or color [:hsb 200 50 50])
        stroke-width (or stroke-width 10)
        shape (or shape :star)] ;rect and heart available

    {:setup
     (fn [_]
       (q/rect-mode :center)
       {:angle 0
        :beat 0})

     :update
     (fn [{:keys [:env/time]} state]
       {:angle (q/map-range (mod (- time offset) interval-r) 0 interval-r 0 q/TWO-PI)
        :beat (q/map-range (mod (- time offset) interval-b) 0 interval-b 0 size-change)})

     :draw
     (fn [state]
       (q/no-fill)
       (stroke color)
       (q/stroke-weight stroke-width)
       (q/push-matrix)
       (q/translate center-x center-y)
       (q/rotate (:angle state))
       (cond
         (= shape :star) (draw-star 0 0 (+ (:beat state) size-1) (+ (:beat state) size-2) 5)
         (= shape :rect) (q/rect 0 0 (+ (:beat state) size-1) (+ (:beat state) size-2))
         (= shape :heart) (draw-heart 0 0(:beat state)))
       (q/pop-matrix))}))

(defn rotate-shape
  "Rotating shapes around center-x and center-y with specified color and stroke-width. Star, rect, and heart available."
  [[x y width height] {:keys [center-x center-y interval-r offset size-change size-1 size-2 color stroke-width shape]}]
  (let [center-x (or center-x (/ width 2))
        center-y (or center-y (/ height 2))
        interval-r (or interval-r 1000)
        offset (or offset 0)
        size-1 (or size-1 40)
        size-2 (or size-2 60)
        color (or color [:hsb 40 50 50])
        stroke-width (or stroke-width 20)
        shape (or shape :star)] ;rect and heart available

    {:setup
     (fn [_]
       (q/rect-mode :center)
       {:angle 0})

     :update
     (fn [{:keys [:env/time]} state]
       {:angle (q/map-range (mod (- time offset) interval-r) 0 interval-r 0 q/TWO-PI)})

     :draw
     (fn [state]
       (q/no-fill)
       (stroke color)
       (q/stroke-weight stroke-width)
       (q/push-matrix)
       (q/translate center-x center-y)
       (q/rotate (:angle state))
       (cond
         (= shape :star) (draw-star 0 0 size-1 size-2 5)
         (= shape :rect) (q/rect 0 0 size-1 size-2)
         (= shape :heart) (draw-heart 0 0 50))
       (q/pop-matrix))}))

(defn rotate-shape-odroid
  "Rotating shapes around center-x and center-y with specified color and stroke-width. Star, rect, and heart available."
  [[x y width height] {:keys [center-x center-y interval offset angle-step size-1 size-2 color stroke-width shape]}]
  (let [center-x (or center-x (/ width 2))
        center-y (or center-y (/ height 2))
        interval (or interval 1000)
        offset (or offset 0)
        angle-step (or angle-step 0.5)
        size-1 (or size-1 40)
        size-2 (or size-2 60)
        color (or color [:hsb 40 50 50])
        stroke-width (or stroke-width 20)
        shape (or shape :star)] ;rect and heart available

    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time
        :angle 0})

     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time grid] :as state}]
       (if (< time (+ last-step-time interval))
         state
         {:last-step-time time
          :angle (+ angle-step (:angle state))}))

     :draw
     (fn [state]
       (q/no-fill)
       (stroke color)
       (q/stroke-weight stroke-width)
       (q/push-matrix)
       (q/translate center-x center-y)
       (q/rotate (:angle state))
       (cond
         (= shape :star) (draw-star 0 0 size-1 size-2 5)
         (= shape :rect) (q/rect 0 0 size-1 size-2)
         (= shape :heart) (draw-heart 0 0 50))
       (q/pop-matrix))}))

(defn draw-shape-odroid
  "Rotating shapes around center-x and center-y with specified color and stroke-width. Star, rect, and heart available."
  [[x y width height] {:keys [center-x center-y size-1 size-2 color stroke-width shape]}]
  (let [center-x (or center-x (/ width 2))
        center-y (or center-y (/ height 2))
        size-1 (or size-1 40)
        size-2 (or size-2 60)
        color (or color [:hsb 40 50 50])
        stroke-width (or stroke-width 20)
        shape (or shape :star) ;rect and heart available
        rotation (if (= shape :star) 0.94 0)]

    {:draw
     (fn [state]
       (q/no-fill)
       (stroke color)
       (q/stroke-weight stroke-width)
       (q/push-matrix)
       (q/translate center-x center-y)
       (q/rotate rotation)
       (cond
         (= shape :star) (draw-star 0 0 size-1 size-2 5)
         (= shape :rect) (q/rect 0 0 size-1 size-2)
         (= shape :heart) (draw-heart 0 0 50))
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
         (stroke-and-fill [:hsb i 60 50])
         (q/vertex (+ center-x (* radius (q/cos (+ (:angle-offset state) (q/radians i)))))
                   (+ center-y (* radius (q/sin (+ (:angle-offset state) (q/radians i)))))))
       (q/end-shape :close))}))

(defn perlin-plasma
  "Plasma hue effect"
  [[x y width height] {:keys [cols rows interval perlin-step variable hue upper-limit-h lower-limit-h upper-limit-b lower-limit-b]}]
  (let [cols (or cols 40)
        rows (or rows 40)
        cell-x (/ width cols)
        cell-y (/ height rows)
        interval (or interval 30)
        perlin-step (or perlin-step 0.05)
        variable (or variable :color)
        hue (or hue 200)
        upper-limit-h (or upper-limit-h 200)
        lower-limit-h (or lower-limit-h 300)
        upper-limit-b (or upper-limit-b -50)
        lower-limit-b (or lower-limit-b -50)]
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
           (cond
             (= variable :color) (stroke-and-fill [:hsb (q/map-range noise -1 1 lower-limit-h upper-limit-h) 60 50])
             (= variable :brightness) (stroke-and-fill [:hsb hue 60 (q/map-range noise -1 1 lower-limit-b upper-limit-b)])
             (= variable :color-and-brightness) (stroke-and-fill [:hsb (q/map-range noise -1 1 lower-limit-h upper-limit-h) 60 (q/map-range noise -1 1 lower-limit-b upper-limit-b)]))
           (q/rect (* i cell-x) (* j cell-y) cell-x cell-y))))}))

(defn gradys-plasma
  "a different noise function"
  [[_ _ width height] {:keys [cols rows interval time-scale-factor]}]
  (let [cols (or cols 100)
        rows (or rows 100)
        cell-x (/ width cols)
        cell-y (/ height rows)
        interval (or interval 30)
        time-scale-factor (or time-scale-factor 1000)
        v (fn [x y scaled-time]
            (let [cx (q/sin (/ scaled-time 3))
                  cy (q/cos (/ scaled-time 2))]
              (* 0.5 (+ (q/sin (+ x scaled-time))
                        (q/sin (* 0.5 (+ y scaled-time)))
                        (q/sin (* 0.5 (+ x y scaled-time)))
                        (q/sin (+ scaled-time (q/sqrt (+ 1 (* cx cx) (* cy cy)))))))))]
    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time})
     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time] :as state}]
       (if (< time (+ last-step-time interval))
         state
         {:last-step-time time}))
     :draw
     (fn [state]
       (q/no-stroke)
       (doseq [[i j] (coord-seq rows cols)]
         (let [v-val (v (* i 0.1) (* j 0.1) (:last-step-time state))
               v-pi (* v-val q/PI)
               r (q/map-range (q/sin (* v-pi)) -1 1 0 100)
               g (q/map-range (q/sin (+ v-pi (* (/ 2 3) q/PI))) -1 1 0 100)
               b (q/map-range (q/sin (+ v-pi (* (/ 4 3) q/PI))) -1 1 0 100)]
           (stroke-and-fill [:rgb r g b])
           (q/rect (* i cell-x) (* j cell-y) cell-x cell-y)))
       (q/text-size 30)
       (stroke-and-fill [:rgb 100 100 100])
       (q/text (str (:last-step-time state)) 100 250))}))

(defn rain
  "Falling/fading colored drops."
  [[x y width height] {:keys [interval num-droplets hue]}]
  (let [interval (or interval 100)
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
          :droplets (mapv #(mod (+ 10 %) height) droplets)}))
     :draw
     (fn [state]
       (doseq [[i j] (coord-seq num-droplets 5)]
         (stroke-and-fill [:hsb hue 50 (* j 10)])
         (q/rect (* i droplet-width) (+ (* j droplet-width) (get (:droplets state) i)) droplet-width droplet-width)))}))

(defn rain-odroid
  "Falling/fading colored drops."
  [[x y width height] {:keys [interval num-droplets hue]}]
  (let [interval (or interval 100)
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
          :droplets (mapv #(mod (+ 10 %) height) droplets)}))
     :draw
     (fn [state]
       (doseq [i (range num-droplets)]
         (stroke-and-fill [:hsb hue 50 40])
         (q/rect (* i droplet-width) (* droplet-width (get (:droplets state) i)) droplet-width droplet-width)))}))


(defn buzzing-bee
  "Horizontal yellow bars moving downwards"
  [[x y width height] {:keys [interval stripe-width color]}]
  (let [interval (or interval 500)
        stripe-width (or stripe-width 20)
        color (or color [:hsb 50 70 50])]
    {:setup
      (fn [_]
        {:offset 0})

      :update
      (fn [{:keys [:env/time]} state]
        {:offset (q/map-range (mod time interval) 0 interval 0 (* 2 stripe-width))})

      :draw
      (fn [state]
        (stroke-and-fill color)
        (doseq [i (range (/ height stripe-width))]
          (cond (= (mod i 2) 0) (q/rect 0 (+ (:offset state) (* i stripe-width)) width stripe-width))))}))

(defn beating-heart
  "A beating heart in the middle of the sketch, or where you define it"
  [[_ _ width height] {:keys [x y scale color interval size-step max-size-diff]}]
  (let [x (or x (/ width 2))
        y (or y (/ height 2))
        scale (or scale (/ width 4))
        color (or color [:hsb 340 70 50])
        interval (or interval 100)
        size-step (or size-step 10)
        max-size-diff (or max-size-diff 20)]

    {:setup
     (fn [{:keys [:env/time]}]
       {:last-step-time time
        :offset 0})

     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time offset] :as state}]
       (if (< time (+ last-step-time interval))
         state
         {:last-step-time time
          :offset (mod (+ offset size-step) max-size-diff)}))

     :draw
     (fn [state]
       (stroke-and-fill color)
       (draw-heart x y (- scale (:offset state))))}))

(defn beating-heart-odroid
  "A beating heart in the middle of the sketch, or where you define it"
  [[_ _ width height] {:keys [x y scale color interval]}]
  (let [x (or x (/ width 2))
        y (or y (/ height 2))
        scale (or scale (/ width 4))
        color (or color [:hsb 340 70 50])
        interval (or interval 5)]
    {:setup
     (fn [{:keys [:env/time]}]
       {:offset 0
        :last-step-time time})


     :update
     (fn [{:keys [:env/time]} {:keys [last-step-time] :as state}]
       (if (< time (+ last-step-time interval))
         state
         {:last-step-time time
          :offset (mod (+ 10 (:offset state)) 100)}))

     :draw
     (fn [state]
       (stroke-and-fill color)
       (draw-heart x y (- scale (:offset state))))}))

(defn chevron
  "chevrons moving"
  [[x y width height] {:keys [interval stripe-width chevron-height]}]
  (let [interval (or interval 500)
        stripe-width (or stripe-width 15)
        chevron-height (or chevron-height (/ width 2))]
    {:setup
     (fn [_]
       {:offset 0})

     :update
     (fn [{:keys [:env/time]} state]
       {:offset (q/map-range (mod time interval) 0 interval 0 (* 4 stripe-width))})

     :draw
     (fn [state]
       (stroke-and-fill [:hsb 50 70 50])
       (doseq [i (range (/ (* 2 height) stripe-width))]
         (if (= (mod i 2) 0)
           (do
             (q/stroke-weight stripe-width)
             (q/line (/ width 2) (+ (:offset state) (* i stripe-width 2)) 0 (- (+ (:offset state) (* i stripe-width 2)) chevron-height))
             (q/line (/ width 2) (+ (:offset state) (* i stripe-width 2)) width (- (+ (:offset state) (* i stripe-width 2)) chevron-height))))))}))

(defn bounce
  "a bouncing line"
  [[x y width height] {:keys [interval stripe-width]}]
  (let [interval (or interval 10000)
        stripe-width (or stripe-width 10)]
    {:setup
     (fn [_]
       {:offset 0})

     :update
     (fn [{:keys [:env/time]} state]
       {:offset (q/map-range (mod time interval) 0 interval 0 q/TWO-PI)})

     :draw
     (fn [state]
       (stroke-and-fill [:hsb 50 70 50])
       (q/stroke-weight stripe-width)
       (let [val (if (< (q/sin (:offset state)) 0)
                     (* -1 (q/sin (:offset state)))
                     (q/sin (:offset state)))
             factor (if (< (q/sin (:offset state)) 0)
                        -1
                        1)]

         (let [y-coord (+ (/ height 2) (* 50 factor (q/sqrt val)))]
           (q/line 0 y-coord width y-coord))))}))

(defn text [_ text {:keys [color size offset] :or {color [0] size 50 offset [0 0]}}]
  "A layer that writes text in color at offset. Defaults to black at [0 0]"
  {:draw (fn [_]
           (fill color)
           (q/text-size size)
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