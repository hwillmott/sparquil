(ns sparquil.layer
  (:require [clojure.spec :as spec]
            [clojure.core.match :refer [match]]
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

(def rainbow-orbit
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
           x (* (/ (q/width) 2.5) (q/cos angle))
           y (* (/ (q/height) 2.5) (q/sin angle))]
       ; Move origin point to the center of the sketch.
       (q/with-translation [(/ (q/width) 2)
                            (/ (q/height) 2)]
                           ; Draw the circle.
                           (q/ellipse x y 100 100))))})


(defn text [text {:keys [color offset] :or {color [0] offset [0 0]}}]
  {:draw (fn [_]
           (fill color)
           (apply q/text text offset))})

; Note that the way you specify grid (matrix) positions is different than
; the way you typically specify pixel positions. Grid coordinates
; throughout are i (row), j (col), while pixels are x (col), y (row).

(defn dims [grid]
  "Return the dims of grid as a vector of [rows cols]"
  [(count grid) (count (first grid))])

(defn grid-neighbor-coords
  "Return a coll of the coords of the neighbors of the cell with coords (i, j) in a grid
  of dimensions (rows, cols)."
  [[rows cols] [i j]]
  (for [i-offset (range -1 2)
        j-offset (range -1 2)
        :when (not= [i-offset j-offset] [0 0])]
    [(mod (+ i i-offset) rows) (mod (+ j j-offset) cols)]))

(defn grid-neighbors
  "Return a coll of the values of the neighbors of the cell at coords"
  [grid coords]
  (map (partial get-in grid) (grid-neighbor-coords (dims grid) coords)))

(defn conway-cell-transition
  "Given the value of a cell and a coll of the values of its neighbors,
   return the next value for the cell."
  [cell neighbors]
  (let [live-neighbors (count (filter identity neighbors))]
    (if cell
      (cond (< live-neighbors 2) false
            (<= 2 live-neighbors 3) true
            (< 3 live-neighbors) false)
      (if (= 3 live-neighbors) true false))))

(defn conway-cell-color [cell]
  "Given a conway cell, return the color it should be drawn"
  (if cell
    [:rgb 51 204 51]
    [:rgb 0 0 204]))

(defn conways
  "Return a layer that will run Conway's Game of Life on a grid of size [rows cols] stepping
  once per step-interval milliseconds.

  Stretches to fit the size of the sketch as determined by (quil/height) and (quil/width)"
  [rows cols step-interval]
  {:setup
   (fn [{:keys [:env/time]}]
     (println time)
     {:last-step-time time
      :grid (into [] (repeatedly rows
                                 (fn [] (into [] (repeatedly cols #(if (> 0.25 (rand)) true false))))))})

   :update
   (fn [{:keys [:env/time]} {:keys [last-step-time grid] :as state}]
     (if (< time (+ last-step-time step-interval))
       state
       {:last-step-time time
        :grid (let [neighbors (partial grid-neighbors grid)]
                (reduce (fn [grid coords]
                          (update-in grid coords conway-cell-transition (neighbors coords)))
                        grid
                        (for [i (range rows)
                              j (range cols)]
                          [i j])))}))

   :draw
   (fn [{:keys [:grid]}]
     (let [x-interval (/ (q/width) cols)
           y-interval (/ (q/height) rows)]
       (dorun
         (for [i (range rows)
               j (range cols)]
           (do (fill (conway-cell-color (get-in grid [i j])))
               (q/rect (* j x-interval) (* i y-interval) x-interval y-interval))))))})
