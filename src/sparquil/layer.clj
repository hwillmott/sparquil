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
    [:rgb _ _ _]                (q/color-mode :rgb 255)
    [:hsb _ _ _]                (q/color-mode :hsb 360 100 100)))

(defn color-apply
  "Calls f, a Quil function that takes params like fill, with color"
  [f color]
  (match color
    (brightness :guard number?) (f brightness)
    [brightness]                (f brightness)
    [:rgb r g b]                (f r g b)
    [:hsb h s v]                (f h s v)))

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
     (q/frame-rate 30)
     {:hue   0
      :angle 0})

   :update
   (fn [env state]
     {:hue   (or (parse-number (:env/color env))
                 (mod (+ (:hue state) 0.7) 360))
      :angle (+ (:angle state) 0.1)})

   :draw
   (fn [state]
     (background 0)
     (fill [:hsb (:hue state) 100 100])
     ; Calculate x and y coordinates of the circle.
     (let [angle (:angle state)
           x (* 150 (q/cos angle))
           y (* 150 (q/sin angle))]
       ; Move origin point to the center of the sketch.
       (q/with-translation [(/ (q/width) 2)
                            (/ (q/height) 2)]
                           ; Draw the circle.
                           (q/ellipse x y 100 100))))})


(defn text [text & {:keys [color offset] :or {color [0] offset [0 0]}}]
  {:draw (fn [_]
           (fill color)
           (apply q/text text offset))})
