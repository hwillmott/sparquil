(ns sparquil.util
  (:require [quil.core :as q]))

(defn degrees->radians [degrees]
  (* degrees (/ Math/PI 180)))

(defn point->pixel-index
  "Given a size and a point, returns the corresponding index in quil/pixel array. If size not
  provided, attempts to read size from quil.core/width and quil.core/height (only works if called
  from animation thread)."
  ([point]
   (point->pixel-index [(q/width) (q/height)] point))
  ([[width height :as size] [x y :as point]]
   (if (and (<= 0 x width) (<= 0 y height))
     (+ (Math/round (double x)) (* width (Math/round (double y))))
     nil)))

(defn pixel-int->rgb [^long pixel-int]
  [(bit-and 0xFF (bit-shift-right pixel-int 16))
   (bit-and 0xFF (bit-shift-right pixel-int 8))
   (bit-and 0xFF pixel-int)])