(ns sparquil.spec
  "Specs for important Sparquil components"
  (:require [clojure.spec :as s]))

(s/def :sketch/state (constantly true))
(s/def :sketch/setup (constantly true))
(s/def :sketch/update (constantly true))
(s/def :sketch/draw (constantly true))

(s/def :sketch/opts (s/keys :req-un [:sketch/setup :sketch/update :sketch/draw]))
