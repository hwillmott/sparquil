(ns sparquil.spec
  "Specs for important Sparquil components"
  (:require [clojure.spec :as s]))

(s/def :sketch/state (constantly true))
(s/def :sketch/setup (constantly true))
(s/def :sketch/update (constantly true))
(s/def :sketch/draw (constantly true))

(s/def :sketch/layer (s/keys :opt-un [:sketch/setup :sketch/update :sketch/draw]))

(s/def :sketch/layer-spec (s/cat :layer-name (constantly true) :args (s/* (constantly true))))

(s/def :sketch/layers (s/map-of keyword?
                                (s/coll-of :sketch/layer-spec :kind vector?)))

(s/def :sketch/config (constantly true))
