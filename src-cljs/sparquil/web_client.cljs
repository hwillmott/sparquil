(ns sparquil.web-client
  (:require [reagent.core :as r]))

(defn some-component []
  [:div
   [:h3 "I am a component!"]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red"]
    " text."]])

(defn mount-root []
  (r/render [some-component] (.getElementById js/document "app")))

(mount-root)
