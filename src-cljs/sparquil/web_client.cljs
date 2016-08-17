(ns sparquil.web-client
  (:require [reagent.core :as r]))

(defn root-component []
  [:div
   [:h1 "Sparquil control center"]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red"]
    " text."]])

(defn mount-root []
  (r/render [root-component] (.getElementById js/document "app")))

(mount-root)
