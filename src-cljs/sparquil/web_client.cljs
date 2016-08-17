(ns sparquil.web-client
  (:require [reagent.core :as r]))

(defn root-component []
  [:div
   [:h1 "Sparquil control center"]
   [:ul.list-group
    [:li.list-group-item "Cat"]
    [:li.list-group-item "Dog"]]])

(defn mount-root []
  (r/render [root-component] (.getElementById js/document "app")))

(mount-root)
