(ns sparquil.web-client
  (:require [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            [ajax.core :refer [GET]]))

(enable-console-print!)

(def state (r/atom {:config nil
                    :scene nil}))

(defn refresh-config! []
  (GET "/config"
       {:handler         (fn [result]
                           (println result)
                           (swap! state assoc :config result))
        :error-handler   #(.log js/console %)
        :format          :transit}))

(defn refresh-scene! []
  (GET "/scene"
       {:handler         (fn [result]
                           (println result)
                           (swap! state assoc :scene result))
        :error-handler   #(.log js/console %)
        :format          :transit}))

(defn scene-list-item [scene-name]
  (let [scene (get-in @state [:config :scenes scene-name])]
    [:li.list-group-item.scene-list-item scene-name]))

(defn scene-list []
  [:ul.list-group.scene-list
   (for [scene-name (keys (get-in @state [:config :scenes]))]
     ^{:key scene-name} [scene-list-item scene-name])])

(defn root []
  (refresh-config!)
  (refresh-scene!)
  (fn []
    [:div.root
     [scene-list]]))

(defn mount-root []
  (r/render [root] (.getElementById js/document "app")))

(mount-root)
