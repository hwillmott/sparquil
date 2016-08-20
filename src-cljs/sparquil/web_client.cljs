(ns sparquil.web-client
  (:require [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            [ajax.core :refer [GET PUT]]))

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

(defn put-scene! [scene]
  (PUT "/scene"
        {:params          {:scene scene}
         :handler         #(swap! state assoc :scene %)
         :error-handler   #(.log js/console %)
         :format          :transit
         :keywords?       true}))

(defn scene-list-item [{:keys [id display-name]}]
  [:a {:class (str "list-group-item scene-list-item"
                   (when (= id (get-in @state [:scene :id])) " active"))
       :href "#"
       :on-click #(put-scene! id)}
   (or display-name name)])

(defn scene-list []
  [:div.panel.panel-default.scene-list-container
   [:div.panel-heading
    [:h2 "Available scenes"]]
   [:div.list-group.scene-list
    (for [scene (get-in @state [:config :scenes])]
      ^{:key (:id scene)} [scene-list-item scene])]])

(defn root []
  (refresh-config!)
  (refresh-scene!)
  (fn []
    [:div.root
     [scene-list]]))

(defn mount-root []
  (r/render [root] (.getElementById js/document "app")))

(mount-root)
