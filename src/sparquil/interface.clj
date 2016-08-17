(ns sparquil.interface
  (:require [com.stuartsierra.component :as component]
            [ring.util.response :refer [resource-response]]
            [ring.middleware.transit :refer [wrap-transit-response]]
            [org.httpkit.server :as hkit :refer [with-channel]]
            [bidi.ring :refer [make-handler ->ResourcesMaybe]]
            [sparquil.sketch :refer [load-scene get-config get-scene-spec]]))

;(defn async-handler [ring-request]
;  ;; unified API for WebSocket and HTTP long polling/streaming
;  (with-channel ring-request channel    ; get the channel
;                (if (websocket? channel)            ; if you want to distinguish them
;                  (on-receive channel (fn [data]     ; two way communication
;                                        (send! channel data)))
;                  (send! channel {:status 200
;                                  :headers {"Content-Type" "text/plain"}
;                                  :body    "Long polling?"}))))

(defn test-handler [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "hello HTTP world!"})

(defn sketch-scene-handler
  "Returns a handler to load the URL-specified scene into sketch"
  [sketch]
  (fn [{:keys [route-params]}]
    (load-scene sketch (:name route-params))))

(defn sketch-config-handler
  "Returns a handler that returns the config of sketch"
  [sketch]
  (fn [_]
    {:status 200
     :headers {"Content-Type" "application/transit+json"}
     :body (get-config sketch)}))

(defn sketch-scene-spec-handler
  "Returns a handler that returns the config of sketch"
  [sketch]
  (fn [_]
    {:status 200
     :headers {"Content-Type" "application/transit+json"}
     :body (get-scene-spec sketch)}))

(defn routes-for-sketch [sketch]
  ["/" [[["scene/" [keyword :name]] (sketch-scene-handler sketch)]
        ["scene" (sketch-scene-spec-handler sketch)]
        ["config" (sketch-config-handler sketch)]
        ["" (fn [_] (resource-response "public/index.html"))]
        ["" (->ResourcesMaybe {:prefix "public/"})]]])


(defrecord InterfaceServer [config server sketch]
  component/Lifecycle
  (start [interface-server]
    (let [handler (-> (make-handler (routes-for-sketch sketch))
                      (wrap-transit-response {:encoding :json, :opts {}}))]
      (reset! server (hkit/run-server handler config)))
    interface-server)

  (stop [interface-server]
    (@server :timeout 100)
    (reset! server nil)
    interface-server))

(defn new-interface-server [config]
  (map->InterfaceServer {:config config
                         :server (atom nil)}))
