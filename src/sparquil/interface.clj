(ns sparquil.interface
  (:require [com.stuartsierra.component :as component]
            [ring.middleware.file :refer [wrap-file]]
            [org.httpkit.server :as hkit :refer [with-channel]]
            [bidi.ring :refer [make-handler]]
            [sparquil.sketch :refer [load-scene]]))

;(defn async-handler [ring-request]
;  ;; unified API for WebSocket and HTTP long polling/streaming
;  (with-channel ring-request channel    ; get the channel
;                (if (websocket? channel)            ; if you want to distinguish them
;                  (on-receive channel (fn [data]     ; two way communication
;                                        (send! channel data)))
;                  (send! channel {:status 200
;                                  :headers {"Content-Type" "text/plain"}
;                                  :body    "Long polling?"}))))

(defn greeting-handler [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "hello HTTP world!"})

(defn sketch-scene-handler
  "Returns a handler to load the URL-specified scene into sketch"
  [sketch]
  (fn [{:keys [route-params]}]
    (load-scene sketch (:name route-params))))

(defn routes-for-sketch [sketch]
  ["/" {"" greeting-handler
        ["scene/" [keyword :name]] (sketch-scene-handler sketch)}])

(defrecord WebInterface [config server sketch]
  component/Lifecycle
  (start [web-interface]
    (let [handler (make-handler (routes-for-sketch sketch))]
      (reset! server (hkit/run-server handler config)))
    web-interface)

  (stop [web-interface]
    (@server :timeout 100)
    (reset! server nil)
    web-interface))

(defn new-web-interface [config]
  (map->WebInterface {:config config
                      :server (atom nil)}))