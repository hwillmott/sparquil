(ns sparquil.display
  (:require [com.stuartsierra.component :as component]
            [sparquil.opc :as opc]))

(defprotocol Displayer
  "A protocol for things that are capable of displaying frames"
  (display [displayer frame] "Display a frame"))

(defrecord FadecandyDisplayer [host port connection]
  component/Lifecycle
  (start [displayer]
    (assoc displayer :connection (opc/open-connection host port)))

  (stop [displayer]
    (opc/close-connection connection)
    (dissoc displayer :connection))

  Displayer
  (display [_ frame]
    (opc/push-pixels {0 frame} connection)))

(defn new-fadecandy-displayer [host port]
  (map->FadecandyDisplayer {:host host :port port}))