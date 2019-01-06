(ns sparquil.opc
  (:import (java.net Socket))
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [sparquil.util :as u]))

(defn open-connection [host port]
  (let [socket (java.net.Socket. host port)]
    (.setTcpNoDelay socket true)
    (.getOutputStream socket)))

(defn close-connection [connection]
  (.close connection))

(defn push-pixels
  "Pushes pixels in pixel-map to the device over connection.
   pixel-map is a map of OPC channel numbers to vectors of pixels."
  [pixel-map connection]
  (doseq [[channel-num pixels] pixel-map]
    (let [command-num 0 ; set color command per OPC
          pixel-byte-length (* 3 (count pixels))
          pixel-byte-length-high (quot pixel-byte-length 256)
          pixel-byte-length-low (mod pixel-byte-length 256)
          header [channel-num command-num pixel-byte-length-high pixel-byte-length-low]]
      (->> pixels
           (mapcat u/pixel-int->rgb)
           (concat header)
           (byte-array)
           (.write connection))
      (.flush connection))))

