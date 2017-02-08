(ns snake.routes.chatsocket
  (:require [compojure.core :refer [GET defroutes]]
            [org.httpkit.server
             :refer [send! with-channel on-close on-receive]]
            [cognitect.transit :as t]))

(defonce channels (atom #{}))

(defn connect! [channel]
  (swap! channels conj channel))

(defn disconnect! [channel status]
  (swap! channels #(remove #{channel} %)))

(defn notify-clients [msg]
  (doseq [channel @channels]
    (send! channel msg)))

(defn ws-handler [request]
  (with-channel request channel
                (connect! channel)
                (on-close channel (partial disconnect! channel))
                (on-receive channel #(notify-clients %))))

(defroutes chatsocket-routes
           (GET "/chat" request (ws-handler request)))