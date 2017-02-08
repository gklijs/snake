(ns snake.routes.gamesocket
  (:require [compojure.core :refer [GET defroutes]]
            [org.httpkit.server
             :refer [send! with-channel on-close on-receive]]
            [cognitect.transit :as t])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream)))

(defonce channels (atom #{}))

(defn readjson [data]
  (let [in (ByteArrayInputStream. (.getBytes data))
        reader (t/reader in :json)
        result (t/read reader)]
    (.reset in)
    result))

(defn writejson [data]
  (let [baos (ByteArrayOutputStream.)
        writer (t/writer baos :json)
        - (t/write writer data)
        result (.toString baos)]
    (.reset baos)
    result))

(defn connect! [channel]
  (swap! channels conj channel))

(defn disconnect! [channel status]
  (swap! channels #(remove #{channel} %)))

(defn notify-clients [msg]
  (println msg)
  (let [game-input (readjson msg)]
    (println game-input)
    (cond
      (seq (:user game-input)) (doseq [channel @channels] (send! channel (writejson (str "user is: " (:user game-input)))))
      :else (doseq [channel @channels] (send! channel (writejson "kaas"))))))

(defn ws-handler [request]
  (with-channel request channel
                (connect! channel)
                (on-close channel (partial disconnect! channel))
                (on-receive channel #(notify-clients %))))

(defroutes gamesocket-routes
           (GET "/game" request (ws-handler request)))