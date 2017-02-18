(ns snake.routes.gamesocket
  (:require [compojure.core :refer [GET defroutes]]
            [org.httpkit.server :refer [send! with-channel on-close on-receive]]
            [cognitect.transit :as t])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream)))

(defonce channels (atom {}))
(defonce userinfo (atom {}))
(defonce unique-key-user-key (atom {}))
(defonce key-counter (atom 0))
(defonce unique-key (atom (keyword "0")))

(defn set-unique-key
  []
  (reset! unique-key (keyword (str (swap! key-counter inc)))))

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

(defn connect! [channel unique-key]
  (swap! channels assoc unique-key channel))

(defn disconnect! [unique-key status]
  (swap! unique-key-user-key #(dissoc % unique-key))
  (swap! channels #(dissoc % unique-key)))

(defn register-user
  "registeres the user"
  [channel unique-key user-key password]
  (letfn [(register [] (do
                         (swap! userinfo assoc-in [user-key :password] password)
                         (swap! unique-key-user-key assoc unique-key (name user-key))
                         (send! channel (writejson (str "Succesfully registered with unique key: " (name unique-key))))))]
    (if-let [thisuserinfo (get @userinfo user-key)]
      (if
        (= password (get thisuserinfo :password))
        (register)
        (send! channel (writejson "Username already exists with other password")))
      (register))))

(defn handle-message
  "Does the message handling"
  [msg unique-key]
  (let [game-input (readjson msg)]
    (if-let [channel (get @channels unique-key)]
      (cond
        (seq (:username game-input)) (register-user channel unique-key (keyword (clojure.string/lower-case (:username game-input))) (:password game-input))
        :else (send! channel (writejson "Could not handle data."))))))

(defn ws-handler [request]
  (with-channel request channel
                (set-unique-key)
                (connect! channel @unique-key)
                (on-close channel (partial disconnect! @unique-key))
                (on-receive channel #(handle-message % @unique-key))))

(defroutes gamesocket-routes
           (GET "/game" request (ws-handler request)))