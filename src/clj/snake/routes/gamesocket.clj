(ns snake.routes.gamesocket
  (:require [compojure.core :refer [GET defroutes]]
            [org.httpkit.server :refer [send! with-channel on-close on-receive]]
            [cognitect.transit :as t]
            [snake.snakepure :as snakepure]
            [overtone.at-at :refer [every stop mk-pool]])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream)))

(def my-pool (mk-pool))
(defonce channels (atom {}))
(defonce userinfo (atom {}))
(defonce unique-key-user-key (atom {}))
(defonce key-counter (atom 0))
(defonce game-state (atom (snakepure/initial-state 5)))

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

(defn get-unique-key
  []
  (keyword (str (swap! key-counter inc))))

(defn send-next-game-state
  "send the next game state to all living snakes in the current game state"
  []
  (let [current-snakes (:snakes @game-state)
        next-game-sate (swap! game-state snakepure/next-state)
        user-key-unique-key (clojure.set/map-invert @unique-key-user-key)]
    (doseq [[k v] current-snakes]
      (if-let [channel (get @channels (get user-key-unique-key (name k)))]
        (send! channel (writejson next-game-sate))))))

(def job (every 1000 #(send-next-game-state) my-pool))

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
                         (swap! game-state assoc-in [:snakes user-key] (snakepure/rand-snake (:board @game-state)))
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
                (let [key (get-unique-key)]
                  (connect! channel key)
                  (on-close channel (partial disconnect! key))
                  (on-receive channel #(handle-message % key)))))

(defroutes gamesocket-routes
           (GET "/game" request (ws-handler request)))