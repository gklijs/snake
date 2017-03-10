(ns snake.routes.gamesocket
  (:require [compojure.core :refer [GET defroutes]]
            [org.httpkit.server :refer [send! with-channel on-close on-receive]]
            [cognitect.transit :as t]
            [snake.snakepure :as snakepure]
            [snake.validation :refer [valid-registration-map?]]
            [overtone.at-at :refer [every stop-and-reset-pool! mk-pool]])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream)))

(def my-pool (mk-pool))
(defonce channels (atom {}))
(defonce userinfo (atom {}))
(defonce highscores (atom {}))
(defonce unique-key-user-key (atom {}))
(defonce key-counter (atom 0))
(defonce game-state (atom (snakepure/initial-state 0)))
(defonce update-job (atom nil))

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

(defn update-score
  [{:keys [highest games-played average] :as score} points]
  (let [new-highest (max highest points)
        new-games-played (inc games-played)
        new-average (/ (+ points (* average games-played)) new-games-played)
        new-float-average (float new-average)]
    {:highest new-highest :games-played new-games-played :average new-average :average-float new-float-average}))

(defn save-score
  "Saves the score to be able to tell the highscores"
  [user-key points]
  (if (contains? @highscores user-key)
    (swap! highscores #(update % user-key update-score points))
    (swap! highscores #(assoc % user-key {:highest points :games-played 1 :average points :average-float (float points)}))
    ))

(defn send-next-game-state
  "Send the next game state to all living snakes in the current game state."
  []
  (let [current-snakes (:snakes @game-state)
        next-game-state (swap! game-state snakepure/next-state)]
    (doseq [[k v] @channels]
      (if (contains? current-snakes (get-in @unique-key-user-key [k :user-key]))
        (send! v (writejson next-game-state))))
    (doseq [[k snake] current-snakes]
      (if (nil? (get-in next-game-state [:snakes k]))
        (save-score k (:points snake))))))

(defn connect! [channel unique-key]
  (swap! channels assoc unique-key channel))

(defn disconnect! [unique-key status]
  (swap! unique-key-user-key #(dissoc % unique-key))
  (swap! channels #(dissoc % unique-key))
  (if (empty? @channels) (do
                           (stop-and-reset-pool! my-pool)
                           (reset! update-job nil))))

(defn register-user
  "registeres the user"
  [channel unique-key registration-map]
  (let [validation (valid-registration-map? registration-map)]
    (if (first validation)
      (let [user-key (keyword (:username registration-map))
            password (:password registration-map)]
        (letfn [(register [] (let [key-map {:user-key user-key}]
                               (swap! userinfo assoc-in [user-key :password] password)
                               (swap! unique-key-user-key assoc unique-key key-map)
                               (send! channel (writejson key-map))
                               (send! channel (writejson (str "server: Succesfully registered with unique key: " (name unique-key))))
                               (if (nil? @update-job) (reset! update-job (every 150 #(send-next-game-state) my-pool)))
                               ))]
          (if-let [thisuserinfo (get @userinfo user-key)]
            (if
              (= password (get thisuserinfo :password))
              (register)
              (send! channel (writejson "server: Username already exists with other password")))
            (register))))
      (send! channel (writejson (str "server: error in registration-map: " (second validation)))))))

(defn move-snake-user
  "If a valid move, updates the game state"
  [new-direction unique-key game-state]
  (if-let [user-key (get-in @unique-key-user-key [unique-key :user-key])]
    (if-let [current-snake (get-in game-state [:snakes user-key])]
      (if-let [new-snake (snakepure/change-direction current-snake new-direction)]
        (assoc-in game-state [:snakes user-key] new-snake)
        game-state)
      game-state)
    game-state
    ))

(defn add-snake-user
  "If user currently has no snake, updates the game state with the new snake"
  [unique-key game-state]
  (if-let [user-key (get-in @unique-key-user-key [unique-key :user-key])]
    (if-not
      (get-in game-state [:snakes user-key])
      (assoc-in game-state [:snakes user-key] (snakepure/rand-snake (:board game-state)))
      game-state)
    game-state
    ))

(defn handle-string-message
  [string-message unique-key]
  (let [user-key (get-in @unique-key-user-key [unique-key :user-key])
        user-string (if user-key (name user-key) "anonymous")]
    (doseq [[k channel] @channels] (send! channel (writejson (str user-string ": " string-message))))))

(defn handle-map-message
  [map-message unique-key]
  (if-let [channel (get @channels unique-key)]
    (cond
      (contains? map-message :username) (register-user channel unique-key map-message)
      (contains? map-message :new-direction) (swap! game-state #(move-snake-user (:new-direction map-message) unique-key %))
      (contains? map-message :start) (swap! game-state #(add-snake-user unique-key %))
      (contains? map-message :highscores) (send! channel (writejson {:highscores @highscores}))
      :else (send! channel (writejson "server: Could not handle map data.")))))

(defn handle-message
  "Does the message handling"
  [msg unique-key]
  (if-let [game-input (readjson msg)]
    (cond
      (string? game-input) (handle-string-message game-input unique-key)
      (map? game-input) (handle-map-message game-input unique-key))))

(defn ws-handler [request]
  (with-channel request channel
                (let [key (get-unique-key)]
                  (connect! channel key)
                  (on-close channel (partial disconnect! key))
                  (on-receive channel #(handle-message % key)))))

(defroutes gamesocket-routes
           (GET "/game" request (ws-handler request)))