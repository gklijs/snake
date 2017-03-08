(ns utils.websockets
  (:require [cognitect.transit :as t]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [dispatch]]))

(defonce ws-chan-chat (atom nil))
(defonce ws-chan-game (atom nil))
(def json-reader (t/reader :json))
(def json-writer (t/writer :json))

(defn logjs
  "This function prints an argument to the js console"
  [argument]
  (.log js/console (clj->js argument)))

(defn receive-transit-msg!
  [update-fn]
  (fn [msg]
    (update-fn
      (->> msg .-data (t/read json-reader)))))

(defn send-transit-chat!
  [msg]
  (if @ws-chan-chat
    (.send @ws-chan-chat (t/write json-writer msg))
    (throw (js/Error. "Websocket is not available!"))))

(defn send-transit-game!
  [msg]
  (if @ws-chan-game
    (.send @ws-chan-game (t/write json-writer msg))
    (throw (js/Error. "Websocket is not available!"))))

(defn make-chat-websocket! [url receive-handler]
  (println "attempting to connect chat websocket")
  (if-let [chan (js/WebSocket. url)]
    (do
      (set! (.-onmessage chan) (receive-transit-msg! receive-handler))
      (reset! ws-chan-chat chan)
      (println "Chat websocket connection established with: " url))
    (throw (js/Error. "Chat websocket connection failed!"))))

(defn make-game-websocket! [url receive-handler]
  (println "attempting to connect game websocket")
  (if-let [chan (js/WebSocket. url)]
    (do
      (set! (.-onmessage chan) (receive-transit-msg! receive-handler))
      (reset! ws-chan-game chan)
      (println "Game websocket connection established with: " url))
    (throw (js/Error. "Game websocket connection failed!"))))

(defn handle-message
  "updates the messages"
  [new-message]
  (cond
    (contains? new-message :board) (dispatch [:remote-game-state new-message])
    (contains? new-message :user-key) (dispatch [:update-game-info new-message])
    (contains? new-message :highscores) (dispatch [:update-game-info new-message])
    (string? new-message) (dispatch [:messages new-message])
    :default (logjs new-message)
    ))

(defn initsockets []
  (make-chat-websocket! (str "ws://" (.-host js/location) "/chat") handle-message)
  (make-game-websocket! (str "ws://" (.-host js/location) "/game") handle-message))
