(ns utils.websocket
  (:require [cljs.core.async :as a]
            [cognitect.transit :as t]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [subscribe dispatch]])
  (:require-macros [cljs.core.async.macros :as a]))

(defonce ws-chan-game (atom nil))
(def json-reader (t/reader :json))
(def json-writer (t/writer :json))

(defn receive-transit-msg!
  [update-fn]
  (fn [msg]
    (update-fn
      (->> msg .-data (t/read json-reader)))))

(defn open-handler
  []
  (let [game-info (subscribe [:game-info])]
    (if-let [registration-map (:registration-map @game-info)]
      (.send @ws-chan-game (t/write json-writer registration-map)))))


(defn close-handler
  [error]
  (if
    (instance? js/CloseEvent error)
    (println (str "Connection was closed clean: " (.-wasClean error)
                  " with errorcode (see https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent): "
                  (.-code error) " and optional reason: " (.-reason error)))
    (println "There was some error with the websocket."))
  (dispatch [:messages "Connection to the server was lost"])
  (reset! ws-chan-game nil))

(defn make-game-websocket! [url receive-handler]
  (println "attempting to connect game websocket")
  (if-let [chan (js/WebSocket. url)]
    (do
      (set! (.-onmessage chan) (receive-transit-msg! receive-handler))
      (set! (.-onopen chan) open-handler)
      (set! (.-onerror chan) close-handler)
      (set! (.-onclose chan) close-handler)
      (reset! ws-chan-game chan)
      (println "Game websocket connection established with: " url))
    (throw (js/Error. "Game websocket connection failed!"))))

(defn handle-message
  "updates the messages"
  [new-message]
  (cond
    (string? new-message) (dispatch [:messages new-message])
    (contains? new-message :step) (dispatch [:remote-game-state new-message])
    (contains? new-message :user-key) (dispatch [:update-game-info new-message])
    (contains? new-message :highscores) (dispatch [:update-game-info new-message])
    :default (println new-message)
    ))

(defn initsocket []
  (make-game-websocket! (str "ws://" (.-host js/location) "/game") handle-message))

(defn send-transit-game!
  [msg]
  (if (or (nil? @ws-chan-game) (> (.-readyState @ws-chan-game) 1)) (initsocket))
  (if (= (.-readyState @ws-chan-game) 1)
    (.send @ws-chan-game (t/write json-writer msg))
    (a/go-loop []
               (if @ws-chan-game
                 (let [ready-state (.-readyState @ws-chan-game)]
                   (cond
                     (= ready-state 0) (do (a/<! (a/timeout 50)) (recur))
                     (= ready-state 1) (.send @ws-chan-game (t/write json-writer msg))
                     :default (dispatch [:messages (str "Could not send: '" msg "' to the server")])
                     ))
                 (dispatch [:messages (str "Could not send: '" msg "' to the server")]))
               )))

