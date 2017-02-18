(ns snake.multi
  (:require [cognitect.transit :as t]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [subscribe dispatch]]))

(defonce ws-chan-chat (atom nil))
(defonce ws-chan-game (atom nil))
(def json-reader (t/reader :json))
(def json-writer (t/writer :json))

(defn receive-transit-msg!
  [update-fn]
  (fn [msg]
    (update-fn
      (->> msg .-data (t/read json-reader)))))

(defn send-transit-msg!
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

(defn message-input []
  (let [value (atom nil)]
    (fn []
      [:input.form-control
       {:type        :text
        :placeholder "type in a message and press enter to send to everybody"
        :value       @value
        :on-change   #(reset! value (-> % .-target .-value))
        :on-key-down
                     #(when (= (.-keyCode %) 13)
                        (send-transit-msg! @value)
                        (reset! value nil))}])))

(defn update-messages!
  "updates the messages"
  [new-message]
  (dispatch [:messages new-message]))

(defn register
  "Validates the input and dend message to server when ok"
  [key username password]
  (if (= (.-keyCode key) 13)
    (if (> (count @username) 7)
      (if (> (count @password) 7)
        (do
          (send-transit-game! {:username @username :password @password})
          (dispatch [:update-game-info {:username @username}])
          (reset! username nil)
          (reset! password nil))
        (update-messages! "Password should have a minimal of 8 characters"))
      (update-messages! "Username should have a minimal of 8 characters"))
    ))

(defn game-input []
  (let [username (atom nil) password (atom nil)]
    (fn []
      [:div.col-sm-6
       [:input.form-control
        {:type        :text
         :placeholder "type in username, should be a minimal of 8 characters"
         :value       @username
         :on-change   #(reset! username (-> % .-target .-value))
         :on-key-down #(register % username password)}]
       [:input.form-control
        {:type        :password
         :placeholder "type in password and press enter to register with server"
         :value       @password
         :on-change   #(reset! password (-> % .-target .-value))
         :on-key-down #(register % username password)}]]
      )))



;; -- View Components ---------------------------------------------------------

(defn message-list
  "renders the list of messages"
  []
  (let [messages (subscribe [:messages])]
    [:ul
     (for [[i message] (map-indexed vector @messages)]
       ^{:key i}
       [:li message])]))

(defn view
  "The multi rendering function"
  []
  [:div
   [:div.container
    [:div.row
     [:div.col-md-12
      [:h2 "Welcome to chat"]]]
    [:div.row
     [:div.col-sm-6
      [message-list]]]
    [:div.row
     [:div.col-sm-6
      [message-input]]]
    [:div.row
     [game-input]]]])

(defn initsockets []
  (make-chat-websocket! (str "ws://" (.-host js/location) "/chat") update-messages!)
  (make-game-websocket! (str "ws://" (.-host js/location) "/game") update-messages!))
