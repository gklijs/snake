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
      {:type :text
       :placeholder "type in a message and press enter"
       :value @value
       :on-change #(reset! value (-> % .-target .-value))
       :on-key-down
       #(when (= (.-keyCode %) 13)
          (send-transit-msg! @value)
          (reset! value nil))}])))

(defn game-input []
  (let [value (atom nil)]
    (fn []
      [:input.form-control
       {:type :text
        :placeholder "type in something and press enter"
        :value @value
        :on-change #(reset! value (-> % .-target .-value))
        :on-key-down
        #(when (= (.-keyCode %) 13)
           (send-transit-game! {:user @value})
           (reset! value nil))}])))

(defn update-messages!
    "updates the messages"
    [new-message]
    (dispatch [:messages new-message]))

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
   [:div.col-sm-6
    [game-input]]]]])

(defn initsockets []
  (make-chat-websocket! (str "ws://" (.-host js/location) "/chat") update-messages!)
  (make-game-websocket! (str "ws://" (.-host js/location) "/game") update-messages!))
