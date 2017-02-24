(ns snake.multi
  (:require [cognitect.transit :as t]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [subscribe dispatch]]))

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
      [:div.d-flex
       [:input.form-control
        {:type        :text
         :placeholder "type in a message and press enter to send to everybody"
         :value       @value
         :on-change   #(reset! value (-> % .-target .-value))
         :on-key-down
                      #(when (= (.-keyCode %) 13)
                         (send-transit-msg! @value)
                         (reset! value nil))}]]
)))

(defn update-messages!
  "updates the messages"
  [new-message]
  (logjs new-message)
  (cond
    (contains? new-message :board) (dispatch [:remote-game-state new-message])
    (contains? new-message :user-key) (dispatch [:update-game-info new-message])
    (string? new-message) (dispatch [:messages new-message])
    :default (logjs new-message)
    ))

(defn send-direction
  "sends the new direction to the server, using the websocker"
  [new-direction game-info]
  (if-let [username (:username game-info)]
    (send-transit-game! {:new-direction new-direction})
    (update-messages! "User not registered, movement will not be send")))

(defn register
  "Validates the input and dend message to server when ok"
  [key username password]
  (if (= (.-keyCode key) 13)
    (if (> (count @username) 7)
      (if (> (count @password) 7)
        (do
          (send-transit-game! {:username @username :password @password})
          (dispatch [:update-game-info {:username @username :password @password}])
          (reset! username nil)
          (reset! password nil))
        (update-messages! "Password should have a minimal of 8 characters"))
      (update-messages! "Username should have a minimal of 8 characters"))
    ))

(defn game-input []
  (let [username (atom nil) password (atom nil)]
    (fn []
      [:div.flex-column
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

(defn render-board
  "Renders the board area of the game"
  []
  (let [remote-game-state (subscribe [:remote-game-state])
        game-info (subscribe [:game-info])]
    (fn []
      (let [board (:board @remote-game-state)
            snakes (:snakes @remote-game-state)
            sweets (:sweets @remote-game-state)
            user-key (:user-key @game-info)
            [width height] board
            snake-head-position-0 #(= (first (get-in snakes [user-key :body])) %)
            snake-head-position-1 #(= (first (get-in snakes [:1 :body])) %)
            snake-head-position-2 #(= (first (get-in snakes [:2 :body])) %)
            snake-head-position-3 #(= (first (get-in snakes [:3 :body])) %)
            snake-head-position-4 #(= (first (get-in snakes [:4 :body])) %)
            snake-rest-positions-0 (into #{} (rest (get-in snakes [user-key :body])))
            snake-rest-positions-1 (into #{} (rest (get-in snakes [:1 :body])))
            snake-rest-positions-2 (into #{} (rest (get-in snakes [:2 :body])))
            snake-rest-positions-3 (into #{} (rest (get-in snakes [:3 :body])))
            snake-rest-positions-4 (into #{} (rest (get-in snakes [:4 :body])))
            sweets-positions (into #{} (:locations sweets))
            cells (for [y (range height)]
                    (into [:div.row.flex-items-xs-center]
                          (for [x (range width)
                                :let [current-pos [x y]]]
                            (cond
                              (snake-head-position-0 current-pos) [:div.col-xs.board-element.snake-on-cell-0.head-of-snake]
                              (snake-head-position-1 current-pos) [:div.col-xs.board-element.snake-on-cell-1.head-of-snake]
                              (snake-head-position-2 current-pos) [:div.col-xs.board-element.snake-on-cell-2.head-of-snake]
                              (snake-head-position-3 current-pos) [:div.col-xs.board-element.snake-on-cell-3.head-of-snake]
                              (snake-head-position-4 current-pos) [:div.col-xs.board-element.snake-on-cell-4.head-of-snake]
                              (snake-rest-positions-0 current-pos) [:div.col-xs.board-element.snake-on-cell-0]
                              (snake-rest-positions-1 current-pos) [:div.col-xs.board-element.snake-on-cell-1]
                              (snake-rest-positions-2 current-pos) [:div.col-xs.board-element.snake-on-cell-2]
                              (snake-rest-positions-3 current-pos) [:div.col-xs.board-element.snake-on-cell-3]
                              (snake-rest-positions-4 current-pos) [:div.col-xs.board-element.snake-on-cell-4]
                              (sweets-positions current-pos) [:div.col-xs.board-element.sweet]
                              :default [:div.col-xs.board-element.cell]))))]
        (into [:div.container] cells)))))

(defn view
  "The multi rendering function"
  []
    [:div.container.row
     [:div.col-md-8
      [render-board]]
     [:div.col-md-4
      [message-list]
      [message-input]
      [game-input]]])

(defn initsockets []
  (make-chat-websocket! (str "ws://" (.-host js/location) "/chat") update-messages!)
  (make-game-websocket! (str "ws://" (.-host js/location) "/game") update-messages!))
