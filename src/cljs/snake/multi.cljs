(ns snake.multi
  (:require [cognitect.transit :as t]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [subscribe dispatch]]
            [quil.core :as q :include-macros true]
            [utils.sketch :refer [sketch-component ellipse draw-snake]]))

(defonce ws-chan-chat (atom nil))
(defonce ws-chan-game (atom nil))
(defonce game-info (atom {}))
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
  (cond
    (contains? new-message :board) (dispatch [:remote-game-state new-message])
    (contains? new-message :user-key) (swap! game-info #(merge % new-message))
    (string? new-message) (dispatch [:messages new-message])
    :default (logjs new-message)
    ))

(defn send-direction
  "sends the new direction to the server, using the websocker"
  [new-direction]
  (if-let [username (:username @game-info)]
    (send-transit-game! {:new-direction new-direction})
    (update-messages! "User not registered, movement will not be send")))

(defn register
  "Validates the input and dend message to server when ok"
  [key username password]
  (if (= (.-keyCode key) 13)
    (if (> (count @username) 7)
      (if (> (count @password) 7)
        (let [info-map {:username @username :password @password}]
          (send-transit-game! info-map)
          (swap! game-info #(merge % info-map)))
        (update-messages! "Password should have a minimal of 8 characters"))
      (update-messages! "Username should have a minimal of 8 characters"))
    ))

(defn start
  "Renders the button to start the game, after the snake has died"
  [user-key game-state]
  (if (nil? (get-in game-state [:snakes user-key]))
    [:div.p-2
     [:button.btn.btn-secondary {:type "button" :on-click #(send-transit-game! {:start true})} "Start"]]))

(defn game-input []
  (let [username (atom nil)
        password (atom nil)
        remote-game-state (subscribe [:remote-game-state])]
    (fn []
      (if-let [user-key (:user-key @game-info)]
        [:div.container.controls [:div.d-flex.justify-content-end
                                  [:div.mr-auto.p-2 [:div.score (str "Score: " (get-in @remote-game-state [:snakes user-key :points]))]]
                                  (start user-key @remote-game-state)
                                  ]]
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
           :on-key-down #(register % username password)}]])
      )))



;; -- View Components ---------------------------------------------------------
(defonce enlarge (atom 6))
(defonce last-drawn-step (atom nil))

(defn message-list
  "renders the list of messages"
  []
  (let [messages (subscribe [:messages])]
    [:ul
     (for [[i message] (map-indexed vector @messages)]
       ^{:key i}
       [:li message])]))

(defn draw
  [user-key]
  (let [game-state (subscribe [:remote-game-state])
        step (:step @game-state)]
    (when (not (= @last-drawn-step step))
      (q/background 30)
      (q/fill 119 255 51)
      (doseq [location (get-in @game-state [:sweets :locations])] (ellipse location @enlarge))
      (doseq [[k snake] (get-in @game-state [:snakes])]
        (if (= k user-key) (q/fill 227 11 92) (q/fill 42 171 210))
        (draw-snake snake @enlarge)
      (reset! last-drawn-step step)
      ))))

(defn get-canvas-size
  []
  (if-let [canvas-container (js/document.getElementById "canvas-container")]
    (if-let [width (.-offsetWidth canvas-container)]
      (let [canvas-width (- width 30)
            canvas-heigth (* canvas-width 0.8)]
        (reset! enlarge (/ canvas-width 50))
        [canvas-width canvas-heigth])
      [300 240])
    [300 240]))

(defn render-main
  "Renders the main view, either the login, or the board"
  []
  (fn []
    (let [remote-game-state (subscribe [:remote-game-state])]
      (if-let [user-key (:user-key @game-info)]
        [:div.container {:id "canvas-container"} [sketch-component get-canvas-size :renderer :p2d :draw #(draw user-key) :settings #(q/smooth 4)]]))))

(defn view
  "The multi rendering function"
  []
  [:div.container.row
   [:div.col-md-8
    [game-input]
    [render-main]]
   [:div.col-md-4
    [message-list]
    [message-input]]])

(defn initsockets []
  (make-chat-websocket! (str "ws://" (.-host js/location) "/chat") update-messages!)
  (make-game-websocket! (str "ws://" (.-host js/location) "/game") update-messages!))
