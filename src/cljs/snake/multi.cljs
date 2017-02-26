(ns snake.multi
  (:require [cognitect.transit :as t]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [subscribe dispatch]]))

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
  [user-key game-state]
  (let [board (:board game-state)
        snakes (:snakes game-state)
        sweets (:sweets game-state)
        [width height] board
        is-head-position-own #(= (first (get-in snakes [user-key :body])) %)
        is-rest-position-own (into #{} (rest (get-in snakes [user-key :body])))
        other-snake-head (atom #{})
        other-snake-rest (atom #{})
        update-atoms (doseq [[k v] snakes] (if-not (= k user-key)
                                             (do
                                               (swap! other-snake-head #(conj % (first (:body v))))
                                               (swap! other-snake-rest #(into % (rest (:body v)))))
                                             ))
        is-head-position-other (into #{} @other-snake-head)
        is-rest-position-other (into #{} @other-snake-rest)
        sweets-positions (into #{} (:locations sweets))
        cells (for [y (range height)]
                (into [:div.row.flex-items-xs-center]
                      (for [x (range width)
                            :let [current-pos [x y]]]
                        (cond
                          (is-head-position-own current-pos) [:div.col-xs.board-element.snake-on-cell-own.head-of-snake]
                          (is-head-position-other current-pos) [:div.col-xs.board-element.snake-on-cell-other.head-of-snake]
                          (is-rest-position-own current-pos) [:div.col-xs.board-element.snake-on-cell-own]
                          (is-rest-position-other current-pos) [:div.col-xs.board-element.snake-on-cell-other]
                          (sweets-positions current-pos) [:div.col-xs.board-element.sweet]
                          :default [:div.col-xs.board-element.cell]))))]
    (into [:div.container] cells)))

(defn render-main
  "Renders the main view, either the login, or the board"
  []
  (fn []
    (let [remote-game-state (subscribe [:remote-game-state])]
      (if-let [user-key (:user-key @game-info)]
        (render-board user-key @remote-game-state)))))

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
