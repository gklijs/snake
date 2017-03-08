(ns snake.multi
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [subscribe dispatch]]
            [utils.sketch :refer [sketch-component draw-game-state]]
            [utils.websockets :refer [send-transit-game! send-transit-chat!]]))

(defonce enlarge (atom 6))
(defonce last-drawn-step (atom nil))
(defonce show-names (atom false))

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
                         (send-transit-chat! @value)
                         (reset! value nil))}]]
      )))

(defn send-direction
  "sends the new direction to the server, using the websocker"
  [new-direction]
  (let [game-info (subscribe [:game-info])]
    (if-let [user-key (:user-key @game-info)]
      (send-transit-game! {:new-direction new-direction})
      (dispatch [:messages "User not registered, movement will not be send"]))))

(defn register
  "Validates the input and dend message to server when ok"
  [key username password]
  (if (= (.-keyCode key) 13)
    (if (> (count @username) 7)
      (if (> (count @password) 7)
        (let [info-map {:username @username :password @password}]
          (send-transit-game! info-map))
        (dispatch [:messages "Password should have a minimal of 8 characters"]))
      (dispatch [:messages "Username should have a minimal of 8 characters"]))
    ))

(defn toggle-name
  "Renders the button to switch showing the names on and off"
  []
  (if @show-names
    [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! show-names false)} "Hide names"]]
    [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! show-names true)} "Show names"]]))

(defn start
  "Renders the button to start the game, after the snake has died"
  [user-key game-state]
  (if (nil? (get-in game-state [:snakes user-key]))
    [:div.p-2
     [:button.btn.btn-secondary {:type "button" :on-click #(send-transit-game! {:start true})} "Start"]]))

(defn game-input []
  (let [username (atom nil)
        password (atom nil)
        remote-game-state (subscribe [:remote-game-state])
        game-info (subscribe [:game-info])]
    (fn []
      (if-let [user-key (:user-key @game-info)]
        [:div.container.controls [:div.d-flex.justify-content-end
                                  [:div.mr-auto.p-2 [:div.score (str "Score: " (get-in @remote-game-state [:snakes user-key :points]))]]
                                  (toggle-name)
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

(defn draw
  [user-key]
  (let [game-state (subscribe [:remote-game-state])
        step (:step @game-state)]
    (when (not (= @last-drawn-step step))
      (draw-game-state @game-state user-key @show-names @enlarge)
      (reset! last-drawn-step step)
      )))

(defn get-canvas-size
  []
  (if-let [canvas-container (js/document.getElementById "canvas-container")]
    (if-let [width (.-offsetWidth canvas-container)]
      (let [excess-width (mod width 50)
            canvas-width (- width excess-width)
            canvas-heigth (* canvas-width 0.8)]
        (reset! enlarge (/ canvas-width 50))
        [canvas-width canvas-heigth])
      [300 240])
    [300 240]))

(defn render-main
  "Renders the main view, either the login, or the board"
  []
  (fn []
    (let [game-info (subscribe [:game-info])]
      (if-let [user-key (:user-key @game-info)]
        [:div.container {:id "canvas-container"} [sketch-component get-canvas-size :renderer :p2d :draw #(draw user-key)]]))))

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
