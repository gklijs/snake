(ns snake.multi
  (:require [cljs.core.async :as a]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [subscribe dispatch]]
            [quil.core :refer [frame-rate]]
            [snake.validation :refer [valid-registration-map? valid-new-direction-map?]]
            [utils.sketch :refer [sketch-component]]
            [utils.websocket :refer [send-transit-game!]])
  (:require-macros [cljs.core.async.macros :as a]))

(defonce show-names (atom false))
(defonce show-scores (atom false))

(defn setup-function
  [user-key]
  (frame-rate 7)
  {:game-state nil :user-key user-key :show-names show-names :show-scores show-scores})

(defn update-function
  [state]
  (let [game-state (subscribe [:remote-game-state])
        last-step (:step @game-state)]
    (a/go
      (while
        (= last-step (:step @game-state))
        (a/<! (a/timeout 10))))
    (assoc state :game-state @game-state)))

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
                         (send-transit-game! @value)
                         (reset! value nil))}]]
      )))

(defn send-direction
  "sends the new direction to the server, using the websocker"
  [new-direction]
  (let [game-info (subscribe [:game-info])]
    (if-let [user-key (:user-key @game-info)]
      (let [direction-map {:new-direction new-direction}
            validation (valid-new-direction-map? direction-map)]
        (if (first validation)
          (send-transit-game! {:new-direction new-direction})
          (dispatch [:messages (str "error: " (second validation))])))
      (dispatch [:messages "User not registered, movement will not be send"]))))

(defn register
  "Validates the input and dend message to server when ok"
  [key username password]
  (if (= (.-keyCode key) 13)
    (let [registration-map {:username @username :password @password}
          validation (valid-registration-map? registration-map)]
      (if (first validation)
        (do
          (dispatch [:update-game-info {:registration-map registration-map}])
          (send-transit-game! registration-map)
          )
        (dispatch [:messages (str "error: " (second validation))])))))

(defn toggle-name
  "Renders the button to switch showing the names on and off"
  []
  (if @show-names
    [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! show-names false)} "Hide names"]]
    [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! show-names true)} "Show names"]]))

(defn toggle-score
  "Renders the button to switch showing the names on and off"
  []
  (if @show-scores
    [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! show-scores false)} "Hide scores"]]
    [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! show-scores true)} "Show scores"]]))

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
                                  (toggle-score)
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

(defn render-main
  "Renders the main view, either the login, or the board"
  []
  (fn []
    (let [game-info (subscribe [:game-info])]
      (if-let [user-key (:user-key @game-info)]
        [sketch-component #(setup-function user-key) update-function]))))

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
