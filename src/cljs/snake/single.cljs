(ns snake.single
  (:require [re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as reagent :refer [atom]]
            [utils.sketch :refer [sketch-component draw-game-state]]))

;; -- View Components ---------------------------------------------------------
(defonce show-names (atom false))
(defonce show-scores (atom false))
(defonce last-drawn-step (atom nil))
(defonce interval (atom nil))

(defn draw
  [enlarge]
  (let [game-state (subscribe [:local-game-state])
        step (:step @game-state)]
    (when (not (= @last-drawn-step step))
      (draw-game-state @game-state :0 @show-names @show-scores enlarge)
      (reset! last-drawn-step step)
      )))

(defn score
  "Renders the player's score"
  []
  (let [local-game-state (subscribe [:local-game-state :snakes :0 :points])]
    [:div.score (str "Score: " (get-in @local-game-state [:snakes :0 :points]))]))

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

(defn start-stop
  "Renders the button to start/pause the game"
  []
  (let [local-game-state (subscribe [:local-game-state])]
    (fn
      []
      [:button.btn.btn-secondary {:type "button" :on-click #(dispatch [:switch-game-running])}
       (str (if (:game-running? @local-game-state) "Pause" "Start"))
       ])))

(defn update-function
  "updated the game state, or switch off the interval when another view is loaded"
  []
  (let [sel-menu-item (subscribe [:sel-menu-item])]
    (if (= @sel-menu-item "single")
      (dispatch [:next-state])
      (do (js/clearInterval @interval) (reset! interval nil)))))

(defn view
  "The game rendering function"
  []
  (do
    (if (nil? @interval)
      (reset! interval (js/setInterval #(update-function) 150)))
    [:div
     [:div.container.controls [:div.d-flex.justify-content-end
                               [:div.mr-auto.p-2 [score]]
                               (toggle-score)
                               (toggle-name)
                               [:div.p-2 [start-stop]]
                               ]]
     [sketch-component draw]]))