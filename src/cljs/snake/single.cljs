(ns snake.single
  (:require [re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as reagent :refer [atom]]
            [utils.sketch :refer [sketch-component draw-game-state]]))

;; -- View Components ---------------------------------------------------------
(defonce enlarge (atom 6))
(defonce show-names (atom false))
(defonce last-drawn-step (atom nil))
(defonce interval (atom nil))

(defn draw
  []
  (let [game-state (subscribe [:local-game-state])
        step (:step @game-state)]
    (when (not (= @last-drawn-step step))
      (draw-game-state @game-state :0 @show-names @enlarge)
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
      (do (js/clearInterval js/window @interval) (reset! interval nil)))))

(defn view
  "The game rendering function"
  []
  (do
    (if (nil? @interval)
      (reset! interval (js/setInterval #(update-function) 150)))
    [:div
     [:div.container.controls [:div.d-flex.justify-content-end
                               [:div.mr-auto.p-2 [score]]
                               (toggle-name)
                               [:div.p-2 [start-stop]]
                               ]]
     [:div.container {:id "canvas-container"} [sketch-component get-canvas-size :renderer :p2d :draw draw]]]))