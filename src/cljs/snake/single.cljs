(ns snake.single
  (:require [re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as reagent :refer [atom]]
            [quil.core :as q :include-macros true]
            [utils.sketch :refer [sketch-component ellipse draw-snake]]))

;; -- View Components ---------------------------------------------------------
(defonce enlarge (atom 6))
(defonce last-drawn-step (atom nil))

(defn draw
  []
  (let [game-state (subscribe [:local-game-state])
        step (:step @game-state)]
    (when (not (= @last-drawn-step step))
      (q/background 30)
      (q/fill 119 255 51)
      (doseq [location (get-in @game-state [:sweets :locations])] (ellipse location @enlarge))
      (doseq [[k snake] (get-in @game-state [:snakes])]
        (if (= k :0) (q/fill 227 11 92) (q/fill 42 171 210))
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

(defn score
  "Renders the player's score"
  []
  (let [local-game-state (subscribe [:local-game-state :snakes :0 :points])]
    [:div.score (str "Score: " (get-in @local-game-state [:snakes :0 :points]))]))

(defn start-stop
  "Renders the button to start/pause the game"
  []
  (let [local-game-state (subscribe [:local-game-state])]
    (fn
      []
      [:button.btn.btn-secondary {:type "button" :on-click #(dispatch [:switch-game-running])}
       (str (if (:game-running? @local-game-state) "Pause" "Start"))
       ])))

(defn view
  "The game rendering function"
  []
  [:div
   [:div.container.controls [:div.d-flex.justify-content-end
                             [:div.mr-auto.p-2 [score]]
                             [:div.p-2 [start-stop]]
                             ]]
   [:div.container {:id "canvas-container"} [sketch-component get-canvas-size :renderer :p2d :draw draw :settings #(q/smooth 4)]]])