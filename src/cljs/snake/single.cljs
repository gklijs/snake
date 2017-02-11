(ns snake.single
  (:require [re-frame.core :refer [subscribe dispatch]]))

;; -- View Components ---------------------------------------------------------

(defn render-board
  "Renders the board area of the game"
  []
  (let [local-game-state (subscribe [:local-game-state])]
    (fn []
      (let [board (:board @local-game-state)
            snake (:snake @local-game-state)
            sweets (:sweets @local-game-state)
            [width height] board
            snake-positions (into #{} (:body snake))
            sweet-positions (into #{} (:locations sweets))
            cells (for [y (range height)]
                    (into [:div.row.flex-items-xs-center]
                          (for [x (range width)
                                :let [current-pos [x y]]]
                            (cond
                              (snake-positions current-pos) [:div.col-xs.board-element.snake-on-cell]
                              (sweet-positions current-pos) [:div.col-xs.board-element.sweet]
                              :default [:div.col-xs.board-element.cell]))))]
        (into [:div.container]
              cells)))))

(defn score
  "Renders the player's score"
  []
  (let [local-game-state (subscribe [:local-game-state])]
    (fn
      []
      [:div.score (str "Score: " (:points @local-game-state))])))

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
   [:div.container.controls [:div.row.flex-items-xs-center
                             [:div.col-xs [score]]
                             [:div.col-xs [start-stop]]
                             ]]
   [render-board]])