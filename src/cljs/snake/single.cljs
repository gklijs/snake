(ns snake.single
  (:require [re-frame.core :refer [subscribe dispatch]]))

;; -- View Components ---------------------------------------------------------

(defn render-board
  "Renders the board area of the game"
  []
  (let [local-game-state (subscribe [:local-game-state])]
    (fn []
      (let [board (:board @local-game-state)
            snakes (:snakes @local-game-state)
            sweets (:sweets @local-game-state)
            [width height] board
            snake-head-position-0 #(= (first (get-in snakes [:0 :body])) %)
            snake-head-position-1 #(= (first (get-in snakes [:1 :body])) %)
            snake-head-position-2 #(= (first (get-in snakes [:2 :body])) %)
            snake-head-position-3 #(= (first (get-in snakes [:3 :body])) %)
            snake-head-position-4 #(= (first (get-in snakes [:4 :body])) %)
            snake-rest-positions-0 (into #{} (rest (get-in snakes [:0 :body])))
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

(defn score
  "Renders the player's score"
  []
  (let [local-game-state (subscribe [:local-game-state])]
    (fn
      []
      [:div.score (str "Score: " (get-in @local-game-state [:snakes :0 :points]))])))

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