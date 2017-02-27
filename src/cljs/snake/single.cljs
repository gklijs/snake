(ns snake.single
  (:require [re-frame.core :refer [subscribe dispatch]]))

;; -- View Components ---------------------------------------------------------

(defn render-board
  "Renders the board area of the game"
  []
  (let [game-state (subscribe [:local-game-state])]
    (fn []
      (let [board (:board @game-state)
            snakes (:snakes @game-state)
            sweets (:sweets @game-state)
            [width height] board
            is-head-position-own #(= (first (get-in snakes [:0 :body])) %)
            is-head-position-other (into #{} (conj (for [[k v]snakes] (if-not (= k :0) (first (:body v))))))
            is-rest-position-own (into #{} (rest (get-in snakes [:0 :body])))
            other-snake-rest (atom #{})
            update-atoms (doseq [[k v] snakes] (if-not (= k :0) (swap! other-snake-rest #(into % (rest (:body v))))))
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
   [:div.container.controls [:div.d-flex.justify-content-end
                             [:div.mr-auto.p-2 [score]]
                             [:div.p-2 [start-stop]]
                             ]]
   [render-board]])