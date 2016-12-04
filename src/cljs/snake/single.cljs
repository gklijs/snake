(ns snake.single
    (:require [re-frame.core :refer [subscribe dispatch]]))

;; -- Pure functions ---------------------------------------------------------

(defn rand-free-position
  "This function takes the snake, locations of the sweets and the board-size as arguments, and
  returns a random position not colliding with the snake body or sweets"
  [snake locations [x y]]
  (let [positions-set (concat  (into #{} (:body snake)) locations)
        board-positions (for [x-pos (range x)
                              y-pos (range y)]
                          [x-pos y-pos])
        free-position? (atom (rand-nth board-positions))]
        (while (some #(= @free-position? %) positions-set)(reset! free-position? (rand-nth board-positions)))
        @free-position?))

(defn valid-head
  "Change the value of the head if it may run out of the board"
  [head board]
  (cond
    (= (first head) -1) [(- (first board) 1) (second head)]
    (= (first head) (first board)) [0 (second head)]
    (= (second head) -1) [(first head) (- (second board) 1)]
    (= (second head) (second board)) [(first head) 0]
    :else head))

(defn grow-snake
  "Computes a value for the tail position and returns whole snake"
  [{:keys [body] :as snake}]
  (let [last-2 (take-last 2 body)]
    (let [direction (mapv - (second last-2) (first last-2))]
      (assoc snake :body (conj body (mapv + (last body) direction))))))

(defn rand-snake
    "this function creates a new random snake, based only on the board"
    [[x y]]
    (let [valid-directons [[0 1][0 -1][-1 0][1 0]]
            snake (atom {})
            start-position [[(rand-int x)(rand-int y)]]]
    (reset! snake (assoc @snake :direction (rand-nth valid-directons)))
    (reset! snake (assoc @snake :body (conj start-position (valid-head (mapv + (:direction @snake) start-position) [x y]))))
    (dotimes [n 4] (swap! snake grow-snake))
    @snake
    ))

(defn move-snake
  "Move the whole snake positions and directions of all snake elements"
  [{:keys [direction body] :as snake} board]
  (let [head-new-position (valid-head (mapv + direction (first body)) board) ]
    (update-in snake [:body] #(into [] (drop-last (cons head-new-position body))))))

(defn collisions
   "Check whether the snake is hit by himself, causing it to die"
  [snake]
  (let [head (first (:body snake))
        body (rest (:body snake))]
    (some #(= head %) body)))

(defn remove-sweet
  "Remove a certain sweet cause it's been eaten"
  [{:keys [locations] :as sweets} sweet]
  (assoc sweets :locations (remove #{sweet} locations)))

(defn process-movement
  "Handles movement stuff"
  [{:keys [snake sweets] :as db-before}]
  (let [db (assoc db-before :direction-changed false)
        sweet (some #{(first (:body snake))}  (:locations sweets))]
    (if sweet
      (-> db
          (update :snake grow-snake)
          (update :points inc)
          (update :sweets remove-sweet sweet))
      db)))

(defn handle-sweets
  "Adds new sweet if there are less sweets than the max number, removes the oldest one otherwhise"
  [{:keys [max-number locations] :as sweets} snake board]
  (if (> max-number (count locations))
    (update-in sweets [:locations] #(conj locations (rand-free-position snake locations board)))
    (update-in sweets [:locations] #(remove #{(last locations)}  locations))))

(defn pop-stored-direction
  [{:keys [stored-direction direction-changed] :as db}]
  (if (false? direction-changed)
    (if (false? stored-direction)
      db
      (-> db
          (assoc-in [:snake :direction] stored-direction)
          (assoc :stored-direction false)))
    db))

(defn next-state
    "gives the next state of the db"
    [{:keys [snake game-running board sweets] :as db}]
    (if (:game-running? db)
          (if (collisions snake)
            (assoc-in db [:game-running?] false)
            (-> db
                (pop-stored-direction)
                (update :snake move-snake board)
                (as-> after-move
                      (process-movement after-move))
                (update :sweets handle-sweets snake board)))
          db))

;; -- View Components ---------------------------------------------------------

(defn render-board
  "Renders the board area of the game"
  []
  (let [board (subscribe [:board])
        snake (subscribe [:snake])
        sweets (subscribe [:sweets])]
    (fn []
      (let [[width height] @board
            snake-positions (into #{} (:body @snake))
            sweet-positions (into #{} (:locations @sweets))
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
  (let [points (subscribe [:points])]
    (fn
      []
      [:div.score (str "Score: " @points)])))

(defn start-stop
  "Renders the button to start/pause the game"
  []
  (let [game-running? (subscribe [:game-running?])]
    (fn
      []
      [:button.btn.btn-secondary {:type "button" :on-click #(dispatch [:switch-game-running])}
      (str (if @game-running? "Pause" "Start"))
      ])))

(defn game
  "The game rendering function"
  []
  [:div
   [:div.container.controls [:div.row.flex-items-xs-center
       [:div.col-xs [score]]
       [:div.col-xs [start-stop]]
    ]]
   [render-board]])