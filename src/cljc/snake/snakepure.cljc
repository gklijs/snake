(ns snake.snakepure)

(defn valid-head
  "Change the value of the head if it may run out of the board"
  [head board]
  (cond
    (= (first head) -1) [(- (first board) 1) (second head)]
    (= (first head) (first board)) [0 (second head)]
    (= (second head) -1) [(first head) (- (second board) 1)]
    (= (second head) (second board)) [(first head) 0]
    :else head))

(defn rand-free-position
  "This function takes the snake, locations of the sweets and the board-size as arguments, and
  returns a random position not colliding with the snake body or sweets"
  [snake locations [x y]]
  (let [positions-set (concat (into #{} (:body snake)) locations)
        board-positions (for [x-pos (range x)
                              y-pos (range y)]
                          [x-pos y-pos])
        free-position? (atom (rand-nth board-positions))]
    (while (some #(= @free-position? %) positions-set) (reset! free-position? (rand-nth board-positions)))
    @free-position?))

(defn grow-snake
  "Computes a value for the tail position and returns whole snake"
  [{:keys [body] :as snake}]
  (let [last-2 (take-last 2 body)]
    (let [direction (mapv - (second last-2) (first last-2))]
      (assoc snake :body (conj body (mapv + (last body) direction))))))

(defn rand-snake
  "this function creates a new random snake, based only on the board"
  [[x y]]
  (let [valid-directons [[0 1] [0 -1] [-1 0] [1 0]]
        start-position [[(+ 5 (rand-int (- x 10))) (+ 5 (rand-int (- y 10)))]]
        direction (rand-nth valid-directons)]
    {
     :direction direction
     :body (conj start-position (mapv + (last (:first start-position)) direction))
     }))

(defn move-snake
  "Move the whole snake positions and directions of all snake elements"
  [{:keys [direction body] :as snake} board]
  (let [head-new-position (valid-head (mapv + direction (first body)) board)]
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

(defn feed-snakes
  "Check if there is some snake head on a sweet, and let it eat it, when it is the case"
  [{:keys [snake sweets] :as game-state}]
  (let [sweet (some #{(first (:body snake))} (:locations sweets))]
    (if sweet
      (-> game-state
          (update :snake grow-snake)
          (update :points inc)
          (update :sweets remove-sweet sweet))
      game-state)))

(defn handle-sweets
  "Adds new sweet if there are less sweets than the max number, removes the oldest one otherwhise"
  [{:keys [max-number locations] :as sweets} snake board]
  (if (> max-number (count locations))
    (update-in sweets [:locations] #(conj locations (rand-free-position snake locations board)))
    (update-in sweets [:locations] #(remove #{(last locations)} locations))))

(defn pop-stored-direction
  [{:keys [stored-direction direction-changed] :as game-state}]
  (if (true? direction-changed)
      (-> game-state
          (assoc-in [:snake :direction] stored-direction)
          (assoc :stored-direction false)
          (assoc :direction-changed false))
    game-state))

(defn next-state
  "gives the next state of the game-state"
  [{:keys [snake game-running? board sweets] :as game-state}]
  (if (true? game-running?)
    (if (collisions snake)
      (assoc-in game-state [:game-running?] false)
      (-> game-state
          (pop-stored-direction)
          (update :snake move-snake board)
          (feed-snakes)
          (update :sweets handle-sweets snake board)))
    game-state))

(defn initial-state
  "Gives the initial game state"
  [number-of-snakes]
  (let [board [50 40]]
    {
     :board             board
     :snake             (rand-snake board)
     :sweets            {:max-number 20
                         :locations  []}
     :game-running?     false
     :direction-changed false
     :stored-direction  false
     :points            0
     }))