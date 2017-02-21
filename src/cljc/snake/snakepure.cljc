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
     :direction          direction
     :body               (conj start-position
                               (mapv - (first start-position) (mapv * [1 1] direction))
                               (mapv - (first start-position) (mapv * [2 2] direction))
                               (mapv - (first start-position) (mapv * [3 3] direction))
                               (mapv - (first start-position) (mapv * [4 4] direction)))
     :direction-changed? false
     :stored-direction   nil
     :points             0
     }))

(defn move-snake
  "Move the whole snake positions and directions of all snake elements"
  [{:keys [direction body] :as snake} board]
  (let [head-new-position (valid-head (mapv + direction (first body)) board)]
    (assoc snake :body (drop-last (cons head-new-position body)))))

(defn add-points-for-killing
  "Adds the point for killing another snake"
  [current-points]
  (+ current-points 5))

(defn handle-collisions
  "Removes any snake when the head hits another snake or itself, add 5 points to the snake responsible for killiing"
  [snakes]
  (let [new-snakes (atom snakes)]
    (doseq [[main-key main-snake] snakes]
      (let [head (first (get main-snake :body))]
        (doseq [[sub-key sub-snake] snakes]
          (if (= main-key sub-key)
            (when (some #(= head %) (rest (get sub-snake :body)))
              (swap! new-snakes #(dissoc % main-key)))
            (when (some #(= head %) (get sub-snake :body))
              (swap! new-snakes #(dissoc % main-key))
              (if (get @new-snakes sub-key)
                (swap! new-snakes #(update-in % [sub-key :points] add-points-for-killing)))
              )))
        ))
    @new-snakes))

(defn remove-sweet
  "Remove a certain sweet cause it's been eaten"
  [{:keys [locations] :as sweets} sweet]
  (assoc sweets :locations (remove #{sweet} locations)))

(defn feed-sweet
  "Give a new state after a certain sweet has been eaten by a certain snake"
  [key sweet game-state]
  (-> game-state
      (update-in [:snakes key] grow-snake)
      (update-in [:snakes key :points] inc)
      (update :sweets remove-sweet sweet)))

(defn feed-snakes
  "Check if there is some snake head on a sweet, and let it eat it, when it is the case"
  [{:keys [snakes sweets] :as game-state}]
  (let [new-state (atom game-state)]
    (doseq [[k v] snakes]
      (let [sweet (some #{(first (get-in v [:body]))} (:locations sweets))]
        (if sweet (swap! new-state #(feed-sweet k sweet %)))))
    @new-state))

(defn handle-sweets
  "Adds new sweet if there are less sweets than the max number, removes the oldest one otherwhise"
  [{:keys [max-number locations] :as sweets} snake board]
  (if (= 0 (rand-int 5))
    (if (> max-number (count locations))
      (assoc sweets :locations (conj locations (rand-free-position snake locations board)))
      (assoc sweets :locations (drop-last locations)))
    sweets))

(defn pop-stored-direction
  [{:keys [stored-direction direction-changed?] :as snake}]
  (if (true? direction-changed?)
    (-> snake
        (assoc :direction stored-direction)
        (assoc :stored-direction nil)
        (assoc :direction-changed? false))
    snake))

(defn next-state
  "Gives the next state of the game-state"
  [{:keys [snakes game-running? board sweets] :as game-state}]
  (if (true? game-running?)
    (if (empty? (:0 snakes))
      (assoc-in game-state [:game-running?] false)
      (let [new-snakes (atom snakes)]
        (doseq [[k v] @new-snakes] (swap! new-snakes #(update % k pop-stored-direction)))
        (doseq [[k v] @new-snakes] (swap! new-snakes #(assoc % k (move-snake v board))))
        (-> game-state
            (assoc :snakes (handle-collisions @new-snakes))
            (feed-snakes)
            (update :sweets handle-sweets @new-snakes board))))
    game-state))

(defn switch-game-running
  "Pause or un-pause to game"
  [{:keys [snakes game-running? board] :as game-state}]
  (if (empty? (:0 snakes))
    (-> game-state
        (assoc-in [:snakes :0] (rand-snake board))
        (assoc-in [:snakes :1] (rand-snake board))
        (assoc-in [:snakes :2] (rand-snake board))
        (assoc-in [:snakes :3] (rand-snake board))
        (assoc-in [:snakes :4] (rand-snake board))
        (assoc-in [:game-running?] true))
    (assoc-in game-state [:game-running?] (not game-running?))))

(defn change-direction
  "Changes direction of the snake, will only be effective after a call to next-state"
  [{:keys [direction direction-changed?] :as snake} new-direction]
  (if snake
    (if (false? direction-changed?)
      (if (not= (map #(* % -1) direction) new-direction)
        (-> snake
            (assoc-in [:stored-direction] new-direction)
            (assoc-in [:direction-changed?] true))))))

(defn initial-state
  "Gives the initial game state"
  [number-of-snakes]
  (let [board [50 40]
        snakes (atom {})]
    (dotimes [n number-of-snakes] (reset! snakes (assoc-in @snakes [(keyword (str n))] (rand-snake board))))
    {
     :board         board
     :snakes        @snakes
     :sweets        {:max-number 20
                     :locations  []}
     :game-running? false
     }))