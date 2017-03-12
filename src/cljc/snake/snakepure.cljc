(ns snake.snakepure)

(defonce valid-directions #{[0 1] [0 -1] [-1 0] [1 0]})
(defonce board-size [50 40])

(defn valid-head
  "Change the value of the head if it may run out of the board"
  [head]
  (cond
    (= (first head) -1) [(- (first board-size) 1) (second head)]
    (= (first head) (first board-size)) [0 (second head)]
    (= (second head) -1) [(first head) (- (second board-size) 1)]
    (= (second head) (second board-size)) [(first head) 0]
    :else head))

(defn rand-free-position
  "This function takes the snake, locations of the sweets and the board-size as arguments, and
  returns a random position not colliding with the snake body or sweets"
  [snakes locations]
  (let [positions-set (atom locations)
        update-positions-set (doseq [[k v] snakes] (swap! positions-set #(into % (:body v))))
        board-positions (for [x-pos (range (first board-size))
                              y-pos (range (second board-size))]
                          [x-pos y-pos])
        free-position? (atom (rand-nth board-positions))]
    (while (some #(= @free-position? %) @positions-set) (reset! free-position? (rand-nth board-positions)))
    @free-position?))

(defn grow-snake
  "Computes a value for the tail position and returns whole snake"
  [{:keys [body] :as snake}]
  (let [last-2 (take-last 2 body)
        direction (mapv - (second last-2) (first last-2))
        new-part (mapv + (last body) direction)]
    (assoc snake :body (concat body [new-part]))))

(defn rand-snake
  "this function creates a new random snake, based only on the board"
  []
  (let [[x y] board-size
        start-position [[(+ 5 (rand-int (- x 10))) (+ 5 (rand-int (- y 10)))]]
        direction (rand-nth (vec valid-directions))]
    {
     :direction        direction
     :body             (conj start-position
                             (mapv - (first start-position) (mapv * [1 1] direction))
                             (mapv - (first start-position) (mapv * [2 2] direction))
                             (mapv - (first start-position) (mapv * [3 3] direction))
                             (mapv - (first start-position) (mapv * [4 4] direction)))
     :stored-direction nil
     :points           0
     }))

(defn move-snake
  "Move the whole snake positions and directions of all snake elements"
  [{:keys [direction body] :as snake}]
  (let [head-new-position (valid-head (mapv + direction (first body)))]
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
      (if-let [sweet (some #{(first (get-in v [:body]))} (:locations sweets))]
        (swap! new-state #(feed-sweet k sweet %))))
    @new-state))

(defn handle-sweets
  "Adds new sweet if there are less sweets than the max number, removes the oldest one otherwhise"
  [{:keys [max-number locations] :as sweets} snakes step]
  (if (= 0 (mod step 5))
    (if (> max-number (count locations))
      (assoc sweets :locations (conj locations (rand-free-position snakes locations)))
      (assoc sweets :locations (drop-last locations)))
    sweets))

(defn pop-stored-direction
  [{:keys [stored-direction] :as snake}]
  (if stored-direction
    (-> snake
        (assoc :direction stored-direction)
        (assoc :stored-direction nil))
    snake))

(defn next-state
  "Gives the next state of the game-state"
  [{:keys [snakes game-running? sweets step] :as game-state}]
  (if (true? game-running?)
    (if (empty? snakes)
      game-state
      (let [new-snakes (atom snakes)]
        (doseq [[k v] @new-snakes] (swap! new-snakes #(update % k pop-stored-direction)))
        (doseq [[k v] @new-snakes] (swap! new-snakes #(assoc % k (move-snake v))))
        (-> game-state
            (assoc :snakes (handle-collisions @new-snakes))
            (feed-snakes)
            (update :sweets handle-sweets @new-snakes step)
            (update :step inc))))
    game-state))

(defn add-snake
  [m intorkey]
  (let [key (if (number? intorkey) (keyword (str intorkey)) intorkey)]
    (assoc m key rand-snake)))

(defn switch-game-running
  "Pause or un-pause to game, only to be used locally"
  [{:keys [snakes game-running?] :as game-state}]
  (if (and (false? game-running?) (empty? (:0 snakes)))
    (-> game-state
        (assoc :snakes (reduce add-snake {} (range 5)))
        (assoc :game-running? true))
    (assoc-in game-state [:game-running?] (not game-running?))))

(defn change-direction
  "Changes direction of the snake, will only be effective after a call to next-state"
  [{:keys [direction stored-direction] :as snake} new-direction]
  (if snake
    (if (nil? stored-direction)
      (if (not= (map #(* % -1) direction) new-direction)
        (assoc snake :stored-direction new-direction)))))

(defn initial-state
  "Gives the initial game state"
  [number-of-snakes]
  {
   :snakes        (reduce add-snake {} (range number-of-snakes))
   :sweets        {:max-number 20
                   :locations  []}
   :game-running? true
   :step          0
   })