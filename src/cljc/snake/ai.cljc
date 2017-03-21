(ns snake.ai
  (:require [snake.snakepure :refer [valid-directions valid-cord move-snake]]))

(defonce possible-directions #{[1 1] [1 -1] [-1 1] [-1 -1]})

(deftype PredictHead [direction heads sd current])

(defn add-some-moves
  [search-directions coll cord]
  (into coll (map #(valid-cord (mapv + cord %)) search-directions)))

(defn predict-next
  "Get the places taken first, in a map which can be easily for the next steps"
  [snake-heads search-directions]
  (reduce (partial add-some-moves search-directions) #{} snake-heads))

(defn add-other-snake-heads
  [user-key m k snake]
  (if (= user-key k)
    m
    (conj m (first (:body snake)))))

(defn add-additional-cord
  [ahead cord m new-x]
  (let [new-y (- ahead new-x)]
    (into m (map #(valid-cord (mapv + (mapv * % [new-x new-y]) cord)) possible-directions))))

(defn add-additional-cords
  [ahead m cord]
  m
  (into m (reduce (partial add-additional-cord ahead cord) `() (range (+ ahead 1)))))

(defn add-snake-bodies
  [m k snake]
  (conj m (drop-last (:body snake))))

(defn get-heads
  [snake-head direction]
  (let [back (mapv * [-1 -1] direction)]
    (if (= 0 (first direction))
      [
       (PredictHead. direction (set [(valid-cord (mapv + snake-head direction))]) (remove #(= back %) valid-directions) true)
       (PredictHead. [1 0] (set [(valid-cord (mapv + snake-head [1 0]))]) [back [1 0]] false)
       (PredictHead. [-1 0] (set [(valid-cord (mapv + snake-head [-1 0]))]) [back [-1 0]] false)
       ]
      [
       (PredictHead. direction (set [(valid-cord (mapv + snake-head direction))]) (remove #(= back %) valid-directions) true)
       (PredictHead. [0 1] (set [(valid-cord (mapv + snake-head [0 1]))]) [back [0 1]] false)
       (PredictHead. [0 -1] (set [(valid-cord (mapv + snake-head [0 -1]))]) [back [0 -1]] false)
       ]
      )))

(defn add-body-parts
  [m body-parts]
  (into m body-parts))

(defn prune-head
  [places-taken m predict-head]
  (let [left-heads (remove places-taken (.heads predict-head))]
    (cond
      (empty? left-heads) m
      (= (count left-heads) (count (.heads predict-head))) (conj m predict-head)
      :default (conj m (PredictHead. (.-direction predict-head) left-heads (.-sd predict-head) (.-current predict-head))))))


(defn prune-my-heads
  "Remove paths which may lead to a collision."
  [my-heads bodies other-heads]
  (let [places-taken (reduce add-body-parts other-heads bodies)]
    (reduce (partial prune-head places-taken) [] my-heads)))

(defn add-sweet
  [m [x y]]
  (assoc-in m [x y] true))

(defn remove-sweet
  [m [x y]]
  (if (contains? m x)
    (if (= (count (get m x)) 1)
      (dissoc m x)
      (update m x dissoc y))
    m))

(defn is-sweet
  [sweets m [x y]]
  (if (get-in sweets [x y])
    (conj m true)
    m))

(defn predict-map
  "Gives back a prediction for the next step, in a format which can be used to look further"
  [game-state user-key]
  (let [old-heads (reduce-kv (partial add-other-snake-heads user-key) #{} (:snakes game-state))
        other-heads (reduce (partial add-additional-cords 1) #{} old-heads)
        sweets (reduce remove-sweet (reduce add-sweet {} (get-in game-state [:sweets :locations])) other-heads)
        bodies (reduce-kv add-snake-bodies [] (:snakes game-state))
        my-first-heads (get-heads (first (get-in game-state [:snakes user-key :body])) (get-in game-state [:snakes user-key :direction]))
        my-heads (prune-my-heads my-first-heads bodies other-heads)]
    {
     :old-heads   old-heads
     :other-heads other-heads
     :sweets      sweets
     :bodies      bodies
     :ahead       1
     :my-heads    my-heads
     }))


(defn next-move
  "Give the next move based on prediction, will always return non-nil if not-nil is true, else might return nil"
  [{:keys [sweets my-heads] :as predict-map} not-nil]
  (if
    (= 1 (count my-heads))
    (.-direction (first my-heads))
    (let [head-to-sweets (filter #(not (empty? (reduce (partial is-sweet sweets) `() (.-heads %)))) my-heads)]
      (cond
        (not (empty? head-to-sweets)) (if-let [current-move (first (filter #(.-current %) head-to-sweets))]
                                        (.-direction current-move)
                                        (.-direction (rand-nth head-to-sweets)))
        not-nil (if-let [current-move (first (filter #(.-current %) my-heads))]
                  (.-direction current-move)
                  (.-direction (rand-nth my-heads)))
        :default nil))
    ))

(defn update-body
  [m body]
  (if (> (count body) 1)
    (conj m (drop-last body))
    m))

(defn update-my-head
  [m predict-head]
  (let [new-heads (predict-next (.-heads predict-head) (.-sd predict-head))]
    (conj m (PredictHead. (.-direction predict-head) new-heads (.-sd predict-head) (.-current predict-head)))
    ))

(defn update-predict-map
  [{:keys [old-heads other-heads sweets bodies ahead my-heads] :as predict-map}]
  (let [new-ahead (inc ahead)
        additional-other-heads (reduce (partial add-additional-cords new-ahead) #{} old-heads)
        new-sweets (reduce remove-sweet sweets additional-other-heads)
        new-other-heads (into other-heads additional-other-heads)
        new-bodies (reduce update-body [] bodies)
        new-my-heads (prune-my-heads (reduce update-my-head [] my-heads) new-bodies new-other-heads)]
    {
     :old-heads   old-heads
     :other-heads new-other-heads
     :sweets      new-sweets
     :bodies      new-bodies
     :ahead       new-ahead
     :my-heads    new-my-heads
     }))

(defn predict-next-best-move
  [game-state user-key max-ahead]
  (if (get-in game-state [:snakes user-key])
    (let [last-predict-map (atom (predict-map game-state user-key))
          predict-map (atom @last-predict-map)
          next (atom (next-move @predict-map false))]
      (while
        (nil? @next)
        (cond
          (empty? (:my-heads @predict-map)) (if (empty? (:my-heads @last-predict-map))
                                              (reset! next (get-in game-state [:snakes user-key :direction]))
                                              (reset! next (next-move @last-predict-map true)))
          (>= (:ahead @predict-map) max-ahead) (reset! next (next-move @predict-map true))
          :default (do (reset! last-predict-map @predict-map)
                       (swap! predict-map update-predict-map)
                       (reset! next (next-move @predict-map false)))
          ))
      @next
      )))
