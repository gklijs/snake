(ns snake.ai
  (:require [snake.snakepure :refer [valid-directions valid-cord move-snake]]))

(defonce possible-directions #{[1 1] [1 -1] [-1 1] [-1 -1]})

(deftype PredictHead [direction heads sd current])
(deftype PredictState [oldHeads otherHeads sweets bodies ahead myHeads ownDirection])

(defn add-some-moves
  [search-directions coll cord]
  (reduce conj! coll (map #(valid-cord (mapv + cord %)) search-directions)))

(defn predict-next
  "Get the places taken first, in a map which can be easily for the next steps"
  [snake-heads search-directions]
  (persistent! (reduce (partial add-some-moves search-directions) (transient #{})  snake-heads)))

(defn add-other-snake-heads
  [user-key m k snake]
  (if (= user-key k)
    m
    (conj m (first (:body snake)))))

(defn add-additional-cord
  [ahead cord m new-x]
  (let [new-y (- ahead new-x)]
    (reduce conj! m (map #(valid-cord (mapv + (mapv * % [new-x new-y]) cord)) possible-directions))))

(defn add-additional-cords
  [ahead m cord]
  m
  (reduce conj! m (persistent! (reduce (partial add-additional-cord ahead cord) (transient []) (range (+ ahead 1))))))

(defn add-snake-bodies
  [m k snake]
  (conj! m (:body snake)))

(defn get-heads
  [snake-head direction]
  (let [back (mapv * [-1 -1] direction)]
    (if (= 0 (first direction))
      [
       (PredictHead. direction (volatile! (set [(valid-cord (mapv + snake-head direction))])) [direction [1 0]] true)
       (PredictHead. direction (volatile! (set [(valid-cord (mapv + snake-head direction))])) [direction [-1 0]] true)
       (PredictHead. [1 0] (volatile! (set [(valid-cord (mapv + snake-head [1 0]))])) [back [1 0]] false)
       (PredictHead. [-1 0] (volatile! (set [(valid-cord (mapv + snake-head [-1 0]))])) [back [-1 0]] false)
       ]
      [
       (PredictHead. direction (volatile! (set [(valid-cord (mapv + snake-head direction))])) [direction [0 1]] true)
       (PredictHead. direction (volatile! (set [(valid-cord (mapv + snake-head direction))])) [direction [0 -1]] true)
       (PredictHead. [0 1] (volatile! (set [(valid-cord (mapv + snake-head [0 1]))])) [back [0 1]] false)
       (PredictHead. [0 -1] (volatile! (set [(valid-cord (mapv + snake-head [0 -1]))])) [back [0 -1]] false)
       ]
      )))

(defn add-body-parts
  [m body-parts]
  (reduce conj! m body-parts))

(defn prune-head
  [places-taken m predict-head]
  (let [left-heads (vswap! (.-heads predict-head) #(remove places-taken %))]
    (if
      (empty? left-heads) m (conj! m predict-head))))


(defn prune-my-heads
  "Remove paths which may lead to a collision."
  [my-heads bodies other-heads]
  (let [places-taken (persistent! (reduce add-body-parts (transient other-heads) bodies))]
    (persistent! (reduce (partial prune-head places-taken) (transient []) my-heads))))

(defn add-sweet
  [m [x y]]
  (if-let [inner-map (get m x)]
    (assoc! m x (assoc! inner-map y true))
    (assoc! m x (transient {y true}))
    ))

(defn remove-sweet
  [m [x y]]
  (if-let [inner-map (get m x)]
    (if (= (count (get m x)) 1)
      (dissoc! m x)
      (assoc! m x (dissoc! inner-map y)))
    m))

(defn is-sweet
  [sweets m [x y]]
  (if (get (get sweets x) y)
    (conj! m true)
    m))

(defn strip-body
  [ahead m body]
  (if (> (count body) ahead)
    (conj! m (drop-last ahead body))
    m))

(defn predict-state
  "Updates a predict-state using the volatile's whitin"
  [game-state user-key]
  (let [ahead 1
        old-heads (reduce-kv (partial add-other-snake-heads user-key) #{} (:snakes game-state))
        other-heads (persistent! (reduce (partial add-additional-cords ahead) (transient #{}) old-heads))
        sweets (reduce remove-sweet (reduce add-sweet (transient {}) (get-in game-state [:sweets :locations])) other-heads)
        bodies (persistent! (reduce-kv add-snake-bodies (transient []) (:snakes game-state)))
        stripped-bodies (persistent! (reduce (partial strip-body ahead) (transient []) bodies))
        my-first-heads (get-heads (first (get-in game-state [:snakes user-key :body])) (get-in game-state [:snakes user-key :direction]))
        my-heads (prune-my-heads my-first-heads stripped-bodies other-heads)
        own-direction (get-in game-state [:snakes user-key :direction])]
    (PredictState. old-heads (volatile! other-heads) sweets bodies (volatile! ahead) (volatile! my-heads) own-direction)))


(defn next-move
  [predict-state]
  (let [my-heads @(.-myHeads predict-state)
        own-direction (.-ownDirection predict-state)
        sweets (.-sweets predict-state)]
    (cond
      (empty? my-heads) [true own-direction]
      (= 1 (count my-heads)) [true (.-direction (first my-heads))]
      (and (= 2 (count my-heads)) (.-current (first my-heads)) (.-current (second my-heads))) [true own-direction]
      :default (if-let [head-to-sweet (first (filter #(> (count (reduce (partial is-sweet sweets) (transient []) @(.-heads %))) 0) my-heads))]
                 [true (.-direction head-to-sweet)]
                 (if-let [current-move (first (filter #(.-current %) my-heads))]
                   [false own-direction]
                   [false (.-direction (rand-nth my-heads))])
                 )
      )))

(defn update-my-head
  [m predict-head]
  (let [new-heads (vswap! (.-heads predict-head) #(predict-next % (.-sd predict-head)))]
    (conj! m predict-head)
    ))

(defn into!
  "Needed to use into when to and from are transients"
  ([to from]
   (if (instance? clojure.lang.LazySeq from)
     (reduce conj! to from)
     (reduce conj! to (persistent! from))
     )))

(defn update-predict-state
  [predict-state]
  (let [ahead (vswap! (.-ahead predict-state) inc)
        additional-other-heads (persistent! (reduce (partial add-additional-cords ahead) (transient #{}) (.-oldHeads predict-state)))
        sweets (reduce remove-sweet (.-sweets predict-state) additional-other-heads)
        new-other-heads (vswap!(.-otherHeads predict-state) #(persistent! (reduce conj! (transient %) additional-other-heads)))
        stripped-bodies (persistent! (reduce (partial strip-body ahead) (transient []) (.-bodies predict-state)))
        new-my-heads (vswap! (.-myHeads predict-state) #(prune-my-heads (persistent! (reduce update-my-head (transient []) %)) stripped-bodies new-other-heads))]))

(defn predict-next-best-move
  [game-state user-key max-ahead]
  (if (get-in game-state [:snakes user-key])
    (let [predict-state (predict-state game-state user-key)
          nextm (volatile! (next-move predict-state))]
      (while
        (and (false? (first @nextm)) (< @(.-ahead predict-state) max-ahead))
        (update-predict-state predict-state)
        (if (empty? @(.-myHeads predict-state))
          (vreset! nextm [true (second @nextm)])
          (vreset! nextm (next-move predict-state))))
      (second @nextm)
      )))
