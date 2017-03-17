(ns snake.ai
  (:require [snake.snakepure :refer [valid-directions valid-cord move-snake]]))

(defn add-valid-move
  [cord coll direction]
  (conj coll (valid-cord (mapv + direction cord))))

(defn add-all-moves
  [coll cord]
  (into coll (reduce (partial add-valid-move cord)  #{} valid-directions)))

(defn add-some-moves
  [excluded coll cord]
  (let [search-directions (remove (into #{} excluded) valid-directions)]
    (into coll (reduce (partial add-valid-move cord)  #{} search-directions))))

(defn predict-next
  "Get the places taken first, in a map which can be easily for the next steps"
  [snake-heads]
  (let [snhe (if (vector? snake-heads) (conj #{} snake-heads) snake-heads)]
    (reduce add-all-moves #{} snake-heads)
    ))

(defn predict-next-excluded
  "Get the places taken first, in a map which can be easily for the next steps"
  [snake-heads excluded]
  (let [excl (if (vector? excluded) (conj #{} excluded) excluded)
        snhe (if (vector? snake-heads) (conj #{} snake-heads) snake-heads)]
    (reduce (partial add-some-moves excl) #{} snhe)
    ))

(defn add-other-snake-next-heads
  [user-key m k snake]
  (if (= user-key k)
    m
    (into m (predict-next-excluded (first (:body snake)) (mapv * [-1 -1] (:direction snake))))))

(defn add-snake-bodies
  [m k snake]
    (conj m (drop-last (:body snake))))

(defn get-heads
  [snake-head direction]
  (if (= 0 (first direction))
  [
   {:direction direction :heads (conj #{} (mapv + snake-head direction)) :excluded (mapv * [-1 -1] direction) :current true}
   {:direction [1 0] :heads (conj #{} (mapv + snake-head [1 0])) :excluded direction}
   {:direction [-1 0] :heads (conj #{} (mapv + snake-head [-1 0])) :excluded direction}
   ]
  [
   {:direction direction :heads (conj #{} (mapv + snake-head direction)) :excluded (mapv * [-1 -1] direction) :current true}
   {:direction [0 1] :heads (conj #{} (mapv + snake-head [0 1])) :excluded direction}
   {:direction [0 -1] :heads (conj #{} (mapv + snake-head [0 -1])) :excluded direction}
   ]
  ))

(defn add-body-parts
  [m body-parts]
  (into m body-parts))

(defn prune-head
  [places-taken m head-map]
  (let [pruned-head (update head-map :heads #(set (remove places-taken %)))]
    (if (empty? (:heads pruned-head))
      m
      (conj m pruned-head))))

(defn prune-my-heads
  "Remove paths which may lead to a collision."
  [my-heads bodies other-heads]
  (let [places-taken (reduce add-body-parts other-heads bodies)]
    (reduce (partial prune-head places-taken) [] my-heads)))

(defn predict-map
  "Gives back a prediction for the next step, in a format which can be used to look further"
  [game-state user-key]
  (let [other-heads (reduce-kv (partial add-other-snake-next-heads user-key) #{} (:snakes game-state))
        sweets (set (remove other-heads (into #{} (get-in game-state [:sweets :locations]))))
        bodies (reduce-kv add-snake-bodies [] (:snakes game-state))
        my-first-heads (get-heads (first (get-in game-state [:snakes user-key :body])) (get-in game-state [:snakes user-key :direction]))
        my-heads (prune-my-heads my-first-heads bodies other-heads)]
    {
     :other-heads other-heads
     :sweets sweets
     :bodies bodies
     :ahead 1
     :my-heads my-heads
     }))


(defn next-move
  "Give the next move based on prediction, will always return non-nil if not-nil is true, else might return nil"
  [{:keys [sweets my-heads] :as predict-map} not-nil]
  (let [head-to-sweets (filter #(not (empty? (clojure.set/intersection sweets (:heads %)))) my-heads)]
    (cond
      (not (empty? head-to-sweets)) (let [current-move (filter #(contains? % :current) head-to-sweets)]
                                      (if (empty? current-move)
                                        (:direction (rand-nth head-to-sweets))
                                        (:direction (rand-nth current-move))))
      not-nil (let [current-move (filter #(contains? % :current) my-heads)]
                (if (empty? current-move)
                  (:direction (rand-nth my-heads))
                  (:direction (rand-nth current-move))))
      :default nil)))

(defn update-body
  [m body]
  (if (> (count body) 1)
    (conj m (drop-last body))
    m))

(defn update-my-head
  [m head-map]
  (conj m (update head-map :heads #(predict-next-excluded % (:excluded head-map)))))

(defn update-predict-map
  [{:keys [other-heads sweets bodies ahead my-heads] :as predict-map}]
  (let [new-other-heads (predict-next other-heads)
        new-sweets (clojure.set/difference sweets new-other-heads)
        new-bodies (reduce update-body [] bodies)
        new-ahead (inc ahead)
        new-my-heads (prune-my-heads (reduce update-my-head [] my-heads) new-bodies new-other-heads)]
    {
     :other-heads new-other-heads
     :sweets new-sweets
     :bodies new-bodies
     :ahead new-ahead
     :my-heads new-my-heads
     }))

(defn predict-next-best-move
  [game-state user-key max-ahead]
  (if (get-in game-state [:snakes user-key])
    (let [last-predict-map (atom (predict-map game-state user-key))
          predict-map (atom @last-predict-map)
          next (atom (next-move predict-map false))]
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
