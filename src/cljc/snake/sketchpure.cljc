(ns snake.sketchpure
  (:require [quil.core :as q :include-macros true]))

(defn- draw-sweet
  [[x y] factor time]
  (let [x-pos (+ (* x factor) (/ factor 2))
        y-pos (+ (* y factor) (/ factor 2))
        size (if (> factor 2) (* time factor) factor)]
    (q/ellipse x-pos y-pos size size)))

(defn- rect
  [[x y] factor]
  (let [x-pos (* x factor)
        y-pos (* y factor)]
    (q/rect x-pos y-pos factor factor)))

(defn- draw-sweets
  [sweets factor]
  (q/fill 119 255 51)
  (let [max-number (* 1.5 (:max-number sweets))
        count (atom 0)]
    (doseq [location (:locations sweets)]
      (draw-sweet location factor (/ (- max-number @count) max-number))
      (swap! count inc))))

(defn- draw-snake
  [snake label factor]
  (let [[x y] (first (:body snake))
        direction (:direction snake)
        upper-left-x (* x factor)
        upper-left-y (* y factor)
        half-factor (/ factor 2)]
    (cond
      (= direction [0 1]) (q/triangle upper-left-x upper-left-y (+ upper-left-x factor) upper-left-y (+ upper-left-x half-factor) (+ upper-left-y factor))
      (= direction [0 -1]) (q/triangle upper-left-x (+ upper-left-y factor) (+ upper-left-x factor) (+ upper-left-y factor) (+ upper-left-x half-factor) upper-left-y)
      (= direction [-1 0]) (q/triangle (+ upper-left-x factor) upper-left-y (+ upper-left-x factor) (+ upper-left-y factor) upper-left-x (+ upper-left-y half-factor))
      (= direction [1 0]) (q/triangle upper-left-x upper-left-y upper-left-x (+ upper-left-y factor) (+ upper-left-x factor) (+ upper-left-y half-factor)))
    (doseq [pos (rest (:body snake))]
      (rect pos factor))
    (when label
      (cond
        (and (> x 30) (< y 5)) (q/text-align :right :top)
        (< y 5) (q/text-align :left :top)
        (> x 30) (q/text-align :right :bottom)
        :default (q/text-align :left :bottom))
      (q/fill 200 200 200)
      (q/text label upper-left-x upper-left-y)
      )))

(defn- draw-snakes
  [snakes own-key show-names show-scores factor]
  (doseq [[k snake] snakes]
    (if (= k own-key) (q/fill 227 11 92) (q/fill 42 171 210))
    (let [label (cond
                  (and show-names show-scores) (str (name k) " (" (:points snake) ")")
                  show-names (name k)
                  show-scores (:points snake)
                  :default nil)]
      (draw-snake snake label factor))))

(defn draw-game-state
  [game-state own-key show-names show-scores factor]
  (q/background 30)
  (q/no-stroke)
  (q/text-size (* 2 factor))
  (draw-sweets (:sweets game-state) factor)
  (draw-snakes (:snakes game-state) own-key show-names show-scores factor))

(defn draw
  [enlarge state]
  (draw-game-state (:game-state state) (:user-key state) @(:show-names state) @(:show-scores state) enlarge))


