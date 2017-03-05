(ns utils.sketch
  (:require [cljs.core.async :as a]
            [goog.dom :as dom]
            [quil.core :as q :include-macros true]
            [reagent.core :as r])
  (:require-macros [cljs.core.async.macros :as a]))

(defn sketch-component
  "Wraps `quil.core/sketch` and plays nicely with Re-frame.
  Below, C = the canvas that will host the sketch.
  Differs from `quil.core/sketch` as follows:
  - Uses a fuction that determines the size of the canvas, this is loaded later, so you could pick the size from a
    container created in the same container. Currently there is no re-load when the size fuction at a later point
    would return an other function.
  - Creates C (rather than C having to be created separately).
  - The `:host` argument must not be provided. (Instead, a unique canvas id is
    created.)
  - Returns a component that wraps C."
  [size-function & {:as sketch-args}]
  (assert (not (contains? sketch-args :host))
          ":host should not be provided, because a unique canvas id will be created")
  (let [saved-sketch-atom (atom ::not-set-yet)]
    [r/create-class
     {:reagent-render
      (fn []
        [:canvas {:id "reframe-canvas"}])
      ;;
      :component-did-mount
      (fn []
        ;; Use a go block so that the canvas exists before we attach the sketch
        ;; to it. (Needed on initial render; not on re-render.)
        (a/go
          (let [size (size-function)
                sketch-args* (merge sketch-args {:host "reframe-canvas" :size size})]
            (reset! saved-sketch-atom
                    (apply q/sketch
                           (apply concat sketch-args*))))
          ))
      ;;
      :component-will-unmount
      (fn []
        (a/go-loop []
                   (if (= @saved-sketch-atom ::not-set-yet)
                     (do                                    ; will probably never get here
                       (a/<! (a/timeout 100))
                       (recur))
                     (q/with-sketch @saved-sketch-atom
                                    (q/exit)))))}]))

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
  [snake name factor]
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
    (when name
      (q/fill 200 200 200)
      (q/text name upper-left-x upper-left-y)
      )))

(defn- draw-snakes
  [snakes own-key show-names factor]
  (doseq [[k snake] snakes]
    (if (= k own-key) (q/fill 227 11 92) (q/fill 42 171 210))
    (if show-names
      (draw-snake snake (name k) factor)
      (draw-snake snake nil factor))
    ))

(defn draw-game-state
  [game-state own-key show-names factor]
  (q/background 30)
  (q/no-stroke)
  (draw-sweets (:sweets game-state) factor)
  (draw-snakes (:snakes game-state) own-key show-names factor))
