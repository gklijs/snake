(ns utils.sketch
  (:require [cljs.core.async :as a]
            [cljsjs.hammer]
            [goog.dom :as dom]
            [quil.core :as q :include-macros true]
            [re-frame.core :refer [reg-event-db path dispatch]]
            [reagent.core :as r]
            [snake.snakepure :refer [board-size]])
  (:require-macros [cljs.core.async.macros :as a]))

(defn- get-canvas-size
  []
  (if-let [canvas-container (js/document.getElementById "canvas-container")]
    (let [[x y] board-size
          width (.-offsetWidth canvas-container)
          excess-width (mod width x)
          canvas-width (- width excess-width)
          factor (/ canvas-width x)
          canvas-heigth (* y factor)]
      [[canvas-width canvas-heigth] factor]
      )
    [[300 240] 6]))

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
  - Returns a component that wraps C.
  - Adds a hammer manager to support touch events in the canvas (though there is still a bug see https://github.com/processing-js/processing-js/pull/118)"
  [draw-function]
  (let [saved-sketch-atom (atom ::not-set-yet)
        hammer-manager (atom nil)]
    [r/create-class
     {:reagent-render
      (fn []
        [:div.container {:id "canvas-container"} [:canvas {:id "reframe-canvas"}]])
      ;;
      :component-did-mount
      (fn [this]
        ;; Use a go block so that the canvas exists before we attach the sketch
        ;; to it. (Needed on initial render; not on re-render.)
        (a/go
          (let [canvas-size (get-canvas-size)
                sketch-args* {:host "reframe-canvas"
                              :size (first canvas-size)
                              :draw #(draw-function (second canvas-size))}]
            (reset! saved-sketch-atom
                    (apply q/sketch
                           (apply concat sketch-args*))))
          (let [mc (new js/Hammer.Manager (r/dom-node this))]
            (js-invoke mc "add" (new js/Hammer.Pan #js{"direction" js/Hammer.DIRECTION_ALL "threshold" 0}))
            (js-invoke mc "on" "pan" #(let [direction (.-direction %)]
                                        (cond
                                          (= direction 8) (dispatch [:change-direction [0 -1]])
                                          (= direction 16) (dispatch [:change-direction [0 1]])
                                          (= direction 4) (dispatch [:change-direction [1 0]])
                                          (= direction 2) (dispatch [:change-direction [-1 0]])
                                          )))
            (reset! hammer-manager mc))))
      :component-will-unmount
      (fn []
        (a/go-loop []
                   (when-let [mc @hammer-manager]
                     (js-invoke mc "destroy"))
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
