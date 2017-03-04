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

(defn ellipse
  [[x y] factor]
  (let [x-pos (+ (* x factor) (/ factor 2))
        y-pos (+ (* y factor) (/ factor 2))]
    (q/ellipse x-pos y-pos factor factor)))

(defn rect
  [[x y] factor]
  (let [x-pos  (* x factor)
        y-pos (* y factor)]
    (q/rect x-pos y-pos factor factor)))

(defn draw-snake
  [snake factor]
  (let [[x y] (first (:body snake))
        direction (:direction snake)]
    (cond
      (= direction [0 1]) (ellipse [x y] factor)
      (= direction [0 -1]) (ellipse [x y] factor)
      (= direction [-1 0]) (ellipse [x y] factor)
      (= direction [1 0]) (ellipse [x y] factor)
      :else (ellipse [x y] factor)))
  (doseq [pos (rest (:body snake))]
    (rect pos factor)))
