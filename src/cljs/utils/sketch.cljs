(ns utils.sketch
  (:require [cljs.core.async :as a]
            [cljsjs.hammer]
            [goog.dom :as dom]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [re-frame.core :refer [reg-event-db path dispatch]]
            [reagent.core :as r]
            [snake.sketchpure :refer [draw]]
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
  [setup-function update-function]
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
                sketch-args* {:host       "reframe-canvas"
                              :size       (first canvas-size)
                              :setup      setup-function
                              :draw       (partial draw (second canvas-size))
                              :update     update-function
                              :middleware [m/fun-mode]}]
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