(ns snake.core)

(defn init! []
  (-> (.getElementById js/document "app")
      (.-innerHTML)
      (set! "Welcome to snake")))
