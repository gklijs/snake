(ns user
  (:require [mount.core :as mount]
            [snake.figwheel :refer [start-fw stop-fw cljs]]
            snake.core))

(defn start []
  (mount/start-without #'snake.core/repl-server))

(defn stop []
  (mount/stop-except #'snake.core/repl-server))

(defn restart []
  (stop)
  (start))


