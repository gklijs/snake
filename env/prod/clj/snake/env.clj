(ns snake.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
               (fn []
                 (log/info "\n-=[snake started successfully]=-"))
   :stop
               (fn []
                 (log/info "\n-=[snake has shut down successfully]=-"))
   :middleware identity})
