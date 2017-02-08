(ns snake.env
  (:require [selmer.parser :as parser]
            [clojure.tools.logging :as log]
            [snake.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
               (fn []
                 (parser/cache-off!)
                 (log/info "\n-=[snake started successfully using the development profile]=-"))
   :stop
               (fn []
                 (log/info "\n-=[snake has shut down successfully]=-"))
   :middleware wrap-dev})
