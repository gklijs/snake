(ns snake.handler
  (:require [compojure.core :refer [routes wrap-routes]]
            [snake.layout :refer [error-page]]
            [snake.routes.home :refer [home-routes]]
            [snake.routes.chatsocket :refer [chatsocket-routes]]
            [snake.routes.gamesocket :refer [gamesocket-routes]]
            [compojure.route :as route]
            [snake.env :refer [defaults]]
            [mount.core :as mount]
            [snake.middleware :as middleware]))

(mount/defstate init-app
                :start ((or (:init defaults) identity))
                :stop ((or (:stop defaults) identity)))

(def app-routes
  (routes
    (-> #'home-routes
        (wrap-routes middleware/wrap-csrf))
    (-> #'chatsocket-routes
        (wrap-routes middleware/wrap-csrf))
    (-> #'gamesocket-routes
        (wrap-routes middleware/wrap-csrf))
    (route/not-found
      (:body
        (error-page {:status 404
                     :title  "page not found"})))))


(defn app [] (middleware/wrap-base #'app-routes))
