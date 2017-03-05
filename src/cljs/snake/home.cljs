(ns snake.home)

;; -- View Components ---------------------------------------------------------

(defn view
  "The home text rendering function"
  []
  [:div
   [:div.container [:div.row.flex-items-xs-center
                    [:div.col-xs [:div.slide
                                  [:h1.presentation-title "Welcome to yet another snake hobby project"]
                                  [:p.presentation-text "You can play a snake-like game either with stupid robots by selecting 'single', or you can select 'multi', and battle with whatever is connected to the server."]
                                  [:p.presentation-text "For now you can control the snake with the arrow buttons, or with touch by swiping to the direction you want to go. When you hit another snake, or yourself, you will die. When another snake is killed by you you get 5 points. There is also some candy which lets you grow one piece at a time and also gives one point. All candy will eventually get smaller and disappear"]
                                  [:p.presentation-text "The game is made with clojure and clojurescript, based on a luminus re-frame template. It uses quil, which generates a canvas to show the game-state. The source of the game can be found on github, see " [:a {:href "https://github.com/gklijs/snake"} "snake"]]
                                  ]]]
    ]])