(ns snake.core
    (:require-macros [reagent.ratom :refer [reaction]])
    (:require [snake.presentation :as presentation]
              [reagent.core :as reagent :refer [atom]]
              [re-frame.core :refer [reg-event-db path reg-sub subscribe dispatch dispatch-sync]]
              [goog.events :as events]))

;; -- Pure functions ---------------------------------------------------------

(defn rand-free-position
  "This function takes the snake, locations of the sweets and the board-size as arguments, and
  returns a random position not colliding with the snake body or sweets"
  [snake locations [x y]]
  (let [positions-set (concat  (into #{} (:body snake)) locations)
        board-positions (for [x-pos (range x)
                              y-pos (range y)]
                          [x-pos y-pos])
        free-position? (atom (rand-nth board-positions))]
        (while (some #(= @free-position? %) positions-set)(reset! free-position? (rand-nth board-positions)))
        @free-position?))

(defn valid-head
  "Change the value of the head if it may run out of the board"
  [head board]
  (cond
    (= (first head) -1) [(- (first board) 1) (second head)]
    (= (first head) (first board)) [0 (second head)]
    (= (second head) -1) [(first head) (- (second board) 1)]
    (= (second head) (second board)) [(first head) 0]
    :else head))

(defn grow-snake
  "Computes a value for the tail position and returns whole snake"
  [{:keys [body] :as snake}]
  (let [last-2 (take-last 2 body)]
    (let [direction (mapv - (second last-2) (first last-2))]
      (assoc snake :body (conj body (mapv + (last body) direction))))))

(defn rand-snake
    "this function creates a new random snake, based only on the board"
    [[x y]]
    (let [valid-directons [[0 1][0 -1][-1 0][1 0]]
            snake (atom {})
            start-position [[(rand-int x)(rand-int y)]]]
    (reset! snake (assoc @snake :direction (rand-nth valid-directons)))
    (reset! snake (assoc @snake :body (conj start-position (valid-head (mapv + (:direction @snake) start-position) [x y]))))
    (dotimes [n 4] (swap! snake grow-snake))
    @snake
    ))

(defn move-snake
  "Move the whole snake positions and directions of all snake elements"
  [{:keys [direction body] :as snake} board]
  (let [head-new-position (valid-head (mapv + direction (first body)) board) ]
    (update-in snake [:body] #(into [] (drop-last (cons head-new-position body))))))

(defn collisions
   "Check whether the snake is hit by himself, causing it to die"
  [snake]
  (let [head (first (:body snake))
        body (rest (:body snake))]
    (some #(= head %) body)))

(defn remove-sweet
  "Remove a certain sweet cause it's been eaten"
  [{:keys [locations] :as sweets} sweet]
  (assoc sweets :locations (remove #{sweet} locations)))

(defn process-movement
  "Handles movement stuff"
  [{:keys [snake sweets] :as db-before}]
  (let [db (assoc db-before :direction-changed false)
        sweet (some #{(first (:body snake))}  (:locations sweets))]
    (if sweet
      (-> db
          (update :snake grow-snake)
          (update :points inc)
          (update :sweets remove-sweet sweet))
      db)))

(defn handle-sweets
  "Adds new sweet if there are less sweets than the max number, removes the oldest one otherwhise"
  [{:keys [max-number locations] :as sweets} snake board]
  (if (> max-number (count locations))
    (update-in sweets [:locations] #(conj locations (rand-free-position snake locations board)))
    (update-in sweets [:locations] #(remove #{(last locations)}  locations))))

(defn pop-stored-direction
  [{:keys [stored-direction direction-changed] :as db}]
  (if (false? direction-changed)
    (if (false? stored-direction)
      db
      (-> db
          (assoc-in [:snake :direction] stored-direction)
          (assoc :stored-direction false)))
    db))

;; -- Js functions ---------------------------------------------------------

(defn logjs
    "This function prints an argument to the js console"
    [argument]
    (.log js/console (clj->js argument)))

(def key-code->move
  "Mapping from the integer key code to the direction vector corresponding to that key"
  {38 [0 -1]
   40 [0 1]
   39 [1 0]
   37 [-1 0]})

(defonce key-handler
         (events/listen js/window "keydown"
                        (fn [e]
                          (let [key-code (.-keyCode e)]
                            (when (contains? key-code->move key-code)
                              (dispatch [:change-direction (key-code->move key-code)]))))))

(defonce snake-moving
         (js/setInterval #(dispatch [:next-state]) 150))

;; -- Create initial values ---------------------------------------------------------

(def initial-board [50 40])

(def initial-sweets {:max-number 20
             :locations []})

(def initial-snake (rand-snake initial-board))

(def initial-state {:board             initial-board
                    :snake             initial-snake
                    :sweets            initial-sweets
                    :points            0
                    :game-running?     false
                    :direction-changed false
                    :stored-direction  false
                    :sel-menu-item     "home"
                    :slide             -1})

;; -- Event Handlers ----------------------------------------------------------

(reg-event-db
  :initialize
  (fn
    [db _]
    (merge db initial-state)))

(reg-event-db
  :change-direction
  (fn [{:keys [snake direction-changed sel-menu-item sel-menu-item slide] :as db} [_ new-direction]]
    (cond
        (= sel-menu-item "single")
            (if (not direction-changed)
              (if (not= (map #(* % -1) (:direction snake)) new-direction)
                (-> db
                    (assoc-in [:snake :direction] new-direction)
                    (assoc :direction-changed true))
                db)
              (assoc db :stored-direction new-direction))
        (= sel-menu-item "presentation")
             (cond
             (= [1 0] new-direction) (-> db
                     (assoc :slide (inc slide)))
             (= [-1 0] new-direction) (-> db
                      (assoc :slide (dec slide)))
              :default db)
        :default db
    )
  ))

(reg-event-db
  :next-state
  (fn
    [{:keys [snake board sel-menu-item slide] :as db} _]
    (cond
        (= sel-menu-item "single")
            (if (:game-running? db)
                  (if (collisions snake)
                    (assoc-in db [:game-running?] false)
                    (-> db
                        (pop-stored-direction)
                        (update :snake move-snake board)
                        (as-> after-move
                              (process-movement after-move))
                        (update :sweets handle-sweets snake board)))
                  db)
        :default db
      )))

(reg-event-db
  :switch-game-running
  (fn
    [{:keys [game-running? snake board] :as db} _]
        (if (collisions snake)
            (-> db
            (assoc :snake (rand-snake board))
            (assoc-in [:sweets :locations] [])
            (assoc :points 0)
            (assoc :game-running? true))
        (assoc-in db [:game-running?] (not (:game-running? db)))
        )))

(reg-event-db
  :sel-menu-item
  (path [:sel-menu-item])
  (fn
    [sel-menu-item [_ value]]
    value))

(reg-event-db
  :slide
  (path [:slide])
  (fn
    [slide [_ value]]
    value))

;; -- Subscription Handlers ---------------------------------------------------

(reg-sub
  :board
  (fn
    [db _]
    (:board db)))


(reg-sub
  :snake
  (fn
    [db _]
    (:snake db)))


(reg-sub
  :sweets
  (fn
    [db _]
    (:sweets db)))

(reg-sub
  :points
  (fn
    [db _]
    (:points db)))


(reg-sub
  :game-running?
  (fn
    [db _]
    (:game-running? db)))


(reg-sub
  :direction-changed
  (fn
    [db _]
    (:direction-changed db)))

(reg-sub
  :stored-direction
  (fn
    [db _]
    (:stored-direction db)))

(reg-sub
  :sel-menu-item
  (fn
    [db _]
    (:sel-menu-item db)))

(reg-sub
  :slide
  (fn
    [db _]
    (:slide db)))

;; -- View Components ---------------------------------------------------------

(defn render-board
  "Renders the board area of the game"
  []
  (let [board (subscribe [:board])
        snake (subscribe [:snake])
        sweets (subscribe [:sweets])]
    (fn []
      (let [[width height] @board
            snake-positions (into #{} (:body @snake))
            sweet-positions (into #{} (:locations @sweets))
            cells (for [y (range height)]
                    (into [:div.row.flex-items-xs-center]
                          (for [x (range width)
                                :let [current-pos [x y]]]
                            (cond
                              (snake-positions current-pos) [:div.col-xs.board-element.snake-on-cell]
                              (sweet-positions current-pos) [:div.col-xs.board-element.sweet]
                              :default [:div.col-xs.board-element.cell]))))]
        (into [:div.container]
              cells)))))

(defn render-slide
    "will show different content, depending on the number of the slide"
    []
    (let [slide (subscribe [:slide])]
    (fn
        []
        (cond
            (> 0 @slide)[:div.slide [:h1 "Myself, Devoxx, Clojure, Clojurescript"][:p][:img {:src "/img/clojure-logo.png"}][:p][:p "Press right to go to the first slide"]]
            (<= (count presentation/my-presentation) @slide)[:div.slide [:h1 "The end"][:p][:img {:src "/img/end.png"}][:p][:p "Press left to go back to the slide"]]
            :default (let [my-slide (nth presentation/my-presentation @slide)]
                [:div.slide
                [:h1.presentation-title (:title my-slide)]
                (if (:first my-slide) [:p.presentation-text (:first my-slide)])
                (if (:second my-slide) [:p.presentation-text (:second my-slide)])
                (if (:third my-slide) [:p.presentation-text (:third my-slide)])
                (if (:youtube my-slide) [:iframe {:width "100%" :height "700px" :src (str "https://www.youtube.com/embed/" (:youtube my-slide))}])
            ])
        )
    )))

(defn score
  "Renders the player's score"
  []
  (let [points (subscribe [:points])]
    (fn
      []
      [:div.score (str "Score: " @points)])))

(defn start-stop
  "Renders the button to start/pause the game"
  []
  (let [game-running? (subscribe [:game-running?])]
    (fn
      []
      [:button.btn.btn-secondary {:type "button" :on-click #(dispatch [:switch-game-running])}
      (str (if @game-running? "Pause" "Start"))
      ])))

(defn switch-button
  "Renders a button to switch the selected menu"
  [value]
  (let [sel-menu-item (subscribe [:sel-menu-item])]
    (fn
      []
      (cond
        (= @sel-menu-item value) [:button.btn.btn-outline-success{:type "button" :on-click #(dispatch [:sel-menu-item value])}value]
        :else [:button.btn.btn-outline-secondary {:type "button" :on-click #(dispatch [:sel-menu-item value])}value]
      ))))

(defn game
  "The game rendering function"
  []
  [:div
   [:div.container.controls [:div.row.flex-items-xs-center
       [:div.col-xs [score]]
       [:div.col-xs [start-stop]]
    ]]
   [render-board]])

(defn presentation
  "The presentation rendering function"
  []
  [:div
   [:div.container [:div.row.flex-items-xs-center
       [:div.col-xs [render-slide]]]
    ]])

(defn content-switcher
  "Selects which content to show in the app"
  []
  (let [sel-menu-item (subscribe [:sel-menu-item])]
    (fn
      []
      (cond
        (= @sel-menu-item "single") [game]
        (= @sel-menu-item "presentation") [presentation]
        :else [:div.container [:div.row.flex-items-xs-center [:h1 "Some day this might show " + @sel-menu-item]]])
      )))

(defn menu
  "The menu rendering function"
  []
   [:div
   [:nav.navbar.navbar-dark.bg-inverse
       [:h1.navbar-brand.mb-0 (str "Snake")]
       [:form.form-inline.float-xs-left
       [(switch-button "home")]
       [(switch-button "single")]
       [(switch-button "multi")]
       [(switch-button "highscores")]
       [(switch-button "presentation")]
    ]]])

;; -- Entry Point -------------------------------------------------------------

(defn mount-components
  "For the figwheel"
  []
  (reagent/render [content-switcher](js/document.getElementById "app"))
  (reagent/render [menu](js/document.getElementById "navbar"))
  )

(defn init!
  "The main app function"
  []
  (dispatch-sync [:initialize])
  (mount-components)
  )




