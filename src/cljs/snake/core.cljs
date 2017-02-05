(ns snake.core
    (:require-macros [reagent.ratom :refer [reaction]])
    (:require [snake.snakepure :as snakepure]
              [snake.presentation :as presentation]
              [snake.single :as single]
              [snake.multi :as multi]
              [reagent.core :as reagent :refer [atom]]
              [re-frame.core :refer [reg-event-db path reg-sub subscribe dispatch dispatch-sync]]
              [goog.events :as events]))

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

(def initial-snake (snakepure/rand-snake initial-board))

(def initial-state {:board             initial-board
                    :snake             initial-snake
                    :sweets            initial-sweets
                    :points            0
                    :game-running?     false
                    :direction-changed false
                    :stored-direction  false
                    :sel-menu-item     "home"
                    :slide             -1
                    :messages          []})

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
            (snakepure/next-state db)
        :default db
      )))

(reg-event-db
  :switch-game-running
  (fn
    [{:keys [game-running? snake board] :as db} _]
        (if (snakepure/collisions snake)
            (-> db
            (assoc :snake (snakepure/rand-snake board))
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

(reg-event-db
  :messages
  (path [:messages])
  (fn
    [messages [_ value]]
    (take 5 (conj messages value))))

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

(reg-sub
  :messages
  (fn
    [db _]
    (:messages db)))

;; -- View Components ---------------------------------------------------------

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

(defn content-switcher
  "Selects which content to show in the app"
  []
  (let [sel-menu-item (subscribe [:sel-menu-item])]
    (fn
      []
      (cond
        (= @sel-menu-item "single") [single/game]
        (= @sel-menu-item "presentation") [presentation/presentation]
        (= @sel-menu-item "multi") [multi/multi]
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
  (multi/initsocket)
  (mount-components)
  )




