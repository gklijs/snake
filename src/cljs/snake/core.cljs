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

(def initial-state {:local-game-state (snakepure/initial-state 1)
                    :sel-menu-item    "home"
                    :slide            -1
                    :messages         []})

;; -- Event Handlers ----------------------------------------------------------

(reg-event-db
  :initialize
  (fn
    [db _]
    (merge db initial-state)))

(reg-event-db
  :change-direction
  (fn [{:keys [local-game-state sel-menu-item slide] :as db} [_ new-direction]]
    (cond
      (= sel-menu-item "single")
      (let [direction-changed (:direction-changed local-game-state)
            snake (:snake local-game-state)]
        (if (not direction-changed)
          (if (not= (map #(* % -1) (:direction snake)) new-direction)
            (-> db
                (assoc-in [:local-game-state :stored-direction] new-direction)
                (assoc-in [:local-game-state :direction-changed] true))
            db)
          (assoc-in db [:local-game-state :stored-direction] new-direction)))
      (= sel-menu-item "presentation")
      (cond
        (= [1 0] new-direction) (update db :slide inc)
        (= [-1 0] new-direction) (update db :slide dec)
        :default db)
      :default db
      )
    ))

(reg-event-db
  :next-state
  (fn
    [{:keys [local-game-state sel-menu-item] :as db} _]
    (cond
      (= sel-menu-item "single") (assoc-in db [:local-game-state] (snakepure/next-state local-game-state))
      :else db)))

(reg-event-db
  :switch-game-running
  (fn
    [{:keys [local-game-state] :as db} _]
    (logjs db)
    (if (snakepure/collisions (:snake local-game-state))
      (-> db
          (assoc-in [:local-game-state :snake] (snakepure/rand-snake (:board local-game-state)))
          (assoc-in [:local-game-state :sweets :locations] [])
          (assoc-in [:local-game-state :points] 0)
          (assoc-in [:local-game-state :game-running?] true))
      (do (logjs db) (assoc-in db [:local-game-state :game-running?] (not (:game-running? local-game-state))))
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
  :local-game-state
  (fn
    [db _]
    (:local-game-state db)))

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
        (= @sel-menu-item value) [:button.btn.btn-outline-success {:type "button" :on-click #(dispatch [:sel-menu-item value])} value]
        :else [:button.btn.btn-outline-secondary {:type "button" :on-click #(dispatch [:sel-menu-item value])} value]
        ))))

(defn content-switcher
  "Selects which content to show in the app"
  []
  (let [sel-menu-item (subscribe [:sel-menu-item])]
    (fn
      []
      (cond
        (= @sel-menu-item "single") [single/view]
        (= @sel-menu-item "presentation") [presentation/view]
        (= @sel-menu-item "multi") [multi/view]
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
  (reagent/render [content-switcher] (js/document.getElementById "app"))
  (reagent/render [menu] (js/document.getElementById "navbar"))
  )

(defn init!
  "The main app function"
  []
  (dispatch-sync [:initialize])
  (multi/initsockets)
  (mount-components)
  )




