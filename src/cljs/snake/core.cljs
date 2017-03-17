(ns snake.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [snake.ai :refer [predict-next-best-move]]
            [snake.snakepure :as snakepure]
            [snake.presentation :as presentation]
            [snake.home :as home]
            [snake.single :as single]
            [snake.multi :as multi]
            [snake.highscores :as highscores]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [reg-event-db path reg-sub subscribe dispatch dispatch-sync]]
            [goog.events :as events]
            [clojure.string :as str]))

;; -- Js functions ---------------------------------------------------------

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

;; -- Create initial values ---------------------------------------------------------

(def initial-state {:local-game-state  nil
                    :sel-menu-item     "home"
                    :slide             -1
                    :messages          []
                    :remote-game-state nil
                    :game-info         {}})

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
      (let [new-snake (snakepure/change-direction (get-in local-game-state [:snakes :0]) new-direction)]
        (if new-snake
          (assoc-in db [:local-game-state :snakes :0] new-snake)
          db))
      (= sel-menu-item "multi")
      (do (multi/send-direction new-direction) db)
      (= sel-menu-item "presentation")
      (cond
        (= [1 0] new-direction) (update db :slide inc)
        (= [-1 0] new-direction) (update db :slide dec)
        :default db)
      :default db
      )
    ))

(def ai-levels {:1 2 :2 4 :3 6 :4 8})

(defn set-ai
  "Will probably be moved elsewhere, but for now sets the the next moves for the snakes"
  [game-state]
  (let [result (atom game-state)]
    (doseq [[k v] (:snakes game-state)]
      (if
        (not (= :0 k))
        (swap! result #(assoc-in % [:snakes k :stored-direction] (predict-next-best-move game-state k (get ai-levels k))))))
    @result))

(reg-event-db
  :next-state
  (fn
    [{:keys [local-game-state] :as db} _]
    (if local-game-state
      (let [next-game-state (snakepure/next-state local-game-state)]
        (if (and (:game-running? local-game-state) (nil? (get-in next-game-state [:snakes :0])))
          (assoc db :local-game-state (assoc next-game-state :game-running? false))
          (assoc db :local-game-state (set-ai next-game-state))
          ))
      (assoc db :local-game-state (assoc (snakepure/initial-state 5) :game-running? false))
      )))

(reg-event-db
  :switch-game-running
  (fn
    [{:keys [local-game-state] :as db} _]
    (assoc db :local-game-state (snakepure/switch-game-running local-game-state))))

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
  :remote-game-state
  (path [:remote-game-state])
  (fn
    [remote-game-state [_ value]]
    value))

(reg-event-db
  :messages
  (path [:messages])
  (fn
    [messages [_ value]]
    (take 10 (conj messages value))))

(reg-event-db
  :update-game-info
  (path [:game-info])
  (fn
    [game-info [_ value]]
    (merge game-info value)))

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

(reg-sub
  :remote-game-state
  (fn
    [db _]
    (:remote-game-state db)))

(reg-sub
  :game-info
  (fn
    [db _]
    (:game-info db)))

;; -- View Components ---------------------------------------------------------

(defn switch-button
  "Renders a button to switch the selected menu"
  [value]
  (let [sel-menu-item (subscribe [:sel-menu-item])]
    (fn
      []
      (cond
        (= @sel-menu-item value) [:button.btn.btn-outline-success {:type "button"} value]
        :else [:button.btn.btn-outline-secondary {:type "button" :on-click #(dispatch [:sel-menu-item value])} value]
        ))))

(defn content-switcher
  "Selects which content to show in the app"
  []
  (let [sel-menu-item (subscribe [:sel-menu-item])]
    (cond
      (= @sel-menu-item "single") [single/view]
      (= @sel-menu-item "presentation") [presentation/view]
      (= @sel-menu-item "multi") [multi/view]
      (= @sel-menu-item "home") [home/view]
      (= @sel-menu-item "highscores") [highscores/view]
      :else [:div.container [:div.row.flex-items-xs-center [:h1 "Some day this might show " + @sel-menu-item]]])
    ))

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
  (reagent/render [menu] (js/document.getElementById "navbar"))
  (reagent/render [content-switcher] (js/document.getElementById "app"))
  )

(defn init!
  "The main app function"
  []
  (dispatch-sync [:initialize])
  (mount-components)
  )




