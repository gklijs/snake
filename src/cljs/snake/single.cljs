(ns snake.single
  (:require [cljs.core.async :as a]
            [snake.ai :refer [predict-next-best-move]]
            [re-frame.core :refer [subscribe dispatch reg-event-db]]
            [reagent.core :as reagent :refer [atom]]
            [quil.core :refer [frame-rate]]
            [snake.snakepure :as snakepure]
            [utils.sketch :refer [sketch-component draw-game-state]])
  (:require-macros [cljs.core.async.macros :as a]))

;; -- Event Handlers ----------------------------------------------------------

(def ai-level (atom 4))
(defonce ai-running (atom false))

(defn set-ai
  "Will probably be moved elsewhere, but for now sets the the next moves for the snakes"
  [game-state]
  (a/go
    (when (not @ai-running))
    (reset! ai-running true)
    (let [start# (system-time)
          _ (doseq [[k v] (:snakes game-state)]
                           (if
                             (not (= :0 k))
                             (dispatch [:set-direction (predict-next-best-move game-state k @ai-level ) k])))
          run-time (- (system-time) start#)]
      (cond
        (> run-time 70) (swap! ai-level dec)
        (and (< run-time 20) (< @ai-level 20)) (swap! ai-level inc)))
    (reset! ai-running false)))

(reg-event-db
  :set-direction
  (fn [{:keys [local-game-state] :as db} [_ new-direction user-key]]
    (if-let [new-snake (snakepure/change-direction (get-in local-game-state [:snakes user-key]) new-direction)]
      (assoc-in db [:local-game-state :snakes user-key] new-snake)
      db)
    ))

(reg-event-db
  :set-local-game-state
  (fn
    [db [_ new-game-state]]
    (if (:game-running? new-game-state)
      (set-ai new-game-state))
    (assoc db :local-game-state new-game-state)
    ))

(reg-event-db
  :switch-game-running
  (fn
    [{:keys [local-game-state] :as db} _]
    (assoc db :local-game-state (snakepure/switch-game-running local-game-state))))


;; -- View Components ---------------------------------------------------------
(defonce show-names (atom false))
(defonce show-scores (atom false))

(defn setup-function
  []
  (frame-rate 7)
  {:game-state nil :user-key :0 :show-names show-names :show-scores show-scores})

(defn update-function
  [state]
  (let [game-state (subscribe [:local-game-state])
        new-game-state (if @game-state
                         (let [next-game-state (snakepure/next-state @game-state)]
                           (if (and (:game-running? @game-state) (nil? (get-in next-game-state [:snakes :0])))
                             (assoc next-game-state :game-running? false)
                             next-game-state))
                         (assoc (snakepure/initial-state 5) :game-running? false))]
    (dispatch [:set-local-game-state new-game-state])
    (assoc state :game-state new-game-state)
    ))

(defn score
  "Renders the player's score"
  []
  (let [local-game-state (subscribe [:local-game-state :snakes :0 :points])]
    [:div.score (str "Score: " (get-in @local-game-state [:snakes :0 :points]))]))

(defn toggle-name
  "Renders the button to switch showing the names on and off"
  []
  (if @show-names
    [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! show-names false)} "Hide names"]]
    [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! show-names true)} "Show names"]]))

(defn toggle-score
  "Renders the button to switch showing the names on and off"
  []
  (if @show-scores
    [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! show-scores false)} "Hide scores"]]
    [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! show-scores true)} "Show scores"]]))

(defn start-stop
  "Renders the button to start/pause the game"
  []
  (let [local-game-state (subscribe [:local-game-state])]
    (fn
      []
      [:button.btn.btn-secondary {:type "button" :on-click #(dispatch [:switch-game-running])}
       (str (if (:game-running? @local-game-state) "Pause" "Start"))
       ])))

(defn view
  "The game rendering function"
  []
  [:div
   [:div.container.controls [:div.d-flex.justify-content-end
                             [:div.mr-auto.p-2 [score]]
                             (toggle-score)
                             (toggle-name)
                             [:div.p-2 [start-stop]]
                             ]]
   [sketch-component setup-function update-function]])