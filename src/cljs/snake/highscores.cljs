(ns snake.highscores
  (:require [re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as reagent :refer [atom]]
            [goog.string :as gstring]
            [goog.string.format]
            [utils.sketch :refer [sketch-component draw-game-state]]
            [utils.websocket :refer [send-transit-game!]]))

;; -- View Components ---------------------------------------------------------
(defonce sort-value (atom [:name false]))
(defonce interval (atom nil))

(defn update-function
  "updated the game state, or switch off the interval when another view is loaded"
  []
  (let [sel-menu-item (subscribe [:sel-menu-item])]
    (if (= @sel-menu-item "highscores")
      (send-transit-game! {:highscores true})
      (do (js/clearInterval @interval) (reset! interval nil)))))

(defn add-row
  [m k v]
  (conj m (assoc v :name (name k))))

(defn update-sort
  [[key reverse?] new-key]
  (if (= key new-key)
    [key (not reverse?)]
    (cond
      (= new-key :name) [new-key false]
      :default [new-key true])))

(defn highscore-table
  "Renders the table of the highscores"
  []
  (let [game-info (subscribe [:game-info])]
    (if-let [highscores (:highscores @game-info)]
      (let [[key reverse?] @sort-value
            unsorted-map (reduce-kv add-row `() highscores)
            sorted-map (if reverse? (reverse (sort-by key unsorted-map)) (sort-by key unsorted-map))]
        [:table.table.table-hover.table-striped.table-bordered
         [:thead>tr
          [:th {:on-click #(swap! sort-value update-sort :name)} "Name"]
          [:th {:on-click #(swap! sort-value update-sort :highest)} "Highest"]
          [:th {:on-click #(swap! sort-value update-sort :games-played)} "Games played"]
          [:th {:on-click #(swap! sort-value update-sort :average)} "Average"]
          [:th {:on-click #(swap! sort-value update-sort :total)} "Total"]
          ]
         [:tbody
          (for [item sorted-map]
            [:tr {:key (:name item)}
             [:th {:scope "row"} (str (:name item))]
             [:td (str (:highest item))]
             [:td (str (:games-played item))]
             [:td (gstring/format "%.1f" (:average item))]
             [:td (str (:total item))]])]
         ]))))

(defn view
  "The game rendering function"
  []
  (do
    (if (nil? @interval)
      (reset! interval (js/setInterval #(update-function) 1000)))
    [:div
     [:div.container (highscore-table)]]))