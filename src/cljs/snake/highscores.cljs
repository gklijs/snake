(ns snake.highscores
  (:require [re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as reagent :refer [atom]]
            [goog.string :as gstring]
            [goog.string.format]
            [utils.sketch :refer [sketch-component draw-game-state]]
            [utils.websocket :refer [send-transit-game!]]))

;; -- View Components ---------------------------------------------------------
(defonce sort-value (atom [:name nil]))
(defonce interval (atom nil))

(defn update-function
  "updated the game state, or switch off the interval when another view is loaded"
  []
  (let [sel-menu-item (subscribe [:sel-menu-item])]
    (if (= @sel-menu-item "highscores")
      (send-transit-game! {:highscores true})
      (do (js/clearInterval js/window @interval) (reset! interval nil)))))

(defn highscore-table
  "Renders the table of the highscores"
  []
  (let [game-info (subscribe [:game-info])]
    (if-let [highscores (:highscores @game-info)]
      (let [unsorted-map (atom ())
            _ (doseq [[k scoremap] highscores] (swap! unsorted-map conj (assoc scoremap :name (name k))))
            [key operator] @sort-value
            sorted-map (if operator (sort-by key operator @unsorted-map) (sort-by key @unsorted-map))]
        [:table.table.table-hover.table-striped.table-bordered
         [:thead>tr
          [:th "Name"]
          [:th "Highest"]
          [:th "Games played"]
          [:th "Average"]
          ]
         [:tbody
          (for [item sorted-map]
            [:tr {:key (:name item)}
             [:th {:scope "row"} (str (:name item))]
             [:td (str (:highest item))]
             [:td (str (:games-played item))]
             [:td (gstring/format "%.1f" (:average-float item))]])]
         ]))))

(defn view
  "The game rendering function"
  []
  (do
    (if (nil? @interval)
      (reset! interval (js/setInterval #(update-function) 1000)))
    [:div
     [:div.container.controls [:div.d-flex.justify-content-end
                               [:div.mr-auto.p-2 [:div (str "Sort by: ")]]
                               [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! sort-value [:name nil])} "Name"]]
                               [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! sort-value [:highest >])} "Highest"]]
                               [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! sort-value [:games-played >])} "Games played"]]
                               [:div.p-2 [:button.btn.btn-secondary {:type "button" :on-click #(reset! sort-value [:average-float >])} "Average"]]
                               ]]
     [:div.container (highscore-table)]]))