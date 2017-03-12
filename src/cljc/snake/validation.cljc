(ns snake.validation
  (:require [snake.snakepure :refer [valid-directions]]))

(defn valid-registration-map?
  "Change the value of the head if it may run out of the board"
  [registration-map]
  (cond
    (not (map? registration-map)) '(false "Input is not a map")
    (not (contains? registration-map :username)) '(false "Input is missing the :username key.")
    (not (contains? registration-map :password)) '(false "Input is missing the :password key.")
    (< (count (:username registration-map)) 8) '(false "Username should have a minimal of 8 characters")
    (< (count (:password registration-map)) 8) '(false "Password should have a minimal of 8 characters")
    :else '(true)))

(defn valid-new-direction-map?
  "Change the value of the head if it may run out of the board"
  [registration-map]
  (cond
    (not (map? registration-map)) '(false "Input is not a map")
    (not (contains? registration-map :new-direction)) '(false "Input is missing the :new-direction key.")
    (not (contains? valid-directions (:new-direction registration-map))) '(false "Invalid next move, valid moves are: [0 1] [0 -1] [-1 0] [1 0].")
    :else '(true)))