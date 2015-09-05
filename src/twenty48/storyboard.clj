;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.storyboard
  "Implements storyboards, which "
  (:require [seesaw.core :as s]))

(defn storyboard
  ""
  [frame & views]
  nil)

(defn show-game
  [{:keys [options]}]
  ;; (config! 

;; (def ls [{:id :a :stuff "a" :hi 5} {:id :b :stuff "b" :hi 6}])
;; (zipmap (map :id ls) (map #(dissoc % :id) ls))
;; (into {} (map (fn [a] [(:id a) (dissoc a :id)]) ls))
;; hash-map

(def game-view
  {:id :game :make make-game-panel :show show-game-panel})

(group-by :id ls)
(s/storyboard :game [g/game-panel g/show-game]
              :options [o/

;; :game [create recreate]
;; :game [create show!]

(defn show!
  ""
  [sb id & more]
  nil)

(defn nav-button
  ""
  []
  nil)
