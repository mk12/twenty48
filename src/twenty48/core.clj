;;; Copyright 2013 Mitchell Kember. Subject to the MIT License.

(ns twenty48.core
  "Gets the ball rolling by defining -main, which creates the Seesaw window."
  (:require [seesaw.core :as s]
            [twenty48.common :as c]
            [twenty48.game :as g]
            [twenty48.options :as o])
  (:gen-class))

;;; TODO
;;; deploy => :on-close :exit
;;; app icon

(defn make-panel
  "Creates the main panel, which contains all the others."
  []
  (c/make-cards :game (g/make-game-panel)
                :game-over (g/make-game-over-panel)
                :options (o/make-options-panel)))

(defn make-frame
  "Creates the application's one and only frame."
  []
  (s/frame :title "Twenty48"
           :size [600 :by 700]
           :content (make-panel)
           :on-close :hide))

(defn -main [& args]
  (s/native!)
  (s/invoke-later (s/show! (make-frame))))
