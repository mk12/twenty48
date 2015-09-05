;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.system
  (:require [twenty48.common :as c]
            [twenty48.game :as g]
            [twenty48.options :as o]
            [twenty48.ui :as ui])
  (:gen-class))

(defn views
  "Creates the views for the application."
  [sys]
  {:game (g/game-view sys)
   :game-over (g/game-over-view sys)
   :options (o/options-view sys)})

(defn system
  "Returns a new instance of the system, an atom containing all the state needed
  by the application."
  []
  (let [sys (atom nil)]
    (-> {:score 0
         :options (o/default-options)
         :frame (ui/frame)
         :views (views sys)}
        (swap! sys merge))))

(defn start
  "Performs side effects with the system to start the application."
  [sys]
  (ui/show-view! sys :game)
  (s/show! (:frame @sys)))

(defn stop
  "Perform side effects with the system to stop the application."
  [sys]
  (s/dispose! (:frame @sys)))
