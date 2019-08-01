;;; Copyright 2019 Mitchell Kember. Subject to the MIT License.

(ns twenty48.system
  (:require [seesaw.core :as s]
            [twenty48.game :as g]
            [twenty48.options :as o]
            [twenty48.ui :as ui]))

(defn views
  "Creates the views for the application."
  [sys]
  {:game (delay (g/game-view sys))
   :options (delay (o/options-view sys))})

(defn system
  "Returns a new instance of the system, an atom containing all the state needed
  by the application. Takes the deployment environment as an argument."
  [env]
  {:pre [(#{:dev :prod} env)]}
  (let [sys (atom {:env env
                   :status :created
                   :options o/default-options
                   :frame (ui/frame env)})]
    (swap! sys assoc :views (views sys))
    sys))

(defn start
  "Performs side effects to start the application. Returns the system."
  [sys]
  ;; The native! function must be called before any other Swing calls.
  (s/native!)
  (ui/show-view-by-id! sys :game)
  (s/show! (:frame @sys))
  (swap! sys assoc :status :running)
  sys)

(defn stop
  "Performs side effects to stop the application. Returns the system."
  [sys]
  (s/dispose! (:frame @sys))
  (swap! sys assoc :status :stopped)
  sys)
