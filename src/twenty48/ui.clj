;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.ui
  (:require [clojure.java.io :as io]
            [seesaw.core :as s]))

;;;;; Frame and views

(defn frame
  "Creates the application's one and only frame."
  []
  (s/frame :title "Twenty48"
           :size [600 :by 700]
           :on-close :exit))

(defn show-view!
  "Gets the view with the given identifier from the system and shows it."
  [sys id]
  (let [view (get-in @sys [:story id])]
    (s/config! (:frame @sys) :content view)
    (s/request-focus! view)))
  
;;;;; User interface elements

(defn label
  "Creates a label with the given text and font size. Extra arguments are passed
  on to seesaw.core/label (they must be key/value pairs)."
  [text size & more]
  (apply s/label
         :text text
         :font {:name "Lucida Grande" :size size}
         more))

(defn slider
  "Creates a slider with the most important options set. Extra arguments are
  passed on to seesaw.core/slider (they must be key/value pairs)."
  [from to major minor & more]
  (apply s/slider
         :min from
         :max to
         :major-tick-spacing major
         :minor-tick-spacing minor
         :snap-to-ticks? true
         :paint-labels? true
         :paint-ticks? true
         more))

(defn image
  "Loads an image located at the given path relative to the resourdices folder.
  Returns a component containing the image. Extra arugments are passed on to
  seesaw.core/label (they must be key/value pairs)."
  [path & more]
  (apply s/label
         :icon (-> path io/resource io/file)
         more))

(defn nav-button
  "Creates a button for navigating to the screen with the given identifer."
  [sys id text]
  (letfn [(action [_] (show-view! sys id))]
    (s/button :text text
              :size [80 :by 40]
              :listen [:action action])))
