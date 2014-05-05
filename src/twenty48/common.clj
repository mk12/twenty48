;;; Copyright 2013 Mitchell Kember. Subject to the MIT License.

(ns twenty48.common
  "Implements some common functionality used app-wide."
  (:require [clojure.java.io :as io]
            [seesaw.core :as s]))

;;;;; Cards state

(def cards (atom nil))

(defn make-cards
  "Creates a stack of cards and returns it after resetting the cards atom.
  Accepts an even number of arguments where each pair is of the form :id panel."
  [& pairs]
  (reset! cards
    (s/card-panel
      :focusable? false
      :items (map (fn [[id panel]] [panel id])
                  (partition 2 pairs)))))

(defn focus-card!
  "Requests focus for the card with the given id in a card panel."
  [cards id]
  (let [css-id (keyword (str "#" (name id)))
        card (s/select cards [css-id])]
    (s/request-focus! card)))

(defn show-card!
  "Shows the card with the given id."
  [id]
  (if-let [cs @cards]
    (focus-card! (s/show-card! cs id) id)))

;;;;; User interface

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
  [id text]
  (letfn [(action [_] (show-card! id))]
    (s/button :text text
              :size [80 :by 40]
              :listen [:action action])))
