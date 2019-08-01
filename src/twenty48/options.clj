;;; Copyright 2019 Mitchell Kember. Subject to the MIT License.

(ns twenty48.options
  "Defines the options screen, where the user can configure the game."
  (:require [seesaw.core :as s]
            [seesaw.mig :as m]
            [twenty48.ui :as ui]))

(def default-options
  {:grid-size 4      ; side length of the grid
   :at-a-time 1      ; number of new blocks added per turn
   :largest-new 2    ; largest block value added (represented as power of 2)
   :probability 90}) ; probability that a new block will be the smallest one

(defn slide-option
  "Creates a slider to control an option."
  [sys id from to major minor]
  (letfn [(action [event]
            (swap! sys assoc-in [:options id] (s/selection event)))]
    (ui/slider from to major minor
               :value (get-in @sys [:options id])
               :listen [:selection action])))

(defn heading [text] (ui/label text 32 :halign :center))
(defn option  [text] (ui/label text 20))

(defn options-view
  "Creates the options view, a screen with configuration options."
  [sys]
  (m/mig-panel
    :id :options
    :constraints ["fillx, wrap 2"
                  "[l][fill,grow]"
                  "[c,nogrid]25:push[t][t][t][t]25:push[c,nogrid]"]
    :items [[(ui/nav-button sys :game "Back") "w 80, ax l"]
            [(heading "Options") "ax c, grow"]
            [:fill-h "w 80, wrap"]
            [(option "Grid Size")]
            [(slide-option sys :grid-size 2 10 2 1)]
            [(option "At a Time")]
            [(slide-option sys :at-a-time 1 5 1 0)]
            [(option "Largest New")]
            [(slide-option sys :largest-new 1 8 1 0)]
            [(option "Probability")]
            [(slide-option sys :probability 0 100 20 10)]
            ["© 2019 Mitchell Kember" "ax c"]]))
