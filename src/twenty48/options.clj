;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.options
  "Defines the options screen, where the user can configure the game."
  (:require [seesaw.core :as s]
            [seesaw.mig :as m]
            [twenty48.common :as c]))

;;;;; Options state

(def default-options
  {:grid-size 4
   :at-a-time 2
   :largest-new 2})

(def options (atom default-options))

(defn slide-option
  "Creates a slider to control an option."
  [id from to major minor]
  (letfn [(action [event]
            (swap! options assoc id (s/selection event)))]
    (c/slider from to major minor
      :value (get @options id)
      :listen [:selection action])))

;;;;; User interface helpers

(defn heading [text] (c/label text 32 :halign :center))
(defn option  [text] (c/label text 20))

;;;;; Options panel

(defn make-options-panel
  "Creates the options panel, a screen with configuration options."
  []
  (m/mig-panel
    :id :options
    :constraints ["fillx, wrap 2"
                  "[l][fill,grow]"
                  "[c,nogrid]25:push[t][t][t]25:push[c,nogrid]"]
    :items [[(c/nav-button :game "Back") "w 80, ax l"]
            [(heading "Options") "ax c, grow"]
            [:fill-h "w 80, wrap"]
            [(option "Grid Size")]
            [(slide-option :grid-size 4 16 2 1)]
            [(option "At a Time")]
            [(slide-option :at-a-time 1 5 1 0)]
            [(option "Largest New")]
            [(slide-option :largest-new 1 8 1 0)]
            ["© 2014 Mitchell Kember" "ax c"]]))
